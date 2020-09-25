/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Richard Warren <Richard.Warren@hdfgroup.org>
 *              Wednesday, July 1, 2020
 *
 * Purpose:     This is part of a parallel subfiling I/O driver.
 *
 */

#include "H5FDsubfile_public.h"

/***********/
/* Headers */
/***********/
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Dprivate.h"  /* Datasets                                 */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5Iprivate.h"  /* IDs                                      */
#include "H5Ipublic.h"   /* IDs                                      */
#include "H5MMprivate.h" /* Memory management                        */
#include "H5Pprivate.h"  /* Property lists                           */
#include "H5private.h"   /* Generic Functions                        */

/*
=========================================
Private functions
=========================================
*/

/* Modifiable via environment variable */
static sf_ioc_selection_t sf_default_ioc_selection = SELECT_IOC_ONE_PER_NODE;

/*
-----------------------------------------------------------------------------------
sf_topology_limit   -- How many different topologies can be recorded (default =
4) sf_topology_entries -- The number of topologies that are currently recorded.
sf_topology_cache   -- Storage for the known topologies
-----------------------------------------------------------------------------------
*/
static size_t         sf_topology_limit = 4;
static sf_topology_t *sf_topology_cache = NULL;

/*
--------------------------------------------------------------------------
sf_context_limit   -- How many contexts can be recorded (default = 4)
sf_context_entries -- The number of contexts that are currently recorded.
sf_context_cache   -- Storage for contexts
--------------------------------------------------------------------------
*/
static size_t               sf_context_limit = 16;
static subfiling_context_t *sf_context_cache = NULL;

/*
-------------------------------------------------------------------------
  Programmer:  Richard Warren <Richard.Warren@hdfgroup.org>
  Purpose:     Return a pointer to the requested storage object.
               There are only 2 object types: TOPOLOGY or CONTEXT
               structures.  An object_id contains the object type
               in upper 32 bits and an index value in the lower 32 bits.
               Storage for an object is allocated as required.

               Topologies are static, i.e. for any one IO Concentrator
               allocation strategy, the results should always be the
               same.
               FIXME: The one exception to this being the 1 IOC per
               N MPI ranks. The value of N can be changed on a per-file
               basis, so we need address that at some point.

               Contexts are 1 per open file.  If only one file is open
               at a time, then we will only use a single context cache
               entry.
  Errors:      returns NULL if input SF_OBJ_TYPE is unrecognized or
               a memory allocation error.

  Revision History -- Initial implementation
-------------------------------------------------------------------------
*/
void *
get_subfiling_object(int64_t object_id)
{
    int obj_type = (int) ((object_id >> 32) & 0x0FFFF);
    /* We don't require a large indexing space
     * 16 bits should be enough..
     */
    size_t index = (object_id & 0x0FFFF);
    if (obj_type == SF_TOPOLOGY) {
        if (sf_topology_cache == NULL) {
            sf_topology_cache = (sf_topology_t *) calloc(
                sf_topology_limit, sizeof(sf_topology_t));
            assert(sf_topology_cache != NULL);
        }
        if (index < sf_topology_limit) {
            return (void *) &sf_topology_cache[index];
        } else {
            puts("Illegal toplogy object index");
        }
    } else if (obj_type == SF_CONTEXT) {
        if (sf_context_cache == NULL) {
            sf_context_cache = (subfiling_context_t *) calloc(
                sf_context_limit, sizeof(subfiling_context_t));
            assert(sf_context_cache != NULL);
        }
        if (index == sf_context_limit) {
            sf_context_limit *= 2;
            sf_context_cache = (subfiling_context_t *) realloc(sf_context_cache,
                sf_context_limit * sizeof(subfiling_context_t));
            assert(sf_context_cache != NULL);
        } else {
            return (void *) &sf_context_cache[index];
        }
    } else {
        printf(
            "get_subfiling_object: UNKNOWN Subfiling object type id = 0x%lx\n",
            object_id);
    }
    return NULL;
}

/*
======================================================
Public vars (for subfiling) and functions
We probably need a function to set and clear this
======================================================
*/
int sf_verbose_flag = 0;

/*
======================================================
File functions

The pread and pwrite posix functions are described as
being thread safe.
======================================================
*/
int
sf_read_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size,
    int subfile_rank)
{
    int     ret = 0;
    ssize_t bytes_read;
    ssize_t bytes_remaining = (ssize_t) data_size;
    char *  this_buffer = data_buffer;

    while (bytes_remaining) {
        if ((bytes_read = (ssize_t) pread(
                 fd, this_buffer, (size_t) bytes_remaining, file_offset)) < 0) {

            perror("pread failed!");
            printf("[ioc(%d) %s] pread(fd, buf, bytes_remaining=%ld, "
                   "file_offset =%ld)\n",
                subfile_rank, __func__, bytes_remaining, file_offset);
            fflush(stdout);
            return -1;
        } else if (bytes_read > 0) {
            bytes_remaining -= bytes_read;
            this_buffer += bytes_read;
            file_offset += bytes_read;
        } else {
            printf("[ioc(%d) %s] ERROR! read of 0 bytes == eof!\n",
                subfile_rank, __func__);
            fflush(stdout);
            return -2;
        }
    }
    return ret;
}

int
sf_write_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size,
    int subfile_rank)
{
    int     ret = 0;
    char *  this_data = (char *) data_buffer;
    ssize_t bytes_remaining = (ssize_t) data_size;
    ssize_t written = 0;
    while (bytes_remaining) {
        if ((written = pwrite(
                 fd, this_data, (size_t) bytes_remaining, file_offset)) < 0) {
            perror("pwrite failed!");
            printf("[ioc(%d) %s] pwrite(fd, data, bytes_remaining=%ld, "
                   "file_offset =%ld)\n",
                subfile_rank, __func__, bytes_remaining, file_offset);
            fflush(stdout);
            return -1;
        } else {
            bytes_remaining -= written;
            this_data += written;
            file_offset += written;
        }
    }
    /* We don't usually use this for each file write.  We usually do the file
     * flush as part of file close operation.
     */
#ifdef SUBFILE_REQUIRE_FLUSH
    fdatasync(fd);
#endif
    return ret;
}

/*
-------------------------------------------------------------------------
  Programmer:  Richard Warren <Richard.Warren@hdfgroup.org>
  Purpose:     Return a character string which represents either the
               default selection method: SELECT_IOC_ONE_PER_NODE; or
               if the user has selected a method via the environment
               variable (H5_IOC_SELECTION_CRITERIA), we return that
               along with any optional qualifier with for that method.

  Errors:      None.

  Revision History -- Initial implementation
-------------------------------------------------------------------------
*/
char *
get_ioc_selection_criteria(sf_ioc_selection_t *selection)
{
    char *optValue = NULL;
    char *envValue = HDgetenv("H5_IOC_SELECTION_CRITERIA");

    /* For non-default options, the environment variable
     * should have the following form:  integer:[integer|string]
     * In particular, EveryNthRank == 1:64 or every 64 ranks assign an IOC
     * or WithConfig == 2:/<full_path_to_config_file>
     */
    if (envValue && (optValue = strchr(envValue, ':'))) {
        *optValue++ = 0;
    }
    if (envValue) {
        int checkValue = atoi(envValue);
        if ((checkValue < 0) || (checkValue >= ioc_selection_options)) {
            *selection = sf_default_ioc_selection;
            return NULL;
        } else {
            *selection = (sf_ioc_selection_t) checkValue;
            return optValue;
        }
    }
    *selection = sf_default_ioc_selection;
    return NULL;
}

/*
-------------------------------------------------------------------------
  Programmer:  Richard Warren <Richard.Warren@hdfgroup.org>
  Purpose:     Called as part of a file open operation, we initialize a
               subfiling context which includes the application topology
               along with other relevant info such as the MPI objects
               (communicators) for communicating with IO concentrators.
               We also identify which MPI ranks will have IOC threads
               started on them.

               We return a context ID via the 'sf_context' variable.

  Errors:      returns an error if we detect any initialization errors,
               including malloc failures or any resource allocation
               problems.

  Revision History -- Initial implementation
-------------------------------------------------------------------------
*/
herr_t
H5FDsubfiling_init(sf_ioc_selection_t ioc_select_method, char *ioc_select_option,
    int64_t *sf_context)
{
    herr_t               ret_value = SUCCEED;
    int                  ioc_count;
    int                  world_rank, world_size;
    sf_topology_t *      thisApp = NULL;
    int                  active_file_maps = active_map_entries();
    int64_t              tag = SF_CONTEXT;
    int64_t              context_id = ((tag << 32) | active_file_maps);
    subfiling_context_t *newContext =
        (subfiling_context_t *) get_subfiling_object(context_id);

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "x*s*IL", ioc_select_method, ioc_select_option, sf_context);

    if (MPI_Comm_size(MPI_COMM_WORLD, &world_size) != MPI_SUCCESS) {
        puts("MPI_Comm_size returned an error");
        ret_value = FAIL;
        goto done;
    }
    if (MPI_Comm_rank(MPI_COMM_WORLD, &world_rank) != MPI_SUCCESS) {
        puts("MPI_Comm_rank returned an error");
        ret_value = FAIL;
        goto done;
    }

    if ((ioc_count = H5FD__determine_ioc_count(world_size, world_rank,
             ioc_select_method, ioc_select_option, &thisApp)) <= 0) {
        puts("Unable to register subfiling topology!");
        ret_value = FAIL;
        goto done;
    }

    newContext->sf_context_id = context_id;

    if (H5FD__init_subfile_context(
            thisApp, ioc_count, world_rank, newContext) != SUCCEED) {
        puts("Unable to initialize a subfiling context!");
        ret_value = FAIL;
        goto done;
    }

    if (newContext->topology->rank_is_ioc) {
        int status = initialize_ioc_threads(newContext);
        if (status)
            goto done;
    }

    if (context_id < 0) {
        ret_value = FAIL;

        goto done;
    }
    *sf_context = context_id;

done:
    FUNC_LEAVE_API(ret_value)

    return ret_value;
}
