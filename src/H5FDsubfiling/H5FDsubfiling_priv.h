/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Private definitions for HDF5 Subfiling VFD
 */

#ifndef H5FDsubfiling_priv_H
#define H5FDsubfiling_priv_H

/********************/
/* Standard Headers */
/********************/

/* TODO: review headers needed */
#include <assert.h>
#include <libgen.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mpi.h"

/**************/
/* H5 Headers */
/**************/

#include "H5private.h"     /* Generic Functions                        */
#include "H5CXprivate.h"   /* API Contexts                             */
#include "H5Dprivate.h"    /* Datasets                                 */
#include "H5Eprivate.h"    /* Error handling                           */
#include "H5FDsubfiling.h" /* Subfiling VFD                            */
#include "H5FDioc.h"       /* IOC VFD                                  */
#include "H5Iprivate.h"    /* IDs                                      */
#include "H5MMprivate.h"   /* Memory management                        */
#include "H5Pprivate.h"    /* Property lists                           */

/* TODO: eventually reorganize common header */
#include "subfiling_common.h"

#if 1 /* JRM */ /* For now, H5FDsubfiling_priv.h needs mercury.  Since the code that needs it will           \
                 * move to its own header, just hack it for now.                                             \
                 */
#include "mercury_thread.h"
#include "mercury_thread_mutex.h"
#include "mercury_thread_pool.h"
#endif /* JRM */

#define DRIVER_INFO_MESSAGE_MAX_INFO   65536
#define DRIVER_INFO_MESSAGE_MAX_LENGTH 65552 /* MAX_INFO + sizeof(info_header_t) */

typedef struct stat_record {
    int64_t op_count; /* How many ops in total */
    double  min;      /* minimum (time)         */
    double  max;      /* maximum (time)        */
    double  total;    /* average (time)        */
} stat_record_t;

typedef enum stat_category { /* Stat (OP) Categories  */
                             WRITE_STAT = 0,
                             WRITE_WAIT,
                             READ_STAT,
                             READ_WAIT,
                             FOPEN_STAT,
                             FCLOSE_STAT,
                             QUEUE_STAT,
                             TOTAL_STAT_COUNT
} stat_category_t;

typedef struct _info_header { /* Header for a driver info message */
    uint8_t version;
    uint8_t unused_1;
    uint8_t unused_2;
    uint8_t unused_3;    /* Actual info message length, but  */
    int32_t info_length; /* CANNOT exceed 64k (65552) bytes  */
    char    vfd_key[8];  /* 's' 'u' 'b' 'f' 'i' 'l' 'i' 'n'  */
} info_header_t;

/* Here are the basic key values to be used when accessing
 * the cache of stored topologies or contexts.
 */
typedef enum {
    SF_BADID    = (-1),
    SF_TOPOLOGY = 1,
    SF_CONTEXT  = 2,
    SF_NTYPES /* number of subfiling object types, MUST BE LAST */
} sf_obj_type_t;

typedef struct {            /* Format of a context map entry  */
    uint64_t h5_file_id;    /* key value (linear search of the cache) */
    hid_t    sf_context_id; /* The return value if matching h5_file_id */
} file_map_to_context_t;

extern FILE *client_log;

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL herr_t H5FD__subfiling__truncate_sub_files(int64_t logical_file_eof, hid_t context_id);
H5_DLL herr_t H5FD__subfiling__get_real_eof(int64_t *logical_eof_ptr, hid_t context_id);

H5_DLL int init__indep_io(void *_sf_context, size_t depth, int ioc_total, int64_t *sf_source_data_offset,
                          int64_t *sf_datasize, int64_t *f_offset, int *first_index, int *n_containers,
                          int64_t offset, int64_t elements, int dtype_extent);

H5_DLL int subfiling_open_file(sf_work_request_t *msg, int subfile_rank, int flags);

H5_DLL void delete_subfiling_context(hid_t context_id);
H5_DLL void set_verbose_flag(int subfile_rank, int new_value);

#ifdef __cplusplus
}
#endif

#endif /* H5FDsubfiling_priv_H */
