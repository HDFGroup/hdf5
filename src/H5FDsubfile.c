
#include "H5FDsubfile_public.h"

#ifdef H5_HAVE_PARALLEL

/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                        */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Dprivate.h"         /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Ipublic.h"          /* IDs                                      */
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Pprivate.h"         /* Property lists                           */

/* 
=========================================
Private functions
========================================
*/

static size_t sf_topology_limit = 4;
static size_t sf_topology_entries = 0;
static sf_topology_t **sf_topology_cache = NULL;

static size_t sf_context_limit = 4;
static size_t sf_context_entries = 0;
static subfiling_context_t **sf_context_cache = NULL;
static hid_t context_id = H5I_INVALID_HID;
static hid_t topology_id = H5I_INVALID_HID;


static int64_t record_subfiling_object(SF_OBJ_TYPE type, void *obj)
{
	size_t index;
	int64_t obj_reference;
	uint64_t tag;
	switch(type) {
	case SF_TOPOLOGY: {
		if (sf_topology_cache == NULL) {
			sf_topology_cache = (sf_topology_t **)
				calloc(sf_topology_limit, sizeof(sf_topology_t *));
		}
		assert(sf_topology_cache != NULL);
		index = sf_topology_entries++;
		tag = SF_TOPOLOGY;
		obj_reference = (int64_t)((tag << 32) | index);
		sf_topology_cache[index] = obj;
		return obj_reference;
		break;
	}
	case SF_CONTEXT: {
		if (sf_context_cache == NULL) {
			sf_context_cache = (subfiling_context_t **)
				calloc(sf_context_limit, sizeof(subfiling_context_t *));
		}
		assert(sf_context_cache != NULL);
		index = sf_context_entries++;
		tag = SF_CONTEXT;
		obj_reference = (int64_t)((tag << 32) | index);
		sf_context_cache[index] = (subfiling_context_t *)obj;
		return obj_reference;
		break;
	}
	default:
		puts("UNKNOWN Subfiling object type");
	}

	return -1;
}

/* 
=========================================
Public vars (for subfiling) and functions
========================================
*/

int sf_verbose_flag = 0;

/*
=========================================
File functions
=========================================

The pread and pwrite posix functions are described as
being thread safe. We include mutex locks and unlocks
to work around any potential threading conflicts...
Those however, are compiled according #ifdef 
*/

int sf_read_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank)
{
    int ret = 0;
    ssize_t bytes_read;
    ssize_t bytes_remaining = (ssize_t)data_size;
    char *this_buffer = data_buffer;

    while(bytes_remaining) {
        if ((bytes_read = (ssize_t)pread(fd, this_buffer, (size_t)bytes_remaining, file_offset)) < 0) {
            perror("pread failed!");
            fflush(stdout);
        }
        else if (bytes_read > 0) {
            if (sf_verbose_flag) {
                printf("[ioc(%d) %s] read %ld bytes of %ld requested\n",
                       subfile_rank, __func__,
                       bytes_read, bytes_remaining);
            }
            bytes_remaining -= bytes_read;
            this_buffer += bytes_read;
            file_offset += bytes_read;
        }
        else {
            printf("[ioc(%d) %s] ERROR! read of 0 bytes == eof!\n", subfile_rank, __func__ );
            fflush(stdout);
            break;
        }
    }
    return ret;
}

int sf_write_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank)
{
    int ret = 0;
    char *this_data = (char *)data_buffer;
    ssize_t bytes_remaining = (ssize_t) data_size;
    ssize_t written = 0;

    while(bytes_remaining) {
        if ((written = pwrite(fd, this_data, (size_t)bytes_remaining, file_offset)) < 0) {
            perror("pwrite failed!");
            fflush(stdout);
        }
        else {
            if (sf_verbose_flag) {
                printf("[ioc(%d) %s] wrote %ld bytes of %ld requested\n",
                       subfile_rank, __func__,
                       written, bytes_remaining);
            }
            bytes_remaining -= written;
            this_data += written;
            file_offset += written;
        }
    }
#ifdef SUBFILE_REQUIRE_FLUSH
    fdatasync(fd);
#endif

    return ret;
}




void * get_subfiling_object(int64_t object_id)
{
	int obj_type = (int)((object_id >> 32) & 0x0FFFF);
    /* We don't require a large indexing space 
	 * 16 bits should be enough..
	 */
	size_t index = (object_id & 0x0FFFF); 
	if (obj_type == SF_TOPOLOGY) {
		if (index < sf_context_entries) {
			return (void *)sf_topology_cache[index];
		}
		else {
			puts("Illegal object index");
		}
	}
	else if (obj_type == SF_CONTEXT) {
		if (index < sf_context_entries) {
			return (void *)sf_context_cache[index];
		}
		else {
			puts("Illegal object index");
		}
	}
	else {
		puts("UNKNOWN Subfiling object type");
	}
	return NULL;
}

herr_t
H5FDsubfiling_init(void)
{
    herr_t ret_value = SUCCEED;
	int ioc_count;
	int world_rank, world_size;
	sf_topology_t *thisApp = NULL;
	subfiling_context_t *newContext = NULL;
	
	FUNC_ENTER_API(FAIL)
    H5TRACE0("e","");

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
	if ((ioc_count = H5FD__determine_ioc_count (world_size, world_rank, &thisApp)) > 0) {
		topology_id = (hid_t)record_subfiling_object(SF_TOPOLOGY, thisApp);
	}
	if (topology_id < 0) {
		puts("Unable to register subfiling topology!");
		ret_value = FAIL;
		goto done;
	}
	if (H5FD__init_subfile_context(&newContext, ioc_count, world_size, world_rank, thisApp->rank_is_ioc) != SUCCEED) {
		puts("Unable to initialize a subfiling context!");
		ret_value = FAIL;
		goto done;
	}
	context_id = (hid_t)record_subfiling_object(SF_CONTEXT, newContext);
	if (context_id < 0) {
		ret_value = FAIL;
		puts("Unable to register subfiling context!");
	}
	
done:
    FUNC_LEAVE_API(ret_value)

	return ret_value;
}

herr_t
H5FDsubfiling_finalize(void)
{
    herr_t ret_value = SUCCEED;         /* Return value */

	FUNC_ENTER_API(FAIL)
    H5TRACE0("e","");
	
	/* Shutdown the IO Concentrator threads */
	sf_shutdown_flag = 1;
	usleep(100);
	MPI_Barrier(MPI_COMM_WORLD);
	delete_subfiling_context(context_id);

    FUNC_LEAVE_API(ret_value)
done:
	return ret_value;
}

hid_t
get_subfiling_context(void)
{
	return context_id;
}

#endif	/* H5_HAVE_PARALLEL */
