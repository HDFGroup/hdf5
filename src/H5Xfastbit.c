/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:	FastBit index routines.
 */

/****************/
/* Module Setup */
/****************/

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions */
#include "H5Xprivate.h"         /* Index */
#include "H5Eprivate.h"		/* Error handling */
#include "H5Iprivate.h"		/* IDs */
#include "H5MMprivate.h"	/* Memory management */
#include "H5Pprivate.h"
#include "H5Qprivate.h"
#include "H5VMprivate.h"
#include "H5Sprivate.h"
/* TODO using private headers but could use public ones */

/* TODO move these includes to someplace where all plugins get it (by default) */
#ifdef H5_HAVE_PARALLEL
#include "H5FDmpi.h"
#include "H5FDmpio.h"
#endif

#ifdef H5_HAVE_FASTBIT
/**
 * Header file that defines an in-memory C API for accessing the querying
 * functionality of FastBit IBIS implementation.  It is primarily for
 * in memory data.
 */
#include <fastbit/iapi.h>

/****************/
/* Local Macros */
/****************/
#define LIMIT_RESULTS_FOR_TESTING
#define SHOW_TIME
// #define H5X_FASTBIT_DEBUG

#ifdef H5X_FASTBIT_DEBUG
#define H5X_FASTBIT_DEBUG_LVL 6
// #define H5X_FASTBIT_DEBUG_LVL 2
#define H5X_FASTBIT_LOG_DEBUG(...) do {                         \
      fprintf(stdout, " # %s(): ", __func__);                   \
      fprintf(stdout, __VA_ARGS__);                             \
      fprintf(stdout, "\n");                                    \
      fflush(stdout);                                           \
  } while (0)
#else
#define H5X_FASTBIT_DEBUG_LVL 0
#define H5X_FASTBIT_LOG_DEBUG(...) do { \
  } while (0)
#endif

#define H5X_FASTBIT_MAX_NAME 1024

struct fastbit_meta_collection {
 /* hsize_t base_offset;   // We'll eventually need this too... */
    hsize_t data_length;
    hsize_t nkeys;
    hsize_t noffsets;
    hsize_t nbitmaps;
};

/******************/
/* Local Typedefs */
/******************/
typedef struct H5X_fastbit_t {
    void *private_metadata;     /* Internal metadata */

    hid_t file_id;              /* ID of the indexed dataset file */
    hid_t dataset_id;           /* ID of the indexed dataset */

    unsigned dataset_ndims;     /* dataset number of dimensions */
    hsize_t *dataset_dims;      /* dataset dimensions */
    hsize_t *dataset_down_dims; /* dataset downed dimensions */

    hid_t opaque_type_id;   /* Datatype used for index datasets */

    hid_t keys_id;          /* Array for keys */
    uint64_t nkeys;
    double *keys;

    hid_t offsets_id;       /* Array for offset */
    uint64_t noffsets;
    int64_t *offsets;

    hid_t bitmaps_id;       /* Array for bitmaps */
    uint64_t nbitmaps;
    uint32_t *bitmaps;

    char *dataset_name;
    char column_name[H5X_FASTBIT_MAX_NAME];

    /* For parallel query/indexing support */
    int nblocks;
    hid_t filespace_id; 
    hid_t memspace_id;
    hid_t index_info_group_id;
    hsize_t filespace_offset;
    int64_t blocksize;
    int64_t nelmts;

    hbool_t idx_reconstructed;
    hbool_t idx_multiblock;
} H5X_fastbit_t;

struct H5X_fastbit_scatter_info {
    const void *src_buf;    /* Source data buffer */
    size_t src_buf_size;    /* Remaining number of elements to return */
};

typedef struct H5X_fastbit_list {
    hid_t dataset_id;
    H5X_fastbit_t *fastbit_info;
    struct H5X_fastbit_list *next;
} H5X_fastbit_list_t;


static H5X_fastbit_list_t *H5X_fastbit_current_datasets = NULL;

/********************/
/* Local Prototypes */
/********************/

static H5X_fastbit_t *
H5X__get_fastbit(hid_t dataset_id);

static H5X_fastbit_t *
H5X__fastbit_init(hid_t dataset_id);

static herr_t
H5X__fastbit_term(H5X_fastbit_t *fastbit);

static unsigned int
H5X__fastbit_hash_string(const char *string);

static enum FastBitDataType
H5X__fastbit_convert_type(hid_t type);

static herr_t
H5X__fastbit_read_data(hid_t dataset_id, const char *column_name, void **buf,
        size_t *buf_size);

static herr_t
H5X__fastbit_build_index(H5X_fastbit_t *fastbit);

static herr_t
H5X__fastbit_define_dataset(H5X_fastbit_t *fastbit, int typeIndex, struct fastbit_meta_collection *gatherInfo,
			    hsize_t *array_size, hsize_t *offset,  hid_t *space_id, hid_t *memspace_id);

static herr_t
H5X__fastbit_build_parallel_index(H5X_fastbit_t *fastbit);

static herr_t
H5X__fastbit_serialize_metadata(H5X_fastbit_t *fastbit, void *buf,
        size_t *buf_size);

static herr_t
H5X__fastbit_deserialize_metadata(H5X_fastbit_t *fastbit, void *buf);

static herr_t
H5X__fastbit_reconstruct_parallel_index(H5X_fastbit_t *fastbit);

static herr_t
H5X__fastbit_reconstruct_index(H5X_fastbit_t *fastbit);

static double
H5X__fasbit_query_get_value_as_double(H5Q_t *query);

static herr_t
H5X__fastbit_evaluate_query(H5X_fastbit_t *fastbit, hid_t query_id,
        uint64_t **coords, uint64_t *ncoords);

static herr_t
H5X__fastbit_create_selection(H5X_fastbit_t *fastbit, hid_t dataspace_id,
        uint64_t *coords, uint64_t ncoords);

static void *
H5X_fastbit_create(hid_t dataset_id, hid_t xcpl_id, hid_t xapl_id,
        size_t *metadata_size, void **metadata);

static herr_t
H5X_fastbit_remove(hid_t file_id, size_t metadata_size, void *metadata);

static void *
H5X_fastbit_open(hid_t dataset_id, hid_t xapl_id, size_t metadata_size,
        void *metadata);

static herr_t
H5X_fastbit_close(void *idx_handle);

static herr_t
H5X_fastbit_pre_update(void *idx_handle, hid_t dataspace_id, hid_t xxpl_id);

static herr_t
H5X_fastbit_post_update(void *idx_handle, const void *buf, hid_t dataspace_id,
        hid_t xxpl_id);

static hid_t
H5X_fastbit_query(void *idx_handle, hid_t dataspace_id, hid_t query_id,
        hid_t xxpl_id);

static herr_t
H5X_fastbit_refresh(void *idx_handle, size_t *metadata_size, void **metadata);

static herr_t
H5X_fastbit_get_size(void *idx_handle, hsize_t *idx_size);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* FastBit index class */
static H5X_idx_class_t idx_class = {.data_class = {
    H5X_fastbit_create,             /* create */
    H5X_fastbit_remove,             /* remove */
    H5X_fastbit_open,               /* open */
    H5X_fastbit_close,              /* close */
    H5X_fastbit_pre_update,         /* pre_update */
    H5X_fastbit_post_update,        /* post_update */
    H5X_fastbit_query,              /* query */
    H5X_fastbit_refresh,            /* refresh */
    NULL,                           /* copy */
    H5X_fastbit_get_size            /* get_size */
}};

const H5X_class_t H5X_FASTBIT[1] = {{
    H5X_CLASS_T_VERS,               /* (From the H5Xpublic.h header file) */
    H5X_PLUGIN_FASTBIT,             /* (Or whatever number is assigned) */
    "FASTBIT index plugin",         /* Whatever name desired */
    H5X_TYPE_DATA,                  /* This plugin operates on dataset elements */
    H5X_SIMPLE_QUERY,               /* plugin does NOT support multiple condition (compound) queries */
    &idx_class                      /* Index class */
}};

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_add_to_list
 *
 * Purpose: Creates and then adds a H5X_fastbit_list_t entry
 *          to a list of known datasets.
 *-------------------------------------------------------------------------
 */

static H5X_fastbit_list_t *
H5X__fastbit_add_to_list(H5X_fastbit_t *fastbit_info)
{
    H5X_fastbit_list_t *ret_value = NULL;
    H5X_fastbit_list_t *new_entry = NULL;

    FUNC_ENTER_NOAPI_NOINIT
    HDassert(fastbit_info);

    if (NULL == (new_entry = (H5X_fastbit_list_t *)H5MM_malloc(sizeof(H5X_fastbit_list_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate FastBit list struct");

    new_entry->dataset_id = fastbit_info->dataset_id;
    new_entry->fastbit_info = fastbit_info;
    new_entry->next = H5X_fastbit_current_datasets;
    H5X_fastbit_current_datasets = new_entry;
    ret_value = new_entry;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_remove_from_list
 *
 * Purpose: Creates and then adds a H5X_fastbit_list_t entry
 *          to a list of known datasets.
 *-------------------------------------------------------------------------
 */
static void H5X__fastbit_remove_from_list(hid_t dataset_id)
{
    H5X_fastbit_list_t *thisEntry = H5X_fastbit_current_datasets;
    H5X_fastbit_list_t *previous = NULL;
    while (thisEntry && thisEntry->dataset_id != dataset_id) {
       previous  = thisEntry;
       thisEntry = thisEntry->next;
    }
    if (thisEntry == H5X_fastbit_current_datasets)
       H5X_fastbit_current_datasets = thisEntry->next;
    if (previous) previous->next = thisEntry->next;
    if (thisEntry) H5MM_free(thisEntry);
}

/*-------------------------------------------------------------------------
 * Function:    H5X__get_fastbit
 *
 * Purpose: Searchs the list of known datasets and returns a
 * H5X_fastbit_t pointer if it contains the requested dataset_id.
 *-------------------------------------------------------------------------
 */
static H5X_fastbit_t *
H5X__get_fastbit(hid_t dataset_id)
{
    H5X_fastbit_list_t *nextEntry = H5X_fastbit_current_datasets;
    while(nextEntry) {
        if (nextEntry->dataset_id == dataset_id) return nextEntry->fastbit_info;
	nextEntry = nextEntry->next;
    }
    return NULL;
}


static void
dump_as_bytes(char *entry, hsize_t length, void *data)
{
  static int mpi_rank = -1;
  char filename[80];
  FILE *save_dump;
  unsigned char *show = (char *)data;
  int i, k;
  int rows = (int)(length/10);
  int lastrow = (int)(length % 10);
  if (mpi_rank < 0) mpi_rank = H5Xparallel_rank();
  sprintf(filename, "%s-dump-%d.txt", entry, mpi_rank);
  save_dump = fopen(filename, "a+");
  
  fprintf(save_dump, "-----------\n\n%s length is %lld bytes::\n",entry, length);
  for (i=0; i< rows; i++) {
    fprintf(save_dump,"%4d\t%02x %02x %02x %02x %02x %02x %02x %02x %02x %02x\n", i, show[0],
	   show[1],show[2],show[3],show[4],show[5],show[6],show[7],show[8],show[9]);
    show += 10;
  }
  if (lastrow) {
    fprintf(save_dump,"%4d\t%02x", rows, show[0]);
    for(i=1; i< lastrow; i++)
      fprintf(save_dump," %02x", show[i]);
    fprintf(save_dump,"\n");
  }
  fclose(save_dump);
}


/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_init
 *
 * Purpose: Configure and set up and the FastBit encoder.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static H5X_fastbit_t *
H5X__fastbit_init(hid_t dataset_id)
{
    H5X_fastbit_t *fastbit = NULL;
    hid_t space_id = FAIL;
    hid_t file_id = FAIL;
    char dataset_name[H5X_FASTBIT_MAX_NAME];
    H5X_fastbit_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (fastbit = (H5X_fastbit_t *) H5MM_malloc(sizeof(H5X_fastbit_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate FastBit struct");
    fastbit->private_metadata = NULL;

    if ((file_id = H5Iget_file_id(dataset_id)) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get file ID from dataset");

    fastbit->dataset_id = dataset_id;
    fastbit->file_id = file_id;

    fastbit->keys_id = FAIL;
    fastbit->nkeys = 0;
    fastbit->keys = NULL;

    fastbit->offsets_id = FAIL;
    fastbit->noffsets = 0;
    fastbit->offsets = NULL;

    fastbit->nbitmaps = 0;
    fastbit->bitmaps = NULL;
    fastbit->bitmaps_id = FAIL;

    if (FAIL == H5Iget_name(dataset_id, dataset_name, H5X_FASTBIT_MAX_NAME))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get dataset name");

    H5X_FASTBIT_LOG_DEBUG("Dataset Name : %s", dataset_name);
    fastbit->dataset_name = strdup(dataset_name);
    sprintf(fastbit->column_name, "array%u", H5X__fastbit_hash_string(dataset_name));

    fastbit->nblocks = 0;
    fastbit->filespace_id = FAIL;
    fastbit->memspace_id = FAIL;
    fastbit->index_info_group_id = FAIL;

    fastbit->idx_reconstructed = FALSE;
    fastbit->idx_multiblock = FALSE;

    /* Initialize FastBit (no config file for now) */
    fastbit_init(NULL);
    fastbit_set_verbose_level(H5X_FASTBIT_DEBUG_LVL);

    /* Get dimensions of dataset */
    if (FAIL == (space_id = H5Dget_space(dataset_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "can't get dataspace from dataset");
    if (0 == (fastbit->dataset_ndims = (unsigned) H5Sget_simple_extent_ndims(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, NULL, "invalid number of dimensions");
    if (NULL == (fastbit->dataset_dims = (hsize_t *) H5MM_malloc(fastbit->dataset_ndims * sizeof(hsize_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, NULL, "can't allocate dim array");
    if (FAIL == H5Sget_simple_extent_dims(space_id, fastbit->dataset_dims, NULL))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, NULL, "can't get dataspace dims");

    /* Useful for coordinate conversion */
    if (NULL == (fastbit->dataset_down_dims = (hsize_t *) H5MM_malloc(fastbit->dataset_ndims * sizeof(hsize_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, NULL, "can't allocate dim array");
    if (FAIL == H5VM_array_down(fastbit->dataset_ndims, fastbit->dataset_dims,
            fastbit->dataset_down_dims))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, NULL, "can't get dataspace down dims");

    /* Create an opaque type to handle FastBit data */
    if (FAIL == (fastbit->opaque_type_id = H5Tcreate(H5T_OPAQUE, 1)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, NULL, "can't create type");
    if (FAIL == H5Tset_tag(fastbit->opaque_type_id, "FastBit metadata type"))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTSET, NULL, "can't set tag to type");

    ret_value = fastbit;

#ifdef H5_HAVE_PARALLEL
    H5X__fastbit_add_to_list(fastbit);
#endif

done:
    if (space_id != FAIL)
        H5Sclose(space_id);
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_init() */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_term
 *
 * Purpose: Free plugin resources.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_term(H5X_fastbit_t *fastbit)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    if (!fastbit)
        HGOTO_DONE(SUCCEED);

    H5MM_free(fastbit->private_metadata);

    if (FAIL == H5Fclose(fastbit->file_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close file ID");

    /* Free dim arrays */
    H5MM_free(fastbit->dataset_dims);
    H5MM_free(fastbit->dataset_down_dims);

    /* Free metadata if created */
    H5MM_free(fastbit->keys);
    H5MM_free(fastbit->offsets);
    H5MM_free(fastbit->bitmaps);

    /* Close opaque type */
    if ((FAIL != fastbit->opaque_type_id) &&
            (FAIL == H5Tclose(fastbit->opaque_type_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close opaque type");

    /* Close anonymous datasets */
    if ((FAIL != fastbit->index_info_group_id) &&
            (FAIL == H5Dclose(fastbit->index_info_group_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");
    if ((FAIL != fastbit->keys_id) &&
            (FAIL == H5Dclose(fastbit->keys_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");
    if ((FAIL != fastbit->offsets_id) &&
            (FAIL == H5Dclose(fastbit->offsets_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");
    if ((FAIL != fastbit->bitmaps_id) &&
            (FAIL == H5Dclose(fastbit->bitmaps_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");

    /* Free array */
    fastbit_iapi_free_array(fastbit->column_name);

    /* Free FastBit resources */
    fastbit_cleanup();

#ifdef H5_HAVE_PARALLEL
    H5X__fastbit_remove_from_list(fastbit->dataset_id);
#endif

    H5MM_free(fastbit);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_term() */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_hash_string
 *
 * Purpose: Hash string to create unique ID to register FastBit array.
 *
 * Return:  ID of hashed string
 *
 *-------------------------------------------------------------------------
 */
static unsigned int
H5X__fastbit_hash_string(const char *string)
{
    /* This is the djb2 string hash function */
    unsigned int ret_value = 5381;
    const unsigned char *p;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    p = (const unsigned char *) string;

    while (*p != '\0') {
        ret_value = (ret_value << 5) + ret_value + *p;
        ++p;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_hash_string */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_convert_type
 *
 * Purpose: Convert H5 type to FastBit type.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static FastBitDataType
H5X__fastbit_convert_type(hid_t type_id)
{
    FastBitDataType ret_value = FastBitDataTypeUnknown; /* Return value */
    hid_t native_type_id = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    if((native_type_id = H5Tget_native_type(type_id, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FastBitDataTypeUnknown, "can't get native type");

    if(H5Tequal(native_type_id, H5T_NATIVE_LLONG_g))
        ret_value = FastBitDataTypeULong;
    else if(H5Tequal(native_type_id, H5T_NATIVE_LONG_g))
        ret_value = FastBitDataTypeLong;
    else if(H5Tequal(native_type_id, H5T_NATIVE_INT_g))
        ret_value = FastBitDataTypeInt;
    else if(H5Tequal(native_type_id, H5T_NATIVE_SHORT_g))
        ret_value = FastBitDataTypeShort;
    else if(H5Tequal(native_type_id, H5T_NATIVE_SCHAR_g))
        ret_value = FastBitDataTypeByte;
    else if(H5Tequal(native_type_id, H5T_NATIVE_DOUBLE_g))
        ret_value = FastBitDataTypeDouble;
    else if(H5Tequal(native_type_id, H5T_NATIVE_FLOAT_g))
        ret_value = FastBitDataTypeFloat;
    else
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FastBitDataTypeUnknown, "unsupported type");

    H5X_FASTBIT_LOG_DEBUG("FastBit converted type: %d\n", ret_value);

done:
    if(FAIL == native_type_id)
        H5Tclose(native_type_id);
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_convert_type */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_read_data
 *
 * Purpose: Read data from dataset.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_read_data(hid_t dataset_id, const char *column_name, void **buf,
        size_t *buf_size)
{
    herr_t ret_value = SUCCEED; /* Return value */
    hid_t type_id = FAIL, space_id = FAIL;
    hid_t mem_space = H5S_ALL;
    hid_t file_space = H5P_DEFAULT;
    hid_t dxpl_id  = H5P_DEFAULT;

    size_t nelmts, elmt_size;
    void *data = NULL;
    size_t data_size;
    FastBitDataType fastbit_type;

    FUNC_ENTER_NOAPI_NOINIT

    if (FAIL == (type_id = H5Dget_type(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get type from dataset");
    if (0 == (elmt_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid size of element");
    if (FAIL == (space_id = H5Dget_space(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from dataset");

#ifdef H5_HAVE_PARALLEL
    if (H5Xparallel_queries_enabled() > 0) {
	int i, ds_ndims = 0;
        hsize_t *start = NULL, *count = NULL, *block = NULL, *stride = NULL;
	H5X_fastbit_t *fastbit = H5X__get_fastbit(dataset_id);
	if (fastbit->filespace_id < 0) fastbit->filespace_id = space_id;
	ds_ndims = H5Xslab_set(fastbit->filespace_id, &start, &count, &stride, &block);
	file_space = fastbit->filespace_id;

        if (H5Sselect_hyperslab(fastbit->filespace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "H5Sselect_hyperslab returned an error");

	if ((mem_space = H5Screate_simple (ds_ndims, block, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "H5Screate_simple returned an error");

	fastbit->memspace_id = mem_space;
	fastbit->filespace_offset = start; /* Save the rank specific file offset */
	nelmts = block[0];
        for (i=1; i< ds_ndims; i++) 
            nelmts *= block[i];

        data_size = nelmts * elmt_size;
	// printf("data_size: %llu = %llu x %llu\n", data_size, nelmts, elmt_size);

        if (NULL == (data = H5MM_malloc(data_size)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate read buffer");

        // printf("H5X__fastbit_read_data: allocated %llu bytes, data=%p\n", data_size, data);

	/* The variable 'start' is the original pointer allocated within H5Xslab_set.
	 * The additional variables: count, block, and stride are all just offsets
	 * from this pointer.  As a result, we only need to free the start pointer.
	 */
	free(start);

        if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "unable to create xfer property");
        if ((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE)) < 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, FAIL, "unable to set MPIO_COLLECTIVE property");
    } 
    else {
        /* Get space info */
        if (0 == (nelmts = (size_t) H5Sget_select_npoints(space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

        /* Allocate buffer to hold data */
        data_size = nelmts * elmt_size;
        if (NULL == (data = H5MM_malloc(data_size)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate read buffer");
    }

#else
    if (0 == (nelmts = (size_t) H5Sget_select_npoints(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

    /* Allocate buffer to hold data */
    data_size = nelmts * elmt_size;
    if (NULL == (data = H5MM_malloc(data_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate read buffer");
#endif      
    /* Read data from dataset */
    if (FAIL == H5Dread(dataset_id, type_id, mem_space, file_space, dxpl_id, data))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

    if (dxpl_id != H5P_DEFAULT)
        H5Pclose(dxpl_id);

    /* Convert type to FastBit type */
    if (FastBitDataTypeUnknown == (fastbit_type = H5X__fastbit_convert_type(type_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCONVERT, FAIL, "can't convert type");

    /* Register array */
    if (0 != fastbit_iapi_register_array(column_name, fastbit_type, data, nelmts))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREGISTER, FAIL, "can't register array");

    H5X_FASTBIT_LOG_DEBUG("Registered array %s", column_name);
    *buf = data;
    *buf_size = data_size;

done:
    /* NOTE: for parallel indexing, we've made a copy of of the mem_space selection
     * Should we create an actual copy and delete the version we created here?
     */
    if (type_id != FAIL)
        H5Tclose(type_id);
    if (space_id != FAIL)
        H5Sclose(space_id);
    if (ret_value == FAIL) {
        H5MM_free(data);
        *buf = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_read_data() */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_build_index
 *
 * Purpose: Create/update an existing index from a dataset.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_build_index(H5X_fastbit_t *fastbit)
{
    static int sequence_id = 0;
    char log_entry[256];

    hid_t key_space_id = FAIL, offset_space_id = FAIL, bitmap_space_id = FAIL;
    hsize_t key_array_size, offset_array_size, bitmap_array_size;
    herr_t ret_value = SUCCEED; /* Return value */

#ifdef H5_HAVE_PARALLEL
    if (H5Xparallel_queries_enabled() > 0)
        return H5X__fastbit_build_parallel_index(fastbit);
#endif

    FUNC_ENTER_NOAPI_NOINIT

    H5X_FASTBIT_LOG_DEBUG("Calling FastBit build index");

    /* Build index */
    if (0 != fastbit_iapi_build_index(fastbit->column_name, (const char *)0))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, FAIL, "FastBit build index failed");

    /* Get arrays from index */
    if (0 != fastbit_iapi_deconstruct_index(fastbit->column_name, &fastbit->keys, &fastbit->nkeys,
            &fastbit->offsets, &fastbit->noffsets, &fastbit->bitmaps, &fastbit->nbitmaps))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "Can't get FastBit index arrays");

    H5X_FASTBIT_LOG_DEBUG("FastBit build index, nkeys=%lu, noffsets=%lu, nbitmaps=%lu\n",
            fastbit->nkeys, fastbit->noffsets, fastbit->nbitmaps);

    /* Create key array with opaque type */
    key_array_size = fastbit->nkeys * sizeof(double);
    if (fastbit->keys_id != FAIL) {
        if (FAIL == H5Odecr_refcount(fastbit->keys_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't decrement dataset refcount");
        if (FAIL == H5Dclose(fastbit->keys_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset");
    }
    if (FAIL == (key_space_id = H5Screate_simple(1, &key_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->keys_id = H5Dcreate_anon(fastbit->file_id, fastbit->opaque_type_id,
            key_space_id, H5P_DEFAULT, H5P_DEFAULT)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
    /* Increment refcount so that anonymous dataset is persistent */
    if (FAIL == H5Oincr_refcount(fastbit->keys_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");

    /* Create offset array with opaque type */
    offset_array_size = fastbit->noffsets * sizeof(int64_t);
    if (fastbit->offsets_id != FAIL) {
        if (FAIL == H5Odecr_refcount(fastbit->offsets_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't decrement dataset refcount");
        if (FAIL == H5Dclose(fastbit->offsets_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset");
    }
    if (FAIL == (offset_space_id = H5Screate_simple(1, &offset_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->offsets_id = H5Dcreate_anon(fastbit->file_id, fastbit->opaque_type_id,
            offset_space_id, H5P_DEFAULT, H5P_DEFAULT)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
    /* Increment refcount so that anonymous dataset is persistent */
    if (FAIL == H5Oincr_refcount(fastbit->offsets_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");

    /* Create bitmap array with opaque type */
    bitmap_array_size = fastbit->nbitmaps * sizeof(uint32_t);
    if (fastbit->bitmaps_id != FAIL) {
        if (FAIL == H5Odecr_refcount(fastbit->bitmaps_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't decrement dataset refcount");
        if (FAIL == H5Dclose(fastbit->bitmaps_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset");
    }
    if (FAIL == (bitmap_space_id = H5Screate_simple(1, &bitmap_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->bitmaps_id = H5Dcreate_anon(fastbit->file_id, fastbit->opaque_type_id,
            bitmap_space_id, H5P_DEFAULT, H5P_DEFAULT)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
    /* Increment refcount so that anonymous dataset is persistent */
    if (FAIL == H5Oincr_refcount(fastbit->bitmaps_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");

    /* Write keys */
    if (FAIL == H5Dwrite(fastbit->keys_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->keys))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    /* Write offsets */
    if (FAIL == H5Dwrite(fastbit->offsets_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->offsets))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    /* Write bitmaps */
    if (FAIL == H5Dwrite(fastbit->bitmaps_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->bitmaps))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    if (sequence_id == -1) {	/* Disabled for now */
      key_array_size = fastbit->nkeys * sizeof(double);
      offset_array_size = fastbit->noffsets * sizeof(int64_t);
      bitmap_array_size = fastbit->nbitmaps * sizeof(uint32_t);

      sprintf(log_entry, "seq-keys-save-%s", fastbit->column_name);
      dump_as_bytes(log_entry, key_array_size, fastbit->keys);

      sprintf(log_entry, "seq-offsets-save-%s", fastbit->column_name);
      dump_as_bytes(log_entry, offset_array_size, fastbit->offsets);

      sprintf(log_entry, "seq-bitmaps-save-%s", fastbit->column_name);
      dump_as_bytes(log_entry, bitmap_array_size, fastbit->bitmaps);

      sequence_id++;
    }


done:
    if (key_space_id != FAIL)
        H5Sclose(key_space_id);
    if (offset_space_id != FAIL)
        H5Sclose(offset_space_id);
    if (bitmap_space_id != FAIL)
        H5Sclose(bitmap_space_id);
    if (err_occurred) {
        H5MM_free(fastbit->keys);
        fastbit->keys = NULL;
        H5MM_free(fastbit->offsets);
        fastbit->offsets = NULL;
        H5MM_free(fastbit->bitmaps);
        fastbit->bitmaps = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_build_index */


#ifdef H5_HAVE_PARALLEL
#define KEYS_ 0
#define OFFSETS_ 1
#define BITMAPS_ 2
#define INFO_ 3
#define BADINDEX_ 4

static herr_t
H5X__fastbit_define_dataset(H5X_fastbit_t *fastbit, int typeIndex, struct fastbit_meta_collection *gatherInfo,
			    hsize_t *array_size, hsize_t *offset, hid_t *space_id, hid_t *memspace_id)
{
    herr_t ret_value = SUCCEED; /* Return value */
    hid_t _space_id = FAIL, _memspace_id = H5S_ALL;;
    int mpi_rank = H5Xparallel_rank();
    hsize_t count;

    FUNC_ENTER_NOAPI_NOINIT

    if ((typeIndex < 0) || (typeIndex >= BADINDEX_)) 
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "Bad data selection index");
    if (FAIL == (_space_id = H5Screate_simple(1, array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple (file) dataspace");

    if (typeIndex == KEYS_) {
        count = (hsize_t)gatherInfo[mpi_rank].nkeys;
        if (FAIL == (_memspace_id = H5Screate_simple(1, &count, NULL)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple (mem) dataspace");
	if (H5Sselect_all(_memspace_id) < 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "invalid memory space selection");
        if ((H5Sselect_hyperslab(_space_id, H5S_SELECT_SET, offset, NULL, &count, NULL)) < 0 )
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");
        if (FAIL == (fastbit->keys_id = H5Dcreate_anon(fastbit->file_id, fastbit->opaque_type_id,
                    _space_id, H5P_DEFAULT, H5P_DEFAULT)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
        /* Increment refcount so that anonymous dataset is persistent */
        if (FAIL == H5Oincr_refcount(fastbit->keys_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");
    }

    else if (typeIndex == OFFSETS_) {
        count = (hsize_t)gatherInfo[mpi_rank].noffsets;
        if (FAIL == (_memspace_id = H5Screate_simple(1, &count, NULL)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple (mem) dataspace");
	if (H5Sselect_all(_memspace_id) < 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "invalid memory space selection");
        if ((H5Sselect_hyperslab(_space_id, H5S_SELECT_SET, offset, NULL, &count, NULL)) < 0 )
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");
        if (FAIL == (fastbit->offsets_id = H5Dcreate_anon(fastbit->file_id, fastbit->opaque_type_id,
                _space_id, H5P_DEFAULT, H5P_DEFAULT)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
        /* Increment refcount so that anonymous dataset is persistent */
        if (FAIL == H5Oincr_refcount(fastbit->offsets_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");
    }

    else if (typeIndex == BITMAPS_) {
        count = (hsize_t)gatherInfo[mpi_rank].nbitmaps;
        if (FAIL == (_memspace_id = H5Screate_simple(1, &count, NULL)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple (mem) dataspace");
	if (H5Sselect_all(_memspace_id) < 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "invalid memory space selection");
        if ((H5Sselect_hyperslab(_space_id, H5S_SELECT_SET, offset, NULL, &count, NULL)) < 0 )
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");
        if (FAIL == (fastbit->bitmaps_id = H5Dcreate_anon(fastbit->file_id, fastbit->opaque_type_id,
                _space_id, H5P_DEFAULT, H5P_DEFAULT)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
        /* Increment refcount so that anonymous dataset is persistent */
        if (FAIL == H5Oincr_refcount(fastbit->bitmaps_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");
    }
    else if (typeIndex == INFO_) {
        if (FAIL == (_memspace_id = H5Screate_simple(1, array_size, NULL)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple (mem) dataspace");
	if (H5Sselect_all(_memspace_id) < 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "invalid memory space selection");
        if ((H5Sselect_hyperslab(_space_id, H5S_SELECT_SET, offset, NULL, array_size, NULL)) < 0 )
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");
        if (FAIL == (fastbit->index_info_group_id = H5Dcreate_anon(fastbit->file_id, fastbit->opaque_type_id,
                _space_id, H5P_DEFAULT, H5P_DEFAULT)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
        /* Increment refcount so that anonymous dataset is persistent */
        if (FAIL == H5Oincr_refcount(fastbit->index_info_group_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");
    }

    /* Save the outputs for use in writing the actual dataset! */
    *space_id = _space_id;
    *memspace_id = _memspace_id;

 done:

    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_build_parallel_index
 *
 * Purpose: Same as H5X__fastbit_build_index, but parallel.
 *          CALLED ONLY FROM THE SERIAL CODE 
 *          (when H5Xparallel_queries_enabled returns TRUE)
 *          This is an attempt to modularize the code and
 *          to make the mode more readable.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_build_parallel_index(H5X_fastbit_t *fastbit)
{
    static int sequence_id = 0;
    char log_entry[256];

    hid_t info_space_id = FAIL;
    hid_t key_memspace_id = H5S_ALL;
    hid_t offset_memspace_id = H5S_ALL;
    hid_t bitmap_memspace_id = H5S_ALL;

    hid_t keys_filespace_id = FAIL, offsets_filespace_id = FAIL, bitmaps_filespace_id = FAIL;
    hid_t keys_memspace_id = FAIL, offsets_memspace_id = FAIL, bitmaps_memspace_id = FAIL;

    hsize_t key_array_size, offset_array_size, bitmap_array_size;
    herr_t ret_value = SUCCEED; /* Return value */
    hid_t dxpl_id = H5P_DEFAULT;
    int i, mpi_rank, mpi_size;
    hsize_t filespace_offset = 0;
    hsize_t nkeys_totalsize = 0, nkeys_offset = 0;
    hsize_t noffsets_totalsize = 0, noffsets_offset = 0;
    hsize_t nbitmaps_totalsize = 0, nbitmaps_offset = 0;
    hsize_t indexinfo_totalsize = 0, indexinfo_offset = 0;
    struct fastbit_meta_collection *gatherInfo = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    H5X_FASTBIT_LOG_DEBUG("Calling FastBit build parallel index");
    /* Build our process local index */
    if (0 != fastbit_iapi_build_index(fastbit->column_name, (const char *)0))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, FAIL, "FastBit build index failed");

    /* Get process local arrays from index */
    if (0 != fastbit_iapi_deconstruct_index(fastbit->column_name, &fastbit->keys, &fastbit->nkeys,
            &fastbit->offsets, &fastbit->noffsets, &fastbit->bitmaps, &fastbit->nbitmaps))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "Can't get FastBit index arrays");

    H5X_FASTBIT_LOG_DEBUG("FastBit build parallel index, nkeys=%lu, noffsets=%lu, nbitmaps=%lu\n",
            fastbit->nkeys, fastbit->noffsets, fastbit->nbitmaps);

    mpi_size = H5Xparallel_size();
    mpi_rank = H5Xparallel_rank();

    /* We need storage for 3 elements per rank:: {nkeys, noffsets, n_bitmaps}
     * 
     *    +-------------+
     *    | (RANK 0)    |
     *    | nkeys       |
     *    | noffsets    |
     *    | nbitmaps    |
     *    +-------------+
     *    |             |
     *    ~             ~
     *    +-------------+
     *    | (RANK N)    |
     *    | nkeys       |
     *    | noffsets    |
     *    | nbitmaps    |
     *    +-------------+
     *    
     */
    /* Allocate storage for 4 elements per rank */
    indexinfo_totalsize = (hsize_t)(sizeof(struct fastbit_meta_collection) * (hsize_t)(mpi_size));
    if (NULL == (gatherInfo = (struct fastbit_meta_collection *)H5MM_malloc(indexinfo_totalsize)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate nkeys array");

    gatherInfo[mpi_rank].data_length = (hsize_t)H5Sget_select_npoints(fastbit->memspace_id);
    gatherInfo[mpi_rank].nkeys    = (hsize_t)(fastbit->nkeys);
    gatherInfo[mpi_rank].noffsets = (hsize_t)(fastbit->noffsets);
    gatherInfo[mpi_rank].nbitmaps = (hsize_t)(fastbit->nbitmaps);

    /* Exchange info with all MPI ranks */
    /* We could use allreduce here if we only needed the total sizes
     * but we also want to figure out the offsets (for the hyperslab selection).
     */
    if (H5Xallgather_by_size(gatherInfo, 4, sizeof(int64_t)) != SUCCEED)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't gather nkeys array");

    /* Gather the total sizes and offset info */
    for(i=0; i < mpi_size; i++) {
        if (i == mpi_rank) {
           nkeys_offset = nkeys_totalsize;
           noffsets_offset = noffsets_totalsize;
           nbitmaps_offset = nbitmaps_totalsize;
           fastbit->filespace_offset = filespace_offset;           
        }
        nkeys_totalsize += gatherInfo[i].nkeys+1;
        noffsets_totalsize += gatherInfo[i].noffsets+1;
        nbitmaps_totalsize += gatherInfo[i].nbitmaps+1;
	filespace_offset += gatherInfo[i].data_length;
    }

    /* Save the exchanged info for future use */
    fastbit->nblocks = mpi_size;
    fastbit->idx_multiblock = TRUE;
    fastbit->nelmts = gatherInfo[mpi_rank].data_length;

    /* Create a transfer property to utilize MPI-IO */
    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0 )
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't define dataset_xfer property");
    if ((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE)) < 0 )
       HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't set dataset_xfer to MPIO_COLLECTIVE");

    /* -------------- */
    /*  INFOGROUP     */
    /* -------------- */       

    /* Filespace descriptors */
    indexinfo_totalsize  /= sizeof(int64_t); /* Previously, was the malloc size in bytes */
                                             /* Fixed to be the total number of elements */
    info_space_id        = H5Screate_simple(1, &indexinfo_totalsize, NULL);
    keys_filespace_id    = H5Screate_simple(1, &nkeys_totalsize, NULL);
    offsets_filespace_id = H5Screate_simple(1, &noffsets_totalsize, NULL);
    bitmaps_filespace_id = H5Screate_simple(1, &nbitmaps_totalsize, NULL);
    
    /* Memspace descriptors */
    keys_memspace_id     = H5Screate_simple(1, &gatherInfo[mpi_rank].nkeys, NULL);
    offsets_memspace_id  = H5Screate_simple(1, &gatherInfo[mpi_rank].noffsets, NULL);
    bitmaps_memspace_id  = H5Screate_simple(1, &gatherInfo[mpi_rank].nbitmaps, NULL);

    if (H5Sselect_all(keys_memspace_id) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "invalid memory space selection");
    if (H5Sselect_all(offsets_memspace_id) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "invalid memory space selection");
    if (H5Sselect_all(bitmaps_memspace_id) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "invalid memory space selection");

    /* Hyperslab selections */
    if ((H5Sselect_hyperslab(keys_filespace_id, H5S_SELECT_SET, (hsize_t *)(&nkeys_offset), NULL,
			     (hsize_t *)(&gatherInfo[mpi_rank].nkeys), NULL)) < 0 )
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");
    if ((H5Sselect_hyperslab(offsets_filespace_id, H5S_SELECT_SET, (hsize_t *)(&noffsets_offset), NULL,
			     (hsize_t *)(&gatherInfo[mpi_rank].noffsets), NULL)) < 0 )
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");
    if ((H5Sselect_hyperslab(bitmaps_filespace_id, H5S_SELECT_SET, &nbitmaps_offset, NULL,
			     &gatherInfo[mpi_rank].nbitmaps, NULL)) < 0 )
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");

    /* --------------------- */
    /* Close open datasets   */
    /* --------------------- */
    if (fastbit->index_info_group_id != FAIL) {
        if (FAIL == H5Odecr_refcount(fastbit->index_info_group_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't decrement dataset refcount");
        if (FAIL == H5Dclose(fastbit->index_info_group_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset");
    }
    if (fastbit->keys_id != FAIL) {
        if (FAIL == H5Odecr_refcount(fastbit->keys_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't decrement dataset refcount");
        if (FAIL == H5Dclose(fastbit->keys_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset");
    }
    if (fastbit->offsets_id != FAIL) {
        if (FAIL == H5Odecr_refcount(fastbit->offsets_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't decrement dataset refcount");
        if (FAIL == H5Dclose(fastbit->offsets_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset");
    }
    if (fastbit->bitmaps_id != FAIL) {
        if (FAIL == H5Odecr_refcount(fastbit->bitmaps_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't decrement dataset refcount");
        if (FAIL == H5Dclose(fastbit->bitmaps_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset");
    }
    /* ---------------------- */
    /* New Anonymous datasets */
    /* ---------------------- */

    /* info */
    if (FAIL == (fastbit->index_info_group_id = H5Dcreate_anon(fastbit->file_id, H5T_NATIVE_LLONG, info_space_id, H5P_DEFAULT, H5P_DEFAULT)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
    if (FAIL == H5Oincr_refcount(fastbit->index_info_group_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");

    /* keys */
    if (FAIL == (fastbit->keys_id = H5Dcreate_anon(fastbit->file_id, H5T_NATIVE_DOUBLE, keys_filespace_id, H5P_DEFAULT, H5P_DEFAULT)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");
    if (FAIL == H5Oincr_refcount(fastbit->keys_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");

    /* offsets */
    if (FAIL == (fastbit->offsets_id = H5Dcreate_anon(fastbit->file_id, H5T_NATIVE_LLONG, offsets_filespace_id, H5P_DEFAULT, H5P_DEFAULT)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");    
    if (FAIL == H5Oincr_refcount(fastbit->offsets_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");

    /* bitmaps */
    if (FAIL == (fastbit->bitmaps_id = H5Dcreate_anon(fastbit->file_id, H5T_NATIVE_UINT, bitmaps_filespace_id, H5P_DEFAULT, H5P_DEFAULT)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");    
    if (FAIL == H5Oincr_refcount(fastbit->bitmaps_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");

    /* ---------------------- */
    /* Write data to datasets */
    /* ---------------------- */

    /* Write parallel info */
    if (FAIL == H5Dwrite(fastbit->index_info_group_id, H5T_NATIVE_LLONG, H5S_ALL,
            H5S_ALL, dxpl_id, gatherInfo))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    /* Write keys */
    if (FAIL == H5Dwrite(fastbit->keys_id, H5T_NATIVE_DOUBLE, keys_memspace_id,
			 keys_filespace_id, dxpl_id, fastbit->keys))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");
    /* Write offsets */
    if (FAIL == H5Dwrite(fastbit->offsets_id, H5T_NATIVE_LLONG, offsets_memspace_id,
			 offsets_filespace_id, dxpl_id, fastbit->offsets))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    /* Write bitmaps */
    if (FAIL == H5Dwrite(fastbit->bitmaps_id, H5T_NATIVE_UINT, bitmaps_memspace_id,
			 bitmaps_filespace_id, dxpl_id, fastbit->bitmaps))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");


    if (sequence_id == -1) {	/* Disabled for now */
      key_array_size = gatherInfo[mpi_rank].nkeys * sizeof(double);
      sprintf(log_entry, "%d-keys-save-%s", sequence_id, fastbit->column_name);
      dump_as_bytes(log_entry, key_array_size, fastbit->keys);

      offset_array_size = gatherInfo[mpi_rank].noffsets * sizeof(int64_t);
      sprintf(log_entry, "%d-offsets-save-%s", sequence_id, fastbit->column_name);
      dump_as_bytes(log_entry, offset_array_size, fastbit->offsets);

      bitmap_array_size = gatherInfo[mpi_rank].nbitmaps * sizeof(uint32_t);
      sprintf(log_entry, "%d-bitmaps-save-%s", sequence_id,  fastbit->column_name);
      dump_as_bytes(log_entry, bitmap_array_size, fastbit->bitmaps);

      sequence_id++;
    }
done:
    if (info_space_id != FAIL)
        H5Sclose(info_space_id);      

    if (keys_filespace_id != FAIL)
        H5Sclose(keys_filespace_id);
    if (offsets_filespace_id != FAIL)
        H5Sclose(offsets_filespace_id);
    if (bitmaps_filespace_id != FAIL)
        H5Sclose(bitmaps_filespace_id);

    if (keys_memspace_id != FAIL)
        H5Sclose(keys_memspace_id);
    if (offsets_memspace_id != FAIL)
        H5Sclose(offsets_memspace_id);
    if (bitmaps_memspace_id != FAIL)
        H5Sclose(bitmaps_memspace_id);

    if (dxpl_id != H5P_DEFAULT)
        H5Pclose(dxpl_id);
    if (err_occurred) {
        H5MM_free(gatherInfo);
        H5MM_free(fastbit->keys);
        fastbit->keys = NULL;
        H5MM_free(fastbit->offsets);
        fastbit->offsets = NULL;
        H5MM_free(fastbit->bitmaps);
        fastbit->bitmaps = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_build_index */

#endif	/* H5_HAVE_PARALLEL */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_merge_data
 *
 * Purpose: Merge buffers.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_scatter_cb(const void **src_buf/*out*/, size_t *src_buf_bytes_used/*out*/,
        void *_info)
{
    struct H5X_fastbit_scatter_info *info = (struct H5X_fastbit_scatter_info *) _info;
    /* Set output variables */
    *src_buf = info->src_buf;
    *src_buf_bytes_used = info->src_buf_size;

    return SUCCEED;
}

static herr_t
H5X__fastbit_merge_data(H5X_fastbit_t *fastbit, const void *data,
        hid_t dataspace_id, void *buf)
{
    hid_t type_id = FAIL, space_id = FAIL;
    struct H5X_fastbit_scatter_info info;
    size_t nelmts_data, data_elmt_size;

    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    //    void *dest_data = H5MM_malloc(buf_size);
    //    if (FAIL == H5Dgather(space_id, data, type_id, buf_size, dest_data, NULL, NULL))
    //        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "cannot gather data");

    if (FAIL == (type_id = H5Dget_type(fastbit->dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get type from dataset");
    if (0 == (data_elmt_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid size of element");

#ifdef H5_HAVE_PARALLEL
    /* For parallel operations, the dataspace_id being provided is one which
     * the defines the local view of the on-disk file, i.e. a hyperslab with
     * MPI rank specific offsets.  Here, we've already
     * written data to the file and need to merge the local data with what
     * what previously there.  We're merging these buffers in memory
     * so we don't want to deal with the offsets that were needed to write the
     * local slice onto disk.
     */
    if (H5Xparallel_queries_enabled() > 0) {
        if (fastbit->memspace_id != FAIL) 
           dataspace_id = fastbit->memspace_id;
        nelmts_data = (size_t) H5Sget_select_npoints(dataspace_id);
	// printf("select_npoints(fastbit->filespace): nelmts = %llu, data_elmt_size = %llu\n", nelmts_data, data_elmt_size);

        info.src_buf = data;
        info.src_buf_size = nelmts_data * data_elmt_size;
        if (FAIL == H5Dscatter(H5X__fastbit_scatter_cb, &info, type_id, dataspace_id, buf))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "cannot scatter data");

    }
    else
#else
    {
        if (0 == (nelmts_data = (size_t) H5Sget_select_npoints(dataspace_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

        info.src_buf = data;
        info.src_buf_size = nelmts_data * data_elmt_size;
        if (FAIL == H5Dscatter(H5X__fastbit_scatter_cb, &info, type_id, dataspace_id, buf))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "cannot scatter data");
    }
#endif

done:
    if (type_id != FAIL)
        H5Tclose(type_id);
    if (space_id != FAIL)
        H5Sclose(space_id);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_serialize_metadata
 *
 * Purpose: Serialize index plugin metadata into buffer.
 * NB. This is not FastBit metadata but only H5X plugin private metadata.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_serialize_metadata(H5X_fastbit_t *fastbit, void *buf, size_t *buf_size)
{
    /* The implementation records 3 values (for the anonymous datasets)
     *  {keys_id, offsets_id, bitmaps_id}
     * For parallel indexing/querying, we add an additional dataset (index_info_group).
     */
    int extra_info = 0;
    size_t metadata_size = 3 * sizeof(int64_t);
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5_HAVE_PARALLEL
    if (H5Xparallel_queries_enabled() > 0) {
        extra_info = 1;
        metadata_size += sizeof(int64_t);
      }
#endif
    if (buf) {
        H5O_info_t dset_info;
        char *buf_ptr = (char *) buf;

        dset_info.addr = 0;
	/* Maybe encode info_group */
	if (extra_info) {
	    if ((fastbit->index_info_group_id != (hid_t)FAIL) && (FAIL == H5Oget_info(fastbit->index_info_group_id, &dset_info)))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get info for anonymous dataset");
            HDmemcpy(buf_ptr, &dset_info.addr, sizeof(haddr_t));
            buf_ptr += sizeof(haddr_t);
	}

        /* Encode keys info */
        if ((fastbit->keys_id != (hid_t)FAIL) && (FAIL == H5Oget_info(fastbit->keys_id, &dset_info)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get info for anonymous dataset");
        HDmemcpy(buf_ptr, &dset_info.addr, sizeof(haddr_t));
        buf_ptr += sizeof(haddr_t);

        /* Encode offsets info */
        if ((fastbit->offsets_id != (hid_t)FAIL) && (FAIL == H5Oget_info(fastbit->offsets_id, &dset_info)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get info for anonymous dataset");
        HDmemcpy(buf_ptr, &dset_info.addr, sizeof(haddr_t));
        buf_ptr += sizeof(haddr_t);

        /* Encode bitmaps info */
        if ((fastbit->bitmaps_id != (hid_t)FAIL) && (FAIL == H5Oget_info(fastbit->bitmaps_id, &dset_info)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get info for anonymous dataset");
        HDmemcpy(buf_ptr, &dset_info.addr, sizeof(haddr_t));
        buf_ptr += sizeof(haddr_t);
    }

    if (buf_size) *buf_size = metadata_size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_deserialize_metadata
 *
 * Purpose: Deserialize index plugin metadata from buffer.
 * NB. This is not FastBit metadata but only H5X plugin private metadata.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_deserialize_metadata(H5X_fastbit_t *fastbit, void *buf)
{
    haddr_t addr;
    char *buf_ptr = (char *) buf;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5_HAVE_PARALLEL
      if (H5Xparallel_queries_enabled() > 0) {
        addr = *((haddr_t *) buf_ptr);
        if (addr && (FAIL == (fastbit->index_info_group_id = H5Oopen_by_addr(fastbit->file_id, *((haddr_t *) buf_ptr)))))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
        buf_ptr += sizeof(haddr_t);        
    }
#endif

    /* Decode keys info */
    addr = *((haddr_t *) buf_ptr);
    if (addr && (FAIL == (fastbit->keys_id = H5Oopen_by_addr(fastbit->file_id, addr))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += sizeof(haddr_t);

    /* Decode offsets info */
    addr = *((haddr_t *) buf_ptr);
    if (addr && (FAIL == (fastbit->offsets_id = H5Oopen_by_addr(fastbit->file_id, *((haddr_t *) buf_ptr)))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += sizeof(haddr_t);

    /* Decode bitmaps info */
    addr = *((haddr_t *) buf_ptr);
    if (addr && (FAIL == (fastbit->bitmaps_id = H5Oopen_by_addr(fastbit->file_id, *((haddr_t *) buf_ptr)))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += sizeof(haddr_t);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_reconstruct_index
 *
 * Purpose: Reconstruct FastBit index
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static int
H5X__fastbit_read_bitmaps(void *context, uint64_t start, uint64_t count,
        uint32_t *data)
{
    unsigned j;
    const uint32_t *bitmaps = (uint32_t *) context + start;

    for (j = 0; j < count; ++ j)
        data[j] = bitmaps[j];

    return 0;
}

static herr_t
H5X__fastbit_reconstruct_parallel_index(H5X_fastbit_t *fastbit)
{
    static int sequence_id = 0;
    char log_entry[256];

    hid_t dxpl_id = H5P_DEFAULT;
    hid_t keys_space_id = FAIL, offsets_space_id = FAIL, bitmaps_space_id = FAIL;
    hid_t keys_filespace_id = FAIL, offsets_filespace_id = FAIL, bitmaps_filespace_id = FAIL;
    hid_t keys_memspace_id = FAIL, offsets_memspace_id = FAIL, bitmaps_memspace_id = FAIL;

    size_t key_array_size, offset_array_size, bitmap_array_size;
    FastBitDataType fastbit_type;
    hid_t type_id = FAIL, space_id = FAIL;
    size_t nelmts, memelmts;

    herr_t ret_value = SUCCEED; /* Return value */
    hsize_t filespace_offset = 0;
    hsize_t nkeys_totalsize = 0, nkeys_offset = 0;
    hsize_t noffsets_totalsize = 0, noffsets_offset = 0;
    hsize_t nbitmaps_totalsize = 0, nbitmaps_offset = 0;

    struct fastbit_meta_collection *gatherInfo = NULL;
    hid_t info_space_id = FAIL;
    size_t info_array_size;
    size_t i, nranks;
    int mpi_size = H5Xparallel_size();
    int mpi_rank = H5Xparallel_rank();

    FUNC_ENTER_NOAPI_NOINIT

    if ((fastbit->keys == NULL) ||
	(fastbit->offsets == NULL) ||
	(fastbit->bitmaps == NULL)) {


        /* Create a transfer property to utilize MPI-IO */
        if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0 )
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't define dataset_xfer property");
        if ((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE)) < 0 )
           HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't set dataset_xfer to MPIO_COLLECTIVE");

        /* Read the spaceinfo from the anonymous dataset.
         * We will compare the computed total size per private store size
         * to ensure that things are ok to proceed.
         */
        if (FAIL == (info_space_id = H5Dget_space(fastbit->index_info_group_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
        /* Note that the npoints is NOT referenced as the number of bytes...
         * It refers to the number of elements that the dataset was written with.
         * In our case, we wrote the data as H5T_NATIVE_LLONG, so we'll get the number
         * of bytes for our calculation of nranks.
         */
        if (0 == (info_array_size = (size_t) H5Sget_select_npoints(info_space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
        if (NULL == (gatherInfo = (struct fastbit_meta_collection *) H5MM_malloc(info_array_size * sizeof(int64_t))))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate gatherInfo");
        if (FAIL == H5Dread(fastbit->index_info_group_id, H5T_NATIVE_LLONG,
                            H5S_ALL, info_space_id, dxpl_id, gatherInfo))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read gatherInfo data");

        /* The gatherInfo array is N groups of 3 int64_t values
         * It should be a simple thing to divide the total dataset size by
         * the size of a single entry to compute the number of rank entries
         * stored.  This needs to match the current MPI size.
         */
        nranks = (info_array_size * sizeof(int64_t)) / sizeof(struct fastbit_meta_collection);

        for(i=0; i< nranks; i++) {
            if (i == mpi_rank) {
                nkeys_offset = nkeys_totalsize;
                noffsets_offset = noffsets_totalsize;
                nbitmaps_offset = nbitmaps_totalsize;
                fastbit->nelmts = (int64_t)(gatherInfo[i].data_length);
		fastbit->filespace_offset = filespace_offset;
            }
            nkeys_totalsize += gatherInfo[i].nkeys +1;
            noffsets_totalsize += gatherInfo[i].noffsets +1;
            nbitmaps_totalsize += gatherInfo[i].nbitmaps +1;
	    filespace_offset += gatherInfo[i].data_length;
        }

	fastbit->nblocks = mpi_size;
	fastbit->idx_multiblock = TRUE;
        fastbit->nelmts = gatherInfo[mpi_rank].data_length;

        /* Prepare to read nkeys */
        if (FAIL == (keys_space_id = H5Dget_space(fastbit->keys_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
        if (0 == (key_array_size = (size_t) H5Sget_select_npoints(keys_space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

        HDassert(nkeys_totalsize == key_array_size);
        fastbit->nkeys = gatherInfo[mpi_rank].nkeys;

        /* Prepare to read noffsets */
        if (FAIL == (offsets_space_id = H5Dget_space(fastbit->offsets_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
        if (0 == (offset_array_size = (size_t) H5Sget_select_npoints(offsets_space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

        HDassert(noffsets_totalsize == offset_array_size);
        fastbit->noffsets = gatherInfo[mpi_rank].noffsets;

        /* Prepare to read nbitmaps */
        if (FAIL == (bitmaps_space_id = H5Dget_space(fastbit->bitmaps_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
        if (0 == (bitmap_array_size = (size_t) H5Sget_select_npoints(bitmaps_space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

        HDassert(nbitmaps_totalsize == bitmap_array_size);
        fastbit->nbitmaps = gatherInfo[mpi_rank].nbitmaps;

        if (NULL == (fastbit->keys = (double *) H5MM_malloc(gatherInfo[mpi_rank].nkeys * sizeof(double))))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate keys");
        if (NULL == (fastbit->offsets = (int64_t *) H5MM_malloc(gatherInfo[mpi_rank].noffsets * sizeof(int64_t))))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate offsets");
        if (NULL == (fastbit->bitmaps = (uint32_t *) H5MM_malloc(gatherInfo[mpi_rank].nbitmaps * sizeof(uint32_t))))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate offsets");

        /* define the hyperslab selection(s) */
        if((keys_filespace_id = H5Screate_simple(1, &nkeys_totalsize, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "H5Screate_simple returned an error");
        if((offsets_filespace_id = H5Screate_simple(1, &noffsets_totalsize, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "H5Screate_simple returned an error");      
        if((bitmaps_filespace_id = H5Screate_simple(1, &nbitmaps_totalsize, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "H5Screate_simple returned an error");      
    
        /* Memspace descriptors */
        if((keys_memspace_id = H5Screate_simple(1, &gatherInfo[mpi_rank].nkeys, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "H5Screate_simple returned an error");      
        if((offsets_memspace_id = H5Screate_simple(1, &gatherInfo[mpi_rank].noffsets, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "H5Screate_simple returned an error");      
        if((bitmaps_memspace_id = H5Screate_simple(1, &gatherInfo[mpi_rank].nbitmaps, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "H5Screate_simple returned an error");      

        if (H5Sselect_all(keys_memspace_id) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "invalid memory space selection");
        if (H5Sselect_all(offsets_memspace_id) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "invalid memory space selection");
        if (H5Sselect_all(bitmaps_memspace_id) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "invalid memory space selection");


        if ((H5Sselect_hyperslab(keys_filespace_id, H5S_SELECT_SET, &nkeys_offset, NULL, &gatherInfo[mpi_rank].nkeys, NULL)) < 0 )
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");
        if ((H5Sselect_hyperslab(offsets_filespace_id, H5S_SELECT_SET, &noffsets_offset, NULL, &gatherInfo[mpi_rank].noffsets, NULL)) < 0 )
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");
        if ((H5Sselect_hyperslab(bitmaps_filespace_id, H5S_SELECT_SET, &nbitmaps_offset, NULL, &gatherInfo[mpi_rank].nbitmaps, NULL)) < 0 )
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't select hyperslab offsets");

        /* Read keys */
        if (FAIL == H5Dread(fastbit->keys_id, H5T_NATIVE_DOUBLE, keys_memspace_id,
    			keys_filespace_id, dxpl_id, fastbit->keys))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read index keys metadata");

        /* Read offsets */
        if (FAIL == H5Dread(fastbit->offsets_id, H5T_NATIVE_LLONG, offsets_memspace_id,
    			offsets_filespace_id, dxpl_id, fastbit->offsets))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read index offsets metadata");

        /* Read bitmaps */
        if (FAIL == H5Dread(fastbit->bitmaps_id, H5T_NATIVE_UINT, bitmaps_memspace_id,
    			bitmaps_filespace_id, dxpl_id, fastbit->bitmaps))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read index bitmaps metadata");

        if (sequence_id == -1) {	/* Disabled for now */
          key_array_size = gatherInfo[mpi_rank].nkeys * sizeof(double);
          sprintf(log_entry, "%d-keys-read-%s", sequence_id, fastbit->column_name);
          dump_as_bytes(log_entry, key_array_size, fastbit->keys);

          offset_array_size = gatherInfo[mpi_rank].noffsets * sizeof(int64_t);
          sprintf(log_entry, "%d-offsets-read-%s", sequence_id, fastbit->column_name);
          dump_as_bytes(log_entry, offset_array_size, fastbit->offsets);
    
          bitmap_array_size = gatherInfo[mpi_rank].nbitmaps * sizeof(uint32_t);
          sprintf(log_entry, "%d-bitmaps-read-%s", sequence_id,  fastbit->column_name);
          dump_as_bytes(log_entry, bitmap_array_size, fastbit->bitmaps);

          sequence_id++;
        }


        /* Reconstruct index */
        H5X_FASTBIT_LOG_DEBUG("Reconstructing index with nkeys=%lu, noffsets=%lu, "
                "nbitmaps=%lu", fastbit->nkeys, fastbit->noffsets, fastbit->nbitmaps);

        /* Get space info */
        if (FAIL == (type_id = H5Dget_type(fastbit->dataset_id)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get type from dataset");
        if (FAIL == (space_id = H5Dget_space(fastbit->dataset_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get dataspace from dataset");

        /* We've restored the number of elements in the gatherInfo array (.data_length)
         * and cached it for use here and possibly elsewhere.
         */
        nelmts = (size_t)fastbit->nelmts;

        /* Convert type to FastBit type */
        if (FastBitDataTypeUnknown == (fastbit_type = H5X__fastbit_convert_type(type_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCONVERT, FAIL, "can't convert type");

        /* Register array */
        if (0 != fastbit_iapi_register_array_index_only(fastbit->column_name,
                fastbit_type, &nelmts, 1, fastbit->keys, fastbit->nkeys,
                fastbit->offsets, fastbit->noffsets, fastbit->bitmaps,
                H5X__fastbit_read_bitmaps))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTREGISTER, FAIL, "can't register array");

        H5X_FASTBIT_LOG_DEBUG("Reconstructed index");
        fastbit->idx_reconstructed = TRUE;
    }

done:

    if (FAIL != type_id)
        H5Tclose(type_id);
    if (dxpl_id != H5P_DEFAULT)
        H5Pclose(dxpl_id);
    if (FAIL != keys_space_id)
        H5Sclose(keys_space_id);
    if (FAIL != offsets_space_id)
        H5Sclose(offsets_space_id);
    if (FAIL != bitmaps_space_id)
        H5Sclose(bitmaps_space_id);
    if (info_space_id != FAIL)
        H5Sclose(info_space_id);
    if (keys_filespace_id != FAIL)
        H5Sclose(keys_filespace_id);
    if (offsets_filespace_id != FAIL)
        H5Sclose(offsets_filespace_id);
    if (bitmaps_filespace_id != FAIL)
        H5Sclose(bitmaps_filespace_id);

    if (keys_memspace_id != FAIL)
        H5Sclose(keys_memspace_id);
    if (offsets_memspace_id != FAIL)
        H5Sclose(offsets_memspace_id);
    if (bitmaps_memspace_id != FAIL)
        H5Sclose(bitmaps_memspace_id);

    FUNC_LEAVE_NOAPI(ret_value);
}

static herr_t
H5X__fastbit_reconstruct_index(H5X_fastbit_t *fastbit)
{
    static int sequence_id = 0;
    char log_entry[256];
    hid_t keys_space_id = FAIL, offsets_space_id = FAIL, bitmaps_space_id = FAIL;
    size_t key_array_size, offset_array_size, bitmap_array_size;
    FastBitDataType fastbit_type;
    hid_t type_id = FAIL, space_id = FAIL;
    size_t nelmts;
    herr_t ret_value = SUCCEED; /* Return value */

#ifdef H5_HAVE_PARALLEL
    if (H5Xparallel_queries_enabled() > 0)
    // if (fastbit->idx_multiblock)
        return H5X__fastbit_reconstruct_parallel_index(fastbit);
#endif
    FUNC_ENTER_NOAPI_NOINIT

    /* TODO don't read keys and offsets if already present */
    if ((fastbit->keys == NULL) ||
	(fastbit->offsets == NULL) ||
	(fastbit->bitmaps == NULL)) {

        if (FAIL == (keys_space_id = H5Dget_space(fastbit->keys_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
        if (0 == (key_array_size = (size_t) H5Sget_select_npoints(keys_space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
        fastbit->nkeys = key_array_size / sizeof(double);

        if (FAIL == (offsets_space_id = H5Dget_space(fastbit->offsets_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
        if (0 == (offset_array_size = (size_t) H5Sget_select_npoints(offsets_space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
        fastbit->noffsets = offset_array_size / sizeof(int64_t);

        if (FAIL == (bitmaps_space_id = H5Dget_space(fastbit->bitmaps_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
        if (0 == (bitmap_array_size = (size_t) H5Sget_select_npoints(bitmaps_space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
        fastbit->nbitmaps = bitmap_array_size / sizeof(uint32_t);

        /* allocate buffer to hold data */
        if (NULL == (fastbit->keys = (double *) H5MM_malloc(key_array_size)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate keys");
        if (NULL == (fastbit->offsets = (int64_t *) H5MM_malloc(offset_array_size)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate offsets");
        if (NULL == (fastbit->bitmaps = (uint32_t *) H5MM_malloc(bitmap_array_size)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate offsets");

        /* Read FastBit keys */
        if (FAIL == H5Dread(fastbit->keys_id, fastbit->opaque_type_id,
                H5S_ALL, keys_space_id, H5P_DEFAULT, fastbit->keys))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

        /* Read FastBit offsets */
        if (FAIL == H5Dread(fastbit->offsets_id, fastbit->opaque_type_id,
                H5S_ALL, offsets_space_id, H5P_DEFAULT, fastbit->offsets))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

        /* Read FastBit bitmaps */
        if (FAIL == H5Dread(fastbit->bitmaps_id, fastbit->opaque_type_id,
                H5S_ALL, bitmaps_space_id, H5P_DEFAULT, fastbit->bitmaps))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

        /* Reconstruct index */
        H5X_FASTBIT_LOG_DEBUG("Reconstructing index with nkeys=%lu, noffsets=%lu, "
                "nbitmaps=%lu", fastbit->nkeys, fastbit->noffsets, fastbit->nbitmaps);

        /* Get space info */
        if (FAIL == (type_id = H5Dget_type(fastbit->dataset_id)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get type from dataset");
        if (FAIL == (space_id = H5Dget_space(fastbit->dataset_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
        if (0 == (nelmts = (size_t) H5Sget_select_npoints(space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

        /* Convert type to FastBit type */
        if (FastBitDataTypeUnknown == (fastbit_type = H5X__fastbit_convert_type(type_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCONVERT, FAIL, "can't convert type");

        /* Register array */
        if (0 != fastbit_iapi_register_array_index_only(fastbit->column_name,
                fastbit_type, &nelmts, 1, fastbit->keys, fastbit->nkeys,
                fastbit->offsets, fastbit->noffsets, fastbit->bitmaps,
                H5X__fastbit_read_bitmaps))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTREGISTER, FAIL, "can't register array");

        H5X_FASTBIT_LOG_DEBUG("Reconstructed index");
        fastbit->idx_reconstructed = TRUE;
    }
done:
    if (FAIL != type_id)
        H5Tclose(type_id);
    if (FAIL != space_id)
        H5Sclose(space_id);
    if (FAIL != keys_space_id)
        H5Sclose(keys_space_id);
    if (FAIL != offsets_space_id)
        H5Sclose(offsets_space_id);
    if (FAIL != bitmaps_space_id)
        H5Sclose(bitmaps_space_id);
    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fasbit_query_get_value_as_double
 *
 * Purpose: Convert query element value to double that can be used by
 *          FastBit selection.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static double
H5X__fasbit_query_get_value_as_double(H5Q_t *query)
{
    H5T_t *type = query->query.select.elem.data_elem.type;
    double ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_LLONG_g), FALSE)) {
        ret_value = (double) *((long long *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_LONG_g), FALSE)) {
        ret_value = (double) *((long *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_INT_g), FALSE)) {
        ret_value = (double) *((int *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_SHORT_g), FALSE)) {
        ret_value = (double) *((short *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_SCHAR_g), FALSE)) {
        ret_value = (double) *((char *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_DOUBLE_g), FALSE)) {
        ret_value = *((double *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_FLOAT_g), FALSE)) {
        ret_value = (double) *((float *) query->query.select.elem.data_elem.value);
    }

    H5X_FASTBIT_LOG_DEBUG("Query element value is: %lf", ret_value);

    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_evaluate_query
 *
 * Purpose: Evaluate query and get matching coordinates.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_evaluate_query(H5X_fastbit_t *fastbit, hid_t query_id,
        uint64_t **coords, uint64_t *ncoords)
{
    H5Q_t *query;
    double cv;
    FastBitSelectionHandle selection_handle = NULL;
    FastBitCompareType compare_type;
    int64_t nhits;
    uint64_t *hit_coords = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (query->is_combined)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "combined queries are not supported");

    /* Translate match op to FastBit op */
    switch(query->query.select.match_op) {
        case H5Q_MATCH_GREATER_THAN:
            compare_type = FastBitCompareGreater;
            break;
        case H5Q_MATCH_LESS_THAN:
            compare_type = FastBitCompareLess;
            break;
        case H5Q_MATCH_EQUAL:
            compare_type = FastBitCompareEqual;
            break;
        case H5Q_MATCH_NOT_EQUAL:
            compare_type = FastBitCompareNotEqual;
            break;
        default:
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "unsupported query op");
    }

    cv = H5X__fasbit_query_get_value_as_double(query);
    selection_handle = fastbit_selection_osr(fastbit->column_name, compare_type, cv);

    H5X_FASTBIT_LOG_DEBUG("Selection estimate: %ld", fastbit_selection_estimate(selection_handle));

    if (0 > (nhits = fastbit_selection_evaluate(selection_handle)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCOMPUTE, FAIL, "can't evaluate selection");

    if (nhits > 0) {
        if (NULL == (hit_coords = (uint64_t *) H5MM_malloc(((size_t) nhits) * sizeof(uint64_t))))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate hit coordinates");

        if (0 > fastbit_selection_get_coordinates(selection_handle, hit_coords,(uint64_t) nhits, 0U))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get coordinates");

#ifdef H5X_FASTBIT_DEBUG
        /* TODO remove debug */
        {
            int k;
	    int64_t loops = nhits/10;
            int64_t i;
            printf(" # %s(): Index read contains following %lld coords: ",  __func__, nhits);
            for (i=0 ; i < loops; i += 10) {
		printf("\n");
                for(k=0; k< 10; k++) {
                    printf("%lu ", hit_coords[i+k]);
                }
	    }
            printf("\n");
        }
#endif
    }

    *coords = hit_coords;
    *ncoords = (uint64_t) nhits;

done:
    if (selection_handle)
        fastbit_selection_free(selection_handle);
    if ((FAIL == ret_value) && (hit_coords != NULL)) {
        H5MM_free(hit_coords);
    }
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_create_selection
 *
 * Purpose: Create dataspace selection from coordinates.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * RAW:: This needs to be able to create a NULL selection so that we don't
 *       FAIL unless there is an actual error.   It should NOT be an error
 *       to not have any matches (i.e. when ncords == 0)!
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_create_selection(H5X_fastbit_t *fastbit, hid_t dataspace_id,
        uint64_t *coords, uint64_t ncoords)
{
    hsize_t count[H5S_MAX_RANK + 1];
    hsize_t start_coord[H5S_MAX_RANK + 1], end_coord[H5S_MAX_RANK + 1], nelmts;
    herr_t ret_value = SUCCEED; /* Return value */
    size_t i, dataset_rank;
    uint64_t colimit = ncoords;

    double t_start, t_end, t_loop, t_select_bounds, t_select_npoints;
    double t_hyperslab = 0, temp, t0, t1;
    double t_min = 0.1, t_max = 0, target = 0.01;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for non-null selections */
    if (ncoords > 0) {
        hsize_t new_coords[H5S_MAX_RANK + 1];
        HDmemset(new_coords, 0, sizeof(new_coords));
        /* Initialize count */
#ifdef LIMIT_RESULTS_FOR_TESTING
        if (colimit > 10000) colimit = 10000;
#endif
        for (i = 0; i < H5S_MAX_RANK; i++)
            count[i] = 1;

#ifdef SHOW_TIME
        t_start = MPI_Wtime();        
#endif
        for (i = 0; i < colimit /* ncoords*/; i++) {
            const hsize_t point = coords[i] + fastbit->filespace_offset;
            /* Convert coordinates */
            if (FAIL == H5VM_array_calc_pre(point, fastbit->dataset_ndims, fastbit->dataset_down_dims, new_coords))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate coord array");

            /* Add converted coordinate to selection */
            if (H5Sselect_hyperslab(dataspace_id, H5S_SELECT_OR, new_coords, NULL, count, NULL))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to add point to selection");
        }

#ifdef SHOW_TIME
        t_end = MPI_Wtime();
	t_loop = (t_end - t_start);
	t_start = t_end;
#endif

        if (FAIL == H5Sget_select_bounds(dataspace_id, start_coord, end_coord))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to get bounds");
#ifdef SHOW_TIME
	t_end = MPI_Wtime();
	t_select_bounds = (t_end - t_start);
	t_start = t_end;
#endif
        if (0 == (nelmts = (hsize_t) H5Sget_select_npoints(dataspace_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
#ifdef SHOW_TIME
	t_end = MPI_Wtime();	
	t_select_npoints = (t_end - t_start);

	printf("\tt_loop = %f (cummulative)\n", t_loop);
	printf("\tt_select_bounds = %f\n", t_select_bounds);
	printf("\tt_select_npoints = %f\n", t_select_npoints);
#endif
        H5X_FASTBIT_LOG_DEBUG("Created dataspace from index with %llu elements [(%llu, %llu):(%llu, %llu)]",
                nelmts, start_coord[0], start_coord[1], end_coord[0], end_coord[1]);
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_create
 *
 * Purpose: This function creates a new instance of a FastBit plugin index.
 *
 * Return:  Success:    Pointer to the new index
 *          Failure:    NULL
 *
 *------------------------------------------------------------------------
 */
static void *
H5X_fastbit_create(hid_t dataset_id, hid_t xcpl_id, hid_t H5_ATTR_UNUSED xapl_id,
        size_t *metadata_size, void **metadata)
{
    H5X_fastbit_t *fastbit = NULL;
    void *ret_value = NULL; /* Return value */
    size_t private_metadata_size = 0;
    void *buf = NULL;
    size_t buf_size;
    hbool_t read_on_create = TRUE;
    double t_start, t_end;

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    /* Initialize FastBit plugin */
    if (NULL == (fastbit = H5X__fastbit_init(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, NULL, "can't initialize FastBit");

    /* Check if the property list is non-default */
    if (FAIL == H5Pget_index_read_on_create(xcpl_id, &read_on_create))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get value from property");

    H5X_FASTBIT_LOG_DEBUG("READ ON CREATE: %d\n", read_on_create);

    if (read_on_create) {
        /* Get data from dataset */
#ifdef SHOW_TIME
        t_start = MPI_Wtime();        
#endif
        if (FAIL == H5X__fastbit_read_data(dataset_id, fastbit->column_name, &buf, &buf_size))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get data from dataset");
#ifdef SHOW_TIME
	t_end = MPI_Wtime();
	printf("H5X__fastbit_read_data finished reading %lld bytes in %f seconds\n", buf_size, t_end -t_start);
        /* Index data */
	t_start = t_end;
#endif
        if (FAIL == H5X__fastbit_build_index(fastbit))
           HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create index data from dataset");
#ifdef SHOW_TIME
	t_end = MPI_Wtime();
	printf("H5X__fastbit_build_index completed in %f seconds\n", t_end -t_start);
#endif
    }

    /* Serialize metadata for H5X interface */
    if (FAIL == H5X__fastbit_serialize_metadata(fastbit, NULL, &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get plugin metadata size");

    if (NULL == (fastbit->private_metadata = H5MM_malloc(private_metadata_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate plugin metadata");

    /* Serialize plugin metadata */
    if (FAIL == H5X__fastbit_serialize_metadata(fastbit, fastbit->private_metadata,
            &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, NULL, "can't serialize plugin metadata");

    /* Metadata for anonymous dataset */
    *metadata = fastbit->private_metadata;
    *metadata_size = private_metadata_size;

    ret_value = fastbit;

done:
    H5MM_free(buf);
    if (NULL == ret_value)
        H5X__fastbit_term(fastbit);
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_remove
 *
 * Purpose: This function removes the FastBit plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_remove(hid_t file_id, size_t metadata_size, void *metadata)
{
    hid_t keys_id = FAIL, offsets_id = FAIL, bitmaps_id = FAIL;
    char *buf_ptr = (char *) metadata;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");
    if (metadata_size < (3 * sizeof(haddr_t)))
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "metadata size is not valid");

    /* Decode keys info */
    if (FAIL == (keys_id = H5Oopen_by_addr(file_id, *((haddr_t *) buf_ptr))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += sizeof(haddr_t);

    /* Decode offsets info */
    if (FAIL == (offsets_id = H5Oopen_by_addr(file_id, *((haddr_t *) buf_ptr))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += sizeof(haddr_t);

    /* Decode bitmaps info */
    if (FAIL == (bitmaps_id = H5Oopen_by_addr(file_id, *((haddr_t *) buf_ptr))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");

    /* Decrement refcount so that anonymous dataset gets deleted */
    if (FAIL == H5Odecr_refcount(keys_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDEC, FAIL, "can't decrement dataset refcount");
    if (FAIL == H5Odecr_refcount(offsets_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDEC, FAIL, "can't decrement dataset refcount");
    if (FAIL == H5Odecr_refcount(bitmaps_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDEC, FAIL, "can't decrement dataset refcount");

done:
    if (FAIL != keys_id)
        H5Dclose(keys_id);
    if (FAIL != offsets_id)
        H5Dclose(offsets_id);
    if (FAIL != bitmaps_id)
        H5Dclose(bitmaps_id);

    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_open
 *
 * Purpose: This function opens an already existing FastBit index from a file.
 *
 * Return:  Success:    Pointer to the index
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5X_fastbit_open(hid_t dataset_id, hid_t H5_ATTR_UNUSED xapl_id, size_t metadata_size,
        void *metadata)
{
    H5X_fastbit_t *fastbit = NULL;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata");

    /* Initialize FastBit plugin */
    if (NULL == (fastbit = H5X__fastbit_init(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, NULL, "can't initialize FastBit");

    /* Deserialize plugin metadata */
    if (FAIL == H5X__fastbit_deserialize_metadata(fastbit, metadata))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDECODE, NULL, "can't deserialize plugin metadata");

    ret_value = fastbit;

done:
    if (NULL == ret_value)
        H5X__fastbit_term(fastbit);
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_close
 *
 * Purpose: This function closes an FastBit index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_close(void *idx_handle)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");
    
    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    if (FAIL == H5X__fastbit_term(fastbit))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTFREE, FAIL, "Cannot terminate FastBit");

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_close() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_pre_update
 *
 * Purpose: This function does a pre_update of indexing information.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_pre_update(void *idx_handle, hid_t filespace_id, hid_t H5_ATTR_UNUSED xxpl_id)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");
    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

#ifdef H5_HAVE_PARALLEL
    fastbit->filespace_id = filespace_id;
#endif

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_pre_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_post_update
 *
 * Purpose: This function does a post_update of indexing information.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_post_update(void *idx_handle, const void *data, hid_t dataspace_id,
        hid_t H5_ATTR_UNUSED xxpl_id)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */
    void *buf = NULL;
    size_t buf_size;

    FUNC_ENTER_NOAPI_NOINIT

    H5X_FASTBIT_LOG_DEBUG("Calling H5X_fastbit_post_update");
    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* Get data from dataset */
    if (FAIL == H5X__fastbit_read_data(fastbit->dataset_id, fastbit->column_name, &buf, &buf_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get data from dataset");

    /* Merge old and new data */
    if (FAIL == H5X__fastbit_merge_data(fastbit, data, dataspace_id, buf))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "can't merge data");

    /* Update index */
    if (FAIL == H5X__fastbit_build_index(fastbit))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create index data from dataset");

done:
    H5MM_free(buf);
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_post_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_query
 *
 * Purpose: This function retrieves indexing information that matches
 * the query and returns results under the form of a dataspace ID.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5X_fastbit_query(void *idx_handle, hid_t H5_ATTR_UNUSED dataspace_id,
        hid_t query_id, hid_t H5_ATTR_UNUSED xxpl_id)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    hid_t space_id = FAIL, ret_space_id = FAIL;
    uint64_t *coords = NULL;
    uint64_t ncoords = 0;
    hid_t ret_value = FAIL; /* Return value */
    double t_start, t_end;

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* If metadata has not been read already, read it */
    if (!fastbit->idx_reconstructed && (FAIL == H5X__fastbit_reconstruct_index(fastbit)))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read FastBit metadata");

    /* Create a copy of the original dataspace */
    if (FAIL == (space_id = H5Dget_space(fastbit->dataset_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
    if (FAIL == (ret_space_id = H5Scopy(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy dataspace");
    if (FAIL == H5Sselect_none(ret_space_id))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't reset selection");

#ifdef SHOW_TIME
    t_start = MPI_Wtime();
#endif

    /* Get matching coordinates values from query */
    if (FAIL == H5X__fastbit_evaluate_query(fastbit, query_id, &coords, &ncoords))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get query range");
#ifdef SHOW_TIME
    t_end = MPI_Wtime();
    printf("H5X__fastbit_evaluate_query took %f seconds to process %lld of %lld total elements, resulting in %lld coords\n", t_end - t_start, fastbit->nelmts, fastbit->dataset_dims[0], ncoords);
    t_start = t_end;
#endif
    /* Create selection */
    if (FAIL == H5X__fastbit_create_selection(fastbit, ret_space_id, coords, ncoords))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get query range");

#ifdef SHOW_TIME
    t_end = MPI_Wtime();
    printf("H5X__fastbit_create_selection took %f seconds\n", t_end - t_start);
#endif
    ret_value = ret_space_id;

done:
    if (coords != NULL)
        H5MM_free(coords);
    if (FAIL != space_id)
        H5Sclose(space_id);
    if ((FAIL == ret_value) && (FAIL != ret_space_id))
        H5Sclose(ret_space_id);
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_query() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_refresh
 *
 * Purpose: This function refreshes metadata.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_refresh(void *idx_handle, size_t *metadata_size, void **metadata)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    size_t private_metadata_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (fastbit->private_metadata)
        H5MM_free(fastbit->private_metadata);

    /* Serialize metadata for H5X interface */
    if (FAIL == H5X__fastbit_serialize_metadata(fastbit, NULL, &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get plugin metadata size");

    if (NULL == (fastbit->private_metadata = H5MM_malloc(private_metadata_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate plugin metadata");

    /* Serialize plugin metadata */
    if (FAIL == H5X__fastbit_serialize_metadata(fastbit, fastbit->private_metadata,
            &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, FAIL, "can't serialize plugin metadata");

    /* Metadata for anonymous dataset */
    *metadata = fastbit->private_metadata;
    *metadata_size = private_metadata_size;

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_get_size
 *
 * Purpose: This function gets the storage size of the index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_get_size(void *idx_handle, hsize_t *idx_size)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    hsize_t bitmaps_size, keys_size, offsets_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");
    if (!idx_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer");

    bitmaps_size = H5Dget_storage_size(fastbit->bitmaps_id);
    keys_size = H5Dget_storage_size(fastbit->keys_id);
    offsets_size = H5Dget_storage_size(fastbit->offsets_id);

    *idx_size = bitmaps_size + keys_size + offsets_size;

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_get_size */

#endif /* H5_HAVE_FASTBIT */
