/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              February, 2012
 *
 * Purpose:	The IOD VOL plugin where access is forwarded to the IOD library 
 *              by the function shipper.
 */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_iod_init_interface

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FDprivate.h"        /* file drivers            		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h"		/* Dataspaces		  		*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"            /* Iod VOL plugin			*/
#include "H5VLiod_client.h"     /* Client IOD helper			*/

#ifdef H5_HAVE_EFF

/* function shipper IDs for different routines */
static hg_id_t H5VL_EFF_INIT_ID;
static hg_id_t H5VL_EFF_FINALIZE_ID;
static hg_id_t H5VL_FILE_CREATE_ID;
static hg_id_t H5VL_FILE_OPEN_ID;
static hg_id_t H5VL_FILE_FLUSH_ID;
static hg_id_t H5VL_FILE_CLOSE_ID;
static hg_id_t H5VL_ATTR_CREATE_ID;
static hg_id_t H5VL_ATTR_OPEN_ID;
static hg_id_t H5VL_ATTR_READ_ID;
static hg_id_t H5VL_ATTR_WRITE_ID;
static hg_id_t H5VL_ATTR_EXISTS_ID;
static hg_id_t H5VL_ATTR_ITERATE_ID;
static hg_id_t H5VL_ATTR_RENAME_ID;
static hg_id_t H5VL_ATTR_REMOVE_ID;
static hg_id_t H5VL_ATTR_CLOSE_ID;
static hg_id_t H5VL_GROUP_CREATE_ID;
static hg_id_t H5VL_GROUP_OPEN_ID;
static hg_id_t H5VL_GROUP_CLOSE_ID;
static hg_id_t H5VL_MAP_CREATE_ID;
static hg_id_t H5VL_MAP_OPEN_ID;
static hg_id_t H5VL_MAP_SET_ID;
static hg_id_t H5VL_MAP_GET_ID;
static hg_id_t H5VL_MAP_GET_COUNT_ID;
static hg_id_t H5VL_MAP_EXISTS_ID;
static hg_id_t H5VL_MAP_ITERATE_ID;
static hg_id_t H5VL_MAP_DELETE_ID;
static hg_id_t H5VL_MAP_CLOSE_ID;
static hg_id_t H5VL_DSET_CREATE_ID;
static hg_id_t H5VL_DSET_OPEN_ID;
static hg_id_t H5VL_DSET_READ_ID;
static hg_id_t H5VL_DSET_WRITE_ID;
static hg_id_t H5VL_DSET_SET_EXTENT_ID;
static hg_id_t H5VL_DSET_CLOSE_ID;
static hg_id_t H5VL_DTYPE_COMMIT_ID;
static hg_id_t H5VL_DTYPE_OPEN_ID;
static hg_id_t H5VL_DTYPE_CLOSE_ID;
static hg_id_t H5VL_LINK_CREATE_ID;
static hg_id_t H5VL_LINK_MOVE_ID;
static hg_id_t H5VL_LINK_ITERATE_ID;
static hg_id_t H5VL_LINK_EXISTS_ID;
static hg_id_t H5VL_LINK_REMOVE_ID;
static hg_id_t H5VL_OBJECT_OPEN_ID;
static hg_id_t H5VL_OBJECT_COPY_ID;
static hg_id_t H5VL_OBJECT_EXISTS_ID;
static hg_id_t H5VL_OBJECT_VISIT_ID;
static hg_id_t H5VL_OBJECT_SET_COMMENT_ID;
static hg_id_t H5VL_OBJECT_GET_COMMENT_ID;
static hg_id_t H5VL_CANCEL_OP_ID;

/* Prototypes */
static void *H5VL_iod_fapl_copy(const void *_old_fa);
static herr_t H5VL_iod_fapl_free(void *_fa);

/* Atrribute callbacks */
static void *H5VL_iod_attribute_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_attribute_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_attribute_read(void *attr, hid_t dtype_id, void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_attribute_write(void *attr, hid_t dtype_id, const void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_attribute_get(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_attribute_remove(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_attribute_close(void *attr, hid_t dxpl_id, void **req);

/* Datatype callbacks */
static void *H5VL_iod_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
static ssize_t H5VL_iod_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_datatype_get(void *obj, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_datatype_close(void *dt, hid_t dxpl_id, void **req);

/* Dataset callbacks */
static void *H5VL_iod_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                    hid_t file_space_id, hid_t plist_id, void *buf, void **req);
static herr_t H5VL_iod_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                     hid_t file_space_id, hid_t plist_id, const void *buf, void **req);
static herr_t H5VL_iod_dataset_set_extent(void *dset, const hsize_t size[], hid_t dxpl_id, void **req);
static herr_t H5VL_iod_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_dataset_close(void *dset, hid_t dxpl_id, void **req);

/* File callbacks */
static void *H5VL_iod_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_file_misc(void *file, H5VL_file_misc_t misc_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_file_close(void *file, hid_t dxpl_id, void **req);

/* Group callbacks */
static void *H5VL_iod_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_group_get(void *obj, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_group_close(void *grp, hid_t dxpl_id, void **req);

/* Link callbacks */
static herr_t H5VL_iod_link_create(H5VL_link_create_type_t create_type, void *obj, 
                                   H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                 void *dst_obj, H5VL_loc_params_t loc_params2,
                                 hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_link_iterate(void *obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                    H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                                    H5L_iterate_t op, void *op_data, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_link_remove(void *obj, H5VL_loc_params_t loc_params, hid_t dxpl_id, void **req);

/* Object callbacks */
static void *H5VL_iod_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                                   void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                                   hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_object_visit(void *obj, H5VL_loc_params_t loc_params, H5_index_t idx_type, 
                                    H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, hid_t dxpl_id, void **req, va_list arguments);

static herr_t H5VL_iod_cancel(void **req, H5_status_t *status);
static herr_t H5VL_iod_test(void **req, H5_status_t *status);
static herr_t H5VL_iod_wait(void **req, H5_status_t *status);

/* IOD-specific file access properties */
typedef struct H5VL_iod_fapl_t {
    MPI_Comm		comm;		/*communicator			*/
    MPI_Info		info;		/*file information		*/
} H5VL_iod_fapl_t;

H5FL_DEFINE(H5VL_iod_file_t);
H5FL_DEFINE(H5VL_iod_attr_t);
H5FL_DEFINE(H5VL_iod_group_t);
H5FL_DEFINE(H5VL_iod_map_t);
H5FL_DEFINE(H5VL_iod_dset_t);
H5FL_DEFINE(H5VL_iod_dtype_t);

static na_addr_t PEER;
static na_class_t *network_class = NULL;

static uint64_t axe_id;
static uint64_t axe_bound;

static H5VL_class_t H5VL_iod_g = {
    IOD,
    "iod",					/* name */
    NULL,                                       /* initialize */
    NULL,                                       /* terminate */
    sizeof(H5VL_iod_fapl_t),		        /*fapl_size */
    H5VL_iod_fapl_copy,			        /*fapl_copy */
    H5VL_iod_fapl_free, 		        /*fapl_free */
    {                                           /* attribute_cls */
        H5VL_iod_attribute_create,              /* create */
        H5VL_iod_attribute_open,                /* open */
        H5VL_iod_attribute_read,                /* read */
        H5VL_iod_attribute_write,               /* write */
        NULL,//H5VL_iod_attr_iterate,               /* iterate */
        H5VL_iod_attribute_get,                 /* get */
        H5VL_iod_attribute_remove,              /* remove */
        H5VL_iod_attribute_close                /* close */
    },
    {                                           /* datatype_cls */
        H5VL_iod_datatype_commit,               /* commit */
        H5VL_iod_datatype_open,                 /* open */
        H5VL_iod_datatype_get_binary,           /* get_size */
        H5VL_iod_datatype_get,                  /* get */
        H5VL_iod_datatype_close                 /* close */
    },
    {                                           /* dataset_cls */
        H5VL_iod_dataset_create,                /* create */
        H5VL_iod_dataset_open,                  /* open */
        H5VL_iod_dataset_read,                  /* read */
        H5VL_iod_dataset_write,                 /* write */
        H5VL_iod_dataset_set_extent,            /* set extent */
        H5VL_iod_dataset_get,                   /* get */
        H5VL_iod_dataset_close                  /* close */
    },
    {                                           /* file_cls */
        H5VL_iod_file_create,                   /* create */
        H5VL_iod_file_open,                     /* open */
        H5VL_iod_file_flush,                    /* flush */
        H5VL_iod_file_get,                      /* get */
        H5VL_iod_file_misc,                     /* misc */
        NULL,                                   /* optional */
        H5VL_iod_file_close                     /* close */
    },
    {                                           /* group_cls */
        H5VL_iod_group_create,                  /* create */
        H5VL_iod_group_open,                    /* open */
        H5VL_iod_group_get,                     /* get */
        H5VL_iod_group_close                    /* close */
    },
    {                                           /* link_cls */
        H5VL_iod_link_create,                   /* create */
        H5VL_iod_link_move,                     /* move */
        H5VL_iod_link_iterate,                  /* iterate */
        H5VL_iod_link_get,                      /* get */
        H5VL_iod_link_remove                    /* remove */
    },
    {                                           /* object_cls */
        H5VL_iod_object_open,                   /* open */
        H5VL_iod_object_copy,                   /* copy */
        H5VL_iod_object_visit,                  /* visit */
        H5VL_iod_object_get,                    /* get */
        H5VL_iod_object_misc,                   /* misc */
        NULL,                                   /* optional */
        NULL                                    /* close */
    },
    {
        H5VL_iod_cancel,
        H5VL_iod_test,
        H5VL_iod_wait
    }
};


/*--------------------------------------------------------------------------
NAME
   H5VL_iod_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_iod_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_iod_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_iod_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5VL_iod_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the iod plugin.
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
H5VL_class_t *
H5VL_iod_init(void)
{
    H5VL_class_t *ret_value = NULL;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set return value */
    ret_value = &H5VL_iod_g;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_init() */


/*-------------------------------------------------------------------------
 * Function:	EFF_init
 *
 * Purpose:	initialize to the EFF stack
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
EFF_init(MPI_Comm comm, MPI_Info UNUSED info)
{
    char mpi_port_name[MPI_MAX_PORT_NAME];
    FILE *config;
    hg_request_t hg_req;
    int num_procs, my_rank;
    na_addr_t ion_target;
    uint64_t axe_seed;
    herr_t ret_value = SUCCEED;

    MPI_Comm_size(comm, &num_procs);
    MPI_Comm_rank(comm, &my_rank);

    /* generate global variables to create and track axe_ids for every
       operation. Each process owns a portion of the ID space and uses
       that space incrementally. */
    axe_seed = (pow(2.0,64.0) - 1) / num_procs;
    axe_id = axe_seed * my_rank + 1;
    axe_bound = axe_seed * (my_rank + 1);

    /* This is a temporary solution for connecting to the server using
       mercury */
    if ((config = fopen("port.cfg", "r")) != NULL) {
        fread(mpi_port_name, sizeof(char), MPI_MAX_PORT_NAME, config);
        printf("Using MPI port name: %s.\n", mpi_port_name);
        fclose(config);
    }

    /* initialize Mercury stuff */
    network_class = NA_MPI_Init(NULL, 0);
    if (HG_SUCCESS != HG_Init(network_class))
        return FAIL;
    if (HG_SUCCESS != HG_Bulk_init(network_class))
        return FAIL;
    if (HG_SUCCESS !=  NA_Addr_lookup(network_class, mpi_port_name, &ion_target))
        return FAIL;

    PEER = ion_target;

    /* Register function and encoding/decoding functions */
    H5VL_EFF_INIT_ID     = MERCURY_REGISTER("eff_init", eff_init_in_t, ret_t);
    H5VL_EFF_FINALIZE_ID = MERCURY_REGISTER("eff_finalize", ret_t, ret_t);

    H5VL_FILE_CREATE_ID = MERCURY_REGISTER("file_create", file_create_in_t, file_create_out_t);
    H5VL_FILE_OPEN_ID   = MERCURY_REGISTER("file_open", file_open_in_t, file_open_out_t);
    H5VL_FILE_FLUSH_ID  = MERCURY_REGISTER("file_flush", file_flush_in_t, ret_t);
    H5VL_FILE_CLOSE_ID  = MERCURY_REGISTER("file_close", file_close_in_t, ret_t);

    H5VL_ATTR_CREATE_ID = MERCURY_REGISTER("attr_create", attr_create_in_t, attr_create_out_t);
    H5VL_ATTR_OPEN_ID   = MERCURY_REGISTER("attr_open", attr_open_in_t, attr_open_out_t);
    H5VL_ATTR_READ_ID   = MERCURY_REGISTER("attr_read", attr_io_in_t, ret_t);
    H5VL_ATTR_WRITE_ID  = MERCURY_REGISTER("attr_write", attr_io_in_t, ret_t);
    H5VL_ATTR_EXISTS_ID = MERCURY_REGISTER("attr_exists", attr_op_in_t, htri_t);
    H5VL_ATTR_ITERATE_ID = MERCURY_REGISTER("attr_iterate", attr_op_in_t, ret_t);
    H5VL_ATTR_RENAME_ID = MERCURY_REGISTER("attr_rename", attr_rename_in_t, ret_t);
    H5VL_ATTR_REMOVE_ID = MERCURY_REGISTER("attr_remove", attr_op_in_t, ret_t);
    H5VL_ATTR_CLOSE_ID  = MERCURY_REGISTER("attr_close", attr_close_in_t, ret_t);

    H5VL_GROUP_CREATE_ID = MERCURY_REGISTER("group_create", group_create_in_t, group_create_out_t);
    H5VL_GROUP_OPEN_ID   = MERCURY_REGISTER("group_open", group_open_in_t, group_open_out_t);
    H5VL_GROUP_CLOSE_ID  = MERCURY_REGISTER("group_close", group_close_in_t, ret_t);

    H5VL_MAP_CREATE_ID = MERCURY_REGISTER("map_create", map_create_in_t, map_create_out_t);
    H5VL_MAP_OPEN_ID   = MERCURY_REGISTER("map_open", map_open_in_t, map_open_out_t);
    H5VL_MAP_SET_ID    = MERCURY_REGISTER("map_set", map_set_in_t, ret_t);
    H5VL_MAP_GET_ID    = MERCURY_REGISTER("map_get", map_get_in_t, map_get_out_t);
    H5VL_MAP_GET_COUNT_ID = MERCURY_REGISTER("map_get_count", map_get_count_in_t, int64_t);
    H5VL_MAP_ITERATE_ID   = MERCURY_REGISTER("map_iterate", map_op_in_t, ret_t);
    H5VL_MAP_EXISTS_ID = MERCURY_REGISTER("map_exists", map_op_in_t, hbool_t);
    H5VL_MAP_DELETE_ID = MERCURY_REGISTER("map_delete", map_op_in_t, ret_t);
    H5VL_MAP_CLOSE_ID  = MERCURY_REGISTER("map_close", map_close_in_t, ret_t);

    H5VL_DSET_CREATE_ID = MERCURY_REGISTER("dset_create", dset_create_in_t, dset_create_out_t);
    H5VL_DSET_OPEN_ID   = MERCURY_REGISTER("dset_open", dset_open_in_t, dset_open_out_t);
    H5VL_DSET_READ_ID   = MERCURY_REGISTER("dset_read", dset_io_in_t, dset_read_out_t);
    H5VL_DSET_WRITE_ID  = MERCURY_REGISTER("dset_write", dset_io_in_t, ret_t);
    H5VL_DSET_SET_EXTENT_ID = MERCURY_REGISTER("dset_set_extent", 
                                               dset_set_extent_in_t, ret_t);
    H5VL_DSET_CLOSE_ID  = MERCURY_REGISTER("dset_close", dset_close_in_t, ret_t);

    H5VL_DTYPE_COMMIT_ID = MERCURY_REGISTER("dtype_commit", dtype_commit_in_t, dtype_commit_out_t);
    H5VL_DTYPE_OPEN_ID   = MERCURY_REGISTER("dtype_open", dtype_open_in_t, dtype_open_out_t);
    H5VL_DTYPE_CLOSE_ID  = MERCURY_REGISTER("dtype_close", dtype_close_in_t, ret_t);

    H5VL_LINK_CREATE_ID  = MERCURY_REGISTER("link_create", link_create_in_t, ret_t);
    H5VL_LINK_MOVE_ID    = MERCURY_REGISTER("link_move", link_move_in_t, ret_t);
    H5VL_LINK_EXISTS_ID  = MERCURY_REGISTER("link_exists", link_op_in_t, htri_t);
    H5VL_LINK_ITERATE_ID = MERCURY_REGISTER("link_iterate", link_op_in_t, ret_t);
    H5VL_LINK_REMOVE_ID  = MERCURY_REGISTER("link_remove", link_op_in_t, ret_t);

    H5VL_OBJECT_OPEN_ID   = MERCURY_REGISTER("object_open", object_op_in_t, object_open_out_t);
    H5VL_OBJECT_COPY_ID   = MERCURY_REGISTER("object_copy", object_copy_in_t, ret_t);
    H5VL_OBJECT_EXISTS_ID = MERCURY_REGISTER("object_exists", object_op_in_t, htri_t);
    H5VL_OBJECT_VISIT_ID  = MERCURY_REGISTER("object_visit", object_op_in_t, ret_t);
    H5VL_OBJECT_SET_COMMENT_ID = MERCURY_REGISTER("set_comment", object_set_comment_in_t, ret_t);
    H5VL_OBJECT_GET_COMMENT_ID = MERCURY_REGISTER("get_comment", object_get_comment_in_t, 
                                                  object_get_comment_out_t);

    H5VL_CANCEL_OP_ID = MERCURY_REGISTER("cancel_op", uint64_t, uint8_t);

    /* forward the init call to the ION and wait for its completion */
    if(HG_Forward(PEER, H5VL_EFF_INIT_ID, &num_procs, &ret_value, &hg_req) < 0)
        return FAIL;
    HG_Wait(hg_req, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE);

    return ret_value;
} /* end EFF_init() */


/*-------------------------------------------------------------------------
 * Function:	EFF_finalize
 *
 * Purpose:	shutdown the EFF stack
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
EFF_finalize(void)
{
    hg_request_t hg_req;
    herr_t ret_value = SUCCEED;

    /* forward the finalize call to the ION and wait for it to complete */
    if(HG_Forward(PEER, H5VL_EFF_FINALIZE_ID, &ret_value, &ret_value, &hg_req) < 0)
        return FAIL;
    HG_Wait(hg_req, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE);

    /* Free addr id */
    if (HG_SUCCESS != NA_Addr_free(network_class, PEER))
        return FAIL;

    /* Finalize interface */
    if (HG_SUCCESS != HG_Bulk_finalize())
        return FAIL;
    if (HG_SUCCESS != HG_Finalize())
        return FAIL;

    return ret_value;
} /* end EFF_finalize() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_iod
 *
 * Purpose:	Modify the file access property list to use the H5VL_IOD
 *		plugin defined in this source file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_iod(hid_t fapl_id, MPI_Comm comm, MPI_Info info)
{
    H5VL_iod_fapl_t fa;
    H5P_genplist_t  *plist;      /* Property list pointer */
    herr_t          ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iMcMi", fapl_id, comm, info);

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if(MPI_COMM_NULL == comm)
	HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid communicator")

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.info = info;

    ret_value = H5P_set_vol(plist, &H5VL_iod_g, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_iod() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_fapl_copy
 *
 * Purpose:	Copies the iod-specific file access properties.
 *
 * Return:	Success:	Ptr to a new property list
 *		Failure:	NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_fapl_copy(const void *_old_fa)
{
    const H5VL_iod_fapl_t *old_fa = (const H5VL_iod_fapl_t*)_old_fa;
    H5VL_iod_fapl_t	  *new_fa = NULL;
    void		  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (new_fa = (H5VL_iod_fapl_t *)H5MM_malloc(sizeof(H5VL_iod_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Copy the general information */
    /* HDmemcpy(new_fa, old_fa, sizeof(H5VL_iod_fapl_t)); */

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed");

    ret_value = new_fa;

done:
    if (NULL == ret_value){
	/* cleanup */
	if (new_fa)
	    H5MM_xfree(new_fa);
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_fapl_free
 *
 * Purpose:	Frees the iod-specific file access properties.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_fapl_free(void *_fa)
{
    herr_t		ret_value = SUCCEED;
    H5VL_iod_fapl_t	*fa = (H5VL_iod_fapl_t*)_fa;

    FUNC_ENTER_NOAPI_NOINIT

    assert(fa);

    /* Free the internal communicator and INFO object */
    assert(MPI_COMM_NULL!=fa->comm);
    if(H5FD_mpi_comm_info_free(&fa->comm, &fa->info) < 0)
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed");
    /* free the struct */
    H5MM_xfree(fa);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_fapl_free() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_checksum
 *
 * Purpose:     Modify the dataset transfer property list to set a
 *              checksum value for the data to be transfered. 
 *              This is used with write operations.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_checksum(hid_t dxpl_id, uint32_t cs)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", dxpl_id, cs);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_CHECKSUM_NAME, &cs) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set checksum value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_checksum() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_checksum
 *
 * Purpose:     Retrieve the checksum value that was set using 
 *              H5Pset_dxpl_checksum.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_checksum(hid_t dxpl_id, uint32_t *cs/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, cs);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(cs)
        if(H5P_get(plist, H5D_XFER_CHECKSUM_NAME, cs) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_checksum() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_checksum_ptr
 *
 * Purpose:     Set a pointer to tell the library where to insert the
 *              checksum that is received from a remote location. 
 *              This is used with read operations.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_checksum_ptr(hid_t dxpl_id, uint32_t *cs)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", dxpl_id, cs);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_CHECKSUM_PTR_NAME, &cs) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set checksum_ptr value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_checksum_ptr() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_checksum_ptr
 *
 * Purpose:     Retrieve the checksum pointer value that was set using 
 *              H5Pset_dxpl_checksum_ptr.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_checksum_ptr(hid_t dxpl_id, uint32_t **cs/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, cs);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(cs)
        if(H5P_get(plist, H5D_XFER_CHECKSUM_PTR_NAME, *cs) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum_ptr value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_checksum_ptr() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_inject_corruption
 *
 * Purpose:     Temporary routine to set a boolean flag that tells the 
 *              library to inject corruption in the stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_inject_corruption(hid_t dxpl_id, hbool_t flag)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", dxpl_id, flag);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_INJECT_CORRUPTION_NAME, &flag) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_inject_corruption() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_inject_corruption
 *
 * Purpose:     Temporary routine to retrieve the boolean flag that tells the 
 *              library to inject corruption in the stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_inject_corruption(hid_t dxpl_id, hbool_t *flag/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, flag);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(flag)
        if(H5P_get(plist, H5D_XFER_INJECT_CORRUPTION_NAME, flag) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_inject_corruption() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dcpl_append_only
 *
 * Purpose:     Set a boolean flag on the dataset creation property list 
 *              to indicate to the VOL plugin that access to this dataset 
 *              will always be in an append/sequence only manner.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dcpl_append_only(hid_t dcpl_id, hbool_t flag)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", dcpl_id, flag);

    if(dcpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_CRT_APPEND_ONLY_NAME, &flag) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dcpl_append_only() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dcpl_append_only
 *
 * Purpose:     Retrieve a boolean flag on the dataset creation property list 
 *              that indicates whether access to this dataset 
 *              will always be in an append/sequence only manner.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dcpl_append_only(hid_t dcpl_id, hbool_t *flag/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dcpl_id, flag);

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    /* Get the transfer mode */
    if(flag)
        if(H5P_get(plist, H5D_CRT_APPEND_ONLY_NAME, flag) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dcpl_append_only() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_create
 *
 * Purpose:	Creates a file as a iod HDF5 file.
 *
 * Return:	Success:	the file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, 
                     hid_t dxpl_id, void **req)
{
    H5VL_iod_fapl_t *fa = NULL;
    H5P_genplist_t *plist;      /* Property list pointer */
    H5VL_iod_file_t *file = NULL;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    file_create_in_t input;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE;  /* Whether we're performing async. I/O */
    void  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if(0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
	flags |= H5F_ACC_EXCL;	 /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /* obtain the process rank from the communicator attached to the fapl ID */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_iod_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get IOD info struct")

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_iod_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD file struct");
    file->remote_file.root_oh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_id = IOD_ID_UNDEFINED;
    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);

    /* Generate an IOD ID for the root group to be created */
    H5VL_iod_gen_obj_id(file->my_rank, file->num_procs, (uint64_t)0, IOD_OBJ_KV, &input.root_id);
    file->remote_file.root_id = input.root_id;

    /* set the input structure for the HG encode routine */
    input.name = name;
    input.flags = flags;
    input.fcpl_id = fcpl_id;
    input.fapl_id = fapl_id;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("File Create %s IOD ROOT ID %llu, axe id %llu\n", 
           name, input.root_id, input.axe_id);
#endif

    /* forward the call to the ION */
    if(HG_Forward(PEER, H5VL_FILE_CREATE_ID, &input, &file->remote_file, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship file create");

    /* create the file object that is passed to the API layer */
    file->file_name = HDstrdup(name);
    file->flags = flags;
    if((file->remote_file.fcpl_id = H5Pcopy(fcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fcpl");
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl");
    file->nopen_objs = 1;

    /* initialize head and tail of the container's linked list of requests */
    file->request_list_head = NULL;
    file->request_list_tail = NULL;

    file->common.obj_type = H5I_FILE;
    /* The name of the location is the root's object name "\" */
    file->common.obj_name = HDstrdup("/");
    file->common.obj_name[1] = '\0';
    file->common.file = file;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_FILE_CREATE;
    request->data = file;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)file;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        file->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        file->common.request = NULL;
    } /* end else */

    ret_value = (void *)file;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_open
 *
 * Purpose:	Opens a file as a iod HDF5 file.
 *
 * Return:	Success:	file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_fapl_t *fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    H5VL_iod_file_t *file = NULL;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    file_open_in_t input;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* obtain the process rank from the communicator attached to the fapl ID */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_iod_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get IOD info struct")

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_iod_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD file struct");

    file->remote_file.coh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_oh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_id = IOD_ID_UNDEFINED;
    file->remote_file.fcpl_id = -1;

    /* set input paramters in struct to give to the function shipper */
    input.name = name;
    input.flags = flags;
    input.fapl_id = fapl_id;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("File Open %s axe id %llu\n", name, input.axe_id);
#endif

    /* forward the call to the server */
    if(HG_Forward(PEER, H5VL_FILE_OPEN_ID, &input, &file->remote_file, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship file create");

    /* create the file object that is passed to the API layer */
    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);
    file->file_name = HDstrdup(name);
    file->flags = flags;
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl");
    file->nopen_objs = 1;

    /* initialize head and tail of the container's linked list */
    file->request_list_head = NULL;
    file->request_list_tail = NULL;

    file->common.obj_type = H5I_FILE; 
    /* The name of the location is the root's object name "\" */
    file->common.obj_name = HDstrdup("/");
    file->common.obj_name[1] = '\0';
    file->common.file = file;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_FILE_OPEN;
    request->data = file;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)file;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        file->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        file->common.request = NULL;
    } /* end else */

    ret_value = (void *)file;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_flush
 *
 * Purpose:	Flushs a iod HDF5 file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not flushed.
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_flush(void *_obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, 
                    hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    H5VL_iod_file_t *file = obj->file;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    int *status;
    file_flush_in_t input;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* set the input structure for the HG encode routine */
    input.coh = file->remote_file.coh;
    input.scope = scope;
    input.axe_id = axe_id ++;

    /* allocate an integer to receive the return value if the file close succeeded or not */
    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the ION */
    if(HG_Forward(PEER, H5VL_FILE_FLUSH_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship file close");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_FILE_FLUSH;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)file;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        file->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_get
 *
 * Purpose:	Gets certain data about a file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_get(void *_obj, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    H5VL_iod_file_t *file = obj->file;
    hg_request_t *hg_req;
    int *status;
    H5VL_iod_request_t *request;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Fget_access_plist */
        case H5VL_FILE_GET_FAPL:
            {
                H5VL_iod_fapl_t fa, *old_fa;
                H5P_genplist_t *new_plist, *old_plist;
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(file->fapl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")

                if(NULL == (new_plist = (H5P_genplist_t *)H5I_object(*plist_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                if(NULL == (old_plist = (H5P_genplist_t *)H5I_object(file->fapl_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

                if(NULL == (old_fa = (H5VL_iod_fapl_t *)H5P_get_vol_info(old_plist)))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get vol info");
                fa.comm = old_fa->comm;
                fa.info = old_fa->info;

                ret_value = H5P_set_vol(new_plist, &H5VL_iod_g, &fa);

                break;
            }
        /* H5Fget_create_plist */
        case H5VL_FILE_GET_FCPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(file->remote_file.fcpl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file creation property list")

                break;
            }
        /* H5Fget_intent */
        case H5VL_FILE_GET_INTENT:
            {
                unsigned *ret = va_arg (arguments, unsigned *);

                if(file->flags & H5F_ACC_RDWR)
                    *ret = H5F_ACC_RDWR;
                else
                    *ret = H5F_ACC_RDONLY;
                break;
            }
        /* H5Fget_name */
        case H5VL_FILE_GET_NAME:
            {
                H5I_type_t UNUSED type = va_arg (arguments, H5I_type_t);
                size_t     size = va_arg (arguments, size_t);
                char      *name = va_arg (arguments, char *);
                ssize_t   *ret  = va_arg (arguments, ssize_t *);
                size_t     len;

                len = HDstrlen(file->file_name);

                if(name) {
                    HDstrncpy(name, file->file_name, MIN(len + 1,size));
                    if(len >= size)
                        name[size-1]='\0';
                } /* end if */

                /* Set the return value for the API call */
                *ret = (ssize_t)len;
                break;
            }
        /* H5I_get_file_id */
        case H5VL_OBJECT_GET_FILE:
            {

                H5I_type_t UNUSED type = va_arg (arguments, H5I_type_t);
                void      **ret = va_arg (arguments, void **);

                *ret = (void*)file;
                break;
            }
        /* H5Fget_obj_count */
        case H5VL_FILE_GET_OBJ_COUNT:
            {
                unsigned types = va_arg (arguments, unsigned);
                ssize_t *ret = va_arg (arguments, ssize_t *);
                size_t  obj_count = 0;      /* Number of opened objects */

                /* Set the return value */
                *ret = (ssize_t)obj_count;
                //break;
            }
        /* H5Fget_obj_ids */
        case H5VL_FILE_GET_OBJ_IDS:
            {
                unsigned types = va_arg (arguments, unsigned);
                size_t max_objs = va_arg (arguments, size_t);
                hid_t *oid_list = va_arg (arguments, hid_t *);
                ssize_t *ret = va_arg (arguments, ssize_t *);
                size_t  obj_count = 0;      /* Number of opened objects */

                /* Set the return value */
                *ret = (ssize_t)obj_count;
                //break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_misc
 *
 * Purpose:	Perform an operation
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_misc(void *obj, H5VL_file_misc_t misc_type, hid_t dxpl_id, void **req, va_list arguments)
{
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (misc_type) {
        /* H5Fis_accessible */
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                hid_t fapl_id       = va_arg (arguments, hid_t);
                const char *name    = va_arg (arguments, const char *);
                htri_t     *ret     = va_arg (arguments, htri_t *);
                H5VL_iod_file_t *file = NULL;
#if 0
                /* attempt to open the file through the MDS plugin */
                if(NULL == (file = (H5VL_iod_file_t *)H5VL_iod_file_open(name, H5F_ACC_RDONLY, fapl_id,
                                                                         H5_REQUEST_NULL)))
                    *ret = FALSE;
                else
                    *ret = TRUE;

                /* close the file if it was succesfully opened */
                if(file && H5VL_iod_file_close((void*)file, H5_REQUEST_NULL) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close file");
                break;
#endif
            }
        /* H5Fmount */
        case H5VL_FILE_MOUNT:
            {
                H5I_type_t type        = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
                H5VL_iod_file_t *child = va_arg (arguments, H5VL_iod_file_t *);
                hid_t plist_id         = va_arg (arguments, hid_t);
            }
        /* H5Fmount */
        case H5VL_FILE_UNMOUNT:
            {
                H5I_type_t  type       = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "MDS Plugin does not support this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_close
 *
 * Purpose:	Closes a file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_close(void *_file, hid_t dxpl_id, void **req)
{
    H5VL_iod_file_t *file = (H5VL_iod_file_t *)_file;
    file_close_in_t input;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    int *status;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* allocate an integer to receive the return value if the file close succeeded or not */
    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    input.coh = file->remote_file.coh;
    input.root_oh = file->remote_file.root_oh;
    input.root_id = file->remote_file.root_id;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("File Close Root ID %llu axe id %llu\n", input.root_id, input.axe_id);
#endif

    /* forward the call to the ION */
    if(HG_Forward(PEER, H5VL_FILE_CLOSE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship file close");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_FILE_CLOSE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)file;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        file->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_group_create
 *
 * Purpose:	Creates a group inside a iod h5 file.
 *
 * Return:	Success:	group
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_group_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, 
                      hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_group_t *grp = NULL; /* the group object that is created and passed to the user */
    group_create_in_t input;
    H5P_genplist_t *plist;
    hid_t lcpl_id;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    uint64_t parent_axe_id;
    char *new_name = NULL; /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the group creation plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get creation properties */
    if(H5P_get(plist, H5VL_GRP_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id");

    /* Retrieve the parent AXE id by traversing the path where the
       group should be created. */
    if(H5VL_iod_get_parent_info(obj, loc_params, name, &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    grp->remote_group.iod_oh.cookie = IOD_OH_UNDEFINED;
    grp->remote_group.iod_id = IOD_ID_UNDEFINED;
    grp->remote_group.gcpl_id = -1;

    /* Generate an IOD ID for the group to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.grp_id);
    grp->remote_group.iod_id = input.grp_id;

    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.name = new_name;
    input.gcpl_id = gcpl_id;
    input.gapl_id = gapl_id;
    input.lcpl_id = lcpl_id;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("Group Create %s, IOD ID %llu, axe id %llu, parent %llu\n", 
           new_name, input.grp_id, input.axe_id, input.parent_axe_id);
#endif

    /* Get the group access plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gapl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_GROUP_CREATE_ID, &input, &grp->remote_group, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship group create");

    /* setup the local group struct */
    /* store the entire path of the group locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (grp->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(grp->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(grp->common.obj_name+obj_name_len, name, name_len);
        grp->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* copy property lists */
    if((grp->remote_group.gcpl_id = H5Pcopy(gcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gcpl");
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");
    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.file = obj->file;
    grp->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_GROUP_CREATE;
    request->data = grp;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)grp;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        grp->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't wait on HG request")

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        grp->common.request = NULL;
    } /* end else */

    ret_value = (void *)grp;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_group_open
 *
 * Purpose:	Opens a group inside a iod h5 file.
 *
 * Return:	Success:	group id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_group_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                    hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_group_t  *grp = NULL; /* the group object that is created and passed to the user */
    H5P_genplist_t *plist;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    uint64_t parent_axe_id;
    char *new_name = NULL; /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    group_open_in_t input;
    void           *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       group should be opened. */
    if(H5VL_iod_get_parent_info(obj, loc_params, name, &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    grp->remote_group.iod_oh.cookie = IOD_OH_UNDEFINED;
    grp->remote_group.iod_id = IOD_ID_UNDEFINED;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.name = new_name;
    input.gapl_id = gapl_id;
    input.axe_id = axe_id ++;

    /* Get the group access plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gapl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("Group Open %s LOC ID %llu, axe id %llu, parent %llu\n", 
           new_name, input.loc_id, input.axe_id, input.parent_axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_GROUP_OPEN_ID, &input, &grp->remote_group, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship group open");

    /* setup the local group struct */
    /* store the entire path of the group locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (grp->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(grp->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(grp->common.obj_name+obj_name_len, name, name_len);
        grp->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* copy property lists */
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");
    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.file = obj->file;
    grp->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_GROUP_OPEN;
    request->data = grp;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)grp;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        grp->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't wait on HG request")

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        grp->common.request = NULL;
    } /* end else */

    ret_value = (void *)grp;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_group_get
 *
 * Purpose:	Gets certain data about a group
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_group_get(void *_grp, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_iod_group_t *grp = (H5VL_iod_group_t *)_grp;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Gget_create_plist */
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(grp->remote_group.gcpl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group create property list")
                break;
            }
        /* H5Gget_info */
        case H5VL_GROUP_GET_INFO:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5G_info_t *ginfo = va_arg (arguments, H5G_info_t *);
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_group_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_group_close
 *
 * Purpose:	Closes a group.
 *
 * Return:	Success:	0
 *		Failure:	-1, group not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_group_close(void *_grp, hid_t dxpl_id, void **req)
{
    H5VL_iod_group_t *grp = (H5VL_iod_group_t *)_grp;
    group_close_in_t input;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(!do_async) {
        if(H5VL_iod_request_wait_some(grp->common.file, grp) < 0)
            HGOTO_ERROR(H5E_FILE,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    /* allocate an integer to receive the return value if the group close succeeded or not */
    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* set the parent axe id */
    if(grp->common.request)
        input.parent_axe_id = grp->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    input.iod_oh = grp->remote_group.iod_oh;
    input.iod_id = grp->remote_group.iod_id;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("Group Close IOD ID %llu, axe id %llu\n", 
           input.iod_id, input.axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_GROUP_CLOSE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship group close");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_GROUP_CLOSE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)grp;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(grp->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        grp->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(grp->common.file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_create
 *
 * Purpose:	Sends a request to the IOD to create a dataset
 *
 * Return:	Success:	dataset object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_dataset_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, 
                        hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the dataset */
    H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    dset_create_in_t input;
    H5P_genplist_t *plist;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    uint64_t parent_axe_id;
    char *new_name = NULL; /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hid_t type_id, space_id, lcpl_id;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the dcpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get datatype, dataspace, and lcpl IDs that were added in the dcpl at the API layer */
    if(H5P_get(plist, H5VL_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id");
    if(H5P_get(plist, H5VL_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id");
    if(H5P_get(plist, H5VL_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id");

    /* Retrieve the parent AXE id by traversing the path where the
       dataset should be created. */
    if(H5VL_iod_get_parent_info(obj, loc_params, name, &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    dset->remote_dset.iod_oh.cookie = IOD_OH_UNDEFINED;
    dset->remote_dset.iod_id = IOD_ID_UNDEFINED;

    /* Generate an IOD ID for the dset to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.array_oid_index, 
                        IOD_OBJ_ARRAY, &input.dset_id);
    dset->remote_dset.iod_id = input.dset_id;

    /* increment the index of ARRAY objects created on the container */
    obj->file->remote_file.array_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.name = new_name;
    input.dcpl_id = dcpl_id;
    input.dapl_id = dapl_id;
    input.lcpl_id = lcpl_id;
    input.type_id = type_id;
    input.space_id = space_id;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("Dataset Create %s IOD ID %llu, axe id %llu, parent %llu\n", 
           new_name, input.dset_id, input.axe_id, input.parent_axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DSET_CREATE_ID, &input, &dset->remote_dset, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship dataset create");

    /* setup the local dataset struct */
    /* store the entire path of the dataset locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (dset->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(dset->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(dset->common.obj_name+obj_name_len, name, name_len);
        dset->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* copy property lists, dtype, and dspace*/
    if((dset->remote_dset.dcpl_id = H5Pcopy(dcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dcpl");
    if((dset->dapl_id = H5Pcopy(dapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");
    if((dset->remote_dset.type_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");
    if((dset->remote_dset.space_id = H5Scopy(space_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dspace");

    /* set common object parameters */
    dset->common.obj_type = H5I_DATASET;
    dset->common.file = obj->file;
    dset->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DSET_CREATE;
    request->data = dset;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dset;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        dset->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        dset->common.request = NULL;
    } /* end else */

    ret_value = (void *)dset;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_open
 *
 * Purpose:	Sends a request to the IOD to open a dataset
 *
 * Return:	Success:	dataset object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_dataset_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                      hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the dataset */
    H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    dset_open_in_t input;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    uint64_t parent_axe_id;
    char *new_name = NULL; /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       dataset should be opened. */
    if(H5VL_iod_get_parent_info(obj, loc_params, name, &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    dset->remote_dset.iod_oh.cookie = IOD_OH_UNDEFINED;
    dset->remote_dset.iod_id = IOD_ID_UNDEFINED;
    dset->remote_dset.dcpl_id = -1;
    dset->remote_dset.type_id = -1;
    dset->remote_dset.space_id = -1;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.name = new_name;
    input.dapl_id = dapl_id;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("Dataset Open %s LOC ID %llu, axe id %llu, parent %llu\n", 
           new_name, input.loc_id, input.axe_id, input.parent_axe_id);
#endif

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DSET_OPEN_ID, &input, &dset->remote_dset, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship dataset open");

    /* setup the local dataset struct */
    /* store the entire path of the dataset locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (dset->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(dset->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(dset->common.obj_name+obj_name_len, name, name_len);
        dset->common.obj_name[obj_name_len+name_len] = '\0';
    }

#if H5VL_IOD_DEBUG
    printf("Dataset Open %s LOC ID %llu, axe id %llu, parent %llu\n", 
           dset->common.obj_name, input.loc_id, input.axe_id, input.parent_axe_id);
#endif

    if((dset->dapl_id = H5Pcopy(dapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

    /* set common object parameters */
    dset->common.obj_type = H5I_DATASET;
    dset->common.file = obj->file;
    dset->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DSET_OPEN;
    request->data = dset;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dset;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        dset->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        dset->common.request = NULL;
    } /* end else */

    ret_value = (void *)dset;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_read
 *
 * Purpose:	Reads raw data from a dataset into a buffer.
 *
 * Return:	Success:	0
 *		Failure:	-1, data not read.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_dataset_read(void *_dset, hid_t mem_type_id, hid_t mem_space_id,
                      hid_t file_space_id, hid_t dxpl_id, void *buf, void **req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    dset_io_in_t input;
    H5P_genplist_t *plist;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    hg_bulk_t *bulk_handle = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    H5VL_iod_read_status_t *status = NULL;
    const H5S_t *mem_space = NULL;
    const H5S_t *file_space = NULL;
    char fake_char;
    size_t buf_size;  /* size of the contiguous buffer */
    size_t type_size; /* size of mem type */
    size_t nelmts;    /* num elements in mem dataspace */
    H5VL_iod_io_info_t *info;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == dset->remote_dset.dcpl_id ||
       -1 == dset->remote_dset.type_id ||
       -1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dset->common.request = NULL;
    }

    /* check arguments */
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    } /* end if */

    if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");

    if(!buf)
        buf = &fake_char;

    /* set the parent axe id */
    if(dset->common.request)
        input.parent_axe_id = dset->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* get the memory type size */
    {
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");

        type_size = H5T_GET_SIZE(dt);
    }

    /* get the number of elements selcted in dataspace */
    nelmts = H5S_GET_SELECT_NPOINTS(mem_space);

    /* calculate the raw data size */
    buf_size = nelmts * type_size;

    /* allocate a bulk data transfer handle */
    if(NULL == (bulk_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a buld data transfer handle");

    /* If the memory selection is contiguous, create simple HG Bulk Handle */
    if(H5S_select_is_contiguous(mem_space)) {
        /* Register memory with bulk_handle */
        if(HG_SUCCESS != HG_Bulk_handle_create(buf, buf_size, HG_BULK_READWRITE, bulk_handle))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");
    }

    /* if the memory selection is non-contiguous, create a segmented selection */
    else {
        hsize_t *off = NULL; /* array that contains the memory addresses of the memory selection */
        size_t *len = NULL; /* array that contains the length of a contiguous block at each address */
        size_t count = 0; /* number of offset/length entries in selection */
        size_t i;
        hg_bulk_segment_t *bulk_segments = NULL;

        /* generate the offsets/lengths pair arrays from the memory dataspace selection */
        if(H5S_get_offsets(mem_space, type_size, nelmts, &off, &len, &count) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't retrieve offets/lengths of memory space");

        /* Register memory with segmented HG handle */
        bulk_segments = (hg_bulk_segment_t *)malloc(count * sizeof(hg_bulk_segment_t));
        for (i = 0; i < count ; i++) {
            hsize_t start_offset = (hsize_t)buf;

            bulk_segments[i].address = (void *)(start_offset + off[i]);
            bulk_segments[i].size = len[i];
        }

        /* create Bulk handle */
        if (HG_SUCCESS != HG_Bulk_handle_create_segments(bulk_segments, count, 
                                                         HG_BULK_READWRITE, bulk_handle))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");

        /* cleanup */
        if(count) {
            free(bulk_segments);
            bulk_segments = NULL;
            free(len);
            len = NULL;
            free(off);
            off = NULL;
        }
    }

    /* Fill input structure */
    input.coh = dset->common.file->remote_file.coh;
    input.iod_oh = dset->remote_dset.iod_oh;
    input.iod_id = dset->remote_dset.iod_id;
    input.bulk_handle = *bulk_handle;
    input.checksum = 0;
    input.dxpl_id = dxpl_id;
    input.space_id = file_space_id;
    input.dset_type_id = dset->remote_dset.type_id;
    input.mem_type_id = mem_type_id;
    input.axe_id = axe_id ++;

    /* allocate structure to receive status of read operation (contains return value and checksum */
    status = (H5VL_iod_read_status_t *)malloc(sizeof(H5VL_iod_read_status_t));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("Dataset Read, axe id %llu, parent %llu\n", 
           input.axe_id, input.parent_axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DSET_READ_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset read");

    /* setup info struct for I/O request. 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_calloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a request");
    info->status = status;
    info->bulk_handle = bulk_handle;
    info->buf_ptr = buf;
    info->nelmts = nelmts;
    info->type_size = type_size;
    info->cs_ptr = NULL;
    /* store a copy of the dataspace selection to be able to calculate the checksum later */
    if(NULL == (info->space = H5S_copy(mem_space, FALSE, TRUE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");
    /* Get the dxpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    /* store the pointer to the buffer where the checksum needs to be placed */
    if(H5P_get(plist, H5D_XFER_CHECKSUM_PTR_NAME, &info->cs_ptr) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum pointer value");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DSET_READ;
    request->data = info;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dset;
    request->status = 0;
    request->state = 0;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dset->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(dset->common.file, request) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_write
 *
 * Purpose:	Writes raw data from a buffer into a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not writed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_dataset_write(void *_dset, hid_t mem_type_id, hid_t mem_space_id,
                       hid_t file_space_id, hid_t dxpl_id, const void *buf, void **req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    dset_io_in_t input;
    H5P_genplist_t *plist;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    hg_bulk_t *bulk_handle = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    const H5S_t *mem_space = NULL;
    const H5S_t *file_space = NULL;
    char fake_char;
    int *status = NULL;
    size_t buf_size;  /* size of the contiguous buffer */
    size_t type_size; /* size of mem type */
    size_t nelmts;    /* num elements in mem dataspace */
    H5VL_iod_io_info_t *info;
    uint64_t parent_axe_id;
    uint32_t internal_cs;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == dset->remote_dset.dcpl_id ||
       -1 == dset->remote_dset.type_id ||
       -1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dset->common.request = NULL;
    }

    /* check arguments */
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    } /* end if */

    if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");

    if(!buf)
        buf = &fake_char;

    /* set the parent axe id */
    if(dset->common.request)
        input.parent_axe_id = dset->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* get the memory type size */
    {
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");

        type_size = H5T_GET_SIZE(dt);
    }

    /* get the number of elements selcted in dataspace */
    nelmts = H5S_GET_SELECT_NPOINTS(mem_space);

    /* calculate the raw data size */
    buf_size = nelmts * type_size;

    /* calculate a checksum for the data */
    internal_cs = H5S_checksum(buf, type_size, nelmts, mem_space);

    /* Verify the checksum value if the dxpl contains a user defined checksum */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        uint32_t user_cs;

        /* Get the dcpl plist structure */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

        if(H5P_get(plist, H5D_XFER_CHECKSUM_NAME, &user_cs) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum value");

        if(user_cs != internal_cs) {
            fprintf(stderr, "Errrr.. In memory Data corruption. expecting %u, got %u\n",
                    user_cs, internal_cs);
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "Checksum verification failed");
        }
    }

    /* allocate a bulk data transfer handle */
    if(NULL == (bulk_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a buld data transfer handle");

    /* If the memory selection is contiguous, create simple HG Bulk Handle */
    if(H5S_select_is_contiguous(mem_space)) {
        /* Register memory with bulk_handle */
        if(HG_SUCCESS != HG_Bulk_handle_create(buf, buf_size, HG_BULK_READ_ONLY, bulk_handle))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");
    }

    /* if the memory selection is non-contiguous, create a segmented selection */
    else {
        hsize_t *off = NULL; /* array that contains the memory addresses of the memory selection */
        size_t *len = NULL; /* array that contains the length of a contiguous block at each address */
        size_t count = 0; /* number of offset/length entries in selection */
        size_t i;
        hg_bulk_segment_t *bulk_segments = NULL;

        /* generate the offsets/lengths pair arrays from the memory dataspace selection */
        if(H5S_get_offsets(mem_space, type_size, nelmts, &off, &len, &count) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't retrieve offets/lengths of memory space");

        /* Register memory with segmented HG handle */
        bulk_segments = (hg_bulk_segment_t *)malloc(count * sizeof(hg_bulk_segment_t));
        for (i = 0; i < count ; i++) {
            hsize_t start_offset = (hsize_t)buf;

            bulk_segments[i].address = (void *)(start_offset + off[i]);
            bulk_segments[i].size = len[i];
        }

        /* create Bulk handle */
        if (HG_SUCCESS != HG_Bulk_handle_create_segments(bulk_segments, count, 
                                                         HG_BULK_READWRITE, bulk_handle))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");

        /* cleanup */
        if(count) {
            free(bulk_segments);
            bulk_segments = NULL;
            free(len);
            len = NULL;
            free(off);
            off = NULL;
        }
    }

    /* Fill input structure */
    input.coh = dset->common.file->remote_file.coh;
    input.iod_oh = dset->remote_dset.iod_oh;
    input.iod_id = dset->remote_dset.iod_id;
    input.bulk_handle = *bulk_handle;
    input.checksum = internal_cs;
    input.dxpl_id = dxpl_id;
    input.space_id = file_space_id;
    input.dset_type_id = dset->remote_dset.type_id;
    input.mem_type_id = mem_type_id;
    input.axe_id = axe_id ++;

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("Dataset Write, axe id %llu, parent %llu\n", 
           input.axe_id, input.parent_axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DSET_WRITE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset write");

    /* setup info struct for I/O request 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_calloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a request");
    info->status = status;
    info->bulk_handle = bulk_handle;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DSET_WRITE;
    request->data = info;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dset;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dset->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(dset->common.file, request) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_set_extent
 *
 * Purpose:	Set Extent of dataset
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_dataset_set_extent(void *_dset, const hsize_t size[], hid_t dxpl_id, void **req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    dset_set_extent_in_t input;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    int *status = NULL;
    uint64_t parent_axe_id;
    size_t num_parents;
    uint64_t *axe_parents = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dset->common.request = NULL;
    }

    /* retrieve the parent axe ids array */
    if(H5VL_iod_get_axe_parents((H5VL_iod_object_t *)dset, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get num AXE parents");

    if(num_parents) {
        if(NULL == (axe_parents = (uint64_t *)H5MM_malloc(sizeof(uint64_t) * num_parents)))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate num parents array");
        if(H5VL_iod_get_axe_parents((H5VL_iod_object_t *)dset, &num_parents, axe_parents) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get AXE parents");
    }

    /* Fill input structure */
    input.coh = dset->common.file->remote_file.coh;
    input.iod_oh = dset->remote_dset.iod_oh;
    input.iod_id = dset->remote_dset.iod_id;
    input.dims.rank = H5Sget_simple_extent_ndims(dset->remote_dset.space_id);
    input.dims.size = size;
    input.parent_axe_ids.count = num_parents;
    input.parent_axe_ids.ids = axe_parents;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    {
        size_t i;

        printf("Dataset Set Extent, axe id %llu, %d parents: ", 
               input.axe_id, num_parents);
        for(i=0 ; i<num_parents ; i++)
            printf("%llu ", axe_parents[i]);
        printf("\n");
    }
#endif

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DSET_SET_EXTENT_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset write");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DSET_SET_EXTENT;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dset;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dset->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        dset->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(dset->common.file, request) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

    /* modify the local dataspace of the dataset */
    {
        int     rank;                       /* Dataspace # of dimensions */
        H5S_t   *space;                     /* Dataset's dataspace */
        hsize_t curr_dims[H5O_LAYOUT_NDIMS];/* Current dimension sizes */

	if(NULL == (space = (H5S_t *)H5I_object_verify(dset->remote_dset.space_id, 
                                                             H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

        /* Check if we are shrinking or expanding any of the dimensions */
        if((rank = H5S_get_simple_extent_dims(space, curr_dims, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataset dimensions");

        /* Modify the size of the data space */
        if(H5S_set_extent(space, size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to modify size of data space");
    }
done:
    axe_parents = (uint64_t *)H5MM_xfree(axe_parents);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_set_extent() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_get
 *
 * Purpose:	Gets certain information about a dataset
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_dataset_get(void *_dset, H5VL_dataset_get_t get_type, hid_t dxpl_id, 
                     void **req, va_list arguments)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(-1 == dset->remote_dset.dcpl_id ||
       -1 == dset->remote_dset.type_id ||
       -1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dset->common.request = NULL;
    }

    switch (get_type) {
        case H5VL_DATASET_GET_DCPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(dset->remote_dset.dcpl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dset creation property list")

                break;
            }
        case H5VL_DATASET_GET_DAPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(dset->dapl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dset access property list")

                break;
            }
        case H5VL_DATASET_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5Scopy(dset->remote_dset.space_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get dataspace ID of dataset");
                break;
            }
        case H5VL_DATASET_GET_SPACE_STATUS:
            {
                H5D_space_status_t *allocation = va_arg (arguments, H5D_space_status_t *);

                *allocation = H5D_SPACE_STATUS_NOT_ALLOCATED;
                break;
            }
        case H5VL_DATASET_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5Tcopy(dset->remote_dset.type_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of dataset")
            }
        case H5VL_DATASET_GET_STORAGE_SIZE:
        case H5VL_DATASET_GET_OFFSET:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_close
 *
 * Purpose:	Closes a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_dataset_close(void *_dset, hid_t dxpl_id, void **req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    dset_close_in_t input;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    size_t num_parents;
    uint64_t *axe_parents = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(-1 == dset->remote_dset.dcpl_id ||
       -1 == dset->remote_dset.type_id ||
       -1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait dset request");
        dset->common.request = NULL;
    }

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(!do_async) {
        if(H5VL_iod_request_wait_some(dset->common.file, dset) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    /* determine the parent axe IDs array for this operation*/
    if(H5VL_iod_get_axe_parents((H5VL_iod_object_t *)dset, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get num AXE parents");
    if(num_parents) {
        if(NULL == (axe_parents = (uint64_t *)H5MM_malloc(sizeof(uint64_t) * num_parents)))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate num parents array");
        if(H5VL_iod_get_axe_parents((H5VL_iod_object_t *)dset, &num_parents, axe_parents) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get AXE parents");
    }

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    input.iod_oh = dset->remote_dset.iod_oh;
    input.iod_id = dset->remote_dset.iod_id;
    input.parent_axe_ids.count = num_parents;
    input.parent_axe_ids.ids = axe_parents;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    {
        size_t i;

        printf("Dataset Close %s, axe id %llu, %d parents: ", 
               dset->common.obj_name, input.axe_id, num_parents);
        for(i=0 ; i<num_parents ; i++)
            printf("%llu ", axe_parents[i]);
        printf("\n");
    }
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DSET_CLOSE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship dset close");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DSET_CLOSE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dset;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dset->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        dset->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(dset->common.file, request) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    axe_parents = (uint64_t *)H5MM_xfree(axe_parents);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_commit
 *
 * Purpose:	Commits a datatype inside the container.
 *
 * Return:	Success:	datatype
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_datatype_commit(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                         hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, 
                         hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the datatype */
    H5VL_iod_dtype_t  *dtype = NULL; /* the datatype object that is created and passed to the user */
    H5T_t *dt = NULL;
    dtype_commit_in_t input;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    char *new_name = NULL; /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    uint64_t parent_axe_id;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       dtype should be created. */
    if(H5VL_iod_get_parent_info(obj, loc_params, name, &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the datatype object that is returned to the user */
    if(NULL == (dtype = H5FL_CALLOC(H5VL_iod_dtype_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    dtype->remote_dtype.iod_oh.cookie = IOD_OH_UNDEFINED;
    dtype->remote_dtype.iod_id = IOD_ID_UNDEFINED;

    /* Generate an IOD ID for the group to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.blob_oid_index, 
                        IOD_OBJ_BLOB, &input.dtype_id);
    dtype->remote_dtype.iod_id = input.dtype_id;

    /* increment the index of KV objects created on the container */
    obj->file->remote_file.blob_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.name = new_name;
    input.tcpl_id = tcpl_id;
    input.tapl_id = tapl_id;
    input.lcpl_id = lcpl_id;
    input.type_id = type_id;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("Datatype Commit %s IOD ID %llu, axe id %llu, parent %llu\n", 
           new_name, input.dtype_id, input.axe_id, input.parent_axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DTYPE_COMMIT_ID, &input, &dtype->remote_dtype, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship dataset create");

    /* setup the local datatype struct */
    /* store the entire path of the datatype locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (dtype->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(dtype->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(dtype->common.obj_name+obj_name_len, name, name_len);
        dtype->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* store a copy of the datatype parameters*/
    if((dtype->remote_dtype.tcpl_id = H5Pcopy(tcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dcpl");
    if((dtype->tapl_id = H5Pcopy(tapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");
    if((dtype->remote_dtype.type_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");

    /* set common object parameters */
    dtype->common.obj_type = H5I_DATASET;
    dtype->common.file = obj->file;
    dtype->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DTYPE_COMMIT;
    request->data = dtype;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dtype;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        dtype->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        dtype->common.request = NULL;
    } /* end else */

    ret_value = (void *)dtype;
done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_open
 *
 * Purpose:	Opens a named datatype.
 *
 * Return:	Success:	datatype id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_datatype_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                       hid_t tapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the datatype */
    H5VL_iod_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */
    dtype_open_in_t input;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    char *new_name = NULL; /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    uint64_t parent_axe_id;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       dtype should be opened. */
    if(H5VL_iod_get_parent_info(obj, loc_params, name, &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the datatype object that is returned to the user */
    if(NULL == (dtype = H5FL_CALLOC(H5VL_iod_dtype_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    dtype->remote_dtype.iod_oh.cookie = IOD_OH_UNDEFINED;
    dtype->remote_dtype.iod_id = IOD_ID_UNDEFINED;
    dtype->remote_dtype.tcpl_id = -1;
    dtype->remote_dtype.type_id = -1;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.name = new_name;
    input.tapl_id = tapl_id;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("Datatype Open %s LOC ID %llu, axe id %llu, parent %llu\n", 
           new_name, input.loc_id, input.axe_id, input.parent_axe_id);
#endif

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DTYPE_OPEN_ID, &input, &dtype->remote_dtype, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship datatype open");

    /* setup the local datatype struct */
    /* store the entire path of the datatype locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (dtype->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(dtype->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(dtype->common.obj_name+obj_name_len, name, name_len);
        dtype->common.obj_name[obj_name_len+name_len] = '\0';
    }

    if((dtype->tapl_id = H5Pcopy(tapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy tapl");

    /* set common object parameters */
    dtype->common.obj_type = H5I_DATATYPE;
    dtype->common.file = obj->file;
    dtype->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DTYPE_OPEN;
    request->data = dtype;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dtype;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        dtype->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        dtype->common.request = NULL;
    } /* end else */

    ret_value = (void *)dtype;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_get_binary
 *
 * Purpose:	gets size required to encode the datatype
 *
 * Return:	Success:	datatype id. 
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5VL_iod_datatype_get_binary(void *obj, unsigned char *buf, size_t size, 
                             hid_t UNUSED dxpl_id, void UNUSED **req)
{
    H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)obj;
    size_t       nalloc = size;
    ssize_t      ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5Tencode(dtype->remote_dtype.type_id, buf, &nalloc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't determine serialized length of datatype")

    ret_value = (ssize_t) nalloc;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_get_binary() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_get
 *
 * Purpose:	Gets certain information about a datatype
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_datatype_get(void UNUSED *obj, H5VL_datatype_get_t get_type, 
                      hid_t UNUSED dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Tget_create_plist */
        case H5VL_DATATYPE_GET_TCPL:
            {
                /* Nothing to return here */
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from datatype")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_close
 *
 * Purpose:	Closes an datatype.
 *
 * Return:	Success:	0
 *		Failure:	-1, datatype not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_datatype_close(void *obj, hid_t dxpl_id, void **req)
{
    H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)obj;
    dtype_close_in_t input;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(!do_async) {
        if(H5VL_iod_request_wait_some(dtype->common.file, dtype) < 0)
            HGOTO_ERROR(H5E_FILE,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    /* set the parent axe id */
    if(dtype->common.request)
        input.parent_axe_id = dtype->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    input.iod_oh = dtype->remote_dtype.iod_oh;
    input.iod_id = dtype->remote_dtype.iod_id;
    input.axe_id = axe_id ++;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_DTYPE_CLOSE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship dtype close");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_DTYPE_CLOSE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)dtype;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dtype->common.file, request);

#if H5VL_IOD_DEBUG
    printf("Datatype Close %s, axe id %llu, parent %d\n", 
           dtype->common.obj_name, input.axe_id, input.parent_axe_id);
#endif

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        dtype->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(dtype->common.file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_create
 *
 * Purpose:	Sends a request to the IOD to create a attribute
 *
 * Return:	Success:	attribute object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_attribute_create(void *_obj, H5VL_loc_params_t loc_params, const char *attr_name, 
                          hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the attribute */
    H5VL_iod_attr_t *attr = NULL; /* the attribute object that is created and passed to the user */
    attr_create_in_t input;
    H5P_genplist_t *plist;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    char *new_name = NULL;   /* resolved path to where we need to start traversal at the server */
    const char *path; /* path on where the traversal starts relative to the location object specified */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hid_t type_id, space_id;
    uint64_t parent_axe_id;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the acpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get datatype, dataspace, and lcpl IDs that were added in the acpl at the API layer */
    if(H5P_get(plist, H5VL_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id");
    if(H5P_get(plist, H5VL_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id");

    /* Retrieve the parent AXE id by traversing the path where the
       attribute should be created. */
    if(H5VL_iod_get_parent_info(obj, loc_params, ".", &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the attribute object that is returned to the user */
    if(NULL == (attr = H5FL_CALLOC(H5VL_iod_attr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    attr->remote_attr.iod_oh.cookie = IOD_OH_UNDEFINED;
    attr->remote_attr.iod_id = IOD_ID_UNDEFINED;

    /* Generate an IOD ID for the attr to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.array_oid_index, 
                        IOD_OBJ_ARRAY, &input.attr_id);
    attr->remote_attr.iod_id = input.attr_id;

    /* increment the index of ARRAY objects created on the container */
    obj->file->remote_file.array_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.path = new_name;
    input.attr_name = attr_name;
    input.acpl_id = acpl_id;
    input.type_id = type_id;
    input.space_id = space_id;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_ATTR_CREATE_ID, &input, &attr->remote_attr, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship attribute create");

    /* setup the local attribute struct */

    /* store the entire path of the attribute locally */
    if(loc_params.type == H5VL_OBJECT_BY_SELF) {
        path = NULL;
        attr->loc_name = HDstrdup(obj->obj_name);
    }
    else if (loc_params.type == H5VL_OBJECT_BY_NAME) {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len;

        path = loc_params.loc_data.loc_by_name.name;
        name_len = HDstrlen(path);

        if (NULL == (attr->loc_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(attr->loc_name, obj->obj_name, obj_name_len);
        HDmemcpy(attr->loc_name+obj_name_len, path, name_len);
        attr->loc_name[obj_name_len+name_len] = '\0';
    }

    /* store the name of the attribute locally */
    attr->common.obj_name = strdup(attr_name);

#if H5VL_IOD_DEBUG
    printf("Attribute Create %s IOD ID %llu, axe id %llu, parent %llu\n", 
           attr_name, input.attr_id, input.axe_id, input.parent_axe_id);
#endif

    /* copy property lists, dtype, and dspace*/
    if((attr->remote_attr.acpl_id = H5Pcopy(acpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy acpl");
    if((attr->remote_attr.type_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");
    if((attr->remote_attr.space_id = H5Scopy(space_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dspace");

    /* set common object parameters */
    attr->common.obj_type = H5I_ATTR;
    attr->common.file = obj->file;
    attr->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_ATTR_CREATE;
    request->data = attr;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)attr;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        attr->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        attr->common.request = NULL;
    } /* end else */

    ret_value = (void *)attr;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_open
 *
 * Purpose:	Sends a request to the IOD to open a attribute
 *
 * Return:	Success:	attribute object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_attribute_open(void *_obj, H5VL_loc_params_t loc_params, const char *attr_name, 
                        hid_t aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the attribute */
    H5VL_iod_attr_t *attr = NULL; /* the attribute object that is created and passed to the user */
    attr_open_in_t input;
    char *new_name = NULL;   /* resolved path to where we need to start traversal at the server */
    const char *path; /* path on where the traversal starts relative to the location object specified */
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    uint64_t parent_axe_id;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       attribute should be opened. */
    if(H5VL_iod_get_parent_info(obj, loc_params, ".", &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the attribute object that is returned to the user */
    if(NULL == (attr = H5FL_CALLOC(H5VL_iod_attr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    attr->remote_attr.iod_oh.cookie = IOD_OH_UNDEFINED;
    attr->remote_attr.iod_id = IOD_ID_UNDEFINED;
    attr->remote_attr.acpl_id = -1;
    attr->remote_attr.type_id = -1;
    attr->remote_attr.space_id = -1;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.path = new_name;
    input.attr_name = attr_name;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("Attribute Open %s LOC ID %llu, axe id %llu, parent %llu\n", 
           attr_name, input.loc_id, input.axe_id, input.parent_axe_id);
#endif

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_ATTR_OPEN_ID, &input, &attr->remote_attr, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship attribute open");

    /* setup the local attribute struct */

    /* store the entire path of the attribute locally */
    if(loc_params.type == H5VL_OBJECT_BY_SELF) {
        path = NULL;
        attr->loc_name = HDstrdup(obj->obj_name);
    }
    else if (loc_params.type == H5VL_OBJECT_BY_NAME) {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len;

        path = loc_params.loc_data.loc_by_name.name;
        name_len = HDstrlen(path);

        if (NULL == (attr->loc_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(attr->loc_name, obj->obj_name, obj_name_len);
        HDmemcpy(attr->loc_name+obj_name_len, path, name_len);
        attr->loc_name[obj_name_len+name_len] = '\0';
    }

    /* store the name of the attribute locally */
    attr->common.obj_name = strdup(attr_name);

    /* set common object parameters */
    attr->common.obj_type = H5I_ATTR;
    attr->common.file = obj->file;
    attr->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_ATTR_OPEN;
    request->data = attr;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)attr;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        attr->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        attr->common.request = NULL;
    } /* end else */

    ret_value = (void *)attr;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_read
 *
 * Purpose:	Reads raw data from a attribute into a buffer.
 *
 * Return:	Success:	0
 *		Failure:	-1, data not read.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_attribute_read(void *_attr, hid_t type_id, void *buf, hid_t dxpl_id, void **req)
{
    H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)_attr;
    attr_io_in_t input;
    H5P_genplist_t *plist;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    hg_bulk_t *bulk_handle = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    H5VL_iod_read_status_t *status = NULL;
    size_t size;
    H5VL_iod_io_info_t *info;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(-1 == attr->remote_attr.space_id) {
        HDassert(attr->common.request);
        /* Synchronously wait on the request attached to the attribute */
        if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        attr->common.request = NULL;
    }

    /* set the parent axe id */
    if(attr->common.request)
        input.parent_axe_id = attr->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* calculate the size of the buffer needed */
    size = H5Sget_simple_extent_npoints(attr->remote_attr.space_id) * H5Tget_size(type_id);

    /* allocate a bulk data transfer handle */
    if(NULL == (bulk_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a buld data transfer handle");

    /* Register memory with bulk_handle */
    if(HG_SUCCESS != HG_Bulk_handle_create(buf, size, HG_BULK_READWRITE, bulk_handle))
        HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "can't create Bulk Data Handle");

    /* Fill input structure */
    input.coh = attr->common.file->remote_file.coh;
    input.iod_oh = attr->remote_attr.iod_oh;
    input.iod_id = attr->remote_attr.iod_id;
    input.bulk_handle = *bulk_handle;
    input.type_id = type_id;
    input.axe_id = axe_id ++;

    /* allocate structure to receive status of read operation (contains return value and checksum */
    status = (H5VL_iod_read_status_t *)malloc(sizeof(H5VL_iod_read_status_t));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_ATTR_READ_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship attribute read");

    /* setup info struct for I/O request. 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_malloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a request");
    info->status = status;
    info->bulk_handle = bulk_handle;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_ATTR_READ;
    request->data = info;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)attr;
    request->status = 0;
    request->state = 0;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(attr->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(attr->common.file, request) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_write
 *
 * Purpose:	Writes raw data from a buffer into a attribute.
 *
 * Return:	Success:	0
 *		Failure:	-1, attribute not writed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_attribute_write(void *_attr, hid_t type_id, const void *buf, hid_t dxpl_id, void **req)
{
    H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)_attr;
    attr_io_in_t input;
    H5P_genplist_t *plist;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    hg_bulk_t *bulk_handle = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    int *status = NULL;
    size_t size;
    H5VL_iod_io_info_t *info;
    uint32_t cs;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(-1 == attr->remote_attr.space_id) {
        /* Synchronously wait on the request attached to the attribute */
        if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        attr->common.request = NULL;
    }

    /* set the parent axe id */
    if(attr->common.request)
        input.parent_axe_id = attr->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* calculate the size of the buffer needed */
    size = H5Sget_simple_extent_npoints(attr->remote_attr.space_id) * H5Tget_size(type_id);

    /* calculate a checksum for the data */
    cs = H5_checksum_fletcher32(buf, size);

    /* MSC- store it in a global variable for now so that the read can see it (for demo purposes */
    printf("Checksum Generated for attribute data at client: %u\n", cs);

    /* allocate a bulk data transfer handle */
    if(NULL == (bulk_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a bulk data transfer handle");

    /* Register memory */
    if(HG_SUCCESS != HG_Bulk_handle_create(buf, size, HG_BULK_READ_ONLY, bulk_handle))
        HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "can't create Bulk Data Handle");

    /* Fill input structure */
    input.coh = attr->common.file->remote_file.coh;
    input.iod_oh = attr->remote_attr.iod_oh;
    input.iod_id = attr->remote_attr.iod_id;
    input.bulk_handle = *bulk_handle;
    input.type_id = type_id;
    input.axe_id = axe_id ++;

    status = (int *)malloc(sizeof(int));

    /* Get the dxpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_BADATOM, FAIL, "can't find object for ID")

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_ATTR_WRITE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship attribute write");

    /* setup info struct for I/O request 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_malloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a request");
    info->status = status;
    info->bulk_handle = bulk_handle;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_ATTR_WRITE;
    request->data = info;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)attr;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(attr->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(attr->common.file, request) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_remove
 *
 * Purpose:	Set Extent of attribute
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_attribute_remove(void *_obj, H5VL_loc_params_t loc_params, const char *attr_name, 
                          hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the attribute */
    attr_op_in_t input;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    char *new_name = NULL;   /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    int *status = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       attribute should be removed. */
    if(H5VL_iod_get_parent_info(obj, loc_params, ".", &iod_id, &iod_oh, 
                                &input.parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.path = new_name;
    input.attr_name = attr_name;
    input.axe_id = axe_id ++;

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_ATTR_REMOVE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship attribute remove");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_ATTR_REMOVE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)obj;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        obj->request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_get
 *
 * Purpose:	Gets certain information about a attribute
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_attribute_get(void *_obj, H5VL_attr_get_t get_type, hid_t dxpl_id, 
                       void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location of operation */
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t  ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Aget_space */
        case H5VL_ATTR_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)obj;

                if(-1 == attr->remote_attr.space_id) {
                    /* Synchronously wait on the request attached to the attribute */
                    if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    attr->common.request = NULL;
                }

                if((*ret_id = H5Scopy(attr->remote_attr.space_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get dataspace ID of attribute")
                break;
            }
        /* H5Aget_type */
        case H5VL_ATTR_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)obj;

                if(-1 == attr->remote_attr.type_id) {
                    /* Synchronously wait on the request attached to the attribute */
                    if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    attr->common.request = NULL;
                }

                if((*ret_id = H5Tcopy(attr->remote_attr.type_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of attribute")
                break;
            }
        /* H5Aget_create_plist */
        case H5VL_ATTR_GET_ACPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)obj;

                if(-1 == attr->remote_attr.acpl_id) {
                    /* Synchronously wait on the request attached to the attribute */
                    if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    attr->common.request = NULL;
                }

                /* Retrieve the file's access property list */
                if((*ret_id = H5Pcopy(attr->remote_attr.acpl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get attr creation property list");
                break;
            }
        /* H5Aget_name */
        case H5VL_ATTR_GET_NAME:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t	buf_size = va_arg (arguments, size_t);
                char    *buf = va_arg (arguments, char *);
                ssize_t	*ret_val = va_arg (arguments, ssize_t *);
                H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)obj;

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    size_t copy_len, nbytes;

                    nbytes = HDstrlen(attr->common.obj_name);
                    HDassert((ssize_t)nbytes >= 0); /*overflow, pretty unlikely --rpm*/

                    /* compute the string length which will fit into the user's buffer */
                    copy_len = MIN(buf_size - 1, nbytes);

                    /* Copy all/some of the name */
                    if(buf && copy_len > 0) {
                        HDmemcpy(buf, attr->common.obj_name, copy_len);

                        /* Terminate the string */
                        buf[copy_len]='\0';
                    } /* end if */
                    *ret_val = (ssize_t)nbytes;
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get name of attr");
                }
                break;
            }
        /* H5Aexists/exists_by_name */
        case H5VL_ATTR_EXISTS:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                char *attr_name = va_arg (arguments, char *);
                htri_t *ret = va_arg (arguments, htri_t *);
                char *new_name = NULL;
                attr_op_in_t input;

                /* Retrieve the parent AXE id by traversing the path where the
                   attribute should be checked. */
                if(H5VL_iod_get_parent_info(obj, loc_params, ".", &iod_id, &iod_oh, 
                                            &input.parent_axe_id, &new_name, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.loc_id = iod_id;
                input.loc_oh = iod_oh;
                input.path = new_name;
                input.attr_name = attr_name;
                input.axe_id = axe_id ++;

                /* get a function shipper request */
                if(do_async) {
                    if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
                        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
                } /* end if */
                else
                    hg_req = &_hg_req;

                /* forward the call to the IONs */
                if(HG_Forward(PEER, H5VL_ATTR_EXISTS_ID, &input, ret, hg_req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship attribute exists");

                /* Get async request for operation */
                if(do_async) {
                    if(NULL == (request = (H5VL_iod_request_t *)
                                H5MM_malloc(sizeof(H5VL_iod_request_t))))
                        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
                } /* end if */
                else
                    request = &_request;

                /* Set up request */
                HDmemset(request, 0, sizeof(*request));
                request->type = HG_ATTR_EXISTS;
                request->data = ret;
                request->req = hg_req;
                request->obj = (H5VL_iod_object_t *)obj;
                request->axe_id = input.axe_id;
                request->next = request->prev = NULL;
                /* add request to container's linked list */
                H5VL_iod_request_add(obj->file, request);

                /* Store/wait on request */
                if(do_async) {
                    /* Sanity check */
                    HDassert(request != &_request);
                    *req = request;
                    /* Track request */
                    obj->request = request;
                } /* end if */
                else {
                    /* Synchronously wait on the request */
                    if(H5VL_iod_request_wait(obj->file, request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    /* Sanity check */
                    HDassert(request == &_request);
                } /* end else */

                if(new_name) free(new_name);
                break;
            }
        /* H5Aget_info */
        case H5VL_ATTR_GET_INFO:
            {
#if 0
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5A_info_t *ainfo = va_arg (arguments, H5A_info_t *);

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr info")
                }
                else if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    char *attr_name = va_arg (arguments, char *);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr info")
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr info")
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr info")
#endif
                break;
            }
        case H5VL_ATTR_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr storage size");
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_close
 *
 * Purpose:	Closes a attribute.
 *
 * Return:	Success:	0
 *		Failure:	-1, attribute not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_attribute_close(void *_attr, hid_t dxpl_id, void **req)
{
    H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)_attr;
    attr_close_in_t input;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    size_t num_parents;
    uint64_t *axe_parents = NULL;
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(-1 == attr->remote_attr.acpl_id ||
       -1 == attr->remote_attr.type_id ||
       -1 == attr->remote_attr.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        attr->common.request = NULL;
    }

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(!do_async) {
        if(H5VL_iod_request_wait_some(attr->common.file, attr) < 0)
            HGOTO_ERROR(H5E_FILE,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    /* get all the parents required to complete before this operation can start */
    if(H5VL_iod_get_axe_parents((H5VL_iod_object_t *)attr, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get num AXE parents");
    if(num_parents) {
        if(NULL == (axe_parents = (uint64_t *)H5MM_malloc(sizeof(uint64_t) * num_parents)))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate num parents array");
        if(H5VL_iod_get_axe_parents((H5VL_iod_object_t *)attr, &num_parents, axe_parents) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get AXE parents");
    }

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    input.iod_oh = attr->remote_attr.iod_oh;
    input.iod_id = attr->remote_attr.iod_id;
    input.parent_axe_ids.count = num_parents;
    input.parent_axe_ids.ids = axe_parents;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    {
        size_t i;

        printf("Attribute Close, axe id %llu, %d parents: ", 
               input.axe_id, num_parents);
        for(i=0 ; i<num_parents ; i++)
            printf("%llu ", axe_parents[i]);
        printf("\n");
    }
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_ATTR_CLOSE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship attr close");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_ATTR_CLOSE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)attr;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(attr->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        attr->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(attr->common.file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    axe_parents = (uint64_t *)H5MM_xfree(axe_parents);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_create
 *
 * Purpose:	Creates an hard/soft/UD/external links.
 *              For now, only Hard and Soft Links are Supported.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_link_create(H5VL_link_create_type_t create_type, void *_obj, H5VL_loc_params_t loc_params,
                     hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object */
    H5VL_iod_object_t *cur_obj = NULL;
    link_create_in_t input;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    H5P_genplist_t    *plist;                      /* Property list pointer */
    char *loc_name = NULL, *new_name = NULL;
    herr_t             ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    switch (create_type) {
        case H5VL_LINK_CREATE_HARD:
            {
                H5VL_iod_object_t *target_obj = NULL;
                H5VL_loc_params_t target_params;

                if(H5P_get(plist, H5VL_LINK_TARGET, &target_obj) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current location");
                if(H5P_get(plist, H5VL_LINK_TARGET_LOC_PARAMS, &target_params) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current name");

                /* object is H5L_SAME_LOC */
                if(NULL == obj && target_obj) {
                    obj = target_obj;
                }

                /* Retrieve the parent info by traversing the path where the
                   link should be created from. */
                if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                            &input.parent_axe_id, &loc_name, &cur_obj) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* object is H5L_SAME_LOC */
                if(NULL == target_obj && obj) {
                    target_obj = obj;
                }
                /* Retrieve the parent info by traversing the path where the
                   link should be created. */
                if(H5VL_iod_get_parent_info(target_obj, target_params, ".", 
                                            &input.target_loc_id, &input.target_loc_oh, 
                                            &input.target_parent_axe_id, &new_name, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* set the input structure for the HG encode routine */
                input.create_type = H5VL_LINK_CREATE_HARD;
                if(obj)
                    input.coh = obj->file->remote_file.coh;
                else
                    input.coh = target_obj->file->remote_file.coh;

                input.lcpl_id = lcpl_id;
                input.lapl_id = lapl_id;
                input.loc_name = loc_name;
                input.target_name = new_name;
                input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
                printf("Hard Link Create axe %llu: %s ID %llu axe %llu to %s ID %llu axe %llu\n", 
                       input.axe_id, loc_name, input.loc_id, input.parent_axe_id,
                       new_name, input.target_loc_id, input.target_parent_axe_id);
#endif

                break;
            }
        case H5VL_LINK_CREATE_SOFT:
            {
                char *target_name;
                H5VL_iod_object_t *target_obj = NULL;
                H5VL_loc_params_t target_params;

                if(H5P_get(plist, H5VL_LINK_TARGET_NAME, &target_name) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for targe name");

                target_params.type = H5VL_OBJECT_BY_NAME;
                target_params.loc_data.loc_by_name.name = target_name;
                target_params.loc_data.loc_by_name.plist_id = lapl_id;

                /* determine target object whether it is absolute or
                   relative to the location object */
                if('/' == *target_name)
                    target_obj = (H5VL_iod_object_t *)obj->file;
                else
                    target_obj = obj;


                /* Retrieve the parent info by traversing the path where the
                   link should be created from. */
                if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                            &input.parent_axe_id, &loc_name, &cur_obj) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* Retrieve the parent info by traversing the path where the
                   target link should be created. */
                if(H5VL_iod_get_parent_info(target_obj, target_params, ".", 
                                            &input.target_loc_id, &input.target_loc_oh, 
                                            &input.target_parent_axe_id, &new_name, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* set the input structure for the HG encode routine */
                input.create_type = H5VL_LINK_CREATE_SOFT;
                input.coh = obj->file->remote_file.coh;

                input.axe_id = axe_id ++;
                input.lcpl_id = lcpl_id;
                input.lapl_id = lapl_id;
                input.loc_name = loc_name;
                input.target_name = new_name;

#if H5VL_IOD_DEBUG
                printf("Soft Link Create axe %llu: %s ID %llu axe %llu to %s ID %llu axe %llu\n", 
                       input.axe_id, loc_name, input.loc_id, input.parent_axe_id,
                       new_name, input.target_loc_id, input.target_parent_axe_id);
#endif

                break;
            }
        /* MSC - not supported now */
        case H5VL_LINK_CREATE_UD:
            {
                H5L_type_t link_type;
                void *udata;
                size_t udata_size;

                if(H5P_get(plist, H5VL_LINK_TYPE, &link_type) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for link type");
                if(H5P_get(plist, H5VL_LINK_UDATA, &udata) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for udata");
                if(H5P_get(plist, H5VL_LINK_UDATA_SIZE, &udata_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for udata size");
            }
        default:
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "invalid link creation call")
    }

    status = (herr_t *)malloc(sizeof(herr_t));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_LINK_CREATE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship link create");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc
                    (sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_LINK_CREATE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)cur_obj;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        cur_obj->request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    if(loc_name) free(loc_name);
    if(new_name) free(new_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_move
 *
 * Purpose:	Renames an object within an HDF5 file and moves it to a new
 *              group.  The original name SRC is unlinked from the group graph
 *              and then inserted with the new name DST (which can specify a
 *              new path for the object) as an atomic operation. The names
 *              are interpreted relative to SRC_LOC_ID and
 *              DST_LOC_ID, which are either file IDs or group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_link_move(void *_src_obj, H5VL_loc_params_t loc_params1, 
                   void *_dst_obj, H5VL_loc_params_t loc_params2,
                   hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, 
                   hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *src_obj = (H5VL_iod_object_t *)_src_obj;
    H5VL_iod_object_t *dst_obj = (H5VL_iod_object_t *)_dst_obj;
    H5VL_iod_object_t *cur_obj;
    link_move_in_t input;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    char *src_name = NULL, *dst_name = NULL;
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent information by traversing the path where the
       link should be moved from. */
    if(H5VL_iod_get_parent_info(src_obj, loc_params1, ".", &input.src_loc_id, &input.src_loc_oh, 
                                &input.src_parent_axe_id, &src_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

    /* Retrieve the parent information by traversing the path where the
       link should be moved to. */
    if(H5VL_iod_get_parent_info(dst_obj, loc_params2, ".", &input.dst_loc_id, &input.dst_loc_oh, 
                                &input.dst_parent_axe_id, &dst_name, &cur_obj) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

    /* if the object, to be moved is open locally, then update its
       link information */
    if(!copy_flag && 0 == strcmp(src_name, ".")) {
        char *link_name = NULL;

        /* generate the entire path of the new link */
        {
            size_t obj_name_len = HDstrlen(dst_obj->obj_name);
            size_t name_len = HDstrlen(loc_params2.loc_data.loc_by_name.name);

            if (NULL == (link_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate");
            HDmemcpy(link_name, dst_obj->obj_name, obj_name_len);
            HDmemcpy(link_name+obj_name_len, loc_params2.loc_data.loc_by_name.name, name_len);
            link_name[obj_name_len+name_len] = '\0';
        }
        //H5VL_iod_update_link(dst_obj, loc_params2, link_name);
        free(link_name);
    }

    /* set the input structure for the HG encode routine */
    input.coh = src_obj->file->remote_file.coh;
    input.copy_flag = copy_flag;
    input.axe_id = axe_id ++;
    input.lcpl_id = lcpl_id;
    input.lapl_id = lapl_id;
    input.src_loc_name = src_name;
    input.dst_loc_name = dst_name;

#if H5VL_IOD_DEBUG
    if(copy_flag)
        printf("Link Copy axe %llu: %s ID %llu axe %llu to %s ID %llu axe %llu\n", 
               input.axe_id, src_name, input.src_loc_id, input.src_parent_axe_id,
               dst_name, input.dst_loc_id, input.dst_parent_axe_id);
    else
        printf("Link Move axe %llu: %s ID %llu axe %llu to %s ID %llu axe %llu\n", 
               input.axe_id, src_name, input.src_loc_id, input.src_parent_axe_id,
               dst_name, input.dst_loc_id, input.dst_parent_axe_id);
#endif

    status = (herr_t *)malloc(sizeof(herr_t));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_LINK_MOVE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship link move");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc
                    (sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_LINK_MOVE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)cur_obj;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dst_obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        cur_obj->request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(dst_obj->file, request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    if(src_name) free(src_name);
    if(dst_name) free(dst_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_iterate
 *
 * Purpose:	Iterates through links in a group
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5VL_iod_link_iterate(void *_obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                    H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                                    H5L_iterate_t op, void *op_data, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    hid_t temp_id;
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_get
 *
 * Purpose:	Gets certain data about a link
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_link_get(void *_obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, 
                  hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    char *new_name = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Lexists */
        case H5VL_LINK_EXISTS:
            {
                link_op_in_t input;
                htri_t *ret    = va_arg (arguments, htri_t *);

                /* Retrieve the parent info by traversing the path where the
                   link should be checked. */
                if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                            &input.parent_axe_id, &new_name, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.axe_id = axe_id ++;
                input.path = new_name;

                /* get a function shipper request */
                if(do_async) {
                    if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
                        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
                } /* end if */
                else
                    hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
                printf("Link Exists axe %llu: %s ID %llu axe %llu\n", 
                       input.axe_id, new_name, input.loc_id, input.parent_axe_id);
#endif

                /* forward the call to the IONs */
                if(HG_Forward(PEER, H5VL_LINK_EXISTS_ID, &input, ret, hg_req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship link exists");

                /* Get async request for operation */
                if(do_async) {
                    if(NULL == (request = (H5VL_iod_request_t *)
                                H5MM_malloc(sizeof(H5VL_iod_request_t))))
                        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
                } /* end if */
                else
                    request = &_request;

                /* Set up request */
                HDmemset(request, 0, sizeof(*request));
                request->type = HG_LINK_EXISTS;
                request->data = ret;
                request->req = hg_req;
                request->obj = (H5VL_iod_object_t *)obj;
                request->axe_id = input.axe_id;
                request->next = request->prev = NULL;
                /* add request to container's linked list */
                H5VL_iod_request_add(obj->file, request);

                /* Store/wait on request */
                if(do_async) {
                    /* Sanity check */
                    HDassert(request != &_request);
                    *req = request;
                } /* end if */
                else {
                    /* Synchronously wait on the request */
                    if(H5VL_iod_request_wait(obj->file, request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    /* Sanity check */
                    HDassert(request == &_request);
                } /* end else */

                if(new_name) free(new_name);
                break;
            }
        /* H5Lget_info/H5Lget_info_by_idx */
        case H5VL_LINK_GET_INFO:
            {
                H5L_info_t *linfo  = va_arg (arguments, H5L_info_t *);
            }
        /* H5Lget_name_by_idx */
        case H5VL_LINK_GET_NAME:
            {
                char       *name   = va_arg (arguments, char *);
                size_t      size   = va_arg (arguments, size_t);
                ssize_t    *ret    = va_arg (arguments, ssize_t *);
            }
        /* H5Lget_val/H5Lget_val_by_idx */
        case H5VL_LINK_GET_VAL:
            {
                void       *buf    = va_arg (arguments, void *);
                size_t     size    = va_arg (arguments, size_t);
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_remove
 *
 * Purpose:	Removes the specified NAME from the group graph and
 *		decrements the link count for the object to which NAME
 *		points.  If the link count reaches zero then all file-space
 *		associated with the object will be reclaimed (but if the
 *		object is open, then the reclamation of the file space is
 *		delayed until all handles to the object are closed).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_link_remove(void *_obj, H5VL_loc_params_t loc_params, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    H5VL_iod_object_t *cur_obj;
    link_op_in_t input;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    char *new_name = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent info by traversing the path where the
       link should be removed. */
    if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                &input.parent_axe_id, &new_name, &cur_obj) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.path = new_name;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("Link Remove axe %llu: %s ID %llu axe %llu\n", 
           input.axe_id, new_name, input.loc_id, input.parent_axe_id);
#endif

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_LINK_REMOVE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship link remove");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_LINK_REMOVE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)cur_obj;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        cur_obj->request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_open
 *
 * Purpose:	Opens a object inside IOD file.
 *
 * Return:	Success:	object id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_object_open(void *_obj, H5VL_loc_params_t loc_params, 
                     H5I_type_t *opened_type, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to open the group */
    hg_request_t hg_req; /* Local function shipper request, for sync. operations */
    H5VL_iod_request_t request; /* Local request, for sync. operations */
    H5VL_iod_remote_object_t remote_obj; /* generic remote object structure */
    object_op_in_t input;
    char *new_name = NULL;
    void *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       object should be opened. */
    if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                &input.parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.axe_id = axe_id ++;
    input.loc_name = new_name;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_OBJECT_OPEN_ID, &input, &remote_obj, &hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship link remove");

    /* Set up request */
    HDmemset(&request, 0, sizeof(request));
    request.type = HG_OBJECT_OPEN;
    request.data = &remote_obj;
    request.req = &hg_req;
    request.obj = obj;
    request.axe_id = input.axe_id;
    request.next = request.prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, &request);

    /* Synchronously wait on the request */
    if(H5VL_iod_request_wait(obj->file, &request) < 0)
        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, NULL, "can't wait on HG request");

    *opened_type = remote_obj.obj_type;

    switch(remote_obj.obj_type) {
        case H5I_DATASET:
            {
                H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */

                /* allocate the dataset object that is returned to the user */
                if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

                dset->remote_dset.iod_oh.cookie = remote_obj.iod_oh.cookie;
                dset->remote_dset.iod_id = remote_obj.iod_id;
                dset->remote_dset.dcpl_id = remote_obj.cpl_id;
                dset->remote_dset.type_id = remote_obj.type_id;
                dset->remote_dset.space_id = remote_obj.space_id;

                if(dset->remote_dset.dcpl_id == H5P_DEFAULT){
                    dset->remote_dset.dcpl_id = H5Pcopy(H5P_DATASET_CREATE_DEFAULT);
                }

                HDassert(dset->remote_dset.dcpl_id);
                HDassert(dset->remote_dset.type_id);
                HDassert(dset->remote_dset.space_id);

                /* setup the local dataset struct */
                /* store the entire path of the dataset locally */
                {
                    size_t obj_name_len = HDstrlen(obj->obj_name);
                    size_t name_len = HDstrlen(loc_params.loc_data.loc_by_name.name);

                    if (NULL == (dset->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
                    HDmemcpy(dset->common.obj_name, obj->obj_name, obj_name_len);
                    HDmemcpy(dset->common.obj_name+obj_name_len, 
                             loc_params.loc_data.loc_by_name.name, name_len);
                    dset->common.obj_name[obj_name_len+name_len] = '\0';
                }

                if((dset->dapl_id = H5Pcopy(H5P_DATASET_CREATE_DEFAULT)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

                /* set common object parameters */
                dset->common.obj_type = H5I_DATASET;
                dset->common.file = obj->file;
                dset->common.file->nopen_objs ++;

                ret_value = (void *)dset;
                break;
            }
        case H5I_DATATYPE:
            {
                H5VL_iod_dtype_t *dtype = NULL; /* the dataset object that is created and passed to the user */

                /* allocate the dataset object that is returned to the user */
                if(NULL == (dtype = H5FL_CALLOC(H5VL_iod_dtype_t)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

                dtype->remote_dtype.iod_oh.cookie = remote_obj.iod_oh.cookie;
                dtype->remote_dtype.iod_id = remote_obj.iod_id;
                dtype->remote_dtype.tcpl_id = remote_obj.cpl_id;
                dtype->remote_dtype.type_id = remote_obj.type_id;

                if(dtype->remote_dtype.tcpl_id == H5P_DEFAULT){
                    dtype->remote_dtype.tcpl_id = H5Pcopy(H5P_DATATYPE_CREATE_DEFAULT);
                }

                HDassert(dtype->remote_dtype.tcpl_id);
                HDassert(dtype->remote_dtype.type_id);

                /* setup the local dataset struct */
                /* store the entire path of the dataset locally */
                {
                    size_t obj_name_len = HDstrlen(obj->obj_name);
                    size_t name_len = HDstrlen(loc_params.loc_data.loc_by_name.name);

                    if (NULL == (dtype->common.obj_name = (char *)HDmalloc
                                 (obj_name_len + name_len + 1)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
                    HDmemcpy(dtype->common.obj_name, obj->obj_name, obj_name_len);
                    HDmemcpy(dtype->common.obj_name+obj_name_len, 
                             loc_params.loc_data.loc_by_name.name, name_len);
                    dtype->common.obj_name[obj_name_len+name_len] = '\0';
                }

                if((dtype->tapl_id = H5Pcopy(H5P_DATATYPE_CREATE_DEFAULT)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

                /* set common object parameters */
                dtype->common.obj_type = H5I_DATATYPE;
                dtype->common.file = obj->file;
                dtype->common.file->nopen_objs ++;

                ret_value = (void *)dtype;
                break;
            }
        case H5I_GROUP:
            {
                H5VL_iod_group_t  *grp = NULL; /* the group object that is created and passed to the user */

                /* allocate the dataset object that is returned to the user */
                if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

                grp->remote_group.iod_oh.cookie = remote_obj.iod_oh.cookie;
                grp->remote_group.iod_id = remote_obj.iod_id;
                grp->remote_group.gcpl_id = remote_obj.cpl_id;

                if(grp->remote_group.gcpl_id == H5P_DEFAULT){
                    grp->remote_group.gcpl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT);
                }

                HDassert(grp->remote_group.gcpl_id);

                /* setup the local dataset struct */
                /* store the entire path of the dataset locally */
                {
                    size_t obj_name_len = HDstrlen(obj->obj_name);
                    size_t name_len = HDstrlen(loc_params.loc_data.loc_by_name.name);

                    if (NULL == (grp->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
                    HDmemcpy(grp->common.obj_name, obj->obj_name, obj_name_len);
                    HDmemcpy(grp->common.obj_name+obj_name_len, 
                             loc_params.loc_data.loc_by_name.name, name_len);
                    grp->common.obj_name[obj_name_len+name_len] = '\0';
                }

                if((grp->gapl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");

                /* set common object parameters */
                grp->common.obj_type = H5I_GROUP;
                grp->common.file = obj->file;
                grp->common.file->nopen_objs ++;

                ret_value = (void *)grp;
                break;
            }
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, NULL, "not a valid file object (dataset, group, or datatype)")
        break;
    }

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_open */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_copy
 *
 * Purpose:	Copys a object through the IOD plugin.
 *
 * Return:	Success:	postive. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_object_copy(void *_src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                     void *_dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                     hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *src_obj = (H5VL_iod_object_t *)_src_obj;
    H5VL_iod_object_t *dst_obj = (H5VL_iod_object_t *)_dst_obj;
    H5VL_iod_object_t *cur_obj;
    object_copy_in_t input;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    char *new_src_name = NULL, *new_dst_name = NULL;
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent information by traversing the path where the
       link should be moved from. */
    if(H5VL_iod_get_parent_info(src_obj, loc_params1, src_name, 
                                &input.src_loc_id, &input.src_loc_oh, 
                                &input.src_parent_axe_id, &new_src_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

    /* Retrieve the parent information by traversing the path where the
       link should be moved to. */
    if(H5VL_iod_get_parent_info(dst_obj, loc_params2, dst_name, 
                                &input.dst_loc_id, &input.dst_loc_oh, 
                                &input.dst_parent_axe_id, &new_dst_name, &cur_obj) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

    /* set the input structure for the HG encode routine */
    input.coh = src_obj->file->remote_file.coh;
    input.axe_id = axe_id ++;
    input.lcpl_id = lcpl_id;
    input.ocpypl_id = ocpypl_id;
    input.src_loc_name = new_src_name;
    input.dst_loc_name = new_dst_name;

#if H5VL_IOD_DEBUG
    printf("Object Copy axe %llu: %s ID %llu axe %llu to %s ID %llu axe %llu\n", 
           input.axe_id, new_src_name, input.src_loc_id, input.src_parent_axe_id,
           new_dst_name, input.dst_loc_id, input.dst_parent_axe_id);
#endif

    status = (herr_t *)malloc(sizeof(herr_t));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_OBJECT_COPY_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship object copy");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc
                    (sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_OBJECT_COPY;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)cur_obj;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dst_obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        cur_obj->request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(dst_obj->file, request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    if(new_src_name) free(new_src_name);
    if(new_dst_name) free(new_dst_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_visit
 *
 * Purpose:	Iterates through all objects linked to an object
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5VL_iod_object_visit(void *_obj, H5VL_loc_params_t loc_params, H5_index_t idx_type,
                                    H5_iter_order_t order, H5O_iterate_t op, void *op_data, 
                                    hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    hid_t temp_id;
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_visit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_misc
 *
 * Purpose:	Perform a plugin specific operation for an object
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_object_misc(void *_obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, 
                     hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    int *status = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (misc_type) {
        /* H5Arename/rename_by_name */
        case H5VL_ATTR_RENAME:
            {
                const char    *old_name  = va_arg (arguments, const char *);
                const char    *new_name  = va_arg (arguments, const char *);
                char *loc_name = NULL;
                attr_rename_in_t input;

                /* Retrieve the parent AXE id by traversing the path where the
                   attribute is located. */
                if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                           &input.parent_axe_id, &loc_name, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.path = loc_name;
                input.old_attr_name = old_name;
                input.new_attr_name = new_name;
                input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
                printf("Attribute Rename %s to %s LOC ID %llu, axe id %llu, parent %llu\n", 
                       old_name, new_name, input.loc_id, input.axe_id, input.parent_axe_id);
#endif

                status = (herr_t *)malloc(sizeof(herr_t));

                /* get a function shipper request */
                if(do_async) {
                    if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
                        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
                } /* end if */
                else
                    hg_req = &_hg_req;

                /* forward the call to the IONs */
                if(HG_Forward(PEER, H5VL_ATTR_RENAME_ID, &input, status, hg_req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship attribute rename");

                /* Get async request for operation */
                if(do_async) {
                    if(NULL == (request = (H5VL_iod_request_t *)
                                H5MM_malloc(sizeof(H5VL_iod_request_t))))
                        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
                } /* end if */
                else
                    request = &_request;

                /* Set up request */
                HDmemset(request, 0, sizeof(*request));
                request->type = HG_ATTR_RENAME;
                request->data = status;
                request->req = hg_req;
                request->obj = (H5VL_iod_object_t *)obj;
                request->axe_id = input.axe_id;
                request->next = request->prev = NULL;
                /* add request to container's linked list */
                H5VL_iod_request_add(obj->file, request);

                /* Store/wait on request */
                if(do_async) {
                    /* Sanity check */
                    HDassert(request != &_request);
                    *req = request;
                    /* Track request */
                    obj->request = request;
                } /* end if */
                else {
                    /* Synchronously wait on the request */
                    if(H5VL_iod_request_wait(obj->file, request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    /* Sanity check */
                    HDassert(request == &_request);
                } /* end else */

                if(loc_name) free(loc_name);
                break;
            }
        /* H5Oset_comment */
        case H5VL_OBJECT_SET_COMMENT:
            {
                const char    *comment  = va_arg (arguments, char *);
                object_set_comment_in_t input;
                char *loc_name = NULL;

                /* Retrieve the parent AXE id by traversing the path where the
                   object is located. */
                if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                           &input.parent_axe_id, &loc_name, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.path = loc_name;
                input.comment = comment;
                input.axe_id = axe_id ++;

                status = (herr_t *)malloc(sizeof(herr_t));

                /* get a function shipper request */
                if(do_async) {
                    if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
                        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
                } /* end if */
                else
                    hg_req = &_hg_req;

                /* forward the call to the IONs */
                if(HG_Forward(PEER, H5VL_OBJECT_SET_COMMENT_ID, &input, status, hg_req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship set comment");

                /* Get async request for operation */
                if(do_async) {
                    if(NULL == (request = (H5VL_iod_request_t *)
                                H5MM_malloc(sizeof(H5VL_iod_request_t))))
                        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
                } /* end if */
                else
                    request = &_request;

                /* Set up request */
                HDmemset(request, 0, sizeof(*request));
                request->type = HG_OBJECT_SET_COMMENT;
                request->data = status;
                request->req = hg_req;
                request->obj = (H5VL_iod_object_t *)obj;
                request->axe_id = input.axe_id;
                request->next = request->prev = NULL;
                /* add request to container's linked list */
                H5VL_iod_request_add(obj->file, request);

                /* Store/wait on request */
                if(do_async) {
                    /* Sanity check */
                    HDassert(request != &_request);
                    *req = request;
                } /* end if */
                else {
                    /* Synchronously wait on the request */
                    if(H5VL_iod_request_wait(obj->file, request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    /* Sanity check */
                    HDassert(request == &_request);
                } /* end else */

                /* store the comment locally if the object is open */
                if(loc_params.type == H5VL_OBJECT_BY_SELF)
                    obj->comment = HDstrdup(comment);

                if(loc_name) free(loc_name);
                break;
            }
        /* H5Oincr_refcount / H5Odecr_refcount */
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                int update_ref  = va_arg (arguments, int);
            }
        case H5VL_REF_CREATE:
            {
                void        *ref      = va_arg (arguments, void *);
                const char  *name     = va_arg (arguments, char *);
                H5R_type_t  ref_type  = va_arg (arguments, H5R_type_t);
                hid_t       space_id  = va_arg (arguments, hid_t);
                size_t ref_size = 0;

                if(ref_type == H5R_DATASET_REGION)
                    ref_size = sizeof(hdset_reg_ref_t);
                else if (ref_type == H5R_OBJECT)
                    ref_size = sizeof(hobj_ref_t);
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_get
 *
 * Purpose:	Gets certain data about a file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_object_get(void *_obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, 
                    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    int *status;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    char *new_name = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Oexists_by_name */
        case H5VL_OBJECT_EXISTS:
            {
                htri_t	  *ret = va_arg (arguments, htri_t *);
                object_op_in_t input;

                /* Retrieve the parent info by traversing the path where the
                   object should be checked. */
                if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                            &input.parent_axe_id, &new_name, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.axe_id = axe_id ++;
                input.loc_name = new_name;

                /* get a function shipper request */
                if(do_async) {
                    if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
                        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
                } /* end if */
                else
                    hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
                printf("Object Exists axe %llu: %s ID %llu axe %llu\n", 
                       input.axe_id, new_name, input.loc_id, input.parent_axe_id);
#endif

                /* forward the call to the IONs */
                if(HG_Forward(PEER, H5VL_OBJECT_EXISTS_ID, &input, ret, hg_req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship object exists");

                /* Get async request for operation */
                if(do_async) {
                    if(NULL == (request = (H5VL_iod_request_t *)
                                H5MM_malloc(sizeof(H5VL_iod_request_t))))
                        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
                } /* end if */
                else
                    request = &_request;

                /* Set up request */
                HDmemset(request, 0, sizeof(*request));
                request->type = HG_OBJECT_EXISTS;
                request->data = ret;
                request->req = hg_req;
                request->obj = (H5VL_iod_object_t *)obj;
                request->axe_id = input.axe_id;
                request->next = request->prev = NULL;
                /* add request to container's linked list */
                H5VL_iod_request_add(obj->file, request);

                /* Store/wait on request */
                if(do_async) {
                    /* Sanity check */
                    HDassert(request != &_request);
                    *req = request;
                } /* end if */
                else {
                    /* Synchronously wait on the request */
                    if(H5VL_iod_request_wait(obj->file, request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    /* Sanity check */
                    HDassert(request == &_request);
                } /* end else */

                if(new_name) free(new_name);
                break;
            }
        /* H5Oget_comment / H5Oget_comment_by_name */
        case H5VL_OBJECT_GET_COMMENT:
            {
                char *comment =  va_arg (arguments, char *);
                size_t size  =  va_arg (arguments, size_t);
                ssize_t *ret =  va_arg (arguments, ssize_t *);
                object_get_comment_in_t input;
                object_get_comment_out_t *result = NULL;
                size_t len;

                /* If the comment is present locally, get it and return */
                if(loc_params.type == H5VL_OBJECT_BY_SELF && obj->comment) {
                    len = HDstrlen(obj->comment);

                    if(comment) {
                        HDstrncpy(comment, obj->comment, MIN(len + 1,size));
                        if(len >= size)
                            comment[size-1]='\0';
                    } /* end if */

                    /* Set the return value for the API call */
                    *ret = (ssize_t)len;
                    break;
                }

                /* Otherwise Go to the server */

                /* Retrieve the parent info by traversing the path where the
                   object should be checked. */
                if(H5VL_iod_get_parent_info(obj, loc_params, ".", &input.loc_id, &input.loc_oh, 
                                            &input.parent_axe_id, &new_name, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current working group");

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.axe_id = axe_id ++;
                input.path = new_name;
                if(comment)
                    input.length = size;
                else
                    input.length = 0;

                if(NULL == (result = (object_get_comment_out_t *)malloc
                            (sizeof(object_get_comment_out_t)))) {
                    HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate get comment return struct");
                }

                result->name.size = input.length;
                result->name.value_size = ret;
                result->name.value = comment;

                /* get a function shipper request */
                if(do_async) {
                    if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
                        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a HG request");
                } /* end if */
                else
                    hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
                printf("Object Get Comment axe %llu: %s ID %llu axe %llu\n", 
                       input.axe_id, new_name, input.loc_id, input.parent_axe_id);
#endif

                /* forward the call to the IONs */
                if(HG_Forward(PEER, H5VL_OBJECT_GET_COMMENT_ID, &input, result, hg_req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship object exists");

                /* Get async request for operation */
                if(do_async) {
                    if(NULL == (request = (H5VL_iod_request_t *)
                                H5MM_malloc(sizeof(H5VL_iod_request_t))))
                        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
                } /* end if */
                else
                    request = &_request;

                /* Set up request */
                HDmemset(request, 0, sizeof(*request));
                request->type = HG_OBJECT_GET_COMMENT;
                request->data = result;
                request->req = hg_req;
                request->obj = (H5VL_iod_object_t *)obj;
                request->axe_id = input.axe_id;
                request->next = request->prev = NULL;
                /* add request to container's linked list */
                H5VL_iod_request_add(obj->file, request);

                /* Store/wait on request */
                if(do_async) {
                    /* Sanity check */
                    HDassert(request != &_request);
                    *req = request;
                } /* end if */
                else {
                    /* Synchronously wait on the request */
                    if(H5VL_iod_request_wait(obj->file, request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    /* Sanity check */
                    HDassert(request == &_request);
                } /* end else */

                if(new_name) free(new_name);
                break;
            }
        /* H5Oget_info / H5Oget_info_by_name / H5Oget_info_by_idx */
        case H5VL_OBJECT_GET_INFO:
            {
                H5O_info_t  *obj_info = va_arg (arguments, H5O_info_t *);
                size_t info_size = 0;
            }
        /* H5Rget_region */
        case H5VL_REF_GET_REGION:
            {
                hid_t       *ret     =  va_arg (arguments, hid_t *);
                H5R_type_t  ref_type =  va_arg (arguments, H5R_type_t);
                void        *ref     =  va_arg (arguments, void *);
                H5S_t       *space = NULL;    /* Dataspace object */
            }
        /* H5Rget_obj_type2 */
        case H5VL_REF_GET_TYPE:
            {
                H5O_type_t  *obj_type  =  va_arg (arguments, H5O_type_t *);
                H5R_type_t  ref_type   =  va_arg (arguments, H5R_type_t);
                const void  *ref       =  va_arg (arguments, const void *);
            }
        /* H5Rget_name */
        case H5VL_REF_GET_NAME:
            {
                ssize_t     *ret       = va_arg (arguments, ssize_t *);
                char        *name      = va_arg (arguments, char *);
                size_t      size       = va_arg (arguments, size_t);
                H5R_type_t  ref_type   = va_arg (arguments, H5R_type_t);
                void        *ref       = va_arg (arguments, void *);
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from object")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_cancel
 *
 * Purpose:	Cancel an asynchronous operation
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		April 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_cancel(void **req, H5_status_t *status)
{
    H5VL_iod_request_t *request = *((H5VL_iod_request_t **)req);
    hg_status_t hg_status;
    int         ret;
    H5VL_iod_state_t state;
    hg_request_t hg_req;       /* Local function shipper request, for sync. operations */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* request has completed already, can not cancel */
    if(request->state == H5VL_IOD_COMPLETED) {
        if(H5VL_iod_request_wait(request->obj->file, request) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait for request");
        *status = request->status;
        request->req = H5MM_xfree(request->req);
        request = (H5VL_iod_request_t *)H5MM_xfree(request);
    }

    /* forward the cancel call to the IONs */
    if(HG_Forward(PEER, H5VL_CANCEL_OP_ID, &request->axe_id, &state, &hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship attribute write");

    /* Wait on the cancel request to return */
    ret = HG_Wait(hg_req, HG_MAX_IDLE_TIME, &hg_status);
    /* If the actual wait Fails, then the status of the cancel
       operation is unknown */
    if(HG_FAIL == ret)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to wait on cancel request")
    else {
        if(hg_status) {
            /* If the operation to be canceled has already completed,
               mark it so and complete it locally */
            if(state == H5VL_IOD_COMPLETED) {
                if(H5VL_IOD_PENDING == request->state) {
                    if(H5VL_iod_request_wait(request->obj->file, request) < 0)
                        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait for request");
                }

                *status = request->status;
                request->req = H5MM_xfree(request->req);
                request = (H5VL_iod_request_t *)H5MM_xfree(request);
            }

            /* if the status returned is cancelled, then cancel it
               locally too */
            else if (state == H5VL_IOD_CANCELLED) {
                request->status = H5AO_CANCELLED;
                request->state = H5VL_IOD_CANCELLED;
                if(H5VL_iod_request_cancel(request->obj->file, request) < 0)
                    fprintf(stderr, "Operation Failed!\n");

                *status = request->status;
                request->req = H5MM_xfree(request->req);
                request = (H5VL_iod_request_t *)H5MM_xfree(request);
            }
        }
        else
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Cancel Operation taking too long. Aborting");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_cancel() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_test
 *
 * Purpose:	Test for an asynchronous operation's completion
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_test(void **req, H5_status_t *status)
{
    H5VL_iod_request_t *request = *((H5VL_iod_request_t **)req);
    hg_status_t hg_status;
    int         ret;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    ret = HG_Wait(*((hg_request_t *)request->req), 0, &hg_status);
    if(HG_FAIL == ret) {
        fprintf(stderr, "failed to wait on request\n");
        request->status = H5AO_FAILED;
        request->state = H5VL_IOD_COMPLETED;
        H5VL_iod_request_delete(request->obj->file, request);
    }
    else {
        if(hg_status) {
            request->status = H5AO_SUCCEEDED;
            request->state = H5VL_IOD_COMPLETED;
            if(H5VL_iod_request_complete(request->obj->file, request) < 0)
                fprintf(stderr, "Operation Failed!\n");
            *status = request->status;
            request->req = H5MM_xfree(request->req);
            request = (H5VL_iod_request_t *)H5MM_xfree(request);
        }
        else
            *status = request->status;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_test() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_wait
 *
 * Purpose:	Wait for an asynchronous operation to complete
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_wait(void **req, H5_status_t *status)
{
    H5VL_iod_request_t *request = *((H5VL_iod_request_t **)req);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL_IOD_PENDING == request->state) {
        if(H5VL_iod_request_wait(request->obj->file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait for request")
    }

    *status = request->status;
    request->req = H5MM_xfree(request->req);
    request = (H5VL_iod_request_t *)H5MM_xfree(request);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_wait() */

void *
H5VL_iod_map_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                    hid_t keytype, hid_t valtype, hid_t lcpl_id, hid_t mcpl_id, 
                    hid_t mapl_id, uint64_t trans, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_map_t *map = NULL; /* the map object that is created and passed to the user */
    map_create_in_t input;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    uint64_t parent_axe_id;
    char *new_name = NULL; /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       map should be created. */
    if(H5VL_iod_get_parent_info(obj, loc_params, name, &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the map object that is returned to the user */
    if(NULL == (map = H5FL_CALLOC(H5VL_iod_map_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    map->remote_map.iod_oh.cookie = IOD_OH_UNDEFINED;
    map->remote_map.iod_id = IOD_ID_UNDEFINED;

    /* Generate an IOD ID for the group to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.map_id);
    map->remote_map.iod_id = input.map_id;

    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.name = new_name;
    input.keytype_id = keytype;
    input.valtype_id = valtype;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("Map Create %s, IOD ID %llu, axe id %llu, parent %llu\n", 
           new_name, input.map_id, input.axe_id, input.parent_axe_id);
#endif

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_MAP_CREATE_ID, &input, &map->remote_map, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship map create");

    /* setup the local map struct */
    /* store the entire path of the map locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (map->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(map->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(map->common.obj_name+obj_name_len, name, name_len);
        map->common.obj_name[obj_name_len+name_len] = '\0';
    }

    if((map->remote_map.keytype_id = H5Tcopy(keytype)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");
    if((map->remote_map.valtype_id = H5Tcopy(valtype)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");

    /* set common object parameters */
    map->common.obj_type = H5I_MAP;
    map->common.file = obj->file;
    map->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_MAP_CREATE;
    request->data = map;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)map;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        map->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't wait on HG request")

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        map->common.request = NULL;
    } /* end else */

    ret_value = (void *)map;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_create() */

void *
H5VL_iod_map_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t mapl_id, 
                  uint64_t trans, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_map_t *map = NULL; /* the map object that is created and passed to the user */
    map_open_in_t input;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    uint64_t parent_axe_id;
    char *new_name = NULL; /* resolved path to where we need to start traversal at the server */
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve the parent AXE id by traversing the path where the
       map should be created. */
    if(H5VL_iod_get_parent_info(obj, loc_params, name, &iod_id, &iod_oh, 
                                &parent_axe_id, &new_name, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the map object that is returned to the user */
    if(NULL == (map = H5FL_CALLOC(H5VL_iod_map_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    map->remote_map.iod_oh.cookie = IOD_OH_UNDEFINED;
    map->remote_map.iod_id = IOD_ID_UNDEFINED;
    map->remote_map.keytype_id = -1;
    map->remote_map.valtype_id = -1;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.parent_axe_id = parent_axe_id;
    input.name = new_name;
    input.axe_id = axe_id ++;

#if H5VL_IOD_DEBUG
    printf("Map Open %s LOC ID %llu, axe id %llu, parent %llu, name len %zu\n", 
           new_name, input.loc_id, input.axe_id, input.parent_axe_id, strlen(input.name));
#endif

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_MAP_OPEN_ID, &input, &map->remote_map, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship map create");

    /* setup the local map struct */
    /* store the entire path of the map locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (map->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(map->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(map->common.obj_name+obj_name_len, name, name_len);
        map->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* set common object parameters */
    map->common.obj_type = H5I_MAP;
    map->common.file = obj->file;
    map->common.file->nopen_objs ++;

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_MAP_OPEN;
    request->data = map;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)map;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        map->common.request = request;
    } /* end if */
    else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(obj->file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't wait on HG request")

        /* Sanity check */
        HDassert(request == &_request);

        /* Request has completed already */
        map->common.request = NULL;
    } /* end else */

    ret_value = (void *)map;

done:
    if(new_name) free(new_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_open() */

herr_t 
H5VL_iod_map_set(void *_map, hid_t key_mem_type_id, const void *key, 
                 hid_t val_mem_type_id, const void *value, hid_t dxpl_id, 
                 uint64_t trans, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_set_in_t input;
    size_t key_size, val_size;
    int *status = NULL;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == map->remote_map.keytype_id ||
       -1 == map->remote_map.valtype_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(map->common.file, map->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on HG request");
        map->common.request = NULL;
    }

    /* set the parent axe id */
    if(map->common.request)
        input.parent_axe_id = map->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* get the Key and Value size */
    {
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(key_mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");
        key_size = H5T_GET_SIZE(dt);

        dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(val_mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");
        val_size = H5T_GET_SIZE(dt);
    }

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.dxpl_id = dxpl_id;
    input.key_maptype_id = map->remote_map.keytype_id;
    input.key_memtype_id = key_mem_type_id;
    input.key.buf_size = key_size;
    input.key.buf = key;
    input.val_maptype_id = map->remote_map.valtype_id;
    input.val_memtype_id = val_mem_type_id;
    input.val.buf_size = val_size;
    input.val.buf = value;
    input.axe_id = axe_id ++;

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    printf("kmap %d kmem %d. vmap %d kmap %d  dxpl %d\n", 
           input.key_maptype_id,input.key_memtype_id,input.val_maptype_id,input.val_memtype_id, dxpl_id);

#if H5VL_IOD_DEBUG
    printf("MAP set, axe id %llu, parent %llu\n", 
           input.axe_id, input.parent_axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_MAP_SET_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship map set");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_MAP_SET;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)map;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(map->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(map->common.file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_set() */

herr_t 
H5VL_iod_map_get(void *_map, hid_t key_mem_type_id, const void *key, 
                 hid_t val_mem_type_id, void *value, hid_t dxpl_id, 
                 uint64_t trans, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_get_in_t input;
    map_get_out_t *output;
    size_t key_size, val_size;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == map->remote_map.keytype_id ||
       -1 == map->remote_map.valtype_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(map->common.file, map->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on HG request");
        map->common.request = NULL;
    }

    /* set the parent axe id */
    if(map->common.request)
        input.parent_axe_id = map->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* get the Key and Value size */
    {
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(key_mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");
        key_size = H5T_GET_SIZE(dt);

        dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(val_mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");
        val_size = H5T_GET_SIZE(dt);
    }

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.dxpl_id = dxpl_id;
    input.key_maptype_id = map->remote_map.keytype_id;
    input.val_maptype_id = map->remote_map.valtype_id;
    input.key_memtype_id = key_mem_type_id;
    input.key.buf_size = key_size;
    input.key.buf = key;
    input.val_memtype_id = val_mem_type_id;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("MAP Get, axe id %llu, parent %llu\n", 
           input.axe_id, input.parent_axe_id);
#endif

    if(NULL == (output = (map_get_out_t *)H5MM_calloc(sizeof(map_get_out_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a request");

    output->val.val = value;
    output->val.val_size = val_size;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_MAP_GET_ID, &input, output, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset read");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_MAP_GET;
    request->data = output;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)map;
    request->status = 0;
    request->state = 0;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(map->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(map->common.file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_get() */

herr_t 
H5VL_iod_map_get_types(void *_map, hid_t *key_type_id, hid_t *val_type_id, 
                       uint64_t trans, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == map->remote_map.keytype_id ||
       -1 == map->remote_map.valtype_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(map->common.file, map->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on HG request");
        map->common.request = NULL;
    }

    if((*key_type_id = H5Tcopy(map->remote_map.keytype_id)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of map key")

    if((*val_type_id = H5Tcopy(map->remote_map.valtype_id)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of map val")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_get_types() */

herr_t 
H5VL_iod_map_get_count(void *_map, hsize_t *count, uint64_t trans, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_get_count_in_t input;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* set the parent axe id */
    if(map->common.request)
        input.parent_axe_id = map->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("MAP Get count, axe id %llu, parent %llu\n", 
           input.axe_id, input.parent_axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_MAP_GET_COUNT_ID, &input, count, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset read");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_MAP_GET_COUNT;
    request->data = count;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)map;
    request->status = 0;
    request->state = 0;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(map->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(map->common.file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_get_count() */

herr_t 
H5VL_iod_map_exists(void *_map, hid_t key_mem_type_id, const void *key, 
                    htri_t *exists, uint64_t trans, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_op_in_t input;
    size_t key_size;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* set the parent axe id */
    if(map->common.request)
        input.parent_axe_id = map->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* get the Key and Value size */
    {
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(key_mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");
        key_size = H5T_GET_SIZE(dt);
    }

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.key_maptype_id = map->remote_map.keytype_id;
    input.key_memtype_id = key_mem_type_id;
    input.key.buf_size = key_size;
    input.key.buf = key;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("MAP EXISTS, axe id %llu, parent %llu\n", 
           input.axe_id, input.parent_axe_id);
#endif

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_MAP_EXISTS_ID, &input, exists, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset read");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_MAP_EXISTS;
    request->data = exists;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)map;
    request->status = 0;
    request->state = 0;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(map->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(map->common.file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_exists() */

herr_t 
H5VL_iod_map_iterate(void *map, hid_t key_mem_type_id, hid_t value_mem_type_id, 
                     H5M_iterate_func_t callback_func, void *context)
{
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_iterate() */

herr_t 
H5VL_iod_map_delete(void *_map, hid_t key_mem_type_id, const void *key, 
                    uint64_t trans, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_op_in_t input;
    size_t key_size;
    int *status = NULL;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* set the parent axe id */
    if(map->common.request)
        input.parent_axe_id = map->common.request->axe_id;
    else {
        input.parent_axe_id = 0;
    }

    /* get the Key and Value size */
    {
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(key_mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");
        key_size = H5T_GET_SIZE(dt);
    }

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.key_maptype_id = map->remote_map.keytype_id;
    input.key_memtype_id = key_mem_type_id;
    input.key.buf_size = key_size;
    input.key.buf = key;
    input.axe_id = axe_id ++;

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

#if H5VL_IOD_DEBUG
    printf("MAP DELETE, axe id %llu, parent %llu\n", 
           input.axe_id, input.parent_axe_id);
#endif

    status = (int *)malloc(sizeof(int));

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_MAP_DELETE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset read");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_MAP_DELETE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)map;
    request->status = 0;
    request->state = 0;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(map->common.file, request);

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        map->common.request = request;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(request == &_request);

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(map->common.file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't wait on HG request");
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_delete() */

herr_t H5VL_iod_map_close(void *_map, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_close_in_t input;
    int *status;
    size_t num_parents;
    uint64_t *axe_parents = NULL;
    hg_request_t _hg_req;       /* Local function shipper request, for sync. operations */
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t _request; /* Local request, for sync. operations */
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE; /* Whether we're performing async. I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(!do_async) {
        if(H5VL_iod_request_wait_some(map->common.file, map) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    /* determine the parent axe IDs array for this operation*/
    if(H5VL_iod_get_axe_parents((H5VL_iod_object_t *)map, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get num AXE parents");
    if(num_parents) {
        if(NULL == (axe_parents = (uint64_t *)H5MM_malloc(sizeof(uint64_t) * num_parents)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate num parents array");
        if(H5VL_iod_get_axe_parents((H5VL_iod_object_t *)map, &num_parents, axe_parents) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get AXE parents");
    }

    status = (int *)malloc(sizeof(int));

    /* get a function shipper request */
    if(do_async) {
        if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a HG request");
    } /* end if */
    else
        hg_req = &_hg_req;

    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.parent_axe_ids.count = num_parents;
    input.parent_axe_ids.ids = axe_parents;
    input.axe_id = axe_id ++;

    /* forward the call to the IONs */
    if(HG_Forward(PEER, H5VL_MAP_CLOSE_ID, &input, status, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship map close");

    /* Get async request for operation */
    if(do_async) {
        if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    } /* end if */
    else
        request = &_request;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = HG_MAP_CLOSE;
    request->data = status;
    request->req = hg_req;
    request->obj = (H5VL_iod_object_t *)map;
    request->axe_id = input.axe_id;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(map->common.file, request);

#if H5VL_IOD_DEBUG
    printf("MAP Close IOD ID %llu, axe id %llu\n", 
           input.iod_id, input.axe_id);
#endif

    /* Store/wait on request */
    if(do_async) {
        /* Sanity check */
        HDassert(request != &_request);

        *req = request;

        /* Track request */
        map->common.request = request;
    } /* end if */
    else {

        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(map->common.file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't wait on HG request");

        /* Sanity check */
        HDassert(request == &_request);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_close() */

#endif /* H5_HAVE_EFF */
