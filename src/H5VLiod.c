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

/* Prototypes */
static void *H5VL_iod_fapl_copy(const void *_old_fa);
static herr_t H5VL_iod_fapl_free(void *_fa);

/* Atrribute callbacks */
static void *H5VL_iod_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t req);
static void *H5VL_iod_attr_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t req);
static herr_t H5VL_iod_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t req);
static herr_t H5VL_iod_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t req);
static herr_t H5VL_iod_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_attr_remove(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t req);
static herr_t H5VL_iod_attr_close(void *attr, hid_t req);

/* Datatype callbacks */
static void *H5VL_iod_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
static void *H5VL_iod_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t req);
static ssize_t H5VL_iod_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t req);
static herr_t H5VL_iod_datatype_close(void *dt, hid_t req);

/* Dataset callbacks */
static void *H5VL_iod_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
static void *H5VL_iod_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t req);
static herr_t H5VL_iod_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                    hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
static herr_t H5VL_iod_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                     hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
static herr_t H5VL_iod_dataset_set_extent(void *dset, const hsize_t size[], hid_t req);
static herr_t H5VL_iod_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_dataset_close(void *dset, hid_t req);

/* File callbacks */
static void *H5VL_iod_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
static void *H5VL_iod_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
static herr_t H5VL_iod_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t req);
static herr_t H5VL_iod_file_get(void *file, H5VL_file_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_file_misc(void *file, H5VL_file_misc_t misc_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_file_optional(void *file, H5VL_file_optional_t optional_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_file_close(void *file, hid_t req);

/* Group callbacks */
static void *H5VL_iod_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
static void *H5VL_iod_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t req);
static herr_t H5VL_iod_group_get(void *obj, H5VL_group_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_group_close(void *grp, hid_t req);

/* Link callbacks */
static herr_t H5VL_iod_link_create(H5VL_link_create_type_t create_type, void *obj, 
                                   H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t req);
static herr_t H5VL_iod_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                 void *dst_obj, H5VL_loc_params_t loc_params2,
                                 hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req);
static herr_t H5VL_iod_link_iterate(void *obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                    H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                                    H5L_iterate_t op, void *op_data, hid_t req);
static herr_t H5VL_iod_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_link_remove(void *obj, H5VL_loc_params_t loc_params, hid_t req);

/* Object callbacks */
static void *H5VL_iod_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t req);
static herr_t H5VL_iod_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                                   void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                                   hid_t ocpypl_id, hid_t lcpl_id, hid_t req);
static herr_t H5VL_iod_object_visit(void *obj, H5VL_loc_params_t loc_params, H5_index_t idx_type, 
                                    H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t req);
//static herr_t H5VL_iod_object_lookup(hid_t loc_id, H5VL_loc_type_t lookup_type, void **location, hid_t req, va_list arguments);
//static herr_t H5VL_iod_object_free_loc(void *location, hid_t req);
static herr_t H5VL_iod_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_object_optional(void *obj, H5VL_loc_params_t loc_params, H5VL_object_optional_t optional_type, hid_t req, va_list arguments);
static herr_t H5VL_iod_object_close(void *obj, H5VL_loc_params_t loc_params, hid_t req);

/* IOD-specific file access properties */
typedef struct H5VL_iod_fapl_t {
    MPI_Comm		comm;		/*communicator			*/
    MPI_Info		info;		/*file information		*/
} H5VL_iod_fapl_t;

H5FL_DEFINE(H5VL_iod_file_t);
H5FL_DEFINE(H5VL_iod_group_t);
H5FL_DEFINE(H5VL_iod_dset_t);

na_addr_t PEER;

static H5VL_class_t H5VL_iod_g = {
    IOD,
    "iod",					/* name */
    NULL,                                       /* initialize */
    NULL,                                       /* terminate */
    sizeof(H5VL_iod_fapl_t),		        /*fapl_size */
    H5VL_iod_fapl_copy,			        /*fapl_copy */
    H5VL_iod_fapl_free, 		        /*fapl_free */
    {                                           /* attribute_cls */
        NULL,//H5VL_iod_attr_create,                /* create */
        NULL,//H5VL_iod_attr_open,                  /* open */
        NULL,//H5VL_iod_attr_read,                  /* read */
        NULL,//H5VL_iod_attr_write,                 /* write */
        NULL,//H5VL_iod_attr_get,                   /* get */
        NULL,//H5VL_iod_attr_remove,                /* remove */
        NULL//H5VL_iod_attr_close                  /* close */
    },
    {                                           /* datatype_cls */
        NULL,//H5VL_iod_datatype_commit,            /* commit */
        NULL,//H5VL_iod_datatype_open,              /* open */
        NULL,//H5VL_iod_datatype_get_binary,        /* get_size */
        NULL//H5VL_iod_datatype_close              /* close */
    },
    {                                           /* dataset_cls */
        H5VL_iod_dataset_create,             /* create */
        H5VL_iod_dataset_open,               /* open */
        H5VL_iod_dataset_read,               /* read */
        H5VL_iod_dataset_write,              /* write */
        NULL,//H5VL_iod_dataset_set_extent,         /* set extent */
        NULL,//H5VL_iod_dataset_get,                /* get */
        H5VL_iod_dataset_close               /* close */
    },
    {                                           /* file_cls */
        H5VL_iod_file_create,                /* create */
        H5VL_iod_file_open,                  /* open */
        H5VL_iod_file_flush,                 /* flush */
        H5VL_iod_file_get,                   /* get */
        H5VL_iod_file_misc,                  /* misc */
        H5VL_iod_file_optional,              /* optional */
        H5VL_iod_file_close                  /* close */
    },
    {                                           /* group_cls */
        H5VL_iod_group_create,               /* create */
        H5VL_iod_group_open,                 /* open */
        H5VL_iod_group_get,                  /* get */
        H5VL_iod_group_close                 /* close */
    },
    {                                           /* link_cls */
        NULL,//H5VL_iod_link_create,                /* create */
        NULL,//H5VL_iod_link_move,                  /* move */
        NULL,//H5VL_iod_link_iterate,               /* iterate */
        NULL,//H5VL_iod_link_get,                   /* get */
        NULL//H5VL_iod_link_remove                 /* remove */
    },
    {                                           /* object_cls */
        NULL,//H5VL_iod_object_open,                /* open */
        NULL,//H5VL_iod_object_copy,                /* copy */
        NULL,//H5VL_iod_object_visit,               /* visit */
        NULL,//H5VL_iod_object_get,                 /* get */
        NULL,//H5VL_iod_object_misc,                /* misc */
        NULL,//H5VL_iod_object_optional,            /* optional */
        NULL//H5VL_iod_object_close                /* close */
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
 *              January, 2012
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
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
EFF_init(MPI_Comm comm, MPI_Info info)
{
    char mpi_port_name[MPI_MAX_PORT_NAME];
    FILE *config;
    herr_t ret_value = SUCCEED;

    //FUNC_ENTER_API(FAIL)

    if ((config = fopen("port.cfg", "r")) != NULL) {
        fread(mpi_port_name, sizeof(char), MPI_MAX_PORT_NAME, config);
        printf("Using MPI port name: %s.\n", mpi_port_name);
        fclose(config);
    }

    if(NA_UNDEFINED == (PEER = H5VL_iod_client_eff_init(mpi_port_name, comm, info)))
        return FAIL;//HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to initialize eff stack");

    //done:
    //FUNC_LEAVE_API(ret_value)
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
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
EFF_finalize(void)
{
    herr_t ret_value = SUCCEED;

    //FUNC_ENTER_API(FAIL)

    if(H5VL_iod_client_eff_finalize(PEER) < 0)
        return FAIL;//HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to shutdown eff stack");
    //done:
    //FUNC_LEAVE_API(ret_value)
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
 *              January, 2012
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
 *              July 2012
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
    //HDmemcpy(new_fa, old_fa, sizeof(H5VL_iod_fapl_t));

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
 *              July 2012
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

herr_t
H5Pset_dxpl_inject_bad_checksum(hid_t dxpl_id, hbool_t flag)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_INJECT_BAD_CHECKSUM_NAME, &flag) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_inject_bad_checksum() */

herr_t
H5Pget_dxpl_inject_bad_checksum(hid_t dxpl_id, hbool_t *flag/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(flag)
        if(H5P_get(plist, H5D_XFER_INJECT_BAD_CHECKSUM_NAME, flag) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_inject_bad_checksum() */



/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_create
 *
 * Purpose:	Creates a file as a iod HDF5 file.
 *
 * Return:	Success:	the file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t UNUSED req)
{
    H5VL_iod_fapl_t *fa = NULL;
    H5P_genplist_t *plist;      /* Property list pointer */
    int my_rank, my_size;
    H5VL_iod_file_t *file = NULL;
    fs_request_t *fs_req;
    H5VL_iod_request_t *request;
    H5VL_iod_file_create_input_t input;
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
    MPI_Comm_rank(fa->comm, &my_rank);
    MPI_Comm_size(fa->comm, &my_size);

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_iod_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD file struct");

    /* set the input structure for the FS encode routine */
    input.name = name;
    input.flags = flags;
    input.fcpl_id = fcpl_id;
    input.fapl_id = fapl_id;

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate a FS request");

    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_FILE_CREATE_ID, &input, &file->remote_file, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship file create");

    /* initialize head and tail of the container's linked list */
    file->request_list_head = NULL;
    file->request_list_tail = NULL;

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");

    request->type = FS_FILE_CREATE;
    request->data = file;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(file, request);
    
    /* create the file object that is passed to the API layer */
    file->file_name = HDstrdup(name);
    file->flags = flags;
    if((file->remote_file.fcpl_id = H5Pcopy(fcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fcpl");
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl");
    file->nopen_objs = 1;

    file->common.obj_type = H5I_FILE;
    file->common.obj_name = strdup("/");
    file->common.obj_name[1] = '\0';
    file->common.file = file;
    file->common.request = request;

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
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t UNUSED req)
{
    H5VL_iod_fapl_t *fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    int my_rank, my_size;
    H5VL_iod_file_t *file = NULL;
    fs_request_t *fs_req;
    H5VL_iod_request_t *request;
    H5VL_iod_file_open_input_t input;
    void  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* obtain the process rank from the communicator attached to the fapl ID */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_iod_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get IOD info struct")
    MPI_Comm_rank(fa->comm, &my_rank);
    MPI_Comm_size(fa->comm, &my_size);

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_iod_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD file struct");

    input.name = name;
    input.flags = flags;
    input.fapl_id = fapl_id;

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate a FS request");

    if(fs_forward(PEER, H5VL_FILE_OPEN_ID, &input, &file->remote_file, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship file create");

    /* initialize head and tail of the container's linked list */
    file->request_list_head = NULL;
    file->request_list_tail = NULL;

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    request->type = FS_FILE_OPEN;
    request->data = file;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(file, request);
    
    /* create the file object that is passed to the API layer */
    file->file_name = HDstrdup(name);
    file->flags = flags;
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl");
    file->nopen_objs = 1;

    file->common.obj_type = H5I_FILE; 
    file->common.obj_name = strdup("/");
    file->common.obj_name[1] = '\0';
    file->common.file = file;
    file->common.request = request;

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
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t UNUSED req)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT


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
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_get(void *obj, H5VL_file_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT


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
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_misc(void *obj, H5VL_file_misc_t misc_type, hid_t UNUSED req, va_list arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */
    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_optional
 *
 * Purpose:	Perform a plugin specific operation on a iod file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_optional(void *obj, H5VL_file_optional_t optional_type, hid_t UNUSED req, va_list arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_close
 *
 * Purpose:	Closes a file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_close(void *_file, hid_t UNUSED req)
{
    H5VL_iod_file_t *file = (H5VL_iod_file_t *)_file;
    fs_request_t *fs_req;
    int *status;
    H5VL_iod_request_t *request;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL_iod_request_wait_all(file) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS requests");

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a FS request");

    status = (int *)malloc(sizeof(int));
    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_FILE_CLOSE_ID, &file->remote_file, status, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship file close");

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    request->type = FS_FILE_CLOSE;
    request->data = status;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(file, request);

    if(H5VL_iod_request_wait(file, request) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS request");

    free(file->file_name);
    free(file->common.obj_name);
    if(H5Pclose(file->fapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
    if(H5Pclose(file->remote_file.fcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
    file = H5FL_FREE(H5VL_iod_file_t, file);

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
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_group_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, 
                      hid_t gapl_id, hid_t UNUSED req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_group_t *grp = NULL; /* the group object that is created and passed to the user */
    H5VL_iod_group_create_input_t input;
    H5P_genplist_t *plist;
    hid_t lcpl_id;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    char *new_name;
    fs_request_t *fs_req;
    H5VL_iod_request_t *request;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get creation properties */
    if(H5P_get(plist, H5VL_GRP_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id");

    /* resolve the location where to create the group by fetching the iod id and object handle
       for the last open group in the path hierarchy. This is where we will start the traversal
       at the server side */
    if(H5VL_iod_local_traverse(obj, loc_params, name, &iod_id, &iod_oh, &new_name) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = new_name;
    input.gcpl_id = gcpl_id;
    input.gapl_id = gapl_id;
    input.lcpl_id = lcpl_id;

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate a FS request");

    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_GROUP_CREATE_ID, &input, &grp->remote_group, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship group create");

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    request->type = FS_GROUP_CREATE;
    request->data = grp;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* setup the local group struct */
    /* store the entire path of the group locally */
    if (NULL == (grp->common.obj_name = (char *)malloc
                 (HDstrlen(obj->obj_name) + HDstrlen(name) + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
    HDstrcpy(grp->common.obj_name, obj->obj_name);
    HDstrcat(grp->common.obj_name, name);
    grp->common.obj_name[HDstrlen(obj->obj_name) + HDstrlen(name) + 1] = '\0';

    /* copy property lists */
    if((grp->remote_group.gcpl_id = H5Pcopy(gcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gcpl");
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");
    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.file = obj->file;
    grp->common.file->nopen_objs ++;
    grp->common.request = request;

    ret_value = (void *)grp;

done:
    free(new_name);
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
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_group_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                    hid_t gapl_id, hid_t UNUSED req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_group_t  *grp = NULL; /* the group object that is created and passed to the user */
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    char *new_name;
    fs_request_t *fs_req;
    H5VL_iod_request_t *request;
    H5VL_iod_group_open_input_t input;
    void           *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* resolve the location where to open the group by fetching the iod id and object handle
       for the last open group in the path hierarchy. This is where we will start the traversal
       at the server side. */
    if(H5VL_iod_local_traverse(obj, loc_params, name, &iod_id, &iod_oh, &new_name) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = new_name;
    input.gapl_id = gapl_id;

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate a FS request");

    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_GROUP_OPEN_ID, &input, &grp->remote_group, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship group open");

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    request->type = FS_GROUP_OPEN;
    request->data = grp;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* setup the local group struct */
    /* store the entire path of the group locally */
    if (NULL == (grp->common.obj_name = (char *)malloc
                 (HDstrlen(obj->obj_name) + HDstrlen(name) + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
    HDstrcpy(grp->common.obj_name, obj->obj_name);
    HDstrcat(grp->common.obj_name, name);
    grp->common.obj_name[HDstrlen(obj->obj_name) + HDstrlen(name) + 1] = '\0';

    /* copy property lists */
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");
    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.file = obj->file;
    grp->common.file->nopen_objs ++;
    grp->common.request = request;

    ret_value = (void *)grp;

done:
    free(new_name);
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
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_group_get(void *obj, H5VL_group_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

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
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_group_close(void *_grp, hid_t UNUSED req)
{
    H5VL_iod_group_t *grp = (H5VL_iod_group_t *)_grp;
    fs_request_t *fs_req;
    int *status;
    H5VL_iod_request_t *request;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL != grp->common.request) {
        if(H5VL_iod_request_wait(grp->common.file, grp->common.request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS request");
        grp->common.request->req = H5MM_xfree(grp->common.request->req);
        grp->common.request = H5MM_xfree(grp->common.request);
    }

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a FS request");

    status = (int *)malloc(sizeof(int));
    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_GROUP_CLOSE_ID, &grp->remote_group, status, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship group close");

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    request->type = FS_GROUP_CLOSE;
    request->data = status;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(grp->common.file, request);

    free(grp->common.obj_name);
    if(H5Pclose(grp->gapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
    if(H5Pclose(grp->remote_group.gcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
    grp = H5FL_FREE(H5VL_iod_group_t, grp);

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
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_dataset_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, 
                        hid_t dapl_id, hid_t UNUSED req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the dataset */
    H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    H5VL_iod_dset_create_input_t input;
    H5P_genplist_t *plist;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    char *new_name;
    fs_request_t *fs_req;
    H5VL_iod_request_t *request;
    hid_t type_id, space_id, lcpl_id;
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

    /* resolve the location where to create the dataset by fetching the iod id and object handle
       for the last open group in the path hierarchy. This is where we will start the traversal
       at the server side */
    if(H5VL_iod_local_traverse(obj, loc_params, name, &iod_id, &iod_oh, &new_name) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = new_name;
    input.dcpl_id = dcpl_id;
    input.dapl_id = dapl_id;
    input.lcpl_id = lcpl_id;
    input.type_id = type_id;
    input.space_id = space_id;

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate a FS request");

    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_DSET_CREATE_ID, &input, &dset->remote_dset, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship dataset create");

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    request->type = FS_DSET_CREATE;
    request->data = dset;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* setup the local dataset struct */
    /* store the entire path of the dataset locally */
    if (NULL == (dset->common.obj_name = (char *)malloc
                 (HDstrlen(obj->obj_name) + HDstrlen(name) + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
    HDstrcpy(dset->common.obj_name, obj->obj_name);
    HDstrcat(dset->common.obj_name, name);
    dset->common.obj_name[HDstrlen(obj->obj_name) + HDstrlen(name) + 1] = '\0';

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
    dset->common.request = request;

    ret_value = (void *)dset;

done:
    free(new_name);
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
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_dataset_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                      hid_t dapl_id, hid_t UNUSED req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the dataset */
    H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    H5VL_iod_dset_open_input_t input;
    iod_obj_id_t iod_id;
    iod_handle_t iod_oh;
    char *new_name;
    fs_request_t *fs_req;
    H5VL_iod_request_t *request;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* resolve the location where to open the dataset by fetching the iod id and object handle
       for the last open group in the path hierarchy. This is where we will start the traversal
       at the server side. */
    if(H5VL_iod_local_traverse(obj, loc_params, name, &iod_id, &iod_oh, &new_name) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current working group");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = new_name;
    input.dapl_id = dapl_id;

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate a FS request");

    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_DSET_OPEN_ID, &input, &dset->remote_dset, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to ship dataset open");

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD VOL request struct");
    request->type = FS_DSET_OPEN;
    request->data = dset;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(obj->file, request);

    /* setup the local dataset struct */
    /* store the entire path of the dataset locally */
    if (NULL == (dset->common.obj_name = (char *)malloc
                 (HDstrlen(obj->obj_name) + HDstrlen(name) + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
    HDstrcpy(dset->common.obj_name, obj->obj_name);
    HDstrcat(dset->common.obj_name, name);
    dset->common.obj_name[HDstrlen(obj->obj_name) + HDstrlen(name) + 1] = '\0';

    if((dset->dapl_id = H5Pcopy(dapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

    /* set common object parameters */
    dset->common.obj_type = H5I_DATASET;
    dset->common.file = obj->file;
    dset->common.file->nopen_objs ++;
    dset->common.request = request;

    ret_value = (void *)dset;

done:
    free(new_name);
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
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_dataset_read(void *_dset, hid_t mem_type_id, hid_t mem_space_id,
                      hid_t file_space_id, hid_t dxpl_id, void *buf, hid_t UNUSED req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    H5VL_iod_dset_io_input_t input;
    fs_request_t *fs_req = NULL;
    bds_handle_t *bds_handle;
    H5VL_iod_request_t *request = NULL;
    H5VL_iod_read_status_t *status = NULL;
    const H5S_t *mem_space = NULL;
    const H5S_t *file_space = NULL;
    char fake_char;
    size_t size;
    H5VL_iod_io_info_t *info;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL != dset->common.request) {
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS request");
        dset->common.request->req = H5MM_xfree(dset->common.request->req);
        dset->common.request = H5MM_xfree(dset->common.request);
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

    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which
     * doesn't support this feature.
     */
    if(!buf)
        buf = &fake_char;

    size = H5Sget_simple_extent_npoints(mem_space_id) *  H5Tget_size(mem_type_id);

    if(NULL == (bds_handle = (bds_handle_t *)H5MM_malloc(sizeof(bds_handle_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a buld data transfer handle");

    /* Register memory */
    if(S_SUCCESS != bds_handle_create(buf, size, BDS_READWRITE, bds_handle))
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");

    /* Fill input structure */
    input.iod_oh = dset->remote_dset.iod_oh;
    input.scratch_oh = dset->remote_dset.scratch_oh;
    input.bds_handle = *bds_handle;
    input.dxpl_id = dxpl_id;
    input.space_id = file_space_id;

    status = (H5VL_iod_read_status_t *)malloc(sizeof(H5VL_iod_read_status_t));

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a FS request");

    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_DSET_READ_ID, &input, status, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset read");

    /* setup info struct for I/O request */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_malloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a request");
    info->status = status;
    info->bds_handle = bds_handle;

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    request->type = FS_DSET_READ;
    request->data = info;
    request->req = fs_req;
    request->obj_name = HDstrdup(dset->common.obj_name);
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dset->common.file, request);

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
                       hid_t file_space_id, hid_t dxpl_id, const void *buf, hid_t UNUSED req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    H5VL_iod_dset_io_input_t input;
    fs_request_t *fs_req = NULL;
    bds_handle_t *bds_handle;
    H5VL_iod_request_t *request = NULL;
    const H5S_t *mem_space = NULL;
    const H5S_t *file_space = NULL;
    char fake_char;
    int *status = NULL;
    size_t size;
    H5VL_iod_io_info_t *info;
    uint32_t cs;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL != dset->common.request) {
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS request");
        dset->common.request->req = H5MM_xfree(dset->common.request->req);
        dset->common.request = H5MM_xfree(dset->common.request);
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

    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which
     * doesn't support this feature.
     */
    if(!buf)
        buf = &fake_char;

    size = H5Sget_simple_extent_npoints(mem_space_id) *  H5Tget_size(mem_type_id);
    cs = H5_checksum_fletcher32(buf, size);
    printf("Checksum Generated for data at client: %u\n", cs);

    if(NULL == (bds_handle = (bds_handle_t *)H5MM_malloc(sizeof(bds_handle_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a bulk data transfer handle");

    /* Register memory */
    if(S_SUCCESS != bds_handle_create((void *)buf, size, BDS_READ_ONLY, bds_handle))
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't create Bulk Data Handle");

    /* Fill input structure */
    input.iod_oh = dset->remote_dset.iod_oh;
    input.scratch_oh = dset->remote_dset.scratch_oh;
    input.bds_handle = *bds_handle;
    input.checksum = cs;
    input.dxpl_id = dxpl_id;
    input.space_id = file_space_id;

    status = (int *)malloc(sizeof(int));

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a FS request");

    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_DSET_WRITE_ID, &input, status, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship dataset write");

    /* setup info struct for I/O request */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_malloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a request");
    info->status = status;
    info->bds_handle = bds_handle;

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    request->type = FS_DSET_WRITE;
    request->data = info;
    request->req = fs_req;
    request->obj_name = HDstrdup(dset->common.obj_name);
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dset->common.file, request);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_close
 *
 * Purpose:	Closes a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_dataset_close(void *_dset, hid_t UNUSED req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    fs_request_t *fs_req;
    int *status;
    H5VL_iod_request_t *request;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL_iod_request_wait_some(dset->common.file, dset->common.obj_name) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS requests");

    if(NULL == (fs_req = (fs_request_t *)H5MM_malloc(sizeof(fs_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate a FS request");

    status = (int *)malloc(sizeof(int));
    /* forward the call to the IONs */
    if(fs_forward(PEER, H5VL_DSET_CLOSE_ID, &dset->remote_dset, status, fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship dset close");

    /* setup a request to track completion of the operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");
    request->type = FS_DSET_CLOSE;
    request->data = status;
    request->req = fs_req;
    request->next = request->prev = NULL;
    /* add request to container's linked list */
    H5VL_iod_request_add(dset->common.file, request);

    free(dset->common.obj_name);
    if(H5Pclose(dset->remote_dset.dcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
    if(H5Pclose(dset->dapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
    if(H5Tclose(dset->remote_dset.type_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
    if(H5Sclose(dset->remote_dset.space_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dspace");

    dset = H5FL_FREE(H5VL_iod_dset_t, dset);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_close() */
