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
 *              January, 2012
 *
 * Purpose:	The mds VOL plugin 
 */

#define H5A_PACKAGE		/*suppress error about including H5Apkg	  */
#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */
#define H5L_PACKAGE		/*suppress error about including H5Lpkg   */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */
#define H5R_PACKAGE		/*suppress error about including H5Rpkg	  */
#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */
#define H5T_PACKAGE		/*suppress error about including H5Tpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_mds_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"             /* Attribute pkg                        */
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Dpkg.h"             /* Dataset pkg                          */
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5Fprivate.h"		/* File access				*/
#include "H5FDmpi.h"            /* MPI-based file drivers		*/
#include "H5FDmds.h"            /* MDS file driver      		*/
#include "H5FDmdc.h"            /* MDC file driver      		*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HGprivate.h"	/* Global Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* links                                */
#include "H5Lpkg.h"             /* links headers			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Rpkg.h"		/* References   			*/
#include "H5Spkg.h"		/* Dataspaces   			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Tpkg.h"		/* Datatypes				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLmds.h"            /* MDS VOL plugin			*/
#include "H5VLmdserver.h"       /* MDS helper stuff			*/
#include "H5VLprivate.h"	/* VOL plugins				*/

#ifdef H5_HAVE_PARALLEL

/* Prototypes */
static void *H5VL_mds_fapl_copy(const void *_old_fa);
static herr_t H5VL_mds_fapl_free(void *_fa);

/* Atrribute callbacks */
static void *H5VL_mds_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t req);
static void *H5VL_mds_attr_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t req);
static herr_t H5VL_mds_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t req);
static herr_t H5VL_mds_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t req);
//static herr_t H5VL_mds_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_attr_remove(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t req);
static herr_t H5VL_mds_attr_close(void *attr, hid_t req);

/* Datatype callbacks */
static void *H5VL_mds_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
static void *H5VL_mds_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t req);
static ssize_t H5VL_mds_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t req);
static herr_t H5VL_mds_datatype_close(void *dt, hid_t req);

/* Dataset callbacks */
static void *H5VL_mds_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
static void *H5VL_mds_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t req);
static herr_t H5VL_mds_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                    hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
static herr_t H5VL_mds_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                     hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
static herr_t H5VL_mds_dataset_set_extent(void *dset, const hsize_t size[], hid_t req);
//static herr_t H5VL_mds_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_dataset_close(void *dset, hid_t req);

/* File callbacks */
static void *H5VL_mds_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
static void *H5VL_mds_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
//static herr_t H5VL_mds_file_get(void *file, H5VL_file_get_t get_type, hid_t req, va_list arguments);
//static herr_t H5VL_mds_file_misc(void *file, H5VL_file_misc_t misc_type, hid_t req, va_list arguments);
//static herr_t H5VL_mds_file_optional(void *file, H5VL_file_optional_t optional_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t req);
static herr_t H5VL_mds_file_close(void *file, hid_t req);

/* Group callbacks */
static void *H5VL_mds_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
static void *H5VL_mds_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t req);
//static herr_t H5VL_mds_group_get(void *obj, H5VL_group_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_group_close(void *grp, hid_t req);

/* Link callbacks */
static herr_t H5VL_mds_link_create(H5VL_link_create_type_t create_type, void *obj, 
                                      H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t req);
static herr_t H5VL_mds_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                    void *dst_obj, H5VL_loc_params_t loc_params2,
                                    hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req);
#if 0
static herr_t H5VL_mds_link_iterate(void *obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                       H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                                       H5L_iterate_t op, void *op_data, hid_t req);
static herr_t H5VL_mds_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_link_remove(void *obj, H5VL_loc_params_t loc_params, hid_t req);

/* Object callbacks */
static void *H5VL_mds_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t req);
static herr_t H5VL_mds_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                                      void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                                      hid_t ocpypl_id, hid_t lcpl_id, hid_t req);
static herr_t H5VL_mds_object_visit(void *obj, H5VL_loc_params_t loc_params, H5_index_t idx_type, 
                                       H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t req);
static herr_t H5VL_mds_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_object_optional(void *obj, H5VL_loc_params_t loc_params, H5VL_object_optional_t optional_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_object_close(void *obj, H5VL_loc_params_t loc_params, hid_t req);
#endif

/* MDS-specific file access properties */
typedef struct H5VL_mds_fapl_t {
    MPI_Comm		comm;		/*communicator			*/
    MPI_Info		info;		/*file information		*/
} H5VL_mds_fapl_t;

static H5VL_class_t H5VL_mds_g = {
    MDS,
    "mds",				     /* name */
    NULL,                                    /* initialize */
    NULL,                                    /* terminate */
    sizeof(H5VL_mds_fapl_t),		     /*fapl_size */
    H5VL_mds_fapl_copy,			     /*fapl_copy */
    H5VL_mds_fapl_free, 		     /*fapl_free */
    {                                        /* attribute_cls */
        H5VL_mds_attr_create,                /* create */
        H5VL_mds_attr_open,                  /* open */
        H5VL_mds_attr_read,                  /* read */
        H5VL_mds_attr_write,                 /* write */
        NULL,//H5VL_mds_attr_get,                   /* get */
        H5VL_mds_attr_remove,                /* remove */
        H5VL_mds_attr_close                  /* close */
    },
    {                                        /* datatype_cls */
        H5VL_mds_datatype_commit,            /* commit */
        H5VL_mds_datatype_open,              /* open */
        H5VL_mds_datatype_get_binary,        /* get_size */
        H5VL_mds_datatype_close              /* close */
    },
    {                                        /* dataset_cls */
        H5VL_mds_dataset_create,             /* create */
        H5VL_mds_dataset_open,               /* open */
        H5VL_mds_dataset_read,               /* read */
        H5VL_mds_dataset_write,              /* write */
        H5VL_mds_dataset_set_extent,         /* set extent */
        NULL,//H5VL_mds_dataset_get,                /* get */
        H5VL_mds_dataset_close               /* close */
    },
    {                                        /* file_cls */
        H5VL_mds_file_create,                /* create */
        H5VL_mds_file_open,                  /* open */
        H5VL_mds_file_flush,                 /* flush */
        NULL,//H5VL_mds_file_get,                   /* get */
        NULL,//H5VL_mds_file_misc,                  /* misc */
        NULL,//H5VL_mds_file_optional,              /* optional */
        H5VL_mds_file_close                  /* close */
    },
    {                                        /* group_cls */
        H5VL_mds_group_create,               /* create */
        H5VL_mds_group_open,                 /* open */
        NULL,//H5VL_mds_group_get,                  /* get */
        H5VL_mds_group_close                 /* close */
    },
    {                                        /* link_cls */
        H5VL_mds_link_create,                /* create */
        H5VL_mds_link_move,                  /* move */
        NULL,//H5VL_mds_link_iterate,               /* iterate */
        NULL,//H5VL_mds_link_get,                   /* get */
        NULL//H5VL_mds_link_remove                 /* remove */
    },
    {                                        /* object_cls */
        NULL,//H5VL_mds_object_open,                /* open */
        NULL,//H5VL_mds_object_copy,                /* copy */
        NULL,//H5VL_mds_object_visit,               /* visit */
        NULL,//H5VL_mds_object_get,                 /* get */
        NULL,//H5VL_mds_object_misc,                /* misc */
        NULL,//H5VL_mds_object_optional,            /* optional */
        NULL,//H5VL_mds_object_close                /* close */
    }
};


/*--------------------------------------------------------------------------
NAME
   H5VL_mds_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_mds_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_mds_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_mds_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5VL_mds_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the mds plugin.
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
H5VL_class_t *
H5VL_mds_init(void)
{
    H5VL_class_t *ret_value = NULL;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set return value */
    ret_value = &H5VL_mds_g;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_init() */


/*---------------------------------------------------------------------------
 * Function:	H5VL_mds_register
 *
 * Purpose:	utility routine to register an ID with the mds VOL plugin 
 *              as an auxilary object
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2012
 *
 *---------------------------------------------------------------------------
 */
hid_t
H5VL_mds_register(H5I_type_t type, void *obj, hbool_t app_ref)
{
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    H5T_t   *dt = NULL;
    hid_t    ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(obj);

    /* Build the vol plugin struct */
    if(NULL == (vol_plugin = (H5VL_t *)H5MM_calloc(sizeof(H5VL_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    vol_plugin->cls = &H5VL_mds_g;
    vol_plugin->nrefs = 1;

    /* if this is a named datatype, we need to create the two-fold datatype 
       to be comaptible with the VOL */
    if(H5I_DATATYPE == type) {
        /* Copy the dataset's datatype */
        if(NULL == (dt = H5T_copy((H5T_t *)obj, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to copy datatype")

        H5T_set_vol_object(dt, obj);
        obj = (void *) dt;
    }

    /* Get an atom for the object with the VOL information as the auxilary struct*/
    if((ret_value = H5I_register2(type, obj, (void *)vol_plugin, app_ref)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

done:
    if(ret_value < 0 && dt && H5T_close(dt) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release datatype")
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5VL_mds_register */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_fapl_copy
 *
 * Purpose:	Copies the mds-specific file access properties.
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
H5VL_mds_fapl_copy(const void *_old_fa)
{
    void		  *ret_value = NULL;
    const H5VL_mds_fapl_t *old_fa = (const H5VL_mds_fapl_t*)_old_fa;
    H5VL_mds_fapl_t	  *new_fa = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (new_fa = (H5VL_mds_fapl_t *)H5MM_malloc(sizeof(H5VL_mds_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the general information */
    HDmemcpy(new_fa, old_fa, sizeof(H5VL_mds_fapl_t));

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")
    ret_value = new_fa;

done:
    if (NULL == ret_value){
	/* cleanup */
	if (new_fa)
	    H5MM_xfree(new_fa);
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_fapl_free
 *
 * Purpose:	Frees the mds-specific file access properties.
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
H5VL_mds_fapl_free(void *_fa)
{
    herr_t		ret_value = SUCCEED;
    H5VL_mds_fapl_t	*fa = (H5VL_mds_fapl_t*)_fa;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    assert(fa);

    /* Free the internal communicator and INFO object */
    assert(MPI_COMM_NULL!=fa->comm);
    H5FD_mpi_comm_info_free(&fa->comm, &fa->info);
    H5MM_xfree(fa);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_fapl_free() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_mds
 *
 * Purpose:	Modify the file access property list to use the H5VL_MDS
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
H5Pset_fapl_mds(hid_t fapl_id, MPI_Comm comm, MPI_Info info)
{
    H5VL_mds_fapl_t    fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iMcMi", fapl_id, comm, info);

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list")
    if(MPI_COMM_NULL == comm)
	HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid communicator")

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.info = info;
    /*
    fa.meta_name = meta_name;
    fa.meta_fapl_id = meta_fapl_id;
    fa.raw_name = raw_name;
    fa.raw_fapl_id = raw_fapl_id;
    */
    /* duplication is done during setting. */
    ret_value = H5P_set_vol(plist, &H5VL_mds_g, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_mds() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_file_create
 *
 * Purpose:	Creates a file for raw data and tells the metadata server
 *              to create the metadata file.
 *
 * Return:	Success:	the file object 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, 
                     hid_t UNUSED req)
{
    void *send_buf;
    size_t buf_size;
    hid_t temp_fapl; /* the fapl created here for the underlying VFD for clients */
    H5VL_mds_fapl_t *fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    int my_rank, my_size;
    H5F_t *new_file = NULL;
    hid_t mds_file; /* Metadata file ID recieved from the MDS */
    H5VL_mds_file_t *file = NULL;
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
    if(NULL == (fa = (H5VL_mds_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get MDS info struct")
    MPI_Comm_rank(fa->comm, &my_rank);
    MPI_Comm_size(fa->comm, &my_size);

    /* the first process in the communicator will tell the MDS process to create the metadata file */
    if (0 == my_rank) {
        /* determine the size of the buffer needed to encode the parameters */
        if(H5VL__encode_file_create_params(NULL, &buf_size, name, flags, fcpl_id, fapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed")

        /* allocate the buffer for encoding the parameters */
        if(NULL == (send_buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* encode the parameters */
        if(H5VL__encode_file_create_params(send_buf, &buf_size, name, flags, fcpl_id, fapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode file create parameters")

        MPI_Pcontrol(0);
        /* send the request to the MDS process and recieve the metadata file ID */
        if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                       &mds_file, sizeof(hid_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                       MPI_COMM_WORLD, MPI_STATUS_IGNORE))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message")
        MPI_Pcontrol(1);
        H5MM_free(send_buf);
    }

    /* Process 0 Bcasts the metadata file ID to other processes if there are any */
    if(my_size > 1) {
        if(MPI_SUCCESS != MPI_Bcast(&mds_file, sizeof(hid_t), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message")
    }
    HDassert(mds_file);

    /* set the underlying VFD for clients (MDC)*/
    temp_fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(temp_fapl, fa->comm, fa->info);
    if(H5P_set_fapl_mdc(fapl_id, name, temp_fapl, mds_file) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, NULL, "failed to set MDC plist")

    /* Create the raw data file */ 
    if(NULL == (new_file = H5F_mdc_open(name, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file")

    /* set the file space manager to use the VFD */
    new_file->shared->fs_strategy = H5F_FILE_SPACE_VFD;

    new_file->id_exists = TRUE;

    /* allocate the file object that is returned to the user */
    if(NULL == (file = (H5VL_mds_file_t *)calloc(1, sizeof(H5VL_mds_file_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* store the metadata file ID in the local raw data file struct 
    H5FD_mdc_set_mdfile(new_file, mds_file);
    */

    /* create the file object that is passed to the API layer */
    file->common.obj_type = H5I_FILE; 
    file->common.obj_id = mds_file;
    file->common.raw_file = new_file;

    ret_value = (void *)file;
done:
    if(NULL == ret_value && new_file) 
        if(H5F_close(new_file) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_file_open
 *
 * Purpose:	Opens a file for raw data and tells the metadata server
 *              to open the metadata file.
 *
 * Return:	Success:	the file object 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t UNUSED req)
{
    void *send_buf;
    size_t buf_size;
    hid_t temp_fapl; /* the fapl opend here for the underlying VFD for clients */
    H5VL_mds_fapl_t *fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    int my_rank, my_size;
    H5F_t *new_file = NULL;
    hid_t mds_file; /* Metadata file ID recieved from the MDS */
    H5VL_mds_file_t *file = NULL;
    void  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* obtain the process rank from the communicator attached to the fapl ID */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_mds_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get MDS info struct")
    MPI_Comm_rank(fa->comm, &my_rank);
    MPI_Comm_size(fa->comm, &my_size);

    /* the first process in the communicator will tell the MDS process to open the metadata file */
    if (0 == my_rank) {
        /* determine the size of the buffer needed to encode the parameters */
        if(H5VL__encode_file_open_params(NULL, &buf_size, name, flags, fapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed")

        /* allocate the buffer for encoding the parameters */
        if(NULL == (send_buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* encode the parameters */
        if(H5VL__encode_file_open_params(send_buf, &buf_size, name, flags, fapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode file open parameters")

        MPI_Pcontrol(0);
        /* send the request to the MDS process and recieve the metadata file ID */
        if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                       &mds_file, sizeof(hid_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                       MPI_COMM_WORLD, MPI_STATUS_IGNORE))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message")
        MPI_Pcontrol(1);
        H5MM_free(send_buf);
    }

    /* Process 0 Bcasts the metadata file ID to other processes if there are any */
    if(my_size > 1) {
        if(MPI_SUCCESS != MPI_Bcast(&mds_file, sizeof(hid_t), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message")
    }
    HDassert(mds_file);

    /* set the underlying VFD for clients (MDC)*/
    temp_fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(temp_fapl, fa->comm, fa->info);
    if(H5P_set_fapl_mdc(fapl_id, name, temp_fapl, mds_file) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, NULL, "failed to set MDC plist")

    /* Open the raw data file */ 
    if(NULL == (new_file = H5F_mdc_open(name, flags, H5P_FILE_CREATE_DEFAULT, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")

    /* set the file space manager to use the VFD */
    new_file->shared->fs_strategy = H5F_FILE_SPACE_VFD;

    new_file->id_exists = TRUE;

    /* allocate the file object that is returned to the user */
    if(NULL == (file = (H5VL_mds_file_t *)calloc(1, sizeof(H5VL_mds_file_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* store the metadata file ID in the local raw data file struct 
    H5FD_mdc_set_mdfile(new_file, mds_file);
    */

    /* open the file object that is passed to the API layer */
    file->common.obj_type = H5I_FILE; 
    file->common.obj_id = mds_file;
    file->common.raw_file = new_file;

    ret_value = (void *)file;
done:
    if(NULL == ret_value && new_file) 
        if(H5F_close(new_file) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_file_flush
 *
 * Purpose:	Flushes a file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not flushed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_mds_file_flush(void *_obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, 
                    hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj;
    hid_t obj_id = obj->obj_id;
    void *send_buf = NULL;
    size_t buf_size;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_file_flush_params(NULL, &buf_size, obj_id, loc_params, scope) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine buffer size needed")

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode flush parameters */
    if(H5VL__encode_file_flush_params(send_buf, &buf_size, obj_id, loc_params, scope) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode file flush parameters")

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

#if 0
    /* MSC - I don't think we have raw data to flush here */
    if(H5F_INTENT(f) & H5F_ACC_RDWR) {
        if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    } /* end if */
#endif

    H5MM_free(send_buf);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_file_close
 *
 * Purpose:	Closes a file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_mds_file_close(void *obj, hid_t UNUSED req)
{
    H5VL_mds_file_t *file = (H5VL_mds_file_t *)obj;
    H5F_t *f = file->common.raw_file;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

#if 0
    /* MSC - I don't think we have raw data to flush here */
    if(H5F_INTENT(f) & H5F_ACC_RDWR) {
        if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    } /* end if */
#endif

    /* close the file */
    if((ret_value = H5F_close(f)) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close file");

    H5MM_free(file);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_attr_create
 *
 * Purpose:	Sends a request to the MDS to create a attr
 *
 * Return:	Success:	attr object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_attr_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t acpl_id, 
                     hid_t aapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to create the attr */
    H5VL_mds_attr_t *attr = NULL; /* the attr object that is created and passed to the user */
    H5A_t          *new_attr = NULL; /* the lighweight attr struct used to hold the attr's metadata */
    void           *send_buf = NULL; /* buffer where the attr create request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    H5P_genplist_t *plist;
    H5T_t          *dt = NULL, *dt1 = NULL;
    H5VL_mds_dtype_t *type = NULL;
    H5S_t          *space = NULL;
    hid_t          type_id, space_id;
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the acpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get datatype, dataspace, and lcpl IDs that were added in the acpl at the API layer */
    if(H5P_get(plist, H5VL_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id");
    if(H5P_get(plist, H5VL_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id");

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_attr_create_params(NULL, &buf_size, obj->obj_id, loc_params, name, 
                                       acpl_id, aapl_id, type_id, space_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_attr_create_params(send_buf, &buf_size, obj->obj_id, loc_params, name, 
                                       acpl_id, aapl_id, type_id, space_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode attr create parameters");

    /* allocate the attr object that is returned to the user */
    if(NULL == (attr = (H5VL_mds_attr_t *)calloc(1, sizeof(H5VL_mds_attr_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata attribute ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &(attr->common.obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a type");
    /* Get the actual datatype object if this is a named datatype */
    if(NULL == (type = (H5VL_mds_dtype_t *)H5T_get_named_type(dt)))
        dt1 = dt;
    else
        dt1 = type->dtype;

    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a data space");

    /* create the "lightweight" client attr */
    if(NULL == (new_attr = H5A__mdc_create(name, dt1, space, acpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "failed to create client attr object");

    new_attr->oloc.file = obj->raw_file;
    /* set the attr struct in the high level object */
    attr->attr = new_attr;

    /* set common object parameters */
    attr->common.obj_type = H5I_ATTR;
    attr->common.raw_file = obj->raw_file;

    ret_value = (void *)attr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_attr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_attr_open
 *
 * Purpose:	Sends a request to the MDS to open a attr
 *
 * Return:	Success:	attr object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_attr_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                   hid_t aapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to create the attr */
    H5VL_mds_attr_t *attr = NULL; /* the attr object that is created and passed to the user */
    H5A_t          *new_attr = NULL; /* the lighweight attr struct used to hold the attr's metadata */
    void           *send_buf = NULL; /* buffer where the attr create request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    int            incoming_msg_size; /* incoming buffer size for MDS returned attr */
    void           *recv_buf = NULL; /* buffer to hold incoming data from MDS */
    MPI_Status     status;
    uint8_t        *p; /* pointer into recv_buf; used for decoding */
    hid_t          acpl_id=H5P_ATTRIBUTE_CREATE_DEFAULT;
    H5T_t          *type = NULL;
    H5S_t          *space = NULL;
    size_t         type_size, space_size, acpl_size;
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_attr_open_params(NULL, &buf_size, obj->obj_id, loc_params, name, aapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_attr_open_params(send_buf, &buf_size, obj->obj_id, loc_params, name, aapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, NULL, "unable to encode attr open parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message");

    /* allocate the attr object that is returned to the user */
    if(NULL == (attr = (H5VL_mds_attr_t *)calloc(1, sizeof(H5VL_mds_attr_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");
    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message");
    MPI_Pcontrol(1);

    p = (uint8_t *)recv_buf;

    /* decode the attr ID at the MDS */
    INT32DECODE(p, attr->common.obj_id);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, acpl_size);
    /* decode property lists if they are not default*/
    if(acpl_size) {
        if((acpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, NULL, "unable to decode property list");
        p += acpl_size;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    if(type_size) {
        /* decode the datatype */
        if(NULL == (type = H5T_decode((unsigned char *)p)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "unable to decode datatype");
        p += type_size;
    }

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    if(space_size) {
        /* decode the dataspace */
        if(NULL == (space = H5S_decode((unsigned char *)p)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, NULL, "unable to decode dataspace");
        p += space_size;
    }

    /* create the "lightweight" client attr */
    if(NULL == (new_attr = H5A__mdc_create(name, type, space, acpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "failed to create client attr object");

    new_attr->oloc.file = obj->raw_file;
    /* set the attr struct in the high level object */
    attr->attr = new_attr;

    /* set common object parameters */
    attr->common.obj_type = H5I_ATTR;
    attr->common.raw_file = obj->raw_file;

    H5MM_free(send_buf);
    H5MM_free(recv_buf);
    ret_value = (void *)attr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_attr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_attr_read
 *
 * Purpose:	Reads in data from attribute.
 *
 *              Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL_mds_attr_read(void *obj, hid_t dtype_id, void *buf, hid_t UNUSED req)
{
    H5VL_mds_attr_t *attr = (H5VL_mds_attr_t *)obj;
    void            *send_buf = NULL;
    size_t           buf_size;
    size_t	     dst_type_size;	/* size of destination type */ 
    hssize_t	     snelmts;		/* elements in attribute */
    size_t	     nelmts;		/* elements in attribute*/
    herr_t           ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    dst_type_size = H5T_GET_SIZE((H5T_t *)H5I_object(dtype_id));
    if((snelmts = H5S_GET_EXTENT_NPOINTS(attr->attr->shared->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, FAIL, "dataspace is invalid");
    H5_ASSIGN_OVERFLOW(nelmts, snelmts, hssize_t, size_t);

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_attr_read_params(NULL, &buf_size, attr->common.obj_id, dtype_id, 
                                     dst_type_size * nelmts) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_attr_read_params(send_buf, &buf_size, attr->common.obj_id, dtype_id,
                                     dst_type_size * nelmts) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr read parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and receive back the attribute data */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   buf, (int)(dst_type_size * nelmts), MPI_BYTE, MDS_RANK, 
                                   H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send and receive message");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_attr_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_attr_write
 *
 * Purpose:	Writes in data from attribute.
 *
 *              Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL_mds_attr_write(void *obj, hid_t dtype_id, const void *buf, hid_t UNUSED req)
{
    H5VL_mds_attr_t *attr = (H5VL_mds_attr_t *)obj;
    void            *send_buf = NULL;
    size_t           buf_size;
    size_t	     dst_type_size;	/* size of destination type */ 
    hssize_t	     snelmts;		/* elements in attribute */
    size_t	     nelmts;		/* elements in attribute*/
    herr_t           ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    dst_type_size = H5T_GET_SIZE((H5T_t *)H5I_object(dtype_id));
    if((snelmts = H5S_GET_EXTENT_NPOINTS(attr->attr->shared->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, FAIL, "dataspace is invalid");
    H5_ASSIGN_OVERFLOW(nelmts, snelmts, hssize_t, size_t);

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_attr_write_params(NULL, &buf_size, attr->common.obj_id, dtype_id, 
                                      buf, dst_type_size * nelmts) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_attr_write_params(send_buf, &buf_size, attr->common.obj_id, dtype_id,
                                      buf, dst_type_size * nelmts) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr write parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and receive back the attribute data */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t),  MPI_BYTE, MDS_RANK, 
                                   H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send and receive message");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_attr_remove
 *
 * Purpose:	Removes a attr.
 *
 * Return:	Success:	0
 *		Failure:	-1, attr not removed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL_mds_attr_remove(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj;
    void            *send_buf = NULL;
    size_t           buf_size;
    herr_t           ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_attr_remove_params(NULL, &buf_size, obj->obj_id, loc_params, name) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_attr_remove_params(send_buf, &buf_size, obj->obj_id, loc_params, name) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr remove parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_attr_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_attr_close
 *
 * Purpose:	Closes a attr.
 *
 * Return:	Success:	0
 *		Failure:	-1, attr not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_mds_attr_close(void *obj, hid_t UNUSED req)
{
    H5VL_mds_attr_t *attr = (H5VL_mds_attr_t *)obj;
    void            *send_buf = NULL;
    size_t           buf_size;
    herr_t           ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    buf_size = 1 /* request type */ + sizeof(int) /* attr id */;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode attr close params */
    if(H5VL__encode_attr_close_params(send_buf, &buf_size, attr->common.obj_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode attr close params")

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

    if(H5A__mdc_close(attr->attr) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute");

    H5MM_free(attr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_attr_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_create
 *
 * Purpose:	Sends a request to the MDS to create a dataset
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
H5VL_mds_dataset_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, 
                        hid_t dapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to create the dataset */
    H5VL_mds_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    H5D_t          *new_dset = NULL; /* the lighweight dataset struct used to hold the dataset's metadata */
    void           *send_buf = NULL; /* buffer where the dataset create request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    int            incoming_msg_size; /* incoming buffer size for MDS returned dataset */
    void           *recv_buf = NULL; /* buffer to hold incoming data from MDS */
    H5P_genplist_t *plist;
    MPI_Status     status;
    uint8_t        *p = NULL; /* pointer into recv_buf; used for decoding */
    hid_t          type_id, space_id, lcpl_id;
    H5O_layout_t   layout;        /* Dataset's layout information */
    void           *ret_value;

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

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_dataset_create_params(NULL, &buf_size, obj->obj_id, loc_params, name, 
                       dcpl_id, dapl_id, type_id, space_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_dataset_create_params(send_buf, &buf_size, obj->obj_id, loc_params, name, 
                                          dcpl_id, dapl_id, type_id, space_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode dataset create parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message");

    H5MM_free(send_buf);

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = (H5VL_mds_dset_t *)calloc(1, sizeof(H5VL_mds_dset_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");
    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    recv_buf = (void *)H5MM_malloc (incoming_msg_size);

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message");
    MPI_Pcontrol(1);

    p = (uint8_t *)recv_buf;

    /* decode the dataset ID at the MDS */
    INT32DECODE(p, dset->common.obj_id);

    /* decode the dataset layout */
    if(FAIL == H5D__decode_layout(p, &layout))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "failed to decode dataset layout");

    /* Initialize the dataset object */
    if(NULL == (new_dset = (H5D_t *)H5MM_malloc(sizeof(H5D_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    /* create the "lightweight" client dataset */
    if(NULL == (new_dset = H5D__mdc_create(obj->raw_file, type_id, space_id, dcpl_id, dapl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "failed to create client dataset object");

    /* set the layout of the dataset */
    new_dset->shared->layout = layout;
    /* Set the dataset's I/O operations */
    if(H5D__layout_set_io_ops(new_dset) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize I/O operations");

#if 0
    {
        unsigned u;
        //H5O_layout_t layout = dataset->shared->layout;

        printf("type %d version %d\n", layout.type, layout.version);
        printf("ndims %d size %d, nchunks %llu\n", layout.u.chunk.ndims,
               layout.u.chunk.size, layout.u.chunk.nchunks);
        for(u = 0; u < layout.u.chunk.ndims; u++) {
            printf("%d: dim %d chunk %llu, down_chunk %llu\n", u, layout.u.chunk.dim[u],
                   layout.u.chunk.chunks[u], layout.u.chunk.down_chunks[u]);
        }
        printf("idx type %d  address %llu\n", layout.storage.u.chunk.idx_type, 
               layout.storage.u.chunk.idx_addr);
    }
#endif

    new_dset->oloc.file = obj->raw_file;
    /* set the dataset struct in the high level object */
    dset->dset = new_dset;

    /* set common object parameters */
    dset->common.obj_type = H5I_DATASET;
    dset->common.raw_file = obj->raw_file;

    H5MM_free(recv_buf);
    ret_value = (void *)dset;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_open
 *
 * Purpose:	Sends a request to the MDS to open a dataset
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
H5VL_mds_dataset_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                      hid_t dapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to create the dataset */
    H5VL_mds_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    H5D_t          *new_dset = NULL; /* the lighweight dataset struct used to hold the dataset's metadata */
    void           *send_buf = NULL; /* buffer where the dataset create request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    int            incoming_msg_size; /* incoming buffer size for MDS returned dataset */
    void           *recv_buf = NULL; /* buffer to hold incoming data from MDS */
    MPI_Status     status;
    uint8_t        *p; /* pointer into recv_buf; used for decoding */
    hid_t          type_id=FAIL, space_id=FAIL, dcpl_id=H5P_DATASET_CREATE_DEFAULT;
    H5O_layout_t   layout;        /* Dataset's layout information */
    size_t         type_size, space_size, dcpl_size, layout_size;
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_dataset_open_params(NULL, &buf_size, obj->obj_id, loc_params, 
                                        name, dapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_dataset_open_params(send_buf, &buf_size, obj->obj_id, loc_params, 
                                        name, dapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode dataset open parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = (H5VL_mds_dset_t *)calloc(1, sizeof(H5VL_mds_dset_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");
    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message");
    MPI_Pcontrol(1);

    p = (uint8_t *)recv_buf;

    /* decode the dataset ID at the MDS */
    INT32DECODE(p, dset->common.obj_id);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dcpl_size);
    /* decode property lists if they are not default*/
    if(dcpl_size) {
        if((dcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, NULL, "unable to decode property list");
        p += dcpl_size;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    if(type_size) {
        /* decode the datatype */
        if((type_id = H5Tdecode((unsigned char *)p)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "unable to decode datatype");
        p += type_size;
    }

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    if(space_size) {
        /* decode the dataspace */
        if((space_id = H5Sdecode((unsigned char *)p)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, NULL, "unable to decode dataspace");
        p += space_size;
    }

    /* decode the layout size */
    UINT64DECODE_VARLEN(p, layout_size);
    /* decode the dataset layout */
    if(FAIL == H5D__decode_layout(p, &layout))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "failed to decode dataset layout");
    p += layout_size;

    /* Initialize the dataset object */
    if(NULL == (new_dset = (H5D_t *)H5MM_malloc(sizeof(H5D_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    /* create the "lightweight" client dataset */
    if(NULL == (new_dset = H5D__mdc_create(obj->raw_file, type_id, space_id, dcpl_id, dapl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "failed to create client dataset object");

    /* set the layout of the dataset */
    new_dset->shared->layout = layout;
    /* Set the dataset's I/O operations */
    if(H5D__layout_set_io_ops(new_dset) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize I/O operations");

    new_dset->oloc.file = obj->raw_file;
    /* set the dataset struct in the high level object */
    dset->dset = new_dset;

    /* set common object parameters */
    dset->common.obj_type = H5I_DATASET;
    dset->common.raw_file = obj->raw_file;

    H5MM_free(send_buf);
    H5MM_free(recv_buf);
    ret_value = (void *)dset;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_read
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
H5VL_mds_dataset_read(void *obj, hid_t mem_type_id, hid_t mem_space_id,
                      hid_t file_space_id, hid_t dxpl_id, void *buf, hid_t UNUSED req)
{
    H5VL_mds_dset_t *dset = (H5VL_mds_dset_t *)obj;
    //hid_t          dset_id = dset->common.obj_id; /* the dataset ID at the MDS */
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

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

    /* read raw data */
    if(H5D__mdc_read(dset->dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_write
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
H5VL_mds_dataset_write(void *obj, hid_t mem_type_id, hid_t mem_space_id,
                       hid_t file_space_id, hid_t dxpl_id, const void *buf, hid_t UNUSED req)
{
    H5VL_mds_dset_t *dset = (H5VL_mds_dset_t *)obj;
    //hid_t          dset_id = dset->common.obj_id; /* the dataset ID at the MDS */
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

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

    /* write raw data */
    if(H5D__mdc_write(dset->dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_set_extent
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
H5VL_mds_dataset_set_extent(void *obj, const hsize_t size[], hid_t UNUSED req)
{
    H5VL_mds_dset_t *dset = (H5VL_mds_dset_t *)obj;
    void            *send_buf = NULL;
    size_t           buf_size;
    int              rank = dset->dset->shared->space->extent.rank;
    herr_t           ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_dataset_set_extent_params(NULL, &buf_size, dset->common.obj_id, rank, size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_dataset_set_extent_params(send_buf, &buf_size, dset->common.obj_id, rank, size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode dataset set_extent parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_set_extent() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_close
 *
 * Purpose:	Closes a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_mds_dataset_close(void *obj, hid_t UNUSED req)
{
    H5VL_mds_dset_t *dset = (H5VL_mds_dset_t *)obj;
    void            *send_buf = NULL;
    size_t           buf_size;
    herr_t           ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    buf_size = 1 /* request type */ + sizeof(int) /* dset id */;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode dataset close params */
    if(H5VL__encode_dataset_close_params(send_buf, &buf_size, dset->common.obj_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode dataset close params")

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

    /* Free the dataset's memory structure */
    H5MM_free(dset->dset);
    //if(H5D_close(dset->dset) < 0)
    //HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "unable to close dataset");

    H5MM_free(dset);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_datatype_commit
 *
 * Purpose:	Commits a datatype inside a mds h5 file.
 *
 * Return:	Success:	datatype id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_datatype_commit(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, 
                         hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to create the datatype */
    H5VL_mds_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */
    H5T_t          *dt = NULL;
    void           *send_buf = NULL; /* buffer where the datatype create request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_datatype_commit_params(NULL, &buf_size, obj->obj_id, loc_params, name, 
                       type_id, lcpl_id, tcpl_id, tapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype ID");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_datatype_commit_params(send_buf, &buf_size, obj->obj_id, loc_params, name, 
                       type_id, lcpl_id, tcpl_id, tapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode datatype commit parameters");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dtype = (H5VL_mds_dtype_t *)calloc(1, sizeof(H5VL_mds_dtype_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata datatype ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &(dtype->common.obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

    if(dtype->common.obj_id < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "MDS failed to commit datatype");

    /* set common object parameters */
    dtype->common.obj_type = H5I_DATATYPE;
    dtype->common.raw_file = obj->raw_file;

    if(NULL == (dtype->dtype = H5T_copy(dt, H5T_COPY_TRANSIENT)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to copy");

    ret_value = (void *)dtype;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_datatype_open
 *
 * Purpose:	Opens a named datatype inside a mds h5 file.
 *
 * Return:	Success:	datatype id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_datatype_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                       hid_t tapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to create the datatype */
    H5VL_mds_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */
    void           *send_buf = NULL; /* buffer where the datatype create request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    int            incoming_msg_size; /* incoming buffer size for MDS returned dataset */
    void           *recv_buf = NULL; /* buffer to hold incoming data from MDS */
    MPI_Status     status;
    uint8_t        *p = NULL; /* pointer into recv_buf; used for decoding */
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_datatype_open_params(NULL, &buf_size, obj->obj_id, loc_params, name, 
                                         tapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_datatype_open_params(send_buf, &buf_size, obj->obj_id, loc_params, name, 
                       tapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode datatype open parameters");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dtype = (H5VL_mds_dtype_t *)calloc(1, sizeof(H5VL_mds_dtype_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dtype = (H5VL_mds_dtype_t *)calloc(1, sizeof(H5VL_mds_dtype_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");

    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    recv_buf = (void *)H5MM_malloc (incoming_msg_size);

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message");
    MPI_Pcontrol(1);

    p = (uint8_t *)recv_buf;

    /* decode the dataset ID at the MDS */
    INT32DECODE(p, dtype->common.obj_id);
    if(dtype->common.obj_id < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "MDS failed to open datatype");

    if(NULL == (dtype->dtype = H5T_decode((const unsigned char *)p)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "unable to decode datatype");

    /* set common object parameters */
    dtype->common.obj_type = H5I_DATATYPE;
    dtype->common.raw_file = obj->raw_file;

    ret_value = (void *)dtype;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_datatype_get_binary
 *
 * Purpose:	gets size required to encode the datatype
 *
 * Return:	Success:	datatype id. 
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5VL_mds_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t UNUSED req)
{
    H5VL_mds_dtype_t *dtype = (H5VL_mds_dtype_t *)obj;
    H5T_t       *type = dtype->dtype;
    size_t       nalloc = size;
    ssize_t      ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5T_encode(type, buf, &nalloc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't determine serialized length of datatype")

    ret_value = (ssize_t) nalloc;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_datatype_get_binary() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_datatype_close
 *
 * Purpose:	Closes an datatype.
 *
 * Return:	Success:	0
 *		Failure:	-1, datatype not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_mds_datatype_close(void *obj, hid_t UNUSED req)
{
    H5VL_mds_dtype_t *dtype = (H5VL_mds_dtype_t *)obj;
    void            *send_buf = NULL;
    size_t           buf_size;
    herr_t           ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    buf_size = 1 /* request type */ + sizeof(int) /* dtype id */;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode datatype close params */
    if(H5VL__encode_datatype_close_params(send_buf, &buf_size, dtype->common.obj_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode datatype close params")

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata file ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    if((ret_value = H5T_close(dtype->dtype)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close datatype")

    H5MM_free(send_buf);
    H5MM_free(dtype);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_group_create
 *
 * Purpose:	Sends a request to the MDS to create a group
 *
 * Return:	Success:	group object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_group_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, 
                      hid_t gapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to create the group */
    H5VL_mds_group_t *grp = NULL; /* the group object that is created and passed to the user */
    void           *send_buf = NULL; /* buffer where the group create request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    H5P_genplist_t *plist;
    hid_t          lcpl_id;
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_GRP_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id")

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_group_create_params(NULL, &buf_size, obj->obj_id, loc_params, name, 
                                        gcpl_id, gapl_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_group_create_params(send_buf, &buf_size, obj->obj_id, loc_params, name, 
                                        gcpl_id, gapl_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode group create parameters");

    /* allocate the group object that is returned to the user */
    if(NULL == (grp = (H5VL_mds_group_t *)calloc(1, sizeof(H5VL_mds_group_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata group ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &(grp->common.obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.raw_file = obj->raw_file;

    ret_value = (void *)grp;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_group_open
 *
 * Purpose:	Sends a request to the MDS to open a group
 *
 * Return:	Success:	group object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *H5VL_mds_group_open(void *_obj, H5VL_loc_params_t loc_params, const char *name, 
                                 hid_t gapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to open the group */
    H5VL_mds_group_t *grp = NULL; /* the group object that is opend and passed to the user */
    void           *send_buf = NULL; /* buffer where the group open request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_group_open_params(NULL, &buf_size, obj->obj_id, loc_params, name, gapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_group_open_params(send_buf, &buf_size, obj->obj_id, loc_params, name, gapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode group open parameters");

    /* allocate the group object that is returned to the user */
    if(NULL == (grp = (H5VL_mds_group_t *)calloc(1, sizeof(H5VL_mds_group_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata group ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &(grp->common.obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.raw_file = obj->raw_file;

    ret_value = (void *)grp;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_group_close
 *
 * Purpose:	Closes a group.
 *
 * Return:	Success:	0
 *		Failure:	-1, group not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_mds_group_close(void *obj, hid_t UNUSED req)
{
    H5VL_mds_group_t *grp = (H5VL_mds_group_t *)obj;
    void            *send_buf = NULL;
    size_t           buf_size;
    herr_t           ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    buf_size = 1 /* request type */ + sizeof(hid_t) /* group id */;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode group close params */
    if(H5VL__encode_group_close_params(send_buf, &buf_size, grp->common.obj_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode group close params")

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

    /* Free the group's memory structure */
    H5MM_free(grp);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_link_create
 *
 * Purpose:	Creates an hard/soft/UD/external links.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_mds_link_create(H5VL_link_create_type_t create_type, void *_obj, H5VL_loc_params_t loc_params,
                     hid_t lcpl_id, hid_t lapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj; /* location object to create the group */
    H5P_genplist_t  *plist;                     /* Property list pointer */
    void            *send_buf = NULL;
    size_t           buf_size = 0;
    herr_t           ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    switch (create_type) {
        case H5VL_LINK_CREATE_HARD:
            {
                hid_t id, cur_id;
                H5VL_mds_object_t *cur_obj;
                H5VL_loc_params_t cur_params;

                /* object is H5L_SAME_LOC */
                if(NULL == obj)
                    id = H5L_SAME_LOC;
                else
                    id = obj->obj_id;

                if(H5P_get(plist, H5VL_LINK_TARGET, &cur_obj) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current location id");
                if(H5P_get(plist, H5VL_LINK_TARGET_LOC_PARAMS, &cur_params) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current name");

                /* object is H5L_SAME_LOC */
                if(NULL == cur_obj)
                    cur_id = H5L_SAME_LOC;
                else
                    cur_id = cur_obj->obj_id;

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_link_create_params(NULL, &buf_size, create_type, id, loc_params, 
                                                   lcpl_id, lapl_id, cur_id, cur_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_link_create_params(send_buf, &buf_size, create_type, id, loc_params, 
                                                   lcpl_id, lapl_id, cur_id, cur_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode link create hard params");

                break;
            }
        case H5VL_LINK_CREATE_SOFT:
            {
                char        *target_name;

                if(H5P_get(plist, H5VL_LINK_TARGET_NAME, &target_name) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for targe name")

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_link_create_params(NULL, &buf_size, create_type, obj->obj_id, 
                                                   loc_params, lcpl_id, lapl_id, target_name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_link_create_params(send_buf, &buf_size, create_type, obj->obj_id, 
                                                   loc_params, lcpl_id, lapl_id, target_name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode link create soft params");

                break;
            }
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

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_link_create_params(NULL, &buf_size, create_type, obj->obj_id, 
                                                   loc_params, lcpl_id, lapl_id,
                                                   link_type, udata, udata_size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_link_create_params(send_buf, &buf_size, create_type, obj->obj_id, 
                                                   loc_params, lcpl_id, lapl_id,
                                                   link_type, udata, udata_size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode link create UD params");

                break;
            }
        default:
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "invalid link creation call")
    }

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_link_move
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
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_mds_link_move(void *_src_obj, H5VL_loc_params_t loc_params1, 
                   void *_dst_obj, H5VL_loc_params_t loc_params2,
                   hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t UNUSED req)
{
    H5VL_mds_object_t *src_obj = (H5VL_mds_object_t *)_src_obj;
    H5VL_mds_object_t *dst_obj = (H5VL_mds_object_t *)_dst_obj;
    void            *send_buf = NULL;
    size_t           buf_size = 0;
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_link_move_params(NULL, &buf_size, src_obj->obj_id, loc_params1, 
                                     dst_obj->obj_id, loc_params2, copy_flag, lcpl_id, lapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_link_move_params(send_buf, &buf_size, src_obj->obj_id, loc_params1, 
                                     dst_obj->obj_id, loc_params2, copy_flag, lcpl_id, lapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode link move parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata link ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_link_move() */

#endif /* H5_HAVE_PARALLEL */
