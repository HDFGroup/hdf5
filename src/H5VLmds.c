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

#if 0
/* Atrribute callbacks */
static void *H5VL_mds_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t req);
static void *H5VL_mds_attr_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t req);
static herr_t H5VL_mds_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t req);
static herr_t H5VL_mds_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t req);
static herr_t H5VL_mds_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_attr_remove(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t req);
static herr_t H5VL_mds_attr_close(void *attr, hid_t req);

/* Datatype callbacks */
static void *H5VL_mds_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
static void *H5VL_mds_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t req);
static ssize_t H5VL_mds_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t req);
static herr_t H5VL_mds_datatype_close(void *dt, hid_t req);
#endif
/* Dataset callbacks */
static void *H5VL_mds_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
static void *H5VL_mds_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t req);
static herr_t H5VL_mds_dataset_close(void *dset, hid_t req);
static herr_t H5VL_mds_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                    hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
static herr_t H5VL_mds_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                     hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
#if 0
static herr_t H5VL_mds_dataset_set_extent(void *dset, const hsize_t size[], hid_t req);
static herr_t H5VL_mds_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_dataset_close(void *dset, hid_t req);
#endif
/* File callbacks */
static void *H5VL_mds_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
static herr_t H5VL_mds_file_close(void *file, hid_t req);
#if 0
static void *H5VL_mds_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
static herr_t H5VL_mds_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t req);
static herr_t H5VL_mds_file_get(void *file, H5VL_file_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_misc(void *file, H5VL_file_misc_t misc_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_optional(void *file, H5VL_file_optional_t optional_type, hid_t req, va_list arguments);

/* Group callbacks */
static void *H5VL_mds_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
static void *H5VL_mds_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t req);
static herr_t H5VL_mds_group_get(void *obj, H5VL_group_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_group_close(void *grp, hid_t req);

/* Link callbacks */
static herr_t H5VL_mds_link_create(H5VL_link_create_type_t create_type, void *obj, 
                                      H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t req);
static herr_t H5VL_mds_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                    void *dst_obj, H5VL_loc_params_t loc_params2,
                                    hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req);
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
        NULL,//H5VL_mds_attr_create,                /* create */
        NULL,//H5VL_mds_attr_open,                  /* open */
        NULL,//H5VL_mds_attr_read,                  /* read */
        NULL,//H5VL_mds_attr_write,                 /* write */
        NULL,//H5VL_mds_attr_get,                   /* get */
        NULL,//H5VL_mds_attr_remove,                /* remove */
        NULL,//H5VL_mds_attr_close                  /* close */
    },
    {                                        /* datatype_cls */
        NULL,//H5VL_mds_datatype_commit,            /* commit */
        NULL,//H5VL_mds_datatype_open,              /* open */
        NULL,//H5VL_mds_datatype_get_binary,        /* get_size */
        NULL,//H5VL_mds_datatype_close              /* close */
    },
    {                                        /* dataset_cls */
        H5VL_mds_dataset_create,             /* create */
        H5VL_mds_dataset_open,               /* open */
        H5VL_mds_dataset_read,               /* read */
        H5VL_mds_dataset_write,              /* write */
        NULL,//H5VL_mds_dataset_set_extent,         /* set extent */
        NULL,//H5VL_mds_dataset_get,                /* get */
        H5VL_mds_dataset_close               /* close */
    },
    {                                        /* file_cls */
        H5VL_mds_file_create,                /* create */
        NULL,//H5VL_mds_file_open,                  /* open */
        NULL,//H5VL_mds_file_flush,                 /* flush */
        NULL,//H5VL_mds_file_get,                   /* get */
        NULL,//H5VL_mds_file_misc,                  /* misc */
        NULL,//H5VL_mds_file_optional,              /* optional */
        H5VL_mds_file_close                  /* close */
    },
    {                                        /* group_cls */
        NULL,//H5VL_mds_group_create,               /* create */
        NULL,//H5VL_mds_group_open,                 /* open */
        NULL,//H5VL_mds_group_get,                  /* get */
        NULL,//H5VL_mds_group_close                 /* close */
    },
    {                                        /* link_cls */
        NULL,//H5VL_mds_link_create,                /* create */
        NULL,//H5VL_mds_link_move,                  /* move */
        NULL,//H5VL_mds_link_iterate,               /* iterate */
        NULL,//H5VL_mds_link_get,                   /* get */
        NULL,//H5VL_mds_link_remove                 /* remove */
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
        if(H5VL_mds_encode(H5VL_MDS_FILE_CREATE, NULL, &buf_size, name, flags, fcpl_id, fapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed")

        /* allocate the buffer for encoding the parameters */
        if(NULL == (send_buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* encode the parameters */
        if(H5VL_mds_encode(H5VL_MDS_FILE_CREATE, send_buf, &buf_size, name, flags, fcpl_id, fapl_id) < 0)
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

    /* We do not have a raw data cache at the client side, so there is nothing to flush here */

    /* close the file */
    if((ret_value = H5F_close(f)) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close file");

    H5MM_free(file);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_close() */


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
    if(H5VL_mds_encode(H5VL_MDS_DSET_CREATE, NULL, &buf_size, obj->obj_id, loc_params, name, 
                       dcpl_id, dapl_id, type_id, space_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL_mds_encode(H5VL_MDS_DSET_CREATE, send_buf, &buf_size, obj->obj_id, loc_params, name, 
                       dcpl_id, dapl_id, type_id, space_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode dataset create parameters");

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
    uint8_t        *p = NULL; /* pointer into recv_buf; used for decoding */
    hid_t          type_id, space_id, dcpl_id;
    H5O_layout_t   layout;        /* Dataset's layout information */
    size_t         type_size, space_size, dcpl_size, layout_size;
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL_mds_encode(H5VL_MDS_DSET_OPEN, NULL, &buf_size, obj->obj_id, loc_params, 
                       name, dapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL_mds_encode(H5VL_MDS_DSET_OPEN, send_buf, &buf_size, obj->obj_id, loc_params, 
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
    recv_buf = (void *)H5MM_malloc (incoming_msg_size);

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
    else {
        dcpl_id = H5P_DATASET_CREATE_DEFAULT;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    /* decode the datatype */
    if((type_id = H5Tdecode(p)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "unable to decode datatype");
    p += type_size;

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    /* decode the dataspace */
    if((space_id = H5Sdecode((const void *)p)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, NULL, "unable to decode dataspace");
    p += space_size;

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
    uint8_t         *p = NULL;
    size_t           buf_size;
    herr_t           ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    buf_size = 1 /* request type */ + sizeof(int) /* dset id */;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    p = (uint8_t *)send_buf;    /* Temporary pointer to encoding buffer */

    /* encode request type */
    *p++ = (uint8_t)H5VL_MDS_DSET_CLOSE;

    /* encode the object id */
    INT32ENCODE(p, dset->common.obj_id);

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata file ID */
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

#endif /* H5_HAVE_PARALLEL */

#if 0
    /* get the datatype the dataset was created with */
    if(NULL == (dt = (H5T_t *)H5I_object(type_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype");
    /* copy and store the datatype in the dataset object */
    if(NULL == (dset->type = H5T_copy(dt, H5T_COPY_TRANSIENT)))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to copy datatype");

    /* get the dataspace the dataset was created with */
    if(NULL == (ds = (H5S_t *)H5I_object(space_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataspace");
    /* copy and store the dataspace in the dataset object */
    if(NULL == (dset->space = H5S_copy(ds, FALSE, TRUE)))
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, NULL, "unable to copy dataspace");

#endif

#if 0
    /* Allocate data space and initialize it if it hasn't been. */
    if(nelmts > 0 && dataset->shared->dcpl_cache.efl.nused == 0 &&
       !(*dataset->shared->layout.ops->is_space_alloc)(&dataset->shared->layout.storage)) {
        hssize_t file_nelmts;   /* Number of elements in file dataset's dataspace */
        hbool_t full_overwrite; /* Whether we are over-writing all the elements */
        size_t dxpl_size = 0;
        size_t buf_size = 0; /* size of send_buf */
        int incoming_msg_size; /* incoming buffer size for MDS returned dataset */
        void *recv_buf = NULL; /* buffer to hold incoming data from MDS */
        MPI_Status status;
        H5P_genplist_t *dxpl;
        void *send_buf = NULL;
        H5O_layout_t layout;
        uint8_t *p = NULL; /* temporary pointer into send_buf for encoding */

        /* Get the number of elements in file dataset's dataspace */
        if((file_nelmts = H5S_GET_EXTENT_NPOINTS(file_space)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "can't retrieve number of elements in file dataset")

        /* Always allow fill values to be written if the dataset has a VL datatype */
        if(H5T_detect_class(dataset->shared->type, H5T_VLEN, FALSE))
            full_overwrite = FALSE;
        else
            full_overwrite = (hbool_t)((hsize_t)file_nelmts == nelmts ? TRUE : FALSE);

        if(H5P_DATASET_XFER_DEFAULT != dxpl_id)
            if((ret_value = H5P__encode(dxpl, FALSE, NULL, &dxpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");

        buf_size = 2 + sizeof(unsigned) + sizeof(int) + 
            H5V_limit_enc_size((uint64_t)dxpl_size) + dxpl_size;

        /* allocate the buffer for encoding the parameters */
        if(NULL == (send_buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        p = (uint8_t *)send_buf;

        /* encode request type */
        *p++ = (uint8_t)H5VL_MDS_DSET_WRITE;

        H5_ENCODE_UNSIGNED(p, full_overwrite);
        //*p++ = (uint8_t)full_overwrite;

        *p++ = (uint8_t)H5D_ALLOC_WRITE;

        /* encode the object id */
        INT32ENCODE(p, dset->common.obj_id);

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, dxpl_size);
        /* encode property lists if they are not default*/
        if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
            if(H5P__encode(dxpl, FALSE, p, &dxpl_size) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += dxpl_size;
        }

        MPI_Pcontrol(0);
        /* send the request to the MDS process */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                                   MPI_COMM_WORLD))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

        /* probe for a message from the mds */
        if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
        /* get the incoming message size from the probe result */
        if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

        /* allocate the receive buffer */
        recv_buf = (void *)H5MM_malloc (incoming_msg_size);

        /* receive the actual message */
        if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                    H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
        MPI_Pcontrol(1);

        p = (uint8_t *)recv_buf;

        /* decode the dataset layout */
        if(FAIL == H5D__decode_layout(p, &layout))
            HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "failed to decode dataset layout");

        /* set the layout of the dataset */
        dataset->shared->layout = layout;

        H5MM_free(send_buf);
        H5MM_free(recv_buf);

 	/* Allocate storage */
        if(H5D__alloc_storage(dataset, dxpl_id, H5D_ALLOC_WRITE, full_overwrite, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage")
    } /* end if */

                hid_t dset_id = FAIL; /* dset id */
                hid_t dxpl_id;
                size_t dxpl_size, buf_size;
                void *send_buf = NULL;
                H5D_t *dataset = NULL;
                hbool_t full_overwrite;
                H5D_time_alloc_t alloc_time;
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                //full_overwrite = (hbool_t)*p++;
                H5_DECODE_UNSIGNED(p, full_overwrite);
                alloc_time = (H5D_time_alloc_t)*p++;

                /* decode the object id */
                INT32DECODE(p, dset_id);
                printf("MDS allocating dataset %d\n", dset_id);
                /* decode the plist size */
                UINT64DECODE_VARLEN(p, dxpl_size);
                /* decode property lists if they are not default*/
                if(dxpl_size) {
                    if((dxpl_id = H5P__decode(p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                    p += dxpl_size;
                }
                else {
                    dxpl_id = H5P_DATASET_XFER_DEFAULT;
                }

                if(NULL == (dataset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dset ID");

                /* Allocate storage */
                if(H5D__alloc_storage(dataset, dxpl_id, alloc_time, full_overwrite, NULL) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage");

                /* determine the buffer size needed to store the encoded layout of the dataset */ 
                if(FAIL == H5D__encode_layout(dataset->shared->layout, NULL, &buf_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode layout of the dataset */ 
                if(FAIL == H5D__encode_layout(dataset->shared->layout, p1, &buf_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");

                /* Send the dataset id to the client */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_free(send_buf);
                break;

#endif

