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
#include "H5Fprivate.h"		/* File access				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5FDmpi.h"            /* MPI-based file drivers		*/
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
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLmds.h"            /* MDS VOL plugin			*/
#include "H5VLmdserver.h"       /* MDS helper stuff			*/

/* Prototypes */
static void *H5VL_mds_fapl_copy(const void *_old_fa);
static herr_t H5VL_mds_fapl_free(void *_fa);

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

/* Dataset callbacks */
static void *H5VL_mds_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
static void *H5VL_mds_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t req);
static herr_t H5VL_mds_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                       hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
static herr_t H5VL_mds_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                        hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
static herr_t H5VL_mds_dataset_set_extent(void *dset, const hsize_t size[], hid_t req);
static herr_t H5VL_mds_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_dataset_close(void *dset, hid_t req);

/* File callbacks */
static void *H5VL_mds_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
static void *H5VL_mds_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
static herr_t H5VL_mds_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t req);
static herr_t H5VL_mds_file_get(void *file, H5VL_file_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_misc(void *file, H5VL_file_misc_t misc_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_optional(void *file, H5VL_file_optional_t optional_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_close(void *file, hid_t req);

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
        H5VL_mds_attr_get,                   /* get */
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
        H5VL_mds_dataset_get,                /* get */
        H5VL_mds_dataset_close               /* close */
    },
    {                                        /* file_cls */
        H5VL_mds_file_create,                /* create */
        H5VL_mds_file_open,                  /* open */
        H5VL_mds_file_flush,                 /* flush */
        H5VL_mds_file_get,                   /* get */
        H5VL_mds_file_misc,                  /* misc */
        H5VL_mds_file_optional,              /* optional */
        H5VL_mds_file_close                  /* close */
    },
    {                                        /* group_cls */
        H5VL_mds_group_create,               /* create */
        H5VL_mds_group_open,                 /* open */
        H5VL_mds_group_get,                  /* get */
        H5VL_mds_group_close                 /* close */
    },
    {                                        /* link_cls */
        H5VL_mds_link_create,                /* create */
        H5VL_mds_link_move,                  /* move */
        H5VL_mds_link_iterate,               /* iterate */
        H5VL_mds_link_get,                   /* get */
        H5VL_mds_link_remove                 /* remove */
    },
    {                                        /* object_cls */
        H5VL_mds_object_open,                /* open */
        H5VL_mds_object_copy,                /* copy */
        H5VL_mds_object_visit,               /* visit */
        H5VL_mds_object_get,                 /* get */
        H5VL_mds_object_misc,                /* misc */
        H5VL_mds_object_optional,            /* optional */
        H5VL_mds_object_close                /* close */
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
H5Pset_fapl_mds(hid_t fapl_id, const char *meta_name, hid_t meta_fapl_id,
                const char *raw_name, hid_t raw_fapl_id, MPI_Comm comm, MPI_Info info)
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
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, 
                     hid_t UNUSED req)
{
    void *send_buf;
    size_t buf_size;
    H5VL_mds_fapl_t *fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    int my_rank;
    MPI_Request mpi_req;
    MPI_Status mpi_stat;
    H5F_t *new_file = NULL;
    H5VL_mds_object_t *file = NULL;
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
    if(NULL == (fa = H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get MDS info struct")
    MPI_Comm_rank(fa->comm, &my_rank);

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
        /* send the message */
        if(MPI_SUCCESS != MPI_Isend(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                    H5VL_MDS_LISTEN_TAG, MPI_COMM_WORLD, &mpi_req))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message")
        MPI_Pcontrol(1);
    }

    /* set the underlying VFD for clients (MDC)*/
    temp_fapl = H5Pcreate(H5P_FILE_ACCESS);
    if(H5P_fapl_set_mdc(temp_fapl, name, fapl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, NULL, "failed to set MDC plist")

    /* Create the raw data file */ 
    if(NULL == (new_file = H5F_open(name, flags, fcpl_id, temp_fapl, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file")

    new_file->id_exists = TRUE;

    /* allocate the file object that is returned to the user */
    if(NULL == (file = (H5VL_mds_file_t *)calloc(1, sizeof(H5VL_mds_file_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* the first process recieves the metadata file ID from the MDS process and 
       bcasts it to the other processes in the communicator that opened the file */
    if (0 == my_rank) {
        MPI_Pcontrol(0);
        if(MPI_SUCCESS != MPI_Recv(&mds_file, sizeof(hid_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG, 
                                   MPI_COMM_WORLD, MPI_STATUS_NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message")
        MPI_Pcontrol(1);
    }

    if(MPI_SUCCESS != MPI_Bcast(&mds_file, sizeof(hid_t), MPI_BYTE, 0, fa->comm))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message")

    HDassert(mds_file);

    (H5FD_mdc_t *)(new_file->shared->lf)->mdfile_id = mds_file;
    file->common.obj_type = H5I_FILE; 
    file->common.obj_id = mds_file;
    file->common.raw_file = new_file;

    if (0 == my_rank) {
        MPI_Wait(&request, &mpi_stat);
        H5MM_free(send_buf);
    }

    ret_value = (void *)file;
done:
    if(NULL == ret_value && new_file) 
        if(H5F_close(new_file) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_create
 *
 * Purpose:	Sends a request to the MDS to create a dataset
 *
 * Return:	Success:	dataset id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_mds_dataset_create(void *_obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, 
                        hid_t dapl_id, hid_t UNUSED req)
{
    void           *send_buf = NULL;
    size_t         buf_size;
    H5P_genplist_t *plist;      /* Property list pointer */
    MPI_Request    mpi_req;
    MPI_Status     mpi_stat;
    hid_t          type_id, space_id, lcpl_id;
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj;
    void           *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
    if(H5P_get(plist, H5VL_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")
    if(H5P_get(plist, H5VL_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id")

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL_mds_encode(H5VL_MDS_DSET_CREATE, NULL, &buf_size, obj->obj_id, loc_params, name, 
                       dcpl_id, dapl_id, type_id, space_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed")

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* encode the parameters */
    if(H5VL_mds_encode(H5VL_MDS_DSET_CREATE, send_buf, &buf_size, obj->obj_id, loc_params, name, 
                       dcpl_id, dapl_id, type_id, space_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode dataset create parameters")

    MPI_Pcontrol(0);
    /* send the message */
    if(MPI_SUCCESS != MPI_Isend(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                H5VL_MDS_LISTEN_TAG, MPI_COMM_WORLD, &mpi_req))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message")

    if(NULL == (dset = (H5VL_mds_object_t *)calloc(1, sizeof(H5VL_mds_object_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    dset->obj_type = H5I_DATASET;

    /* Recieve the Dataset ID from the MDS process */
    if(MPI_SUCCESS != MPI_Recv(&(dset->obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5VL_MDS_SEND_TAG, 
                               MPI_COMM_WORLD, MPI_STATUS_NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message")

    MPI_Wait(&request, &mpi_stat);
    MPI_Pcontrol(1);

    H5MM_free(send_buf);
    ret_value = (void *)dset_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_write
 *
 * Purpose:	Writes raw data from a buffer into a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not writed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_mds_dataset_write(void *_obj, hid_t mem_type_id, hid_t mem_space_id,
                       hid_t file_space_id, hid_t dxpl_id, const void *buf, hid_t UNUSED req)
{
    H5VL_mds_object_t *obj = (H5VL_mds_object_t *)_obj;
    hid_t          dset_id = obj->obj_id;
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    herr_t         ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */

    if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

    /* write raw data */
    if(H5D__write(dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_write() */
