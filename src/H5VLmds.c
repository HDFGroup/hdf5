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
#include "H5MDprivate.h"        /* MDS helper stuff			*/
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
static herr_t H5VL_mds_file_get(void *file, H5VL_file_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_misc(void *file, H5VL_file_misc_t misc_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_optional(void *file, H5VL_file_optional_t optional_type, hid_t req, va_list arguments);
static herr_t H5VL_mds_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t req);
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

/* MDS-specific file access properties */
typedef struct H5VL_mds_fapl_t {
    MPI_Comm		comm;		/*communicator			*/
    MPI_Info		info;		/*file information		*/
    char                *raw_ext;
    hid_t               raw_fapl;
    char                *meta_ext;
    hid_t               meta_fapl;
} H5VL_mds_fapl_t;

/* Declare a free list to manage the MDS object structs */
H5FL_DEFINE(H5MD_file_t);
H5FL_DEFINE(H5MD_attr_t);
H5FL_DEFINE(H5MD_dset_t);
H5FL_DEFINE(H5MD_dtype_t);
H5FL_DEFINE(H5MD_group_t);

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
    void                  *obj = NULL; /* fapl object for copying */
    const H5VL_mds_fapl_t *old_fa = (const H5VL_mds_fapl_t*)_old_fa;
    H5VL_mds_fapl_t	  *new_fa = NULL;
    void		  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (new_fa = (H5VL_mds_fapl_t *)H5MM_malloc(sizeof(H5VL_mds_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Copy the general information */
    HDmemcpy(new_fa, old_fa, sizeof(H5VL_mds_fapl_t));

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed");

    /* duplicate the raw and meta file extensions */
    new_fa->raw_ext = HDstrdup(old_fa->raw_ext);
    new_fa->meta_ext = HDstrdup(old_fa->meta_ext);

    /* copy the fapl for raw data */
    if(NULL == (obj = (H5P_genplist_t *)H5I_object_verify(old_fa->raw_fapl, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list");
    if((new_fa->raw_fapl = H5P_copy_plist((H5P_genplist_t *)obj, TRUE)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, NULL, "can't copy property list");

    /* copy the fapl for meta data */
    if(NULL == (obj = (H5P_genplist_t *)H5I_object_verify(old_fa->meta_fapl, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list");
    if((new_fa->meta_fapl = H5P_copy_plist((H5P_genplist_t *)obj, TRUE)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, NULL, "can't copy property list");

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

    FUNC_ENTER_NOAPI_NOINIT

    assert(fa);

    /* Free the internal communicator and INFO object */
    assert(MPI_COMM_NULL!=fa->comm);
    H5FD_mpi_comm_info_free(&fa->comm, &fa->info);

    /* Close the property lists */
    if(H5I_dec_app_ref(fa->raw_fapl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");
    if(H5I_dec_app_ref(fa->meta_fapl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");

    /* free the extensions */
    H5MM_xfree(fa->raw_ext);
    H5MM_xfree(fa->meta_ext);

    /* free the struct */
    H5MM_xfree(fa);

done:
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
H5Pset_fapl_mds(hid_t fapl_id, char *raw_ext, hid_t raw_fapl, char *meta_ext, hid_t meta_fapl,
                MPI_Comm comm, MPI_Info info)
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
    if(H5P_DEFAULT == raw_fapl)
        raw_fapl = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(raw_fapl, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")
    if(H5P_DEFAULT == meta_fapl)
        meta_fapl = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(meta_fapl, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.info = info;
    fa.raw_ext = raw_ext;
    fa.meta_ext = meta_ext;
    fa.raw_fapl = raw_fapl;
    fa.meta_fapl = meta_fapl;

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
    H5VL_mds_fapl_t *fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    int my_rank, my_size;
    H5F_t *new_file = NULL;
    hid_t mds_file; /* Metadata file ID recieved from the MDS */
    H5MD_file_t *file = NULL;
    char *raw_name = NULL;
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
        char *meta_name = NULL;

        /* generate the meta data file name by adding the meta data extension to the user file name */
        if(NULL == (meta_name = (char *)H5MM_malloc (HDstrlen(name) + HDstrlen(fa->meta_ext) + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
        sprintf(meta_name, "%s%s", name, fa->meta_ext);

        /* determine the size of the buffer needed to encode the parameters */
        if(H5VL__encode_file_create_params(NULL, &buf_size, meta_name, flags, fcpl_id, fa->meta_fapl) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed")

        /* allocate the buffer for encoding the parameters */
        if(NULL == (send_buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* encode the parameters */
        if(H5VL__encode_file_create_params(send_buf, &buf_size, meta_name, flags, fcpl_id, fa->meta_fapl) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode file create parameters")

        MPI_Pcontrol(0);
        /* send the request to the MDS process and recieve the metadata file ID */
        if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                       &mds_file, sizeof(hid_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                       MPI_COMM_WORLD, MPI_STATUS_IGNORE))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message")
        MPI_Pcontrol(1);

        H5MM_xfree(send_buf);
        H5MM_xfree(meta_name);
    }

    /* Process 0 Bcasts the metadata file ID to other processes if there are any */
    if(my_size > 1) {
        if(MPI_SUCCESS != MPI_Bcast(&mds_file, sizeof(hid_t), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message")
    }
    HDassert(mds_file);

    /* generate the raw data file name by adding the raw data extension to the user file name */
    if(NULL == (raw_name = (char *)H5MM_malloc (HDstrlen(name) + HDstrlen(fa->raw_ext) + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    sprintf(raw_name, "%s%s", name, fa->raw_ext);

    /* set the file VFD as the client forwarding VFD using the user's raw data fapl 
       as the underlying fapl */
    if(H5P_set_fapl_mdc(fapl_id, raw_name, fa->raw_fapl, mds_file) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, NULL, "failed to set MDC plist")

    /* Create the raw data file */ 
    if(NULL == (new_file = H5MD_file_open(raw_name, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file")

    /* set the file space manager to use the VFD */
    new_file->shared->fs_strategy = H5F_FILE_SPACE_VFD;
    new_file->id_exists = TRUE;

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5MD_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate MDS file struct");

    /* create the file object that is passed to the API layer */
    file->name = HDstrdup(name);
    file->common.obj_type = H5I_FILE; 
    file->common.obj_id = mds_file;
    file->common.raw_file = new_file;
    file->common.file = file;

    ret_value = (void *)file;

done:
    H5MM_xfree(raw_name);
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
    H5VL_mds_fapl_t *fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    int my_rank, my_size;
    H5F_t *new_file = NULL;
    hid_t mds_file; /* Metadata file ID recieved from the MDS */
    H5MD_file_t *file = NULL;
    char *raw_name = NULL;
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
        char *meta_name = NULL;

        /* generate the meta data file name by adding the meta data extension to the user file name */
        if(NULL == (meta_name = (char *)H5MM_malloc (HDstrlen(name) + HDstrlen(fa->meta_ext) + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
        sprintf(meta_name, "%s%s", name, fa->meta_ext);

        /* determine the size of the buffer needed to encode the parameters */
        if(H5VL__encode_file_open_params(NULL, &buf_size, meta_name, flags, fapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed")

        /* allocate the buffer for encoding the parameters */
        if(NULL == (send_buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* encode the parameters */
        if(H5VL__encode_file_open_params(send_buf, &buf_size, meta_name, flags, fapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode file open parameters")

        MPI_Pcontrol(0);
        /* send the request to the MDS process and recieve the metadata file ID */
        if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                       &mds_file, sizeof(hid_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                       MPI_COMM_WORLD, MPI_STATUS_IGNORE))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message")
        MPI_Pcontrol(1);

        H5MM_xfree(send_buf);
        H5MM_xfree(meta_name);
    }

    /* Process 0 Bcasts the metadata file ID to other processes if there are any */
    if(my_size > 1) {
        if(MPI_SUCCESS != MPI_Bcast(&mds_file, sizeof(hid_t), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message")
    }
    HDassert(mds_file);

    /* generate the raw data file name by adding the raw data extension to the user file name */
    if(NULL == (raw_name = (char *)H5MM_malloc (HDstrlen(name) + HDstrlen(fa->raw_ext) + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    sprintf(raw_name, "%s%s", name, fa->raw_ext);

    /* set the file VFD as the client forwarding VFD using the user's raw data fapl 
       as the underlying fapl */
    if(H5P_set_fapl_mdc(fapl_id, raw_name, fa->raw_fapl, mds_file) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, NULL, "failed to set MDC plist")

    /* Open the raw data file */ 
    if(NULL == (new_file = H5MD_file_open(raw_name, flags, H5P_FILE_CREATE_DEFAULT, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")

    /* set the file space manager to use the VFD */
    new_file->shared->fs_strategy = H5F_FILE_SPACE_VFD;
    new_file->id_exists = TRUE;

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5MD_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate MDS file struct");

    /* open the file object that is passed to the API layer */
    file->name = HDstrdup(name);
    file->common.obj_type = H5I_FILE; 
    file->common.obj_id = mds_file;
    file->common.raw_file = new_file;
    file->common.file = file;

    ret_value = (void *)file;

done:
    H5MM_xfree(raw_name);
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
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
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

    H5MM_xfree(send_buf);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_file_get
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
H5VL_mds_file_get(void *_obj, H5VL_file_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    H5F_t *f = obj->raw_file;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Fget_access_plist */
        case H5VL_FILE_GET_FAPL:
            {
                H5P_genplist_t *new_plist;              /* New property list */
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5F_get_access_plist(f, TRUE)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")

                if(NULL == (new_plist = (H5P_genplist_t *)H5I_object(*plist_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

                /* Set the VOL class in the property list - we don't
                   have a VOL info for the mds plugin */
                if(H5P_set(new_plist, H5F_ACS_VOL_NAME, &H5VL_mds_g) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set file VOL plugin")
                break;
            }
        /* H5Fget_create_plist */
        case H5VL_FILE_GET_FCPL:
            {
                H5P_genplist_t *plist;      /* Property list */
                hid_t *plist_id = va_arg (arguments, hid_t *);

                if(NULL == (plist = (H5P_genplist_t *)H5I_object(f->shared->fcpl_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

                /* Create the property list object to return */
                if((*plist_id = H5P_copy_plist(plist, TRUE)) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to copy file creation properties")

                break;
            }
        /* H5Fget_obj_count */
        case H5VL_FILE_GET_OBJ_COUNT:
            {
                unsigned types = va_arg (arguments, unsigned);
                ssize_t *ret = va_arg (arguments, ssize_t *);
                size_t  obj_count = 0;      /* Number of opened objects */

                /* Perform the query */
                if(H5MD_file_get_obj_count(f, types, TRUE, &obj_count) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_obj_count failed")

                /* Set the return value */
                *ret = (ssize_t)obj_count;
                break;
            }
        /* H5Fget_obj_ids */
        case H5VL_FILE_GET_OBJ_IDS:
            {
                unsigned types = va_arg (arguments, unsigned);
                size_t max_objs = va_arg (arguments, size_t);
                hid_t *oid_list = va_arg (arguments, hid_t *);
                ssize_t *ret = va_arg (arguments, ssize_t *);
                size_t  obj_count = 0;      /* Number of opened objects */

                /* Perform the query */
                if(H5MD_file_get_obj_ids(f, types, max_objs, oid_list, TRUE, &obj_count) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_obj_ids failed")

                /* Set the return value */
                *ret = (ssize_t)obj_count;
                break;
            }
        /* H5Fget_intent */
        case H5VL_FILE_GET_INTENT:
            {
                unsigned *ret = va_arg (arguments, unsigned *);

                /* HDF5 uses some flags internally that users don't know about.
                 * Simplify things for them so that they only get either H5F_ACC_RDWR
                 * or H5F_ACC_RDONLY.
                 */
                if(H5F_INTENT(f) & H5F_ACC_RDWR)
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

                len = HDstrlen(obj->file->name);

                if(name) {
                    HDstrncpy(name, obj->file->name, MIN(len + 1,size));
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
                H5MD_file_t *file = obj->file;

                f->id_exists = TRUE;
                *ret = (void*)file;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information")
    } /* end switch */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_file_misc
 *
 * Purpose:	Perform an operation
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
H5VL_mds_file_misc(void *_obj, H5VL_file_misc_t misc_type, hid_t UNUSED req, va_list arguments)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    void *send_buf = NULL;
    size_t buf_size;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (misc_type) {
        /* H5Fmount */
        case H5VL_FILE_MOUNT:
            {
                H5I_type_t type        = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
                H5MD_file_t *child = va_arg (arguments, H5MD_file_t *);
                hid_t plist_id         = va_arg (arguments, hid_t);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_file_misc_params(NULL, &buf_size, misc_type, obj->obj_id, type, name,
                                                child->common.obj_id, plist_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_file_misc_params(send_buf, &buf_size, misc_type, obj->obj_id, type, name,
                                                child->common.obj_id, plist_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode file get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Fmount */
        case H5VL_FILE_UNMOUNT:
            {
                H5I_type_t  type       = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_file_misc_params(NULL, &buf_size, misc_type, obj->obj_id, type, name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_file_misc_params(send_buf, &buf_size, misc_type, obj->obj_id, type, name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode file get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Fis_accessible */
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                hid_t fapl_id       = va_arg (arguments, hid_t);
                const char *name    = va_arg (arguments, const char *);
                htri_t     *ret     = va_arg (arguments, htri_t *);
                H5MD_file_t *file = NULL;

                /* attempt to open the file through the MDS plugin */
                if(NULL == (file = (H5MD_file_t *)H5VL_mds_file_open(name, H5F_ACC_RDONLY, fapl_id,
                                                                         H5_REQUEST_NULL)))
                    *ret = FALSE;
                else
                    *ret = TRUE;

                /* close the file if it was succesfully opened */
                if(file && H5VL_mds_file_close((void*)file, H5_REQUEST_NULL) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close file");
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "MDS Plugin does not support this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_file_optional
 *
 * Purpose:	Perform a plugin specific operation on a mds file
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
H5VL_mds_file_optional(void *_obj, H5VL_file_optional_t optional_type, hid_t UNUSED req, va_list arguments)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    size_t buf_size = 0;
    void *send_buf = NULL;
    int incoming_msg_size; /* incoming buffer size for MDS returned attr */
    void *recv_buf = NULL; /* buffer to hold incoming data from MDS */
    MPI_Status status;
    uint8_t *p = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (optional_type) {
        /* H5Fget_filesize */
        case H5VL_FILE_GET_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);
                H5F_t *f = obj->raw_file;
                haddr_t eof;                     /* End of file address */

                /* Go get the actual file size */
                if(HADDR_UNDEF == (eof = H5FDget_eof(f->shared->lf)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get file size")
                *ret = (hsize_t)eof;
                break;
            }
        /* H5Fget_freespace */
        case H5VL_FILE_GET_FREE_SPACE:
            {
                hssize_t    *ret = va_arg (arguments, hssize_t *);

                buf_size = 2 + sizeof(int32_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, obj->obj_id);

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               ret, sizeof(int64_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        case H5VL_FILE_GET_FREE_SECTIONS:
            {
                H5F_sect_info_t *sect_info = va_arg (arguments, H5F_sect_info_t *);
                ssize_t         *ret       = va_arg (arguments, ssize_t *);
                H5F_mem_t       type       = va_arg (arguments, H5F_mem_t);
                size_t          nsects     = va_arg (arguments, size_t);
                uint8_t flag; /* flag to indicate whether sect_info is NULL or not */

                buf_size += 4 + sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)nsects);

                if(NULL == sect_info)
                    flag = 0;
                else
                    flag = 1;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, obj->obj_id);
                *p++ = (uint8_t)type;
                UINT64ENCODE_VARLEN(p, nsects);
                *p++ = (uint8_t)flag;

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                           MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv(recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                /* decode the return value */
                INT64DECODE(p, *ret);
                if(1 == flag) {
                    unsigned u;
                    for(u=0 ; u<nsects ; u++) {
                        UINT64DECODE_VARLEN(p, sect_info[u].addr);
                        UINT64DECODE_VARLEN(p, sect_info[u].size);
                    }
                }
                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Fget_info2 */
        case H5VL_FILE_GET_INFO:
            {
                H5I_type_t  type   = va_arg (arguments, H5I_type_t);
                H5F_info2_t *finfo = va_arg (arguments, H5F_info2_t *);

                buf_size = 3 + sizeof(int32_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, obj->obj_id);
                *p++ = (uint8_t)type;

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                           MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv(recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                H5_DECODE_UNSIGNED(p, finfo->super.version);
                UINT64DECODE_VARLEN(p, finfo->super.super_size);
                UINT64DECODE_VARLEN(p, finfo->super.super_ext_size);
                H5_DECODE_UNSIGNED(p, finfo->free.version);
                UINT64DECODE_VARLEN(p, finfo->free.meta_size);
                UINT64DECODE_VARLEN(p, finfo->free.tot_space);
                H5_DECODE_UNSIGNED(p, finfo->sohm.version);
                UINT64DECODE_VARLEN(p, finfo->sohm.hdr_size);
                UINT64DECODE_VARLEN(p, finfo->sohm.msgs_info.index_size);
                UINT64DECODE_VARLEN(p, finfo->sohm.msgs_info.heap_size);

                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Fget_mdc_config */
        case H5VL_FILE_GET_MDC_CONF:
            {
                H5AC_cache_config_t *config_ptr = va_arg (arguments, H5AC_cache_config_t *);

                buf_size = 2 + sizeof(int32_t)*2;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, config_ptr->version);
                INT32ENCODE(p, obj->obj_id);

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                           MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv(recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                if(H5P__facc_cache_config_dec((const void **)(&p), config_ptr) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to decode cache config");

                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Fget_mdc_hit_rate */
        case H5VL_FILE_GET_MDC_HR:
            {
                double *hit_rate_ptr = va_arg (arguments, double *);

                buf_size = 2 + sizeof(int32_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, obj->obj_id);

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               hit_rate_ptr, sizeof(double), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Fget_mdc_size */
        case H5VL_FILE_GET_MDC_SIZE:
            {
                size_t *max_size_ptr        = va_arg (arguments, size_t *);
                size_t *min_clean_size_ptr  = va_arg (arguments, size_t *);
                size_t *cur_size_ptr        = va_arg (arguments, size_t *); 
                int    *cur_num_entries_ptr = va_arg (arguments, int *); 

                buf_size = 2 + sizeof(int32_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, obj->obj_id)

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                           MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv(recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                UINT64DECODE_VARLEN(p, *max_size_ptr);
                UINT64DECODE_VARLEN(p, *min_clean_size_ptr);
                UINT64DECODE_VARLEN(p, *cur_size_ptr);
                INT32DECODE(p, *cur_num_entries_ptr);

                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Fclear_elink_file_cache */
        case H5VL_FILE_CLEAR_ELINK_CACHE:
            {
                buf_size = 2 + sizeof(int32_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, obj->obj_id);

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Freopen */
        case H5VL_FILE_REOPEN:
            {
                void  **ret = va_arg (arguments, void **);
                hid_t mds_file;
                H5MD_file_t *old_file = (H5MD_file_t *)obj;
                H5MD_file_t *file = NULL;
                H5F_t *new_file = NULL;

                buf_size = 2 + sizeof(int32_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, old_file->common.obj_id);

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the metadata file ID */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &mds_file, sizeof(hid_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                MPI_Pcontrol(1);

                /* Open the raw data file */ 
                if(NULL == (new_file = H5F_reopen(old_file->common.raw_file)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to open file");

                /* set the file space manager to use the VFD */
                new_file->shared->fs_strategy = H5F_FILE_SPACE_VFD;
                new_file->id_exists = TRUE;

                /* allocate the file object that is returned to the user */
                if(NULL == (file = H5FL_CALLOC(H5MD_file_t)))
                    HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate MDS file struct");

                /* open the file object that is passed to the API layer */
                file->common.obj_type = H5I_FILE; 
                file->common.obj_id = mds_file;
                file->common.raw_file = new_file;
                file->common.file = file;

                *ret = (void *)file;

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Freset_mdc_hit_rate_stats */
        case H5VL_FILE_RESET_MDC_HIT_RATE:
            {
                buf_size = 2 + sizeof(int32_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, obj->obj_id);

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        case H5VL_FILE_SET_MDC_CONFIG:
            {
                size_t temp_size = 0;
                H5AC_cache_config_t *config_ptr = va_arg (arguments, H5AC_cache_config_t *);

                if(H5P__facc_cache_config_enc(config_ptr, (void **)(&p), &temp_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode cache config");

                buf_size = 2 + sizeof(int32_t) + temp_size;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p = (uint8_t *)send_buf;

                *p++ = (uint8_t)H5VL_FILE_OPTIONAL;
                *p++ = (uint8_t)optional_type;
                INT32ENCODE(p, obj->obj_id);

                if(H5P__facc_cache_config_enc(config_ptr, (void **)(&p), &temp_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode cache config");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        case H5VL_FILE_GET_VFD_HANDLE:
        case H5VL_FILE_GET_FILE_IMAGE:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Function not supported or not recognized by the MDS plugin")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_file_optional() */


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
    H5MD_file_t *file = (H5MD_file_t *)obj;
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

    HDfree(file->name);
    file = H5FL_FREE(H5MD_file_t, file);

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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to create the attr */
    H5MD_attr_t *attr = NULL; /* the attr object that is created and passed to the user */
    H5A_t          *new_attr = NULL; /* the lighweight attr struct used to hold the attr's metadata */
    void           *send_buf = NULL; /* buffer where the attr create request is encoded and sent to the mds */
    size_t         buf_size = 0; /* size of send_buf */
    H5P_genplist_t *plist;
    H5T_t          *dt = NULL, *dt1 = NULL;
    H5MD_dtype_t *type = NULL;
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
    if(NULL == (attr = H5FL_CALLOC(H5MD_attr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata attribute ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &(attr->common.obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a type");
    /* Get the actual datatype object if this is a named datatype */
    if(NULL == (type = (H5MD_dtype_t *)H5T_get_named_type(dt)))
        dt1 = dt;
    else
        dt1 = type->dtype;

    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a data space");

    /* create the "lightweight" client attr */
    if(NULL == (new_attr = H5MD_attr_create(name, dt1, space, acpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "failed to create client attr object");

    new_attr->oloc.file = obj->raw_file;
    /* set the attr struct in the high level object */
    attr->attr = new_attr;

    /* set common object parameters */
    attr->common.obj_type = H5I_ATTR;
    attr->common.raw_file = obj->raw_file;
    attr->common.file = obj->file;

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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to create the attr */
    H5MD_attr_t *attr = NULL; /* the attr object that is created and passed to the user */
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
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message");

    /* allocate the attr object that is returned to the user */
    if(NULL == (attr = H5FL_CALLOC(H5MD_attr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");
    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
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
    if(NULL == (new_attr = H5MD_attr_create(name, type, space, acpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "failed to create client attr object");

    new_attr->oloc.file = obj->raw_file;
    /* set the attr struct in the high level object */
    attr->attr = new_attr;

    /* set common object parameters */
    attr->common.obj_type = H5I_ATTR;
    attr->common.raw_file = obj->raw_file;
    attr->common.file = obj->file;

    H5MM_xfree(send_buf);
    H5MM_xfree(recv_buf);
    ret_value = (void *)attr;

done:
    if(acpl_id && acpl_id != H5P_ATTRIBUTE_CREATE_DEFAULT && H5I_dec_ref(acpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, NULL, "can't close plist");
    if(H5S_close(space) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't release dataspace info");
    if(H5T_close(type) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't close datatype");

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
    H5MD_attr_t *attr = (H5MD_attr_t *)obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   buf, (int)(dst_type_size * nelmts), MPI_BYTE, MDS_RANK, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send and receive message");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);
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
    H5MD_attr_t *attr = (H5MD_attr_t *)obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t),  MPI_BYTE, MDS_RANK, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send and receive message");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);
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
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_attr_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_attr_get
 *
 * Purpose:	Gets certain information about an attribute
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_mds_attr_get(void *_obj, H5VL_attr_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Aexists/exists_by_name */
        case H5VL_ATTR_EXISTS:
            {
                H5MD_object_t *obj = (H5MD_object_t *)_obj;
                H5VL_loc_params_t loc_params  = va_arg (arguments, H5VL_loc_params_t);
                char *attr_name      = va_arg (arguments, char *);
                htri_t *ret       = va_arg (arguments, htri_t *);
                void *send_buf = NULL;
                size_t buf_size;

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_attr_get_params(NULL, &buf_size, get_type, obj->obj_id, loc_params, 
                                                attr_name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_attr_get_params(send_buf, &buf_size, get_type, obj->obj_id, loc_params, 
                                                attr_name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               ret, sizeof(htri_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);

                break;
            }
        /* H5Aget_space */
        case H5VL_ATTR_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5MD_attr_t *attr = (H5MD_attr_t *)_obj;

                if((*ret_id = H5A_get_space(attr->attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get space ID of attribute")
                break;
            }
        /* H5Aget_type */
        case H5VL_ATTR_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5MD_attr_t *attr = (H5MD_attr_t *)_obj;

                if((*ret_id = H5A_get_type(attr->attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of attribute")
                break;
            }
        /* H5Aget_create_plist */
        case H5VL_ATTR_GET_ACPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5MD_attr_t *attr = (H5MD_attr_t *)_obj;

                if((*ret_id = H5A_get_create_plist(attr->attr)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for attr")

                break;
            }
        /* H5Aget_name */
        case H5VL_ATTR_GET_NAME:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t	size = va_arg (arguments, size_t);
                char    *buf = va_arg (arguments, char *);
                ssize_t	*ret_val = va_arg (arguments, ssize_t *);
                H5MD_object_t *obj = (H5MD_object_t *)_obj;
                void *send_buf = NULL;
                size_t buf_size;
                void *recv_buf = NULL; /* buffer to hold incoming data from MDS */
                int incoming_msg_size; /* incoming buffer size for MDS returned buffer */
                uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */
                MPI_Status status;

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_attr_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                loc_params, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_attr_get_params(send_buf, &buf_size, get_type, obj->obj_id, 
                                                loc_params, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                INT64DECODE(p, *ret_val);

                if(buf && size)
                    HDstrcpy(buf, (char *)p);
                p += size;

                H5MM_xfree(recv_buf);
               
                break;
            }
        /* H5Aget_info */
        case H5VL_ATTR_GET_INFO:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5A_info_t *ainfo = va_arg (arguments, H5A_info_t *);
                H5MD_object_t *obj = (H5MD_object_t *)_obj;
                void *send_buf = NULL;
                size_t buf_size;
                void *recv_buf = NULL; /* buffer to hold incoming data from MDS */
                int incoming_msg_size; /* incoming buffer size for MDS returned buffer */
                uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */
                MPI_Status status;

                if(H5VL_OBJECT_BY_SELF == loc_params.type || H5VL_OBJECT_BY_IDX == loc_params.type) {
                    /* determine the size of the buffer needed to encode the parameters */
                    if(H5VL__encode_attr_get_params(NULL, &buf_size, get_type, obj->obj_id,
                                                    loc_params) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                    /* allocate the buffer for encoding the parameters */
                    if(NULL == (send_buf = H5MM_malloc(buf_size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                    /* encode the parameters */
                    if(H5VL__encode_attr_get_params(send_buf, &buf_size, get_type, obj->obj_id,
                                                    loc_params) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr get parameters");
                }
                else if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    char *attr_name = va_arg (arguments, char *);

                    /* determine the size of the buffer needed to encode the parameters */
                    if(H5VL__encode_attr_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                    loc_params, attr_name) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                    /* allocate the buffer for encoding the parameters */
                    if(NULL == (send_buf = H5MM_malloc(buf_size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                    /* encode the parameters */
                    if(H5VL__encode_attr_get_params(send_buf, &buf_size, get_type, obj->obj_id, 
                                                    loc_params, attr_name) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr get parameters");
                }

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                H5_DECODE_UNSIGNED(p, ainfo->corder_valid);
                UINT32DECODE(p, ainfo->corder);
                ainfo->cset = (H5T_cset_t)*p++;
                UINT64DECODE_VARLEN(p, ainfo->data_size);

                H5MM_xfree(recv_buf);
                break;
            }
        case H5VL_ATTR_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);
                H5MD_attr_t *attr = (H5MD_attr_t *)_obj;

                /* Set return value */
                *ret = attr->attr->shared->data_size;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_attr_get() */


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
    H5MD_attr_t *attr = (H5MD_attr_t *)obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

    if(H5MD_attr_close(attr->attr) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute");

    attr = H5FL_FREE(H5MD_attr_t, attr);

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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to create the dataset */
    H5MD_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
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
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message");
    H5MM_xfree(send_buf);

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5MD_dset_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");
    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message");
    MPI_Pcontrol(1);

    p = (uint8_t *)recv_buf;

    /* decode the dataset ID at the MDS */
    INT32DECODE(p, dset->common.obj_id);

    /* decode the dataset layout */
    if(FAIL == H5D__decode_layout(p, &layout))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "failed to decode dataset layout");

    /* create the "lightweight" client dataset */
    if(NULL == (new_dset = H5MD_dset_create(obj->raw_file, type_id, space_id, dcpl_id, dapl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "failed to create client dataset object");

    /* set the layout of the dataset */
    new_dset->shared->layout = layout;
    /* Set the dataset's I/O operations */
    if(H5D__layout_set_io_ops(new_dset) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize I/O operations");

    /* reset the chunk ops to use the stub client ops */
    new_dset->shared->layout.storage.u.chunk.ops = H5D_COPS_CLIENT;

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
    dset->common.file = obj->file;

    H5MM_xfree(recv_buf);
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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to create the dataset */
    H5MD_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
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
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message");

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5MD_dset_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");
    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
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
        H5T_t *dt;
        /* Create datatype by decoding buffer */
        if(NULL == (dt = H5T_decode((const unsigned char *)p)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "can't decode object");
        /* Register the type and return the ID */
        if((type_id = H5I_register(H5I_DATATYPE, dt, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register data type");
        p += type_size;
    }

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    if(space_size) {
        H5S_t *ds = NULL;
        if((ds = H5S_decode((const unsigned char *)p)) == NULL)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, NULL, "can't decode object");
        /* Register the type and return the ID */
        if((space_id = H5I_register(H5I_DATASPACE, ds, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, NULL, "unable to register dataspace");
        p += space_size;
    }

    /* decode the layout size */
    UINT64DECODE_VARLEN(p, layout_size);
    /* decode the dataset layout */
    if(FAIL == H5D__decode_layout(p, &layout))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "failed to decode dataset layout");
    p += layout_size;

    /* create the "lightweight" client dataset */
    if(NULL == (new_dset = H5MD_dset_create(obj->raw_file, type_id, space_id, dcpl_id, dapl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "failed to create client dataset object");

    /* set the layout of the dataset */
    new_dset->shared->layout = layout;
    /* Set the dataset's I/O operations */
    if(H5D__layout_set_io_ops(new_dset) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize I/O operations");
    /* reset the chunk ops to use the stub client ops */
    new_dset->shared->layout.storage.u.chunk.ops = H5D_COPS_CLIENT;

    new_dset->oloc.file = obj->raw_file;
    /* set the dataset struct in the high level object */
    dset->dset = new_dset;

    /* set common object parameters */
    dset->common.obj_type = H5I_DATASET;
    dset->common.raw_file = obj->raw_file;
    dset->common.file = obj->file;

    H5MM_xfree(send_buf);
    H5MM_xfree(recv_buf);
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
    H5MD_dset_t *dset = (H5MD_dset_t *)obj;
    hid_t          dset_id = dset->common.obj_id; /* the dataset ID at the MDS */
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    H5P_genplist_t *plist = NULL;
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

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* set the ID of the dataset at the MDS in the XFER property */
    if(H5P_set(plist, H5MD_DSET_ID, &dset_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for mds dataset id");

    /* read raw data */
    if(H5MD_dset_read(dset->dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
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
    H5MD_dset_t *dset = (H5MD_dset_t *)obj;
    hid_t          dset_id = dset->common.obj_id; /* the dataset ID at the MDS */
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    H5P_genplist_t *plist = NULL;
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

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* set the ID of the dataset at the MDS in the XFER property */
    if(H5P_set(plist, H5MD_DSET_ID, &dset_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for mds dataset id");

    /* write raw data */
    if(H5MD_dset_write(dset->dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
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
    H5MD_dset_t *dset = (H5MD_dset_t *)obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_set_extent() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_dataset_get
 *
 * Purpose:	Gets certain information about a dataset
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_mds_dataset_get(void *obj, H5VL_dataset_get_t get_type, hid_t req, va_list arguments)
{
    H5MD_dset_t *dataset = (H5MD_dset_t *)obj;
    H5D_t       *dset = dataset->dset;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL_native_dataset_get((void *)dset, get_type, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed");

#if 0
    switch (get_type) {
        /* H5Dget_space */
        case H5VL_DATASET_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5D_get_space(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get space ID of dataset")

                break;
            }
            /* H5Dget_space_statuc */
        case H5VL_DATASET_GET_SPACE_STATUS:
            {
                H5D_space_status_t *allocation = va_arg (arguments, H5D_space_status_t *);

                /* Read data space address and return */
                if(H5D__get_space_status(dset, allocation, H5AC_ind_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get space status")

                break;
            }
            /* H5Dget_type */
        case H5VL_DATASET_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5D_get_type(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of dataset")

                break;
            }
            /* H5Dget_create_plist */
        case H5VL_DATASET_GET_DCPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                hid_t    dcpl_id;

                dcpl_id = dset->shared->dcpl_id;

                if(dcpl_id == H5P_DATASET_CREATE_DEFAULT) {
                    if(H5I_inc_ref(dcpl_id, FALSE) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTINC, FAIL, "can't increment default DCPL ID");
                    *ret_id = dcpl_id;
                }
                else {
                    H5P_genplist_t  *plist;

                    /* Get the property list */
                    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

                    *ret_id = H5P_copy_plist(plist, FALSE);
                } /* end else */
                break;
            }
            /* H5Dget_access_plist */
        case H5VL_DATASET_GET_DAPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5D_get_access_plist(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get access property list for dataset")

                break;
            }
            /* H5Dget_storage_size */
        case H5VL_DATASET_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);

                /* Set return value */
                if(H5D__get_storage_size(dset, H5AC_ind_dxpl_id, ret) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get size of dataset's storage")
                break;
            }
            /* H5Dget_offset */
        case H5VL_DATASET_GET_OFFSET:
            {
                haddr_t *ret = va_arg (arguments, haddr_t *);

                switch(dset->shared->layout.type) {
                    case H5D_CHUNKED:
                    case H5D_COMPACT:
                        break;

                    case H5D_CONTIGUOUS:
                        /* If dataspace hasn't been allocated or dataset is stored in
                         * an external file, the value will be HADDR_UNDEF. */
                        if(dset->shared->dcpl_cache.efl.nused == 0 || H5F_addr_defined(dset->shared->layout.storage.u.contig.addr))
                            /* Return the absolute dataset offset from the beginning of file. */
                            *ret = dset->shared->layout.storage.u.contig.addr;
                        break;

                    case H5D_LAYOUT_ERROR:
                    case H5D_NLAYOUTS:
                    default:
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "unknown dataset layout type");
                } /*lint !e788 All appropriate cases are covered */

                /* Set return value */
                *ret = H5D__get_offset(dset);
                if(!H5F_addr_defined(*ret))
                    *ret = HADDR_UNDEF;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
    }
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_dataset_get() */


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
    H5MD_dset_t *dset = (H5MD_dset_t *)obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

    /* Free the dataset's memory structure */
    if(H5MD_dset_close(dset->dset) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "unable to close dataset");

    dset = H5FL_FREE(H5MD_dset_t, dset);

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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to create the datatype */
    H5MD_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */
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
    if(NULL == (dtype = H5FL_CALLOC(H5MD_dtype_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata datatype ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &(dtype->common.obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

    if(dtype->common.obj_id < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "MDS failed to commit datatype");

    /* set common object parameters */
    dtype->common.obj_type = H5I_DATATYPE;
    dtype->common.raw_file = obj->raw_file;
    dtype->common.file = obj->file;

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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to create the datatype */
    H5MD_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */
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
    if(NULL == (dtype = H5FL_CALLOC(H5MD_dtype_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to send message");

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");

    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    recv_buf = (void *)H5MM_malloc (incoming_msg_size);

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
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
    dtype->common.file = obj->file;

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
    H5MD_dtype_t *dtype = (H5MD_dtype_t *)obj;
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
    H5MD_dtype_t *dtype = (H5MD_dtype_t *)obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    if((ret_value = H5T_close(dtype->dtype)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close datatype")

    H5MM_xfree(send_buf);
    dtype = H5FL_FREE(H5MD_dtype_t, dtype);

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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to create the group */
    H5MD_group_t *grp = NULL; /* the group object that is created and passed to the user */
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
    if(NULL == (grp = H5FL_CALLOC(H5MD_group_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata group ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &(grp->common.obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.raw_file = obj->raw_file;
    grp->common.file = obj->file;

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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to open the group */
    H5MD_group_t *grp = NULL; /* the group object that is opend and passed to the user */
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
    if(NULL == (grp = H5FL_CALLOC(H5MD_group_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata group ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &(grp->common.obj_id), sizeof(hid_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.raw_file = obj->raw_file;
    grp->common.file = obj->file;

    ret_value = (void *)grp;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_group_get
 *
 * Purpose:	Gets certain information about an group
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_mds_group_get(void *_obj, H5VL_group_get_t get_type, hid_t UNUSED req, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Gget_create_plist */
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t *new_gcpl_id = va_arg (arguments, hid_t *);
                H5MD_group_t *grp = (H5MD_group_t *)_obj;
                void           *send_buf = NULL; /* buffer where the datatype create request is encoded and sent to the mds */
                size_t         buf_size = 0; /* size of send_buf */
                int            incoming_msg_size; /* incoming buffer size for MDS returned dataset */
                void           *recv_buf = NULL; /* buffer to hold incoming data from MDS */
                MPI_Status     status;
                size_t         gcpl_size = 0;
                uint8_t        *p = NULL; /* pointer into recv_buf; used for decoding */

                buf_size = 2 + sizeof(int32_t);
                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_group_get_params(send_buf, &buf_size, H5VL_GROUP_GET_GCPL, 
                                                 grp->common.obj_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine buffer size needed");

                MPI_Pcontrol(0);
                /* send the request to the MDS process */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");

                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                recv_buf = (void *)H5MM_malloc (incoming_msg_size);

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                /* decode the plist size */
                UINT64DECODE_VARLEN(p, gcpl_size);
                /* decode property lists if they are not default*/
                if(gcpl_size) {
                    if((*new_gcpl_id = H5P__decode(p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                    p += gcpl_size;
                }
                else
                    *new_gcpl_id = H5P_GROUP_CREATE_DEFAULT;

                H5MM_xfree(send_buf);
                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Gget_info */
        case H5VL_GROUP_GET_INFO:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5G_info_t *ginfo = va_arg (arguments, H5G_info_t *);
                H5MD_object_t *obj = (H5MD_object_t *)_obj;
                void *send_buf = NULL;
                size_t buf_size;
                void *recv_buf = NULL; /* buffer to hold incoming data from MDS */
                int incoming_msg_size; /* incoming buffer size for MDS returned buffer */
                uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */
                MPI_Status status;

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_group_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                 loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_group_get_params(send_buf, &buf_size, get_type, obj->obj_id,
                                                 loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode group get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                ginfo->storage_type = (H5G_storage_type_t)*p++;
                UINT64DECODE_VARLEN(p, ginfo->nlinks);
                INT64DECODE(p, ginfo->max_corder);
                H5_DECODE_UNSIGNED(p, ginfo->mounted);

                H5MM_xfree(recv_buf);
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_group_get() */


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
    H5MD_group_t *grp = (H5MD_group_t *)obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

    /* Free the group's memory structure */
    grp = H5FL_FREE(H5MD_group_t, grp);

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
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to create the group */
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
                H5MD_object_t *cur_obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

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
    H5MD_object_t *src_obj = (H5MD_object_t *)_src_obj;
    H5MD_object_t *dst_obj = (H5MD_object_t *)_dst_obj;
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
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_link_iterate
 *
 * Purpose:	Iterates through links in a group
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5VL_mds_link_iterate(void *_obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                    H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                                    H5L_iterate_t op, void *op_data, hid_t UNUSED req)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    void *send_buf = NULL;
    size_t buf_size = 0;
    hid_t temp_id;
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_link_iterate_params(NULL, &buf_size, obj->obj_id, loc_params, recursive, 
                                        idx_type, order, idx) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_link_iterate_params(send_buf, &buf_size, obj->obj_id, loc_params, recursive, 
                                        idx_type, order, idx) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode link iterate parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the return value */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                               H5MD_LISTEN_TAG, MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    H5MM_xfree(send_buf);
    MPI_Pcontrol(1);

    /* store the original object ID here since it will get replaced during iterations and restore it
       after the iterations are done */
    temp_id = obj->obj_id;

    while (1) {
        hid_t group_id = FAIL;
        char *name = NULL;
        H5L_info_t linfo;
        MPI_Status status;
        void *recv_buf = NULL;
        int incoming_msg_size = 0;
        size_t len = 0;
        int ret;
        uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */        

        MPI_Pcontrol(0);
        /* probe for a message from the mds */
        if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
        /* get the incoming message size from the probe result */
        if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

        /* allocate the receive buffer */
        if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        /* receive the actual message */
        if(MPI_SUCCESS != MPI_Recv(recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
        MPI_Pcontrol(1);

        p = (uint8_t *)recv_buf;

        /* decode the ret value from the server to see whether we need to continue iterating or stop */
        INT32DECODE(p, ret_value);
        if(ret_value == SUCCEED || ret_value == FAIL) {
            H5MM_xfree(recv_buf);
            if(NULL != idx) {
                if(MPI_SUCCESS != MPI_Recv(idx, sizeof(hsize_t), MPI_BYTE, MDS_RANK, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
            }
            break;
        }

        /* decode the server object id that is meant for the user callback */
        INT32DECODE(p, obj->obj_id);

        /* decode length of the link name and the actual link name */
        UINT64DECODE_VARLEN(p, len);
        if(0 != len) {
            name = H5MM_xstrdup((const char *)(p));
            p += len;
        }

        linfo.type = (H5L_type_t)*p++;
        H5_DECODE_UNSIGNED(p, linfo.corder_valid);
        INT64DECODE(p, linfo.corder);
        linfo.cset = (H5T_cset_t)*p++;

        if(linfo.type == H5L_TYPE_HARD) {
            UINT64DECODE_VARLEN(p, linfo.u.address);
        }
        else if (linfo.type == H5L_TYPE_SOFT || linfo.type == H5L_TYPE_EXTERNAL ||
                 (linfo.type >= H5L_TYPE_UD_MIN && linfo.type <= H5L_TYPE_MAX)) {
            UINT64DECODE_VARLEN(p, linfo.u.val_size);
        }
        else
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "invalid link type");

        H5MM_xfree(recv_buf);

        /* Get the group ID for the object */
        if((group_id = H5I_get_id(obj, H5I_GROUP)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get group ID")
        if(H5I_inc_ref(group_id, TRUE) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTSET, FAIL, "incrementing file ID failed")

        /* execute the user iterate callback */
        ret = op(group_id, name, &linfo, op_data);

        /* send result to MDS */
        MPI_Pcontrol(0);
        /* send the request to the MDS process and recieve the return value */
        if(MPI_SUCCESS != MPI_Send(&ret, sizeof(int), MPI_BYTE, MDS_RANK, 
                                   H5MD_LISTEN_TAG, MPI_COMM_WORLD))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
        MPI_Pcontrol(1);

        /* decrement the ref count on the group ID but do not call the free method, because that 
         * will send a close request to the MDS. The MDS will already have closed this group at
         * its side part of the native iterate implementation. We only need to free the aux struct */
        if(H5I_dec_ref_no_free(group_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't decrement ref count on group ID");

        H5MM_xfree(name);
    }
    obj->obj_id = temp_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_link_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_link_get
 *
 * Purpose:	Gets certain data about a link
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_mds_link_get(void *_obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, 
                  hid_t UNUSED req, va_list arguments)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    void *send_buf = NULL;
    size_t buf_size;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Lexists */
        case H5VL_LINK_EXISTS:
            {
                htri_t *ret    = va_arg (arguments, htri_t *);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_link_get_params(NULL, &buf_size, get_type, obj->obj_id, loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_link_get_params(send_buf, &buf_size, get_type, obj->obj_id, loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode link get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               ret, sizeof(htri_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);

                break;
            }
        /* H5Lget_info/H5Lget_info_by_idx */
        case H5VL_LINK_GET_INFO:
            {
                H5L_info_t *linfo  = va_arg (arguments, H5L_info_t *);
                void *recv_buf = NULL; /* buffer to hold incoming data from MDS */
                int incoming_msg_size; /* incoming buffer size for MDS returned buffer */
                uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */
                MPI_Status status;

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_link_get_params(NULL, &buf_size, get_type, obj->obj_id, loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_link_get_params(send_buf, &buf_size, get_type, obj->obj_id, loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode link get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                linfo->type = (H5L_type_t)*p++;
                H5_DECODE_UNSIGNED(p, linfo->corder_valid);
                INT64DECODE(p, linfo->corder);
                linfo->cset = (H5T_cset_t)*p++;

                if(linfo->type == H5L_TYPE_HARD) {
                    UINT64DECODE_VARLEN(p, linfo->u.address);
                }
                else if (linfo->type == H5L_TYPE_SOFT || linfo->type == H5L_TYPE_EXTERNAL ||
                         (linfo->type >= H5L_TYPE_UD_MIN && linfo->type <= H5L_TYPE_MAX)) {
                    UINT64DECODE_VARLEN(p, linfo->u.val_size);
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "invalid link type");

                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Lget_name_by_idx */
        case H5VL_LINK_GET_NAME:
            {
                char       *name   = va_arg (arguments, char *);
                size_t      size   = va_arg (arguments, size_t);
                ssize_t    *ret    = va_arg (arguments, ssize_t *);
                void *recv_buf = NULL; /* buffer to hold incoming data from MDS */
                int incoming_msg_size; /* incoming buffer size for MDS returned buffer */
                uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */
                MPI_Status status;

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_link_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                loc_params, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_link_get_params(send_buf, &buf_size, get_type, obj->obj_id, 
                                                loc_params, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode link get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                INT64DECODE(p, *ret);
                if(name && size)
                    HDstrcpy(name, (char *)p);
                p += size;

                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Lget_val/H5Lget_val_by_idx */
        case H5VL_LINK_GET_VAL:
            {
                void       *buf    = va_arg (arguments, void *);
                size_t     size    = va_arg (arguments, size_t);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_link_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                loc_params, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_link_get_params(send_buf, &buf_size, get_type, obj->obj_id, 
                                                loc_params, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode link get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               buf, (int)size, MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to communicate with MDS server");
                MPI_Pcontrol(1);
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_link_remove
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
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_mds_link_remove(void *_obj, H5VL_loc_params_t loc_params, hid_t UNUSED req)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    void            *send_buf = NULL;
    size_t           buf_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_link_remove_params(NULL, &buf_size, obj->obj_id, loc_params) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_link_remove_params(send_buf, &buf_size, obj->obj_id, loc_params) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode link remove parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata link ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_link_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_object_open
 *
 * Purpose:	Opens a object inside MDS file.
 *
 * Return:	Success:	object id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *H5VL_mds_object_open(void *_obj, H5VL_loc_params_t loc_params, 
                                  H5I_type_t *opened_type, hid_t UNUSED req)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj; /* location object to open the group */
    void *send_buf = NULL; /* buffer where the group open request is encoded and sent to the mds */
    size_t buf_size = 0; /* size of send_buf */
    void *recv_buf = NULL; /* buffer to hold incoming data from MDS */
    int incoming_msg_size; /* incoming buffer size for MDS returned buffer */
    uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */
    MPI_Status status;
    hid_t new_id; /* id of the object opened at the MDS side */
    void *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_object_open_params(NULL, &buf_size, obj->obj_id, loc_params) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_object_open_params(send_buf, &buf_size, obj->obj_id, loc_params) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to encode object open parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata object ID */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to communicate with MDS server");

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to probe for a message");
    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to get incoming message size");

    /* allocate the receive buffer */
    if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to receive message");
    MPI_Pcontrol(1);

    p = (uint8_t *)recv_buf;

    INT32DECODE(p, new_id);
    *opened_type = (H5I_type_t)*p++;

    switch(*opened_type) {
        case H5I_DATASET:
            {
                H5MD_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
                H5D_t *new_dset = NULL; /* the lighweight dataset struct used to hold the dataset's metadata */
                hid_t type_id=FAIL, space_id=FAIL, dcpl_id=H5P_DATASET_CREATE_DEFAULT;
                H5O_layout_t layout; /* Dataset's layout information */
                size_t type_size, space_size, dcpl_size, layout_size;

                /* allocate the dataset object that is returned to the user */
                if(NULL == (dset = H5FL_CALLOC(H5MD_dset_t)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

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
                    H5T_t *dt;
                    /* Create datatype by decoding buffer */
                    if(NULL == (dt = H5T_decode((const unsigned char *)p)))
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "can't decode object");
                    /* Register the type and return the ID */
                    if((type_id = H5I_register(H5I_DATATYPE, dt, FALSE)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register data type");
                    p += type_size;
                }

                /* decode the space size */
                UINT64DECODE_VARLEN(p, space_size);
                if(space_size) {
                    H5S_t *ds = NULL;
                    if((ds = H5S_decode((const unsigned char *)p)) == NULL)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, NULL, "can't decode object");
                    /* Register the type and return the ID */
                    if((space_id = H5I_register(H5I_DATASPACE, ds, FALSE)) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, NULL, "unable to register dataspace");
                    p += space_size;
                }

                /* decode the layout size */
                UINT64DECODE_VARLEN(p, layout_size);
                /* decode the dataset layout */
                if(FAIL == H5D__decode_layout(p, &layout))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "failed to decode dataset layout");
                p += layout_size;

                /* create the "lightweight" client dataset */
                if(NULL == (new_dset = H5MD_dset_create(obj->raw_file, type_id, space_id, 
                                                       dcpl_id, H5P_DATASET_ACCESS_DEFAULT)))
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "failed to create client dataset object");

                /* set the layout of the dataset */
                new_dset->shared->layout = layout;
                /* Set the dataset's I/O operations */
                if(H5D__layout_set_io_ops(new_dset) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize I/O operations");
                /* reset the chunk ops to use the stub client ops */
                new_dset->shared->layout.storage.u.chunk.ops = H5D_COPS_CLIENT;

                new_dset->oloc.file = obj->raw_file;

                /* set the dataset struct in the high level object */
                dset->dset = new_dset;

                /* set common object parameters */
                dset->common.obj_type = H5I_DATASET;
                dset->common.raw_file = obj->raw_file;
                dset->common.file = obj->file;
                dset->common.obj_id = new_id;

                ret_value = (void *)dset;
                break;
            }
        case H5I_DATATYPE:
            {
                H5MD_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */

                /* allocate the dataset object that is returned to the user */
                if(NULL == (dtype = H5FL_CALLOC(H5MD_dtype_t)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

                if(NULL == (dtype->dtype = H5T_decode((const unsigned char *)p)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "unable to decode datatype");

                /* set common object parameters */
                dtype->common.obj_type = H5I_DATATYPE;
                dtype->common.raw_file = obj->raw_file;
                dtype->common.file = obj->file;
                dtype->common.obj_id = new_id;

                ret_value = (void *)dtype;
                break;
            }
        case H5I_GROUP:
            {
                H5MD_group_t *grp = NULL; /* the group object that is opend and passed to the user */

                /* allocate the group object that is returned to the user */
                if(NULL == (grp = H5FL_CALLOC(H5MD_group_t)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate MDS object struct");

                /* set common object parameters */
                grp->common.obj_type = H5I_GROUP;
                grp->common.raw_file = obj->raw_file;
                grp->common.file = obj->file;
                grp->common.obj_id = new_id;

                ret_value = (void *)grp;
                break;
            }
        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
        case H5I_DATASPACE:
        case H5I_ATTR:
        case H5I_REFERENCE:
        case H5I_VFL:
        case H5I_VOL:
        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
        case H5I_NTYPES:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, NULL, "not a valid file object (dataset, group, or datatype)")
        break;
    }

    H5MM_xfree(send_buf);
    H5MM_xfree(recv_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_object_open */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_object_copy
 *
 * Purpose:	Copys a object through the MDS plugin.
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
H5VL_mds_object_copy(void *_src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                     void *_dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                     hid_t ocpypl_id, hid_t lcpl_id, hid_t UNUSED req)
{
    H5MD_object_t *src_obj = (H5MD_object_t *)_src_obj;
    H5MD_object_t *dst_obj = (H5MD_object_t *)_dst_obj;
    void        *send_buf = NULL;
    size_t      buf_size = 0;
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_object_copy_params(NULL, &buf_size, src_obj->obj_id, loc_params1, src_name,
                                       dst_obj->obj_id, loc_params2, dst_name, 
                                       ocpypl_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_object_copy_params(send_buf, &buf_size, src_obj->obj_id, loc_params1, src_name,
                                       dst_obj->obj_id, loc_params2, dst_name, 
                                       ocpypl_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode link move parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata link ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to communicate with MDS server");
    MPI_Pcontrol(1);

    H5MM_xfree(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_object_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_object_visit
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
static herr_t H5VL_mds_object_visit(void *_obj, H5VL_loc_params_t loc_params, H5_index_t idx_type,
                                    H5_iter_order_t order, H5O_iterate_t op, void *op_data, 
                                    hid_t UNUSED req)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    void *send_buf = NULL;
    size_t buf_size = 0;
    hid_t temp_id;
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the parameters */
    if(H5VL__encode_object_visit_params(NULL, &buf_size, obj->obj_id, loc_params, idx_type, order) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine buffer size needed");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* encode the parameters */
    if(H5VL__encode_object_visit_params(send_buf, &buf_size, obj->obj_id, loc_params, idx_type, order) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode link iterate parameters");

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                               H5MD_LISTEN_TAG, MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    H5MM_xfree(send_buf);
    MPI_Pcontrol(1);

    /* store the original object ID here since it will get replaced during iterations and restore it
       after the iterations are done */
    temp_id = obj->obj_id;

    while (1) {
        hid_t new_id = FAIL;
        char *name = NULL;
        H5O_info_t info;
        MPI_Status status;
        void *recv_buf = NULL;
        int incoming_msg_size = 0;
        size_t len = 0, info_size = 0;
        herr_t ret;
        uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */

        MPI_Pcontrol(0);
        /* probe for a message from the mds */
        if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
        /* get the incoming message size from the probe result */
        if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

        /* allocate the receive buffer */
        if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        /* receive the actual message */
        if(MPI_SUCCESS != MPI_Recv(recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
        MPI_Pcontrol(1);

        p = (uint8_t *)recv_buf;

        /* decode the ret value from the server to see whether we need to continue iterating or stop */
        INT32DECODE(p, ret_value);
        if(ret_value == SUCCEED || ret_value == FAIL) {
            H5MM_xfree(recv_buf);
            break;
        }

        /* decode the server object id that is meant for the user callback */
        INT32DECODE(p, obj->obj_id);

        /* decode length of the link name and the actual link name */
        UINT64DECODE_VARLEN(p, len);
        if(0 != len) {
            name = H5MM_xstrdup((const char *)(p));
            p += len;
        }

        UINT64DECODE_VARLEN(p, info_size);
        if(info_size) {
            /* decode info of the object */ 
            if(FAIL == H5O__decode_info(p, &info))
                HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to decode object info");
            p += info_size;
        }

        H5MM_xfree(recv_buf);

        /* Get the ID for the object */
        if((new_id = H5I_get_id(obj, obj->obj_type)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get object ID")
        if(H5I_inc_ref(new_id, TRUE) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTSET, FAIL, "incrementing file ID failed")

        /* execute the user iterate callback */
        ret = op(new_id, name, &info, op_data);

        /* send result to MDS */
        MPI_Pcontrol(0);
        /* send the request to the MDS process and receive the return value */
        if(MPI_SUCCESS != MPI_Send(&ret, sizeof(int), MPI_BYTE, MDS_RANK, 
                                   H5MD_LISTEN_TAG, MPI_COMM_WORLD))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
        MPI_Pcontrol(1);

        /* decrement the ref count on the group ID but do not call the free method, because that 
         * will send a close request to the MDS. The MDS will already have closed this group at
         * its side part of the native iterate implementation. We only need to free the aux struct */
        if(H5I_dec_ref_no_free(new_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't decrement ref count on object ID");
        H5MM_xfree(name);
    }
    obj->obj_id = temp_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_object_visit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_object_misc
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
H5VL_mds_object_misc(void *_obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, 
                     hid_t UNUSED req, va_list arguments)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    void *send_buf = NULL;
    size_t buf_size = 0;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (misc_type) {
        /* H5Arename/rename_by_name */
        case H5VL_ATTR_RENAME:
            {
                const char    *old_name  = va_arg (arguments, const char *);
                const char    *new_name  = va_arg (arguments, const char *);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_misc_params(NULL, &buf_size, misc_type, obj->obj_id, loc_params, 
                                                   old_name, new_name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_misc_params(send_buf, &buf_size, misc_type, obj->obj_id, 
                                                   loc_params, old_name, new_name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr rename params");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Oincr_refcount / H5Odecr_refcount */
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                int update_ref  = va_arg (arguments, int);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_misc_params(NULL, &buf_size, misc_type, obj->obj_id, loc_params, 
                                                   update_ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_misc_params(send_buf, &buf_size, misc_type, obj->obj_id, 
                                                   loc_params, update_ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr rename params");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Oset_comment */
        case H5VL_OBJECT_SET_COMMENT:
            {
                const char    *comment  = va_arg (arguments, char *);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_misc_params(NULL, &buf_size, misc_type, obj->obj_id, loc_params, 
                                                   comment) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_misc_params(send_buf, &buf_size, misc_type, obj->obj_id, 
                                                   loc_params, comment) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr rename params");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
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

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_misc_params(NULL, &buf_size, misc_type, obj->obj_id, loc_params, 
                                                   name, ref_type, space_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_misc_params(send_buf, &buf_size, misc_type, obj->obj_id, 
                                                   loc_params, name, ref_type, space_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr rename params");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               ref, (int)ref_size, MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_object_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_object_optional
 *
 * Purpose:	Perform a plugin specific operation for an objectibute
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
H5VL_mds_object_optional(void UNUSED *obj, H5VL_loc_params_t UNUSED loc_params, 
                         H5VL_object_optional_t optional_type, hid_t UNUSED req, 
                         va_list UNUSED arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (optional_type) {
        case H5VL_OPTIONAL:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this operation on object");       
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_object_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_object_get
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
H5VL_mds_object_get(void *_obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, 
                    hid_t UNUSED req, va_list arguments)
{
    H5MD_object_t *obj = (H5MD_object_t *)_obj;
    void *send_buf = NULL;
    size_t buf_size = 0;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Oexists_by_name */
        case H5VL_OBJECT_EXISTS:
            {
                htri_t	  *ret      = va_arg (arguments, htri_t *);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_get_params(NULL, &buf_size, get_type, obj->obj_id, loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_get_params(send_buf, &buf_size, get_type, obj->obj_id, loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr rename params");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               ret, sizeof(htri_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Oget_info / H5Oget_info_by_name / H5Oget_info_by_idx */
        case H5VL_OBJECT_GET_INFO:
            {
                H5O_info_t  *obj_info = va_arg (arguments, H5O_info_t *);
                MPI_Status status;
                void *recv_buf = NULL;
                int incoming_msg_size = 0;
                size_t info_size = 0;
                uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_get_params(NULL, &buf_size, get_type, obj->obj_id, loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_get_params(send_buf, &buf_size, get_type, obj->obj_id, loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode attr rename params");

                MPI_Pcontrol(0);
                /* send the request to the MDS process */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv(recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                UINT64DECODE_VARLEN(p, info_size);
                if(info_size) {
                    /* decode info of the object */ 
                    if(FAIL == H5O__decode_info(p, obj_info))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to decode object info");
                    p += info_size;
                }

                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Oget_comment / H5Oget_comment_by_name */
        case H5VL_OBJECT_GET_COMMENT:
            {
                char     *comment =  va_arg (arguments, char *);
                size_t   size  =  va_arg (arguments, size_t);
                ssize_t  *ret     =  va_arg (arguments, ssize_t *);
                MPI_Status status;
                void *recv_buf = NULL;
                int incoming_msg_size = 0;
                uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                loc_params, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_get_params(send_buf, &buf_size, get_type, obj->obj_id, 
                                                loc_params, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode object get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                INT64DECODE(p, *ret);
                if(comment && size)
                    HDstrcpy(comment, (char *)p);
                p += size;

                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Rget_region */
        case H5VL_REF_GET_REGION:
            {
                hid_t       *ret     =  va_arg (arguments, hid_t *);
                H5R_type_t  ref_type =  va_arg (arguments, H5R_type_t);
                void        *ref     =  va_arg (arguments, void *);
                H5S_t       *space = NULL;    /* Dataspace object */
                MPI_Status status;
                void *recv_buf = NULL;
                int incoming_msg_size = 0;
                size_t space_size = 0;
                uint8_t *p = NULL; /* pointer into recv_buf; used for decoding */

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                  loc_params, ref_type, ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_get_params(send_buf, &buf_size, get_type, obj->obj_id, 
                                                  loc_params, ref_type, ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode object get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                /* decode the space size */
                UINT64DECODE_VARLEN(p, space_size);
                if(space_size) {
                    /* decode the dataspace */
                    if((space = H5S_decode((const unsigned char *)p)) == NULL)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
                    /* Register the type and return the ID */
                    if((*ret = H5I_register(H5I_DATASPACE, space, FALSE)) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, FAIL, "unable to register dataspace");
                    p += space_size;
                }

                H5MM_xfree(recv_buf);
                break;
            }
        /* H5Rget_obj_type2 */
        case H5VL_REF_GET_TYPE:
            {
                H5O_type_t  *obj_type  =  va_arg (arguments, H5O_type_t *);
                H5R_type_t  ref_type   =  va_arg (arguments, H5R_type_t);
                const void  *ref       =  va_arg (arguments, const void *);

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                  loc_params, ref_type, ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_get_params(send_buf, &buf_size, get_type, obj->obj_id, 
                                                  loc_params, ref_type, ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode object get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                               obj_type, 1, MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                               MPI_COMM_WORLD, MPI_STATUS_IGNORE))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send & receive message");
                MPI_Pcontrol(1);

                H5MM_xfree(send_buf);
                break;
            }
        /* H5Rget_name */
        case H5VL_REF_GET_NAME:
            {
                ssize_t     *ret       = va_arg (arguments, ssize_t *);
                char        *name      = va_arg (arguments, char *);
                size_t      size       = va_arg (arguments, size_t);
                H5R_type_t  ref_type   = va_arg (arguments, H5R_type_t);
                void        *ref       = va_arg (arguments, void *);
                MPI_Status status;
                void *recv_buf = NULL;
                int incoming_msg_size = 0;
                uint8_t *p = NULL; /* pointer into recv_buf, used for decoding */

                /* determine the size of the buffer needed to encode the parameters */
                if(H5VL__encode_object_get_params(NULL, &buf_size, get_type, obj->obj_id, 
                                                  loc_params, ref_type, ref, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to determine buffer size needed");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* encode the parameters */
                if(H5VL__encode_object_get_params(send_buf, &buf_size, get_type, obj->obj_id, 
                                                  loc_params, ref_type, ref, size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode object get parameters");

                MPI_Pcontrol(0);
                /* send the request to the MDS process and recieve the return value */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                           H5MD_LISTEN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);

                /* probe for a message from the mds */
                if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
                /* get the incoming message size from the probe result */
                if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

                /* allocate the receive buffer */
                if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* receive the actual message */
                if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                            H5MD_RETURN_TAG, MPI_COMM_WORLD, &status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
                MPI_Pcontrol(1);

                p = (uint8_t *)recv_buf;

                INT64DECODE(p, *ret);
                if(name && size)
                    HDstrcpy(name, (char *)p);
                p += size;

                H5MM_xfree(recv_buf);
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from object")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_object_get() */

#endif /* H5_HAVE_PARALLEL */
