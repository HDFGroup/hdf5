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
 * Purpose:	The native VOL plugin where access is to a single HDF5 file 
 *              using HDF5 VFDs. 
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_native_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLnative.h"         /* Native VOL plugin			*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/

/* The driver identification number, initialized at runtime */
static hid_t H5VL_NATIVE_g = 0;


/* Prototypes */
static hid_t  H5VL_native_open(const char *name, unsigned flags, hid_t fcpl_id, 
                               hid_t fapl_id, hid_t dxpl_id);
static herr_t H5VL_native_close(hid_t fid);
static hid_t  H5VL_native_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
static herr_t H5VL_native_flush(hid_t fid, H5F_scope_t scope);
static herr_t H5VL_native_get(hid_t file_id, H5VL_file_get_t get_type, void *data, int argc, void **argv);
static herr_t H5VL_native_term(void);

static const H5VL_class_t H5VL_native_g = {
    "native",					/* name */
    H5VL_native_term,                           /*terminate             */
    0, 						/*fapl_size		*/
    NULL,					/*fapl_get		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    {                                           /* file_cls */
        H5VL_native_open,                       /* open */
        H5VL_native_close,                      /* close */
        H5VL_native_create,                     /* create */
        H5VL_native_flush,                      /* flush */
        H5VL_native_get                         /* get */
    },
    {                                           /* dataset_cls */
        NULL,                                   /* open */
        NULL,                                   /* close */
        NULL,                                   /* create */
        NULL,                                   /* read */
        NULL,                                   /* write */
        NULL                                    /* set_extent */
    },
    {                                           /* attribute_cls */
        NULL,                                   /* open */
        NULL,                                   /* close */
        NULL,                                   /* create */
        NULL,                                   /* delete */
        NULL,                                   /* read */
        NULL                                    /* write */
    },
    {                                           /* datatype_cls */
        NULL                                   /* open */
    },
    {                                           /* link_cls */
        NULL,                                   /* create */
        NULL,                                   /* delete */
        NULL,                                   /* move */
        NULL                                    /* copy */
    },
    {                                           /* object_cls */
        NULL,                                   /* create */
        NULL,                                   /* open */
        NULL,                                   /* close */
        NULL,                                   /* move */
        NULL                                    /* copy */
    }
};


/*--------------------------------------------------------------------------
NAME
   H5VL_native_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_native_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_native_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_native_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5VL_native_init())
} /* H5VL_native_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the native plugin.
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_native_init(void)
{
    hid_t ret_value;            /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5I_VOL != H5I_get_type(H5VL_NATIVE_g))
        H5VL_NATIVE_g = H5VL_register(&H5VL_native_g, sizeof(H5VL_class_t), FALSE);

    /* Set return value */
    ret_value = H5VL_NATIVE_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_init() */


/*---------------------------------------------------------------------------
 * Function:	H5VL_native_term
 *
 * Purpose:	Shut down the VOL plugin
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL_native_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VOL ID */
    H5VL_NATIVE_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_native_term() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_native
 *
 * Purpose:	Modify the file access property list to use the H5VL_NATIVE
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
H5Pset_fapl_native(hid_t fapl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_vol(plist, H5VL_NATIVE, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_native() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_open
 *
 * Purpose:	Opens a file as a native HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_open(const char *name, unsigned flags, hid_t fcpl_id, 
                 hid_t fapl_id, hid_t dxpl_id)
{
    H5F_t *new_file;           /* file struct */
    hid_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Open the file */ 
    if(NULL == (new_file = H5F_open(name, flags, fcpl_id, fapl_id, dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to open file")

    /* Get an atom for the file */
    if((ret_value = H5I_register(H5I_FILE, new_file, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

    /* Keep this ID in file object structure */
    new_file->file_id = ret_value;

#if 0
    new_file->vol_id = plugin_id;
    if(H5I_inc_ref(new_file->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINC, FAIL, "unable to increment ref count on VOL plugin")
#endif

done:
    if(ret_value < 0 && new_file && H5F_try_close(new_file) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_create
 *
 * Purpose:	Creates a file as a native HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    H5F_t *new_file;           /* file struct */
    hid_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Create the file */ 
    if(NULL == (new_file = H5F_open(name, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

    /* Get an atom for the file */
    if((ret_value = H5I_register(H5I_FILE, new_file, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

    /* Keep this ID in file object structure */
    new_file->file_id = ret_value;

done:
    if(ret_value < 0 && new_file && H5F_try_close(new_file) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_close
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
H5VL_native_close(hid_t file_id)
{
    int nref;
    H5F_t *f;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check/fix arguments. */
    if(H5I_FILE != H5I_get_type(file_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file ID")

    /* get the file struct */
    if(NULL == (f = (H5F_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* Flush file if this is the last reference to this id and we have write
     * intent, unless it will be flushed by the "shared" file being closed.
     * This is only necessary to replicate previous behaviour, and could be
     * disabled by an option/property to improve performance. */
    if((f->shared->nrefs > 1) && (H5F_INTENT(f) & H5F_ACC_RDWR)) {
        if((nref = H5I_get_ref(f->file_id, FALSE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get ID ref count")
        if(nref == 1)
            if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    } /* end if */

    /*
     * Decrement reference count on atom.  When it reaches zero the file will
     * be closed.
     */
    if(H5I_dec_ref(f->file_id) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_flush
 *
 * Purpose:	Flushs a native HDF5 file.
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
H5VL_native_flush(hid_t object_id, H5F_scope_t scope)
{
    H5F_t	*f = NULL;              /* File to flush */
    H5O_loc_t	*oloc = NULL;           /* Object location for ID */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch(H5I_get_type(object_id)) {
        case H5I_FILE:
            if(NULL == (f = (H5F_t *)H5I_object(object_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
            break;

        case H5I_GROUP:
            {
                H5G_t	*grp;

                if(NULL == (grp = (H5G_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid group identifier")
                oloc = H5G_oloc(grp);
            }
            break;

        case H5I_DATATYPE:
            {
                H5T_t	*type;

                if(NULL == (type = (H5T_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid type identifier")
                oloc = H5T_oloc(type);
            }
            break;

        case H5I_DATASET:
            {
                H5D_t	*dset;

                if(NULL == (dset = (H5D_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier")
                oloc = H5D_oloc(dset);
            }
            break;

        case H5I_ATTR:
            {
                H5A_t	*attr;

                if(NULL == (attr = (H5A_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid attribute identifier")
                oloc = H5A_oloc(attr);
            }
            break;

        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_DATASPACE:
        case H5I_REFERENCE:
        case H5I_VFL:
        case H5I_VOL:
        case H5I_UID:
        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
        case H5I_NTYPES:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    } /* end switch */

    if(!f) {
	if(!oloc)
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "object is not assocated with a file")
	f = oloc->file;
    } /* end if */
    if(!f)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "object is not associated with a file")

    /* Flush the file */
    /*
     * Nothing to do if the file is read only.	This determination is
     * made at the shared open(2) flags level, implying that opening a
     * file twice, once for read-only and once for read-write, and then
     * calling H5Fflush() with the read-only handle, still causes data
     * to be flushed.
     */
    if(H5F_ACC_RDWR & H5F_INTENT(f)) {
        /* Flush other files, depending on scope */
        if(H5F_SCOPE_GLOBAL == scope) {
            /* Call the flush routine for mounted file hierarchies */
            if(H5F_flush_mounts(f, H5AC_dxpl_id) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush mounted file hierarchy")
            } /* end if */
        else {
            /* Call the flush routine, for this file */
            if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file's cached information")
        } /* end else */
    } /* end if */ 

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_get
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
H5VL_native_get(hid_t obj_id, H5VL_file_get_t get_type, void *data, int argc, void **argv)
{
    H5F_t	*f = NULL;              /* File struct */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check/fix arguments. */
    if(H5I_get_type(obj_id) == H5I_FILE ) {
        if(NULL == (f = (H5F_t *)H5I_object(obj_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
    } /* end if */
    else {
        H5G_loc_t     loc;        /* Object location */
        /* Get symbol table entry */
        if(H5G_loc(obj_id, &loc) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object ID")
        f = loc.oloc->file;
    } /* end else */

    switch (get_type) {
    /* H5Fget_access_plist */
    case H5F_GET_FAPL:
        {
            hid_t plist_id;

            /* Retrieve the file's access property list */
            if((plist_id = H5F_get_access_plist(f, TRUE)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")
            *((hid_t *)data) = plist_id;
            break;
        }
    /* H5Fget_create_plist */
    case H5F_GET_FCPL:
        {
            H5P_genplist_t *plist;      /* Property list */
            hid_t plist_id;

            if(NULL == (plist = (H5P_genplist_t *)H5I_object(f->shared->fcpl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

            /* Create the property list object to return */
            if((plist_id = H5P_copy_plist(plist, TRUE)) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to copy file creation properties")
            *((hid_t *)data) = plist_id;
            break;
        }
    /* H5Fget_filesize */
    case H5F_GET_SIZE:
        {
            haddr_t    eof;                     /* End of file address */

            /* Go get the actual file size */
            if(HADDR_UNDEF == (eof = H5FDget_eof(f->shared->lf)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get file size")
            *((hsize_t *)data) = (hsize_t)eof;
            break;
        }
    /* H5Fget_freespace */
    case H5F_GET_FREE_SPACE:
        {
            hsize_t	tot_space;	/* Amount of free space in the file */

            /* Go get the actual amount of free space in the file */
            if(H5MF_get_freespace(f, H5AC_ind_dxpl_id, &tot_space, NULL) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")
            *((hssize_t *)data) = (hssize_t)tot_space;
            break;
        }
    case H5F_GET_FREE_SECTIONS:
        {
            /* Go get the free-space section information in the file */
            if((*((ssize_t *)argv[0]) = H5MF_get_free_sections(f, H5AC_ind_dxpl_id, 
                                                   *((H5F_mem_t *)argv[1]), 
                                                   *((size_t *)argv[2]), 
                                                   (H5F_sect_info_t *)data)) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")
            break;
        }
    /* H5Fget_info2 */
    case H5F_GET_INFO:
        {
            H5F_info2_t *finfo = (H5F_info2_t *)data;
            /* For file IDs, get the file object directly */
            /* (This prevents the H5G_loc() call from returning the file pointer for
             * the top file in a mount hierarchy)
             */
            HDassert(f->shared);

            /* Reset file info struct */
            HDmemset(finfo, 0, sizeof(*finfo));

            /* Get the size of the superblock and any superblock extensions */
            if(H5F_super_size(f, H5AC_ind_dxpl_id, &finfo->super.super_size, 
                              &finfo->super.super_ext_size) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve superblock sizes")

            /* Get the size of any persistent free space */
            if(H5MF_get_freespace(f, H5AC_ind_dxpl_id, &finfo->free.tot_space, 
                                  &finfo->free.meta_size) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve free space information")

            /* Check for SOHM info */
            if(H5F_addr_defined(f->shared->sohm_addr))
                if(H5SM_ih_size(f, H5AC_ind_dxpl_id, &finfo->sohm.hdr_size, &finfo->sohm.msgs_info) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve SOHM index & heap storage info")

            /* Set version # fields */
            finfo->super.version = f->shared->sblock->super_vers;
            finfo->sohm.version = f->shared->sohm_vers;
            finfo->free.version = HDF5_FREESPACE_VERSION;
            break;
        }
    /* H5Fget_intent */
    case H5F_GET_INTENT:
        {
            /* HDF5 uses some flags internally that users don't know about.
             * Simplify things for them so that they only get either H5F_ACC_RDWR
             * or H5F_ACC_RDONLY.
             */
            if(H5F_INTENT(f) & H5F_ACC_RDWR)
                *((unsigned *)data) = H5F_ACC_RDWR;
            else
                *((unsigned *)data) = H5F_ACC_RDONLY;
            break;
        }
    /* H5Fget_name */
    case H5F_GET_NAME:
        {
            size_t    len, size = *((size_t *)argv[1]);
            ssize_t   ret = *((ssize_t *)argv[1]);
            char      *name = (char *)data;

            len = HDstrlen(H5F_OPEN_NAME(f));

            if(name) {
                HDstrncpy(name, H5F_OPEN_NAME(f), MIN(len + 1,size));
                if(len >= size)
                    name[size-1]='\0';
            } /* end if */

            /* Set the return value for the API call */
            ret = (ssize_t)len;
            break;
        }
    /* H5Fget_ */
    case H5F_GET_OBJ_COUNT:
        {
            break;
        }
    /* H5Fget_create_plist */
    case H5F_GET_OBJ_IDS:
        {
            break;
        }
    /* H5Fget_vfd_handle */
    case H5F_GET_VFD_HANDLE:
        {
            hid_t fapl;

            fapl = *((hid_t *)data);
            /* Retrieve the VFD handle for the file */
            if(H5F_get_vfd_handle(f, fapl, argv) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve VFD handle")
            break;
        }
    /* H5Fget_mdc_config */
    case H5F_GET_MDC_CONF:
        {
            /* Go get the resize configuration */
            if(H5AC_get_cache_auto_resize_config(f->shared->cache, (H5AC_cache_config_t *)data) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_auto_resize_config() failed.")
            break;
        }
    /* H5Fget_mdc_hit_rate */
    case H5F_GET_MDC_HR:
        {
            /* Go get the current hit rate */
            if(H5AC_get_cache_hit_rate(f->shared->cache, (double *)data) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_hit_rate() failed.")
            break;
        }
    /* H5Fget_mdc_size */
    case H5F_GET_MDC_SIZE:
        {
            int32_t    cur_num_entries;

            /* Go get the size data */
            if(H5AC_get_cache_size(f->shared->cache, (size_t *)argv[0], (size_t *)argv[1], 
                                   (size_t *)argv[2], &cur_num_entries) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_size() failed.")

            if(data != NULL)
                *((int *)data) = (int)cur_num_entries;   
            break;
        }
    default:
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
}
