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

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */
#define H5T_PACKAGE		/*suppress error about including H5Tpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_native_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Dpkg.h"             /* Dataset pkg                          */
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Tpkg.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLnative.h"         /* Native VOL plugin			*/

/* The driver identification number, initialized at runtime */
static hid_t H5VL_NATIVE_g = 0;


/* Prototypes */
static herr_t H5VL_native_term(void);
static hid_t  H5VL_native_file_open(const char *name, unsigned flags, hid_t fcpl_id, 
                               hid_t fapl_id, hid_t dxpl_id);
static herr_t H5VL_native_file_close(hid_t fid);
static hid_t  H5VL_native_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
static herr_t H5VL_native_file_flush(hid_t fid, H5F_scope_t scope);
static herr_t H5VL_native_file_get(hid_t file_id, H5VL_file_get_t get_type, 
                                   int num_args, va_list arguments);

static hid_t H5VL_native_dataset_create(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id,
                                        hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id);
static hid_t H5VL_native_dataset_open(hid_t loc_id, const char *name, hid_t dapl_id);
static herr_t H5VL_native_dataset_close(hid_t dataset_id);
static herr_t H5VL_native_dataset_read(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
                                       hid_t file_space_id, hid_t plist_id, void *buf);
static herr_t H5VL_native_dataset_write(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
                                        hid_t file_space_id, hid_t plist_id, const void *buf);
static herr_t H5VL_native_dataset_get(hid_t id, H5VL_dataset_get_t get_type, 
                                      int num_args, va_list arguments);

static herr_t H5VL_native_datatype_commit(hid_t loc_id, const char *name, hid_t type_id, 
                                          hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id);
static hid_t H5VL_native_datatype_open(hid_t loc_id, const char *name, hid_t tapl_id);

static hid_t H5VL_native_group_create(hid_t loc_id, const char *name, hid_t lcpl_id, 
                                      hid_t gcpl_id, hid_t gapl_id);
static hid_t H5VL_native_group_open(hid_t loc_id, const char *name, hid_t gapl_id);
static herr_t H5VL_native_group_close(hid_t group_id);
static herr_t H5VL_native_group_get(hid_t obj_id, H5VL_group_get_t get_type, 
                                    int num_args, va_list arguments);

static hid_t H5VL_native_object_open(hid_t loc_id, void *location, hid_t lapl_id);
static herr_t H5VL_native_object_close(hid_t object_id);
static herr_t H5VL_native_object_get(hid_t id, H5VL_object_get_t get_type, 
                                     int num_args, va_list arguments);
static herr_t H5VL_native_object_lookup(hid_t loc_id, H5VL_object_lookup_t lookup_type, 
                                        int num_args, va_list arguments);

H5VL_class_t H5VL_native_g = {
    "native",					/* name */
    0,                                          /* nrefs */
    H5VL_native_term,                           /*terminate */
    {                                           /* attribute_cls */
        NULL,                                   /* create */
        NULL,                                   /* open */
        NULL,                                   /* read */
        NULL,                                   /* write */
        NULL,                                   /* delete */
        NULL                                    /* close */
    },
    {                                           /* datatype_cls */
        H5VL_native_datatype_commit,            /* commit */
        H5VL_native_datatype_open               /* open */
    },
    {                                           /* dataset_cls */
        H5VL_native_dataset_create,             /* create */
        H5VL_native_dataset_open,               /* open */
        H5VL_native_dataset_read,               /* read */
        H5VL_native_dataset_write,              /* write */
        H5VL_native_dataset_get,                /* get */
        H5VL_native_dataset_close               /* close */
    },
    {                                           /* group_cls */
        H5VL_native_group_create,               /* create */
        H5VL_native_group_open,                 /* open */
        H5VL_native_group_get,                  /* get */
        H5VL_native_group_close                 /* close */
    },
    {                                           /* file_cls */
        H5VL_native_file_create,                /* create */
        H5VL_native_file_open,                  /* open */
        H5VL_native_file_flush,                 /* flush */
        H5VL_native_file_get,                   /* get */
        H5VL_native_file_close                  /* close */
    },
    {                                           /* link_cls */
        NULL,                                   /* create */
        NULL,                                   /* delete */
        NULL,                                   /* move */
        NULL                                    /* copy */
    },
    {                                           /* object_cls */
        H5VL_native_object_open,                /* open */
        NULL,                                   /* move */
        NULL,                                   /* copy */
        H5VL_native_object_lookup,              /* lookup */
        H5VL_native_object_get,                 /* get */
        H5VL_native_object_close                /* close */
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

    FUNC_LEAVE_NOAPI(SUCCEED)
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
H5VL_class_t *
H5VL_native_init(void)
{
    H5VL_class_t *ret_value = NULL;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set return value */
    ret_value = &H5VL_native_g;
    ret_value->nrefs ++;

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
    H5VL_native_g.nrefs = 0;

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

    ret_value = H5P_set_vol(plist, &H5VL_native_g);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_native() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_open
 *
 * Purpose:	Opens a file as a native HDF5 file.
 *
 * Return:	Success:	file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_file_open(const char *name, unsigned flags, hid_t fcpl_id, 
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

done:
    if(ret_value < 0 && new_file && H5F_try_close(new_file) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_create
 *
 * Purpose:	Creates a file as a native HDF5 file.
 *
 * Return:	Success:	the file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    H5F_t *new_file;           /* file struct */
    hid_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if (0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
	flags |= H5F_ACC_EXCL;	 /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

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
} /* end H5VL_native_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_close
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
H5VL_native_file_close(hid_t file_id)
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
} /* end H5VL_native_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_flush
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
H5VL_native_file_flush(hid_t object_id, H5F_scope_t scope)
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
} /* end H5VL_native_file_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_file_get
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
H5VL_native_file_get(hid_t obj_id, H5VL_file_get_t get_type, int num_args, va_list arguments)
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
        case H5VL_FILE_GET_FAPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5F_get_access_plist(f, TRUE)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")
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
        /* H5Fget_filesize */
        case H5VL_FILE_GET_SIZE:
            {
                haddr_t    eof;                     /* End of file address */
                hsize_t    *ret = va_arg (arguments, hsize_t *);

                /* Go get the actual file size */
                if(HADDR_UNDEF == (eof = H5FDget_eof(f->shared->lf)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get file size")
                *ret = (hsize_t)eof;
                break;
            }
        /* H5Fget_freespace */
        case H5VL_FILE_GET_FREE_SPACE:
            {
                hsize_t	tot_space;	/* Amount of free space in the file */
                hssize_t    *ret = va_arg (arguments, hssize_t *);

                /* Go get the actual amount of free space in the file */
                if(H5MF_get_freespace(f, H5AC_ind_dxpl_id, &tot_space, NULL) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")
                *ret = (hssize_t)tot_space;
                break;
            }
        case H5VL_FILE_GET_FREE_SECTIONS:
            {
                H5F_sect_info_t *sect_info = va_arg (arguments, H5F_sect_info_t *);
                ssize_t         *ret       = va_arg (arguments, ssize_t *);
                H5F_mem_t       type       = va_arg (arguments, H5F_mem_t);
                size_t          nsects     = va_arg (arguments, size_t);

                /* Go get the free-space section information in the file */
                if((*ret = H5MF_get_free_sections(f, H5AC_ind_dxpl_id, 
                                                  type, nsects, sect_info)) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")
                break;
            }
        /* H5Fget_info2 */
        case H5VL_FILE_GET_INFO:
            {
                H5F_info2_t *finfo = va_arg (arguments, H5F_info2_t *);

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
                char      *name = va_arg (arguments, char *);
                ssize_t   *ret  = va_arg (arguments, ssize_t *);
                size_t     size = va_arg (arguments, size_t);
                size_t     len;

                len = HDstrlen(H5F_OPEN_NAME(f));

                if(name) {
                    HDstrncpy(name, H5F_OPEN_NAME(f), MIN(len + 1,size));
                    if(len >= size)
                        name[size-1]='\0';
                } /* end if */

                /* Set the return value for the API call */
                *ret = (ssize_t)len;
                break;
            }
        /* H5Fget_vfd_handle */
        case H5VL_FILE_GET_VFD_HANDLE:
            {
                void **file_handle = va_arg (arguments, void **);
                hid_t  fapl        = va_arg (arguments, hid_t);

                /* Retrieve the VFD handle for the file */
                if(H5F_get_vfd_handle(f, fapl, file_handle) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve VFD handle")
                break;
            }
        /* H5Fget_mdc_config */
        case H5VL_FILE_GET_MDC_CONF:
            {
                H5AC_cache_config_t *config_ptr = va_arg (arguments, H5AC_cache_config_t *);

                /* Go get the resize configuration */
                if(H5AC_get_cache_auto_resize_config(f->shared->cache, config_ptr) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_auto_resize_config() failed.")
                break;
            }
        /* H5Fget_mdc_hit_rate */
        case H5VL_FILE_GET_MDC_HR:
            {
                double *hit_rate_ptr = va_arg (arguments, double *);

                /* Go get the current hit rate */
                if(H5AC_get_cache_hit_rate(f->shared->cache, hit_rate_ptr) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_hit_rate() failed.")
                break;
            }
        /* H5Fget_mdc_size */
        case H5VL_FILE_GET_MDC_SIZE:
            {
                size_t *max_size_ptr        = va_arg (arguments, size_t *);
                size_t *min_clean_size_ptr  = va_arg (arguments, size_t *);
                size_t *cur_size_ptr        = va_arg (arguments, size_t *); 
                int    *cur_num_entries_ptr = va_arg (arguments, int *); 
                int32_t cur_num_entries;

                /* Go get the size data */
                if(H5AC_get_cache_size(f->shared->cache, max_size_ptr, min_clean_size_ptr, 
                                       cur_size_ptr, &cur_num_entries) < 0)
                    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_size() failed.")

                if(cur_num_entries_ptr != NULL)
                    *cur_num_entries_ptr = (int)cur_num_entries;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information")
    } /* end switch */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_group_create
 *
 * Purpose:	Creates a group inside a native h5 file.
 *
 * Return:	Success:	group id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_group_create(hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id)
{
    H5G_loc_t	    loc;                /* Location to create group */
    H5G_t	   *grp = NULL;         /* New group created */
    hid_t           ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    /* if name is NULL then this is from H5Gcreate_anon */
    if (name == NULL) {
        H5G_obj_create_t gcrt_info;         /* Information for group creation */
        /* Set up group creation info */
        gcrt_info.gcpl_id = gcpl_id;
        gcrt_info.cache_type = H5G_NOTHING_CACHED;
        HDmemset(&gcrt_info.cache, 0, sizeof(gcrt_info.cache));

        /* Create the new group & get its ID */
        if(NULL == (grp = H5G__create(loc.oloc->file, &gcrt_info, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group")            
    }
    /* otherwise it's from H5Gcreate */
    else {
        /* Create the new group & get its ID */
        if(NULL == (grp = H5G__create_named(&loc, name, lcpl_id, gcpl_id, gapl_id, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group")
    }

    if((ret_value = H5I_register(H5I_GROUP, grp, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")

done:
    if (name == NULL) {
        /* Release the group's object header, if it was created */
        if(grp) {
            H5O_loc_t *oloc;         /* Object location for group */

            /* Get the new group's object location */
            if(NULL == (oloc = H5G_oloc(grp)))
                HDONE_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to get object location of group")

            /* Decrement refcount on group's object header in memory */
            if(H5O_dec_rc_by_loc(oloc, H5AC_dxpl_id) < 0)
                HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "unable to decrement refcount on newly created object")
         } /* end if */
    }

    if(ret_value < 0)
        if(grp && H5G_close(grp) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_group_open
 *
 * Purpose:	Opens a group inside a native h5 file.
 *
 * Return:	Success:	group id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_group_open(hid_t loc_id, const char *name, hid_t gapl_id)
{
    H5G_loc_t	    loc;                /* Location to open group */
    H5G_t	   *grp = NULL;         /* New group opend */
    hid_t           ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    /* Open the group */
    if((grp = H5G__open_name(&loc, name, gapl_id, H5AC_dxpl_id)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")

    /* Register an ID for the group */
    if((ret_value = H5I_register(H5I_GROUP, grp, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")

done:
    if(ret_value < 0)
        if(grp && H5G_close(grp) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_group_close
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
H5VL_native_group_close(hid_t group_id)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    if(NULL == H5I_object_verify(group_id,H5I_GROUP))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /*
     * Decrement the counter on the group atom.	 It will be freed if the count
     * reaches zero.
     */
    if(H5I_dec_app_ref(group_id) < 0)
    	HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close group")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_group_get
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
H5VL_native_group_get(hid_t obj_id, H5VL_group_get_t get_type, int num_args, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
    /* H5Gget_create_plist */
    case H5VL_GROUP_GET_GCPL:
        {
            hid_t *new_gcpl_id;
            H5G_t *grp = NULL;

            /* Check args */
            if(NULL == (grp = (H5G_t *)H5I_object_verify(obj_id, H5I_GROUP)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

            new_gcpl_id = va_arg (arguments, hid_t *);

            if((*new_gcpl_id = H5G_get_create_plist(grp)) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for group")
            break;
        }
    /* H5Fget_info2 */
    case H5VL_GROUP_GET_INFO:
        {
            H5G_info_t  *grp_info = va_arg (arguments, H5G_info_t *);
            haddr_t     *addr = va_arg (arguments, haddr_t *);
            H5G_loc_t    loc;
            H5O_loc_t    oloc;            	/* Opened object object location */

            if(H5G_loc(obj_id, &loc) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

            if(!H5F_addr_defined(*addr))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no address supplied")

            oloc.addr = *addr;
            oloc.file = loc.oloc->file;

            /* Retrieve the group's information */
            if(H5G__obj_info(&oloc, grp_info/*out*/, H5AC_ind_dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
            break;
        }
    default:
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_open
 *
 * Purpose:	Opens a object inside a native h5 file.
 *
 * Return:	Success:	object id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_object_open(hid_t loc_id, void *location, hid_t lapl_id)
{
    H5G_loc_t   loc;
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;               /* Opened object group hier. path */
    H5O_loc_t   obj_oloc;               /* Opened object object location */
    haddr_t     addr = *((haddr_t *)location);
    hid_t       ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no address supplied")

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);
    obj_loc.oloc->addr = addr;
    obj_loc.oloc->file = loc.oloc->file;
    H5G_name_reset(obj_loc.path);       /* objects opened through this routine don't have a path name */

    /* Open the object */
    if((ret_value = H5O_open_by_loc(&obj_loc, lapl_id, H5AC_dxpl_id, TRUE)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_close
 *
 * Purpose:	Closes a object.
 *
 * Return:	Success:	0
 *		Failure:	-1, object not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_object_close(hid_t object_id)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the type of the object and close it in the correct way */
    switch(H5I_get_type(object_id)) {
        case H5I_GROUP:
        case H5I_DATATYPE:
        case H5I_DATASET:
            if(H5I_object(object_id) == NULL)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object")
            if(H5I_dec_app_ref(object_id) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
            break;

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
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, FAIL, "not a valid file object ID (dataset, group, or datatype)")
        break;
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_get
 *
 * Purpose:	Gets certain data about a file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_object_get(hid_t id, H5VL_object_get_t get_type, int num_args, va_list arguments)
{
    herr_t      ret_value = SUCCEED;    /* Return value */
    H5G_loc_t	loc;                    /* Location of group */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    switch (get_type) {
    /* H5Oget_info / H5Oget_info_by_name / H5Oget_info_by_idx */
    case H5VL_OBJECT_GET_INFO:
        {
            H5O_info_t  *obj_info = va_arg (arguments, H5O_info_t *);
            haddr_t     *addr = va_arg (arguments, haddr_t *);
            H5O_loc_t    oloc;            	/* Opened object object location */

            if(!H5F_addr_defined(*addr))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no address supplied")

            oloc.addr = *addr;
            oloc.file = loc.oloc->file;

            /* Retrieve the object's information */
            if(H5O_get_info(&oloc, H5AC_ind_dxpl_id, TRUE, obj_info) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve object info")
            break;
        }
    /* H5Oget_comment / H5Oget_comment_by_name */
    case H5VL_OBJECT_GET_COMMENT:
        {
            ssize_t  *ret     =  va_arg (arguments, ssize_t *);
            char     *comment =  va_arg (arguments, char *);
            size_t   bufsize  =  va_arg (arguments, size_t);

            if(3 == num_args) {
                /* Retrieve the object's comment */
                if((*ret = H5G_loc_get_comment(&loc, ".", comment/*out*/, bufsize, 
                                              H5P_LINK_ACCESS_DEFAULT, H5AC_ind_dxpl_id)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
            }
            else if(5 == num_args) {
                char *name    = va_arg (arguments, char *);
                hid_t lapl_id = va_arg (arguments, hid_t);

                /* Retrieve the object's comment */
                if((*ret = H5G_loc_get_comment(&loc, name, comment/*out*/, bufsize, lapl_id, H5AC_ind_dxpl_id)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
            }
            break;
        }
    default:
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from object")
    }
done:
    /* Release the object location 
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
    */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_get() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_native_object_lookup
 *
 * Purpose:	Lookup the object location in the file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_object_lookup(hid_t loc_id, H5VL_object_lookup_t lookup_type, 
                          int num_args, va_list arguments)
{
    H5G_loc_t	loc;
    H5G_loc_t   obj_loc;
    H5G_name_t  obj_path;               /* Opened object group hier. path */
    H5O_loc_t   obj_oloc;               /* Opened object object location */
    haddr_t     **location;
    haddr_t     obj_addr;
    hbool_t     loc_found = FALSE;      /* Entry at 'name' found */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    location = va_arg (arguments, haddr_t **);
    *location = (haddr_t *) malloc (sizeof (haddr_t));

    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    /* Set up opened group location to fill in */
    obj_loc.path = &obj_path;
    obj_loc.oloc = &obj_oloc;
    H5G_loc_reset(&obj_loc);

    switch (lookup_type) {
    case H5VL_OBJECT_LOOKUP:
        {
            obj_addr = loc.oloc->addr;
            break;
        }
    case H5VL_OBJECT_LOOKUP_BY_NAME:
        {
            char        *name   = va_arg (arguments, char *);
            hid_t       lapl_id = va_arg (arguments, hid_t);

            HDassert(name && *name);

            /* Find the object's location */
            if((ret_value = H5G_loc_find(&loc, name, &obj_loc/*out*/, lapl_id, H5AC_dxpl_id)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
            loc_found = TRUE;
            obj_addr = (haddr_t)obj_loc.oloc->addr;
            break;
        }
    case H5VL_OBJECT_LOOKUP_BY_IDX:
        {
            char            *group_name   = va_arg (arguments, char *);
            H5_index_t      idx_type      = va_arg (arguments, H5_index_t);
            H5_iter_order_t order         = va_arg (arguments, H5_iter_order_t);
            hsize_t         n             = va_arg (arguments, hsize_t);
            hid_t           lapl_id       = va_arg (arguments, hid_t);

            /* Find the object's location, according to the order in the index */
            if((ret_value = H5G_loc_find_by_idx(&loc, group_name, idx_type, order, n,
                                                &obj_loc/*out*/, lapl_id, H5AC_dxpl_id)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")
            loc_found = TRUE;
            obj_addr = (haddr_t)obj_loc.oloc->addr;
            break;
        }
    case H5VL_OBJECT_LOOKUP_BY_ADDR:
        {
            obj_addr = va_arg (arguments, haddr_t);
            break;
        }
    default:
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't lookup this object")
    }

    *location[0] = obj_addr;
done:
    /* Release the object location if we failed after copying it */
    if(ret_value == FAIL && loc_found)
        if(H5G_loc_free(&obj_loc) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_object_lookup() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_datatype_commit
 *
 * Purpose:	Commits a datatype inside a native h5 file.
 *
 * Return:	Success:	datatype id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_datatype_commit(hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id,
                            hid_t tcpl_id, hid_t tapl_id)
{
    H5G_loc_t	loc;                    /* Location to commit datatype */
    H5T_t	*type;                  /* Datatype for ID */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    if(NULL == (type = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")

    /* Commit the type */
    if(H5T__commit_named(&loc, name, type, lcpl_id, tcpl_id, tapl_id, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to commit datatype")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_datatype_open
 *
 * Purpose:	Opens a named datatype inside a native h5 file.
 *
 * Return:	Success:	datatype id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_datatype_open(hid_t loc_id, const char *name, hid_t tapl_id)
{
    H5T_t      *type = NULL;           /* Datatype opened in file */
    H5G_loc_t	 loc;                   /* Group location of object to open */
    H5G_name_t   path;            	/* Datatype group hier. path */
    H5O_loc_t    oloc;            	/* Datatype object location */
    H5O_type_t   obj_type;              /* Type of object at location */
    H5G_loc_t    type_loc;              /* Group object for datatype */
    hbool_t      obj_found = FALSE;     /* Object at 'name' found */
    hid_t        dxpl_id = H5AC_dxpl_id; /* dxpl to use to open datatype */
    hid_t        ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

   if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

   /* Set up datatype location to fill in */
    type_loc.oloc = &oloc;
    type_loc.path = &path;
    H5G_loc_reset(&type_loc);

    /*
     * Find the named datatype object header and read the datatype message
     * from it.
     */
    if(H5G_loc_find(&loc, name, &type_loc/*out*/, tapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_NOTFOUND, FAIL, "not found")
    obj_found = TRUE;

    /* Check that the object found is the correct type */
    if(H5O_obj_type(&oloc, &obj_type, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get object type")
    if(obj_type != H5O_TYPE_NAMED_DATATYPE)
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a named datatype")

    /* Open it */
    if(NULL == (type = H5T_open(&type_loc, dxpl_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTOPENOBJ, FAIL, "unable to open named datatype")

    /* Register the type and return the ID */
    if((ret_value = H5I_register(H5I_DATATYPE, type, TRUE)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register named datatype")

done:
    if(ret_value < 0) {
        if(type != NULL)
            H5T_close(type);
        else {
            if(obj_found && H5F_addr_defined(type_loc.oloc->addr))
                H5G_loc_free(&type_loc);
        } /* end else */
    } /* end if */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_create
 *
 * Purpose:	Creates a dataset inside a native h5 file.
 *
 * Return:	Success:	dataset id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_dataset_create(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id,
                           hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id)
{
    H5G_loc_t	   loc;                 /* Object location to insert dataset into */
    H5D_t	   *dset = NULL;        /* New dataset's info */
    const H5S_t    *space;              /* Dataspace for dataset */
    hid_t           ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID")
    if(H5I_DATATYPE != H5I_get_type(type_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype ID")
    if(NULL == (space = (const H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace ID")

    /* H5Dcreate_anon */
    if (NULL == name && lcpl_id == 0) {
        /* build and open the new dataset */
        if(NULL == (dset = H5D_create(loc.oloc->file, type_id, space, dcpl_id, dapl_id, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset")

        /* Register the new dataset to get an ID for it */
        if((ret_value = H5I_register(H5I_DATASET, dset, TRUE)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register dataset")
    }
    /* H5Dcreate2 */
    else {
        /* Create the new dataset & get its ID */
        if(NULL == (dset = H5D__create_named(&loc, name, type_id, space, lcpl_id, 
                                             dcpl_id, dapl_id, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset")
        if((ret_value = H5I_register(H5I_DATASET, dset, TRUE)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register dataset")
    }
done:
    if(NULL == name && lcpl_id == 0) {
        /* Release the dataset's object header, if it was created */
        if(dset) {
            H5O_loc_t *oloc;         /* Object location for dataset */

            /* Get the new dataset's object location */
            if(NULL == (oloc = H5D_oloc(dset)))
                HDONE_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to get object location of dataset")

            /* Decrement refcount on dataset's object header in memory */
            if(H5O_dec_rc_by_loc(oloc, H5AC_dxpl_id) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "unable to decrement refcount on newly created object")
        } /* end if */
    }
    if(ret_value < 0)
        if(dset && H5D_close(dset) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_open
 *
 * Purpose:	Opens a dataset inside a native h5 file.
 *
 * Return:	Success:	dataset id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_native_dataset_open(hid_t loc_id, const char *name, hid_t dapl_id)
{
    H5D_t       *dset = NULL;
    H5G_loc_t	 loc;		        /* Object location of group */
    H5G_loc_t	 dset_loc;		/* Object location of dataset */
    H5G_name_t   path;            	/* Dataset group hier. path */
    H5O_loc_t    oloc;            	/* Dataset object location */
    H5O_type_t   obj_type;              /* Type of object at location */
    hbool_t      loc_found = FALSE;     /* Location at 'name' found */
    hid_t        dxpl_id = H5AC_dxpl_id;    /* dxpl to use to open datset */
    hid_t        ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    /* Set up dataset location to fill in */
    dset_loc.oloc = &oloc;
    dset_loc.path = &path;
    H5G_loc_reset(&dset_loc);

    /* Find the dataset object */
    if(H5G_loc_find(&loc, name, &dset_loc, dapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "not found")
    loc_found = TRUE;

    /* Check that the object found is the correct type */
    if(H5O_obj_type(&oloc, &obj_type, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get object type")
    if(obj_type != H5O_TYPE_DATASET)
        HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, FAIL, "not a dataset")

    /* Open the dataset */
    if(NULL == (dset = H5D_open(&dset_loc, dapl_id, dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't open dataset")

    /* Register an atom for the dataset */
    if((ret_value = H5I_register(H5I_DATASET, dset, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "can't register dataset atom")

done:
    if(ret_value < 0) {
        if(dset) {
            if(H5D_close(dset) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")
        } /* end if */
        else {
            if(loc_found && H5G_loc_free(&dset_loc) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't free location")
        } /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_close
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
H5VL_native_dataset_close(hid_t dset_id)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    if(NULL == H5I_object_verify(dset_id, H5I_DATASET))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")

    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.  
     *
     * Pass in TRUE for the 3rd parameter to tell the function to remove
     * dataset's ID even though the freeing function might fail.  Please
     * see the comments in H5I_dec_ref for details. (SLU - 2010/9/7)
     */
    if(H5I_dec_app_ref_always_close(dset_id) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "can't decrement count on dataset ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_read
 *
 * Purpose:	Reads raw data from a dataset into a buffer.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not readd.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_read(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
                         hid_t file_space_id, hid_t plist_id, void *buf/*out*/)
{
    H5D_t	  *dset = NULL;
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    char           fake_char;
    herr_t         ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(NULL == dset->oloc.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
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

    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which
     * doesn't support this feature.
     */
    if(!buf)
        buf = &fake_char;

    /* read raw data */
    if(H5D_read(dset, mem_type_id, mem_space, file_space, plist_id, buf/*out*/) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_write
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
H5VL_native_dataset_write(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
                          hid_t file_space_id, hid_t dxpl_id, const void *buf)
{
    H5D_t	  *dset = NULL;
    const H5S_t   *mem_space = NULL;
    const H5S_t   *file_space = NULL;
    char           fake_char;
    herr_t         ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(NULL == dset->oloc.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
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

    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which
     * doesn't support this feature.
     */
    if(!buf)
        buf = &fake_char;

    /* write raw data */
    if(H5D_write(dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_native_dataset_get
 *
 * Purpose:	Gets certain data about a file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_native_dataset_get(hid_t id, H5VL_dataset_get_t get_type, int num_args, va_list arguments)
{
    H5D_t	*dset = NULL;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

   /* Check args */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")

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
                if(FAIL==(ret_value=H5D_get_space_status(dset, allocation, H5AC_ind_dxpl_id)))
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

                if((*ret_id = H5D_get_create_plist(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for dataset")

                break;
            }
            /* H5Dget_type */
        case H5VL_DATASET_GET_DAPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5D_get_access_plist(dset)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get access property list for dataset")

                break;
            }
            /* H5Dget_type */
        case H5VL_DATASET_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);

                /* Set return value */
                *ret = H5D_get_storage_size(dset, H5AC_ind_dxpl_id);
                break;
            }
            /* H5Dget_type */
        case H5VL_DATASET_GET_OFFSET:
            {
                haddr_t *ret = va_arg (arguments, haddr_t *);

                /* Set return value */
                *ret = H5D_get_offset(dset);
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_native_dataset_get() */
