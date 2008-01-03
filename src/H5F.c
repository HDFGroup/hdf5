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

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5F_init_interface


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5Gprivate.h"		/* Groups				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Tprivate.h"		/* Datatypes				*/

/* Predefined file drivers */
#include "H5FDcore.h"		/*temporary in-memory files		*/
#include "H5FDfamily.h"		/*family of files			*/
#include "H5FDlog.h"            /* sec2 driver with logging, for debugging */
#include "H5FDmpi.h"            /* MPI-based file drivers		*/
#include "H5FDmulti.h"		/*multiple files partitioned by mem usage */
#include "H5FDsec2.h"		/*Posix unbuffered I/O			*/
#include "H5FDstdio.h"		/* Standard C buffered I/O		*/
#ifdef H5_HAVE_WINDOWS 
#include "H5FDwindows.h"        /* Windows buffered I/O     */
#endif
#include "H5FDdirect.h"         /*Linux direct I/O			*/

/* Struct only used by functions H5F_get_objects and H5F_get_objects_cb */
typedef struct H5F_olist_t {
    H5I_type_t obj_type;        /* Type of object to look for */
    hid_t      *obj_id_list;    /* Pointer to the list of open IDs to return */
    unsigned   *obj_id_count;   /* Number of open IDs */
    struct {
        hbool_t local;          /* Set flag for "local" file searches */
        union {
            H5F_file_t *shared; /* Pointer to shared file to look inside */
            const H5F_t *file;  /* Pointer to file to look inside */
        } ptr;
    } file_info;
    unsigned   list_index;      /* Current index in open ID array */
    int   max_index;            /* Maximum # of IDs to put into array */
} H5F_olist_t;

/* PRIVATE PROTOTYPES */
static unsigned H5F_get_objects(const H5F_t *f, unsigned types, int max_objs, hid_t *obj_id_list);
static int H5F_get_objects_cb(void *obj_ptr, hid_t obj_id, void *key);
static herr_t H5F_get_vfd_handle(const H5F_t *file, hid_t fapl, void** file_handle);
static H5F_t *H5F_new(H5F_file_t *shared, hid_t fcpl_id, hid_t fapl_id,
                      H5FD_t *lf);
static herr_t H5F_dest(H5F_t *f, hid_t dxpl_id);
static herr_t H5F_flush(H5F_t *f, hid_t dxpl_id, H5F_scope_t scope, unsigned flags);
static herr_t H5F_close(H5F_t *f);

/* Declare a free list to manage the H5F_t struct */
H5FL_DEFINE(H5F_t);

/* Declare a free list to manage the H5F_file_t struct */
H5FL_DEFINE(H5F_file_t);


/*-------------------------------------------------------------------------
 * Function:	H5F_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 16, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5F_init, FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_init() */


/*-------------------------------------------------------------------------
 * Function:	H5F_init_interface
 *
 * Purpose:	Initialize interface-specific information.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *              Friday, November 20, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_init_interface(void)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5F_init_interface)

    /*
     * Initialize the atom group for the file IDs.
     */
    if(H5I_register_type(H5I_FILE, (size_t)H5I_FILEID_HASHSIZE, 0, (H5I_free_t)H5F_close)<H5I_FILE)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5F_term_interface
 *
 * Purpose:	Terminate this interface: free all memory and reset global
 *		variables to their initial values.  Release all ID groups
 *		associated with this interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				have affected other interfaces; zero
 *				otherwise.
 *
 *		Failure:        Never fails.
 *
 * Programmer:	Robb Matzke
 *              Friday, February 19, 1999
 *
 *-------------------------------------------------------------------------
 */
int
H5F_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_term_interface)

    if(H5_interface_initialize_g) {
	if((n = H5I_nmembers(H5I_FILE)) != 0) {
            H5I_clear_type(H5I_FILE, FALSE);
	} else {
            /* Make certain we've cleaned up all the shared file objects */
            H5F_sfile_assert_num(0);

	    H5I_dec_type_ref(H5I_FILE);
	    H5_interface_initialize_g = 0;
	    n = 1; /*H5I*/
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* H5F_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_create_plist
 *
 * Purpose:	Get an atom for a copy of the file-creation property list for
 *		this file. This function returns an atom with a copy of the
 *		properties used to create a file.
 *
 * Return:	Success:	template ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_create_plist(hid_t file_id)
{
    H5F_t *file;                /* File info */
    H5P_genplist_t *plist;      /* Property list */
    hid_t ret_value;            /* Return value */

    FUNC_ENTER_API(H5Fget_create_plist, FAIL)
    H5TRACE1("i", "i", file_id);

    /* check args */
    if(NULL == (file = H5I_object_verify(file_id, H5I_FILE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
    if(NULL == (plist = H5I_object(file->shared->fcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Create the property list object to return */
    if((ret_value = H5P_copy_plist(plist)) < 0)
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to copy file creation properties")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_create_plist() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_access_plist
 *
 * Purpose:	Returns a copy of the file access property list of the
 *		specified file.
 *
 *              NOTE: Make sure that, if you are going to overwrite
 *              information in the copied property list that was
 *              previously opened and assigned to the property list, then
 *              you must close it before overwriting the values.
 *
 * Return:	Success:	Object ID for a copy of the file access
 *				property list.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, February 18, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_access_plist(hid_t file_id)
{
    H5F_t *f;           /* File info */
    hid_t ret_value;    /* Return value */

    FUNC_ENTER_API(H5Fget_access_plist, FAIL)
    H5TRACE1("i", "i", file_id);

    /* Check args */
    if(NULL == (f = H5I_object_verify(file_id, H5I_FILE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Retrieve the file's access property list */
    if((ret_value = H5F_get_access_plist(f)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_access_plist() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_access_plist
 *
 * Purpose:	Returns a copy of the file access property list of the
 *		specified file.
 *
 *              NOTE: Make sure that, if you are going to overwrite
 *              information in the copied property list that was
 *              previously opened and assigned to the property list, then
 *              you must close it before overwriting the values.
 *
 * Return:	Success:	Object ID for a copy of the file access
 *				property list.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5F_get_access_plist(H5F_t *f)
{
    H5P_genplist_t *new_plist;              /* New property list */
    H5P_genplist_t *old_plist;              /* Old property list */
    void		*driver_info=NULL;
    hid_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5F_get_access_plist, FAIL)

    /* Check args */
    HDassert(f);

    /* Make a copy of the default file access property list */
    if(NULL == (old_plist = H5I_object(H5P_LST_FILE_ACCESS_g)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")
    if((ret_value = H5P_copy_plist(old_plist)) < 0)
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "can't copy file access property list")
    if(NULL == (new_plist = H5I_object(ret_value)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Copy properties of the file access property list */
    if(H5P_set(new_plist, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, &(f->shared->mdc_initCacheCfg)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set initial metadata cache resize config.")
    if(H5P_set(new_plist, H5F_ACS_DATA_CACHE_ELMT_SIZE_NAME, &(f->shared->rdcc_nelmts)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set data cache element size")
    if(H5P_set(new_plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, &(f->shared->rdcc_nbytes)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set data cache byte size")
    if(H5P_set(new_plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, &(f->shared->rdcc_w0)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set preempt read chunks")
    if(H5P_set(new_plist, H5F_ACS_ALIGN_THRHD_NAME, &(f->shared->threshold)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set alignment threshold")
    if(H5P_set(new_plist, H5F_ACS_ALIGN_NAME, &(f->shared->alignment)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set alignment")
    if(H5P_set(new_plist, H5F_ACS_GARBG_COLCT_REF_NAME, &(f->shared->gc_ref)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set garbage collect reference")
    if(H5P_set(new_plist, H5F_ACS_META_BLOCK_SIZE_NAME, &(f->shared->lf->meta_aggr.alloc_size)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set metadata cache size")
    if(H5P_set(new_plist, H5F_ACS_SIEVE_BUF_SIZE_NAME, &(f->shared->sieve_buf_size)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't sieve buffer size")
    if(H5P_set(new_plist, H5F_ACS_SDATA_BLOCK_SIZE_NAME, &(f->shared->lf->sdata_aggr.alloc_size)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set 'small data' cache size")
    if(H5P_set(new_plist, H5F_ACS_LATEST_FORMAT_NAME, &(f->shared->latest_format)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set 'latest format' flag")

    /*
     * Since we're resetting the driver ID and info, close them if they
     * exist in this new property list.
     */
    if(H5P_facc_close(ret_value, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't free the old driver information")

    /* Increment the reference count on the driver ID and insert it into the property list */
    if(H5I_inc_ref(f->shared->lf->driver_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINC, FAIL, "unable to increment ref count on VFL driver")
    if(H5P_set(new_plist, H5F_ACS_FILE_DRV_ID_NAME, &(f->shared->lf->driver_id)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set file driver ID")

    /* Set the driver "info" in the property list */
    driver_info = H5FD_fapl_get(f->shared->lf);
    if(driver_info != NULL && H5P_set(new_plist, H5F_ACS_FILE_DRV_INFO_NAME, &driver_info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set file driver info")

    /* Set the file close degree appropriately */
    if(f->shared->fc_degree == H5F_CLOSE_DEFAULT && H5P_set(new_plist, H5F_ACS_CLOSE_DEGREE_NAME, &(f->shared->lf->cls->fc_degree)) < 0) {
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set file close degree")
    } else if(f->shared->fc_degree != H5F_CLOSE_DEFAULT && H5P_set(new_plist, H5F_ACS_CLOSE_DEGREE_NAME, &(f->shared->fc_degree)) < 0) {
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set file close degree")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_access_plist() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_obj_count
 *
 * Purpose:	Public function returning the number of opened object IDs
 *		(files, datasets, groups and datatypes) in the same file.
 *
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:	Raymond Lu
 *		Wednesday, Dec 5, 2001
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
int
H5Fget_obj_count(hid_t file_id, unsigned types)
{
    H5F_t    *f=NULL;
    int   ret_value;            /* Return value */

    FUNC_ENTER_API(H5Fget_obj_count, FAIL)
    H5TRACE2("Is", "iIu", file_id, types);

    if( file_id != (hid_t)H5F_OBJ_ALL && (NULL==(f=H5I_object_verify(file_id,H5I_FILE))) )
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file id")
    if( (types&H5F_OBJ_ALL)==0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not an object type")

    if((ret_value = H5F_get_obj_count(f, types)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCOUNT, FAIL, "can't get object count")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5F_get_obj_count
 *
 * Purpose:	Private function return the number of opened object IDs
 *		(files, datasets, groups, datatypes) in the same file.
 *
 * Return:      Non-negative on success; negative on failure.
 *
 * Programmer:  Raymond Lu
 *              Wednesday, Dec 5, 2001
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5F_get_obj_count(const H5F_t *f, unsigned types)
{
    unsigned   ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_get_obj_count)

    ret_value=H5F_get_objects(f, types, -1, NULL);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Fget_object_ids
 *
 * Purpose:	Public function to return a list of opened object IDs.
 *
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:  Raymond Lu
 *              Wednesday, Dec 5, 2001
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_obj_ids(hid_t file_id, unsigned types, int max_objs, hid_t *oid_list)
{
    herr_t   ret_value;
    H5F_t    *f=NULL;

    FUNC_ENTER_API(H5Fget_obj_ids, FAIL)
    H5TRACE4("e", "iIuIs*i", file_id, types, max_objs, oid_list);

    if( file_id != (hid_t)H5F_OBJ_ALL && (NULL==(f=H5I_object_verify(file_id,H5I_FILE))) )
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file id")
    if( (types&H5F_OBJ_ALL)==0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not an object type")
    assert(oid_list);

    ret_value = H5F_get_obj_ids(f, types, max_objs, oid_list);

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5F_get_obj_ids
 *
 * Purpose:     Private function to return a list of opened object IDs.
 *
 * Return:      Non-negative on success; negative on failure.
 *
 * Programmer:  Raymond Lu
 *              Wednesday, Dec 5, 2001
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5F_get_obj_ids(const H5F_t *f, unsigned types, int max_objs, hid_t *oid_list)
{
    unsigned ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_get_obj_ids)

    ret_value = H5F_get_objects(f, types, max_objs, oid_list);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*---------------------------------------------------------------------------
 * Function:	H5F_get_objects
 *
 * Purpose:	This function is called by H5F_get_obj_count or
 *		H5F_get_obj_ids to get number of object IDs and/or a
 *		list of opened object IDs (in return value).
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:  Raymond Lu
 *              Wednesday, Dec 5, 2001
 *
 * Modification:
 *
 *---------------------------------------------------------------------------
 */
static unsigned
H5F_get_objects(const H5F_t *f, unsigned types, int max_index, hid_t *obj_id_list)
{
    unsigned obj_id_count=0;    /* Number of open IDs */
    H5F_olist_t olist;          /* Structure to hold search results */
    unsigned ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_get_objects)

    /* Set up search information */
    olist.obj_id_list  = (max_index==0 ? NULL : obj_id_list);
    olist.obj_id_count = &obj_id_count;
    olist.list_index   = 0;
    olist.max_index   = max_index;

    /* Determine if we are searching for local or global objects */
    if(types&H5F_OBJ_LOCAL) {
        olist.file_info.local = TRUE;
        olist.file_info.ptr.file = f;
    } /* end if */
    else {
        olist.file_info.local = FALSE;
        olist.file_info.ptr.shared = f ? f->shared : NULL;
    } /* end else */

    /* Search through file IDs to count the number, and put their
     * IDs on the object list */
    if(types & H5F_OBJ_FILE) {
        olist.obj_type = H5I_FILE;
        (void)H5I_search(H5I_FILE, H5F_get_objects_cb, &olist);
    } /* end if */

    /* Search through dataset IDs to count number of datasets, and put their
     * IDs on the object list */
    if( (max_index < 0 || (int)olist.list_index < max_index) && (types & H5F_OBJ_DATASET) ) {
        olist.obj_type = H5I_DATASET;
        (void)H5I_search(H5I_DATASET, H5F_get_objects_cb, &olist);
    }

    /* Search through group IDs to count number of groups, and put their
     * IDs on the object list */
    if( (max_index < 0 || (int)olist.list_index < max_index) && (types & H5F_OBJ_GROUP) ) {
        olist.obj_type = H5I_GROUP;
        (void)H5I_search(H5I_GROUP, H5F_get_objects_cb, &olist);
    }

    /* Search through datatype IDs to count number of named datatypes, and put their
     * IDs on the object list */
    if( (max_index < 0 || (int)olist.list_index < max_index) && (types & H5F_OBJ_DATATYPE) ) {
        olist.obj_type = H5I_DATATYPE;
        (void)H5I_search(H5I_DATATYPE, H5F_get_objects_cb, &olist);
    }

    /* Search through attribute IDs to count number of attributes, and put their
     * IDs on the object list */
    if( (max_index < 0 || (int)olist.list_index < max_index) && (types & H5F_OBJ_ATTR) ) {
        olist.obj_type = H5I_ATTR;
        (void)H5I_search(H5I_ATTR, H5F_get_objects_cb, &olist);
    }

    /* Set the number of objects currently open */
    ret_value = obj_id_count;

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5F_get_objects_cb
 *
 * Purpose:	H5F_get_objects' callback function.  It verifies if an
 * 		object is in the file, and either count it or put its ID
 *		on the list.
 *
 * Programmer:  Raymond Lu
 *              Wednesday, Dec 5, 2001
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
static int
H5F_get_objects_cb(void *obj_ptr, hid_t obj_id, void *key)
{
    H5F_olist_t *olist = (H5F_olist_t *)key;    /* Alias for search info */
    int      ret_value = FALSE;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5F_get_objects_cb)

    assert(obj_ptr);
    assert(olist);

    /* Count file IDs */
    if(olist->obj_type == H5I_FILE) {
        if((olist->file_info.local &&
                        (!olist->file_info.ptr.file || (olist->file_info.ptr.file && (H5F_t*)obj_ptr == olist->file_info.ptr.file) ))
                ||  (!olist->file_info.local &&
                        ( !olist->file_info.ptr.shared || (olist->file_info.ptr.shared && ((H5F_t*)obj_ptr)->shared == olist->file_info.ptr.shared) ))) {
            /* Add the object's ID to the ID list, if appropriate */
            if(olist->obj_id_list) {
                olist->obj_id_list[olist->list_index] = obj_id;
		olist->list_index++;
	    }

            /* Increment the number of open objects */
	    if(olist->obj_id_count)
	    	(*olist->obj_id_count)++;

            /* Check if we've filled up the array */
            if(olist->max_index>=0 && (int)olist->list_index>=olist->max_index)
                HGOTO_DONE(TRUE)  /* Indicate that the iterator should stop */
	}
    } else { /* either count opened object IDs or put the IDs on the list */
        H5O_loc_t *oloc;        /* Group entry info for object */

    	switch(olist->obj_type) {
	    case H5I_ATTR:
	        oloc = H5A_oloc((H5A_t*)obj_ptr);
                break;
	    case H5I_GROUP:
	        oloc = H5G_oloc((H5G_t*)obj_ptr);
                break;
	    case H5I_DATASET:
	        oloc = H5D_oloc((H5D_t*)obj_ptr);
		break;
	    case H5I_DATATYPE:
                if(H5T_is_named((H5T_t*)obj_ptr)==TRUE)
                    oloc = H5T_oloc((H5T_t*)obj_ptr);
                else
                    oloc = NULL;
		break;
            default:
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "unknown data object")
	}

        if((olist->file_info.local &&
                    ( (!olist->file_info.ptr.file && olist->obj_type==H5I_DATATYPE && H5T_is_immutable((H5T_t*)obj_ptr)==FALSE)
                            || (!olist->file_info.ptr.file && olist->obj_type!=H5I_DATATYPE)
                            || (oloc && oloc->file == olist->file_info.ptr.file) ))
                || (!olist->file_info.local &&
                    ((!olist->file_info.ptr.shared && olist->obj_type==H5I_DATATYPE && H5T_is_immutable((H5T_t*)obj_ptr)==FALSE)
                            || (!olist->file_info.ptr.shared && olist->obj_type!=H5I_DATATYPE)
                            || (oloc && oloc->file && oloc->file->shared == olist->file_info.ptr.shared) ))) {
            /* Add the object's ID to the ID list, if appropriate */
            if(olist->obj_id_list) {
            	olist->obj_id_list[olist->list_index] = obj_id;
		olist->list_index++;
	    }

            /* Increment the number of open objects */
	    if(olist->obj_id_count)
            	(*olist->obj_id_count)++;

            /* Check if we've filled up the array */
            if(olist->max_index>=0 && (int)olist->list_index>=olist->max_index)
                HGOTO_DONE(TRUE)  /* Indicate that the iterator should stop */
    	}
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5Fget_vfd_handle
 *
 * Purpose:     Returns a pointer to the file handle of the low-level file
 *              driver.
 *
 * Return:      Success:        non-negative value.
 *
 *              Failture:       negative.
 *
 * Programmer:  Raymond Lu
 *              Sep. 16, 2002
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_vfd_handle(hid_t file_id, hid_t fapl, void **file_handle)
{
    H5F_t               *file=NULL;
    herr_t              ret_value;

    FUNC_ENTER_API(H5Fget_vfd_handle, FAIL)
    H5TRACE3("e", "ii**x", file_id, fapl, file_handle);

    /* Check args */
    assert(file_handle);
    if(NULL==(file=H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file id")

    ret_value=H5F_get_vfd_handle(file, fapl, file_handle);

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5F_get_vfd_handle
 *
 * Purpose:     Returns a pointer to the file handle of the low-level file
 *              driver.  This is the private function for H5Fget_vfd_handle.
 *
 * Return:      Success:        Non-negative.
 *
 *              Failture:       negative.
 *
 * Programmer:  Raymond Lu
 *              Sep. 16, 2002
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_get_vfd_handle(const H5F_t *file, hid_t fapl, void**file_handle)
{
    herr_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5F_get_vfd_handle)

    assert(file_handle);
    if((ret_value=H5FD_get_vfd_handle(file->shared->lf, fapl, file_handle)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get file handle for file driver")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Fis_hdf5
 *
 * Purpose:	Check the file signature to detect an HDF5 file.
 *
 * Bugs:	This function is not robust: it only uses the default file
 *		driver when attempting to open the file when in fact it
 *		should use all known file drivers.
 *
 * Return:	Success:	TRUE/FALSE
 *
 *		Failure:	Negative
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *		Robb Matzke, 1999-08-02
 *		Rewritten to use the virtual file layer.
 *-------------------------------------------------------------------------
 */
htri_t
H5Fis_hdf5(const char *name)
{
    H5FD_t	*file = NULL;
    htri_t	ret_value;

    FUNC_ENTER_API(H5Fis_hdf5, FAIL)
    H5TRACE1("t", "*s", name);

    /* Check args and all the boring stuff. */
    if (!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "no file name specified")

    /* Open the file at the virtual file layer */
    if (NULL==(file=H5FD_open(name, H5F_ACC_RDONLY, H5P_FILE_ACCESS_DEFAULT, HADDR_UNDEF)))
	HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "unable to open file")

    /* The file is an hdf5 file if the hdf5 file signature can be found */
    ret_value = (HADDR_UNDEF!=H5F_locate_signature(file, H5AC_ind_dxpl_id));

done:
    /* Close the file */
    if (file)
        if(H5FD_close(file) < 0 && ret_value>=0)
            HDONE_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close file")

    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5F_new
 *
 * Purpose:	Creates a new file object and initializes it.  The
 *		H5Fopen and H5Fcreate functions then fill in various
 *		fields.	 If SHARED is a non-null pointer then the shared info
 *		to which it points has the reference count incremented.
 *		Otherwise a new, empty shared info struct is created and
 *		initialized with the specified file access property list.
 *
 * Errors:
 *
 * Return:	Success:	Ptr to a new file struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *		Raymond Lu, Oct 14, 2001
 *		Changed the file creation and access property list to the
 *		new generic property list.
 *
 *		J Mainzer, Mar 10, 2005
 *		Updated for the new metadata cache, and associated
 *		property list changes.
 *
 *		J Mainzer, Jun 30, 2005
 *		Added lf parameter so the shared->lf field can be
 *		initialized prior to the call to H5AC_create() if a
 *		new instance of H5F_file_t is created.  lf should be
 *		NULL if shared isn't, and vise versa.
 *
 *-------------------------------------------------------------------------
 */
static H5F_t *
H5F_new(H5F_file_t *shared, hid_t fcpl_id, hid_t fapl_id, H5FD_t *lf)
{
    H5F_t	*f = NULL, *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5F_new)

    if(NULL == (f = H5FL_CALLOC(H5F_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate top file structure")
    f->file_id = -1;

    if(shared) {
        HDassert(lf == NULL);
	f->shared = shared;
    } /* end if */
    else {
        unsigned super_vers = HDF5_SUPERBLOCK_VERSION_DEF;      /* Superblock version for file */
        H5P_genplist_t *plist;              /* Property list */

        HDassert(lf != NULL);
        if(NULL == (f->shared = H5FL_CALLOC(H5F_file_t)))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate shared file structure")
	f->shared->super_addr = HADDR_UNDEF;
	f->shared->base_addr = HADDR_UNDEF;
	f->shared->extension_addr = HADDR_UNDEF;
	f->shared->sohm_addr = HADDR_UNDEF;
	f->shared->sohm_vers = HDF5_SHAREDHEADER_VERSION;
	f->shared->driver_addr = HADDR_UNDEF;
        f->shared->lf = lf;

	/*
	 * Copy the file creation and file access property lists into the
	 * new file handle.  We do this early because some values might need
	 * to change as the file is being opened.
	 */
        if(NULL == (plist = H5I_object(fcpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not property list")
        f->shared->fcpl_id = H5P_copy_plist(plist);

        /* Get the FCPL values to cache */
        if(H5P_get(plist, H5F_CRT_ADDR_BYTE_NUM_NAME, &f->shared->sizeof_addr) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get byte number for address")
        if(H5P_get(plist, H5F_CRT_OBJ_BYTE_NUM_NAME, &f->shared->sizeof_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get byte number for object size")
        if(H5P_get(plist, H5F_CRT_SYM_LEAF_NAME, &f->shared->sym_leaf_k) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get byte number for object size")
        if(H5P_get(plist, H5F_CRT_BTREE_RANK_NAME, &f->shared->btree_k[0]) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "unable to get rank for btree internal nodes")
        if(H5P_get(plist, H5F_CRT_SHMSG_NINDEXES_NAME, &f->shared->sohm_nindexes)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get number of SOHM indexes")
        HDassert(f->shared->sohm_nindexes < 255);

        /* Get the FAPL values to cache */
        if(NULL == (plist = H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not file access property list")
        if(H5P_get(plist, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, &(f->shared->mdc_initCacheCfg)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get initial metadata cache resize config")
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_ELMT_SIZE_NAME, &(f->shared->rdcc_nelmts)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get data cache element size")
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, &(f->shared->rdcc_nbytes)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get data cache cache size")
        if(H5P_get(plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, &(f->shared->rdcc_w0)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get preempt read chunk")
        if(H5P_get(plist, H5F_ACS_ALIGN_THRHD_NAME, &(f->shared->threshold)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get alignment threshold")
        if(H5P_get(plist, H5F_ACS_ALIGN_NAME, &(f->shared->alignment)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get alignment")
        if(H5P_get(plist, H5F_ACS_GARBG_COLCT_REF_NAME,&(f->shared->gc_ref)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get garbage collect reference")
        if(H5P_get(plist, H5F_ACS_SIEVE_BUF_SIZE_NAME, &(f->shared->sieve_buf_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get sieve buffer size")
        if(H5P_get(plist, H5F_ACS_LATEST_FORMAT_NAME, &(f->shared->latest_format)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get 'latest format' flag")

        /* Get the VFD values to cache */
        f->shared->maxaddr = H5FD_get_maxaddr(lf);
        if(!H5F_addr_defined(f->shared->maxaddr))
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad maximum address from VFD")

        /* Bump superblock version if we are to use the latest version of the format */
        if(f->shared->latest_format)
            super_vers = HDF5_SUPERBLOCK_VERSION_LATEST;
        /* Bump superblock version to create superblock extension for SOHM info */
        else if(f->shared->sohm_nindexes > 0)
            super_vers = HDF5_SUPERBLOCK_VERSION_2;
        /* Check for non-default indexed storage B-tree internal 'K' value
         * and set the version # of the superblock to 1 if it is a non-default
         * value.
         */
        else if(f->shared->btree_k[H5B_ISTORE_ID] != HDF5_BTREE_ISTORE_IK_DEF)
            super_vers = HDF5_SUPERBLOCK_VERSION_1;

        /* If a newer superblock version is required, set it here */
        if(super_vers != HDF5_SUPERBLOCK_VERSION_DEF) {
            H5P_genplist_t *c_plist;              /* Property list */

            if(NULL == (c_plist = H5I_object(f->shared->fcpl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not property list")
            if(H5P_set(c_plist, H5F_CRT_SUPER_VERS_NAME, &super_vers) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, NULL, "unable to set superblock version")
        } /* end if */

	/*
	 * Create a metadata cache with the specified number of elements.
	 * The cache might be created with a different number of elements and
	 * the access property list should be updated to reflect that.
	 */
	if(SUCCEED != H5AC_create(f, &(f->shared->mdc_initCacheCfg)))
	    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create metadata cache")

        /* Create the file's "open object" information */
        if(H5FO_create(f) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create open object data structure")

        /* Add new "shared" struct to list of open files */
        if(H5F_sfile_add(f->shared) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to append to list of open files")
    } /* end else */

    f->shared->nrefs++;

    /* Create the file's "top open object" information */
    if(H5FO_top_create(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create open object data structure")

    /* Set return value */
    ret_value = f;

done:
    if(!ret_value && f) {
	if(!shared)
            H5FL_FREE(H5F_file_t, f->shared);
	H5FL_FREE(H5F_t, f);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_new() */


/*-------------------------------------------------------------------------
 * Function:	H5F_dest
 *
 * Purpose:	Destroys a file structure.  This function flushes the cache
 *		but doesn't do any other cleanup other than freeing memory
 *		for the file struct.  The shared info for the file is freed
 *		only when its reference count reaches zero.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_dest(H5F_t *f, hid_t dxpl_id)
{
    herr_t	   ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5F_dest)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    if(1 == f->shared->nrefs) {
        /* Flush at this point since the file will be closed */
        /* (Only try to flush here if the file structure was successfully
         *      initialized (i.e., the file struct is being shutdown in an
         *      orderly manner with the 'closing' flag set)
         */
        if(f->closing) {
#if H5AC_DUMP_STATS_ON_CLOSE
            /* Dump debugging info */
            H5AC_stats(f);
#endif /* H5AC_DUMP_STATS_ON_CLOSE */

            /* Flush and invalidate all caches */
            if(H5F_flush(f, dxpl_id, H5F_SCOPE_LOCAL, H5F_FLUSH_INVALIDATE | H5F_FLUSH_CLOSING) < 0)
                /* Push error, but keep going*/
                HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
        } /* end if */

        /* Remove shared file struct from list of open files */
        if(H5F_sfile_remove(f->shared) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")

        /*
         * Do not close the root group since we didn't count it, but free
         * the memory associated with it.
         */
        if(f->shared->root_grp) {
            /* Free the ID to name buffer */
            if(H5G_free_grp_name(f->shared->root_grp) < 0)
                /* Push error, but keep going*/
                HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")

            /* Free the memory for the root group */
            if(H5G_free(f->shared->root_grp) < 0)
                /* Push error, but keep going*/
                HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")
            f->shared->root_grp = NULL;
        } /* end if */
        if(H5AC_dest(f, dxpl_id))
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")
        if(H5FO_dest(f) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")
        f->shared->cwfs = H5MM_xfree(f->shared->cwfs);
        if(H5G_node_close(f) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")

        /* Destroy file creation properties */
        if(H5I_GENPROP_LST != H5I_get_type(f->shared->fcpl_id))
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a property list")
        if(H5I_dec_ref(f->shared->fcpl_id) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close property list")

        /* Close low-level file */
        if(H5FD_close(f->shared->lf) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "problems closing file")

        /* Destroy shared file struct */
        f->shared = H5FL_FREE(H5F_file_t,f->shared);

    } else if(f->shared->nrefs > 0) {
        /*
         * There are other references to the shared part of the file.
         * Only decrement the reference count.
         */
        --f->shared->nrefs;
    }

    /* Free the non-shared part of the file */
    f->name = H5MM_xfree(f->name);
    f->mtab.child = H5MM_xfree(f->mtab.child);
    f->mtab.nalloc = 0;
    if(H5FO_top_dest(f) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "problems closing file")
    f->shared = NULL;
    H5FL_FREE(H5F_t, f);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5F_open
 *
 * Purpose:	Opens (or creates) a file.  This function understands the
 *		following flags which are similar in nature to the Posix
 *		open(2) flags.
 *
 *		H5F_ACC_RDWR:	Open with read/write access. If the file is
 *				currently open for read-only access then it
 *				will be reopened. Absence of this flag
 *				implies read-only access.
 *
 *		H5F_ACC_CREAT:	Create a new file if it doesn't exist yet.
 *				The permissions are 0666 bit-wise AND with
 *				the current umask.  H5F_ACC_WRITE must also
 *				be specified.
 *
 *		H5F_ACC_EXCL:	This flag causes H5F_open() to fail if the
 *				file already exists.
 *
 *		H5F_ACC_TRUNC:	The file is truncated and a new HDF5 superblock
 *				is written.  This operation will fail if the
 *				file is already open.
 *
 *		Unlinking the file name from the group directed graph while
 *		the file is opened causes the file to continue to exist but
 *		one will not be able to upgrade the file from read-only
 *		access to read-write access by reopening it. Disk resources
 *		for the file are released when all handles to the file are
 *		closed. NOTE: This paragraph probably only applies to Unix;
 *		deleting the file name in other OS's has undefined results.
 *
 *		The CREATE_PARMS argument is optional.	A null pointer will
 *		cause the default file creation parameters to be used.
 *
 *		The ACCESS_PARMS argument is optional.  A null pointer will
 *		cause the default file access parameters to be used.
 *
 * Return:	Success:	A new file pointer.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, September 23, 1997
 *
 * Modifications:
 *		Albert Cheng, 1998-02-05
 *		Added the access_parms argument to pass down access template
 *		information.
 *
 * 		Robb Matzke, 1998-02-18
 *		The H5F_access_t changed to allow more generality.  The low
 *		level driver is part of the file access template so the TYPE
 *		argument has been removed.
 *
 * 		Robb Matzke, 1999-08-02
 *		Rewritten to use the virtual file layer.
 *
 * 		Robb Matzke, 1999-08-16
 *		Added decoding of file driver information block, which uses a
 *		formerly reserved address slot in the boot block in order to
 *		be compatible with previous versions of the file format.
 *
 * 		Robb Matzke, 1999-08-20
 *		Optimizations for opening a file. If the driver can't
 *		determine when two file handles refer to the same file then
 *		we open the file in one step.  Otherwise if the first attempt
 *		to open the file fails then we skip the second attempt if the
 *		arguments would be the same.
 *
 *		Raymond Lu, 2001-10-14
 *		Changed the file creation and access property lists to the
 *		new generic property list.
 *
 *		Bill Wendling, 2003-03-18
 *		Modified H5F_flush call to take one flag instead of
 *		multiple Boolean flags.
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5F_open(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id)
{
    H5F_t              *file = NULL;        /*the success return value      */
    H5F_file_t         *shared = NULL;      /*shared part of `file'         */
    H5FD_t             *lf = NULL;          /*file driver part of `shared'  */
    unsigned            tent_flags;         /*tentative flags               */
    H5FD_class_t       *drvr;               /*file driver class info        */
    H5P_genplist_t     *a_plist;            /*file access property list     */
    H5F_close_degree_t  fc_degree;          /*file close degree             */
    H5F_t              *ret_value;          /*actual return value           */

    FUNC_ENTER_NOAPI(H5F_open, NULL)

    /*
     * If the driver has a `cmp' method then the driver is capable of
     * determining when two file handles refer to the same file and the
     * library can insure that when the application opens a file twice
     * that the two handles coordinate their operations appropriately.
     * Otherwise it is the application's responsibility to never open the
     * same file more than once at a time.
     */
    if((drvr = H5FD_get_class(fapl_id)) == NULL)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "unable to retrieve VFL class")

    /*
     * Opening a file is a two step process. First we try to open the
     * file in a way which doesn't affect its state (like not truncating
     * or creating it) so we can compare it with files that are already
     * open. If that fails then we try again with the full set of flags
     * (only if they're different than the original failed attempt).
     * However, if the file driver can't distinquish between files then
     * there's no reason to open the file tentatively because it's the
     * application's responsibility to prevent this situation (there's no
     * way for us to detect it here anyway).
     */
    if(drvr->cmp)
	tent_flags = flags & ~(H5F_ACC_CREAT|H5F_ACC_TRUNC|H5F_ACC_EXCL);
    else
	tent_flags = flags;

    if(NULL == (lf = H5FD_open(name, tent_flags, fapl_id, HADDR_UNDEF))) {
	if(tent_flags == flags)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")
        H5E_clear_stack(NULL);
	tent_flags = flags;
	if(NULL == (lf = H5FD_open(name, tent_flags, fapl_id, HADDR_UNDEF)))
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")
    } /* end if */

    /* Is the file already open? */
    if((shared = H5F_sfile_search(lf)) != NULL) {
	/*
	 * The file is already open, so use that one instead of the one we
	 * just opened. We only one one H5FD_t* per file so one doesn't
	 * confuse the other.  But fail if this request was to truncate the
	 * file (since we can't do that while the file is open), or if the
	 * request was to create a non-existent file (since the file already
	 * exists), or if the new request adds write access (since the
	 * readers don't expect the file to change under them).
	 */
	if(H5FD_close(lf) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to close low-level file info")
	if(flags & H5F_ACC_TRUNC)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to truncate a file which is already open")
	if(flags & H5F_ACC_EXCL)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file exists")
	if((flags & H5F_ACC_RDWR) && 0 == (shared->flags & H5F_ACC_RDWR))
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file is already open for read-only")

        /* Allocate new "high-level" file struct */
        if((file = H5F_new(shared, fcpl_id, fapl_id, NULL)) == NULL)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create new file object")
    } /* end if */
    else {
        /* Check if tentative open was good enough */
        if(flags != tent_flags) {
            /*
             * This file is not yet open by the library and the flags we used to
             * open it are different than the desired flags. Close the tentative
             * file and open it for real.
             */
            if(H5FD_close(lf) < 0) {
                file = NULL; /*to prevent destruction of wrong file*/
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to close low-level file info")
            } /* end if */
            if(NULL == (lf = H5FD_open(name, flags, fapl_id, HADDR_UNDEF))) {
                file = NULL; /*to prevent destruction of wrong file*/
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")
            } /* end if */
        } /* end if */

        if(NULL == (file = H5F_new(NULL, fcpl_id, fapl_id, lf)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create new file object")
        file->shared->flags = flags;
    } /* end else */

    /* Short cuts */
    shared = file->shared;
    lf = shared->lf;

    /*
     * The intent at the top level file struct are not necessarily the same as
     * the flags at the bottom.	 The top level describes how the file can be
     * accessed through the HDF5 library.  The bottom level describes how the
     * file can be accessed through the C library.
     */
    file->intent = flags;
    file->name = H5MM_xstrdup(name);

    /* Get the file access property list, for future queries */
    if(NULL == (a_plist = H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not file access property list")

    /* This step is for h5repart tool only. If user wants to change file driver from
     * family to sec2 while using h5repart, this private property should be set so that
     * in the later step, the library can ignore the family driver information saved
     * in the superblock.
     */
    if(H5P_exist_plist(a_plist, H5F_ACS_FAMILY_TO_SEC2_NAME) > 0) {
        if(H5P_get(a_plist, H5F_ACS_FAMILY_TO_SEC2_NAME, &shared->fam_to_sec2) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property of changing family to sec2")
    } /* end if */
    else
        shared->fam_to_sec2 = FALSE;

    /*
     * Read or write the file superblock, depending on whether the file is
     * empty or not.
     */
    if(0 == H5FD_get_eof(lf) && (flags & H5F_ACC_RDWR)) {
        /*
         * We've just opened a fresh new file (or truncated one). We need
         * to create & write the superblock.
         */

        /* Initialize information about the superblock and allocate space for it */
        /* (Writes superblock extension messages, if there are any) */
        if(H5F_super_init(file, dxpl_id) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to allocate file superblock")

        /* Create and open the root group */
        /* (This must be after the space for the superblock is allocated in
         *      the file, since the superblock must be at offset 0)
         */
        if(H5G_mkroot(file, dxpl_id, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create/open root group")

        /* Write the superblock to the file */
        /* (This must be after the root group is created, since the root
         *      group's symbol table entry is part of the superblock)
         */
        if(H5F_super_write(file, dxpl_id) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to write file superblock")
    } else if (1 == shared->nrefs) {
        H5G_loc_t           root_loc;           /*root location                 */
        H5O_loc_t           root_oloc;          /*root object location          */
        H5G_name_t          root_path;          /*root group hier. path         */

        /* Set up root location to fill in */
        root_loc.oloc = &root_oloc;
        root_loc.path = &root_path;
        H5G_loc_reset(&root_loc);

	/* Read the superblock if it hasn't been read before. */
        if(H5F_super_read(file, dxpl_id, &root_loc) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_READERROR, NULL, "unable to read superblock")

	/* Open the root group */
	if(H5G_mkroot(file, dxpl_id, &root_loc) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to read root group")
    } /* end if */

    /*
     * Decide the file close degree.  If it's the first time to open the
     * file, set the degree to access property list value; if it's the
     * second time or later, verify the access property list value matches
     * the degree in shared file structure.
     */
    if(H5P_get(a_plist, H5F_ACS_CLOSE_DEGREE_NAME, &fc_degree) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get file close degree")

    if(shared->nrefs == 1) {
        if(fc_degree == H5F_CLOSE_DEFAULT)
            shared->fc_degree = lf->cls->fc_degree;
        else
            shared->fc_degree = fc_degree;
    } else if(shared->nrefs > 1) {
        if(fc_degree == H5F_CLOSE_DEFAULT && shared->fc_degree != lf->cls->fc_degree)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "file close degree doesn't match")
        if(fc_degree != H5F_CLOSE_DEFAULT && fc_degree != shared->fc_degree)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "file close degree doesn't match")
    } /* end if */

    /* Success */
    ret_value = file;

done:
    if(!ret_value && file)
        if(H5F_dest(file, dxpl_id) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_open() */


/*-------------------------------------------------------------------------
 * Function:	H5Fcreate
 *
 * Purpose:	This is the primary function for creating HDF5 files . The
 *		flags parameter determines whether an existing file will be
 *		overwritten or not.  All newly created files are opened for
 *		both reading and writing.  All flags may be combined with the
 *		bit-wise OR operator (`|') to change the behavior of the file
 *		create call.
 *
 *		The more complex behaviors of a file's creation and access
 *		are controlled through the file-creation and file-access
 *		property lists.  The value of H5P_DEFAULT for a template
 *		value indicates that the library should use the default
 *		values for the appropriate template.
 *
 * See also:	H5Fpublic.h for the list of supported flags. H5Ppublic.h for
 * 		the list of file creation and file access properties.
 *
 * Return:	Success:	A file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 * 		Robb Matzke, 1997-07-18
 *		File struct creation and destruction is through H5F_new() and
 *		H5F_dest(). Writing the root symbol table entry is done with
 *		H5G_encode().
 *
 *  		Robb Matzke, 1997-08-29
 *		Moved creation of the boot block to H5F_flush().
 *
 *  		Robb Matzke, 1997-09-23
 *		Most of the work is now done by H5F_open() since H5Fcreate()
 *		and H5Fopen() originally contained almost identical code.
 *
 * 		Robb Matzke, 1998-02-18
 *		Better error checking for the creation and access property
 *		lists. It used to be possible to swap the two and core the
 *		library.  Also, zero is no longer valid as a default property
 *		list; one must use H5P_DEFAULT instead.
 *
 * 		Robb Matzke, 1999-08-02
 *		The file creation and file access property lists are passed
 *		to the H5F_open() as object IDs.
 *
 *		Raymond Lu, 2001-10-14
 *              Changed the file creation and access property list to the
 * 		new generic property list.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fcreate(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    H5F_t	*new_file = NULL;	/*file struct for new file	*/
    hid_t	ret_value;	        /*return value			*/

    FUNC_ENTER_API(H5Fcreate, FAIL)
    H5TRACE4("i", "*sIuii", filename, flags, fcpl_id, fapl_id);

    /* Check/fix arguments */
    if (!filename || !*filename)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")
    if (flags & ~(H5F_ACC_EXCL|H5F_ACC_TRUNC|H5F_ACC_DEBUG))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags")
    if ((flags & H5F_ACC_EXCL) && (flags & H5F_ACC_TRUNC))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "mutually exclusive flags for file creation")

    /* Check file creation property list */
    if(H5P_DEFAULT == fcpl_id)
        fcpl_id = H5P_FILE_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fcpl_id, H5P_FILE_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file create property list")

    /* Check the file access property list */
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /*
     * Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if (0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
	flags |= H5F_ACC_EXCL;	 /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /*
     * Create a new file or truncate an existing file.
     */
    if(NULL == (new_file = H5F_open(filename, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

    /* Get an atom for the file */
    if((ret_value = H5I_register(H5I_FILE, new_file)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file")

    /* Keep this ID in file object structure */
    new_file->file_id = ret_value;

done:
    if(ret_value < 0 && new_file)
        if(H5F_close(new_file) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

    FUNC_LEAVE_API(ret_value)
} /* end H5Fcreate() */


/*-------------------------------------------------------------------------
 * Function:	H5Fopen
 *
 * Purpose:	This is the primary function for accessing existing HDF5
 *		files.  The FLAGS argument determines whether writing to an
 *		existing file will be allowed or not.  All flags may be
 *		combined with the bit-wise OR operator (`|') to change the
 *		behavior of the file open call.  The more complex behaviors
 *		of a file's access are controlled through the file-access
 *		property list.
 *
 * See Also:	H5Fpublic.h for a list of possible values for FLAGS.
 *
 * Return:	Success:	A file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *	  	Robb Matzke, 1997-07-18
 *		File struct creation and destruction is through H5F_new() and
 *		H5F_dest(). Reading the root symbol table entry is done with
 *		H5G_decode().
 *
 *  		Robb Matzke, 1997-09-23
 *		Most of the work is now done by H5F_open() since H5Fcreate()
 *		and H5Fopen() originally contained almost identical code.
 *
 *	 	Robb Matzke, 1998-02-18
 *		Added better error checking for the flags and the file access
 *		property list.  It used to be possible to make the library
 *		dump core by passing an object ID that was not a file access
 *		property list.
 *
 * 		Robb Matzke, 1999-08-02
 *		The file access property list is passed to the H5F_open() as
 *		object IDs.
 *-------------------------------------------------------------------------
 */
hid_t
H5Fopen(const char *filename, unsigned flags, hid_t fapl_id)
{
    H5F_t	*new_file = NULL;	/*file struct for new file	*/
    hid_t	ret_value;	        /*return value			*/

    FUNC_ENTER_API(H5Fopen, FAIL)
    H5TRACE3("i", "*sIui", filename, flags, fapl_id);

    /* Check/fix arguments. */
    if(!filename || !*filename)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")
    if((flags & ~H5F_ACC_PUBLIC_FLAGS) ||
            (flags & H5F_ACC_TRUNC) || (flags & H5F_ACC_EXCL))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file open flags")
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /* Open the file */
    if(NULL == (new_file = H5F_open(filename, flags, H5P_FILE_CREATE_DEFAULT, fapl_id, H5AC_dxpl_id)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to open file")

    /* Get an atom for the file */
    if((ret_value = H5I_register(H5I_FILE, new_file)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

    /* Keep this ID in file object structure */
    new_file->file_id = ret_value;

done:
    if(ret_value < 0 && new_file && H5F_try_close(new_file) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

    FUNC_LEAVE_API(ret_value)
} /* end H5Fopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Fflush
 *
 * Purpose:	Flushes all outstanding buffers of a file to disk but does
 *		not remove them from the cache.  The OBJECT_ID can be a file,
 *		dataset, group, attribute, or named data type.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, August  6, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1998-10-16
 *		Added the `scope' argument.
 *
 *		Bill Wendling, 2003-03-18
 *		Modified H5F_flush call to take one flag instead of
 *		several Boolean flags.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fflush(hid_t object_id, H5F_scope_t scope)
{
    H5F_t	*f = NULL;
    H5G_t	*grp = NULL;
    H5T_t	*type = NULL;
    H5D_t	*dset = NULL;
    H5A_t	*attr = NULL;
    H5O_loc_t	*oloc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Fflush, FAIL)
    H5TRACE2("e", "iFs", object_id, scope);

    switch(H5I_get_type(object_id)) {
        case H5I_FILE:
            if(NULL == (f = H5I_object(object_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
            break;

        case H5I_GROUP:
            if(NULL == (grp = H5I_object(object_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid group identifier")
            oloc = H5G_oloc(grp);
            break;

        case H5I_DATATYPE:
            if(NULL == (type = H5I_object(object_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid type identifier")
            oloc = H5T_oloc(type);
            break;

        case H5I_DATASET:
            if(NULL == (dset = H5I_object(object_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier")
            oloc = H5D_oloc(dset);
            break;

        case H5I_ATTR:
            if(NULL == (attr = H5I_object(object_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid attribute identifier")
            oloc = H5A_oloc(attr);
            break;

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
    if(H5F_flush(f, H5AC_dxpl_id, scope, H5F_FLUSH_NONE) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "flush failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fflush() */


/*-------------------------------------------------------------------------
 * Function:	H5F_flush
 *
 * Purpose:	Flushes (and optionally invalidates) cached data plus the
 *		file superblock.  If the logical file size field is zero
 *		then it is updated to be the length of the superblock.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 29 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_flush(H5F_t *f, hid_t dxpl_id, H5F_scope_t scope, unsigned flags)
{
    unsigned		nerrors = 0;    /* Errors from nested flushes */
    unsigned		i;              /* Index variable */
    unsigned int	H5AC_flags;     /* translated flags for H5AC_flush() */
    herr_t              ret_value;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5F_flush)

    /* Sanity check arguments */
    HDassert(f);

    /*
     * Nothing to do if the file is read only.	This determination is
     * made at the shared open(2) flags level, implying that opening a
     * file twice, once for read-only and once for read-write, and then
     * calling H5F_flush() with the read-only handle, still causes data
     * to be flushed.
     */
    if(0 == (H5F_ACC_RDWR & f->shared->flags))
	HGOTO_DONE(SUCCEED)

    /* Flush other files, depending on scope */
    if(H5F_SCOPE_GLOBAL == scope) {
	while(f->mtab.parent)
            f = f->mtab.parent;

	scope = H5F_SCOPE_DOWN;
    } /* end while */
    if(H5F_SCOPE_DOWN == scope)
        for(i = 0; i < f->mtab.nmounts; i++)
            if(H5F_flush(f->mtab.child[i].file, dxpl_id, scope, flags) < 0)
                nerrors++;

    /* Flush any cached dataset storage raw data */
    if(H5D_flush(f, dxpl_id, flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush dataset cache")

    /* flush (and invalidate, if requested) the entire metadata cache */
    H5AC_flags = 0;
    if((flags & H5F_FLUSH_INVALIDATE) != 0 )
        H5AC_flags |= H5AC__FLUSH_INVALIDATE_FLAG;
    if(H5AC_flush(f, dxpl_id, H5AC_flags) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush metadata cache")

    /*
     * If we are invalidating everything (which only happens just before
     * the file closes), release the unused portion of the metadata and
     * "small data" blocks back to the free lists in the file.
     */
    if(flags & H5F_FLUSH_INVALIDATE) {
        if(H5FD_aggr_reset(f->shared->lf, &(f->shared->lf->meta_aggr), dxpl_id) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't reset metadata block")

        if(H5FD_aggr_reset(f->shared->lf, &(f->shared->lf->sdata_aggr), dxpl_id) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't reset 'small data' block")
    } /* end if */

    /* Write the superblock to disk */
    if(H5F_super_write(f, dxpl_id) != SUCCEED)
        HGOTO_ERROR(H5E_CACHE, H5E_WRITEERROR, FAIL, "unable to write superblock to file")

    /* Flush file buffers to disk. */
    if(H5FD_flush(f->shared->lf, dxpl_id, (unsigned)((flags & H5F_FLUSH_CLOSING) > 0)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "low level flush failed")

    /* Check flush errors for children - errors are already on the stack */
    ret_value = (nerrors ? FAIL : SUCCEED);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5F_close
 *
 * Purpose:	Closes a file or causes the close operation to be pended.
 *		This function is called two ways: from the API it gets called
 *		by H5Fclose->H5I_dec_ref->H5F_close when H5I_dec_ref()
 *		decrements the file ID reference count to zero.  The file ID
 *		is removed from the H5I_FILE group by H5I_dec_ref() just
 *		before H5F_close() is called. If there are open object
 *		headers then the close is pended by moving the file to the
 *		H5I_FILE_CLOSING ID group (the f->closing contains the ID
 *		assigned to file).
 *
 *		This function is also called directly from H5O_close() when
 *		the last object header is closed for the file and the file
 *		has a pending close.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, September 23, 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_close(H5F_t *f)
{
    herr_t	        ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5F_close)

    /* Sanity check */
    HDassert(f);
    HDassert(f->file_id > 0);   /* This routine should only be called when a file ID's ref count drops to zero */

    /* Perform checks for "semi" file close degree here, since closing the
     * file is not allowed if there are objects still open */
    if(f->shared->fc_degree == H5F_CLOSE_SEMI) {
        unsigned nopen_files = 0;       /* Number of open files in file/mount hierarchy */
        unsigned nopen_objs = 0;        /* Number of open objects in file/mount hierarchy */

        /* Get the number of open objects and open files on this file/mount hierarchy */
        if(H5F_mount_count_ids(f, &nopen_files, &nopen_objs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_MOUNT, FAIL, "problem checking mount hierarchy")

        /* If there are no other file IDs open on this file/mount hier., but
         * there are still open objects, issue an error and bail out now,
         * without decrementing the file ID's reference count and triggering
         * a "real" attempt at closing the file */
        if(nopen_files == 1 && nopen_objs > 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file, there are objects still open")
    } /* end if */

    /* Reset the file ID for this file */
    f->file_id = -1;

    /* Attempt to close the file/mount hierarchy */
    if(H5F_try_close(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_close() */


/*-------------------------------------------------------------------------
 * Function:	H5F_try_close
 *
 * Purpose:	Attempts to close a file due to one of several actions:
 *                      - The reference count on the file ID dropped to zero
 *                      - The last open object was closed in the file
 *                      - The file was unmounted
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, July 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_try_close(H5F_t *f)
{
    unsigned            nopen_files = 0;        /* Number of open files in file/mount hierarchy */
    unsigned            nopen_objs = 0;         /* Number of open objects in file/mount hierarchy */
    herr_t	        ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5F_try_close)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    /* Check if this file is already in the process of closing */
    if(f->closing)
        HGOTO_DONE(SUCCEED)

    /* Get the number of open objects and open files on this file/mount hierarchy */
    if(H5F_mount_count_ids(f, &nopen_files, &nopen_objs) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_MOUNT, FAIL, "problem checking mount hierarchy")

    /*
     * Close file according to close degree:
     *
     *  H5F_CLOSE_WEAK:	if there are still objects open, wait until
     *			they are all closed.
     *  H5F_CLOSE_SEMI:	if there are still objects open, return fail;
     *			otherwise, close file.
     *  H5F_CLOSE_STRONG:	if there are still objects open, close them
     *			first, then close file.
     */
    switch(f->shared->fc_degree) {
        case H5F_CLOSE_WEAK:
            /*
             * If file or object IDS are still open then delay deletion of
             * resources until they have all been closed.  Flush all
             * caches and update the object header anyway so that failing to
             * close all objects isn't a major problem.
             */
            if((nopen_files + nopen_objs) > 0)
                HGOTO_DONE(SUCCEED)
            break;

        case H5F_CLOSE_SEMI:
            /* Can leave safely if file IDs are still open on this file */
            if(nopen_files > 0)
                HGOTO_DONE(SUCCEED)

            /* Sanity check: If close degree if "semi" and we have gotten this
             * far and there are objects left open, bail out now */
            HDassert(nopen_files == 0 && nopen_objs == 0);

            /* If we've gotten this far (ie. there are no open objects in the file), fall through to flush & close */
            break;

        case H5F_CLOSE_STRONG:
            /* If there are other open files in the hierarchy, we can leave now */
            if(nopen_files > 0)
                HGOTO_DONE(SUCCEED)

            /* If we've gotten this far (ie. there are no open file IDs in the file/mount hierarchy), fall through to flush & close */
            break;

        default:
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file, unknown file close degree")
    } /* end switch */

    /* Mark this file as closing (prevents re-entering file shutdown code below) */
    f->closing = TRUE;

    /* If the file close degree is "strong", close all the open objects in this file */
    if(f->shared->fc_degree == H5F_CLOSE_STRONG) {
        HDassert(nopen_files ==  0);

        /* Forced close of all opened objects in this file */
        if(f->nopen_objs > 0) {
            unsigned obj_count;     /* # of open objects */
            hid_t objs[128];        /* Array of objects to close */
            unsigned u;             /* Local index variable */

            /* Get the list of IDs of open dataset, group, & attribute objects */
            while((obj_count = H5F_get_obj_ids(f, H5F_OBJ_LOCAL|H5F_OBJ_DATASET|H5F_OBJ_GROUP|H5F_OBJ_ATTR, (int)(sizeof(objs)/sizeof(objs[0])), objs)) != 0) {
                /* Try to close all the open objects in this file */
                for(u = 0; u < obj_count; u++)
                    if(H5I_dec_ref(objs[u]) < 0)
                        HGOTO_ERROR(H5E_ATOM, H5E_CLOSEERROR, FAIL, "can't close object")
            } /* end while */

            /* Get the list of IDs of open named datatype objects */
            /* (Do this separately from the dataset & attribute IDs, because
             * they could be using one of the named datatypes and then the
             * open named datatype ID will get closed twice)
             */
            while((obj_count = H5F_get_obj_ids(f, H5F_OBJ_LOCAL|H5F_OBJ_DATATYPE, (int)(sizeof(objs)/sizeof(objs[0])), objs)) != 0) {
                /* Try to close all the open objects in this file */
                for(u = 0; u < obj_count; u++)
                    if(H5I_dec_ref(objs[u]) < 0)
                        HGOTO_ERROR(H5E_ATOM, H5E_CLOSEERROR, FAIL, "can't close object")
            } /* end while */
        } /* end if */
    } /* end if */

    /* Check if this is a child file in a mounting hierarchy & proceed up the
     * hierarchy if so.
     */
    if(f->mtab.parent)
        if(H5F_try_close(f->mtab.parent) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close parent file")

    /* Unmount and close each child before closing the current file. */
    if(H5F_close_mounts(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't unmount child files")

    /* Flush at this point since the file will be closed.  Don't invalidate
     * the cache, since this file might still be open using another handle.
     * However, make sure we flush in case that handle is read-only; its
     * copy of the cache needs to be clean.
     * Only try to flush the file if it was opened with write access.
     */
    if(f->intent&H5F_ACC_RDWR) {
        /* Flush and destroy all caches */
        if(H5F_flush(f, H5AC_dxpl_id, H5F_SCOPE_LOCAL, H5C__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    } /* end if */

    /*
     * Destroy the H5F_t struct and decrement the reference count for the
     * shared H5F_file_t struct. If the reference count for the H5F_file_t
     * struct reaches zero then destroy it also.
     */
    if(H5F_dest(f, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_try_close() */


/*-------------------------------------------------------------------------
 * Function:	H5Fclose
 *
 * Purpose:	This function closes the file specified by FILE_ID by
 *		flushing all data to storage, and terminating access to the
 *		file through FILE_ID.  If objects (e.g., datasets, groups,
 *		etc.) are open in the file then the underlying storage is not
 *		closed until those objects are closed; however, all data for
 *		the file and the open objects is flushed.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Saturday, February 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fclose(hid_t file_id)
{
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_API(H5Fclose, FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check/fix arguments. */
    if(H5I_FILE != H5I_get_type(file_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file ID")

    /*
     * Decrement reference count on atom.  When it reaches zero the file will
     * be closed.
     */
    if(H5I_dec_ref(file_id) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fclose() */


/*-------------------------------------------------------------------------
 * Function:	H5Freopen
 *
 * Purpose:	Reopen a file.  The new file handle which is returned points
 *		to the same file as the specified file handle.  Both handles
 *		share caches and other information.  The only difference
 *		between the handles is that the new handle is not mounted
 *		anywhere and no files are mounted on it.
 *
 * Return:	Success:	New file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, October 16, 1998
 *
 * Modifications:
 *              Quincey Koziol, May 14, 2002
 *              Keep old file's read/write intent in reopened file.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Freopen(hid_t file_id)
{
    H5F_t	*old_file = NULL;
    H5F_t	*new_file = NULL;
    hid_t	ret_value;

    FUNC_ENTER_API(H5Freopen, FAIL)
    H5TRACE1("i", "i", file_id);

    /* Check arguments */
    if(NULL == (old_file = H5I_object_verify(file_id, H5I_FILE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Get a new "top level" file struct, sharing the same "low level" file struct */
    if(NULL == (new_file = H5F_new(old_file->shared, H5P_FILE_CREATE_DEFAULT, H5P_FILE_ACCESS_DEFAULT, NULL)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to reopen file")

    /* Keep old file's read/write intent in new file */
    new_file->intent = old_file->intent;

    /* Duplicate old file's name */
    new_file->name = H5MM_xstrdup(old_file->name);

    if((ret_value = H5I_register(H5I_FILE, new_file)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

    /* Keep this ID in file object structure */
    new_file->file_id = ret_value;

done:
    if(ret_value < 0 && new_file)
	if(H5F_dest(new_file, H5AC_dxpl_id) < 0)
	    HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file")

    FUNC_LEAVE_API(ret_value)
} /* end H5Freopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_intent
 *
 * Purpose:	Public API to retrieve the file's 'intent' flags passed
 *              during H5Fopen()
 *
 * Return:	Non-negative on success/negative on failure
 *
 * Programmer:	James Laird
 *		August 23, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_intent(hid_t file_id, unsigned *intent_flags)
{
    H5F_t * file = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(H5Fget_intent, FAIL)
    H5TRACE2("e", "i*Iu", file_id, intent_flags);

    if(NULL == (file = H5I_object_verify(file_id, H5I_FILE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* If no intent flags were passed in, exit quietly */
    if(!intent_flags)
	HGOTO_DONE(SUCCEED)
    
    *intent_flags = H5F_get_intent(file);

    /* HDF5 uses some flags internally that users don't know about.
     * Simplify things for them so that they get one of H5F_ACC_RDWR
     * or H5F_ACC_RDONLY.
     */
    if(*intent_flags & H5F_ACC_RDWR)
        *intent_flags = H5F_ACC_RDWR;
    else
        *intent_flags = H5F_ACC_RDONLY;

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5F_get_intent
 *
 * Purpose:	Quick and dirty routine to retrieve the file's 'intent' flags
 *          (Mainly added to stop non-file routines from poking about in the
 *          H5F_t data structure)
 *
 * Return:	'intent' on success/abort on failure (shouldn't fail)
 *
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		September 29, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5F_get_intent(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_get_intent)

    HDassert(f);

    FUNC_LEAVE_NOAPI(f->intent)
} /* end H5F_get_intent() */


/*-------------------------------------------------------------------------
 * Function:	H5F_sizeof_addr
 *
 * Purpose:	Quick and dirty routine to retrieve the size of the file's size_t
 *          (Mainly added to stop non-file routines from poking about in the
 *          H5F_t data structure)
 *
 * Return:	'sizeof_addr' on success/abort on failure (shouldn't fail)
 *
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		September 29, 2000
 *
 * Modifications:
 *
 *		Raymond Lu, Oct 14, 2001
 *		Changed to generic property list.
 *
 *-------------------------------------------------------------------------
 */
size_t
H5F_sizeof_addr(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_sizeof_addr)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->sizeof_addr)
}


/*-------------------------------------------------------------------------
 * Function:	H5F_sizeof_size
 *
 * Purpose:	Quick and dirty routine to retrieve the size of the file's off_t
 *          (Mainly added to stop non-file routines from poking about in the
 *          H5F_t data structure)
 *
 * Return:	'sizeof_size' on success/abort on failure (shouldn't fail)
 *
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		September 29, 2000
 *
 *-------------------------------------------------------------------------
 */
size_t
H5F_sizeof_size(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_sizeof_size)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->sizeof_size)
} /* H5F_sizeof_size() */


/*-------------------------------------------------------------------------
 * Function:	H5F_sym_leaf_k
 *
 * Purpose:	Replaced a macro to retrieve the symbol table leaf size,
 *              now that the generic properties are being used to store
 *              the values.
 *
 * Return:	Success:	Non-negative, and the symbol table leaf size is
 *                              returned.
 *
 * 		Failure:	Negative (should not happen)
 *
 * Programmer:	Raymond Lu
 *		slu@ncsa.uiuc.edu
 *		Oct 14 2001
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5F_sym_leaf_k(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_sym_leaf_k)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->sym_leaf_k)
} /* end H5F_sym_leaf_k() */


/*-------------------------------------------------------------------------
 * Function:	H5F_Kvalue
 *
 * Purpose:	Replaced a macro to retrieve a B-tree key value for a certain
 *              type, now that the generic properties are being used to store
 *              the B-tree values.
 *
 * Return:	Success:	Non-negative, and the B-tree key value is
 *                              returned.
 *
 * 		Failure:	Negative (should not happen)
 *
 * Programmer:	Raymond Lu
 *		slu@ncsa.uiuc.edu
 *		Oct 14 2001
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5F_Kvalue(const H5F_t *f, const H5B_class_t *type)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_Kvalue)

    assert(f);
    assert(f->shared);
    assert(type);

    FUNC_LEAVE_NOAPI(f->shared->btree_k[type->id])
} /* end H5F_Kvalue() */


/*-------------------------------------------------------------------------
 * Function:	H5F_rdcc_nelmts
 *
 * Purpose:	Replaced a macro to retrieve the raw data cache number of elments,
 *              now that the generic properties are being used to store
 *              the values.
 *
 * Return:	Success:	Non-negative, and the raw data cache number of
 *                              of elemnts is returned.
 *
 * 		Failure:	Negative (should not happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jun  1 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5F_rdcc_nelmts(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_rdcc_nelmts)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->rdcc_nelmts)
} /* end H5F_rdcc_nelmts() */


/*-------------------------------------------------------------------------
 * Function:	H5F_rdcc_nbytes
 *
 * Purpose:	Replaced a macro to retrieve the raw data cache number of bytes,
 *              now that the generic properties are being used to store
 *              the values.
 *
 * Return:	Success:	Non-negative, and the raw data cache number of
 *                              of bytes is returned.
 *
 * 		Failure:	Negative (should not happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jun  1 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5F_rdcc_nbytes(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_rdcc_nbytes)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->rdcc_nbytes)
} /* end H5F_rdcc_nbytes() */


/*-------------------------------------------------------------------------
 * Function:	H5F_rdcc_w0
 *
 * Purpose:	Replaced a macro to retrieve the raw data cache 'w0' value
 *              now that the generic properties are being used to store
 *              the values.
 *
 * Return:	Success:	Non-negative, and the raw data cache 'w0' value
 *                              is returned.
 *
 * 		Failure:	Negative (should not happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jun  2 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
double
H5F_rdcc_w0(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_rdcc_w0)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->rdcc_w0)
} /* end H5F_rdcc_w0() */


/*-------------------------------------------------------------------------
 * Function:	H5F_has_feature
 *
 * Purpose:	Check if a file has a particular feature enabled
 *
 * Return:	Success:	Non-negative - TRUE or FALSE
 * 		Failure:	Negative (should not happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5F_has_feature(const H5F_t *f, unsigned feature)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_has_feature)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI((hbool_t)(f->shared->lf->feature_flags&feature))
} /* end H5F_has_feature() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_driver_id
 *
 * Purpose:	Quick and dirty routine to retrieve the file's 'driver_id' value
 *          (Mainly added to stop non-file routines from poking about in the
 *          H5F_t data structure)
 *
 * Return:	'driver_id' on success/abort on failure (shouldn't fail)
 *
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		October 10, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5F_get_driver_id(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_get_driver_id)

    assert(f);
    assert(f->shared);
    assert(f->shared->lf);

    FUNC_LEAVE_NOAPI(f->shared->lf->driver_id)
}


/*-------------------------------------------------------------------------
 * Function:	H5F_get_fileno
 *
 * Purpose:	Quick and dirty routine to retrieve the file's 'fileno' value
 *          (Mainly added to stop non-file routines from poking about in the
 *          H5F_t data structure)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		March 27, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_get_fileno(const H5F_t *f, unsigned long *filenum)
{
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5F_get_fileno, FAIL)

    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(filenum);

    /* Retrieve the file's serial number */
    if(H5FD_get_fileno(f->shared->lf, filenum) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_BADRANGE, FAIL, "can't retrieve fileno")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_fileno() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_id
 *
 * Purpose:	Get the file ID, incrementing it, or "resurrecting" it as
 *              appropriate.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Oct 29, 2003
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5F_get_id(H5F_t *file)
{
    hid_t       ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5F_get_id)

    HDassert(file);

    if(file->file_id == -1) {
        /* Get an atom for the file */
        if((file->file_id = H5I_register(H5I_FILE, file)) < 0)
	    HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file")
    } else {
        /* Increment reference count on atom. */
        if(H5I_inc_ref(file->file_id) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTSET, FAIL, "incrementing file ID failed")
    } /* end else */

    ret_value = file->file_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_id() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_base_addr
 *
 * Purpose:	Quick and dirty routine to retrieve the file's 'base_addr' value
 *          (Mainly added to stop non-file routines from poking about in the
 *          H5F_t data structure)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu <slu@ncsa.uiuc.edu>
 *		December 20, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5F_get_base_addr(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_get_base_addr)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->base_addr)
} /* end H5F_get_base_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_eoa
 *
 * Purpose:	Quick and dirty routine to retrieve the file's 'eoa' value
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		June 1, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5F_get_eoa(const H5F_t *f)
{
    haddr_t	ret_value;

    FUNC_ENTER_NOAPI(H5F_get_eoa, HADDR_UNDEF)

    assert(f);
    assert(f->shared);

    /* Dispatch to driver */
    if (HADDR_UNDEF==(ret_value=H5FD_get_eoa(f->shared->lf, H5FD_MEM_DEFAULT)))
	HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, HADDR_UNDEF, "driver get_eoa request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_eoa() */

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:	H5F_mpi_get_rank
 *
 * Purpose:	Retrieves the rank of an MPI process.
 *
 * Return:	Success:	The rank (non-negative)
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, January 30, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5F_mpi_get_rank(const H5F_t *f)
{
    int	ret_value;

    FUNC_ENTER_NOAPI(H5F_mpi_get_rank, FAIL)

    assert(f && f->shared);

    /* Dispatch to driver */
    if ((ret_value=H5FD_mpi_get_rank(f->shared->lf)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "driver get_rank request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_mpi_get_rank() */


/*-------------------------------------------------------------------------
 * Function:	H5F_mpi_get_comm
 *
 * Purpose:	Retrieves the file's communicator
 *
 * Return:	Success:	The communicator (non-negative)
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, January 30, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
MPI_Comm
H5F_mpi_get_comm(const H5F_t *f)
{
    MPI_Comm	ret_value;

    FUNC_ENTER_NOAPI(H5F_mpi_get_comm, MPI_COMM_NULL)

    assert(f && f->shared);

    /* Dispatch to driver */
    if ((ret_value=H5FD_mpi_get_comm(f->shared->lf))==MPI_COMM_NULL)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, MPI_COMM_NULL, "driver get_comm request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_mpi_get_comm() */


/*-------------------------------------------------------------------------
 * Function:    H5F_mpi_get_size
 *
 * Purpose:     Retrieves the size of an MPI process.
 *
 * Return:      Success:        The size (positive)
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              Friday, May 6, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5F_mpi_get_size(const H5F_t *f)
{
    int ret_value;

    FUNC_ENTER_NOAPI(H5F_mpi_get_size, FAIL)

    assert(f && f->shared);

    /* Dispatch to driver */
    if ((ret_value=H5FD_mpi_get_size(f->shared->lf)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "driver get_size request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_mpi_get_size() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:	H5F_grp_btree_shared
 *
 * Purpose:	Replaced a macro to retrieve the shared B-tree node info
 *              now that the generic properties are being used to store
 *              the values.
 *
 * Return:	Success:	Non-void, and the shared B-tree node info
 *                              is returned.
 *
 * 		Failure:	void (should not happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jul  5 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5RC_t *
H5F_grp_btree_shared(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_grp_btree_shared)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->grp_btree_shared)
} /* end H5F_grp_btree_shared() */


/*-------------------------------------------------------------------------
 * Function:	H5F_sieve_buf_size
 *
 * Purpose:	Replaced a macro to retrieve the dataset sieve buffer size
 *              now that the generic properties are being used to store
 *              the values.
 *
 * Return:	Success:	Non-void, and the dataset sieve buffer size
 *                              is returned.
 *
 * 		Failure:	void (should not happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jul  8 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5F_sieve_buf_size(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_sieve_buf_size)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->sieve_buf_size)
} /* end H5F_sieve_buf_size() */


/*-------------------------------------------------------------------------
 * Function:	H5F_gc_ref
 *
 * Purpose:	Replaced a macro to retrieve the "garbage collect
 *              references flag" now that the generic properties are being used
 *              to store the values.
 *
 * Return:	Success:	The "garbage collect references flag"
 *                              is returned.
 *
 * 		Failure:	(should not happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jul  8 2005
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5F_gc_ref(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_gc_ref)

    HDassert(f);
    HDassert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->gc_ref)
} /* end H5F_gc_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_fcpl
 *
 * Purpose:	Retrieve the value of a file's FCPL.
 *
 * Return:	Success:	The FCPL for the file.
 *
 * 		Failure:	? (should not happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 25 2005
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5F_get_fcpl(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_get_fcpl)

    assert(f);
    assert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->fcpl_id)
} /* end H5F_get_fcpl() */


/*-------------------------------------------------------------------------
 * Function:	H5F_use_latest_format
 *
 * Purpose:	Retrieve the 'use the latest version of the format' flag for
 *              the file.
 *
 * Return:	Success:	Non-negative, the 'use the latest format' flag
 *
 * 		Failure:	(can't happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct  2 2006
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5F_use_latest_format(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_use_latest_format)

    HDassert(f);
    HDassert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->latest_format)
} /* end H5F_use_latest_format() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_fc_degree
 *
 * Purpose:	Retrieve the 'file close degree' for the file.
 *
 * Return:	Success:	Non-negative, the 'file close degree'
 *
 * 		Failure:	(can't happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar  5 2007
 *
 *-------------------------------------------------------------------------
 */
H5F_close_degree_t
H5F_get_fc_degree(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_get_fc_degree)

    HDassert(f);
    HDassert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->fc_degree)
} /* end H5F_get_fc_degree() */


/*-------------------------------------------------------------------------
 * Function:	H5F_incr_nopen_objs
 *
 * Purpose:	Increment the number of open objects for a file.
 *
 * Return:	Success:	The number of open objects, after the increment
 *
 * 		Failure:	(can't happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar  6 2007
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5F_incr_nopen_objs(H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_incr_nopen_objs)

    HDassert(f);

    FUNC_LEAVE_NOAPI(++f->nopen_objs)
} /* end H5F_incr_nopen_objs() */


/*-------------------------------------------------------------------------
 * Function:	H5F_decr_nopen_objs
 *
 * Purpose:	Decrement the number of open objects for a file.
 *
 * Return:	Success:	The number of open objects, after the decrement
 *
 * 		Failure:	(can't happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar  6 2007
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5F_decr_nopen_objs(H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_decr_nopen_objs)

    HDassert(f);

    FUNC_LEAVE_NOAPI(--f->nopen_objs)
} /* end H5F_decr_nopen_objs() */


/*-------------------------------------------------------------------------
 * Function:	H5F_store_msg_crt_idx
 *
 * Purpose:	Retrieve the 'store message creation index' flag for the file.
 *
 * Return:	Success:	Non-negative, the 'store message creation index' flag
 *
 * 		Failure:	(can't happen)
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar  6 2007
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5F_store_msg_crt_idx(const H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_store_msg_crt_idx)

    HDassert(f);
    HDassert(f->shared);

    FUNC_LEAVE_NOAPI(f->shared->store_msg_crt_idx)
} /* end H5F_store_msg_crt_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5F_block_read
 *
 * Purpose:	Reads some data from a file/server/etc into a buffer.
 *		The data is contiguous.	 The address is relative to the base
 *		address for the file.
 *
 * Errors:
 *		IO	  READERROR	Low-level read failed.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 * Modifications:
 *		Albert Cheng, 1998-06-02
 *		Added XFER_MODE argument
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 * 		Robb Matzke, 1999-08-02
 *		Modified to use the virtual file layer. The data transfer
 *		property list is passed in by object ID since that's how the
 *		virtual file layer needs it.
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_read(const H5F_t *f, H5FD_mem_t type, haddr_t addr, size_t size, hid_t dxpl_id,
	       void *buf/*out*/)
{
    haddr_t		    abs_addr;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5F_block_read, FAIL)

    assert (f);
    assert (f->shared);
    assert(size<SIZET_MAX);
    assert (buf);

    /* convert the relative address to an absolute address */
    abs_addr = f->shared->base_addr + addr;

    /* Read the data */
    if(H5FD_read(f->shared->lf, type, dxpl_id, abs_addr, size, buf) < 0)
	HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file read failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5F_block_write
 *
 * Purpose:	Writes some data from memory to a file/server/etc.  The
 *		data is contiguous.  The address is relative to the base
 *		address.
 *
 * Errors:
 *		IO	  WRITEERROR	Low-level write failed.
 *		IO	  WRITEERROR	No write intent.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 * Modifications:
 *		Albert Cheng, 1998-06-02
 *		Added XFER_MODE argument
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 * 		Robb Matzke, 1999-08-02
 *		Modified to use the virtual file layer. The data transfer
 *		property list is passed in by object ID since that's how the
 *		virtual file layer needs it.
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_write(const H5F_t *f, H5FD_mem_t type, haddr_t addr, size_t size,
        hid_t dxpl_id, const void *buf)
{
    haddr_t		    abs_addr;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5F_block_write, FAIL)

    assert (f);
    assert (f->shared);
    assert (size<SIZET_MAX);
    assert (buf);

    if (0==(f->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "no write intent")

    /* Convert the relative address to an absolute address */
    abs_addr = f->shared->base_addr + addr;

    /* Write the data */
    if (H5FD_write(f->shared->lf, type, dxpl_id, abs_addr, size, buf))
	HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "file write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5F_addr_encode
 *
 * Purpose:	Encodes an address into the buffer pointed to by *PP and
 *		then increments the pointer to the first byte after the
 *		address.  An undefined value is stored as all 1's.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *-------------------------------------------------------------------------
 */
void
H5F_addr_encode(const H5F_t *f, uint8_t **pp/*in,out*/, haddr_t addr)
{
    unsigned		    i;

    assert(f);
    assert(pp && *pp);

    if (H5F_addr_defined(addr)) {
	for (i=0; i<H5F_SIZEOF_ADDR(f); i++) {
	    *(*pp)++ = (uint8_t)(addr & 0xff);
	    addr >>= 8;
	}
	assert("overflow" && 0 == addr);

    } else {
	for (i=0; i<H5F_SIZEOF_ADDR(f); i++)
	    *(*pp)++ = 0xff;
    }
}


/*-------------------------------------------------------------------------
 * Function:	H5F_addr_decode
 *
 * Purpose:	Decodes an address from the buffer pointed to by *PP and
 *		updates the pointer to point to the next byte after the
 *		address.
 *
 *		If the value read is all 1's then the address is returned
 *		with an undefined value.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_decode(const H5F_t *f, const uint8_t **pp/*in,out*/, haddr_t *addr_p/*out*/)
{
    unsigned		    i;
    haddr_t		    tmp;
    uint8_t		    c;
    hbool_t		    all_zero = TRUE;

    assert(f);
    assert(pp && *pp);
    assert(addr_p);

    *addr_p = 0;

    for (i=0; i<H5F_SIZEOF_ADDR(f); i++) {
	c = *(*pp)++;
	if (c != 0xff)
            all_zero = FALSE;

	if (i<sizeof(*addr_p)) {
	    tmp = c;
	    tmp <<= (i * 8);	/*use tmp to get casting right */
	    *addr_p |= tmp;
	} else if (!all_zero) {
	    assert(0 == **pp);	/*overflow */
	}
    }
    if (all_zero)
        *addr_p = HADDR_UNDEF;
}


/*-------------------------------------------------------------------------
 * Function:    H5Fget_freespace
 *
 * Purpose:     Retrieves the amount of free space in the file.
 *
 * Return:      Success:        Amount of free space for type
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Oct  6, 2003
 *
 *-------------------------------------------------------------------------
 */
hssize_t
H5Fget_freespace(hid_t file_id)
{
    H5F_t      *file=NULL;      /* File object for file ID */
    hssize_t    ret_value;      /* Return value */

    FUNC_ENTER_API(H5Fget_freespace, FAIL)
    H5TRACE1("Hs", "i", file_id);

    /* Check args */
    if(NULL == (file = H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Go get the actual amount of free space in the file */
    if((ret_value = H5FD_get_freespace(file->shared->lf)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_freespace() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_filesize
 *
 * Purpose:     Retrieves the file size of the HDF5 file. This function
 *              is called after an existing file is opened in order
 *		to learn the true size of the underlying file.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  David Pitt
 *              david.pitt@bigpond.com
 *              Apr 27, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_filesize(hid_t file_id, hsize_t *size)
{
    H5F_t      *file=NULL;      /* File object for file ID */
    herr_t     ret_value = SUCCEED;      /* Return value */
    haddr_t    eof;

    FUNC_ENTER_API(H5Fget_filesize, FAIL)
    H5TRACE2("e", "i*h", file_id, size);

    /* Check args */
    if(NULL==(file=H5I_object_verify(file_id, H5I_FILE)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Go get the actual file size */
    if((eof = H5FDget_eof(file->shared->lf))==HADDR_UNDEF)
         HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get file size")

    *size = (hsize_t)eof;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_filesize() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_config
 *
 * Purpose:     Retrieves the current automatic cache resize configuration
 *		from the metadata cache, and return it in *config_ptr.
 *
 *		Note that the version field of *config_Ptr must be correctly
 *		filled in by the caller.  This allows us to adapt for
 *		obsolete versions of the structure.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 * Modifications:
 *
 *		Reworked for the addition of the config_ptr parameter.
 *							JRM -- 4/7/05
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5Fget_mdc_config(hid_t file_id,
		  H5AC_cache_config_t *config_ptr)
{
    H5F_t      *file=NULL;      /* File object for file ID */
    herr_t     ret_value = SUCCEED;      /* Return value */
    herr_t     result;

    FUNC_ENTER_API(H5Fget_mdc_config, FAIL)
    H5TRACE2("e", "i*x", file_id, config_ptr);

    /* Check args */
    if ( NULL == (file = H5I_object_verify(file_id, H5I_FILE)) ) {

         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")
    }

    if ( ( NULL == config_ptr ) ||
         ( config_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION ) ) {

         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Bad config_ptr")
    }

    /* Go get the resize configuration */
    result = H5AC_get_cache_auto_resize_config(file->shared->cache, config_ptr);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5AC_get_cache_auto_resize_config() failed.");
    }

done:

    FUNC_LEAVE_API(ret_value)

} /* H5Fget_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:    H5Fset_mdc_config
 *
 * Purpose:     Sets the current metadata cache automatic resize
 *		configuration, using the contents of the instance of
 *		H5AC_cache_config_t pointed to by config_ptr.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5Fset_mdc_config(hid_t file_id,
		  H5AC_cache_config_t *config_ptr)
{
    H5F_t      *file=NULL;      /* File object for file ID */
    herr_t     ret_value = SUCCEED;      /* Return value */
    herr_t     result;

    FUNC_ENTER_API(H5Fset_mdc_config, FAIL)
    H5TRACE2("e", "i*x", file_id, config_ptr);

    /* Check args */
    if ( NULL == (file = H5I_object_verify(file_id, H5I_FILE)) ) {

         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")
    }

    /* set the resize configuration  */
    result = H5AC_set_cache_auto_resize_config(file->shared->cache, config_ptr);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "H5AC_set_cache_auto_resize_config() failed.");
    }

done:

    FUNC_LEAVE_API(ret_value)

} /* H5Fset_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_hit_rate
 *
 * Purpose:     Retrieves the current hit rate from the metadata cache.
 *		This rate is the overall hit rate since the last time
 *		the hit rate statistics were reset either manually or
 *		automatically.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5Fget_mdc_hit_rate(hid_t file_id,
                    double *hit_rate_ptr)
{
    H5F_t      *file=NULL;      /* File object for file ID */
    herr_t     ret_value = SUCCEED;      /* Return value */
    herr_t     result;

    FUNC_ENTER_API(H5Fget_mdc_hit_rate, FAIL)
    H5TRACE2("e", "i*d", file_id, hit_rate_ptr);

    /* Check args */
    if ( NULL == (file = H5I_object_verify(file_id, H5I_FILE)) ) {

         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")
    }

    if ( NULL == hit_rate_ptr ) {

         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL hit rate pointer")
    }

    /* Go get the current hit rate */
    result = H5AC_get_cache_hit_rate(file->shared->cache, hit_rate_ptr);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5AC_get_cache_hit_rate() failed.");
    }

done:

    FUNC_LEAVE_API(ret_value)

} /* H5Fget_mdc_hit_rate() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_size
 *
 * Purpose:     Retrieves the maximum size, minimum clean size, current
 *		size, and current number of entries from the metadata
 *		cache associated with the specified file.  If any of
 *		the ptr parameters are NULL, the associated datum is
 *		not returned.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5Fget_mdc_size(hid_t file_id,
                size_t *max_size_ptr,
                size_t *min_clean_size_ptr,
                size_t *cur_size_ptr,
                int *cur_num_entries_ptr)
{
    H5F_t      *file=NULL;      /* File object for file ID */
    herr_t     ret_value = SUCCEED;      /* Return value */
    herr_t     result;
    int32_t    cur_num_entries;

    FUNC_ENTER_API(H5Fget_mdc_size, FAIL)
    H5TRACE5("e", "i*z*z*z*Is", file_id, max_size_ptr, min_clean_size_ptr,
             cur_size_ptr, cur_num_entries_ptr);

    /* Check args */
    if ( NULL == (file = H5I_object_verify(file_id, H5I_FILE)) ) {

         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")
    }

    /* Go get the size data */
    result = H5AC_get_cache_size(file->shared->cache,
                                 max_size_ptr,
                                 min_clean_size_ptr,
                                 cur_size_ptr,
                                 &cur_num_entries);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5AC_get_cache_size() failed.");

    } else if ( cur_num_entries_ptr != NULL ) {

	*cur_num_entries_ptr = (int)cur_num_entries;
    }

done:

    FUNC_LEAVE_API(ret_value)

} /* H5Fget_mdc_size() */


/*-------------------------------------------------------------------------
 * Function:    H5Freset_mdc_hit_rate_stats
 *
 * Purpose:     Reset the hit rate statistic whose current value can
 *		be obtained via the H5Fget_mdc_hit_rate() call.  Note
 *		that this statistic will also be reset once per epoch
 *		by the automatic cache resize code if it is enabled.
 *
 *		It is probably a bad idea to call this function unless
 *		you are controlling cache size from your program instead
 *		of using our cache size control code.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Freset_mdc_hit_rate_stats(hid_t file_id)
{
    H5F_t      *file;                   /* File object for file ID */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(H5Freset_mdc_hit_rate_stats, FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check args */
    if(NULL == (file = H5I_object_verify(file_id, H5I_FILE)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Reset the hit rate statistic */
    if(H5AC_reset_cache_hit_rate_stats(file->shared->cache) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "can't reset cache hit rate")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Freset_mdc_hit_rate_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_name
 *
 * Purpose:     Gets the name of the file to which object OBJ_ID belongs.
 *              If `name' is non-NULL then write up to `size' bytes into that
 *              buffer and always return the length of the entry name.
 *              Otherwise `size' is ignored and the function does not store the name,
 *              just returning the number of characters required to store the name.
 *              If an error occurs then the buffer pointed to by `name' (NULL or non-NULL)
 *              is unchanged and the function returns a negative value.
 *
 * Return:      Success:        The length of the file name
 *              Failure:        Negative
 *
 * Programmer:  Raymond Lu
 *              June 29, 2004
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_name(hid_t obj_id, char *name/*out*/, size_t size)
{
    H5F_t         *f;           /* Top file in mount hierarchy */
    size_t        len;
    ssize_t       ret_value;

    FUNC_ENTER_API (H5Fget_name, FAIL)
    H5TRACE3("Zs", "ixz", obj_id, name, size);

    /* For file IDs, get the file object directly */
    /* (This prevents the H5G_loc() call from returning the file pointer for
     * the top file in a mount hierarchy)
     */
    if(H5I_get_type(obj_id) == H5I_FILE ) {
        if(NULL == (f = H5I_object(obj_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
    } /* end if */
    else {
        H5G_loc_t     loc;        /* Object location */

        /* Get symbol table entry */
        if(H5G_loc(obj_id, &loc) < 0)
             HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object ID")
        f = loc.oloc->file;
    } /* end else */

    len = HDstrlen(f->name);

    if(name) {
        HDstrncpy(name, f->name, MIN(len+1,size));
        if(len >= size)
            name[size-1]='\0';
    } /* end if */

    /* Set return value */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_name() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_info
 *		1. Get storage size for superblock extension if there is one
 *              2. Get the amount of btree and heap storage for entries
 *                 in the SOHM table if there is one.
 *		Consider success when there is no superblock extension and/or SOHM table
 *
 * Return:      Success:        non-negative on success
 *              Failure:        Negative
 *
 * Programmer:  Vailin Choi
 *              July 11, 2007
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_info(hid_t obj_id, H5F_info_t *finfo)
{
    H5F_t *f;                           /* Top file in mount hierarchy */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Fget_info, FAIL)
    H5TRACE2("e", "i*x", obj_id, finfo);

    /* Check args */
    if(!finfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no info struct")

    /* For file IDs, get the file object directly */
    /* (This prevents the H5G_loc() call from returning the file pointer for
     * the top file in a mount hierarchy)
     */
    if(H5I_get_type(obj_id) == H5I_FILE ) {
        if(NULL == (f = H5I_object(obj_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
    } /* end if */
    else {
        H5G_loc_t     loc;        /* Object location */

        /* Get symbol table entry */
        if(H5G_loc(obj_id, &loc) < 0)
             HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object ID")
        f = loc.oloc->file;
    } /* end else */
    HDassert(f->shared);

    /* Reset file info struct */
    HDmemset(finfo, 0, sizeof(H5F_info_t));

    /* Check for superblock extension info */
    if(H5F_addr_defined(f->shared->extension_addr))
        if(H5F_super_ext_size(f, H5AC_ind_dxpl_id, &finfo->super_ext_size) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve superblock extension size")

    /* Check for SOHM info */
    if(H5F_addr_defined(f->shared->sohm_addr))
        if(H5SM_ih_size(f, H5AC_ind_dxpl_id, finfo) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve SOHM btree & heap storage info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_info() */

