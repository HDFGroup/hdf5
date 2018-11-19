/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/****************/
/* Module Setup */
/****************/

#include "H5Fmodule.h"          /* This source code file is part of the H5F module */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                        */
#include "H5Aprivate.h"         /* Attributes                               */
#include "H5ACprivate.h"        /* Metadata cache                           */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Dprivate.h"         /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Fpkg.h"             /* File access                              */
#include "H5FDprivate.h"        /* File drivers                             */
#include "H5Gprivate.h"         /* Groups                                   */
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5Lprivate.h"         /* Links                                    */
#include "H5MFprivate.h"        /* File memory management                   */
#include "H5MVprivate.h"        /* File memory management for VFD SWMR      */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Pprivate.h"         /* Property lists                           */
#include "H5SMprivate.h"        /* Shared Object Header Messages            */
#include "H5Tprivate.h"         /* Datatypes                                */


/****************/
/* Local Macros */
/****************/

/* VFD SWMR */

/* Prepend entry to the delayed free spaced release linked list */ 
#define H5F_DC_PREPEND(entry_ptr, head_ptr, tail_ptr, len)          \
{                                                                   \
    if((head_ptr) == NULL) {                                        \
       (head_ptr) = (entry_ptr);                                    \
       (tail_ptr) = (entry_ptr);                                    \
    } else {                                                        \
       (head_ptr)->prev = (entry_ptr);                              \
       (entry_ptr)->next = (head_ptr);                              \
       (head_ptr) = (entry_ptr);                                    \
    }                                                               \
    (len)++;                                                        \
} /* H5F_DC_PREPEND() */

/* Remove entry from delayed free spaced release linked list */ 
#define H5F_DC_REMOVE(entry_ptr, head_ptr, tail_ptr, len)           \
{                                                                   \
    if((head_ptr) == (entry_ptr)) {                                 \
          (head_ptr) = (entry_ptr)->next;                           \
          if((head_ptr) != NULL )                                   \
             (head_ptr)->prev = NULL;                               \
    } else                                                          \
        (entry_ptr)->prev->next = (entry_ptr)->next;                \
    if((tail_ptr) == (entry_ptr)) {                                 \
        (tail_ptr) = (entry_ptr)->prev;                             \
        if((tail_ptr) != NULL)                                      \
            (tail_ptr)->next = NULL;                                \
    } else                                                          \
        (entry_ptr)->next->prev = (entry_ptr)->prev;                \
    entry_ptr->next = NULL;                                         \
    entry_ptr->prev = NULL;                                         \
    (len)--;                                                        \
} /* H5F_DC_REMOVE() */

/******************/
/* Local Typedefs */
/******************/

/* Struct only used by functions H5F__get_objects and H5F__get_objects_cb */
typedef struct H5F_olist_t {
    H5I_type_t obj_type;        /* Type of object to look for */
    hid_t      *obj_id_list;    /* Pointer to the list of open IDs to return */
    size_t     *obj_id_count;   /* Number of open IDs */
    struct {
        hbool_t local;          /* Set flag for "local" file searches */
        union {
            H5F_file_t *shared; /* Pointer to shared file to look inside */
            const H5F_t *file;  /* Pointer to file to look inside */
        } ptr;
    } file_info;
    size_t     list_index;      /* Current index in open ID array */
    size_t     max_nobjs;       /* Maximum # of IDs to put into array */
} H5F_olist_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5F__get_objects(const H5F_t *f, unsigned types, size_t max_index, hid_t *obj_id_list, hbool_t app_ref, size_t *obj_id_count_ptr);
static int H5F__get_objects_cb(void *obj_ptr, hid_t obj_id, void *key);
static herr_t H5F__build_name(const char *prefix, const char *file_name, char **full_name/*out*/);
static char *H5F__getenv_prefix_name(char **env_prefix/*in,out*/);
static herr_t H5F__build_actual_name(const H5F_t *f, const H5P_genplist_t *fapl, const char *name, char ** /*out*/ actual_name);
static herr_t H5F__flush_phase1(H5F_t *f);
static herr_t H5F__flush_phase2(H5F_t *f, hbool_t closing);

/* VFD SWMR */
static herr_t H5F__vfd_swmr_init(H5F_t *f, hbool_t file_create);
static herr_t H5F__vfd_swmr_close_or_flush(H5F_t *f, hbool_t closing);
static herr_t H5F__vfd_swmr_update_end_of_tick_and_tick_num(H5F_t *f, hbool_t incr_tick_num);
static herr_t H5F__vfd_swmr_construct_write_md_hdr(H5F_t *f, uint32_t num_entries);
static herr_t H5F__vfd_swmr_construct_write_md_idx(H5F_t *f, uint32_t num_entries, struct H5FD_vfd_swmr_idx_entry_t index[]);
static herr_t H5F__idx_entry_cmp(const void *_entry1, const void *_entry2);
static herr_t H5F__vfd_swmr_writer__create_index(H5F_t * f);



/*********************/
/* Package Variables */
/*********************/

/* VFD SWMR globals */
H5F_t *vfd_swmr_file_g;                 /* Points to the file struct */
hbool_t vfd_swmr_g = FALSE;             /* Is this a VFD SWMR configured file */
hbool_t vfd_swmr_writer_g = FALSE;      /* Is this the VFD SWMR writer */
uint64_t tick_num_g = 0;                /* The current tick_num */
struct timespec end_of_tick_g;          /* The current end_of_tick */


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5F_t struct */
H5FL_DEFINE(H5F_t);

/* Declare a free list to manage the H5F_file_t struct */
H5FL_DEFINE(H5F_file_t);

/* Declare a free list to manage the H5F_vfd_swmr_dl_entry_t struct */
H5FL_DEFINE(H5F_vfd_swmr_dl_entry_t);




/*-------------------------------------------------------------------------
 * Function:    H5F_get_access_plist
 *
 * Purpose:     Returns a copy of the file access property list of the
 *              specified file.
 *
 *              NOTE: Make sure that, if you are going to overwrite
 *              information in the copied property list that was
 *              previously opened and assigned to the property list, then
 *              you must close it before overwriting the values.
 *
 * Return:      Success:    Object ID for a copy of the file access
 *                          property list.
 *              Failure:    H5I_INVALID_HID
 *-------------------------------------------------------------------------
 */
hid_t
H5F_get_access_plist(H5F_t *f, hbool_t app_ref)
{
    H5P_genplist_t *new_plist;              /* New property list */
    H5P_genplist_t *old_plist;              /* Old property list */
    H5FD_driver_prop_t driver_prop;         /* Property for driver ID & info */
    hbool_t driver_prop_copied = FALSE;     /* Whether the driver property has been set up */
    unsigned   efc_size = 0;
    hid_t      ret_value = H5I_INVALID_HID; /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Check args */
    HDassert(f);

    /* Make a copy of the default file access property list */
    if(NULL == (old_plist = (H5P_genplist_t *)H5I_object(H5P_LST_FILE_ACCESS_ID_g)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not a property list")
    if((ret_value = H5P_copy_plist(old_plist, app_ref)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, H5I_INVALID_HID, "can't copy file access property list")
    if(NULL == (new_plist = (H5P_genplist_t *)H5I_object(ret_value)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not a property list")

    /* Copy properties of the file access property list */
    if(H5P_set(new_plist, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, &(f->shared->mdc_initCacheCfg)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set initial metadata cache resize config.")
    if(H5P_set(new_plist, H5F_ACS_DATA_CACHE_NUM_SLOTS_NAME, &(f->shared->rdcc_nslots)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set data cache number of slots")
    if(H5P_set(new_plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, &(f->shared->rdcc_nbytes)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set data cache byte size")
    if(H5P_set(new_plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, &(f->shared->rdcc_w0)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set preempt read chunks")
    if(H5P_set(new_plist, H5F_ACS_ALIGN_THRHD_NAME, &(f->shared->threshold)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set alignment threshold")
    if(H5P_set(new_plist, H5F_ACS_ALIGN_NAME, &(f->shared->alignment)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set alignment")
    if(H5P_set(new_plist, H5F_ACS_GARBG_COLCT_REF_NAME, &(f->shared->gc_ref)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set garbage collect reference")
    if(H5P_set(new_plist, H5F_ACS_META_BLOCK_SIZE_NAME, &(f->shared->meta_aggr.alloc_size)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set metadata cache size")
    if(H5P_set(new_plist, H5F_ACS_SIEVE_BUF_SIZE_NAME, &(f->shared->sieve_buf_size)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't sieve buffer size")
    if(H5P_set(new_plist, H5F_ACS_SDATA_BLOCK_SIZE_NAME, &(f->shared->sdata_aggr.alloc_size)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set 'small data' cache size")
    if(H5P_set(new_plist, H5F_ACS_LIBVER_LOW_BOUND_NAME, &f->shared->low_bound) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set 'low' bound for library format versions")
    if(H5P_set(new_plist, H5F_ACS_LIBVER_HIGH_BOUND_NAME, &f->shared->high_bound) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set 'high' bound for library format versions")
    if(H5P_set(new_plist, H5F_ACS_METADATA_READ_ATTEMPTS_NAME, &(f->shared->read_attempts)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set 'read attempts ' flag")
    if(H5P_set(new_plist, H5F_ACS_OBJECT_FLUSH_CB_NAME, &(f->shared->object_flush)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set object flush callback")

    if(f->shared->efc)
        efc_size = H5F__efc_max_nfiles(f->shared->efc);
    if(H5P_set(new_plist, H5F_ACS_EFC_SIZE_NAME, &efc_size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5I_INVALID_HID, "can't set elink file cache size")
    if(f->shared->pb_ptr != NULL) {
        if(H5P_set(new_plist, H5F_ACS_PAGE_BUFFER_SIZE_NAME, &(f->shared->pb_ptr->max_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5I_INVALID_HID, "can't set page buffer size")
        if(H5P_set(new_plist, H5F_ACS_PAGE_BUFFER_MIN_META_PERC_NAME, &(f->shared->pb_ptr->min_meta_perc)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5I_INVALID_HID, "can't set minimum metadata fraction of page buffer")
        if(H5P_set(new_plist, H5F_ACS_PAGE_BUFFER_MIN_RAW_PERC_NAME, &(f->shared->pb_ptr->min_raw_perc)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5I_INVALID_HID, "can't set minimum raw data fraction of page buffer")
    } /* end if */

    if(H5P_set(new_plist, H5F_ACS_VFD_SWMR_CONFIG_NAME, &(f->shared->vfd_swmr_config)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set initial metadata cache resize config.")

#ifdef H5_HAVE_PARALLEL
    if(H5P_set(new_plist, H5_COLL_MD_READ_FLAG_NAME, &(f->coll_md_read)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5I_INVALID_HID, "can't set collective metadata read flag")
    if(H5P_set(new_plist, H5F_ACS_COLL_MD_WRITE_FLAG_NAME, &(f->coll_md_write)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5I_INVALID_HID, "can't set collective metadata read flag")
#endif /* H5_HAVE_PARALLEL */
    if(H5P_set(new_plist, H5F_ACS_META_CACHE_INIT_IMAGE_CONFIG_NAME, &(f->shared->mdc_initCacheImageCfg)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set initial metadata cache resize config.")

    /* Prepare the driver property */
    driver_prop.driver_id = f->shared->lf->driver_id;
    driver_prop.driver_info = H5FD_fapl_get(f->shared->lf);
    driver_prop_copied = TRUE;

    /* Set the driver property */
    if(H5P_set(new_plist, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set file driver ID & info")

    /* Set the file close degree appropriately */
    if(f->shared->fc_degree == H5F_CLOSE_DEFAULT && H5P_set(new_plist, H5F_ACS_CLOSE_DEGREE_NAME, &(f->shared->lf->cls->fc_degree)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set file close degree")
    else if(f->shared->fc_degree != H5F_CLOSE_DEFAULT && H5P_set(new_plist, H5F_ACS_CLOSE_DEGREE_NAME, &(f->shared->fc_degree)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, H5I_INVALID_HID, "can't set file close degree")

done:
    /* Release the copy of the driver info, if it was set up */
    if(driver_prop_copied && H5FD_fapl_close(driver_prop.driver_id, driver_prop.driver_info) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, H5I_INVALID_HID, "can't close copy of driver info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_access_plist() */


/*-------------------------------------------------------------------------
 * Function: H5F_get_obj_count
 *
 * Purpose:  Private function return the number of opened object IDs
 *           (files, datasets, groups, datatypes) in the same file.
 *
 * Return:      SUCCEED on success, FAIL on failure.
 *-------------------------------------------------------------------------
 */
herr_t
H5F_get_obj_count(const H5F_t *f, unsigned types, hbool_t app_ref, size_t *obj_id_count_ptr)
{
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(obj_id_count_ptr);

    /* Perform the query */
    if((ret_value = H5F__get_objects(f, types, 0, NULL, app_ref, obj_id_count_ptr)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "H5F__get_objects failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_obj_count() */


/*-------------------------------------------------------------------------
 * Function:    H5F_get_obj_ids
 *
 * Purpose:     Private function to return a list of opened object IDs.
 *
 * Return:      Non-negative on success; can't fail.
 *-------------------------------------------------------------------------
 */
herr_t
H5F_get_obj_ids(const H5F_t *f, unsigned types, size_t max_objs, hid_t *oid_list, hbool_t app_ref, size_t *obj_id_count_ptr)
{
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(obj_id_count_ptr);

    /* Perform the query */
    if((ret_value = H5F__get_objects(f, types, max_objs, oid_list, app_ref, obj_id_count_ptr)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "H5F__get_objects failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_obj_ids() */


/*---------------------------------------------------------------------------
 * Function:    H5F__get_objects
 *
 * Purpose:     This function is called by H5F_get_obj_count or
 *              H5F_get_obj_ids to get number of object IDs and/or a
 *              list of opened object IDs (in return value).
 *
 * Return:      SUCCEED/FAIL
 *---------------------------------------------------------------------------
 */
static herr_t
H5F__get_objects(const H5F_t *f, unsigned types, size_t max_nobjs, hid_t *obj_id_list, hbool_t app_ref, size_t *obj_id_count_ptr)
{
    size_t obj_id_count = 0;    /* Number of open IDs */
    H5F_olist_t olist;          /* Structure to hold search results */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(obj_id_count_ptr);

    /* Set up search information */
    olist.obj_id_list  = (max_nobjs==0 ? NULL : obj_id_list);
    olist.obj_id_count = &obj_id_count;
    olist.list_index   = 0;
    olist.max_nobjs    = max_nobjs;

    /* Determine if we are searching for local or global objects */
    if(types & H5F_OBJ_LOCAL) {
        olist.file_info.local = TRUE;
        olist.file_info.ptr.file = f;
    }
    else {
        olist.file_info.local = FALSE;
        olist.file_info.ptr.shared = f ? f->shared : NULL;
    }

    /* Iterate through file IDs to count the number, and put their
     * IDs on the object list.  */
    if(types & H5F_OBJ_FILE) {
        olist.obj_type = H5I_FILE;
        if(H5I_iterate(H5I_FILE, H5F__get_objects_cb, &olist, app_ref) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)")
    }

    /* If the caller just wants to count the number of objects (OLIST.MAX_NOBJS is zero),
     * or the caller wants to get the list of IDs and the list isn't full,
     * search through dataset IDs to count number of datasets, and put their
     * IDs on the object list */
    if(!olist.max_nobjs || (olist.max_nobjs && olist.list_index<olist.max_nobjs)) {
        if(types & H5F_OBJ_DATASET) {
            olist.obj_type = H5I_DATASET;
            if(H5I_iterate(H5I_DATASET, H5F__get_objects_cb, &olist, app_ref) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(2)")
        }
    }

    /* If the caller just wants to count the number of objects (OLIST.MAX_NOBJS is zero),
     * or the caller wants to get the list of IDs and the list isn't full,
     * search through group IDs to count number of groups, and put their
     * IDs on the object list */
    if(!olist.max_nobjs || (olist.max_nobjs && olist.list_index<olist.max_nobjs)) {
        if(types & H5F_OBJ_GROUP) {
            olist.obj_type = H5I_GROUP;
            if(H5I_iterate(H5I_GROUP, H5F__get_objects_cb, &olist, app_ref) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(3)")
        }
    }

    /* If the caller just wants to count the number of objects (OLIST.MAX_NOBJS is zero),
     * or the caller wants to get the list of IDs and the list isn't full,
     * search through datatype IDs to count number of named datatypes, and put their
     * IDs on the object list */
    if(!olist.max_nobjs || (olist.max_nobjs && olist.list_index<olist.max_nobjs)) {
        if(types & H5F_OBJ_DATATYPE) {
            olist.obj_type = H5I_DATATYPE;
            if(H5I_iterate(H5I_DATATYPE, H5F__get_objects_cb, &olist, app_ref) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(4)")
        }
    }

    /* If the caller just wants to count the number of objects (OLIST.MAX_NOBJS is zero),
     * or the caller wants to get the list of IDs and the list isn't full,
     * search through attribute IDs to count number of attributes, and put their
     * IDs on the object list */
    if(!olist.max_nobjs || (olist.max_nobjs && olist.list_index<olist.max_nobjs)) {
        if(types & H5F_OBJ_ATTR) {
            olist.obj_type = H5I_ATTR;
            if(H5I_iterate(H5I_ATTR, H5F__get_objects_cb, &olist, app_ref) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(5)")
        }
    }

    /* Set the number of objects currently open */
    *obj_id_count_ptr = obj_id_count;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__get_objects() */


/*-------------------------------------------------------------------------
 * Function:    H5F__get_objects_cb
 *
 * Purpose:     H5F__get_objects' callback function.  It verifies if an
 *              object is in the file, and either count it or put its ID
 *              on the list.
 *
 * Return:      H5_ITER_STOP if the array of object IDs is filled up.
 *              H5_ITER_CONT otherwise.
 *-------------------------------------------------------------------------
 */
static int
H5F__get_objects_cb(void *obj_ptr, hid_t obj_id, void *key)
{
    H5F_olist_t *olist = (H5F_olist_t *)key;    /* Alias for search info */
    hbool_t     add_obj = FALSE;
    int         ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_STATIC

    HDassert(obj_ptr);
    HDassert(olist);

    /* Count file IDs */
    if(olist->obj_type == H5I_FILE) {
        if((olist->file_info.local &&
                (!olist->file_info.ptr.file ||
                (olist->file_info.ptr.file && (H5F_t*)obj_ptr == olist->file_info.ptr.file))) ||
                (!olist->file_info.local &&
                (!olist->file_info.ptr.shared ||
                (olist->file_info.ptr.shared && ((H5F_t*)obj_ptr)->shared == olist->file_info.ptr.shared)))) {
            add_obj = TRUE;
        }
    }
    else { /* Either count opened object IDs or put the IDs on the list */
        H5O_loc_t *oloc;        /* Group entry info for object */

        switch(olist->obj_type) {
            case H5I_ATTR:
                oloc = H5A_oloc((H5A_t *)obj_ptr);
                break;

            case H5I_GROUP:
                oloc = H5G_oloc((H5G_t *)obj_ptr);
                break;

            case H5I_DATASET:
                oloc = H5D_oloc((H5D_t *)obj_ptr);
                break;

            case H5I_DATATYPE:
                if(H5T_is_named((H5T_t*)obj_ptr)==TRUE)
                    oloc = H5T_oloc((H5T_t*)obj_ptr);
                else
                    oloc = NULL;
                break;

            case H5I_UNINIT:
            case H5I_BADID:
            case H5I_FILE:
            case H5I_DATASPACE:
            case H5I_REFERENCE:
            case H5I_VFL:
            case H5I_GENPROP_CLS:
            case H5I_GENPROP_LST:
            case H5I_ERROR_CLASS:
            case H5I_ERROR_MSG:
            case H5I_ERROR_STACK:
            case H5I_NTYPES:
            default:
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5_ITER_ERROR, "unknown or invalid data object")
        } /* end switch */

        if((olist->file_info.local &&
                    ((!olist->file_info.ptr.file && olist->obj_type == H5I_DATATYPE && H5T_is_immutable((H5T_t *)obj_ptr) == FALSE) ||
                    (!olist->file_info.ptr.file && olist->obj_type != H5I_DATATYPE) ||
                    (oloc && oloc->file == olist->file_info.ptr.file))) ||
                    (!olist->file_info.local &&
                    ((!olist->file_info.ptr.shared && olist->obj_type == H5I_DATATYPE && H5T_is_immutable((H5T_t *)obj_ptr) == FALSE) ||
                    (!olist->file_info.ptr.shared && olist->obj_type != H5I_DATATYPE) ||
                    (oloc && oloc->file && oloc->file->shared == olist->file_info.ptr.shared)))) {
            add_obj = TRUE;
        }
    } /* end else */

    if(add_obj) {
        /* Add the object's ID to the ID list, if appropriate */
        if(olist->obj_id_list) {
            olist->obj_id_list[olist->list_index] = obj_id;
            olist->list_index++;
        }

        /* Increment the number of open objects */
        if(olist->obj_id_count)
            (*olist->obj_id_count)++;

        /* Check if we've filled up the array.  Return H5_ITER_STOP only if
         * we have filled up the array. Otherwise return H5_ITER_CONT(RET_VALUE is
         * preset to H5_ITER_CONT) because H5I_iterate needs the return value of
         * H5_ITER_CONT to continue the iteration.
         */
        if(olist->max_nobjs > 0 && olist->list_index >= olist->max_nobjs)
            HGOTO_DONE(H5_ITER_STOP)  /* Indicate that the iterator should stop */
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__get_objects_cb() */


/*--------------------------------------------------------------------------
 * Function:    H5F__build_name
 *
 * Purpose:     Prepend PREFIX to FILE_NAME and store in FULL_NAME
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------*/
static herr_t
H5F__build_name(const char *prefix, const char *file_name, char **full_name/*out*/)
{
    size_t      prefix_len;             /* length of prefix */
    size_t      fname_len;              /* Length of external link file name */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    prefix_len = HDstrlen(prefix);
    fname_len = HDstrlen(file_name);

    /* Allocate a buffer to hold the filename + prefix + possibly the delimiter + terminating null byte */
    if(NULL == (*full_name = (char *)H5MM_malloc(prefix_len + fname_len + 2)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "unable to allocate filename buffer")

    /* Compose the full file name */
    HDsnprintf(*full_name, (prefix_len + fname_len + 2), "%s%s%s", prefix,
        ((prefix_len == 0 || H5_CHECK_DELIMITER(prefix[prefix_len - 1])) ? "" : H5_DIR_SEPS), file_name);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__build_name() */


/*--------------------------------------------------------------------------
 * Function:    H5F__getenv_prefix_name
 *
 * Purpose:     Get the first pathname in the list of pathnames stored in env_prefix,
 *              which is separated by the environment delimiter.
 *              env_prefix is modified to point to the remaining pathnames
 *              in the list.
 *
 * Return:      A pointer to a pathname (can't fail but can return NULL)
--------------------------------------------------------------------------*/
static char *
H5F__getenv_prefix_name(char **env_prefix/*in,out*/)
{
    char *strret;               /* Pointer to next separator */
    char *ret_value = NULL;     /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Set return value now */
    ret_value = *env_prefix;

    /* Advance to next component, if possible */
    strret = HDstrchr(*env_prefix, H5_COLON_SEPC);
    if(strret == NULL)
        *env_prefix = NULL;
    else {
        /* Advance to next component */
        *env_prefix = strret + 1;

        /* Terminate current component (pointed to by ret_value) */
        *strret = '\0';
    } /* end else */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__getenv_prefix_name() */


/*-------------------------------------------------------------------------
 * Function:    H5F_prefix_open_file
 *
 * Purpose:     Attempts to open a dataset file.
 *
 * Return:      Pointer to an opened file on success / NULL on failure
 *-------------------------------------------------------------------------
 */
H5F_t *
H5F_prefix_open_file(H5F_t *primary_file, H5F_prefix_open_t prefix_type,
    const char *prop_prefix, const char *file_name, unsigned file_intent,
    hid_t fapl_id)
{
    H5F_t       *src_file = NULL;       /* Source file */
    char        *full_name = NULL;      /* File name with prefix */
    char        *actual_file_name = NULL; /* File's actual name */
    char        *temp_file_name = NULL; /* Temporary pointer to file name */
    size_t      temp_file_name_len;     /* Length of temporary file name */
    H5F_t       *ret_value = NULL;      /* Return value  */

    FUNC_ENTER_NOAPI_NOINIT

    /* Simplify intent flags for open calls */
    file_intent &= (H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE | H5F_ACC_SWMR_READ);

    /* Copy the file name to use */
    if(NULL == (temp_file_name = H5MM_strdup(file_name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    temp_file_name_len = HDstrlen(temp_file_name);

    /* Target file_name is an absolute pathname: see RM for detailed description */
    if(H5_CHECK_ABSOLUTE(file_name) || H5_CHECK_ABS_PATH(file_name)) {
        /* Try opening file */
       src_file = H5F__efc_open(primary_file, file_name, file_intent, H5P_FILE_CREATE_DEFAULT, fapl_id);

        /* Adjust temporary file name if file not opened */
        if(NULL == src_file) {
            char *ptr;

            /* Reset the error stack */
            H5E_clear_stack(NULL);

            /* Get last component of file_name */
            H5_GET_LAST_DELIMITER(file_name, ptr)
            HDassert(ptr);

            /* Increment past delimiter */
            ptr++;

            /* Copy into the temp. file name */
            HDstrncpy(temp_file_name, ptr, temp_file_name_len);
            temp_file_name[temp_file_name_len - 1] = '\0';
        } /* end if */
    } /* end if */
    else if(H5_CHECK_ABS_DRIVE(file_name)) {
        /* Try opening file */
        src_file = H5F__efc_open(primary_file, file_name, file_intent, H5P_FILE_CREATE_DEFAULT, fapl_id);

        /* Adjust temporary file name if file not opened */
        if(NULL == src_file) {
            /* Reset the error stack */
            H5E_clear_stack(NULL);

            /* Strip "<drive-letter>:" */
            HDstrncpy(temp_file_name, &file_name[2], temp_file_name_len);
            temp_file_name[temp_file_name_len - 1] = '\0';
        } /* end if */
    } /* end if */

    /* Try searching from paths set in the environment variable */
    if(src_file == NULL) {
        char *env_prefix;

        /* Get the appropriate environment variable */
        if(H5F_PREFIX_VDS == prefix_type)
            env_prefix = HDgetenv("HDF5_VDS_PREFIX");
        else if(H5F_PREFIX_ELINK == prefix_type)
            env_prefix = HDgetenv("HDF5_EXT_PREFIX");
        else
            HGOTO_ERROR(H5E_FILE, H5E_BADTYPE, NULL, "prefix type is not sensible")

        /* If environment variable is defined, iterate through prefixes it defines */
        if(NULL != env_prefix) {
            char *tmp_env_prefix, *saved_env;

            /* Make a copy of the environment variable string */
            if(NULL == (saved_env = tmp_env_prefix = H5MM_strdup(env_prefix)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

            /* Loop over prefixes in environment variable */
            while((tmp_env_prefix) && (*tmp_env_prefix)) {
                char *out_prefix_name;

                out_prefix_name = H5F__getenv_prefix_name(&tmp_env_prefix/*in,out*/);
                if(out_prefix_name && (*out_prefix_name)) {
                    if(H5F__build_name(out_prefix_name, temp_file_name, &full_name/*out*/) < 0) {
                        saved_env = (char *)H5MM_xfree(saved_env);
                        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't prepend prefix to filename")
                    } /* end if */

                    /* Try opening file */
                    src_file = H5F__efc_open(primary_file, full_name, file_intent, H5P_FILE_CREATE_DEFAULT, fapl_id);

                    /* Release copy of file name */
                    full_name = (char *)H5MM_xfree(full_name);

                    /* Check for file not opened */
                    if(NULL == src_file)
                        /* Reset the error stack */
                        H5E_clear_stack(NULL);
                    /* Leave if file was opened */
                    else
                        break;
                    H5E_clear_stack(NULL);
                } /* end if */
            } /* end while */

            saved_env = (char *)H5MM_xfree(saved_env);
        } /* end if */
    } /* end if */

    /* Try searching from property list */
    if(src_file == NULL && prop_prefix) {
        /* Construct name to open */
        if(H5F__build_name(prop_prefix, temp_file_name, &full_name/*out*/) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't prepend prefix to filename")

        /* Try opening file */
        src_file = H5F__efc_open(primary_file, full_name, file_intent, H5P_FILE_CREATE_DEFAULT, fapl_id);

        /* Release name */
        full_name = (char *)H5MM_xfree(full_name);

        /* Check for file not opened */
        if(NULL == src_file)
            /* Reset the error stack */
            H5E_clear_stack(NULL);
    } /* end if */

    /* Try searching from main file's "extpath": see description in H5F_open() & H5_build_extpath() */
    if(src_file == NULL) {
        char *dspath;

        if(NULL != (dspath = H5F_EXTPATH(primary_file))) {
            /* Construct name to open */
            if(H5F__build_name(dspath, temp_file_name, &full_name/*out*/) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't prepend prefix to filename")

            /* Try opening file */
            src_file = H5F__efc_open(primary_file, full_name, file_intent, H5P_FILE_CREATE_DEFAULT, fapl_id);

            /* Release name */
            full_name = (char *)H5MM_xfree(full_name);

            /* Check for file not opened */
            if(NULL == src_file)
                /* Reset the error stack */
                H5E_clear_stack(NULL);
        } /* end if */
    } /* end if */

    /* Try the relative file_name stored in temp_file_name */
    if(src_file == NULL) {
        /* Try opening file */
        src_file = H5F__efc_open(primary_file, temp_file_name, file_intent, H5P_FILE_CREATE_DEFAULT, fapl_id);

        /* Check for file not opened */
        if(NULL == src_file)
            /* Reset the error stack */
            H5E_clear_stack(NULL);
    } /* end if */

    /* try the 'resolved' name for the virtual file */
    if(src_file == NULL) {
        char *ptr = NULL;

        /* Copy resolved file name */
        if(NULL == (actual_file_name = H5MM_strdup(H5F_ACTUAL_NAME(primary_file))))
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't duplicate resolved file name string")

        /* get last component of file_name */
        H5_GET_LAST_DELIMITER(actual_file_name, ptr)
        if(!ptr)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file, file name = '%s', temp_file_name = '%s'", file_name, temp_file_name)

        /* Truncate filename portion from actual file name path */
        *ptr = '\0';

        /* Build new file name for the external file */
        if(H5F__build_name(actual_file_name, temp_file_name, &full_name/*out*/) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't prepend prefix to filename")
        actual_file_name = (char *)H5MM_xfree(actual_file_name);

        /* Try opening with the resolved name */
        src_file = H5F__efc_open(primary_file, full_name, file_intent, H5P_FILE_CREATE_DEFAULT, fapl_id);

        /* Release name */
        full_name = (char *)H5MM_xfree(full_name);

        /* Check for file not opened */
        if(NULL == src_file)
            /* Reset the error stack */
            H5E_clear_stack(NULL);
    } /* end if */

    /* Success */
    ret_value = src_file;

done:
    if((NULL == ret_value) && src_file)
        if(H5F_efc_close(primary_file, src_file) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close source file")
    if(full_name)
        full_name = (char *)H5MM_xfree(full_name);
    if(temp_file_name)
        temp_file_name = (char *)H5MM_xfree(temp_file_name);
    if(actual_file_name)
        actual_file_name = (char *)H5MM_xfree(actual_file_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F_prefix_open_file() */


/*-------------------------------------------------------------------------
 * Function:    H5F__is_hdf5
 *
 * Purpose:     Check the file signature to detect an HDF5 file.
 *
 * Bugs:        This function is not robust: it only uses the default file
 *              driver when attempting to open the file when in fact it
 *              should use all known file drivers.
 *
 * Return:      TRUE/FALSE/FAIL
 *-------------------------------------------------------------------------
 */
htri_t
H5F__is_hdf5(const char *name)
{
    H5FD_t         *file = NULL;            /* Low-level file struct                    */
    haddr_t         sig_addr;               /* Addess of hdf5 file signature            */
    htri_t          ret_value = FAIL;       /* Return value                             */

    FUNC_ENTER_PACKAGE

    /* Open the file */
    if(NULL == (file = H5FD_open(name, H5F_ACC_RDONLY, H5P_FILE_ACCESS_DEFAULT, HADDR_UNDEF)))
        HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "unable to open file")

    /* The file is an hdf5 file if the hdf5 file signature can be found */
    if(H5FD_locate_signature(file, &sig_addr) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, FAIL, "unable to locate file signature")
    ret_value = (HADDR_UNDEF != sig_addr);

done:
    /* Close the file */
    if(file)
        if(H5FD_close(file) < 0 && TRUE == ret_value)
            HDONE_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__is_hdf5() */


/*-------------------------------------------------------------------------
 * Function:    H5F__new
 *
 * Purpose:     Creates a new file object and initializes it. The
 *              H5Fopen and H5Fcreate functions then fill in various fields.
 *              If SHARED is a non-null pointer then the shared info
 *              to which it points has the reference count incremented.
 *              Otherwise a new, empty shared info struct is created and
 *              initialized with the specified file access property list.
 *
 * Return:      Success:    Pointer to a new file struct
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5F__new(H5F_file_t *shared, unsigned flags, hid_t fcpl_id, hid_t fapl_id, H5FD_t *lf)
{
    H5F_t       *f          = NULL;
    H5F_t       *ret_value  = NULL;

    FUNC_ENTER_PACKAGE

    if(NULL == (f = H5FL_CALLOC(H5F_t)))
        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate top file structure")
    f->file_id = H5I_INVALID_HID;

    if(shared) {
        HDassert(lf == NULL);
        f->shared = shared;
    }
    else {
        H5P_genplist_t *plist;          /* Property list */
        unsigned efc_size;              /* External file cache size */
        size_t u;                       /* Local index variable */

        HDassert(lf != NULL);
        if(NULL == (f->shared = H5FL_CALLOC(H5F_file_t)))
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate shared file structure")

        f->shared->flags = flags;
        f->shared->sohm_addr = HADDR_UNDEF;
        f->shared->sohm_vers = HDF5_SHAREDHEADER_VERSION;
        f->shared->accum.loc = HADDR_UNDEF;
        f->shared->lf = lf;

        /* Initialization for handling file space */
        for(u = 0; u < NELMTS(f->shared->fs_addr); u++) {
            f->shared->fs_state[u] = H5F_FS_STATE_CLOSED;
            f->shared->fs_addr[u] = HADDR_UNDEF;
            f->shared->fs_man[u] = NULL;
        }
        f->shared->first_alloc_dealloc = FALSE;
        f->shared->eoa_pre_fsm_fsalloc = HADDR_UNDEF;
        f->shared->eoa_post_fsm_fsalloc = HADDR_UNDEF;
        f->shared->eoa_post_mdci_fsalloc = HADDR_UNDEF;

        /* Initialization for handling file space (for paged aggregation) */
        f->shared->pgend_meta_thres = H5F_FILE_SPACE_PGEND_META_THRES;

        /* intialize point of no return */
        f->shared->point_of_no_return = FALSE;

        /* Copy the file creation and file access property lists into the
         * new file handle. We do this early because some values might need
         * to change as the file is being opened.
         */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fcpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not property list")
        f->shared->fcpl_id = H5P_copy_plist(plist, FALSE);

        /* Get the FCPL values to cache */
        if(H5P_get(plist, H5F_CRT_ADDR_BYTE_NUM_NAME, &f->shared->sizeof_addr) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get byte number for address")
        if(H5P_get(plist, H5F_CRT_OBJ_BYTE_NUM_NAME, &f->shared->sizeof_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get byte number for object size")
        if(H5P_get(plist, H5F_CRT_SHMSG_NINDEXES_NAME, &f->shared->sohm_nindexes) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get number of SOHM indexes")
        HDassert(f->shared->sohm_nindexes < 255);
        if(H5P_get(plist, H5F_CRT_FILE_SPACE_STRATEGY_NAME, &f->shared->fs_strategy) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get file space strategy")
        if(H5P_get(plist, H5F_CRT_FREE_SPACE_PERSIST_NAME, &f->shared->fs_persist) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get file space persisting status")
        if(H5P_get(plist, H5F_CRT_FREE_SPACE_THRESHOLD_NAME, &f->shared->fs_threshold) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get free-space section threshold")
        if(H5P_get(plist, H5F_CRT_FILE_SPACE_PAGE_SIZE_NAME, &f->shared->fs_page_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get file space page size")
        HDassert(f->shared->fs_page_size >= H5F_FILE_SPACE_PAGE_SIZE_MIN);

        /* Temporary for multi/split drivers: fail file creation
         * when persisting free-space or using paged aggregation strategy.
         */
        if(H5F_HAS_FEATURE(f, H5FD_FEAT_PAGED_AGGR))
            if(f->shared->fs_strategy == H5F_FSPACE_STRATEGY_PAGE || f->shared->fs_persist)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't open with this strategy or persistent fs")

        /* Get the FAPL values to cache */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not file access property list")
        if(H5P_get(plist, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, &(f->shared->mdc_initCacheCfg)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get initial metadata cache resize config")
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_NUM_SLOTS_NAME, &(f->shared->rdcc_nslots)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get data cache number of slots")
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, &(f->shared->rdcc_nbytes)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get data cache byte size")
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
        if(H5P_get(plist, H5F_ACS_LIBVER_LOW_BOUND_NAME, &(f->shared->low_bound)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get 'low' bound for library format versions")
        if(H5P_get(plist, H5F_ACS_LIBVER_HIGH_BOUND_NAME, &(f->shared->high_bound)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get 'high' bound for library format versions")
        if(H5P_get(plist, H5F_ACS_USE_MDC_LOGGING_NAME, &(f->shared->use_mdc_logging)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get 'use mdc logging' flag")
        if(H5P_get(plist, H5F_ACS_START_MDC_LOG_ON_ACCESS_NAME, &(f->shared->start_mdc_log_on_access)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get 'start mdc log on access' flag")
        if(H5P_get(plist, H5F_ACS_META_BLOCK_SIZE_NAME, &(f->shared->meta_aggr.alloc_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get metadata cache size")
        f->shared->meta_aggr.feature_flag = H5FD_FEAT_AGGREGATE_METADATA;
        if(H5P_get(plist, H5F_ACS_SDATA_BLOCK_SIZE_NAME, &(f->shared->sdata_aggr.alloc_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get 'small data' cache size")
        f->shared->sdata_aggr.feature_flag = H5FD_FEAT_AGGREGATE_SMALLDATA;
        if(H5P_get(plist, H5F_ACS_EFC_SIZE_NAME, &efc_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get elink file cache size")
        if(efc_size > 0)
            if(NULL == (f->shared->efc = H5F__efc_create(efc_size)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't create external file cache")
#ifdef H5_HAVE_PARALLEL
        if(H5P_get(plist, H5_COLL_MD_READ_FLAG_NAME, &(f->coll_md_read)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get collective metadata read flag")
        if(H5P_get(plist, H5F_ACS_COLL_MD_WRITE_FLAG_NAME, &(f->coll_md_write)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get collective metadata write flag")
#endif /* H5_HAVE_PARALLEL */
        if(H5P_get(plist, H5F_ACS_META_CACHE_INIT_IMAGE_CONFIG_NAME, &(f->shared->mdc_initCacheImageCfg)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get initial metadata cache resize config")

        /* Get the VFD values to cache */
        f->shared->maxaddr = H5FD_get_maxaddr(lf);
        if(!H5F_addr_defined(f->shared->maxaddr))
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad maximum address from VFD")
        if(H5FD_get_feature_flags(lf, &f->shared->feature_flags) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get feature flags from VFD")

        /* Require the SWMR feature flag if SWMR I/O is desired */
        if(!H5F_HAS_FEATURE(f, H5FD_FEAT_SUPPORTS_SWMR_IO) && (H5F_INTENT(f) & (H5F_ACC_SWMR_WRITE | H5F_ACC_SWMR_READ)))
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "must use a SWMR-compatible VFD when SWMR is specified")

        if(H5FD_get_fs_type_map(lf, f->shared->fs_type_map) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get free space type mapping from VFD")
        if(H5MF_init_merge_flags(f) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "problem initializing free space merge flags")
        f->shared->tmp_addr = f->shared->maxaddr;
        /* Disable temp. space allocation for parallel I/O (for now) */
        /* (When we've arranged to have the relocated metadata addresses (and
         *      sizes) broadcast during the "end of epoch" metadata operations,
         *      this can be enabled - QAK)
         */
        /* (This should be disabled when the metadata journaling branch is
         *      merged into the trunk and journaling is enabled, at least until
         *      we make it work. - QAK)
         */
        f->shared->use_tmp_space = !H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI);

        /* Retrieve the # of read attempts here so that sohm in superblock will get the correct # of attempts */
        if(H5P_get(plist, H5F_ACS_METADATA_READ_ATTEMPTS_NAME, &f->shared->read_attempts) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get the # of read attempts")

        /* When opening file with SWMR access, the # of read attempts is H5F_SWMR_METADATA_READ_ATTEMPTS if not set */
        /* When opening file without SWMR access, the # of read attempts is always H5F_METADATA_READ_ATTEMPTS (set or not set) */
        if(H5F_INTENT(f) & (H5F_ACC_SWMR_READ | H5F_ACC_SWMR_WRITE)) {
            /* If no value for read attempts has been set, use the default */
            if(!f->shared->read_attempts)
                f->shared->read_attempts = H5F_SWMR_METADATA_READ_ATTEMPTS;

            /* Turn off accumulator with SWMR */
            f->shared->feature_flags &= ~(unsigned)H5FD_FEAT_ACCUMULATE_METADATA;
            if(H5FD_set_feature_flags(f->shared->lf, f->shared->feature_flags) < 0)
                 HGOTO_ERROR(H5E_FILE, H5E_CANTSET, NULL, "can't set feature_flags in VFD")
        }
        else {
            /* If no value for read attempts has been set, use the default */
            if(!f->shared->read_attempts)
                f->shared->read_attempts = H5F_METADATA_READ_ATTEMPTS;
        }

        /* Determine the # of bins for metdata read retries */
        if(H5F_set_retries(f) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't set retries and retries_nbins")

        /* Get the metadata cache log location (if we're logging) */
        {
            char *mdc_log_location = NULL;      /* location of metadata cache log location */

            if(H5P_get(plist, H5F_ACS_MDC_LOG_LOCATION_NAME, &mdc_log_location) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get mdc log location")
            if(mdc_log_location != NULL) {
                size_t len = HDstrlen(mdc_log_location);
                if(NULL == (f->shared->mdc_log_location = (char *)H5MM_calloc((len + 1) * sizeof(char))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate memory for mdc log file name")
                HDstrncpy(f->shared->mdc_log_location, mdc_log_location, len);
            }
            else
                f->shared->mdc_log_location = NULL;
        } /* end block */

        /* Get object flush callback information */
        if(H5P_get(plist, H5F_ACS_OBJECT_FLUSH_CB_NAME, &(f->shared->object_flush)) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get object flush cb info")

        /* Get VFD SWMR configuration */
        if(H5P_get(plist, H5F_ACS_VFD_SWMR_CONFIG_NAME, &(f->shared->vfd_swmr_config)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get VFD SWMR config info")

        /* Initialization for VFD SWMR */
        f->shared->vfd_swmr_md_fd = -1;
        f->shared->fs_man_md = NULL;
        f->shared->dl_head_ptr = NULL;
        f->shared->dl_tail_ptr = NULL;
        f->shared->dl_len = 0;

        /* Create a metadata cache with the specified number of elements.
         * The cache might be created with a different number of elements and
         * the access property list should be updated to reflect that.
         */
        if(H5AC_create(f, &(f->shared->mdc_initCacheCfg), &(f->shared->mdc_initCacheImageCfg)) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create metadata cache")

        /* Create the file's "open object" information */
        if(H5FO_create(f) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create open object data structure")

        /* Add new "shared" struct to list of open files */
        if(H5F__sfile_add(f->shared) < 0)
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
        if(!shared) {
            /* Attempt to clean up some of the shared file structures */
            if(f->shared->efc)
                if(H5F__efc_destroy(f->shared->efc) < 0)
                    HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, NULL, "can't destroy external file cache")
            if(f->shared->fcpl_id > 0)
                if(H5I_dec_ref(f->shared->fcpl_id) < 0)
                    HDONE_ERROR(H5E_FILE, H5E_CANTDEC, NULL, "can't close property list")

            f->shared = H5FL_FREE(H5F_file_t, f->shared);
        }
        f = H5FL_FREE(H5F_t, f);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__new() */


/*-------------------------------------------------------------------------
 * Function:    H5F__dest
 *
 * Purpose:     Destroys a file structure.  This function flushes the cache
 *              but doesn't do any other cleanup other than freeing memory
 *              for the file struct.  The shared info for the file is freed
 *              only when its reference count reaches zero.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F__dest(H5F_t *f, hbool_t flush)
{
    herr_t       ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    if(1 == f->shared->nrefs) {
        int actype;                         /* metadata cache type (enum value) */

        /* Flush at this point since the file will be closed (phase 1).
         * Only try to flush the file if it was opened with write access, and if
         * the caller requested a flush.
         */
        if((H5F_ACC_RDWR & H5F_INTENT(f)) && flush)
            if(H5F__flush_phase1(f) < 0)
                /* Push error, but keep going*/
                HDONE_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush cached data (phase 1)")

        /* Notify the metadata cache that the file is about to be closed.
         * This allows the cache to set up for creating a metadata cache
         * image if this has been requested.
         */
        if(H5AC_prep_for_file_close(f) < 0)
            /* Push error, but keep going */
            HDONE_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "metadata cache prep for close failed")

        /* Flush at this point since the file will be closed (phase 2).
         * Only try to flush the file if it was opened with write access, and if
         * the caller requested a flush.
         */
        if((H5F_ACC_RDWR & H5F_INTENT(f)) && flush)
            if(H5F__flush_phase2(f, TRUE) < 0)
                /* Push error, but keep going */
                HDONE_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush cached data (phase 2)")

        /* With the shutdown modifications, the contents of the metadata cache
         * should be clean at this point, with the possible exception of the
         * the superblock and superblock extension.
         *
         * Verify this.
         */
        HDassert(H5AC_cache_is_clean(f, H5AC_RING_MDFSM));

        /* Release the external file cache */
        if(f->shared->efc) {
            if(H5F__efc_destroy(f->shared->efc) < 0)
                /* Push error, but keep going*/
                HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't destroy external file cache")
            f->shared->efc = NULL;
        } /* end if */

        /* With the shutdown modifications, the contents of the metadata cache
         * should be clean at this point, with the possible exception of the
         * the superblock and superblock extension.
         *
         * Verify this.
         */
        HDassert(H5AC_cache_is_clean(f, H5AC_RING_MDFSM));

        /* Release objects that depend on the superblock being initialized */
        if(f->shared->sblock) {
            /* Shutdown file free space manager(s) */
            /* (We should release the free space information now (before
             *      truncating the file and before the metadata cache is shut
             *      down) since the free space manager is holding some data
             *      structures in memory and also because releasing free space
             *      can shrink the file's 'eoa' value)
             *
             * Update 11/1/16:
             *
             *      With recent library shutdown modifications, the free space
             *      managers should be settled and written to file at this point
             *      (assuming they are persistent).  In this case, closing the
             *      free space managers should have no effect on EOA.
             *
             *                                          -- JRM
             */
            if(H5F_ACC_RDWR & H5F_INTENT(f)) {
                if(H5MF_close(f) < 0)
                    /* Push error, but keep going*/
                    HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release file free space info")

                /* at this point, only the superblock and superblock
                 * extension should be dirty.
                 */
                HDassert(H5AC_cache_is_clean(f, H5AC_RING_MDFSM));

                /* Flush the file again (if requested), as shutting down the
                 * free space manager may dirty some data structures again.
                 */
                if(flush) {
                    /* Clear status_flags */
                    f->shared->sblock->status_flags &= (uint8_t)(~H5F_SUPER_WRITE_ACCESS);
                    f->shared->sblock->status_flags &= (uint8_t)(~H5F_SUPER_SWMR_WRITE_ACCESS);

                    /* Mark EOA info dirty in cache, so change will get encoded */
                    if(H5F_eoa_dirty(f) < 0)
                        /* Push error, but keep going*/
                        HDONE_ERROR(H5E_FILE, H5E_CANTMARKDIRTY, FAIL, "unable to mark superblock as dirty")

                    /* Release any space allocated to space aggregators,
                     * so that the eoa value corresponds to the end of the
                     * space written to in the file.
                     *
                     * At most, this should change the superblock or the
                     * superblock extension messages.
                     */
                    if(H5MF_free_aggrs(f) < 0)
                        /* Push error, but keep going*/
                        HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release file space")

                    /* Truncate the file to the current allocated size */
                    if(H5FD_truncate(f->shared->lf, TRUE) < 0)
                        /* Push error, but keep going*/
                        HDONE_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "low level truncate failed")

                    /* at this point, only the superblock and superblock
                     * extension should be dirty.
                     */
                    HDassert(H5AC_cache_is_clean(f, H5AC_RING_MDFSM));
                } /* end if */
            } /* end if */

            /* if it exists, unpin the driver information block cache entry,
             * since we're about to destroy the cache
             */
            if(f->shared->drvinfo)
                if(H5AC_unpin_entry(f->shared->drvinfo) < 0)
                    /* Push error, but keep going*/
                    HDONE_ERROR(H5E_FSPACE, H5E_CANTUNPIN, FAIL, "unable to unpin drvinfo")

            /* Unpin the superblock, since we're about to destroy the cache */
            if(H5AC_unpin_entry(f->shared->sblock) < 0)
                /* Push error, but keep going*/
                HDONE_ERROR(H5E_FSPACE, H5E_CANTUNPIN, FAIL, "unable to unpin superblock")
            f->shared->sblock = NULL;
        } /* end if */

        /* with the possible exception of the superblock and superblock
         * extension, the metadata cache should be clean at this point.
         *
         * Verify this.
         */
        HDassert(H5AC_cache_is_clean(f, H5AC_RING_MDFSM));

        /* Remove shared file struct from list of open files */
        if(H5F__sfile_remove(f->shared) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")

        /* Shutdown the metadata cache */
        /* (Flushes any remaining dirty entries, which should only be the
         *      superblock and / or driver info at this point)
         */
        if(H5AC_dest(f))
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")

        /* Shutdown the page buffer cache */
        if(H5PB_dest(f) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing page buffer cache")

        /* Clean up the metadata cache log location string */
        if(f->shared->mdc_log_location)
            f->shared->mdc_log_location = (char *)H5MM_xfree(f->shared->mdc_log_location);

        /*
         * Do not close the root group since we didn't count it, but free
         * the memory associated with it.
         */
        if(f->shared->root_grp) {
            /* Free the root group */
            if(H5G_root_free(f->shared->root_grp) < 0)
                /* Push error, but keep going*/
                HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")
            f->shared->root_grp = NULL;
        } /* end if */

        /* Destroy other components of the file */
        if(H5F__accum_reset(f, TRUE) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")
        if(H5FO_dest(f) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")
        f->shared->cwfs = (struct H5HG_heap_t **)H5MM_xfree(f->shared->cwfs);
        if(H5G_node_close(f) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "problems closing file")

        /* Destroy file creation properties */
        if(H5I_GENPROP_LST != H5I_get_type(f->shared->fcpl_id))
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_BADTYPE, FAIL, "not a property list")
        if(H5I_dec_ref(f->shared->fcpl_id) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close property list")

        /* VFD SWMR: closing down */
        if(H5F_ACC_RDWR & H5F_INTENT(f) && f->shared->vfd_swmr_md_fd >= 0) {
            if(H5F__vfd_swmr_close_or_flush(f, TRUE) < 0)
                HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close the metadata file")
        }

        /* Close the file */
        if(H5FD_close(f->shared->lf) < 0)
            /* Push error, but keep going*/
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close file")

        /* Free mount table */
        f->shared->mtab.child = (H5F_mount_t *)H5MM_xfree(f->shared->mtab.child);
        f->shared->mtab.nalloc = 0;

        /* Clean up the metadata retries array */
        for(actype = 0; actype < (int)H5AC_NTYPES; actype++)
            if(f->shared->retries[actype])
                f->shared->retries[actype] = (uint32_t *)H5MM_xfree(f->shared->retries[actype]);

        /* Destroy shared file struct */
        f->shared = (H5F_file_t *)H5FL_FREE(H5F_file_t, f->shared);

    }
    else if(f->shared->nrefs > 0) {
        /*
         * There are other references to the shared part of the file.
         * Only decrement the reference count.
         */
        --f->shared->nrefs;
    }

    /* Free the non-shared part of the file */
    f->open_name = (char *)H5MM_xfree(f->open_name);
    f->actual_name = (char *)H5MM_xfree(f->actual_name);
    f->extpath = (char *)H5MM_xfree(f->extpath);
    if(H5FO_top_dest(f) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "problems closing file")
    f->shared = NULL;
    f = H5FL_FREE(H5F_t, f);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__dest() */


/*-------------------------------------------------------------------------
 * Function:    H5F_open
 *
 * Purpose:    Opens (or creates) a file.  This function understands the
 *        following flags which are similar in nature to the Posix
 *        open(2) flags.
 *
 *        H5F_ACC_RDWR:    Open with read/write access. If the file is
 *                currently open for read-only access then it
 *                will be reopened. Absence of this flag
 *                implies read-only access.
 *
 *        H5F_ACC_CREAT:    Create a new file if it doesn't exist yet.
 *                The permissions are 0666 bit-wise AND with
 *                the current umask.  H5F_ACC_WRITE must also
 *                be specified.
 *
 *        H5F_ACC_EXCL:    This flag causes H5F_open() to fail if the
 *                file already exists.
 *
 *        H5F_ACC_TRUNC:    The file is truncated and a new HDF5 superblock
 *                is written.  This operation will fail if the
 *                file is already open.
 *
 *        Unlinking the file name from the group directed graph while
 *        the file is opened causes the file to continue to exist but
 *        one will not be able to upgrade the file from read-only
 *        access to read-write access by reopening it. Disk resources
 *        for the file are released when all handles to the file are
 *        closed. NOTE: This paragraph probably only applies to Unix;
 *        deleting the file name in other OS's has undefined results.
 *
 *        The CREATE_PARMS argument is optional.    A null pointer will
 *        cause the default file creation parameters to be used.
 *
 *        The ACCESS_PARMS argument is optional.  A null pointer will
 *        cause the default file access parameters to be used.
 *
 * The following two tables show results of file opens for single and concurrent access:
 *
 * SINGLE PROCESS ACCESS                        CONCURRENT ACCESS
 *
 *             #1st open#                                   #1st open#
 *             -- SR SR -- -- SR SR --                      -- SR SR -- -- SR SR --
 *             -- -- SW SW SW SW -- --                      -- -- SW SW SW SW -- --
 *              W  W  W  W  R  R  R  R                       W  W  W  W  R  R  R  R
 * #2nd open#                                   #2nd open#
 *            --------------------------                   --------------------------
 *   -- --  W | s  x  x  s  x  x  f  f |          -- --  W | f  x  x  f  x  x  f  f |
 *   SR --  W | x  x  x  x  x  x  x  x |          SR --  W | x  x  x  x  x  x  x  x |
 *   SR SW  W | x  x  x  x  x  x  x  x |          SR SW  W | x  x  x  x  x  x  x  x |
 *   -- SW  W | f  x  x  s  x  x  f  f |          -- SW  W | f  x  x  f  x  x  f  f |
 *   -- SW  R | x  x  x  x  x  x  x  x |          -- SW  R | x  x  x  x  x  x  x  x |
 *   SR SW  R | x  x  x  x  x  x  x  x |          SR SW  R | x  x  x  x  x  x  x  x |
 *   SR --  R | s  x  x  s  x  x  s  f |          SR --  R | f  x  x  s  x  x  s  s |
 *   -- --  R | s  x  x  s  x  x  s  s |          -- --  R | f  x  x  f  x  x  s  s |
 *            --------------------------                   --------------------------
 *
 *      Notations:
 *        W:  H5F_ACC_RDWR
 *        R:  H5F_ACC_RDONLY
 *        SW: H5F_ACC_SWMR_WRITE
 *        SR: H5F_ACC_SWMR_READ
 *
 *        x: the first open or second open itself fails due to invalid flags combination
 *        f: the open fails with flags combination from both the first and second opens
 *        s: the open succeeds with flags combination from both the first and second opens
 *
 *
 * Return:    Success:    A new file pointer.
 *        Failure:    NULL
 *-------------------------------------------------------------------------
 */
H5F_t *
H5F_open(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    H5F_t              *file = NULL;        /*the success return value      */
    H5F_file_t         *shared = NULL;      /*shared part of `file'         */
    H5FD_t             *lf = NULL;          /*file driver part of `shared'  */
    unsigned            tent_flags;         /*tentative flags               */
    H5FD_class_t       *drvr;               /*file driver class info        */
    H5P_genplist_t     *a_plist;            /*file access property list     */
    H5F_close_degree_t  fc_degree;          /*file close degree             */
    size_t              page_buf_size;
    unsigned            page_buf_min_meta_perc;
    unsigned            page_buf_min_raw_perc;
    hbool_t             set_flag = FALSE;   /*set the status_flags in the superblock */
    hbool_t             clear = FALSE;      /*clear the status_flags         */
    hbool_t             evict_on_close;     /* evict on close value from plist  */
    char               *lock_env_var = NULL;/*env var pointer               */
    hbool_t             use_file_locking;   /*read from env var             */
    hbool_t             ci_load = FALSE;    /* whether MDC ci load requested */
    hbool_t             ci_write = FALSE;   /* whether MDC CI write requested */
    hbool_t             file_create = FALSE;    /* creating a new file or not */
    H5FD_t             *underlying_lf = NULL;   /* underlying file driver for VFD SWMR */
    H5F_t              *ret_value = NULL;       /*actual return value           */

    FUNC_ENTER_NOAPI(NULL)

    /*
     * If the driver has a `cmp' method then the driver is capable of
     * determining when two file handles refer to the same file and the
     * library can insure that when the application opens a file twice
     * that the two handles coordinate their operations appropriately.
     * Otherwise it is the application's responsibility to never open the
     * same file more than once at a time.
     */
    if(NULL == (drvr = H5FD_get_class(fapl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "unable to retrieve VFL class")

    /* Check the environment variable that determines if we care
     * about file locking. File locking should be used unless explicitly
     * disabled.
     */
    lock_env_var = HDgetenv("HDF5_USE_FILE_LOCKING");
    if(lock_env_var && !HDstrcmp(lock_env_var, "FALSE"))
        use_file_locking = FALSE;
    else
        use_file_locking = TRUE;

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
        if(tent_flags == flags) {
#ifndef H5_USING_MEMCHECKER
            time_t mytime = HDtime(NULL);

            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: time = %s, name = '%s', tent_flags = %x", HDctime(&mytime), name, tent_flags)
#else /* H5_USING_MEMCHECKER */
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: name = '%s', tent_flags = %x", name, tent_flags)
#endif /* H5_USING_MEMCHECKER */
        } /* end if */
        H5E_clear_stack(NULL);
        tent_flags = flags;
        if(NULL == (lf = H5FD_open(name, tent_flags, fapl_id, HADDR_UNDEF))) {
#ifndef H5_USING_MEMCHECKER
            time_t mytime = HDtime(NULL);

            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: time = %s, name = '%s', tent_flags = %x", HDctime(&mytime), name, tent_flags)
#else /* H5_USING_MEMCHECKER */
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: name = '%s', tent_flags = %x", name, tent_flags)
#endif /* H5_USING_MEMCHECKER */
        } /* end if */
    } /* end if */

    /* For VFD SWMR driver, retrieve the underlying vfd for the search in H5F__sfile_search() */
    if(H5FD_is_vfd_swmr_driver(lf))
        underlying_lf = H5FD_vfd_swmr_get_underlying_vfd(lf);
    else
        underlying_lf = lf;

    /* Is the file already open? */
    if((shared = H5F__sfile_search(underlying_lf)) != NULL) {
        /*
         * The file is already open, so use that one instead of the one we
         * just opened. We only one one H5FD_t* per file so one doesn't
         * confuse the other.  But fail if this request was to truncate the
         * file (since we can't do that while the file is open), or if the
         * request was to create a non-existent file (since the file already
         * exists), or if the new request adds write access (since the
         * readers don't expect the file to change under them), or if the
         * SWMR write/read access flags don't agree.
         */
        if(H5FD_close(lf) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to close low-level file info")
        if(flags & H5F_ACC_TRUNC)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to truncate a file which is already open")
        if(flags & H5F_ACC_EXCL)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file exists")
        if((flags & H5F_ACC_RDWR) && 0 == (shared->flags & H5F_ACC_RDWR))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file is already open for read-only")

        if((flags & H5F_ACC_SWMR_WRITE) && 0 == (shared->flags & H5F_ACC_SWMR_WRITE))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "SWMR write access flag not the same for file that is already open")
        if((flags & H5F_ACC_SWMR_READ) && !((shared->flags & H5F_ACC_SWMR_WRITE) || (shared->flags & H5F_ACC_SWMR_READ) || (shared->flags & H5F_ACC_RDWR)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "SWMR read access flag not the same for file that is already open")

        /* Allocate new "high-level" file struct */
        if((file = H5F__new(shared, flags, fcpl_id, fapl_id, NULL)) == NULL)
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
            if(H5FD_close(lf) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to close low-level file info")

            if(NULL == (lf = H5FD_open(name, flags, fapl_id, HADDR_UNDEF)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")
        } /* end if */

        /* Place an advisory lock on the file */
        if(use_file_locking)
            if(H5FD_lock(lf, (hbool_t)((flags & H5F_ACC_RDWR) ? TRUE : FALSE)) < 0) {
                /* Locking failed - Closing will remove the lock */
                if(H5FD_close(lf) < 0)
                    HDONE_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to close low-level file info")
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to lock the file")
            } /* end if */

        /* Create the 'top' file structure */
        if(NULL == (file = H5F__new(NULL, flags, fcpl_id, fapl_id, lf))) {
            /* If this is the only time the file has been opened and the struct
             * returned is NULL, H5FD_close() will never be called via H5F_dest()
             * so we have to close lf here before heading to the error handling.
             */
            if(H5FD_close(lf) < 0)
                HDONE_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to close low-level file info")
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to initialize file structure")
        } /* end if */

        /* Need to set status_flags in the superblock if the driver has a 'lock' method */
        if(drvr->lock)
            set_flag = TRUE;
    } /* end else */

    /* Check to see if both SWMR and cache image are requested.  Fail if so */
    if(H5C_cache_image_status(file, &ci_load, &ci_write) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get MDC cache image status")
    if((ci_load || ci_write) && (flags & (H5F_ACC_SWMR_READ | H5F_ACC_SWMR_WRITE)))
        HGOTO_ERROR(H5E_FILE, H5E_UNSUPPORTED, NULL, "can't have both SWMR and cache image")

    /* Retain the name the file was opened with */
    file->open_name = H5MM_xstrdup(name);

    /* Short cuts */
    shared = file->shared;
    lf = shared->lf;

    /* Get the file access property list, for future queries */
    if(NULL == (a_plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not file access property list")

    /* Check if page buffering is enabled */
    if(H5P_get(a_plist, H5F_ACS_PAGE_BUFFER_SIZE_NAME, &page_buf_size) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get page buffer size")
    if(page_buf_size) {
#ifdef H5_HAVE_PARALLEL
        /* Collective metadata writes are not supported with page buffering */
        if(file->coll_md_write)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "collective metadata writes are not supported with page buffering")

        /* Temporary: fail file create when page buffering feature is enabled for parallel */
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "page buffering is disabled for parallel")
#endif /* H5_HAVE_PARALLEL */
        /* Query for other page buffer cache properties */
        if(H5P_get(a_plist, H5F_ACS_PAGE_BUFFER_MIN_META_PERC_NAME, &page_buf_min_meta_perc) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get minimum metadata fraction of page buffer")
        if(H5P_get(a_plist, H5F_ACS_PAGE_BUFFER_MIN_RAW_PERC_NAME, &page_buf_min_raw_perc) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get minimum raw data fraction of page buffer")
    } /* end if */


    /*
     * Read or write the file superblock, depending on whether the file is
     * empty or not.
     */
    if(0 == (MAX(H5FD_get_eof(lf, H5FD_MEM_SUPER), H5FD_get_eoa(lf, H5FD_MEM_SUPER))) && (flags & H5F_ACC_RDWR)) {
        /*
         * We've just opened a fresh new file (or truncated one). We need
         * to create & write the superblock.
         */

        /* Create the page buffer before initializing the superblock */
        if(page_buf_size)
            if(H5PB_create(file, page_buf_size, page_buf_min_meta_perc, page_buf_min_raw_perc) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create page buffer")

        /* Initialize information about the superblock and allocate space for it */
        /* (Writes superblock extension messages, if there are any) */
        if(H5F__super_init(file) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to allocate file superblock")

        /* Create and open the root group */
        /* (This must be after the space for the superblock is allocated in
         *      the file, since the superblock must be at offset 0)
         */
        if(H5G_mkroot(file, TRUE) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create/open root group")

        file_create = TRUE;

    } /* end if */
    else if(1 == shared->nrefs) {
        /* Read the superblock if it hasn't been read before. */
        if(H5F__super_read(file, a_plist, TRUE) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_READERROR, NULL, "unable to read superblock")

        /* Create the page buffer before initializing the superblock */
        if(page_buf_size)
            if(H5PB_create(file, page_buf_size, page_buf_min_meta_perc, page_buf_min_raw_perc) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to create page buffer")

        /* Open the root group */
        if(H5G_mkroot(file, FALSE) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to read root group")

    } /* end if */

    /* Checked if configured for VFD SWMR */
    if(H5F_VFD_SWMR_CONFIG(file)) {
        /* Page buffering and page allocation strategy have to be enabled */
        if(!page_buf_size || !H5F_PAGED_AGGR(file))
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "file open fail with VFD SWMR writer")

        /* Initialization for VFD SWMR writer and reader */
        if(1 == shared->nrefs) {
            if(H5F__vfd_swmr_init(file, file_create) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTSET, NULL, "file open fail with initialization for VFD SWMR")
        }
    }

    /*
     * Decide the file close degree.  If it's the first time to open the
     * file, set the degree to access property list value; if it's the
     * second time or later, verify the access property list value matches
     * the degree in shared file structure.
     */
    if(H5P_get(a_plist, H5F_ACS_CLOSE_DEGREE_NAME, &fc_degree) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get file close degree")

    /* This is a private property to clear the status_flags in the super block */
    /* Use by h5clear and a routine in test/flush2.c to clear the test file's status_flags */
    if(H5P_exist_plist(a_plist, H5F_ACS_CLEAR_STATUS_FLAGS_NAME) > 0) {
        if(H5P_get(a_plist, H5F_ACS_CLEAR_STATUS_FLAGS_NAME, &clear) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get clearance for status_flags")
        else if(clear)
            file->shared->sblock->status_flags = 0;
    } /* end if */

    if(shared->nrefs == 1) {
        if(fc_degree == H5F_CLOSE_DEFAULT)
            shared->fc_degree = lf->cls->fc_degree;
        else
            shared->fc_degree = fc_degree;
    } /* end if */
    else if(shared->nrefs > 1) {
        if(fc_degree == H5F_CLOSE_DEFAULT && shared->fc_degree != lf->cls->fc_degree)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "file close degree doesn't match")
        if(fc_degree != H5F_CLOSE_DEFAULT && fc_degree != shared->fc_degree)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "file close degree doesn't match")
    } /* end if */

    /* Record the evict-on-close MDC behavior.  If it's the first time opening
     * the file, set it to access property list value; if it's the second time
     * or later, verify that the access property list value matches the value
     * in shared file structure.
     */
    if(H5P_get(a_plist, H5F_ACS_EVICT_ON_CLOSE_FLAG_NAME, &evict_on_close) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get evict on close value")
    if(shared->nrefs == 1)
        shared->evict_on_close = evict_on_close;
    else if(shared->nrefs > 1) {
        if(shared->evict_on_close != evict_on_close)
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "file evict-on-close value doesn't match")
    } /* end if */

    /* Formulate the absolute path for later search of target file for external links */
    if(H5_build_extpath(name, &file->extpath) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to build extpath")

    /* Formulate the actual file name, after following symlinks, etc. */
    if(H5F__build_actual_name(file, a_plist, name, &file->actual_name) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to build actual name")

    if(set_flag) {
        if(H5F_INTENT(file) & H5F_ACC_RDWR) { /* Set and check consistency of status_flags */
            /* Skip check of status_flags for file with < superblock version 3 */
            if(file->shared->sblock->super_vers >= HDF5_SUPERBLOCK_VERSION_3) {

                if(file->shared->sblock->status_flags & H5F_SUPER_WRITE_ACCESS ||
                        file->shared->sblock->status_flags & H5F_SUPER_SWMR_WRITE_ACCESS)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file is already open for write/SWMR write (may use <h5clear file> to clear file consistency flags)")
            } /* version 3 superblock */

            file->shared->sblock->status_flags |= H5F_SUPER_WRITE_ACCESS;
            if(H5F_INTENT(file) & H5F_ACC_SWMR_WRITE || H5F_USE_VFD_SWMR(file))
                file->shared->sblock->status_flags |= H5F_SUPER_SWMR_WRITE_ACCESS;

            /* Flush the superblock & superblock extension */
            if(H5F_super_dirty(file) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTMARKDIRTY, NULL, "unable to mark superblock as dirty")
            if(H5F_flush_tagged_metadata(file, H5AC__SUPERBLOCK_TAG) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, NULL, "unable to flush superblock")
            if(H5F_flush_tagged_metadata(file, file->shared->sblock->ext_addr) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, NULL, "unable to flush superblock extension")

            /* Remove the file lock for SWMR_WRITE */
            if(use_file_locking && ((H5F_INTENT(file) & H5F_ACC_SWMR_WRITE) || H5F_USE_VFD_SWMR(file))) {
                if(H5FD_unlock(file->shared->lf) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to unlock the file")
            } /* end if */
        } /* end if */
        else { /* H5F_ACC_RDONLY: check consistency of status_flags */
            /* Skip check of status_flags for file with < superblock version 3 */
            if(file->shared->sblock->super_vers >= HDF5_SUPERBLOCK_VERSION_3) {
                if(H5F_INTENT(file) & H5F_ACC_SWMR_READ || H5F_USE_VFD_SWMR(file)) {
                    if((file->shared->sblock->status_flags & H5F_SUPER_WRITE_ACCESS &&
                            !(file->shared->sblock->status_flags & H5F_SUPER_SWMR_WRITE_ACCESS))
                            ||
                            (!(file->shared->sblock->status_flags & H5F_SUPER_WRITE_ACCESS) &&
                            file->shared->sblock->status_flags & H5F_SUPER_SWMR_WRITE_ACCESS))
                        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file is not already open for SWMR writing")
                } /* end if */
                else if((file->shared->sblock->status_flags & H5F_SUPER_WRITE_ACCESS) ||
                        (file->shared->sblock->status_flags & H5F_SUPER_SWMR_WRITE_ACCESS))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file is already open for write (may use <h5clear file> to clear file consistency flags)")
            } /* version 3 superblock */
        } /* end else */
    } /* end if set_flag */

    /* Success */
    ret_value = file;

done:
    if((NULL == ret_value) && file) {
        if(file->shared->root_grp && file->shared->nrefs == 1) {
            if(H5AC_expunge_tag_type_metadata(file, H5G_oloc(file->shared->root_grp)->addr, H5AC_OHDR_ID, H5AC__NO_FLAGS_SET, FALSE) < 0)
                HDONE_ERROR(H5E_FILE, H5E_CANTEXPUNGE, NULL, "unable to expunge root group tagged entries")
        }

        if(H5F__dest(file, FALSE) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_open() */


/*-------------------------------------------------------------------------
 * Function:    H5F_flush_phase1
 *
 * Purpose:     First phase of flushing cached data.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__flush_phase1(H5F_t *f)
{
    herr_t   ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check arguments */
    HDassert(f);

    /* Flush any cached dataset storage raw data */
    if(H5D_flush_all(f) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush dataset cache")

    /* Release any space allocated to space aggregators, so that the eoa value
     *  corresponds to the end of the space written to in the file.
     */
    /* (needs to happen before cache flush, with superblock write, since the
     *  'eoa' value is written in superblock -QAK)
     */
    if(H5MF_free_aggrs(f) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release file space")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__flush_phase1() */


/*-------------------------------------------------------------------------
 * Function:    H5F__flush_phase2
 *
 * Purpose:     Second phase of flushing cached data.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__flush_phase2(H5F_t *f, hbool_t closing)
{
    herr_t   ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check arguments */
    HDassert(f);

    /* Flush the entire metadata cache */
    if(H5AC_flush(f) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush metadata cache")

#ifdef H5_HAVE_PARALLEL
    if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI)) {
        /* Since we just returned from a call to H5AC_flush(), we just
         * passed through a barrier.  Hence we can skip the barrier on
         * entry to the mpio file driver truncate call below, and the first
         * barrier in the following call to flush the cache again.
         */
        H5CX_set_mpi_file_flushing(TRUE);
    }
#endif /* H5_HAVE_PARALLEL */

    /* Truncate the file to the current allocated size */
    if(H5FD_truncate(f->shared->lf, closing) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "low level truncate failed")

    /* Flush the entire metadata cache again since the EOA could have changed in the truncate call. */
    if(H5AC_flush(f) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush metadata cache")

#ifdef H5_HAVE_PARALLEL
    if(H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI))
        /* Reset the "flushing the file" flag */
        H5CX_set_mpi_file_flushing(FALSE);
#endif /* H5_HAVE_PARALLEL */

    /* Flush out the metadata accumulator */
    if(H5F__accum_flush(f) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_IO, H5E_CANTFLUSH, FAIL, "unable to flush metadata accumulator")

    /* Flush the page buffer */
    if(H5PB_flush(f) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_IO, H5E_CANTFLUSH, FAIL, "page buffer flush failed")

    /* Flush file buffers to disk. */
    if(H5FD_flush(f->shared->lf, closing) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_IO, H5E_CANTFLUSH, FAIL, "low level flush failed")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__flush_phase2() */


/*-------------------------------------------------------------------------
 * Function:    H5F__flush
 *
 * Purpose:     Flushes cached data.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__flush(H5F_t *f)
{
    herr_t   ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check arguments */
    HDassert(f);

    /* First phase of flushing data */
    if(H5F__flush_phase1(f) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush file data")

    /* Second phase of flushing data */
    if(H5F__flush_phase2(f, FALSE) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush file data")

    /* VFD SWMR when flushing the HDF5 file */
    if(f->shared->nrefs == 1 && f->shared->vfd_swmr_writer && f->shared->vfd_swmr_md_fd >= 0) {
        HDassert(H5F_ACC_RDWR & H5F_INTENT(f));

        if(H5F__vfd_swmr_close_or_flush(f, FALSE) < 0)
            HDONE_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "unable to encode and write to the metadata file")
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__flush() */


/*-------------------------------------------------------------------------
 * Function:    H5F__close
 *
 * Purpose:     Internal routine to close a file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__close(hid_t file_id)
{
    H5F_t       *f;                     /* File pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_PACKAGE

    /* Flush file if this is the last reference to this id and we have write
     * intent, unless it will be flushed by the "shared" file being closed.
     * This is only necessary to replicate previous behaviour, and could be
     * disabled by an option/property to improve performance.
     */
    if(NULL == (f = (H5F_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_FILE, H5E_BADTYPE, FAIL, "invalid file identifier")
    if((f->shared->nrefs > 1) && (H5F_INTENT(f) & H5F_ACC_RDWR)) {
        int nref;       /* Number of references to file ID */

        if((nref = H5I_get_ref(file_id, FALSE)) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get ID ref count")
        if(nref == 1)
            if(H5F__flush(f) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    } /* end if */

    /* Decrement reference count on file ID */
    /* (When it reaches zero the file will be closed) */
    if(H5I_dec_app_ref(file_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "decrementing file ID failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__close() */


/*-------------------------------------------------------------------------
 * Function:    H5F_try_close
 *
 * Purpose:     Attempts to close a file due to one of several actions:
 *              - The reference count on the file ID dropped to zero
 *              - The last open object was closed in the file
 *              - The file was unmounted
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_try_close(H5F_t *f, hbool_t *was_closed /*out*/)
{
    unsigned            nopen_files = 0;        /* Number of open files in file/mount hierarchy */
    unsigned            nopen_objs = 0;         /* Number of open objects in file/mount hierarchy */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    /* Set the was_closed flag to the default value.
     * This flag lets downstream code know if the file struct is
     * still accessible and/or likely to contain useful data.
     * It's needed by the evict-on-close code. Clients can ignore
     * this value by passing in NULL.
     */
    if(was_closed)
        *was_closed = FALSE;

    /* Check if this file is already in the process of closing */
    if(f->closing) {
        if(was_closed)
            *was_closed = TRUE;
        HGOTO_DONE(SUCCEED)
    }

    /* Get the number of open objects and open files on this file/mount hierarchy */
    if(H5F__mount_count_ids(f, &nopen_files, &nopen_objs) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_MOUNT, FAIL, "problem checking mount hierarchy")

    /*
     * Close file according to close degree:
     *
     *  H5F_CLOSE_WEAK:    if there are still objects open, wait until
     *            they are all closed.
     *  H5F_CLOSE_SEMI:    if there are still objects open, return fail;
     *            otherwise, close file.
     *  H5F_CLOSE_STRONG:    if there are still objects open, close them
     *            first, then close file.
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
             * far and there are objects left open, bail out now.
             */
            HDassert(nopen_files == 0 && nopen_objs == 0);

            /* If we've gotten this far (ie. there are no open objects in the file), fall through to flush & close */
            break;

        case H5F_CLOSE_STRONG:
            /* If there are other open files in the hierarchy, we can leave now */
            if(nopen_files > 0)
                HGOTO_DONE(SUCCEED)

            /* If we've gotten this far (ie. there are no open file IDs in the file/mount hierarchy), fall through to flush & close */
            break;

        case H5F_CLOSE_DEFAULT:
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
            size_t obj_count;       /* # of open objects */
            hid_t objs[128];        /* Array of objects to close */
            herr_t result;          /* Local result from obj ID query */
            size_t u;               /* Local index variable */

            /* Get the list of IDs of open dataset, group, & attribute objects */
            while((result = H5F_get_obj_ids(f, H5F_OBJ_LOCAL | H5F_OBJ_DATASET | H5F_OBJ_GROUP | H5F_OBJ_ATTR, (int)(sizeof(objs) / sizeof(objs[0])), objs, FALSE, &obj_count)) <= 0
                    && obj_count != 0 ) {

                /* Try to close all the open objects in this file */
                for(u = 0; u < obj_count; u++)
                    if(H5I_dec_ref(objs[u]) < 0)
                        HGOTO_ERROR(H5E_ATOM, H5E_CLOSEERROR, FAIL, "can't close object")
            }
            if(result < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "H5F_get_obj_ids failed(1)")

            /* Get the list of IDs of open named datatype objects */
            /* (Do this separately from the dataset & attribute IDs, because
             * they could be using one of the named datatypes and then the
             * open named datatype ID will get closed twice)
             */
            while((result = H5F_get_obj_ids(f, H5F_OBJ_LOCAL | H5F_OBJ_DATATYPE, (int)(sizeof(objs) / sizeof(objs[0])), objs, FALSE, &obj_count)) <= 0
                    && obj_count != 0) {

                /* Try to close all the open objects in this file */
                for(u = 0; u < obj_count; u++)
                    if(H5I_dec_ref(objs[u]) < 0)
                        HGOTO_ERROR(H5E_ATOM, H5E_CLOSEERROR, FAIL, "can't close object")
            }
            if(result < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_obj_ids failed(2)")
        } /* end if */
    } /* end if */

    /* Check if this is a child file in a mounting hierarchy & proceed up the
     * hierarchy if so.
     */
    if(f->parent)
        if(H5F_try_close(f->parent, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close parent file")

    /* Unmount and close each child before closing the current file. */
    if(H5F__close_mounts(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't unmount child files")

    /* If there is more than one reference to the shared file struct and the
     * file has an external file cache, we should see if it can be closed.  This
     * can happen if a cycle is formed with external file caches.
     */
    if(f->shared->efc && (f->shared->nrefs > 1))
        if(H5F__efc_try_close(f) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't attempt to close EFC")

    /* Delay flush until the shared file struct is closed, in H5F__dest.  If the
     * application called H5Fclose, it would have been flushed in that function
     * (unless it will have been flushed in H5F_dest anyways).
     */

    /* Destroy the H5F_t struct and decrement the reference count for the
     * shared H5F_file_t struct. If the reference count for the H5F_file_t
     * struct reaches zero then destroy it also.
     */
    if(H5F__dest(f, TRUE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

    /* Since we closed the file, this should be set to TRUE */
    if(was_closed)
        *was_closed = TRUE;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_try_close() */


/*-------------------------------------------------------------------------
 * Function:    H5F__reopen
 *
 * Purpose:     Reopen a file.  The new file handle which is returned points
 *		        to the same file as the specified file handle.  Both handles
 *		        share caches and other information.  The only difference
 *		        between the handles is that the new handle is not mounted
 *		        anywhere and no files are mounted on it.
 *
 * Return:      Success:    A pointer to a file struct
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5F__reopen(H5F_t *f)
{
    H5F_t       *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    /* Get a new "top level" file struct, sharing the same "low level" file struct */
    if(NULL == (ret_value = H5F__new(f->shared, 0, H5P_FILE_CREATE_DEFAULT, H5P_FILE_ACCESS_DEFAULT, NULL)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to reopen file")

    /* Duplicate old file's names */
    ret_value->open_name    = H5MM_xstrdup(f->open_name);
    ret_value->actual_name  = H5MM_xstrdup(f->actual_name);
    ret_value->extpath      = H5MM_xstrdup(f->extpath);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__reopen() */


/*-------------------------------------------------------------------------
 * Function:    H5F_get_id
 *
 * Purpose:     Get the file ID, incrementing it, or "resurrecting" it as
 *              appropriate.
 *
 * Return:      Success:    An ID for a file
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5F_get_id(H5F_t *file, hbool_t app_ref)
{
    hid_t ret_value = H5I_INVALID_HID;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    if(file->file_id == -1) {
        /* Get an atom for the file */
        if((file->file_id = H5I_register(H5I_FILE, file, app_ref)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize file")
    }
    else {
        /* Increment reference count on existing ID */
        if(H5I_inc_ref(file->file_id, app_ref) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTSET, H5I_INVALID_HID, "incrementing file ID failed")
    }

    ret_value = file->file_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_id() */


/*-------------------------------------------------------------------------
 * Function:    H5F_incr_nopen_objs
 *
 * Purpose:     Increment the number of open objects for a file.
 *
 * Return:      Success:    The number of open objects, after the increment
 *              Failure:    (can't happen)
 *-------------------------------------------------------------------------
 */
unsigned
H5F_incr_nopen_objs(H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(f);

    FUNC_LEAVE_NOAPI(++f->nopen_objs)
} /* end H5F_incr_nopen_objs() */


/*-------------------------------------------------------------------------
 * Function:    H5F_decr_nopen_objs
 *
 * Purpose:     Decrement the number of open objects for a file.
 *
 * Return:      Success:    The number of open objects, after the decrement
 *              Failure:    (can't happen)
 *-------------------------------------------------------------------------
 */
unsigned
H5F_decr_nopen_objs(H5F_t *f)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(f);

    FUNC_LEAVE_NOAPI(--f->nopen_objs)
} /* end H5F_decr_nopen_objs() */


/*-------------------------------------------------------------------------
 * Function:    H5F__build_actual_name
 *
 * Purpose:     Retrieve the name of a file, after following symlinks, etc.
 *
 * Note:        Currently only working for "POSIX I/O compatible" VFDs
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__build_actual_name(const H5F_t *f, const H5P_genplist_t *fapl, const char *name,
    char **actual_name/*out*/)
{
    hid_t       new_fapl_id = H5I_INVALID_HID;       /* ID for duplicated FAPL */
#ifdef H5_HAVE_SYMLINK
    /* This has to be declared here to avoid unfreed resources on errors */
    char *realname = NULL;              /* Fully resolved path name of file */
#endif /* H5_HAVE_SYMLINK */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(f);
    HDassert(fapl);
    HDassert(name);
    HDassert(actual_name);

    /* Clear actual name pointer to begin with */
    *actual_name = NULL;

/* Assume that if the OS can't create symlinks, that we don't need to worry
 *      about resolving them either. -QAK
 */
#ifdef H5_HAVE_SYMLINK
    /* Check for POSIX I/O compatible file handle */
    if(H5F_HAS_FEATURE(f, H5FD_FEAT_POSIX_COMPAT_HANDLE)) {
        h5_stat_t lst;   /* Stat info from lstat() call */

        /* Call lstat() on the file's name */
        if(HDlstat(name, &lst) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve stat info for file")

        /* Check for symbolic link */
        if(S_IFLNK == (lst.st_mode & S_IFMT)) {
            H5P_genplist_t *new_fapl;   /* Duplicated FAPL */
            int *fd;                    /* POSIX I/O file descriptor */
            h5_stat_t st;               /* Stat info from stat() call */
            h5_stat_t fst;              /* Stat info from fstat() call */
            hbool_t want_posix_fd;      /* Flag for retrieving file descriptor from VFD */

            /* Allocate realname buffer */
            if(NULL == (realname = (char *)H5MM_calloc((size_t)PATH_MAX * sizeof(char))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

            /* Perform a sanity check that the file or link wasn't switched
             * between when we opened it and when we called lstat().  This is
             * according to the security best practices for lstat() documented
             * here: https://www.securecoding.cert.org/confluence/display/seccode/POS35-C.+Avoid+race+conditions+while+checking+for+the+existence+of+a+symbolic+link
             */

            /* Copy the FAPL object to modify */
            if((new_fapl_id = H5P_copy_plist(fapl, FALSE)) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "unable to copy file access property list")
            if(NULL == (new_fapl = (H5P_genplist_t *)H5I_object(new_fapl_id)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL, "can't get property list")

            /* Set the character encoding on the new property list */
            want_posix_fd = TRUE;
            if(H5P_set(new_fapl, H5F_ACS_WANT_POSIX_FD_NAME, &want_posix_fd) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set character encoding")

            /* Retrieve the file handle */
            if(H5F_get_vfd_handle(f, new_fapl_id, (void **)&fd) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve POSIX file descriptor")

            /* Stat the filename we're resolving */
            if(HDstat(name, &st) < 0)
                HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to stat file")

            /* Stat the file we opened */
            if(HDfstat(*fd, &fst) < 0)
                HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to fstat file")

            /* Verify that the files are really the same */
            if(st.st_mode != fst.st_mode || st.st_ino != fst.st_ino || st.st_dev != fst.st_dev)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "files' st_ino or st_dev fields changed!")

            /* Get the resolved path for the file name */
            if(NULL == HDrealpath(name, realname))
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve real path for file")

            /* Duplicate the resolved path for the file name */
            if(NULL == (*actual_name = (char *)H5MM_strdup(realname)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "can't duplicate real path")
        } /* end if */
    } /* end if */
#endif /* H5_HAVE_SYMLINK */

    /* Check if we've resolved the file's name */
    if(NULL == *actual_name) {
        /* Just duplicate the name used to open the file */
        if(NULL == (*actual_name = (char *)H5MM_strdup(name)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "can't duplicate open name")
    } /* end else */

done:
    /* Close the property list */
    if(new_fapl_id > 0)
        if(H5I_dec_app_ref(new_fapl_id) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, FAIL, "can't close duplicated FAPL")
#ifdef H5_HAVE_SYMLINK
    if(realname)
        realname = (char *)H5MM_xfree(realname);
#endif /* H5_HAVE_SYMLINK */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__build_actual_name() */


/*-------------------------------------------------------------------------
 * Function:    H5F_addr_encode_len
 *
 * Purpose:     Encodes an address into the buffer pointed to by *PP and
 *              then increments the pointer to the first byte after the
 *              address.  An undefined value is stored as all 1's.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
H5F_addr_encode_len(size_t addr_len, uint8_t **pp/*in,out*/, haddr_t addr)
{
    unsigned    u;              /* Local index variable */

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(addr_len);
    HDassert(pp && *pp);

    if(H5F_addr_defined(addr)) {
        for(u = 0; u < addr_len; u++) {
            *(*pp)++ = (uint8_t)(addr & 0xff);
            addr >>= 8;
        } /* end for */
        HDassert("overflow" && 0 == addr);
    } /* end if */
    else {
        for(u = 0; u < addr_len; u++)
            *(*pp)++ = 0xff;
    } /* end else */

    FUNC_LEAVE_NOAPI_VOID
} /* end H5F_addr_encode_len() */


/*-------------------------------------------------------------------------
 * Function:    H5F_addr_encode
 *
 * Purpose:     Encodes an address into the buffer pointed to by *PP and
 *              then increments the pointer to the first byte after the
 *              address.  An undefined value is stored as all 1's.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
H5F_addr_encode(const H5F_t *f, uint8_t **pp/*in,out*/, haddr_t addr)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(f);

    H5F_addr_encode_len(H5F_SIZEOF_ADDR(f), pp, addr);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5F_addr_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5F_addr_decode_len
 *
 * Purpose:     Decodes an address from the buffer pointed to by *PP and
 *              updates the pointer to point to the next byte after the
 *              address.
 *
 *              If the value read is all 1's then the address is returned
 *              with an undefined value.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
H5F_addr_decode_len(size_t addr_len, const uint8_t **pp/*in,out*/, haddr_t *addr_p/*out*/)
{
    hbool_t        all_zero = TRUE;    /* True if address was all zeroes */
    unsigned       u;                  /* Local index variable */

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(addr_len);
    HDassert(pp && *pp);
    HDassert(addr_p);

    /* Reset value in destination */
    *addr_p = 0;

    /* Decode bytes from address */
    for(u = 0; u < addr_len; u++) {
        uint8_t        c;          /* Local decoded byte */

        /* Get decoded byte (and advance pointer) */
        c = *(*pp)++;

        /* Check for non-undefined address byte value */
        if(c != 0xff)
            all_zero = FALSE;

        if(u < sizeof(*addr_p)) {
            haddr_t        tmp = c;    /* Local copy of address, for casting */

            /* Shift decoded byte to correct position */
            tmp <<= (u * 8);    /*use tmp to get casting right */

            /* Merge into already decoded bytes */
            *addr_p |= tmp;
        } /* end if */
        else
            if(!all_zero)
                HDassert(0 == **pp);    /*overflow */
    } /* end for */

    /* If 'all_zero' is still TRUE, the address was entirely composed of '0xff'
     *  bytes, which is the encoded form of 'HADDR_UNDEF', so set the destination
     *  to that value */
    if(all_zero)
        *addr_p = HADDR_UNDEF;

    FUNC_LEAVE_NOAPI_VOID
} /* end H5F_addr_decode_len() */


/*-------------------------------------------------------------------------
 * Function:    H5F_addr_decode
 *
 * Purpose:     Decodes an address from the buffer pointed to by *PP and
 *              updates the pointer to point to the next byte after the
 *              address.
 *
 *              If the value read is all 1's then the address is returned
 *              with an undefined value.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
H5F_addr_decode(const H5F_t *f, const uint8_t **pp/*in,out*/, haddr_t *addr_p/*out*/)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(f);

    H5F_addr_decode_len(H5F_SIZEOF_ADDR(f), pp, addr_p);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5F_addr_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5F_set_grp_btree_shared
 *
 * Purpose:     Set the grp_btree_shared field with a valid ref-count pointer.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F_set_grp_btree_shared(H5F_t *f, H5UC_t *rc)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);
    HDassert(rc);

    f->shared->grp_btree_shared = rc;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5F_set_grp_btree_shared() */


/*-------------------------------------------------------------------------
 * Function:    H5F_set_sohm_addr
 *
 * Purpose:     Set the sohm_addr field with a new value.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F_set_sohm_addr(H5F_t *f, haddr_t addr)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    f->shared->sohm_addr = addr;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5F_set_sohm_addr() */


/*-------------------------------------------------------------------------
 * Function:    H5F_set_sohm_vers
 *
 * Purpose:     Set the sohm_vers field with a new value.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F_set_sohm_vers(H5F_t *f, unsigned vers)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    f->shared->sohm_vers = vers;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5F_set_sohm_vers() */


/*-------------------------------------------------------------------------
 * Function:    H5F_set_sohm_nindexes
 *
 * Purpose:     Set the sohm_nindexes field with a new value.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F_set_sohm_nindexes(H5F_t *f, unsigned nindexes)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    f->shared->sohm_nindexes = nindexes;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5F_set_sohm_nindexes() */


/*-------------------------------------------------------------------------
 * Function:    H5F_set_store_msg_crt_idx
 *
 * Purpose:     Set the store_msg_crt_idx field with a new value.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F_set_store_msg_crt_idx(H5F_t *f, hbool_t flag)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    f->shared->store_msg_crt_idx = flag;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5F_set_store_msg_crt_idx() */


/*-------------------------------------------------------------------------
 * Function:    H5F__set_libver_bounds()
 *
 * Purpose:     Set the file's low and high bound to the input parameters
 *              'low' and 'high' respectively.
 *              This is done only if the existing setting is different
 *              from the inputs.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi; December 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__set_libver_bounds(H5F_t *f, H5F_libver_t low, H5F_libver_t high)
{
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);

    /* Set the bounds only if the existing setting is different from the inputs */
    if(f->shared->low_bound != low || f->shared->high_bound != high) {
        /* Call the flush routine, for this file */
        /* Note: This is done in case the binary format for representing a
         *      metadata entry class changes when the file format low / high
         *      bounds are changed and an unwritten entry of that class is
         *      sitting in the metadata cache.
         *
         *      If that happens, it's possible that the entry's size could
         *      become larger, potentially corrupting the file (if the larger
         *      entry is fully written, overwriting data outside its allocated
         *      space), or corrupting the entry (if the entry is truncated to
         *      fit into the allocated space).
         *
         *      Although I'm not aware of any metadata with this behavior
         *      currently, it would be very difficult to guard against and / or
         *      detect, but if we flush everything here, the format version
         *      for metadata entries in the cache will be finalized and these
         *      sorts of problems can be avoided.
         *
         *      QAK - April, 2018
         */
        if(H5F__flush(f) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file's cached information")

        /* Set the new bounds */
        f->shared->low_bound = low;
        f->shared->high_bound = high;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__set_libver_bounds() */


/*-------------------------------------------------------------------------
 * Function:    H5F__get_file_image
 *
 * Purpose:     Private version of H5Fget_file_image
 *
 * Return:      Success:        Bytes copied / number of bytes needed.
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
ssize_t
H5F__get_file_image(H5F_t *file, void *buf_ptr, size_t buf_len)
{
    H5FD_t     *fd_ptr;                 /* file driver */
    haddr_t     eoa;                    /* End of file address */
    ssize_t     ret_value = -1;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    if(!file || !file->shared || !file->shared->lf)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, (-1), "file_id yields invalid file pointer")
    fd_ptr = file->shared->lf;
    if(!fd_ptr->cls)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, (-1), "fd_ptr yields invalid class pointer")

    /* the address space used by the split and multi file drivers is not
     * a good fit for this call.  Since the plan is to depreciate these
     * drivers anyway, don't bother to do a "force fit".
     *
     * The following clause tests for the multi file driver, and fails
     * if the supplied file has the multi file driver as its top level
     * file driver.  However, this test will not work if there is some
     * other file driver sitting on top of the multi file driver.
     *
     * I'm not sure if this is possible at present, but in all likelyhood,
     * it will become possible in the future.  On the other hand, we may
     * remove the split/multi file drivers before then.
     *
     * I am leaving this solution in for now, but we should review it,
     * and improve the solution if necessary.
     *
     *                                          JRM -- 11/11/22
     */
    if(HDstrcmp(fd_ptr->cls->name, "multi") == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, (-1), "Not supported for multi file driver.")

    /* While the family file driver is conceptually fully compatible
     * with the get file image operation, it sets a file driver message
     * in the super block that prevents the image being opened with any
     * driver other than the family file driver.  Needless to say, this
     * rather defeats the purpose of the get file image operation.
     *
     * While this problem is quire solvable, the required time and
     * resources are lacking at present.  Hence, for now, we don't
     * allow the get file image operation to be perfomed on files
     * opened with the family file driver.
     *
     * Observe that the following test only looks at the top level
     * driver, and fails if there is some other driver sitting on to
     * of the family file driver.
     *
     * I don't think this can happen at present, but that may change
     * in the future.
     *                                   JRM -- 12/21/11
     */
    if(HDstrcmp(fd_ptr->cls->name, "family") == 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, (-1), "Not supported for family file driver.")

    /* Go get the actual file size */
    if(HADDR_UNDEF == (eoa = H5FD_get_eoa(file->shared->lf, H5FD_MEM_DEFAULT)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, (-1), "unable to get file size")

    /* set ret_value = to eoa -- will overwrite this if appropriate */
    ret_value = (ssize_t)eoa;

    /* test to see if a buffer was provided -- if not, we are done */
    if(buf_ptr != NULL) {
        size_t    space_needed;        /* size of file image */
        hsize_t tmp;
        size_t tmp_size;

        /* Check for buffer too small */
        if((haddr_t)buf_len < eoa)
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, (-1), "supplied buffer too small")

        space_needed = (size_t)eoa;

        /* read in the file image */
        /* (Note compensation for base address addition in internal routine) */
        if(H5FD_read(fd_ptr, H5FD_MEM_DEFAULT, 0, space_needed, buf_ptr) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_READERROR, (-1), "file image read request failed")

        /* Offset to "status_flags" in the superblock */
        tmp = H5F_SUPER_STATUS_FLAGS_OFF(file->shared->sblock->super_vers);
        /* Size of "status_flags" depends on the superblock version */
        tmp_size = H5F_SUPER_STATUS_FLAGS_SIZE(file->shared->sblock->super_vers);

        /* Clear "status_flags" */
        HDmemset((uint8_t *)(buf_ptr) + tmp, 0, tmp_size);

    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__get_file_image() */


/*-------------------------------------------------------------------------
 * Function:    H5F__get_info
 *
 * Purpose:     Private version of H5Fget_info
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F__get_info(H5F_t *f, H5F_info2_t *finfo)
{
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);
    HDassert(finfo);

    /* Reset file info struct */
    HDmemset(finfo, 0, sizeof(*finfo));

    /* Get the size of the superblock and any superblock extensions */
    if(H5F__super_size(f, &finfo->super.super_size, &finfo->super.super_ext_size) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to retrieve superblock sizes")

    /* Get the size of any persistent free space */
    if(H5MF_get_freespace(f, &finfo->free.tot_space, &finfo->free.meta_size) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to retrieve free space information")

    /* Check for SOHM info */
    if(H5F_addr_defined(f->shared->sohm_addr))
        if(H5SM_ih_size(f, &finfo->sohm.hdr_size, &finfo->sohm.msgs_info) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to retrieve SOHM index & heap storage info")

    /* Set version # fields */
    finfo->super.version = f->shared->sblock->super_vers;
    finfo->sohm.version = f->shared->sohm_vers;
    finfo->free.version = HDF5_FREESPACE_VERSION;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__get_info() */


/*-------------------------------------------------------------------------
 * Function:    H5F_track_metadata_read_retries
 *
 * Purpose:     To track the # of a "retries" (log10) for a metadata item.
 *              This routine should be used only when:
 *              "retries" > 0
 *              f->shared->read_attempts > 1 (does not have retry when 1)
 *              f->shared->retries_nbins > 0 (calculated based on f->shared->read_attempts)
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F_track_metadata_read_retries(H5F_t *f, unsigned actype, unsigned retries)
{
    unsigned log_ind;            /* Index to the array of retries based on log10 of retries */
    double tmp;                  /* Temporary value, to keep compiler quiet */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared->read_attempts > 1);
    HDassert(f->shared->retries_nbins > 0);
    HDassert(retries > 0);
    HDassert(retries < f->shared->read_attempts);
    HDassert(actype < H5AC_NTYPES);

    /* Allocate memory for retries */
    if(NULL == f->shared->retries[actype])
        if(NULL == (f->shared->retries[actype] = (uint32_t *)H5MM_calloc((size_t)f->shared->retries_nbins * sizeof(uint32_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Index to retries based on log10 */
    tmp = HDlog10((double)retries);
    log_ind = (unsigned)tmp;
    HDassert(log_ind < f->shared->retries_nbins);

    /* Increment the # of the "retries" */
    f->shared->retries[actype][log_ind]++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F_track_metadata_read_retries() */


/*-------------------------------------------------------------------------
 * Function:    H5F_set_retries
 *
 * Purpose:     To initialize data structures for read retries:
 *              --zero out "retries"
 *              --set up "retries_nbins" based on read_attempts
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F_set_retries(H5F_t *f)
{
    double tmp;                /* Temporary variable */

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);

    /* Initialize the tracking for metadata read retries */
    HDmemset(f->shared->retries, 0, sizeof(f->shared->retries));

    /* Initialize the # of bins for retries */
    f->shared->retries_nbins = 0;
    if(f->shared->read_attempts > 1) {
        tmp = HDlog10((double)(f->shared->read_attempts - 1));
        f->shared->retries_nbins = (unsigned)tmp + 1;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5F_set_retries() */


/*-------------------------------------------------------------------------
 * Function:    H5F_object_flush_cb
 *
 * Purpose:     To invoke the callback function for object flush that is set
 *              in the file's access property list.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F_object_flush_cb(H5F_t *f, hid_t obj_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    /* Invoke object flush callback if there is one */
    if(f->shared->object_flush.func && f->shared->object_flush.func(obj_id, f->shared->object_flush.udata) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "object flush callback returns error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F_object_flush_cb() */


/*-------------------------------------------------------------------------
 * Function: H5F__set_base_addr
 *
 * Purpose:  Quick and dirty routine to set the file's 'base_addr' value
 *
 * Return:   Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5F__set_base_addr(const H5F_t *f, haddr_t addr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(f);
    HDassert(f->shared);

    /* Dispatch to driver */
    if(H5FD_set_base_addr(f->shared->lf, addr) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "driver set_base_addr request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__set_base_addr() */


/*-------------------------------------------------------------------------
 * Function: H5F__set_eoa
 *
 * Purpose:  Quick and dirty routine to set the file's 'eoa' value
 *
 * Return:   Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5F__set_eoa(const H5F_t *f, H5F_mem_t type, haddr_t addr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(f);
    HDassert(f->shared);

    /* Dispatch to driver */
    if(H5FD_set_eoa(f->shared->lf, type, addr) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "driver set_eoa request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__set_eoa() */


/*-------------------------------------------------------------------------
 * Function: H5F__set_paged_aggr
 *
 * Purpose:  Quick and dirty routine to set the file's paged_aggr mode
 *
 * Return:   Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5F__set_paged_aggr(const H5F_t *f, hbool_t paged)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    /* Dispatch to driver */
    if(H5FD_set_paged_aggr(f->shared->lf, paged) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "driver set paged aggr mode failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__set_paged_aggr() */


/*-------------------------------------------------------------------------
 * Function:    H5F__get_max_eof_eoa
 *
 * Purpose:     Determine the maximum of (EOA, EOF) for the file
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F__get_max_eof_eoa(const H5F_t *f, haddr_t *max_eof_eoa)
{
    haddr_t eof;                /* Relative address for EOF */
    haddr_t eoa;                /* Relative address for EOA */
    haddr_t tmp_max;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);

    /* Get the relative EOA and EOF */
    eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_DEFAULT);
    eof = H5FD_get_eof(f->shared->lf, H5FD_MEM_DEFAULT);

    /* Determine the maximum */
    tmp_max = MAX(eof, eoa);
    if(HADDR_UNDEF == tmp_max)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "file get eof/eoa requests failed")

    *max_eof_eoa = tmp_max;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__get_max_eof_eoa() */

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:    H5F_set_coll_md_read
 *
 * Purpose:     Set the coll_md_read field with a new value.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
void
H5F_set_coll_md_read(H5F_t *f, H5P_coll_md_read_flag_t cmr)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);

    f->coll_md_read = cmr;

    FUNC_LEAVE_NOAPI_VOID
} /* H5F_set_coll_md_read() */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5F_get_metadata_read_retry_info
 *
 * Purpose:     Private function to retrieve the collection of read retries
 *              for metadata items with checksum.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_get_metadata_read_retry_info(H5F_t *file, H5F_retry_info_t *info)
{
    unsigned     i, j;            /* Local index variable */
    size_t       tot_size;        /* Size of each retries[i] */
    herr_t       ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(file);
    HDassert(info);

    /* Copy the # of bins for "retries" array */
    info->nbins = file->shared->retries_nbins;

    /* Initialize the array of "retries" */
    HDmemset(info->retries, 0, sizeof(info->retries));

    /* Return if there are no bins -- no retries */
    if(!info->nbins)
        HGOTO_DONE(SUCCEED);

    /* Calculate size for each retries[i] */
    tot_size = info->nbins * sizeof(uint32_t);

    /* Map and copy information to info's retries for metadata items with tracking for read retries */
    j = 0;
    for(i = 0; i < H5AC_NTYPES; i++) {
        switch (i) {
            case H5AC_OHDR_ID:
            case H5AC_OHDR_CHK_ID:
            case H5AC_BT2_HDR_ID:
            case H5AC_BT2_INT_ID:
            case H5AC_BT2_LEAF_ID:
            case H5AC_FHEAP_HDR_ID:
            case H5AC_FHEAP_DBLOCK_ID:
            case H5AC_FHEAP_IBLOCK_ID:
            case H5AC_FSPACE_HDR_ID:
            case H5AC_FSPACE_SINFO_ID:
            case H5AC_SOHM_TABLE_ID:
            case H5AC_SOHM_LIST_ID:
            case H5AC_EARRAY_HDR_ID:
            case H5AC_EARRAY_IBLOCK_ID:
            case H5AC_EARRAY_SBLOCK_ID:
            case H5AC_EARRAY_DBLOCK_ID:
            case H5AC_EARRAY_DBLK_PAGE_ID:
            case H5AC_FARRAY_HDR_ID:
            case H5AC_FARRAY_DBLOCK_ID:
            case H5AC_FARRAY_DBLK_PAGE_ID:
            case H5AC_SUPERBLOCK_ID:
                HDassert(j < H5F_NUM_METADATA_READ_RETRY_TYPES);
                if(file->shared->retries[i] != NULL) {
                    /* Allocate memory for retries[i]
                     *
                     * This memory should be released by the user with
                     * the H5free_memory() call.
                     */
                    if(NULL == (info->retries[j] = (uint32_t *)H5MM_malloc(tot_size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

                    /* Copy the information */
                    HDmemcpy(info->retries[j], file->shared->retries[i], tot_size);
                }

                /* Increment location in info->retries[] array */
                j++;
                break;

            default:
                break;
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_metadata_read_retry_info() */


/*-------------------------------------------------------------------------
 * Function:    H5F__start_swmr_write
 *
 * Purpose:     Private version of H5Fstart_swmr_write
 *
 *              1) Refresh opened objects: part 1
 *              2) Flush & reset accumulator
 *              3) Mark the file in SWMR writing mode
 *              4) Set metadata read attempts and retries info
 *              5) Disable accumulator
 *              6) Evict all cache entries except the superblock
 *              7) Refresh opened objects (part 2)
 *              8) Unlock the file
 *
 *              Pre-conditions:
 *
 *              1) The file being opened has v3 superblock
 *              2) The file is opened with H5F_ACC_RDWR
 *              3) The file is not already marked for SWMR writing
 *              4) Current implementaion for opened objects:
 *                  --only allow datasets and groups without attributes
 *                  --disallow named datatype with/without attributes
 *                  --disallow opened attributes attached to objects
 *
 * NOTE:        Currently, only opened groups and datasets are allowed
 *              when enabling SWMR via H5Fstart_swmr_write().
 *              Will later implement a different approach--
 *              set up flush dependency/proxy even for file opened without
 *              SWMR to resolve issues with opened objects.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__start_swmr_write(H5F_t *f)
{
    hbool_t ci_load = FALSE;        /* whether MDC ci load requested */
    hbool_t ci_write = FALSE;       /* whether MDC CI write requested */
    size_t grp_dset_count = 0;      /* # of open objects: groups & datasets */
    size_t nt_attr_count = 0;       /* # of opened named datatypes  + opened attributes */
    hid_t *obj_ids = NULL;          /* List of ids */
    H5G_loc_t *obj_glocs = NULL;    /* Group location of the object */
    H5O_loc_t *obj_olocs = NULL;    /* Object location */
    H5G_name_t *obj_paths = NULL;   /* Group hierarchy path */
    size_t u;                       /* Local index variable */
    hbool_t setup = FALSE;          /* Boolean flag to indicate whether SWMR setting is enabled */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    /* Should have write permission */
    if((H5F_INTENT(f) & H5F_ACC_RDWR) == 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    /* Check superblock version */
    if(f->shared->sblock->super_vers < HDF5_SUPERBLOCK_VERSION_3)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "file superblock version - should be at least 3")

    /* Check for correct file format version */
    if((f->shared->low_bound != H5F_LIBVER_V110) || (f->shared->high_bound != H5F_LIBVER_V110))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "file format version does not support SWMR - needs to be 1.10 or greater")

    /* Should not be marked for SWMR writing mode already */
    if(f->shared->sblock->status_flags & H5F_SUPER_SWMR_WRITE_ACCESS)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "file already in SWMR writing mode")

    /* Check to see if cache image is enabled.  Fail if so */
    if(H5C_cache_image_status(f, &ci_load, &ci_write) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get MDC cache image status")
    if(ci_load || ci_write )
        HGOTO_ERROR(H5E_FILE, H5E_UNSUPPORTED, FAIL, "can't have both SWMR and MDC cache image")

    /* Flush the superblock extension */
    if(H5F_flush_tagged_metadata(f, f->shared->sblock->ext_addr) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush superblock extension")

    /* Flush data buffers */
    if(H5F__flush(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file's cached information")

    /* Get the # of opened named datatypes and attributes */
    if(H5F_get_obj_count(f, H5F_OBJ_DATATYPE|H5F_OBJ_ATTR, FALSE, &nt_attr_count) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "H5F_get_obj_count failed")
    if(nt_attr_count)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "named datatypes and/or attributes opened in the file")

    /* Get the # of opened datasets and groups */
    if(H5F_get_obj_count(f, H5F_OBJ_GROUP|H5F_OBJ_DATASET, FALSE, &grp_dset_count) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "H5F_get_obj_count failed")

    if(grp_dset_count) {
        /* Allocate space for group and object locations */
        if((obj_ids = (hid_t *) H5MM_malloc(grp_dset_count * sizeof(hid_t))) == NULL)
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate buffer for hid_t")
        if((obj_glocs = (H5G_loc_t *) H5MM_malloc(grp_dset_count * sizeof(H5G_loc_t))) == NULL)
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate buffer for H5G_loc_t")
        if((obj_olocs = (H5O_loc_t *) H5MM_malloc(grp_dset_count * sizeof(H5O_loc_t))) == NULL)
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate buffer for H5O_loc_t")
        if((obj_paths = (H5G_name_t *) H5MM_malloc(grp_dset_count * sizeof(H5G_name_t))) == NULL)
            HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate buffer for H5G_name_t")

        /* Get the list of opened object ids (groups & datasets) */
        if(H5F_get_obj_ids(f, H5F_OBJ_GROUP|H5F_OBJ_DATASET, grp_dset_count, obj_ids, FALSE, &grp_dset_count) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "H5F_get_obj_ids failed")

        /* Refresh opened objects (groups, datasets) in the file */
        for(u = 0; u < grp_dset_count; u++) {
            H5O_loc_t *oloc;            /* object location */
            H5G_loc_t tmp_loc;

            /* Set up the id's group location */
            obj_glocs[u].oloc = &obj_olocs[u];
            obj_glocs[u].path = &obj_paths[u];
            H5G_loc_reset(&obj_glocs[u]);

            /* get the id's object location */
            if((oloc = H5O_get_loc(obj_ids[u])) == NULL)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an object")

            /* Make deep local copy of object's location information */
            H5G_loc(obj_ids[u], &tmp_loc);
            H5G_loc_copy(&obj_glocs[u], &tmp_loc, H5_COPY_DEEP);

            /* Close the object */
            if(H5I_dec_ref(obj_ids[u]) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEOBJ, FAIL, "decrementing object ID failed")
        } /* end for */
    } /* end if */

    /* Flush and reset the accumulator */
    if(H5F__accum_reset(f, TRUE) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTRESET, FAIL, "can't reset accumulator")

    /* Turn on SWMR write in shared file open flags */
    f->shared->flags |= H5F_ACC_SWMR_WRITE;

    /* Mark the file in SWMR writing mode */
    f->shared->sblock->status_flags |= H5F_SUPER_SWMR_WRITE_ACCESS;

    /* Set up metadata read attempts */
    f->shared->read_attempts = H5F_SWMR_METADATA_READ_ATTEMPTS;

    /* Initialize "retries" and "retries_nbins" */
    if(H5F_set_retries(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't set retries and retries_nbins")

    /* Turn off usage of accumulator */
    f->shared->feature_flags &= ~(unsigned)H5FD_FEAT_ACCUMULATE_METADATA;
    if(H5FD_set_feature_flags(f->shared->lf, f->shared->feature_flags) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set feature_flags in VFD")

    setup = TRUE;

    /* Mark superblock as dirty */
    if(H5F_super_dirty(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTMARKDIRTY, FAIL, "unable to mark superblock as dirty")

    /* Flush the superblock */
    if(H5F_flush_tagged_metadata(f, H5AC__SUPERBLOCK_TAG) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush superblock")

    /* Evict all flushed entries in the cache except the pinned superblock */
    if(H5F__evict_cache_entries(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to evict file's cached information")

    /* Refresh (reopen) the objects (groups & datasets) in the file */
    for(u = 0; u < grp_dset_count; u++) {
        if(H5O_refresh_metadata_reopen(obj_ids[u], &obj_glocs[u], TRUE) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CLOSEERROR, FAIL, "can't refresh-close object")
    }

    /* Unlock the file */
    if(H5FD_unlock(f->shared->lf) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to unlock the file")

done:
    if(ret_value < 0 && setup) {

        /* Re-enable accumulator */
        f->shared->feature_flags |= (unsigned)H5FD_FEAT_ACCUMULATE_METADATA;
        if(H5FD_set_feature_flags(f->shared->lf, f->shared->feature_flags) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set feature_flags in VFD")

        /* Reset the # of read attempts */
        f->shared->read_attempts = H5F_METADATA_READ_ATTEMPTS;
        if(H5F_set_retries(f) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't set retries and retries_nbins")

        /* Un-set H5F_ACC_SWMR_WRITE in shared open flags */
        f->shared->flags &= ~H5F_ACC_SWMR_WRITE;

        /* Unmark the file: not in SWMR writing mode */
        f->shared->sblock->status_flags &= (uint8_t)(~H5F_SUPER_SWMR_WRITE_ACCESS);

        /* Mark superblock as dirty */
        if(H5F_super_dirty(f) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTMARKDIRTY, FAIL, "unable to mark superblock as dirty")

        /* Flush the superblock */
        if(H5F_flush_tagged_metadata(f, H5AC__SUPERBLOCK_TAG) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush superblock")
    } /* end if */

    /* Free memory */
    if(obj_ids)
        H5MM_xfree(obj_ids);
    if(obj_glocs)
        H5MM_xfree(obj_glocs);
    if(obj_olocs)
        H5MM_xfree(obj_olocs);
    if(obj_paths)
        H5MM_xfree(obj_paths);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__start_swmr_write() */


/*-------------------------------------------------------------------------
 * Function:    H5F__format_convert
 *
 * Purpose:     Private version of H5Fformat_convert
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5F__format_convert(H5F_t *f)
{
    hbool_t mark_dirty = FALSE;        /* Whether to mark the file's superblock dirty */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(f);
    HDassert(f->shared);

    /* Check if the superblock should be downgraded */
    if(f->shared->sblock->super_vers > HDF5_SUPERBLOCK_VERSION_V18_LATEST) {
        f->shared->sblock->super_vers = HDF5_SUPERBLOCK_VERSION_V18_LATEST;
        mark_dirty = TRUE;
    }

    /* Check for persistent freespace manager, which needs to be downgraded */
    if(!(f->shared->fs_strategy == H5F_FILE_SPACE_STRATEGY_DEF &&
        f->shared->fs_persist == H5F_FREE_SPACE_PERSIST_DEF &&
        f->shared->fs_threshold == H5F_FREE_SPACE_THRESHOLD_DEF &&
        f->shared->fs_page_size == H5F_FILE_SPACE_PAGE_SIZE_DEF)) {

        /* Check to remove free-space manager info message from superblock extension */
        if(H5F_addr_defined(f->shared->sblock->ext_addr))
            if(H5F__super_ext_remove_msg(f, H5O_FSINFO_ID) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "error in removing message from superblock extension")

        /* Close freespace manager */
        if(H5MF_try_close(f) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "unable to free free-space address")

        /* Set non-persistent freespace manager */
        f->shared->fs_strategy = H5F_FILE_SPACE_STRATEGY_DEF;
        f->shared->fs_persist = H5F_FREE_SPACE_PERSIST_DEF;
        f->shared->fs_threshold = H5F_FREE_SPACE_THRESHOLD_DEF;
        f->shared->fs_page_size = H5F_FILE_SPACE_PAGE_SIZE_DEF;

        /* Indicate that the superblock should be marked dirty */
        mark_dirty = TRUE;
    } /* end if */

    /* Check if we should mark the superblock dirty */
    if(mark_dirty)
        /* Mark superblock as dirty */
        if(H5F_super_dirty(f) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTMARKDIRTY, FAIL, "unable to mark superblock as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__format_convert() */

/*
 * VFD SWMR
 */


/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_init
 *
 * Purpose:     Initialize globals and the corresponding fields in 
 *              file pointer.
 *
 *              For both VFD SWMR writer and reader:
 *
 *                  --set vfd_swmr_g to TRUE
 *                  --set vfd_swmr_file_g to f
 *                  --set end_of_tick to the current time + tick length
 *
 *              For VFD SWMR writer:
 *
 *                  --set vfd_swmr_writer_g to TRUE
 *                  --set tick_num_g to 0
 *                  --create the metadata file
 *                  --when opening an existing HDF5 file, write header and 
 *                    empty index in the metadata file
 *
 *              For VFD SWMR reader:
 *
 *                  --set vfd_swmr_writer_g to FALSE
 *                  --set tick_num_g to the current tick read from the 
 *                    metadata file
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_init(H5F_t *f, hbool_t file_create)
{
    hsize_t md_size;                /* Size of the metadata file */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_STATIC

    HDassert(H5F_VFD_SWMR_CONFIG(f));

    vfd_swmr_g = f->shared->vfd_swmr = TRUE;
    vfd_swmr_file_g = f;

    if(H5F_INTENT(f) & H5F_ACC_RDWR) {

        HDassert(f->shared->vfd_swmr_config.vfd_swmr_writer);

        vfd_swmr_writer_g = f->shared->vfd_swmr_writer = TRUE;
        tick_num_g = f->shared->tick_num = 0;

        /* Create the metadata file */
        if ( ((f->shared->vfd_swmr_md_fd = 
               HDopen(f->shared->vfd_swmr_config.md_file_path, O_CREAT|O_RDWR, 
                      H5_POSIX_CREATE_MODE_RW))) < 0 )

            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, \
                        "unable to create the metadata file")

        md_size = (hsize_t)f->shared->vfd_swmr_config.md_pages_reserved * 
                  f->shared->fs_page_size;

        /* Set the metadata file size to md_pages_reserved */
        if ( -1 == HDftruncate(f->shared->vfd_swmr_md_fd, (HDoff_t)md_size) )

            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, \
                        "truncate fail for the metadata file")

        /* Set eof for metadata file to md_pages_reserved */
        f->shared->vfd_swmr_md_eoa = (haddr_t)md_size;

        /* When opening an existing HDF5 file, create header and empty 
         * index in the metadata file 
         */
        if ( !file_create ) {

            if ( H5F__vfd_swmr_construct_write_md_hdr(f, 0) < 0 )

                HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, \
                            "fail to create header in md")

            if ( H5F__vfd_swmr_construct_write_md_idx(f, 0, NULL) < 0 )

                HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, \
                            "fail to create index in md")
        }

    } else { /* VFD SWMR reader  */

        HDassert(!f->shared->vfd_swmr_config.vfd_swmr_writer);

        vfd_swmr_writer_g = f->shared->vfd_swmr_writer = FALSE;

        /* Set tick_num_g to the current tick read from the metadata file */
        if ( H5FD_vfd_swmr_get_tick_and_idx(f->shared->lf, FALSE, 
                                            &tick_num_g, NULL, NULL) < 0 )

            HGOTO_ERROR(H5E_FILE, H5E_CANTLOAD, FAIL, \
                        "unable to load/decode metadata file")

        f->shared->tick_num = tick_num_g;
    }

    /* Update end_of_tick */
    if ( H5F__vfd_swmr_update_end_of_tick_and_tick_num(f, FALSE) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to update end of tick")

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_init() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_construct_write_md_hdr
 *
 * Purpose:     Encode and write header to the metadata file.
 *
 *              This is used by the VFD SWMR writer:
 *
 *                  --when opening an existing HDF5 file
 *                  --when closing the HDF5 file
 *                  --after flushing an HDF5 file
 *                  --when updating the metadata file
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_construct_write_md_hdr(H5F_t *f, uint32_t num_entries)
{
    uint8_t image[H5FD_MD_HEADER_SIZE]; /* Buffer for header */
    uint8_t *p = NULL;                  /* Pointer to buffer */
    uint32_t metadata_chksum;           /* Computed metadata checksum value */
    unsigned hdr_size = H5FD_MD_HEADER_SIZE;    /* Size of header and index */
    herr_t ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_STATIC

    /*
     * Encode metadata file header
     */
    p = image;

    /* Encode magic for header */
    HDmemcpy(p, H5FD_MD_HEADER_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    p += H5_SIZEOF_MAGIC;

    /* Encode page size, tick number, index offset, index length */
    UINT32ENCODE(p, f->shared->fs_page_size);
    UINT64ENCODE(p, f->shared->tick_num);
    UINT64ENCODE(p, hdr_size);
    UINT64ENCODE(p, H5FD_MD_INDEX_SIZE(num_entries));

    /* Calculate checksum for header */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - image), 0);

    /* Encode checksum for header */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity checks on header */
    HDassert((size_t)(p - image == hdr_size));

    /* Set to beginning of the file */
    if ( HDlseek(f->shared->vfd_swmr_md_fd, (HDoff_t)0, SEEK_SET) < 0 )

        HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, \
                    "unable to seek in metadata file")

    /* Write header to the metadata file */
    if ( HDwrite(f->shared->vfd_swmr_md_fd, image, hdr_size) !=  hdr_size )

        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, \
                    "error in writing header to metadata file")

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_construct_write_md_hdr() */


/*-------------------------------------------------------------------------

 * Function:    H5F__vfd_swmr_construct_write_md_idx
 *
 * Purpose:     Encode and write index to the metadata file.
 *
 *              This is used by the VFD SWMR writer:
 *
 *                  --when opening an existing HDF5 file
 *                  --when closing the HDF5 file
 *                  --after flushing an HDF5 file
 *                  --when updating the metadata file
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_construct_write_md_idx(H5F_t *f, uint32_t num_entries, 
    struct H5FD_vfd_swmr_idx_entry_t index[])
{
    uint8_t *image = NULL;      /* Pointer to buffer */
    uint8_t *p = NULL;          /* Pointer to buffer */
    uint32_t metadata_chksum;   /* Computed metadata checksum value */
    unsigned idx_size = H5FD_MD_INDEX_SIZE(num_entries);  /* Size of index */
    unsigned i;                 /* Local index variable */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    HDassert((num_entries!= 0 && index != NULL) || 
             (num_entries == 0 && index == NULL));

    /* Allocate space for the buffer to hold the index */
    if ( (image = (uint8_t *)HDmalloc(idx_size)) == NULL )

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "memory allocation failed for md index")

    /*
     * Encode metadata file index
     */
    p = image;

    /* Encode magic for index */
    HDmemcpy(p, H5FD_MD_INDEX_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    p += H5_SIZEOF_MAGIC;

    /* Encode tick number */
    UINT64ENCODE(p, f->shared->tick_num);

    /* Encode number of entries in index */
    UINT32ENCODE(p, num_entries);

    /* Encode the index entries */
    for(i = 0; i < num_entries; i++) {
        UINT32ENCODE(p, index[i].hdf5_page_offset); 
        UINT32ENCODE(p, index[i].md_file_page_offset);
        UINT32ENCODE(p, index[i].length); 
        UINT32ENCODE(p, index[i].chksum); 
    }

    /* Calculate checksum for index */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - image), 0);

    /* Encode checksum for index */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity checks on index */
    HDassert((size_t)(p - image ==  idx_size));

    /* Verify the md file descriptor exists */
    HDassert(f->shared->vfd_swmr_md_fd >= 0);

    /* Set to right after the header */
    if ( HDlseek(f->shared->vfd_swmr_md_fd, (HDoff_t)H5FD_MD_HEADER_SIZE, 
                 SEEK_SET) < 0)

        HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, \
                    "unable to seek in metadata file")

    /* Write index to the metadata file */
    if ( HDwrite(f->shared->vfd_swmr_md_fd, image, idx_size) != idx_size )

        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, \
                    "error in writing index to metadata file")

done:

    if ( image ) {

        HDfree(image);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_construct_write_idx() */



/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_update_end_of_tick_and_tick_num
 *
 * Purpose:     Update end_of_tick (end_of_tick_g, f->shared->end_of_tick)
 *              Update tick_num (tick_num_g, f->shared->tick_num)
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_update_end_of_tick_and_tick_num(H5F_t *f, hbool_t incr_tick_num)
{
    struct timespec curr;               /* Current time in struct timespec */
    struct timespec new_end_of_tick;    /* new end_of_tick in struct timespec */
    long curr_nsecs;                    /* current time in nanoseconds */
    long tlen_nsecs;                    /* tick_len in nanoseconds */
#if 0 /* JRM */
    long end_nsecs;                     /* end_of_tick in nanoseconds */
#endif /* JRM */
    long new_end_nsecs;                 /* new end_of_tick in nanoseconds */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Get current time in struct timespec */
    if ( HDclock_gettime(CLOCK_MONOTONIC, &curr) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, \
                    "can't get time via clock_gettime")

    /* Convert curr to nsecs */
    curr_nsecs = curr.tv_sec * SECOND_TO_NANOSECS + curr.tv_nsec;

    /* Convert tick_len to nanosecs */
    tlen_nsecs = f->shared->vfd_swmr_config.tick_len * TENTH_SEC_TO_NANOSECS;

    /* 
     *  Update tick_num_g, f->shared->tick_num 
     */
    if ( incr_tick_num ) {

#if 0 /* JRM */
        /* Convert end_of_tick_g to nanoseconds */
        end_nsecs = end_of_tick_g.tv_sec * SECOND_TO_NANOSECS + 
                                           end_of_tick_g.tv_nsec;

        /* Increment tick_num by # of elapsed ticks */
        tick_num_g += (1+ (uint64_t)((curr_nsecs - end_nsecs) / tlen_nsecs));
#else /* JRM */
        /* Regardless of elapsed time, only increment the tick num by 1
         * so as to avoid the possibility of using up all of max_lag in
         * one or two ticks.
         */
        tick_num_g++;
#endif /* JRM */
        f->shared->tick_num = tick_num_g;
    }

    /* 
     * Update end_of_tick_g, f->shared->end_of_tick
     */
    /* Calculate new end_of_tick */

    /* TODO: The modulo operation is very expensive on most machines -- 
     *       re-work this code so as to avoid it.
     *
     *                                    JRM -- 11/12/18
     */

    new_end_nsecs = curr_nsecs + tlen_nsecs;
    new_end_of_tick.tv_nsec = new_end_nsecs % SECOND_TO_NANOSECS;
    new_end_of_tick.tv_sec = new_end_nsecs / SECOND_TO_NANOSECS;

    /* Update end_of_tick */
    HDmemcpy(&end_of_tick_g, &new_end_of_tick, sizeof(struct timespec));
    HDmemcpy(&f->shared->end_of_tick, &new_end_of_tick, 
             sizeof(struct timespec));

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_update_end_of_tick_and_tick_num() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_close_or_flush
 *
 * Purpose:     Used by the VFD SWMR writer when the HDF5 file is closed 
 *              or flushed:
 *
 *              1) For file close:
 *                  --write header and an empty index to the metadata file
 *                  --increment tick_num
 *                  --close the metadata file
 *                  --unlink the metadata file
 *                  --close the free-space manager for the metadata file
 *
 *              2) For file flush:
 *                  --write header and an empty index to the metadata file
 *                  --increment tick_num 
 *                  --start a new tick (??check with JM for sure)
 *                    ??update end_of_tick
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_close_or_flush(H5F_t *f, hbool_t closing)
{
    H5F_vfd_swmr_dl_entry_t *curr, *next;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    HDassert(f->shared->vfd_swmr_writer);
    HDassert(f->shared->vfd_swmr_md_fd >= 0);

    /* Write empty index to the md file */
    if ( H5F__vfd_swmr_construct_write_md_idx(f, 0, NULL) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to create index in md")


    /* Write header to the md file */
    if ( H5F__vfd_swmr_construct_write_md_hdr(f, 0) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to create header in md")

    /* Increment tick_num */
    tick_num_g = ++f->shared->tick_num;

    if ( closing ) { /* For file close */

        /* Close the md file */
        if(HDclose(f->shared->vfd_swmr_md_fd) < 0)

            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, \
                        "unable to close the metadata file")
        f->shared->vfd_swmr_md_fd = -1;

        /* Unlink the md file */
        if ( HDunlink(f->shared->vfd_swmr_config.md_file_path) < 0 )

            HGOTO_ERROR(H5E_FILE, H5E_CANTREMOVE, FAIL, \
                        "unable to unlink the metadata file")

        /* Close the free-space manager for the metadata file */
        if ( H5MV_close(f) < 0 )

            HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, \
                "unable to close the free-space manager for the metadata file")

        /* Free the delayed list */ 
        curr = f->shared->dl_head_ptr;

        while ( curr != NULL ) {

            next = curr->next;
            curr = H5FL_FREE(H5F_vfd_swmr_dl_entry_t, curr);
            curr = next;

        } /* end while */

        f->shared->dl_head_ptr = f->shared->dl_tail_ptr = NULL;

        vfd_swmr_file_g = NULL;

    } else { /* For file flush */

        /* Update end_of_tick */
        if ( H5F__vfd_swmr_update_end_of_tick_and_tick_num(f, TRUE) < 0 )

            HDONE_ERROR(H5E_FILE, H5E_CANTSET, FAIL, \
                        "unable to update end of tick")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_close_or_flush() */


/*-------------------------------------------------------------------------
 * Function: H5F__idx_entry_cmp()
 *
 * Purpose:  Callback used by HDqsort to sort entries in the index
 *
 * Return:   0 if the entries are the same
 *           -1 if entry1's offset is less than that of entry2
 *           1 if entry1's offset is greater than that of entry2
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__idx_entry_cmp(const void *_entry1, const void *_entry2)
{
    const H5FD_vfd_swmr_idx_entry_t *entry1 = (const H5FD_vfd_swmr_idx_entry_t *)_entry1;
    const H5FD_vfd_swmr_idx_entry_t *entry2 = (const H5FD_vfd_swmr_idx_entry_t *)_entry2;

    int ret_value = 0;          /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(entry1);
    HDassert(entry2);

    if(entry1->hdf5_page_offset < entry2->hdf5_page_offset)
        ret_value = -1;
    else if(entry1->hdf5_page_offset > entry2->hdf5_page_offset)
        ret_value = 1;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__idx_entry_cmp() */

/*-------------------------------------------------------------------------
 *
 * Function: H5F_update_vfd_swmr_metadata_file()
 *
 * Purpose:  Update the metadata file with the input index
 *
 *           --Sort index
 *
 *           --For each non-null entry_ptr in the index entries:
 *               --Insert previous image of the entry onto the delayed list
 *               --Allocate space for the entry in the metadata file
 *               --Compute checksum
 *               --Update index entry
 *               --Write the entry to the metadata file
 *               --Set entry_ptr to NULL
 *
 *           --Construct on disk image of the index and write index to the 
 *             metadata file
 *
 *           --Construct on disk image of the header and write header to 
 *             the metadata file
 *
 *           --Release time out entries from the delayed list to the 
 *             free-space manager
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: Vailin Choi  11/??/18
 *
 * Changes:  None.
 *
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_update_vfd_swmr_metadata_file(H5F_t *f, uint32_t num_entries, 
    struct H5FD_vfd_swmr_idx_entry_t index[])
{
    H5F_vfd_swmr_dl_entry_t *prev;          /* Points to the previous entry 
                                             * in the delayed list 
                                             */
    H5F_vfd_swmr_dl_entry_t *dl_entry;      /* Points to an entry in the 
                                             * delayed list 
                                             */
    haddr_t md_addr;                        /* Address in the metadata file */
    unsigned i;                             /* Local index variable */
    herr_t ret_value = SUCCEED;             /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sort index entries by increasing offset in the HDF5 file */
    if ( num_entries ) {

        HDqsort(index, num_entries, sizeof(H5FD_vfd_swmr_idx_entry_t), 
                H5F__idx_entry_cmp);
    }

    /* For each non-null entry_ptr in the index:
     *
     *  --Insert previous image of the entry (if exists) to the 
     *    beginning of the delayed list
     *
     *  --Allocate space for the entry in the metadata file 
     *
     *  --Compute checksum, update the index entry, write entry to 
     *    the metadata file
     *
     *  --Set entry_ptr to NULL
     */
    for ( i = 0; i < num_entries; i++ ) {

        if ( index[i].entry_ptr != NULL ) {

            /* Prepend previous image of the entry to the delayed list */
            if ( index[i].md_file_page_offset ) {

                if ( NULL == (dl_entry = H5FL_CALLOC(H5F_vfd_swmr_dl_entry_t)))

                    HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, \
                                "unable to allocate the delayed entry")

                dl_entry->hdf5_page_offset = index[i].hdf5_page_offset;
                dl_entry->md_file_page_offset = index[i].md_file_page_offset;
                dl_entry->length = index[i].length;
                dl_entry->tick_num = f->shared->tick_num;

                H5F_DC_PREPEND(dl_entry, f->shared->dl_head_ptr, \
                               f->shared->dl_tail_ptr, f->shared->dl_len);
            }

            /* Allocate space for the entry in the metadata file */
            if((md_addr = H5MV_alloc(f, index[i].length)) == HADDR_UNDEF)

                HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, \
                            "error in allocating space from the metadata file")

            /* Compute checksum and update the index entry */
            index[i].md_file_page_offset = md_addr/f->shared->fs_page_size;
            index[i].chksum = H5_checksum_metadata(index[i].entry_ptr, 
                                                 (size_t)(index[i].length), 0);

            /* Seek and write the entry to the metadata file */
            if ( HDlseek(f->shared->vfd_swmr_md_fd, (HDoff_t)md_addr, 
                         SEEK_SET) < 0)

                HGOTO_ERROR(H5E_FILE, H5E_SEEKERROR, FAIL, \
                            "unable to seek in the metadata file")

            if ( HDwrite(f->shared->vfd_swmr_md_fd, index[i].entry_ptr, 
                         index[i].length) != index[i].length )

                HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, \
                 "error in writing the page/multi-page entry to metadata file")

            /* Set entry_ptr to NULL */
            index[i].entry_ptr = NULL;

        } /* end if */
    } /* end for */

    /* Construct and write index to the metadata file */
    if ( H5F__vfd_swmr_construct_write_md_idx(f, num_entries, index) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, \
                    "fail to construct & write index to md")

    /* Construct and write header to the md file */
    if ( H5F__vfd_swmr_construct_write_md_hdr(f, num_entries) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, \
                    "fail to construct & write header to md")

    /* 
     * Release time out entries from the delayed list by scanning the 
     * list from the bottom up:
     *
     *      --release to the metadata file free space manager all index 
     *        entries that have resided on the list for more than 
     *        max_lag ticks
     *
     *      --remove the associated entries from the list
     */
     dl_entry = f->shared->dl_tail_ptr;

     while ( dl_entry != NULL ) {

        prev = dl_entry->prev;

        /* max_lag is at least 3 */
        if ( ( f->shared->tick_num > f->shared->vfd_swmr_config.max_lag ) &&
             ( dl_entry->tick_num <= 
               f->shared->tick_num - f->shared->vfd_swmr_config.max_lag ) ) {

            if ( H5MV_free(f, dl_entry->md_file_page_offset * 
                           f->shared->fs_page_size, dl_entry->length) < 0 )

                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                            "unable to flush clean entry")

            /* Remove the entry from the delayed list */
            H5F_DC_REMOVE(dl_entry, f->shared->dl_head_ptr, \
                          f->shared->dl_tail_ptr, f->shared->dl_len)

            /* Free the delayed entry struct */
            H5FL_FREE(H5F_vfd_swmr_dl_entry_t, dl_entry);

        } else {

            break;
        }

        dl_entry = prev;

    } /* end while */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5F_update_vfd_swmr_metadata_file() */


/*-------------------------------------------------------------------------
 *
 * Function: H5F_vfd_swmr_writer__delay_write
 *
 * Purpose:  Given the base address of a page of metadata, or of a multi-
 *           page metadata entry, determine whether the write must be 
 *           delayed.
 *
 *           At the conceptual level, the VFD SWMR writer must delay the 
 *           write of any metadata page or multi-page metadata that 
 *           overwrites an existing metadata page or multi-page metadata 
 *           entry until it has appeared in the metadata file index for 
 *           at least max_lag ticks.  Since the VFD SWMR reader goes 
 *           to the HDF5 file for any piece of metadata not listed in 
 *           the metadata file index, failure to delay such writes can 
 *           result in message from the future bugs.
 *
 *           The easy case case is pages or multi-page metadata entries
 *           have just been allocated.  Obviously, these can be written 
 *           immediately.  This case is tracked and tested by the page 
 *           buffer proper.
 *
 *           This routine looks up the supplied page in the metadata file 
 *           index.
 *
 *           If the entry doesn't exist, the function sets 
 *           *delay_write_until_ptr to the current tick plus max_lag.
 *
 *           If the entry exists, the function sets *delay_write_until_ptr
 *           equal to the entries delayed flush field if it is greater than
 *           or equal to the current tick, or zero otherwise.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 11/4/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_writer__delay_write(H5F_t *f, uint64_t page, 
    uint64_t * delay_write_until_ptr)
{
    int32_t top = -1;
    int32_t bottom = 0;
    int32_t probe;
    uint64_t delay_write_until = 0;
    H5FD_vfd_swmr_idx_entry_t * ie_ptr = NULL;
    H5FD_vfd_swmr_idx_entry_t * idx = NULL;
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->vfd_swmr);
    HDassert(f->shared->vfd_swmr_writer);

    idx = f->shared->mdf_idx;

    HDassert((idx) ||( f->shared->tick_num <= 0));

    /* do a binary search on the metadata file index to see if
     * it already contains an entry for *pbe_ptr.
     */

    ie_ptr = NULL;

    if ( idx ) {

        top = f->shared->mdf_idx_entries_used - 1;
        bottom = 0;
    }

    while ( top >= bottom ) {

        HDassert(idx);

        probe = top + bottom / 2;

        if ( idx[probe].hdf5_page_offset < page ) {

            bottom = probe + 1;

        } else if ( idx[probe].hdf5_page_offset > page ) {

            top = probe - 1;

        } else { /* found it */

            ie_ptr = idx + probe;
            bottom = top + 1; /* to exit loop */
        }
    }

    if ( ie_ptr ) {

        if ( ie_ptr->delayed_flush >= f->shared->tick_num ) {

            delay_write_until = ie_ptr->delayed_flush;
        }
    } else {

        delay_write_until = f->shared->tick_num + 
                            f->shared->vfd_swmr_config.max_lag;
    }

    if ( ( delay_write_until != 0 ) && 
         ( ! ( ( delay_write_until >= f->shared->tick_num ) &&
               ( delay_write_until <= 
                 (f->shared->tick_num + f->shared->vfd_swmr_config.max_lag) ) 
             )
         )
       )
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, \
                    "VFD SWMR write delay out of range")

    *delay_write_until_ptr = delay_write_until;
  
done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F_vfd_swmr_writer__delay_write() */


/*-------------------------------------------------------------------------
 *
 * Function: H5F_vfd_swmr_writer_end_of_tick
 *
 * Purpose:  Main routine for managing the end of tick for the VFD 
 *           SWMR writer.  
 *
 *           This function performs all end of tick operations for the 
 *           writer -- specifically:
 * 
 *            1) If requested, flush all raw data to the HDF5 file.
 *
 *               (Not for first cut.)
 *
 *            2) Flush the metadata cache to the page buffer.
 *
 *            3) If this is the first tick (i.e. tick == 0), create the
 *               in memory version of the metadata file index.
 *
 *            4) Scan the page buffer tick list, and use it to update 
 *               the metadata file index, adding or modifying entries as 
 *               appropriate.
 *
 *            5) Scan the metadata file index for entries that can be 
 *               removed -- specifically entries that have been written 
 *               to the HDF5 file more than max_lag ticks ago, and haven't
 *               been modified since. 
 *
 *               (This is an optimization -- adress it later)
 *
 *            6) Scan the page buffer delayed write list for entries that 
 *               may now be written, and move any such entries to the 
 *               page buffer LRU.
 *
 *               (For the first cut, we will assume file was just created,
 *                that there have been no flushes, and that no entries 
 *                have been removed from the metadata file index.  Under 
 *                these circumstances, the delayed write list must always 
 *                be empty. Thus delay implementing this.)
 *
 *            7) Update the metadata file.  Must do this before we 
 *               release the tick list, as otherwise the page buffer 
 *               entry images may not be available.
 *
 *            8) Release the page buffer tick list.
 *
 *            9) Release any delayed writes whose delay has expired.
 *
 *           10) Increment the tick, and update the end of tick.
 *
 *           In passing, generate log entries as appropriate.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 11/4/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_writer_end_of_tick(void)
{
    int32_t idx_entries_added = 0;
    int32_t idx_entries_modified = 0;
    int32_t idx_ent_not_in_tl = 0;
    int32_t idx_ent_not_in_tl_flushed = 0;
    H5F_t * f;
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    f = vfd_swmr_file_g;

    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->pb_ptr);
    HDassert(f->shared->vfd_swmr_writer);
  
    /* 1) If requested, flush all raw data to the HDF5 file.
     *
     *    (Not for first cut.)
     */
    if ( f->shared->vfd_swmr_config.flush_raw_data ) {

        HDassert(FALSE);
    }


    /* 2) Flush the metadata cache to the page buffer. */
    if ( H5AC_flush(f) < 0 ) 

        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, \
                    "Can't flush metadata cache to the page buffer")
 

    /* 3) If this is the first tick (i.e. tick == 0), create the
     *    in memory version of the metadata file index.
     */
    if ( ( f->shared->tick_num == 0 ) &&
         ( H5F__vfd_swmr_writer__create_index(f) < 0 ) )


    /* 4) Scan the page buffer tick list, and use it to update 
     *    the metadata file index, adding or modifying entries as 
     *    appropriate.
     */
    if ( H5PB_vfd_swmr__update_index(f, &idx_entries_added, 
                                     &idx_entries_modified, 
                                     &idx_ent_not_in_tl, 
                                     &idx_ent_not_in_tl_flushed) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't update MD file index")


    /* 5) Scan the metadata file index for entries that can be 
     *    removed -- specifically entries that have been written 
     *    to the HDF5 file more than max_lag ticks ago, and haven't
     *    been modified since. 
     *
     *    (This is an optimization -- adress it later)
     */


    /* 6) Scan the page buffer delayed write list for entries that 
     *    may now be written, and move any such entries to the 
     *    page buffer LRU.
     *
     *    (For the first cut, we will assume file was just created,
     *     that there have been no flushes, and that no entries 
     *     have been removed from the metadata file index.  Under 
     *     these circumstances, the delayed write list must always 
     *     be empty. Thus delay implementing this.)
     */
    HDassert( f->shared->pb_ptr->dwl_len == 0 );


    /* 7) Update the metadata file.  Must do this before we 
     *    release the tick list, as otherwise the page buffer 
     *    entry images may not be available.
     *
     *    Note that this operation will restore the index to 
     *    sorted order.
     */
    if ( H5F_update_vfd_swmr_metadata_file(f, 
                (uint32_t)(f->shared->mdf_idx_entries_used + idx_entries_added),
                f->shared->mdf_idx) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't update MD file")

    /* at this point the metadata file index should be sorted -- update
     * f->shared->mdf_idx_entries_used.
     */
    f->shared->mdf_idx_entries_used += idx_entries_added;

    HDassert(f->shared->mdf_idx_entries_used <= f->shared->mdf_idx_len);


    /* 8) Release the page buffer tick list. */
    if ( H5PB_vfd_swmr__release_tick_list(f) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't release tick list")


    /* 9) Release any delayed writes whose delay has expired */
    if ( H5PB_vfd_swmr__release_delayed_writes(f) < 0 )

        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't release delayed writes")
 

    /* 10) Increment the tick, and update the end of tick. */
    if( vfd_swmr_file_g ) {

        /* Update end_of_tick */
        if ( H5F__vfd_swmr_update_end_of_tick_and_tick_num(vfd_swmr_file_g, 
                                                           TRUE) < 0 )

            HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, \
                        "unable to update end of tick")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5F_vfd_swmr_writer_end_of_tick() */


/*-------------------------------------------------------------------------
 *
 * Function: H5F__vfd_swmr_writer__create_index
 *
 * Purpose:  Allocate and initialize the index for the VFD SWMR metadata 
 *           file.
 *
 *           In the first cut at VFD SWMR, the index is of fixed size,
 *           as specified by the md_pages_reserved field of the VFD 
 *           SWMR configuration.  If we exceed this size we will simply
 *           abort.  Needless to say, this will have to change in the
 *           production version, but it is good enough for the working
 *           prototype.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 11/5/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__vfd_swmr_writer__create_index(H5F_t * f)
{
    int i;
    size_t bytes_available;
    int32_t entries_in_index;
    size_t index_size;
    H5FD_vfd_swmr_idx_entry_t * index = NULL;
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    f = vfd_swmr_file_g;

    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->vfd_swmr_writer);
    HDassert(f->shared->mdf_idx == NULL);
    HDassert(f->shared->mdf_idx_len == 0);
    HDassert(f->shared->mdf_idx_entries_used == 0);

    bytes_available = (size_t)f->shared->fs_page_size * 
                      (size_t)(f->shared->vfd_swmr_config.md_pages_reserved) -
                      H5FD_MD_HEADER_SIZE;

    HDassert(bytes_available > 0);

    entries_in_index = (int32_t)(bytes_available / H5FD_MD_INDEX_ENTRY_SIZE);

    HDassert(entries_in_index > 0);

    index_size = sizeof(H5FD_vfd_swmr_idx_entry_t) * (size_t)entries_in_index;
    index = (H5FD_vfd_swmr_idx_entry_t *)HDmalloc(index_size);

    if ( index == NULL ) 

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "memory allocation failed for md index")
  
    for ( i = 0; i < entries_in_index; i++ ) {

        index[i].hdf5_page_offset    = 0;
        index[i].md_file_page_offset = 0;
        index[i].length              = 0;
        index[i].chksum              = 0;
        index[i].entry_ptr           = NULL;
        index[i].tick_of_last_change = 0;
        index[i].clean               = FALSE;
        index[i].tick_of_last_flush  = 0;
        index[i].delayed_flush       = 0;
        index[i].moved_to_hdf5_file  = FALSE;
    }

    f->shared->mdf_idx              = index;
    f->shared->mdf_idx_len          = entries_in_index;
    f->shared->mdf_idx_entries_used = 0;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5F__vfd_swmr_writer__create_index() */

/*-------------------------------------------------------------------------
 * Function: H5F_vfd_swmr_reader_end_of_tick
 *
 * Purpose:  Dummy right now
 *
 * Return:   SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_reader_end_of_tick(void)
{
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* construct */
    if(vfd_swmr_file_g) {
        /* Update end_of_tick */
        if(H5F__vfd_swmr_update_end_of_tick_and_tick_num(vfd_swmr_file_g, FALSE) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to update end of tick")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_vfd_swmr_reader_end_of_tick() */
