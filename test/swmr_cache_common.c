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

/* Programmer:  Houjun Tang
 *              10/9/17
 *
 *		This file contains tests for the full SWMR freedspace management
 *		implemented in H5MF*
 */
#include "swmr_cache_common.h"


/* global variable declarations: */
const char *failure_mssg_g = NULL;
hbool_t pass_g = TRUE; /* set to false on error */

int                                 entry_type_id_g = -1;
fullswmr_cache_entry_t             *entry_array_g;  
H5F_t                              *entry_file_ptr_array_g;
hsize_t                             entry_array_nelem_g;
hsize_t                             entry_array_freed_g;
hsize_t                             entry_array_size_g;
haddr_t                             allocated_base_addr_array_g;
H5C_t *                             saved_cache_g = NULL;

static herr_t fullswmr_cache_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t fullswmr_cache_serialize(const H5F_t *f, void *image_ptr,size_t len, void *thing);
static void  *fullswmr_cache_deserialize(const void *image_ptr, size_t len, void *udata_ptr, hbool_t *dirty_ptr);
static herr_t fullswmr_cache_image_len(const void *thing, size_t *image_len_ptr);
static herr_t fullswmr_cache_free_icr(void *thing);



/* Callback class */
static const H5C_class_t FULLSWMR_CACHE_TEST_CLASS[1] = {{
    FULLSWMR_CACHE_ENTRY_TYPE,
    "fullswmr_cache_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    fullswmr_cache_get_initial_load_size,
    NULL,
    NULL,
    fullswmr_cache_deserialize,
    fullswmr_cache_image_len,
    NULL,
    fullswmr_cache_serialize,
    NULL,
    fullswmr_cache_free_icr,
    NULL,
}};

const H5C_class_t *fullswmr_test_classes_g[H5C__MAX_NUM_TYPE_IDS];


/*-------------------------------------------------------------------------
 * Function:	fullswmr_expunge_entry()
 *
 * Purpose:	Expunge the entry indicated by the sequence ID and index.
 *
 * Return:	void
 *
 * Programmer:	Houjun Tang 
 *              10/24/17
 *
 *-------------------------------------------------------------------------
 */

void
fullswmr_expunge_entry(H5F_t * file_ptr, int32_t idx)
{
    herr_t result;
    haddr_t entry_addr;

    if ( pass_g ) {

        HDassert( ( 0 <= idx ) && ( (size_t)idx <= entry_array_nelem_g ) );

        entry_addr = allocated_base_addr_array_g + idx*entry_array_size_g;

        result = H5C_expunge_entry(file_ptr, H5AC_ind_read_dxpl_id,
                fullswmr_test_classes_g[entry_type_id_g], entry_addr, H5C__NO_FLAGS_SET);

        if ( result < 0 ) {
            pass_g = FALSE;
            failure_mssg_g = "error in H5C_expunge_entry().";
        }
    }
    
    entry_array_freed_g++;

    return;
} /* expunge_entry() */



/*-------------------------------------------------------------------------
 * Function:	fullswmr_addr_to_index
 *
 * Purpose:	Given an address, compute the index of the associated entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Programmer:	Houjun Tang 
 *              10/11/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *-------------------------------------------------------------------------
 */
void
fullswmr_addr_to_index(haddr_t addr, int *index_ptr)
{
    int idx  = -1;

    HDassert( index_ptr );

    idx = (int)((addr - allocated_base_addr_array_g) / entry_array_size_g);
    HDassert( ( idx >= 0 ) && ( (size_t)idx < entry_array_nelem_g ) );

    *index_ptr = idx;

    return;

} /* fullswmr_addr_to_index() */


/*-------------------------------------------------------------------------
 * Function:	fullswmr_cache_get_initial_load_size 
 *
 * Purpose:	Query the image size for loading an entry.  The helper
 *              functions funnel into get_initial_load_size proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	Quincey Koziol
 *              5/18/10
 *
 * Programmer:	Houjun Tang 
 *              10/9/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *-------------------------------------------------------------------------
 */
static herr_t
fullswmr_cache_get_initial_load_size(void *udata, size_t *image_length)
{
    fullswmr_cache_entry_t *entry;
    haddr_t addr = *(const haddr_t *)udata;
    int idx;

    fullswmr_addr_to_index(addr, &idx);

    entry = &entry_array_g[idx];

    HDassert(entry->index == idx);
    HDassert(entry->index >= 0);
    HDassert(entry->index < (int)entry_array_nelem_g);
    /* HDassert(entry->addr == addr); */

    *image_length = entry->size;

    return(SUCCEED);
} /* fullswmr_get_initial_load_size() */


/*-------------------------------------------------------------------------
 * Function:	fullswmr_serialize 
 *
 * Purpose:	Serialize the supplied entry.  For now this consistes of
 * 		loading the type and index of the entry into the first
 * 		three bytes of the image (if it is long enough -- if not
 * 		just load the low order byte of the index into the first
 * 		byte of the image).
 *
 * 		The helper functions verify that the correct version of
 * 		serialize is being called, and then call serialize
 * 		proper.
 *
 * Return:	SUCCEED if successful, FAIL otherwise.
 *
 * Programmer:	John Mainzer
 *              9/19/07
 *
 * Programmer:	Houjun Tang 
 *              10/9/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *-------------------------------------------------------------------------
 */
herr_t
fullswmr_cache_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    fullswmr_cache_entry_t *entry;

    HDassert(image_ptr);
    HDassert(thing);

    entry = (fullswmr_cache_entry_t *)thing;

    /* HDassert(entry->self == entry); */
    HDassert(entry->size == len);

    /* shouldn't serialize the entry unless it is dirty */
    HDassert(entry->is_dirty);

    HDassert(entry->index >= 0);
    HDassert(entry->index < (int)entry_array_nelem_g);

    /* null out the image to avoid spurious failures */
    HDmemset(image_ptr, 0, len);

    entry->is_dirty = FALSE;

    return(SUCCEED);
} /* fullswmr_serialize() */


/*-------------------------------------------------------------------------
 * Function:	fullswmr_cache_deserialize 
 *
 * Purpose:	deserialize the entry.  The helper functions verify that the
 *		correct version of deserialize is being called, and then call
 *		deserialize proper.
 *
 * Return:	void * (pointer to the in core representation of the entry)
 *
 * Programmer:	John Mainzer
 *              9/20/07
 *
 * Programmer:	Houjun Tang 
 *              10/9/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *-------------------------------------------------------------------------
 */
static void *
fullswmr_cache_deserialize(const void *image, size_t len, void *udata, hbool_t *dirty)
{
    fullswmr_cache_entry_t *entry;
    haddr_t addr = *(haddr_t *)udata;
    int idx;

    fullswmr_addr_to_index(addr, &idx);

    entry = &entry_array_g[idx];

    HDassert(entry->index == idx);
    HDassert(entry->index >= 0);
    HDassert(entry->index < (int)entry_array_nelem_g);
    /* HDassert(entry->addr == addr); */
    HDassert(entry->size == len);
    HDassert(dirty != NULL);

    /* for now *dirty will always be FALSE */
    *dirty = FALSE;

    /* verify that the image contains the expected data. */
    HDassert(image != NULL);

    entry->header.is_dirty = FALSE;
    entry->is_dirty = FALSE;

    return((void *)entry);
} /* fullswmr_cache_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	fullswmr_cache_image_len 
 *
 * Purpose:	Return the real (and possibly reduced) length of the image.
 * 		The helper functions verify that the correct version of
 * 		deserialize is being called, and then call deserialize
 * 		proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              9/19/07
 *
 * Programmer:	Houjun Tang 
 *              10/9/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *-------------------------------------------------------------------------
 */
herr_t
fullswmr_cache_image_len(const void *thing, size_t *image_length)
{
    const fullswmr_cache_entry_t *entry;
    int idx;

    HDassert(thing);
    HDassert(image_length);

    entry = (const fullswmr_cache_entry_t *)thing;

    /* HDassert(entry->self == entry); */

    idx = entry->index;

    HDassert(entry->index == idx);
    HDassert(entry->index >= 0);
    HDassert(entry->index < (int)entry_array_nelem_g);

    HDassert(entry == &entry_array_g[idx]);

    HDassert(entry->size <= entry_array_size_g);
    HDassert(entry->size > 0);

    *image_length = entry->size;

    return(SUCCEED);
} /* fullswmr_image_len() */


/*-------------------------------------------------------------------------
 * Function:	fullswmr_cache_free_icr 
 *
 * Purpose:	Nominally, this callback is supposed to free the
 * 		in core representation of the entry.
 *
 * 		In the context of this test bed, we use it to do
 * 		do all the processing we used to do on a destroy.
 * 		In particular, we use it to release all the pins
 * 		that this entry may have on other entries.
 *
 * 		The helper functions verify that the correct version of
 * 		serialize is being called, and then call free_icr
 * 		proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              9/19/07
 *
 * Programmer:	Houjun Tang 
 *              10/9/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *-------------------------------------------------------------------------
 */
herr_t
fullswmr_cache_free_icr(void *thing)
{
    fullswmr_cache_entry_t *entry = (fullswmr_cache_entry_t*)thing;

    HDassert(entry);

    HDassert(entry->index >= 0);
    HDassert(entry->index <= (int)entry_array_nelem_g);
    HDassert(entry == &entry_array_g[entry->index]);

    /* HDassert((entry->header.destroy_in_progress) || */
    /*           (entry->header.addr == entry->addr)); */
    HDassert(entry->header.magic == H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC);
    HDassert(entry->header.size == entry->size);
    HDassert(entry->header.tl_next == NULL);
    HDassert(entry->header.tl_prev == NULL);

    /* entry->destroyed = TRUE; */

    return(SUCCEED);
} /* fullswmr_cache_free_icr() */


/*****************************************************************************
 *
 * Function:    fullswmr_setup_cache()
 *
 * Purpose:     Open an HDF file.  This will allocate an instance and
 * 		initialize an associated instance of H5C_t.  However,
 * 		we want to test an instance of H5C_t, so allocate and
 * 		initialize one with the file ID returned by the call to
 * 		H5Fcreate().  Return a pointer to this instance of H5C_t.
 *
 *		Observe that we open a HDF file because the cache now
 *		writes directly to file, and we need the file I/O facilities
 *		associated with the file.
 *
 *		To avoid tripping on error check code, must allocate enough
 *		space in the file to hold all the test entries and their
 *		alternates.  This is a little sticky, as the addresses of
 *		all the test entries are determined at compile time.
 *
 *		Deal with this by choosing BASE_ADDR large enough that
 *		the base address of the allocate space will be less than
 *		or equal to BASE_ADDR, and then requesting an extra BASE_ADDR
 *		bytes, so we don't have to wory about exceeding the allocation.
 *
 * Return:      Success:        Ptr to H5C_t
 *
 *              Failure:        NULL
 *
 * Programmer:  JRM -- 9/13/07
 *
 * Programmer:	Houjun Tang 
 *              10/9/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *              fullswmr_setup_cache can be called multiple times, the 
 *
 *****************************************************************************/

H5F_t *
fullswmr_setup_cache(hsize_t entry_size, int n_entry, unsigned file_flags)
{
    const char *filename;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    H5F_t * ret_val = NULL;
    haddr_t allocated_base_addr;
    hid_t fapl_id = H5P_DEFAULT;
    hid_t fcpl_id = H5P_DEFAULT;
    fullswmr_cache_entry_t *cache_entries = NULL;
    int i;
    int32_t max_type_id;


    entry_array_g = (fullswmr_cache_entry_t *)HDcalloc((size_t)(n_entry), sizeof(fullswmr_cache_entry_t));
    if(NULL == entry_array_g) {
        pass_g = FALSE;
        failure_mssg_g = "HDcalloc() failed.\n";
        goto done;
    }

    /* Store the just allocated entries address to the global array */
    entry_array_nelem_g = (hsize_t)(n_entry);
    entry_array_freed_g = 0;
    entry_array_size_g  = entry_size;

    /* Populate the content of helper array */
    for (i = 0; i < n_entry; i++) {
        cache_entries            = &entry_array_g[i];
        cache_entries->size      = entry_size;
        cache_entries->index     = (int)i;
        cache_entries->is_dirty  = FALSE;
    }

    if((fcpl_id = H5Pcreate(H5P_FILE_CREATE)) == FAIL) {
        pass_g = FALSE;
        failure_mssg_g = "H5Pcreate(H5P_FILE_CREATE) failed.\n";
        goto done;
    }

    if( (fapl_id = H5Pcreate(H5P_FILE_ACCESS)) == FAIL ) {
        pass_g = FALSE;
        failure_mssg_g = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        goto done;
    }

    if(H5Pset_swmr_deltat(fapl_id, FULLSWMR_DELTAT_SECONDS*1000000) < 0) {
        pass_g = FALSE;
        failure_mssg_g = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        goto done;
    }

    filename = "fullswmr_cache_test";
    fid = H5Fcreate(filename, file_flags, fcpl_id, fapl_id);
    if(fid < 0) {
        pass_g = FALSE;
        failure_mssg_g = "H5Fcreate() failed.";
        goto done;
    } /* end if */

    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0) {
        pass_g = FALSE;
        failure_mssg_g = "H5Fflush() failed.";
        goto done;
    } 
    else {
        file_ptr = (H5F_t *)H5I_object_verify(fid, H5I_FILE);
        if(file_ptr == NULL) {
            pass_g = FALSE;
            failure_mssg_g = "Can't get file_ptr.";
            goto done;
        }
        entry_file_ptr_array_g = file_ptr;
    }

    /* A bit of fancy footwork here:
     *
     * The call to H5Fcreate() allocates an instance of H5C_t,
     * initializes it, and stores its address in f->shared->cache.
     *
     * We don't want to use this cache, as it has a bunch of extra
     * initialization that may change over time, and in any case
     * it will not in general be configured the way we want it.
     *
     * We used to deal with this problem by storing the file pointer
     * in another instance of H5C_t, and then ignoring the original
     * version.  However, this strategy doesn't work any more, as
     * we can't store the file pointer in the instance of H5C_t,
     * and we have modified many cache routines to use a file
     * pointer to look up the target cache.
     *
     * Thus we now make note of the address of the instance of
     * H5C_t created by the call to H5Fcreate(), set
     * file_ptr->shared->cache to NULL, call H5C_create()
     * to allocate a new instance of H5C_t for test purposes,
     * and store than new instance's address in
     * file_ptr->shared->cache.
     *
     * On shut down, we call H5C_dest on our instance of H5C_t,
     * set file_ptr->shared->cache to point to the original
     * instance, and then close the file normally.
     */

    /* HDassert(saved_cache == NULL); */

    /* Copy the existing cache class from original file cache ptr */
    max_type_id = file_ptr->shared->cache->max_type_id;
    HDmemcpy(fullswmr_test_classes_g, file_ptr->shared->cache->class_table_ptr, H5C__MAX_NUM_TYPE_IDS*sizeof(H5C_class_t*));

    /* replace H5AC_SOHM_TABLE_ID with our own class */
    entry_type_id_g = H5AC_SOHM_TABLE_ID;
    fullswmr_test_classes_g[entry_type_id_g] = FULLSWMR_CACHE_TEST_CLASS;

    saved_cache_g = file_ptr->shared->cache;
    file_ptr->shared->cache = NULL;
    cache_ptr = H5C_create(4*1024*1024, 2*1024*1024, max_type_id, fullswmr_test_classes_g, NULL, TRUE, NULL, NULL);
    cache_ptr->max_type_id = max_type_id;
    file_ptr->shared->cache = cache_ptr;


    /* allocate space for test entries */
    allocated_base_addr = H5MF_alloc(file_ptr, H5FD_MEM_DEFAULT, H5AC_ind_read_dxpl_id, 
                                     (hsize_t)n_entry*entry_size);
    if(allocated_base_addr == HADDR_UNDEF) {
        pass_g = FALSE;
        failure_mssg_g = "H5MF_alloc() failed.";
        goto done;
    } 
    allocated_base_addr_array_g = allocated_base_addr;

    /* Need to set this else all cache tests will fail */
    cache_ptr->ignore_tags = TRUE;
    H5C_stats__reset(cache_ptr);

    ret_val = file_ptr;

    /* Insert the allocated entries into the cache */
    for(i = 0; i < n_entry; i++) {
        fullswmr_insert_entry(file_ptr, (int)i, H5C__NO_FLAGS_SET);
        if(!pass_g) 
            CACHE_ERROR("insert_entry failed")
    } /* end for */


done:
    if (fcpl_id != H5P_DEFAULT) 
        H5Pclose(fcpl_id);
    if (fapl_id != H5P_DEFAULT) 
        H5Pclose(fapl_id);

    return(ret_val);
} /* fullswmr_setup_cache() */


/*-------------------------------------------------------------------------
 * Function:	fullswmr_flush_cache()
 *
 * Purpose:	Flush the specified cache, destroying all entries if
                requested.  If requested, dump stats first.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/23/04
 *
 *-------------------------------------------------------------------------
 */

void
fullswmr_flush_cache(H5F_t * file_ptr, hbool_t destroy_entries, hbool_t dump_stats, hbool_t dump_detailed_stats)
{
    hbool_t verbose = FALSE;

    /* verify_unprotected(); */

    if(pass_g) {
        H5C_t * cache_ptr;
        herr_t result = 0;

        HDassert(file_ptr);

        cache_ptr = file_ptr->shared->cache;

        if(destroy_entries)
            result = H5C_flush_cache(file_ptr, H5AC_ind_read_dxpl_id, H5C__FLUSH_INVALIDATE_FLAG);

        else
            result = H5C_flush_cache(file_ptr, H5AC_ind_read_dxpl_id, H5C__NO_FLAGS_SET);

        if(dump_stats)
            H5C_stats(cache_ptr, "test cache", dump_detailed_stats);

        if(result < 0) {
            pass_g = FALSE;
            failure_mssg_g = "error in H5C_flush_cache().";
        }
        else if((destroy_entries) && ((cache_ptr->index_len != 0)
                || (cache_ptr->index_size != 0)
                || (cache_ptr->clean_index_size != 0)
                || (cache_ptr->dirty_index_size != 0))) {

            if(verbose) {
                HDfprintf(stdout,
                        "%s: unexpected il/is/cis/dis = %lld/%lld/%lld/%lld.\n",
                        FUNC,
                        (long long)(cache_ptr->index_len),
                        (long long)(cache_ptr->index_size),
                        (long long)(cache_ptr->clean_index_size),
                        (long long)(cache_ptr->dirty_index_size));
            }
            pass_g = FALSE;
            failure_mssg_g = "non zero index len/sizes after H5C_flush_cache() with invalidate.";
        }
    }

    return;

} /* fullswmr_flush_cache() */


/*-------------------------------------------------------------------------
 * Function:	fullswmr_takedown_cache()
 *
 * Purpose:	Flush the specified cache and destroy it.  If requested,
 *		dump stats first.  Then close and delete the associate
 *		file.
 *
 *		If pass_g is FALSE, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/14/07
 *
 * Programmer:	Houjun Tang 
 *              10/9/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *-------------------------------------------------------------------------
 */

void
fullswmr_takedown_cache(H5F_t * file_ptr,
               hbool_t dump_stats,
               hbool_t dump_detailed_stats)
{
    /* char filename[512]; */
    hid_t fid;

    if ( file_ptr != NULL ) {
        H5C_t * cache_ptr = file_ptr->shared->cache;

        if ( dump_stats ) 
            H5C_stats(cache_ptr, "test cache", dump_detailed_stats);
        

	if ( H5C_prep_for_file_close(file_ptr, H5P_DATASET_XFER_DEFAULT) < 0 ) {
            pass_g = FALSE;
            failure_mssg_g = "unexpected failure of prep for file close.\n";
        }

        fullswmr_flush_cache(file_ptr, TRUE, FALSE, FALSE);

        H5C_dest(file_ptr, H5AC_ind_read_dxpl_id);
        
        /* Give back the original saved cache */
        file_ptr->shared->cache = saved_cache_g;
    }

    fid = H5F_get_file_id(file_ptr);
    if ( fid >= 0 ) {
        /* Free all allocated cache entries */
        if ( H5F_addr_defined(allocated_base_addr_array_g) ) {
            if (NULL == file_ptr)  {
                file_ptr = (H5F_t *)H5I_object_verify(fid, H5I_FILE);
                HDassert(file_ptr);
            }

            H5MF_xfree(file_ptr, H5FD_MEM_DEFAULT, H5AC_ind_read_dxpl_id, 
                       allocated_base_addr_array_g+entry_array_freed_g*entry_array_size_g,
                       entry_array_size_g*(entry_array_nelem_g-entry_array_freed_g));
            allocated_base_addr_array_g = HADDR_UNDEF;
        }

	if (H5Fclose(fid) < 0) {
            pass_g = FALSE;
	    failure_mssg_g = "couldn't close test file.";
	} 
    }

    HDfree(entry_array_g);
    /* Reset all global helper arrays */
    entry_array_g                = NULL;
    entry_file_ptr_array_g       = NULL;
    entry_array_nelem_g          = 0;
    entry_array_freed_g          = 0;
    entry_array_size_g           = 0;

    return;

} /* fullswmr_takedown_cache() */


/*-------------------------------------------------------------------------
 * Function:	fullswmr_insert_entry()
 *
 * Purpose:	Insert the entry indicated by the type and index.
 *
 *		Do nothing if pass_g is false.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Programmer:	Houjun Tang 
 *              10/11/17
 *              Cloned from cache_common.c, removed entry_type and other
 *              unnecessary codes for this test program.
 *-------------------------------------------------------------------------
 */

void
fullswmr_insert_entry(H5F_t * file_ptr, int idx, unsigned flags)
{
    H5C_t * cache_ptr;
    herr_t result;
    hid_t xfer = H5AC_ind_read_dxpl_id;
    fullswmr_cache_entry_t * entry_ptr;
    haddr_t entry_addr;

    if ( pass_g ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr );
        HDassert( ( 0 <= idx ) && ( idx <= (int)entry_array_nelem_g ) );

        entry_ptr  = &entry_array_g[idx];
        entry_addr = allocated_base_addr_array_g + (haddr_t)((hsize_t)idx*entry_array_size_g);

        entry_ptr->is_dirty = TRUE;

        result = H5C_insert_entry(file_ptr, xfer, fullswmr_test_classes_g[entry_type_id_g], entry_addr, 
                                  (void *)entry_ptr, flags);

        if ( ( result < 0 ) ||
             /* ( entry_ptr->header.is_protected ) || */
             ( entry_ptr->size != entry_ptr->header.size ) ) {

            pass_g = FALSE;
            failure_mssg_g = "error in H5C_insert().";

        } /* end if */

    } /* end if */

    return;

} /* fullswmr_insert_entry() */
