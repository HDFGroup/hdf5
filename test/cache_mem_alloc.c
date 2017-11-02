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
 *              10/27/17
 *
 *		This file contains tests for the full SWMR freedspace management
 *		implemented in H5MF*
 */
#include "swmr_cache_common.h"

#define H5MF_FRIEND      /*suppress error about including H5MFpkg      */
#define H5MF_TESTING
#include "H5MFpkg.h"


/* private typedef declarations: */


/* private function declarations: */
static unsigned check_freedspace_without_swmr_write(void);
static unsigned check_holding_tank_with_swmr_write(void);
static unsigned check_freedspace_flush_dependency(void);
static unsigned check_freedspace_reuse_2deltat(void);


/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/


/* Full SWMR tests */



/*-------------------------------------------------------------------------
 * Function:    fullswmr_free_entry()
 *
 * Purpose:     Free and expung a test entry, update related arrays.
 *
 * Return:      0 on success, non-zero on failure
 *
 * Programmer:  Houjun Tang
 *              10/25/17
 *
 *-------------------------------------------------------------------------
 */
static unsigned
fullswmr_free_entry(H5F_t *file_ptr, int idx)
{
    haddr_t addr;
    hsize_t size;

    addr = allocated_base_addr_array_g + (haddr_t)(idx*entry_array_size_g);
    size = entry_array_size_g;

    if (H5MF_xfree(file_ptr, H5FD_MEM_DEFAULT, H5AC_ind_read_dxpl_id, addr, size) < 0) 
        CACHE_ERROR("H5MF_xfree failed")

    fullswmr_expunge_entry(file_ptr, idx);
    if (!pass_g)
        CACHE_ERROR("Expunge entry failed")

done:
    return (unsigned)!pass_g;
} /* fullswmr_free_entry */



/*-------------------------------------------------------------------------
 * Function:    check_freedspace_without_swmr_write()
 *
 * Purpose:     Create and open file without SWMR-write flag, verify that 
 *              freed space is immediately returned to use for future 
 *              allocations.
 *
 * Return:      0 on success, non-zero on failure
 *
 * Programmer:  Houjun Tang
 *              9/18/17
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_freedspace_without_swmr_write(void)
{
    H5F_t * file_ptr = NULL;            /* File for this test */
    int    u, n_entry, n_free_entry;
    hsize_t entry_size;
    hsize_t freedspace_size = 0, meta_size = 0;

    pass_g           = TRUE;
    entry_size       = 1024;
    n_entry          = 5;

    TESTING("freedspace without swmr write")

    /* Setup cache */
    file_ptr    = fullswmr_setup_cache(entry_size, n_entry, H5F_ACC_TRUNC);

    if(!pass_g) 
        CACHE_ERROR("setup_cache failed")

    /* 
     * Free the allocated space then verify that the freed space is immediately returned 
     * to use for future allocations.
     */
    if(pass_g) {

        n_free_entry = 2;
        for(u = 0; u < n_free_entry; u++) {

            fullswmr_free_entry(file_ptr, u);
            if (!pass_g)
                CACHE_ERROR("Free entry failed")

            if (H5MF_get_freespace(file_ptr, H5AC_ind_read_dxpl_id, &freedspace_size, &meta_size) < 0)
                CACHE_ERROR("H5MF_get_freespace failed")

            if (freedspace_size != (hsize_t)(u+1)*entry_array_size_g) {
                CACHE_ERROR("Total free space size is zero!")
                goto done;
            }
        }
    }

done:
    if(file_ptr)
        fullswmr_takedown_cache(file_ptr, FALSE, FALSE);

    if(pass_g)
        PASSED()
    else {
        H5_FAILED();
        HDfprintf(stdout, "%s.\n", failure_mssg_g);
    } /* end else */

    return (unsigned)!pass_g;
} /* end check_freedspace_without_swmr_write() */



/*-------------------------------------------------------------------------
 * Function:    check_holding_tank_with_swmr_write()
 *
 * Purpose:     Open file with SWMR-write flag, free space when there are 
 *              no dirty entries in the metadata cache, verify that the 
 *              no freedspace is created, nothing in the holding tank.
 *
 * Return:      0 on success, non-zero on failure
 *
 * Programmer:  Houjun Tang
 *              9/18/17
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_holding_tank_with_swmr_write(void)
{
    H5F_t * file_ptr = NULL;            /* File for this test */
    int u, n_entry;
    hsize_t entry_size;
    hsize_t freedspace_size = 0, meta_size = 0;
    fullswmr_cache_entry_t *entry_ptr;

    pass_g           = TRUE;
    entry_size       = 1024;
    n_entry          = 5;

    TESTING("holding tank with swmr write")

    /* Setup cache */
    file_ptr    = fullswmr_setup_cache(entry_size, n_entry, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE);

    if(!pass_g) 
        CACHE_ERROR("setup_cache failed")

    /* Free the cache so there is no dirty entry */
    fullswmr_flush_cache(file_ptr, TRUE, FALSE, FALSE);

    if(pass_g) {

        /* Free an entry and check the holding tank */
        fullswmr_free_entry(file_ptr, 0);
        if (!pass_g)
            CACHE_ERROR("Free entry failed")

        if (H5MF_get_freespace(file_ptr, H5AC_ind_read_dxpl_id, &freedspace_size, &meta_size) < 0)
            CACHE_ERROR("H5MF_get_freespace failed")

        /* NOTE: the H5MF_get_freespace includes size of deferred space, so freedspace_size 
         *       will be the freed size. */
        if (freedspace_size != entry_array_size_g) 
            CACHE_ERROR("Total free space size is not correct with SMWR write!")
 
        /* Each cache entry should have no parent (freedspace entry) */
        for (u = 1; u < n_entry; u++) {
            entry_ptr = &entry_array_g[u];
            if ( entry_ptr->header.flush_dep_nparents != 0 || entry_ptr->header.flush_dep_parent != NULL )
                CACHE_ERROR("Error with freedspace flush dependency!")
        }
       
        /* Check the holding tank, should be empty */
        if (file_ptr->shared->freedspace_head != NULL) {
            CACHE_ERROR("Holding tank is not empty!")
        }

    } /* end of if */


done:
    if(file_ptr)
        fullswmr_takedown_cache(file_ptr, FALSE, FALSE);

    if(pass_g)
        PASSED()
    else {
        H5_FAILED();
        HDfprintf(stdout, "%s.\n", failure_mssg_g);
    } /* end else */

    return (unsigned)!pass_g;
} /* end check_holding_tank_with_swmr_write() */


/*-------------------------------------------------------------------------
 * Function:    check_freedspace_flush_dependency()
 *
 * Purpose:     Open file with SWMR-write flag, dirty some entries in the metadata cache, then:
 *               - Free metadata block in the file, verify that flush dependencies are correct
 *               - Free raw data block in the file, verify that flush dependencies are correct
 *               - Flush dirty metadata entries in the file (individually), verifying that the 
 *                 freed space blocks are correctly placed into the holding tank queue at the 
 *                 correct time.
 * Return:      0 on success, non-zero on failure
 *
 * Programmer:  Houjun Tang
 *              10/27/17
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_freedspace_flush_dependency(void)
{
    H5F_t * file_ptr = NULL;            /* File for this test */
    int u, n_entry, n_freedspace = 0;
    hsize_t entry_size;
    fullswmr_cache_entry_t * entry_ptr;
    H5MF_freedspace_t *freedspace_ptr;
    unsigned flush_flags;
    hsize_t holding_tank_total_size = 0;
    haddr_t raw_addr;

    pass_g           = TRUE;
    entry_size       = 1024;
    n_entry          = 5;

    TESTING("freedspace flush dependency")

    /* Setup cache */
    file_ptr    = fullswmr_setup_cache(entry_size, n_entry, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE);

    if(!pass_g) 
        CACHE_ERROR("setup_cache failed")

    /* All entries are set dirty after insert */

    /*  Test 1: Free metadata block in the file, verify that flush dependencies are correct */
    if(pass_g) {

        fullswmr_free_entry(file_ptr, 0);
        if (!pass_g)
            CACHE_ERROR("Free entry failed")

        /* Each cache entry should have exactly one parent (freedspace entry) */
        for (u = 1; u < n_entry; u++) {
            entry_ptr = &entry_array_g[u];
            if ( entry_ptr->header.flush_dep_nparents != 1 || entry_ptr->header.flush_dep_parent == NULL || 
                 entry_ptr->header.flush_dep_parent[0]->type != H5AC_FREEDSPACE) 
                CACHE_ERROR("Error with freedspace flush dependency!")
        }

    }
    fullswmr_takedown_cache(file_ptr, FALSE, FALSE);

    /*  Test 2: Create and free raw data block in the file, verify that flush dependencies are correct */
    file_ptr = fullswmr_setup_cache(entry_size, n_entry, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE);
    if(!pass_g) 
        CACHE_ERROR("setup_cache failed")

    if(pass_g) {

        raw_addr = H5MF_alloc(file_ptr, H5FD_MEM_DRAW, H5AC_ind_read_dxpl_id, entry_size);
        if (raw_addr == HADDR_UNDEF) 
            CACHE_ERROR("H5MF_alloc failed")

        if (H5MF_xfree(file_ptr, H5FD_MEM_DRAW, H5AC_ind_read_dxpl_id, raw_addr, entry_size) < 0) 
            CACHE_ERROR("H5MF_xfree failed")

        /* Each cache entry should have exactly one parent (freedspace entry) */
        for (u = 0; u < n_entry; u++) {
            entry_ptr = &entry_array_g[u];
            if ( entry_ptr->header.flush_dep_nparents != 1 || entry_ptr->header.flush_dep_parent == NULL || 
                 entry_ptr->header.flush_dep_parent[0]->type != H5AC_FREEDSPACE) 
                CACHE_ERROR("Error with freedspace flush dependency!")
        }
    }
    fullswmr_takedown_cache(file_ptr, FALSE, FALSE);

    /*  Test 3: Flush dirty metadata entries in the file (individually), verifying that the 
     *          freed space blocks are correctly placed into the holding tank queue at the 
     *          correct time.
     */
    file_ptr = fullswmr_setup_cache(entry_size, n_entry, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE);
    if(!pass_g) 
        CACHE_ERROR("setup_cache failed")

    if(pass_g) {

        /* free the first entry */
        fullswmr_free_entry(file_ptr, 0);
        if (!pass_g)
            CACHE_ERROR("Free entry failed")

        /* flush the rest */
        for (u = 1; u < n_entry; u++) {
            entry_ptr = &entry_array_g[u];

            flush_flags = H5C__FLUSH_CLEAR_ONLY_FLAG;

            if(H5C__flush_single_entry(file_ptr, H5AC_ind_read_dxpl_id, &entry_ptr->header, flush_flags) < 0)
                CACHE_ERROR("Error with flush single entry!")
        }
        
        /* Check the holding tank with the just freed entries */
        if (file_ptr->shared->freedspace_head == NULL) {
            CACHE_ERROR("Holding tank is empty!")
        }
        else{
            n_freedspace = 0;
            freedspace_ptr = file_ptr->shared->freedspace_head;
            while(NULL != freedspace_ptr) {
                holding_tank_total_size += freedspace_ptr->size;
                n_freedspace++;
                freedspace_ptr = freedspace_ptr->next;
            }
            /* We should have 1 freedspace entries in the holding tank */
            if (1 != n_freedspace || holding_tank_total_size != entry_array_size_g) {
                CACHE_ERROR("Error with holding tank entries!")
            }
        }


    }

done:
    if(file_ptr)
        fullswmr_takedown_cache(file_ptr, FALSE, FALSE);

    if(pass_g)
        PASSED()
    else {
        H5_FAILED();
        HDfprintf(stdout, "%s.\n", failure_mssg_g);
    } /* end else */

    return (unsigned)!pass_g;


} /* end check_freedspace_flush_dependency() */




/*-------------------------------------------------------------------------
 * Function:    check_freedspace_reuse_2deltat()
 *
 * Purpose:     Check if a cache block is freed, its previous allocated 
 *              address won't be reused until 2 delta t has pass_ged.
 *
 * Return:      0 on success, non-zero on failure
 *
 * Programmer:  Houjun Tang
 *              10/27/17
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_freedspace_reuse_2deltat(void)
{
    H5F_t  *file_ptr = NULL;            /* File for this test */
    int     n_entry, u;
    hsize_t entry_size;
    fullswmr_cache_entry_t * entry_ptr;
    unsigned flush_flags;
    haddr_t new_alloc_addr1 = HADDR_UNDEF, new_alloc_addr2 = HADDR_UNDEF;
 
    pass_g           = TRUE;
    entry_size       = 1024;
    n_entry          = 5;

    TESTING("freedspace reuse after 2 delta t (wait 2 seconds)")

    /* Setup cache and insert some entries */
    file_ptr    = fullswmr_setup_cache(entry_size, n_entry, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE);
    if(!pass_g) 
        CACHE_ERROR("setup_cache failed")

    /* All entries are set dirty after insert */

    if(pass_g) {

        /* free the first entry */
        fullswmr_free_entry(file_ptr, 0);
        if (!pass_g)
            CACHE_ERROR("Free entry failed")

        /* Flush and freedspace is pushed to holding tank */
        for (u = 1; u < n_entry; u++) {
            entry_ptr = &entry_array_g[u];
            flush_flags = H5C__FLUSH_CLEAR_ONLY_FLAG;
            if(H5C__flush_single_entry(file_ptr, H5AC_ind_read_dxpl_id, &entry_ptr->header, flush_flags) < 0)
                CACHE_ERROR("Error with flush single entry!")
        }
        
        new_alloc_addr1 = H5MF_alloc(file_ptr, H5FD_MEM_DEFAULT, H5AC_ind_read_dxpl_id, (hsize_t)(entry_size));
        if (new_alloc_addr1 == HADDR_UNDEF) 
            CACHE_ERROR("H5MF_alloc failed")
        else if (new_alloc_addr1 == allocated_base_addr_array_g)
            CACHE_ERROR("Freed space is reused before 2 delta t time")

        /* Delta t is set to be FULLSWMR_DELTAT_SECONDS (1s) in setup_cache */
        sleep(2*FULLSWMR_DELTAT_SECONDS);

        new_alloc_addr2 = H5MF_alloc(file_ptr, H5FD_MEM_DEFAULT, H5AC_ind_read_dxpl_id, (hsize_t)(entry_size));
        if (new_alloc_addr2 == HADDR_UNDEF) 
            CACHE_ERROR("H5MF_alloc failed")
        else if (new_alloc_addr2 != allocated_base_addr_array_g)
            CACHE_ERROR("Freed space is not reused after 2 delta t time");
    }

done:
    if (new_alloc_addr1 != HADDR_UNDEF) {
        if (H5MF_xfree(file_ptr, H5FD_MEM_DEFAULT, H5AC_ind_read_dxpl_id, new_alloc_addr1, entry_size) < 0) 
            CACHE_ERROR("H5MF_xfree failed")
    }

    if (new_alloc_addr2 != HADDR_UNDEF) {
        if (H5MF_xfree(file_ptr, H5FD_MEM_DEFAULT, H5AC_ind_read_dxpl_id, new_alloc_addr2, entry_size) < 0) 
            CACHE_ERROR("H5MF_xfree failed")
    }

    if(file_ptr)
        fullswmr_takedown_cache(file_ptr, FALSE, FALSE);

    if(pass_g)
        PASSED()
    else {
        H5_FAILED();
        HDfprintf(stdout, "%s.\n", failure_mssg_g);
    } /* end else */

    return (unsigned)!pass_g;

}



/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:  Houjun Tang
 *              10/2/17
 *
 *-------------------------------------------------------------------------
 */

int
main(void)
{
    unsigned nerrs = 0;
    const char *driver_env = NULL;

    /* Don't run this test using certain file drivers */
    driver_env = HDgetenv("HDF5_DRIVER");
    if(driver_env == NULL)
        driver_env = "none";

    /* Only perform this test with default (sec2), none, or core file driver */
    if(HDstrcmp(driver_env, "sec2") != 0 && HDstrcmp(driver_env, "none") != 0 
                                         && HDstrcmp(driver_env, "core") != 0) {
        printf("Unsuported file driver [%s], exiting current test.\n", driver_env);
        return EXIT_SUCCESS;
    }

    H5open();

    printf("================================================\n");
    printf("Internal full SWMR cache memory allocation tests\n");
    printf("================================================\n");

    /* Each test will call setup_cache() which set up the file space */
    nerrs += check_freedspace_without_swmr_write();
    nerrs += check_holding_tank_with_swmr_write();
    nerrs += check_freedspace_flush_dependency();
    nerrs += check_freedspace_reuse_2deltat();

    if(nerrs > 0)
        return EXIT_FAILURE;
    else
        return EXIT_SUCCESS;

} /* main() */

