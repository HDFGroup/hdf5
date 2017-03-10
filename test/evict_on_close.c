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
 * Programmer:  Dana Robinson
 *              Spring 2016
 *
 * Purpose:     Tests the basic operation of the evict-on-close cache
 *              behavior. Tests that ensure the tagging is handled correctly
 *              are located in cache.c.
 */

#define H5C_FRIEND		/*suppress error about including H5Cpkg   */
#define H5D_FRIEND		/*suppress error about including H5Dpkg	  */
#define H5D_TESTING
#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */
#define H5F_TESTING
#define H5G_FRIEND		/*suppress error about including H5Gpkg	  */
#define H5I_FRIEND		/*suppress error about including H5Ipkg	  */
#define H5I_TESTING


#include "h5test.h"
#include "H5Cpkg.h"
#include "H5Dpkg.h"
#include "H5Fpkg.h"
#include "H5Gpkg.h"
#include "H5Ipkg.h"

/* Uncomment to manually inspect cache states */
/* #define EOC_MANUAL_INSPECTION */

const char *FILENAMES[] = {
    "evict-on-close",           /* 0 */
    NULL
};
#define FILENAME_BUF_SIZE       1024

/* Group names */
#define GROUP_OLD_STYLE_NAME        "old_style_groups"
#define GROUP_NEW_STYLE_NAME        "new_style_groups"

/* Dataset names */
#define DSET_COMPACT_NAME           "compact"
#define DSET_CONTIGUOUS_NAME        "contiguous"
#define DSET_BTREE_NAME             "v1_btree"
#define DSET_EARRAY_NAME            "earray"
#define DSET_BT2_NAME               "v2_btree"
#define DSET_FARRAY_NAME            "farray"
#define DSET_SINGLE_NAME            "single"

/* Number of data elements in a dataset */
#define NELEMENTS                   1024

/* Number of subgroups in each style of group */
#define NSUBGROUPS                  128

/* Max size of subgroup name, not including NULL */
#define SUBGROUP_NAME_SIZE          16

/* Prototypes */
static hbool_t verify_tag_not_in_cache(H5F_t *f, haddr_t tag);
static herr_t check_evict_on_close_api(void);
static hid_t generate_eoc_test_file(hid_t fapl_id);
static herr_t check_dset_scheme(hid_t fid, const char *dset_name);
static herr_t check_group_layout(hid_t fid, const char *group_name);


/*-------------------------------------------------------------------------
 * Function:    verify_tag_not_in_cache()
 *
 * Purpose:     Ensure that metadata cache entries with a given tag are not
 *              present in the cache.
 *
 * Return:      TRUE/FALSE
 *
 * Programmer:  Dana Robinson
 *              Fall 2016
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
verify_tag_not_in_cache(H5F_t *f, haddr_t tag)
{
    H5C_t *cache_ptr = NULL;                /* cache pointer                */
    int i = 0;                              /* iterator                     */

    /* Get Internal Cache Pointers */
    cache_ptr = f->shared->cache;

    for(i = 0; i < H5C__HASH_TABLE_LEN; i++) {
        H5C_cache_entry_t *entry_ptr;    /* entry pointer                */

        entry_ptr = cache_ptr->index[i];
        while(entry_ptr != NULL) {
            if(tag == entry_ptr->tag_info->tag)
                return TRUE;
            else
                entry_ptr = entry_ptr->ht_next;
        } /* end while */
    } /* end for */

    return FALSE;
} /* end verify_tag_not_in_cache() */


/*-------------------------------------------------------------------------
 * Function:    generate_eoc_test_file()
 *
 * Purpose:     Generate the evict-on-close test file.
 *
 * Return:      Success: The file ID of the created file
 *              Failure: -1
 *
 * Programmer:  Dana Robinson
 *              Fall 2016
 *
 *-------------------------------------------------------------------------
 */
static hid_t
generate_eoc_test_file(hid_t fapl_id)
{
    char    filename[FILENAME_BUF_SIZE];    /* decorated file name          */
    hid_t   fid = -1;                       /* file ID (returned)           */
    hid_t   fapl_copy_id = -1;              /* ID of copied fapl            */
    hid_t   gid1 = -1, gid2 = -1;           /* group IDs                    */
    hid_t   sid = -1;                       /* dataspace ID                 */
    hid_t   dcpl_id = -1;                   /* dataset creation plist       */
    hid_t   did = -1;                       /* dataset ID                   */
    int     rank;                           /* # of array dimensions        */
    hsize_t current_dims[2];                /* current dataset size         */
    hsize_t maximum_dims[2];                /* maximum dataset size         */
    hsize_t chunk_dims[2];                  /* chunk dimensions             */
    H5D_chunk_index_t idx_type;             /* dataset chunk index type     */
    H5D_layout_t layout_type;               /* dataset layout type          */
    int     *data = NULL;                   /* buffer for fake data         */
    int     n;                              /* # of data elements           */
    int     i;                              /* iterator (# subgroups)       */

    TESTING("generating evict-on-close test file");

    /* Get a VFD-specific filename */
    h5_fixname(FILENAMES[0], fapl_id, filename, sizeof(filename));

    /* Copy the fapl and set the latest file format */
    if((fapl_copy_id = H5Pcopy(fapl_id)) < 0)
        TEST_ERROR;
    if(H5Pset_libver_bounds(fapl_copy_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Create a data buffer for dataset writes */
    if(NULL == (data = (int *)HDcalloc(NELEMENTS, sizeof(int))))
        TEST_ERROR;

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;


    /*******************************************/
    /* CREATE OBJECTS WITH THE OLD FILE FORMAT */
    /*******************************************/

    /*******************/
    /* Generate groups */
    /*******************/

    /*********************************************/
    /* Old-style (version 1 B-tree + local heap) */
    /*********************************************/

    /* A single group is created in the root group, followed
     * by a large number of groups in the new group. This will
     * ensure that the file data structures for groups include
     * multiple cache entries.
     */
    if((gid1 = H5Gcreate2(fid, GROUP_OLD_STYLE_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create sub-groups */
    for(i = 0; i < NSUBGROUPS; i++) {
        char subgroup_name[SUBGROUP_NAME_SIZE];

        /* Create the group name */
        HDmemset(subgroup_name, '\0', SUBGROUP_NAME_SIZE);
        if(HDsnprintf(subgroup_name, (size_t)(SUBGROUP_NAME_SIZE - 1), "%d", i) < 0)
            TEST_ERROR

        if((gid2 = H5Gcreate2(gid1, subgroup_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR;
        if(H5Gclose(gid2) < 0)
            TEST_ERROR;
    } /* end for */

    if(H5Gclose(gid1) < 0)
        TEST_ERROR;

    /********************************************************************/
    /* Generate datasets and ensure that the chunking scheme is correct */
    /********************************************************************/

    /***********************************/
    /* Old file format data structures */
    /***********************************/

    /********************/
    /* Version 1 B-tree */
    /********************/

    /* Create dataspace */
    n = NELEMENTS;
    rank = 1;
    current_dims[0] = (hsize_t)n;
    maximum_dims[0] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(rank, current_dims, maximum_dims)) < 0)
        TEST_ERROR;

    /* Create dcpl and set up chunking */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    chunk_dims[0] = 1;
    if(H5Pset_chunk(dcpl_id, rank, chunk_dims) < 0)
        TEST_ERROR;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DSET_BTREE_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if(idx_type != H5D_CHUNK_IDX_BTREE)
        FAIL_PUTS_ERROR("should be using version 1 B-tree as the chunk index");

    /* Write a bunch of fake data */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* Close IDs for this dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR;
    if(H5Sclose(sid) < 0)
        TEST_ERROR;
    if(H5Pclose(dcpl_id) < 0)
        TEST_ERROR;


    /**********************************************/
    /* CREATE OBJECTS WITH THE LATEST FILE FORMAT */
    /**********************************************/

    /* Close the file */
    if(H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Reopen the file with the "latest file format" fapl */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_copy_id)) < 0)
        TEST_ERROR;

    /*******************/
    /* Generate groups */
    /*******************/

    /***********************************************/
    /* New-style (version 2 B-tree + fractal heap) */
    /***********************************************/

    /* A single group is created in the root group, followed
     * by a large number of groups in the new group. This will
     * ensure that the file data structures for groups include
     * multiple cache entries.
     */
    if((gid1 = H5Gcreate2(fid, GROUP_NEW_STYLE_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create sub-groups */
    for(i = 0; i < NSUBGROUPS; i++) {
        char subgroup_name[SUBGROUP_NAME_SIZE];

        /* Create the group name */
        HDmemset(subgroup_name, '\0', SUBGROUP_NAME_SIZE);
        if(HDsnprintf(subgroup_name, (size_t)(SUBGROUP_NAME_SIZE - 1), "%d", i) < 0)
            TEST_ERROR

        if((gid2 = H5Gcreate2(gid1, subgroup_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR;
        if(H5Gclose(gid2) < 0)
            TEST_ERROR;
    } /* end for */

    if(H5Gclose(gid1) < 0)
        TEST_ERROR;

    /********************************************************************/
    /* Generate datasets and ensure that the chunking scheme is correct */
    /********************************************************************/

    /********************/
    /* Extensible Array */
    /********************/

    /* Create dataspace */
    n = NELEMENTS;
    rank = 1;
    current_dims[0] = (hsize_t)n;
    maximum_dims[0] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(rank, current_dims, maximum_dims)) < 0)
        TEST_ERROR;

    /* Create dcpl and set up chunking */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    chunk_dims[0] = 1;
    if(H5Pset_chunk(dcpl_id, rank, chunk_dims) < 0)
        TEST_ERROR;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DSET_EARRAY_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if(idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("should be using extensible array as the chunk index");

    /* Write a bunch of fake data */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* Close IDs for this dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR;
    if(H5Sclose(sid) < 0)
        TEST_ERROR;
    if(H5Pclose(dcpl_id) < 0)
        TEST_ERROR;

    /********************/
    /* Version 2 B-Tree */
    /********************/

    /* Create dataspace */
    n = NELEMENTS;
    rank = 2;
    current_dims[0] = (hsize_t)2;
    current_dims[1] = (hsize_t)(n/2);
    maximum_dims[0] = H5S_UNLIMITED;
    maximum_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(rank, current_dims, maximum_dims)) < 0)
        TEST_ERROR;

    /* Create dcpl and set up chunking */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    chunk_dims[0] = 1;
    chunk_dims[1] = 1;
    if(H5Pset_chunk(dcpl_id, rank, chunk_dims) < 0)
        TEST_ERROR;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DSET_BT2_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if(idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using version 2 B-tree as the chunk index");

    /* Write a bunch of fake data */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* Close IDs for this dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR;
    if(H5Sclose(sid) < 0)
        TEST_ERROR;
    if(H5Pclose(dcpl_id) < 0)
        TEST_ERROR;

    /***************/
    /* Fixed Array */
    /***************/

    /* Create dataspace */
    n = NELEMENTS;
    rank = 1;
    current_dims[0] = (hsize_t)n;
    maximum_dims[0] = (hsize_t)n;
    if((sid = H5Screate_simple(rank, current_dims, maximum_dims)) < 0)
        TEST_ERROR;

    /* Create dcpl and set up chunking */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    chunk_dims[0] = 1;
    chunk_dims[1] = 1;
    if(H5Pset_chunk(dcpl_id, rank, chunk_dims) < 0)
        TEST_ERROR;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DSET_FARRAY_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if(idx_type != H5D_CHUNK_IDX_FARRAY)
        FAIL_PUTS_ERROR("should be using fixed array as the chunk index");

    /* Write a bunch of fake data */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* Close IDs for this dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR;
    if(H5Sclose(sid) < 0)
        TEST_ERROR;
    if(H5Pclose(dcpl_id) < 0)
        TEST_ERROR;

    /****************/
    /* Single Chunk */
    /****************/

    /* Create dataspace */
    n = NELEMENTS;
    rank = 1;
    current_dims[0] = (hsize_t)n;
    maximum_dims[0] = (hsize_t)n;
    if((sid = H5Screate_simple(rank, current_dims, maximum_dims)) < 0)
        TEST_ERROR;

    /* Create dcpl and set up chunking */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    chunk_dims[0] = (hsize_t)n;
    chunk_dims[1] = (hsize_t)n;
    if(H5Pset_chunk(dcpl_id, rank, chunk_dims) < 0)
        TEST_ERROR;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DSET_SINGLE_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if(idx_type != H5D_CHUNK_IDX_SINGLE)
        FAIL_PUTS_ERROR("should be using single chunk as the chunk index");

    /* Write a bunch of fake data */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* Close IDs for this dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR;
    if(H5Sclose(sid) < 0)
        TEST_ERROR;
    if(H5Pclose(dcpl_id) < 0)
        TEST_ERROR;

    /**************/
    /* Contiguous */
    /**************/

    /* Create dataspace */
    n = NELEMENTS;
    rank = 1;
    current_dims[0] = (hsize_t)n;
    maximum_dims[0] = (hsize_t)n;
    if((sid = H5Screate_simple(rank, current_dims, maximum_dims)) < 0)
        TEST_ERROR;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DSET_CONTIGUOUS_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct layout scheme */
    if(H5D__layout_type_test(did, &layout_type) < 0)
        TEST_ERROR;
    if(layout_type != H5D_CONTIGUOUS)
        FAIL_PUTS_ERROR("should be using contiguous layout");

    /* Write a bunch of fake data */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* Close IDs for this dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR;
    if(H5Sclose(sid) < 0)
        TEST_ERROR;

    /***********/
    /* Compact */
    /***********/

    /* Create dataspace */
    n = 1;
    rank = 1;
    current_dims[0] = (hsize_t)n;
    maximum_dims[0] = (hsize_t)n;
    if((sid = H5Screate_simple(rank, current_dims, maximum_dims)) < 0)
        TEST_ERROR;

    /* Create dcpl and set up compact layout */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if(H5Pset_layout(dcpl_id, H5D_COMPACT) < 0)
        TEST_ERROR;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DSET_COMPACT_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct layout scheme */
    if(H5D__layout_type_test(did, &layout_type) < 0)
        TEST_ERROR;
    if(layout_type != H5D_COMPACT)
        FAIL_PUTS_ERROR("should be using compact layout");

    /* Write a bunch of fake data */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* Close IDs for this dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR;
    if(H5Sclose(sid) < 0)
        TEST_ERROR;
    if(H5Pclose(dcpl_id) < 0)
        TEST_ERROR;

    /********/
    /* DONE */
    /********/

    /* Close/free everything else */
    if(H5Pclose(fapl_copy_id) < 0)
        TEST_ERROR;

    HDfree(data);

    PASSED();
    return fid;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid1);
        H5Gclose(gid2);
        H5Fclose(fid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl_id);
        H5Pclose(fapl_copy_id);
    } H5E_END_TRY;

    HDfree(data);

    H5_FAILED();
    return -1;

} /* end generate_eoc_test_file() */


/*-------------------------------------------------------------------------
 * Function:    check_group_layout()
 *
 * Purpose:     Verify that the evict-on-close feature works for a given
 *              group layout (new-style vs. old-style).
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Fall 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
check_group_layout(hid_t fid, const char *group_name)
{
    H5F_t   *file_ptr = NULL;               /* ptr to internal file struct  */
    hid_t   gid1 = -1, gid2 = -1;           /* group IDs                    */
    H5G_t   *grp_ptr = NULL;                /* ptr to internal group struct */
    haddr_t tag1, tag2;                     /* MD cache tags for groups     */
    uint32_t before, during, after;         /* cache sizes                  */
    int i;                                  /* iterator                     */

    /* NOTE: The TESTING() macro is called in main() */

    /* Get a pointer to the file struct */
    if(NULL == (file_ptr = (H5F_t *)H5I_object_verify(fid, H5I_FILE)))
        TEST_ERROR;

    /* Record the number of cache entries */
    before = file_ptr->shared->cache->index_len;

#ifdef EOC_MANUAL_INSPECTION
    HDprintf("\nCACHE BEFORE GROUP OPEN:\n");
    if(H5AC_dump_cache(file_ptr) < 0)
        TEST_ERROR;
    HDprintf("NUMBER OF CACHE ENTRIES: %u\n", before);
#endif

    /* Open the main group and get its tag */
    if((gid1 = H5Gopen2(fid, group_name, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if(NULL == (grp_ptr = (H5G_t *)H5I_object_verify(gid1, H5I_GROUP)))
        TEST_ERROR;
    tag1 = grp_ptr->oloc.addr;


    /* Open and close all sub-groups */
    for(i = 0; i < NSUBGROUPS; i++) {
        char subgroup_name[SUBGROUP_NAME_SIZE];

        /* Create the group name */
        HDmemset(subgroup_name, '\0', SUBGROUP_NAME_SIZE);
        if(HDsnprintf(subgroup_name, (size_t)(SUBGROUP_NAME_SIZE - 1), "%d", i) < 0)
            TEST_ERROR

        if((gid2 = H5Gopen2(gid1, subgroup_name, H5P_DEFAULT)) < 0)
            TEST_ERROR;

        if(NULL == (grp_ptr = (H5G_t *)H5I_object_verify(gid2, H5I_GROUP)))
            TEST_ERROR;
        tag2 = grp_ptr->oloc.addr;

        if(H5Gclose(gid2) < 0)
            TEST_ERROR;

        if(TRUE == verify_tag_not_in_cache(file_ptr, tag2))
            TEST_ERROR;
    } /* end for */

    /* Record the number of cache entries */
    during = file_ptr->shared->cache->index_len;

#ifdef EOC_MANUAL_INSPECTION
    HDprintf("\nCACHE AFTER OPENING GROUPS (WHILE OPEN):\n");
    if(H5AC_dump_cache(file_ptr) < 0)
        TEST_ERROR;
    HDprintf("MAIN GROUP TAG: %#X\n", tag1);
    HDprintf("NUMBER OF CACHE ENTRIES: %u\n", during);
#endif

    /* Close the main group */
    if(H5Gclose(gid1) < 0)
        TEST_ERROR;

    /* Record the number of cache entries */
    after = file_ptr->shared->cache->index_len;

#ifdef EOC_MANUAL_INSPECTION
    HDprintf("\nCACHE AFTER CLOSING GROUPS:\n");
    if(H5AC_dump_cache(file_ptr) < 0)
        TEST_ERROR;
    HDprintf("NUMBER OF CACHE ENTRIES: %u\n", after);
#endif

    /* Ensure that the cache does not contain entries with the tag */
    if(TRUE == verify_tag_not_in_cache(file_ptr, tag1))
        TEST_ERROR;
    /* Compare the number of cache entries */
    if(before != after || before == during)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid1);
        H5Gclose(gid2);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;

} /* end check_group_layout() */


/*-------------------------------------------------------------------------
 * Function:    check_dset_scheme()
 *
 * Purpose:     Verify that the evict-on-close feature works for a given
 *              dataset layout and/or chunk index.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Fall 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
check_dset_scheme(hid_t fid, const char *dset_name)
{
    H5F_t   *file_ptr = NULL;               /* ptr to internal file struct  */
    hid_t   did = -1;                       /* dataset ID                   */
    H5D_t   *dset_ptr = NULL;               /* ptr to internal dset struct  */
    haddr_t tag;                            /* MD cache tag for dataset     */
    int     *data = NULL;                   /* buffer for fake data         */
    uint32_t before, during, after;         /* cache sizes                  */

    /* NOTE: The TESTING() macro is called in main() */

    /* Get a pointer to the file struct */
    if(NULL == (file_ptr = (H5F_t *)H5I_object_verify(fid, H5I_FILE)))
        TEST_ERROR;

    /* Create the data buffer */
    if(NULL == (data = (int *)HDcalloc(NELEMENTS, sizeof(int))))
        TEST_ERROR;

    /* Record the number of cache entries */
    before = file_ptr->shared->cache->index_len;

#ifdef EOC_MANUAL_INSPECTION
    HDprintf("\nCACHE BEFORE DATASET OPEN:\n");
    if(H5AC_dump_cache(file_ptr) < 0)
        TEST_ERROR;
    HDprintf("NUMBER OF CACHE ENTRIES: %u\n", before);
#endif

    /* Open dataset and get the metadata tag */
    if((did = H5Dopen2(fid, dset_name, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if(NULL == (dset_ptr = (H5D_t *)H5I_object_verify(did, H5I_DATASET)))
        TEST_ERROR;
    tag = dset_ptr->oloc.addr;

    /* Read data from the dataset so the cache gets populated with chunk entries
     * and the like.
     */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* Record the number of cache entries */
    during = file_ptr->shared->cache->index_len; 

#ifdef EOC_MANUAL_INSPECTION
    HDprintf("\nCACHE AFTER DATA READ (WHILE OPEN):\n");
    if(H5AC_dump_cache(file_ptr) < 0)
        TEST_ERROR;
    HDprintf("TAG: %#X\n", tag);
    HDprintf("NUMBER OF CACHE ENTRIES: %u\n", during);
#endif

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR;

    /* Record the number of cache entries */
    after = file_ptr->shared->cache->index_len;

#ifdef EOC_MANUAL_INSPECTION
    HDprintf("\nCACHE AFTER DATASET CLOSE:\n");
    if(H5AC_dump_cache(file_ptr) < 0)
        TEST_ERROR;
    HDprintf("NUMBER OF CACHE ENTRIES: %u\n", after);
#endif

    /* Ensure that the cache does not contain entries with the tag */
    if(TRUE == verify_tag_not_in_cache(file_ptr, tag))
        TEST_ERROR;

    /* Compare the number of cache entries */
    if(before != after || before == during)
        TEST_ERROR;

    HDfree(data);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;

} /* check_dset_scheme() */


/*-------------------------------------------------------------------------
 * Function:    check_evict_on_close_api()
 *
 * Purpose:     Verify that the H5Pset/get_evict_on_close() calls behave
 *              correctly.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Spring 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
check_evict_on_close_api(void)
{
    hid_t       fapl_id = -1;
    hid_t       dapl_id = -1;
    hbool_t     evict_on_close;
    herr_t      status;

    TESTING("evict on close API");

    /* Create a fapl */
    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Check the default */
    evict_on_close = TRUE;
    if(H5Pget_evict_on_close(fapl_id, &evict_on_close) < 0)
        TEST_ERROR;
    if(evict_on_close != FALSE)
        FAIL_PUTS_ERROR("Incorrect default evict on close value.");

    /* Set the evict on close property */
    evict_on_close = TRUE;
    if(H5Pset_evict_on_close(fapl_id, evict_on_close) < 0)
        TEST_ERROR;

    /* Make sure we can get it back out */
    evict_on_close = FALSE;
    if(H5Pget_evict_on_close(fapl_id, &evict_on_close) < 0)
        TEST_ERROR;
    if(evict_on_close != TRUE)
        FAIL_PUTS_ERROR("Incorrect evict on close value.");

    /* close fapl */
    if(H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    /**********************************************/
    /* Trying passing in a non-fapl property list */
    /**********************************************/

    if((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        TEST_ERROR;

    /* ensure using an incorrect access plist fails */
    H5E_BEGIN_TRY {
        status = H5Pset_evict_on_close(dapl_id, evict_on_close);
    } H5E_END_TRY;
    if(status >= 0)
        FAIL_PUTS_ERROR("H5Pset_evict_on_close() accepted invalid access plist.");

    /* ensure an invalid plist fails */
    H5E_BEGIN_TRY {
        status = H5Pget_evict_on_close((hid_t)-1, &evict_on_close);
    } H5E_END_TRY;
    if(status >= 0)
        FAIL_PUTS_ERROR("H5Pget_evict_on_close() accepted invalid hid_t.");

    /* close dapl */
    if(H5Pclose(dapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5_FAILED();
    return FAIL;

} /* check_evict_on_close_api() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Return:      EXIT_FAILURE/EXIT_SUCCESS
 *
 * Programmer:  Dana Robinson
 *              Spring 2016
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fapl_id = -1;       /* VFD-specific fapl                    */
    hid_t       fid = -1;           /* file ID                              */
    unsigned    nerrors = 0;        /* number of test errors                */

    HDprintf("Testing evict-on-close cache behavior\n");

    /* Initialize */
    h5_reset();

    /* Test H5P call to set up EoC (does not require VFD-specific fapl) */
    nerrors += check_evict_on_close_api() < 0 ? 1 : 0;

    /* Set up VFD-specific fapl */
    if((fapl_id = h5_fileaccess()) < 0) {
        nerrors++;
        PUTS_ERROR("Unable to get VFD-specific fapl\n");
    } /* end if */

    /* Set evict-on-close property */
    if(H5Pset_evict_on_close(fapl_id, TRUE) < 0) {
        nerrors++;
        PUTS_ERROR("Unable to set evict-on-close property\n");
    } /* end if */

    /* Generate the test file */
    if((fid = generate_eoc_test_file(fapl_id)) < 0) {
        nerrors++;
        PUTS_ERROR("Unable to generate test file\n");
    } /* end if */

    /* Run tests with a variety of dataset configurations
     * PASSED() and H5_FAILED() are handled in check_configuration()
     */
    TESTING("evict on close with version 1 B-tree chunk index");
        nerrors += check_dset_scheme(fid, DSET_BTREE_NAME) < 0 ? 1 : 0;
    TESTING("evict on close with extensible array chunk index");
        nerrors += check_dset_scheme(fid, DSET_EARRAY_NAME) < 0 ? 1 : 0;
    TESTING("evict on close with version 2 B-tree chunk index");
        nerrors += check_dset_scheme(fid, DSET_BT2_NAME) < 0 ? 1 : 0;
    TESTING("evict on close with fixed array chunk index");
        nerrors += check_dset_scheme(fid, DSET_FARRAY_NAME) < 0 ? 1 : 0;
    TESTING("evict on close with \'single chunk\' chunk index");
        nerrors += check_dset_scheme(fid, DSET_SINGLE_NAME) < 0 ? 1 : 0;
    TESTING("evict on close with contiguous layout");
        nerrors += check_dset_scheme(fid, DSET_CONTIGUOUS_NAME) < 0 ? 1 : 0;
    TESTING("evict on close with compact layout");
        nerrors += check_dset_scheme(fid, DSET_COMPACT_NAME) < 0 ? 1 : 0;

    /* Run tests with old- and new-style groups
     * PASSED() and H5_FAILED() are handled in check_configuration()
     */
    TESTING("evict on close with old-style groups");
        nerrors += check_group_layout(fid, GROUP_OLD_STYLE_NAME) < 0 ? 1 : 0;
    TESTING("evict on close with new-style groups");
        nerrors += check_group_layout(fid, GROUP_NEW_STYLE_NAME) < 0 ? 1 : 0;

    /* Close the test file */
    if(H5Fclose(fid) < 0) {
        nerrors++;
        PUTS_ERROR("Unable to close the test file.\n");
    } /* end if */

    /* Clean up files and close the VFD-specific fapl */
    h5_delete_all_test_files(FILENAMES, fapl_id);
    if(H5Pclose(fapl_id) < 0) {
        nerrors++;
        PUTS_ERROR("Unable to close VFD-specific fapl.\n");
    } /* end if */

    if(nerrors)
        goto error;

    HDprintf("All evict-on-close tests passed.\n");

    return EXIT_SUCCESS;

error:

    HDprintf("***** %u evict-on-close test%s FAILED! *****\n",
        nerrors, nerrors > 1 ? "S" : "");

    h5_delete_all_test_files(FILENAMES, fapl_id);
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fapl_id);
    } H5E_END_TRY;

    return EXIT_FAILURE;

} /* end main() */

