/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:    Test H5Ocopy() for references.
 */

#include "testhdf5.h"

#define H5F_FRIEND /*suppress error about including H5Fpkg */
#define H5F_TESTING
#include "H5Fpkg.h" /* File access                          */

static const char *FILENAME[] = {"objcopy_ref_src",
                                 "objcopy_ref_dst",
                                 "objcopy_ref_ext",
                                 "objcopy_ref_src2",
                                 "verbound_ref_src",
                                 "verbound_ref_dst",
                                 NULL};

/* Configuration, really a series of bit flags.  Maximum value is all three
 * bit flags enabled.
 */
#define CONFIG_SHARE_SRC      1
#define CONFIG_SHARE_DST      2
#define CONFIG_SRC_NEW_FORMAT 4
#define CONFIG_DST_NEW_FORMAT 8
#define CONFIG_DENSE          16
#define MAX_CONFIGURATION     31

#define NAME_DATASET_SIMPLE      "dataset_simple"
#define NAME_DATASET_SUB_SUB     "/g0/g00/g000/dataset_simple"
#define NAME_GROUP_UNCOPIED      "/uncopied"
#define NAME_GROUP_TOP           "/g0"
#define NAME_GROUP_SUB           "/g0/g00"
#define NAME_GROUP_SUB_SUB2      "g000"
#define NAME_GROUP_LINK          "/g_links"
#define NAME_GROUP_LINK2         "/g_links2"
#define NAME_GROUP_REF           "ref_grp"
#define NAME_LINK_SOFT           "/g_links/soft_link_to_dataset_simple"
#define NAME_LINK_SOFT2          "/g_links2/soft_link_to_dataset_simple"
#define NAME_LINK_EXTERN         "/g_links/external_link_to_dataset_simple"
#define NAME_LINK_EXTERN2        "/g_links2/external_link_to_dataset_simple"
#define NAME_LINK_SOFT_DANGLE    "/g_links/soft_link_to_nowhere"
#define NAME_LINK_SOFT_DANGLE2   "/g_links2/soft_link_to_nowhere"
#define NAME_LINK_EXTERN_DANGLE  "/g_links/external_link_to_nowhere"
#define NAME_LINK_EXTERN_DANGLE2 "/g_links2/external_link_to_nowhere"

#define NAME_BUF_SIZE 1024
#define ATTR_NAME_LEN 80
#define DIM_SIZE_1    12
#define DIM_SIZE_2    6

unsigned num_attributes_g; /* Number of attributes created */

/* Table containing object id and object name */
/* (Used for detecting duplicate objects when comparing groups */
static struct {
    size_t       nalloc; /* number of slots allocated */
    size_t       nobjs;  /* number of objects */
    H5O_token_t *obj;    /* tokens for objects seen */
} idtab_g;

/* Local function prototypes */
static int compare_data(hid_t parent1, hid_t parent2, hid_t pid, hid_t tid, size_t nelmts, const void *buf1,
                        const void *buf2, hid_t obj_owner);
static int compare_datasets(hid_t did, hid_t did2, hid_t pid, const void *wbuf);
static int compare_groups(hid_t gid, hid_t gid2, hid_t pid, int depth, unsigned copy_flags);

/*-------------------------------------------------------------------------
 * Function:    token_insert
 *
 * Purpose:     Add a token to the table.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
token_insert(H5O_info2_t *oinfo)
{
    size_t n;

    /* Don't add it if the link count is 1 because such an object can only
     * be encountered once. */
    if (oinfo->rc < 2)
        return;

    /* Extend the table */
    if (idtab_g.nobjs >= idtab_g.nalloc) {
        idtab_g.nalloc = MAX(256, 2 * idtab_g.nalloc);
        idtab_g.obj    = (H5O_token_t *)realloc(idtab_g.obj, idtab_g.nalloc * sizeof(idtab_g.obj[0]));
    }

    /* Insert the entry */
    n              = idtab_g.nobjs++;
    idtab_g.obj[n] = oinfo->token;
} /* end token_insert() */

/*-------------------------------------------------------------------------
 * Function:    token_lookup
 *
 * Purpose:     Check if a token has already been encountered
 *
 * Return:      Success:    true/false
 *              Failure:    (can't fail)
 *
 *-------------------------------------------------------------------------
 */
static H5_ATTR_PURE bool
token_lookup(hid_t loc_id, H5O_info2_t *oinfo)
{
    size_t n;
    int    token_cmp;

    if (oinfo->rc < 2)
        return false; /*only one link possible*/

    for (n = 0; n < idtab_g.nobjs; n++) {
        if (H5Otoken_cmp(loc_id, &(idtab_g.obj[n]), &oinfo->token, &token_cmp) < 0)
            return false;
        if (0 == token_cmp)
            return true;
    }

    return false;
} /* end token_lookup() */

/*-------------------------------------------------------------------------
 * Function:    token_reset
 *
 * Purpose:     Reset the token tracking data structures
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
token_reset(void)
{
    if (idtab_g.obj)
        free(idtab_g.obj);
    idtab_g.obj    = NULL;
    idtab_g.nalloc = idtab_g.nobjs = 0;
} /* end token_reset() */

/*-------------------------------------------------------------------------
 * Function:    attach_ref_attr
 *
 * Purpose:     Create an attribute with object references
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
attach_ref_attr(hid_t file_id, hid_t loc_id)
{
    char  dsetname1[] = "dataset1_pointed_by_ref_attr";
    char  dsetname2[] = "dataset2_pointed_by_ref_attr";
    hid_t did1 = (H5I_INVALID_HID), did2 = (H5I_INVALID_HID), aid = (H5I_INVALID_HID),
          sid = (H5I_INVALID_HID), sid_ref = (H5I_INVALID_HID);
    hsize_t   dims[2]     = {2, 9};
    hsize_t   dims_ref[1] = {2};
    H5R_ref_t ref[2];
    int       data1[2][9] = {{1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 18}};
    int       data2[2][9] = {{2, 2, 2, 2, 2, 2, 2, 2, 2}, {2, 2, 2, 2, 2, 2, 2, 2, 18}};

    /* creates two simple datasets */
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((sid_ref = H5Screate_simple(1, dims_ref, NULL)) < 0)
        TEST_ERROR;
    if ((did1 = H5Dcreate2(file_id, dsetname1, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0)
        TEST_ERROR;
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data1) < 0)
        TEST_ERROR;
    if ((did2 = H5Dcreate2(file_id, dsetname2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0)
        TEST_ERROR;
    if (H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data2) < 0)
        TEST_ERROR;

    /* create an attribute with two object references */
    if (H5Rcreate_object(file_id, dsetname1, H5P_DEFAULT, &ref[0]) < 0)
        TEST_ERROR;
    if (H5Rcreate_object(file_id, dsetname2, H5P_DEFAULT, &ref[1]) < 0)
        TEST_ERROR;
    if ((aid = H5Acreate2(loc_id, "obj_ref_attr", H5T_STD_REF, sid_ref, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Awrite(aid, H5T_STD_REF, ref) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Sclose(sid_ref) < 0)
        TEST_ERROR;
    if (H5Dclose(did1) < 0)
        TEST_ERROR;
    if (H5Dclose(did2) < 0)
        TEST_ERROR;
    if (H5Aclose(aid) < 0)
        TEST_ERROR;
    if (H5Rdestroy(&ref[0]) < 0)
        TEST_ERROR;
    if (H5Rdestroy(&ref[1]) < 0)
        TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Sclose(sid_ref);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Aclose(aid);
        H5Rdestroy(&ref[0]);
        H5Rdestroy(&ref[1]);
    }
    H5E_END_TRY

    return (-1);
}

/*-------------------------------------------------------------------------
 * Function:    attach_reg_ref_attr
 *
 * Purpose:     Create an attribute with object references
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
attach_reg_ref_attr(hid_t file_id, hid_t loc_id)
{
    const char dsetnamev[] = "dataset_pointed_by_reg_ref_attr";
    hid_t      aid         = (H5I_INVALID_HID);
    hid_t      space_id    = (H5I_INVALID_HID); /* dataspace identifiers */
    hid_t      spacer_id   = (H5I_INVALID_HID); /* dataspace identifiers */
    hid_t      dsetv_id    = (H5I_INVALID_HID); /*dataset identifiers*/
    hsize_t    dims[2]     = {2, 9};
    hsize_t    dimsr[1]    = {2};
    int        rank        = 2;
    int        rankr       = 1;
    H5R_ref_t  ref[2];
    int        data[2][9]  = {{1, 1, 2, 3, 3, 4, 5, 5, 999}, {1, 2, 2, 3, 4, 4, 5, 6, 999}};
    hsize_t    start[2]    = {0, 3};
    hsize_t    count[2]    = {2, 3};
    hsize_t    coord[3][2] = {{0, 0}, {1, 6}, {0, 8}};
    size_t     num_points  = 3;

    /* create a 2D dataset */
    if ((space_id = H5Screate_simple(rank, dims, NULL)) < 0)
        TEST_ERROR;
    if ((spacer_id = H5Screate_simple(rankr, dimsr, NULL)) < 0)
        TEST_ERROR;
    if ((dsetv_id = H5Dcreate2(file_id, dsetnamev, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    /* create reg_ref of block selection */
    if (H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR;
    if (H5Rcreate_region(file_id, dsetnamev, space_id, H5P_DEFAULT, &ref[0]) < 0)
        TEST_ERROR;

    /* create reg_ref of point selection */
    if (H5Sselect_none(space_id) < 0)
        TEST_ERROR;
    if (H5Sselect_elements(space_id, H5S_SELECT_SET, num_points, (const hsize_t *)coord) < 0)
        TEST_ERROR;
    if (H5Rcreate_region(file_id, dsetnamev, space_id, H5P_DEFAULT, &ref[1]) < 0)
        TEST_ERROR;

    /* create reg_ref attribute */
    if ((aid = H5Acreate2(loc_id, "reg_ref_attr", H5T_STD_REF, spacer_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Awrite(aid, H5T_STD_REF, ref) < 0)
        TEST_ERROR;

    /* attach the reg_ref attribute to the dataset itself */
    if (H5Aclose(aid) < 0)
        TEST_ERROR;
    if ((aid = H5Acreate2(dsetv_id, "reg_ref_attr", H5T_STD_REF, spacer_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Awrite(aid, H5T_STD_REF, ref) < 0)
        TEST_ERROR;

    if (H5Sclose(spacer_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dsetv_id) < 0)
        TEST_ERROR;
    if (H5Aclose(aid) < 0)
        TEST_ERROR;
    if (H5Rdestroy(&ref[0]) < 0)
        TEST_ERROR;
    if (H5Rdestroy(&ref[1]) < 0)
        TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(spacer_id);
        H5Sclose(space_id);
        H5Dclose(dsetv_id);
        H5Aclose(aid);
        H5Rdestroy(&ref[0]);
        H5Rdestroy(&ref[1]);
    }
    H5E_END_TRY

    return (-1);
}

/*-------------------------------------------------------------------------
 * Function:    create_reg_ref_dataset
 *
 * Purpose:     Create a dataset with region references
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
create_reg_ref_dataset(hid_t file_id, hid_t loc_id)
{
    const char dsetnamev[]  = "dataset_pointed_by_ref_dset";
    const char dsetnamer[]  = "dataset_with_reg_ref";
    const char dsetnamer1[] = "compact_dataset_with_reg_ref";
    const char dsetnamer2[] = "compressed_dataset_with_reg_ref";
    hid_t      space_id     = (H5I_INVALID_HID); /* dataspace identifiers */
    hid_t      spacer_id    = (H5I_INVALID_HID);
    hid_t      dsetv_id     = (H5I_INVALID_HID); /*dataset identifiers*/
    hid_t      dsetr_id     = (H5I_INVALID_HID);
    hsize_t    dims[2]      = {2, 9};
    hsize_t    dimsr[1]     = {2};
    int        rank         = 2;
    int        rankr        = 1;
    hsize_t    chunk_size   = 1;
    H5R_ref_t  ref[2];
    int        data[2][9] = {{1, 1, 2, 3, 3, 4, 5, 5, 6}, {1, 2, 2, 3, 4, 4, 5, 6, 6}};
    hsize_t    start[2];
    hsize_t    count[2];
    hsize_t    coord[3][2] = {{0, 0}, {1, 6}, {0, 8}};
    size_t     num_points  = 3;
    hid_t      pid         = (H5I_INVALID_HID);

    if ((space_id = H5Screate_simple(rank, dims, NULL)) < 0)
        TEST_ERROR;
    if ((spacer_id = H5Screate_simple(rankr, dimsr, NULL)) < 0)
        TEST_ERROR;
    if ((dsetv_id = H5Dcreate2(file_id, dsetnamev, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;
    if ((dsetr_id = H5Dcreate2(loc_id, dsetnamer, H5T_STD_REF, spacer_id, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0)
        TEST_ERROR;

    start[0] = 0;
    start[1] = 3;
    count[0] = 2;
    count[1] = 3;
    if (H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR;
    if (H5Rcreate_region(file_id, dsetnamev, space_id, H5P_DEFAULT, &ref[0]) < 0)
        TEST_ERROR;
    if (H5Sselect_none(space_id) < 0)
        TEST_ERROR;
    if (H5Sselect_elements(space_id, H5S_SELECT_SET, num_points, (const hsize_t *)coord) < 0)
        TEST_ERROR;
    if (H5Rcreate_region(file_id, dsetnamev, space_id, H5P_DEFAULT, &ref[1]) < 0)
        TEST_ERROR;
    if (H5Dwrite(dsetr_id, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref) < 0)
        TEST_ERROR;
    if (H5Dclose(dsetr_id) < 0)
        TEST_ERROR;

    /* create and set compact plist */
    if ((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_layout(pid, H5D_COMPACT) < 0)
        TEST_ERROR;

    if ((dsetr_id = H5Dcreate2(loc_id, dsetnamer1, H5T_STD_REF, spacer_id, H5P_DEFAULT, pid, H5P_DEFAULT)) <
        0)
        TEST_ERROR;
    if (H5Pclose(pid) < 0)
        TEST_ERROR;
    if (H5Dwrite(dsetr_id, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref) < 0)
        TEST_ERROR;
    if (H5Dclose(dsetr_id) < 0)
        TEST_ERROR;

    /* create and set comp & chunk plist */
    if ((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(pid, 1, &chunk_size) < 0)
        TEST_ERROR;
    if (H5Pset_deflate(pid, 9) < 0)
        TEST_ERROR;

    if ((dsetr_id = H5Dcreate2(loc_id, dsetnamer2, H5T_STD_REF, spacer_id, H5P_DEFAULT, pid, H5P_DEFAULT)) <
        0)
        TEST_ERROR;
    if (H5Pclose(pid) < 0)
        TEST_ERROR;
    if (H5Dwrite(dsetr_id, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref) < 0)
        TEST_ERROR;
    if (H5Dclose(dsetr_id) < 0)
        TEST_ERROR;

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Sclose(spacer_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dsetv_id) < 0)
        TEST_ERROR;
    if (H5Rdestroy(&ref[0]) < 0)
        TEST_ERROR;
    if (H5Rdestroy(&ref[1]) < 0)
        TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Sclose(spacer_id);
        H5Dclose(dsetr_id);
        H5Dclose(dsetv_id);
        H5Pclose(pid);
        H5Rdestroy(&ref[0]);
        H5Rdestroy(&ref[1]);
    }
    H5E_END_TRY

    return (-1);
}

/*-------------------------------------------------------------------------
 * Function:    test_copy_attach_attributes
 *
 * Purpose:     Attach NUM_ATTRIBUTES attributes to the object to be copied
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_attach_attributes(hid_t loc_id, hid_t type_id)
{
    hid_t    aid = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    char     attr_name[ATTR_NAME_LEN];
    int      attr_data[2];
    hsize_t  dim1 = 2;
    hid_t    acpl = H5I_INVALID_HID;
    unsigned u;
    int      ret_value = -1;

    if ((sid = H5Screate_simple(1, &dim1, NULL)) < 0)
        goto done;

    /* Create attribute creation plist */
    if ((acpl = H5Pcreate(H5P_ATTRIBUTE_CREATE)) < 0)
        goto done;

    for (u = 0; u < num_attributes_g; u++) {
        snprintf(attr_name, sizeof(attr_name), "%u attr", u);

        /* Set attribute data */
        attr_data[0] = (int)(100 * u);
        attr_data[1] = (int)(200 * u);

        /* Set attribute character set (alternate) */
        if (u % 2) {
            if (H5Pset_char_encoding(acpl, H5T_CSET_ASCII) < 0)
                goto done;
        } /* end if */
        else if (H5Pset_char_encoding(acpl, H5T_CSET_UTF8) < 0)
            goto done;

        if ((aid = H5Acreate2(loc_id, attr_name, type_id, sid, acpl, H5P_DEFAULT)) < 0)
            goto done;

        if (H5Awrite(aid, H5T_NATIVE_INT, attr_data) < 0)
            goto done;

        if (aid > 0)
            H5Aclose(aid);

        aid = -1;
    }

    ret_value = 0;

done:
    if (sid > 0)
        H5Sclose(sid);
    if (aid > 0)
        H5Aclose(aid);
    if (acpl > 0)
        H5Pclose(acpl);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    compare_attribute
 *
 * Purpose:     Compare two attributes to check that they are equal
 *
 * Return:      true if attributes are equal/false if they are different
 *
 *-------------------------------------------------------------------------
 */
static int
compare_attribute(hid_t aid, hid_t aid2, hid_t pid, const void *wbuf, hid_t obj_owner)
{
    hid_t      sid = H5I_INVALID_HID, sid2 = H5I_INVALID_HID; /* Dataspace IDs */
    hid_t      tid = H5I_INVALID_HID, tid2 = H5I_INVALID_HID; /* Datatype IDs */
    size_t     elmt_size;                                     /* Size of datatype */
    htri_t     is_committed;                                  /* If the datatype is committed */
    htri_t     is_committed2;                                 /* If the datatype is committed */
    H5A_info_t ainfo;                                         /* Attribute info */
    H5A_info_t ainfo2;                                        /* Attribute info */
    hssize_t   nelmts;                                        /* # of elements in dataspace */
    void      *rbuf  = NULL;                                  /* Buffer for reading raw data */
    void      *rbuf2 = NULL;                                  /* Buffer for reading raw data */

    /* Check the character sets are equal */
    if (H5Aget_info(aid, &ainfo) < 0)
        TEST_ERROR;
    if (H5Aget_info(aid2, &ainfo2) < 0)
        TEST_ERROR;
    if (ainfo.cset != ainfo2.cset)
        TEST_ERROR;

    /* Check the creation orders are equal (if appropriate) */
    if (ainfo.corder_valid != ainfo2.corder_valid)
        TEST_ERROR;
    if (ainfo.corder_valid)
        if (ainfo.corder != ainfo2.corder)
            TEST_ERROR;

    /* Check the datatypes are equal */

    /* Open the datatype for the source attribute */
    if ((tid = H5Aget_type(aid)) < 0)
        TEST_ERROR;

    /* Open the datatype for the destination attribute */
    if ((tid2 = H5Aget_type(aid2)) < 0)
        TEST_ERROR;

    /* Check that both datatypes are committed/not committed */
    if ((is_committed = H5Tcommitted(tid)) < 0)
        TEST_ERROR;
    if ((is_committed2 = H5Tcommitted(tid2)) < 0)
        TEST_ERROR;
    if (is_committed != is_committed2)
        TEST_ERROR;

    /* Compare the datatypes */
    if (H5Tequal(tid, tid2) != true)
        TEST_ERROR;

    /* Determine the size of datatype (for later) */
    if ((elmt_size = H5Tget_size(tid)) == 0)
        TEST_ERROR;

    /* Check the dataspaces are equal */

    /* Open the dataspace for the source attribute */
    if ((sid = H5Aget_space(aid)) < 0)
        TEST_ERROR;

    /* Open the dataspace for the destination attribute */
    if ((sid2 = H5Aget_space(aid2)) < 0)
        TEST_ERROR;

    /* Compare the dataspaces */
    if (H5Sextent_equal(sid, sid2) != true)
        TEST_ERROR;

    /* Determine the number of elements in dataspace (for later) */
    if ((nelmts = H5Sget_simple_extent_npoints(sid2)) < 0)
        TEST_ERROR;

    /* Check the raw data is equal */

    /* Allocate & initialize space for the raw data buffers */
    if ((rbuf = calloc(elmt_size, (size_t)nelmts)) == NULL)
        TEST_ERROR;
    if ((rbuf2 = calloc(elmt_size, (size_t)nelmts)) == NULL)
        TEST_ERROR;

    /* Read data from the source attribute */
    if (H5Aread(aid, tid, rbuf) < 0)
        TEST_ERROR;

    /* Read data from the destination attribute */
    if (H5Aread(aid2, tid2, rbuf2) < 0)
        TEST_ERROR;

    /* Check raw data read in against data written out */
    if (wbuf) {
        if (!compare_data(aid, (hid_t)0, pid, tid, (size_t)nelmts, wbuf, rbuf, obj_owner))
            TEST_ERROR;
        if (!compare_data(aid2, (hid_t)0, pid, tid2, (size_t)nelmts, wbuf, rbuf2, obj_owner))
            TEST_ERROR;
    } /* end if */
    /* Don't have written data, just compare data between the two attributes */
    else if (!compare_data(aid, aid2, pid, tid, (size_t)nelmts, rbuf, rbuf2, obj_owner))
        TEST_ERROR;

    /* Reclaim vlen data, if necessary */
    if (H5Tdetect_class(tid, H5T_VLEN) == true || H5Tdetect_class(tid, H5T_REFERENCE) == true)
        if (H5Treclaim(tid, sid, H5P_DEFAULT, rbuf) < 0)
            TEST_ERROR;
    if (H5Tdetect_class(tid2, H5T_VLEN) == true || H5Tdetect_class(tid2, H5T_REFERENCE) == true)
        if (H5Treclaim(tid2, sid2, H5P_DEFAULT, rbuf2) < 0)
            TEST_ERROR;

    /* Release raw data buffers */
    free(rbuf);
    rbuf = NULL;
    free(rbuf2);
    rbuf2 = NULL;

    /* close the source dataspace */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    /* close the destination dataspace */
    if (H5Sclose(sid2) < 0)
        TEST_ERROR;

    /* close the source datatype */
    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    /* close the destination datatype */
    if (H5Tclose(tid2) < 0)
        TEST_ERROR;

    return true;

error:
    if (rbuf)
        free(rbuf);
    if (rbuf2)
        free(rbuf2);
    H5E_BEGIN_TRY
    {
        H5Sclose(sid2);
        H5Sclose(sid);
        H5Tclose(tid2);
        H5Tclose(tid);
    }
    H5E_END_TRY
    return false;
} /* end compare_attribute() */

/*-------------------------------------------------------------------------
 * Function:    compare_std_attributes
 *
 * Purpose:     Compare "standard" attributes on two objects to check that they are equal
 *
 * Return:    true if objects have same attributes/false if they are different
 *
 * Note:    This isn't very general, the attributes are assumed to be
 *              those written in test_copy_attach_attributes().
 *
 *-------------------------------------------------------------------------
 */
static int
compare_std_attributes(hid_t oid, hid_t oid2, hid_t pid)
{
    hid_t       aid = H5I_INVALID_HID, aid2 = H5I_INVALID_HID; /* Attribute IDs */
    H5O_info2_t oinfo1, oinfo2;                                /* Object info */
    unsigned    cpy_flags;                                     /* Object copy flags */

    /* Retrieve the object copy flags from the property list, if it's non-DEFAULT */
    if (pid != H5P_DEFAULT) {
        if (H5Pget_copy_object(pid, &cpy_flags) < 0)
            TEST_ERROR;
    } /* end if */
    else
        cpy_flags = 0;

    /* Check the number of attributes on source dataset */
    if (H5Oget_info3(oid, &oinfo1, H5O_INFO_NUM_ATTRS) < 0)
        TEST_ERROR;

    /* Check the number of attributes on destination dataset */
    if (H5Oget_info3(oid2, &oinfo2, H5O_INFO_NUM_ATTRS) < 0)
        TEST_ERROR;

    if (cpy_flags & H5O_COPY_WITHOUT_ATTR_FLAG) {
        /* Check that the destination has no attributes */
        if (oinfo2.num_attrs != 0)
            TEST_ERROR;
    } /* end if */
    else {
        char     attr_name[ATTR_NAME_LEN]; /* Attribute name */
        unsigned i;                        /* Local index variable */

        /* Compare the number of attributes */
        if (oinfo1.num_attrs != oinfo2.num_attrs)
            TEST_ERROR;

        /* Check the attributes are equal */
        for (i = 0; i < (unsigned)oinfo1.num_attrs; i++) {
            if ((aid = H5Aopen_by_idx(oid, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)i, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0)
                TEST_ERROR;
            if (H5Aget_name(aid, (size_t)ATTR_NAME_LEN, attr_name) < 0)
                TEST_ERROR;

            if ((aid2 = H5Aopen(oid2, attr_name, H5P_DEFAULT)) < 0)
                TEST_ERROR;

            /* Check the attributes are equal */
            if (!compare_attribute(aid, aid2, pid, NULL, oid))
                TEST_ERROR;

            /* Close the attributes */
            if (H5Aclose(aid) < 0)
                TEST_ERROR;
            if (H5Aclose(aid2) < 0)
                TEST_ERROR;
        } /* end for */
    }     /* end if */

    /* Objects should be the same. :-) */
    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid2);
        H5Aclose(aid);
    }
    H5E_END_TRY
    return false;
} /* end compare_std_attributes() */

/*-------------------------------------------------------------------------
 * Function:    compare_data
 *
 * Purpose:     Compare two buffers of data to check that they are equal
 *
 * Return:    true if buffer are equal/false if they are different
 *
 *-------------------------------------------------------------------------
 */
static int
compare_data(hid_t parent1, hid_t parent2, hid_t pid, hid_t tid, size_t nelmts, const void *buf1,
             const void *buf2, hid_t obj_owner)
{
    size_t elmt_size; /* Size of an element */

    /* Check size of each element */
    if ((elmt_size = H5Tget_size(tid)) == 0)
        TEST_ERROR;

    /* If the type is a compound containing a vlen, loop over all elements for
     * each compound member.  Compounds containing reference  are not supported
     * yet. */
    if ((H5Tget_class(tid) == H5T_COMPOUND) && (H5Tdetect_class(tid, H5T_VLEN) == true)) {
        hid_t          memb_id;   /* Member id */
        const uint8_t *memb1;     /* Pointer to current member */
        const uint8_t *memb2;     /* Pointer to current member */
        int            nmembs;    /* Number of members */
        size_t         memb_off;  /* Member offset */
        size_t         memb_size; /* Member size */
        unsigned       memb_idx;  /* Member index */
        size_t         elmt;      /* Current element */

        /* Get number of members in compound */
        if ((nmembs = H5Tget_nmembers(tid)) < 0)
            TEST_ERROR;

        /* Loop over members */
        for (memb_idx = 0; memb_idx < (unsigned)nmembs; memb_idx++) {
            /* Get member offset.  Note that we cannot check for an error here.
             */
            memb_off = H5Tget_member_offset(tid, memb_idx);

            /* Get member id */
            if ((memb_id = H5Tget_member_type(tid, memb_idx)) < 0)
                TEST_ERROR;

            /* Get member size */
            if ((memb_size = H5Tget_size(memb_id)) == 0)
                TEST_ERROR;

            /* Set up pointers to member in the first element */
            memb1 = (const uint8_t *)buf1 + memb_off;
            memb2 = (const uint8_t *)buf2 + memb_off;

            /* Check if this member contains (or is) a vlen */
            if (H5Tget_class(memb_id) == H5T_VLEN) {
                hid_t base_id; /* vlen base type id */

                /* Get base type of vlen datatype */
                if ((base_id = H5Tget_super(memb_id)) < 0)
                    TEST_ERROR;

                /* Iterate over all elements, recursively calling this function
                 * for each */
                for (elmt = 0; elmt < nelmts; elmt++) {
                    /* Check vlen lengths */
                    if (((const hvl_t *)((const void *)memb1))->len !=
                        ((const hvl_t *)((const void *)memb2))->len)
                        TEST_ERROR;

                    /* Check vlen data */
                    if (!compare_data(parent1, parent2, pid, base_id,
                                      ((const hvl_t *)((const void *)memb1))->len,
                                      ((const hvl_t *)((const void *)memb1))->p,
                                      ((const hvl_t *)((const void *)memb2))->p, obj_owner))
                        TEST_ERROR;

                    /* Update member pointers */
                    memb1 += elmt_size;
                    memb2 += elmt_size;
                } /* end for */
            }
            else {
                /* vlens cannot currently be nested below the top layer of a
                 * compound */
                assert(H5Tdetect_class(memb_id, H5T_VLEN) == false);

                /* Iterate over all elements, calling memcmp() for each */
                for (elmt = 0; elmt < nelmts; elmt++) {
                    if (memcmp(memb1, memb2, memb_size) != 0)
                        TEST_ERROR;

                    /* Update member pointers */
                    memb1 += elmt_size;
                    memb2 += elmt_size;
                } /* end for */
            }     /* end else */
        }         /* end for */
    }
    else if (H5Tdetect_class(tid, H5T_VLEN) == true) {
        const hvl_t *vl_buf1, *vl_buf2; /* Aliases for buffers to compare */
        hid_t        base_tid;          /* Base type of vlen datatype */
        size_t       u;                 /* Local index variable */

        /* Check for "simple" vlen datatype */
        if (H5Tget_class(tid) != H5T_VLEN)
            TEST_ERROR;

        /* Get base type of vlen datatype */
        if ((base_tid = H5Tget_super(tid)) < 0)
            TEST_ERROR;

        /* Loop over elements in buffers */
        vl_buf1 = (const hvl_t *)buf1;
        vl_buf2 = (const hvl_t *)buf2;
        for (u = 0; u < nelmts; u++, vl_buf1++, vl_buf2++) {
            /* Check vlen lengths */
            if (vl_buf1->len != vl_buf2->len)
                TEST_ERROR;

            /* Check vlen data */
            if (!compare_data(parent1, parent2, pid, base_tid, vl_buf1->len, vl_buf1->p, vl_buf2->p,
                              obj_owner))
                TEST_ERROR;
        } /* end for */

        if (H5Tclose(base_tid) < 0)
            TEST_ERROR;
    } /* end if */
    else if (H5Tdetect_class(tid, H5T_REFERENCE) == true) {
        size_t u; /* Local index variable */

        /* Check for "simple" reference datatype */
        if (H5Tget_class(tid) != H5T_REFERENCE)
            TEST_ERROR;

        /* Check for object or region reference */
        if (H5Tequal(tid, H5T_STD_REF) > 0) {
            H5R_ref_t *ref_buf1, *ref_buf2; /* Aliases for buffers to compare */

            /* Loop over elements in buffers */
            H5_GCC_CLANG_DIAG_OFF("cast-qual")
            ref_buf1 = (H5R_ref_t *)buf1;
            ref_buf2 = (H5R_ref_t *)buf2;
            H5_GCC_CLANG_DIAG_ON("cast-qual")
            for (u = 0; u < nelmts; u++, ref_buf1++, ref_buf2++) {
                hid_t      obj1_id, obj2_id;     /* IDs for objects referenced */
                H5O_type_t obj1_type, obj2_type; /* Types of objects referenced */

                /* Check for types of objects handled */
                if (H5Rget_obj_type3(ref_buf1, H5P_DEFAULT, &obj1_type) < 0)
                    TEST_ERROR;
                if (H5Rget_obj_type3(ref_buf2, H5P_DEFAULT, &obj2_type) < 0)
                    TEST_ERROR;
                if (obj1_type != obj2_type)
                    TEST_ERROR;

                /* Open referenced objects */
                if ((obj1_id = H5Ropen_object(ref_buf1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    TEST_ERROR;
                if ((obj2_id = H5Ropen_object(ref_buf2, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    TEST_ERROR;

                /* break the infinite loop when the ref_object points to itself */
                if (obj_owner > 0) {
                    H5O_info2_t oinfo1, oinfo2;
                    int         token_cmp;

                    if (H5Oget_info3(obj_owner, &oinfo1, H5O_INFO_BASIC) < 0)
                        TEST_ERROR;
                    if (H5Oget_info3(obj1_id, &oinfo2, H5O_INFO_BASIC) < 0)
                        TEST_ERROR;
                    if (H5Otoken_cmp(obj1_id, &oinfo1.token, &oinfo2.token, &token_cmp) < 0)
                        TEST_ERROR;
                    if (0 == token_cmp) {
                        if (H5Oclose(obj1_id) < 0)
                            TEST_ERROR;
                        if (H5Oclose(obj2_id) < 0)
                            TEST_ERROR;
                        return true;
                    }
                }

                /* Check for types of objects handled */
                switch (obj1_type) {
                    case H5O_TYPE_DATASET:
                        if (compare_datasets(obj1_id, obj2_id, pid, NULL) != true)
                            TEST_ERROR;
                        break;

                    case H5O_TYPE_GROUP:
                        if (compare_groups(obj1_id, obj2_id, pid, -1, 0) != true)
                            TEST_ERROR;
                        break;

                    case H5O_TYPE_NAMED_DATATYPE:
                        if (H5Tequal(obj1_id, obj2_id) != true)
                            TEST_ERROR;
                        break;

                    case H5O_TYPE_MAP:
                        /* Maps not supported in native VOL connector */

                    case H5O_TYPE_UNKNOWN:
                    case H5O_TYPE_NTYPES:
                    default:
                        TEST_ERROR;
                } /* end switch */

                /* Close objects */
                if (H5Oclose(obj1_id) < 0)
                    TEST_ERROR;
                if (H5Oclose(obj2_id) < 0)
                    TEST_ERROR;

                if (H5Rget_type(ref_buf1) == H5R_DATASET_REGION2) {
                    hid_t obj1_sid, obj2_sid; /* Dataspace IDs for objects referenced */

                    /* Get regions for referenced datasets */
                    if ((obj1_sid = H5Ropen_region(ref_buf1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                        TEST_ERROR;
                    if ((obj2_sid = H5Ropen_region(ref_buf2, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                        TEST_ERROR;

                    /* Check if dataspaces are the same shape */
                    if (H5Sselect_shape_same(obj1_sid, obj2_sid) < 0)
                        TEST_ERROR;

                    /* Close dataspaces */
                    if (H5Sclose(obj1_sid) < 0)
                        TEST_ERROR;
                    if (H5Sclose(obj2_sid) < 0)
                        TEST_ERROR;
                } /* end if */
            }     /* end for */
        }         /* end if */
        else
            TEST_ERROR;
    } /* end else */
    else if (memcmp(buf1, buf2, (elmt_size * nelmts)) != 0)
        TEST_ERROR;

    /* Data should be the same. :-) */
    return true;

error:
    return false;
} /* end compare_data() */

/*-------------------------------------------------------------------------
 * Function:    compare_datasets
 *
 * Purpose:     Compare two datasets to check that they are equal
 *
 * Return:    true if datasets are equal/false if they are different
 *
 *-------------------------------------------------------------------------
 */
static int
compare_datasets(hid_t did, hid_t did2, hid_t pid, const void *wbuf)
{
    hid_t    sid = H5I_INVALID_HID, sid2 = H5I_INVALID_HID;   /* Dataspace IDs */
    hid_t    tid = H5I_INVALID_HID, tid2 = H5I_INVALID_HID;   /* Datatype IDs */
    hid_t    dcpl = H5I_INVALID_HID, dcpl2 = H5I_INVALID_HID; /* Dataset creation property list IDs */
    size_t   elmt_size;                                       /* Size of datatype */
    htri_t   is_committed;                                    /* If the datatype is committed */
    htri_t   is_committed2;                                   /* If the datatype is committed */
    int      nfilters;                                        /* Number of filters applied to dataset */
    hssize_t nelmts;                                          /* # of elements in dataspace */
    void    *rbuf  = NULL;                                    /* Buffer for reading raw data */
    void    *rbuf2 = NULL;                                    /* Buffer for reading raw data */
    H5D_space_status_t space_status;                          /* Dataset's raw dataspace status */
    H5D_space_status_t space_status2;                         /* Dataset's raw dataspace status */

    /* Check the datatypes are equal */

    /* Open the datatype for the source dataset */
    if ((tid = H5Dget_type(did)) < 0)
        TEST_ERROR;

    /* Open the datatype for the destination dataset */
    if ((tid2 = H5Dget_type(did2)) < 0)
        TEST_ERROR;

    /* Check that both datatypes are committed/not committed */
    if ((is_committed = H5Tcommitted(tid)) < 0)
        TEST_ERROR;
    if ((is_committed2 = H5Tcommitted(tid2)) < 0)
        TEST_ERROR;
    if (is_committed != is_committed2)
        TEST_ERROR;

    /* Compare the datatypes */
    if (H5Tequal(tid, tid2) != true)
        TEST_ERROR;

    /* Determine the size of datatype (for later) */
    if ((elmt_size = H5Tget_size(tid)) == 0)
        TEST_ERROR;

    /* Check the dataspaces are equal */

    /* Open the dataspace for the source dataset */
    if ((sid = H5Dget_space(did)) < 0)
        TEST_ERROR;

    /* Open the dataspace for the destination dataset */
    if ((sid2 = H5Dget_space(did2)) < 0)
        TEST_ERROR;

    /* Compare the dataspaces */
    if (H5Sextent_equal(sid, sid2) != true)
        TEST_ERROR;

    /* Determine the number of elements in dataspace (for later) */
    if ((nelmts = H5Sget_simple_extent_npoints(sid)) < 0)
        TEST_ERROR;

    /* Check the dataset creation property lists are equal */

    /* Open the dataset creation property list for the source dataset */
    if ((dcpl = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;

    /* Open the dataset creation property list for the destination dataset */
    if ((dcpl2 = H5Dget_create_plist(did2)) < 0)
        TEST_ERROR;

    /* Compare the rest of the dataset creation property lists */
    if (H5Pequal(dcpl, dcpl2) != true)
        TEST_ERROR;

    /* Get the number of filters on dataset (for later) */
    if ((nfilters = H5Pget_nfilters(dcpl)) < 0)
        TEST_ERROR;

    /* close the source dataset creation property list */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    /* close the destination dataset creation property list */
    if (H5Pclose(dcpl2) < 0)
        TEST_ERROR;

    /* Check the allocated storage is the same */

    /* Check that the space allocation status is the same */
    if (H5Dget_space_status(did, &space_status) < 0)
        TEST_ERROR;
    if (H5Dget_space_status(did2, &space_status2) < 0)
        TEST_ERROR;
    if (space_status != space_status2)
        TEST_ERROR;

    /* Check that the space used is the same */
    /* (Don't check if the dataset is filtered (i.e. compressed, etc.) and
     *  the datatype is VLEN, since the addresses for the vlen
     *  data in each dataset will (probably) be different and the storage
     *  size will thus vary)
     */
    if (!(nfilters > 0 && (H5Tdetect_class(tid, H5T_VLEN) ||
                           (H5Tdetect_class(tid, H5T_REFERENCE) && H5Tequal(tid, H5T_STD_REF))))) {
        hsize_t storage_size  = H5Dget_storage_size(did);  /* Dataset's raw data storage size */
        hsize_t storage_size2 = H5Dget_storage_size(did2); /* 2nd Dataset's raw data storage size */

        if (storage_size != storage_size2)
            TEST_ERROR;
    } /* end if */

    /* Check the raw data is equal */

    /* Allocate & initialize space for the raw data buffers */
    if ((rbuf = calloc(elmt_size, (size_t)nelmts)) == NULL)
        TEST_ERROR;
    if ((rbuf2 = calloc(elmt_size, (size_t)nelmts)) == NULL)
        TEST_ERROR;

    /* Read data from datasets */
    if (H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;
    if (H5Dread(did2, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf2) < 0)
        TEST_ERROR;

    /* Check raw data read in against data written out */
    if (wbuf) {
        if (!compare_data(did, (hid_t)0, pid, tid, (size_t)nelmts, wbuf, rbuf, did))
            TEST_ERROR;
        if (!compare_data(did2, (hid_t)0, pid, tid2, (size_t)nelmts, wbuf, rbuf2, did2))
            TEST_ERROR;
    } /* end if */
    /* Don't have written data, just compare data between the two datasets */
    else if (!compare_data(did, did2, pid, tid, (size_t)nelmts, rbuf, rbuf2, did))
        TEST_ERROR;

    /* Reclaim vlen data, if necessary */
    if (H5Tdetect_class(tid, H5T_VLEN) == true || H5Tdetect_class(tid, H5T_REFERENCE) == true)
        if (H5Treclaim(tid, sid, H5P_DEFAULT, rbuf) < 0)
            TEST_ERROR;
    if (H5Tdetect_class(tid2, H5T_VLEN) == true || H5Tdetect_class(tid2, H5T_REFERENCE) == true)
        if (H5Treclaim(tid2, sid2, H5P_DEFAULT, rbuf2) < 0)
            TEST_ERROR;

    /* Release raw data buffers */
    free(rbuf);
    rbuf = NULL;
    free(rbuf2);
    rbuf2 = NULL;

    /* close the source dataspace */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    /* close the destination dataspace */
    if (H5Sclose(sid2) < 0)
        TEST_ERROR;

    /* close the source datatype */
    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    /* close the destination datatype */
    if (H5Tclose(tid2) < 0)
        TEST_ERROR;

    /* Check if the attributes are equal */
    if (compare_std_attributes(did, did2, pid) != true)
        TEST_ERROR;

    /* Datasets should be the same. :-) */
    return true;

error:
    H5E_BEGIN_TRY
    {
        if (rbuf)
            free(rbuf);
        if (rbuf2)
            free(rbuf2);
        H5Pclose(dcpl2);
        H5Pclose(dcpl);
        H5Sclose(sid2);
        H5Sclose(sid);
        H5Tclose(tid2);
        H5Tclose(tid);
    }
    H5E_END_TRY
    return false;
} /* end compare_datasets() */

/*-------------------------------------------------------------------------
 * Function:    compare_groups
 *
 * Purpose:     Compare two groups to check that they are "equal"
 *
 * Return:    true if group are equal/false if they are different
 *
 *-------------------------------------------------------------------------
 */
static int
compare_groups(hid_t gid, hid_t gid2, hid_t pid, int depth, unsigned copy_flags)
{
    H5G_info_t ginfo;     /* Group info struct */
    H5G_info_t ginfo2;    /* Group info struct */
    hsize_t    idx;       /* Index over the objects in group */
    unsigned   cpy_flags; /* Object copy flags */

    /* Retrieve the object copy flags from the property list, if it's non-DEFAULT */
    if (pid != H5P_DEFAULT) {
        if (H5Pget_copy_object(pid, &cpy_flags) < 0)
            TEST_ERROR;
    } /* end if */
    else
        cpy_flags = 0;

    /* Check if both groups have the same # of objects */
    if (H5Gget_info(gid, &ginfo) < 0)
        TEST_ERROR;
    if (H5Gget_info(gid2, &ginfo2) < 0)
        TEST_ERROR;
    if ((cpy_flags & H5O_COPY_SHALLOW_HIERARCHY_FLAG) && depth == 0) {
        if (ginfo2.nlinks != 0)
            TEST_ERROR;
    } /* end if */
    else {
        if (ginfo.nlinks != ginfo2.nlinks)
            TEST_ERROR;
    } /* end if */

    /* Check contents of groups */
    if (ginfo2.nlinks > 0) {
        char        objname[NAME_BUF_SIZE];  /* Name of object in group */
        char        objname2[NAME_BUF_SIZE]; /* Name of object in group */
        H5L_info2_t linfo;                   /* Link information */
        H5L_info2_t linfo2;                  /* Link information */

        /* Loop over contents of groups */
        for (idx = 0; idx < ginfo.nlinks; idx++) {
            /* Check name of objects */
            if (H5Lget_name_by_idx(gid, ".", H5_INDEX_NAME, H5_ITER_INC, idx, objname, (size_t)NAME_BUF_SIZE,
                                   H5P_DEFAULT) < 0)
                TEST_ERROR;
            if (H5Lget_name_by_idx(gid2, ".", H5_INDEX_NAME, H5_ITER_INC, idx, objname2,
                                   (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0)
                TEST_ERROR;
            if (strcmp(objname, objname2) != 0)
                TEST_ERROR;

            /* Get link info */
            if (H5Lget_info2(gid, objname, &linfo, H5P_DEFAULT) < 0)
                TEST_ERROR;
            if (H5Lget_info2(gid2, objname2, &linfo2, H5P_DEFAULT) < 0)
                TEST_ERROR;
            if (linfo.type != linfo2.type)
                TEST_ERROR;

            /* Extra checks for "real" objects */
            if (linfo.type == H5L_TYPE_HARD) {
                hid_t             oid, oid2;     /* IDs of objects within group */
                H5O_info2_t       oinfo, oinfo2; /* Data model object info */
                H5O_native_info_t ninfo, ninfo2; /* Native file format object info */

                /* Compare some pieces of the object info */
                /* Get data model object info */
                if (H5Oget_info_by_name3(gid, objname, &oinfo, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
                    TEST_ERROR;
                if (H5Oget_info_by_name3(gid2, objname2, &oinfo2, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
                    TEST_ERROR;

                /* Get native object info */
                if (H5Oget_native_info_by_name(gid, objname, &ninfo, H5O_NATIVE_INFO_HDR, H5P_DEFAULT) < 0)
                    TEST_ERROR;
                if (H5Oget_native_info_by_name(gid2, objname2, &ninfo2, H5O_NATIVE_INFO_HDR, H5P_DEFAULT) < 0)
                    TEST_ERROR;

                if (oinfo.type != oinfo2.type)
                    TEST_ERROR;
                if (oinfo.rc != oinfo2.rc)
                    TEST_ERROR;

                /* If NULL messages are preserved, the number of messages
                 * should be the same in the destination.
                 * Otherwise, it should simply be true that the number
                 * of messages hasn't increased.
                 */
                if (H5O_COPY_PRESERVE_NULL_FLAG & copy_flags) {
                    if (ninfo.hdr.nmesgs != ninfo2.hdr.nmesgs)
                        ;
                    else if (ninfo.hdr.nmesgs < ninfo2.hdr.nmesgs)
                        TEST_ERROR;
                }

                /* Check for object already having been compared */
                if (token_lookup(gid, &oinfo))
                    continue;
                else
                    token_insert(&oinfo);

                /* Open objects */
                if ((oid = H5Oopen(gid, objname, H5P_DEFAULT)) < 0)
                    FAIL_STACK_ERROR;
                if ((oid2 = H5Oopen(gid2, objname2, H5P_DEFAULT)) < 0)
                    FAIL_STACK_ERROR;

                /* Compare objects within group */
                switch (oinfo.type) {
                    case H5O_TYPE_GROUP:
                        /* Compare groups */
                        if (compare_groups(oid, oid2, pid, depth - 1, copy_flags) != true)
                            TEST_ERROR;
                        break;

                    case H5O_TYPE_DATASET:
                        /* Compare datasets */
                        if (compare_datasets(oid, oid2, pid, NULL) != true)
                            TEST_ERROR;
                        break;

                    case H5O_TYPE_NAMED_DATATYPE:
                        /* Compare datatypes */
                        if (H5Tequal(oid, oid2) != true)
                            TEST_ERROR;
                        break;

                    case H5O_TYPE_MAP:
                        assert(0 && "maps not supported in native VOL connector");

                        /* clang complains about implicit fallthrough here and
                         * our usual attributes and fall-through comments don't
                         * quiet the compiler.
                         */
                        H5_CLANG_DIAG_OFF("implicit-fallthrough")
                    case H5O_TYPE_UNKNOWN:
                    case H5O_TYPE_NTYPES:
                    default:
                        assert(0 && "Unknown type of object");
                        break;
                        H5_CLANG_DIAG_ON("implicit-fallthrough")
                } /* end switch */

                /* Close objects */
                if (H5Oclose(oid) < 0)
                    TEST_ERROR;
                if (H5Oclose(oid2) < 0)
                    TEST_ERROR;
            } /* end if */
            else {
                /* Check that both links are the same size */
                if (linfo.u.val_size != linfo2.u.val_size)
                    TEST_ERROR;

                /* Compare link values */
                if (linfo.type == H5L_TYPE_SOFT ||
                    (linfo.type >= H5L_TYPE_UD_MIN && linfo.type <= H5L_TYPE_MAX)) {
                    char linkval[NAME_BUF_SIZE];  /* Link value */
                    char linkval2[NAME_BUF_SIZE]; /* Link value */

                    /* Get link values */
                    assert(linfo.u.val_size <= NAME_BUF_SIZE);
                    if (H5Lget_val(gid, objname, linkval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0)
                        TEST_ERROR;
                    if (H5Lget_val(gid2, objname2, linkval2, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0)
                        TEST_ERROR;

                    /* Compare link data */
                    if (memcmp(linkval, linkval2, linfo.u.val_size) != 0)
                        TEST_ERROR;
                } /* end else-if */
                else {
                    assert(0 && "Unknown type of link");
                } /* end else */
            }     /* end else */
        }         /* end for */
    }             /* end if */

    /* Check if the attributes are equal */
    if (compare_std_attributes(gid, gid2, pid) != true)
        TEST_ERROR;

    /* Groups should be the same. :-) */
    return true;

error:
    H5E_BEGIN_TRY
    {
    }
    H5E_END_TRY
    return false;
} /* end compare_groups() */

/*-------------------------------------------------------------------------
 * Function:    test_copy_option
 *
 * Purpose:     Create a group in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_option(hid_t fcpl_src, hid_t fcpl_dst, hid_t src_fapl, hid_t dst_fapl, unsigned flag,
                 bool crt_intermediate_grp, const char *test_desciption)
{
    hid_t    fid_src = H5I_INVALID_HID, fid_dst = H5I_INVALID_HID, fid_ext = H5I_INVALID_HID; /* File IDs */
    hid_t    sid = H5I_INVALID_HID;                                                    /* Dataspace ID */
    hid_t    did = H5I_INVALID_HID;                                                    /* Dataset ID */
    hid_t    gid = H5I_INVALID_HID, gid2 = H5I_INVALID_HID, gid_ref = H5I_INVALID_HID; /* Group IDs */
    hid_t    gid_sub = H5I_INVALID_HID, gid_sub_sub = H5I_INVALID_HID;                 /* Sub-group ID */
    hid_t    pid = H5I_INVALID_HID, lcpl_id = H5I_INVALID_HID;                         /* Property IDs */
    unsigned cpy_flags;                                                                /* Object copy flags */
    int      depth = -1;                                                               /* Copy depth */
    hsize_t  dim2d[2];
    int      buf[DIM_SIZE_1][DIM_SIZE_2];
    int      i, j;
    char     src_filename[NAME_BUF_SIZE];
    char     dst_filename[NAME_BUF_SIZE];

    TESTING(test_desciption);

    /* set initial data values */
    for (i = 0; i < DIM_SIZE_1; i++)
        for (j = 0; j < DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100 * i + j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], src_fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], dst_fapl, dst_filename, sizeof dst_filename);

    /* Reset file token checking info */
    token_reset();

    /* create source file */
    if ((fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, fcpl_src, src_fapl)) < 0)
        TEST_ERROR;

    /* create group at the SRC file */
    if ((gid = H5Gcreate2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* attach attributes to the group */
    if (test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create dataspace */
    if ((sid = H5Screate_simple(2, dim2d, NULL)) < 0)
        TEST_ERROR;

    /* add a dataset to the top group */
    if ((did = H5Dcreate2(gid, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT,
                          H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;
    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* create a sub-group */
    if ((gid_sub = H5Gcreate2(fid_src, NAME_GROUP_SUB, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* add a dataset to the sub group */
    if ((did = H5Dcreate2(gid_sub, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT,
                          H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;
    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* create sub-sub-group */
    if ((gid_sub_sub = H5Gcreate2(gid_sub, NAME_GROUP_SUB_SUB2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* add a dataset to the sub sub group */
    if ((did = H5Dcreate2(gid_sub_sub, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT,
                          H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;

    /* close dataset */
    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* close dataspace */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Gclose(gid_sub_sub) < 0)
        TEST_ERROR;

    if (H5Gclose(gid_sub) < 0)
        TEST_ERROR;

    /* close the group */
    if (H5Gclose(gid) < 0)
        FAIL_STACK_ERROR;

    if ((flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0) {
        /* Create group to copy */
        if ((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        if (H5Lcreate_soft(NAME_DATASET_SUB_SUB, fid_src, NAME_LINK_SOFT, H5P_DEFAULT, H5P_DEFAULT) < 0)
            FAIL_STACK_ERROR;
        if (H5Lcreate_soft("nowhere", fid_src, NAME_LINK_SOFT_DANGLE, H5P_DEFAULT, H5P_DEFAULT) < 0)
            FAIL_STACK_ERROR;
        if (H5Gclose(gid) < 0)
            FAIL_STACK_ERROR;

        /* Create group to compare with */
        if ((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        if (H5Lcreate_hard(fid_src, NAME_DATASET_SUB_SUB, H5L_SAME_LOC, NAME_LINK_SOFT2, H5P_DEFAULT,
                           H5P_DEFAULT) < 0)
            FAIL_STACK_ERROR;
        if (H5Lcreate_soft("nowhere", fid_src, NAME_LINK_SOFT_DANGLE2, H5P_DEFAULT, H5P_DEFAULT) < 0)
            FAIL_STACK_ERROR;
        if (H5Gclose(gid) < 0)
            FAIL_STACK_ERROR;
    } /* end if */

    if ((flag & H5O_COPY_EXPAND_EXT_LINK_FLAG) > 0) {
        char ext_filename[NAME_BUF_SIZE];

        h5_fixname(FILENAME[2], src_fapl, ext_filename, sizeof ext_filename);

        /* Create the external file and dataset */
        if ((fid_ext = H5Fcreate(ext_filename, H5F_ACC_TRUNC, fcpl_src, src_fapl)) < 0)
            TEST_ERROR;
        if ((sid = H5Screate_simple(2, dim2d, NULL)) < 0)
            TEST_ERROR;
        if ((did = H5Dcreate2(fid_ext, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0)
            TEST_ERROR;
        if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
            TEST_ERROR;
        if (H5Dclose(did) < 0)
            TEST_ERROR;
        if (H5Fclose(fid_ext) < 0)
            TEST_ERROR;

        /* Create group to copy */
        if (!(flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG)) {
            if ((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                TEST_ERROR;
        } /* end if */
        else if ((gid = H5Gopen2(fid_src, NAME_GROUP_LINK, H5P_DEFAULT)) < 0)
            TEST_ERROR;
        if (H5Lcreate_external(ext_filename, NAME_DATASET_SIMPLE, fid_src, NAME_LINK_EXTERN, H5P_DEFAULT,
                               H5P_DEFAULT) < 0)
            TEST_ERROR;
        if (H5Lcreate_external("no_file", "no_object", fid_src, NAME_LINK_EXTERN_DANGLE, H5P_DEFAULT,
                               H5P_DEFAULT) < 0)
            TEST_ERROR;
        if (H5Gclose(gid) < 0)
            TEST_ERROR;

        /* Create group to compare with */
        if (!(flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG)) {
            if ((gid = H5Gcreate2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                TEST_ERROR;
        } /* end if */
        else if ((gid = H5Gopen2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT)) < 0)
            TEST_ERROR;
        if ((did = H5Dcreate2(fid_src, NAME_LINK_EXTERN2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0)
            TEST_ERROR;
        if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
            TEST_ERROR;
        if (H5Lcreate_external("no_file", "no_object", fid_src, NAME_LINK_EXTERN_DANGLE2, H5P_DEFAULT,
                               H5P_DEFAULT) < 0)
            TEST_ERROR;
        if (H5Dclose(did) < 0)
            TEST_ERROR;
        if (H5Gclose(gid) < 0)
            TEST_ERROR;

        /* Close dataspace */
        if (H5Sclose(sid) < 0)
            TEST_ERROR;
    } /* end if */

    if ((flag & H5O_COPY_EXPAND_REFERENCE_FLAG) > 0) {
        if ((gid_ref = H5Gcreate2(fid_src, NAME_GROUP_REF, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR;

        /* create an attribute of new object references */
        if (attach_ref_attr(fid_src, gid_ref) < 0)
            TEST_ERROR;

        /* create an attribute of region references */
        if (attach_reg_ref_attr(fid_src, gid_ref) < 0)
            TEST_ERROR;

        /* create a dataset of region references */
        if (create_reg_ref_dataset(fid_src, gid_ref) < 0)
            TEST_ERROR;

        /* Close group holding reference objects */
        if (H5Gclose(gid_ref) < 0)
            TEST_ERROR;
    } /* end if */

    /* close the SRC file */
    if (H5Fclose(fid_src) < 0)
        TEST_ERROR;

    /* open the source file with read-only */
    /* (except when expanding soft links */
    if ((flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0) {
        if ((fid_src = H5Fopen(src_filename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR;
    } /* end if */
    else if ((fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, src_fapl)) < 0)
        TEST_ERROR;

    /* create destination file */
    if ((fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, fcpl_dst, dst_fapl)) < 0)
        TEST_ERROR;

    /* Create an uncopied object in destination file so that tokens in source and destination
       files aren't the same */
    if (H5Gclose(H5Gcreate2(fid_dst, NAME_GROUP_UNCOPIED, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* create property to pass copy options */
    if ((pid = H5Pcreate(H5P_OBJECT_COPY)) < 0)
        TEST_ERROR;

    /* set options for object copy */
    if (H5Pset_copy_object(pid, flag) < 0)
        TEST_ERROR;

    /* Verify object copy flags */
    if (H5Pget_copy_object(pid, &cpy_flags) < 0)
        TEST_ERROR;
    if (cpy_flags != flag)
        TEST_ERROR;

    /* copy the group from SRC to DST */
    if (crt_intermediate_grp) {
        /* Create link creation plist to pass in intermediate group creation */
        if ((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0)
            TEST_ERROR;
        if (H5Pset_create_intermediate_group(lcpl_id, true) < 0)
            TEST_ERROR;

        if (H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, "/new_g0/new_g00", pid, lcpl_id) < 0)
            TEST_ERROR;

        if (H5Pclose(lcpl_id) < 0)
            TEST_ERROR;

        /* open the group for copy */
        if ((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* open the destination group */
        if ((gid2 = H5Gopen2(fid_dst, "/new_g0/new_g00", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }
    else if (((flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0) || ((flag & H5O_COPY_EXPAND_EXT_LINK_FLAG) > 0)) {
        if (H5Ocopy(fid_src, NAME_GROUP_LINK, fid_dst, NAME_GROUP_LINK, pid, H5P_DEFAULT) < 0)
            TEST_ERROR;

        if ((flag & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0)
            /* Unlink dataset to copy from original location */
            /* (So group comparison works properly) */
            if (H5Ldelete(fid_src, NAME_DATASET_SUB_SUB, H5P_DEFAULT) < 0)
                FAIL_STACK_ERROR;

        /* open the group for copy */
        if ((gid = H5Gopen2(fid_src, NAME_GROUP_LINK2, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* open the destination group */
        if ((gid2 = H5Gopen2(fid_dst, NAME_GROUP_LINK, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }
    else if (flag & (H5O_COPY_WITHOUT_ATTR_FLAG | H5O_COPY_PRESERVE_NULL_FLAG)) {
        if (H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, pid, H5P_DEFAULT) < 0)
            TEST_ERROR;

        /* open the group for copy */
        if ((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* open the destination group */
        if ((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }
    else if (flag & H5O_COPY_SHALLOW_HIERARCHY_FLAG) {
        if (H5Ocopy(fid_src, NAME_GROUP_TOP, fid_dst, NAME_GROUP_TOP, pid, H5P_DEFAULT) < 0)
            TEST_ERROR;

        /* open the group for copy */
        if ((gid = H5Gopen2(fid_src, NAME_GROUP_TOP, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* open the destination group */
        if ((gid2 = H5Gopen2(fid_dst, NAME_GROUP_TOP, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Set the copy depth */
        depth = 1;
    }
    else if ((flag & H5O_COPY_EXPAND_REFERENCE_FLAG) > 0) {
        if (H5Ocopy(fid_src, NAME_GROUP_REF, fid_dst, NAME_GROUP_REF, pid, H5P_DEFAULT) < 0)
            TEST_ERROR;

        /* open the group for copy */
        if ((gid = H5Gopen2(fid_src, NAME_GROUP_REF, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* open the destination group */
        if ((gid2 = H5Gopen2(fid_dst, NAME_GROUP_REF, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        /* Unknown flag */
        TEST_ERROR;
    } /* end else */

    /* Check if the groups are equal */
    if (compare_groups(gid, gid2, pid, depth, flag) != true)
        TEST_ERROR;
    if (H5Gclose(gid2) < 0)
        TEST_ERROR;
    if (H5Gclose(gid) < 0)
        TEST_ERROR;

    /* close the SRC file */
    if (H5Fclose(fid_src) < 0)
        TEST_ERROR;

    /* close the DST file */
    if (H5Fclose(fid_dst) < 0)
        TEST_ERROR;

    /* close properties */
    if (H5Pclose(pid) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(lcpl_id);
        H5Pclose(pid);
        H5Sclose(sid);
        H5Dclose(did);
        H5Gclose(gid_ref);
        H5Gclose(gid_sub);
        H5Gclose(gid2);
        H5Gclose(gid);
        H5Fclose(fid_dst);
        H5Fclose(fid_src);
        H5Fclose(fid_ext);
    }
    H5E_END_TRY
    return 1;
} /* end test_copy_option */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test H5Ocopy()
 *
 *              Tests a number of cases: messages can be stored in the
 *              new or old format, messages can be shared in either,
 *              both, or neither of the source and destination files.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int      nerrors = 0;
    hid_t    fapl, fapl2;
    hid_t    fcpl_shared, ocpl;
    unsigned max_compact, min_dense;
    int      configuration; /* Configuration of tests. */
    int      ExpressMode;

    /* Setup */
    h5_reset();
    fapl = h5_fileaccess();

    ExpressMode = GetTestExpress();
    if (ExpressMode > 1)
        printf("***Express test mode on.  Some tests may be skipped\n");

    /* Copy the file access property list */
    if ((fapl2 = H5Pcopy(fapl)) < 0)
        TEST_ERROR;

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if (H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Create an FCPL with sharing enabled */
    if ((fcpl_shared = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_shared_mesg_nindexes(fcpl_shared, 1) < 0)
        TEST_ERROR;
    if (H5Pset_shared_mesg_index(fcpl_shared, 0, H5O_SHMESG_ALL_FLAG, 10) < 0)
        TEST_ERROR;

    /* Obtain the default attribute storage phase change values */
    if ((ocpl = H5Pcreate(H5P_OBJECT_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pget_attr_phase_change(ocpl, &max_compact, &min_dense) < 0)
        TEST_ERROR;
    if (H5Pclose(ocpl) < 0)
        TEST_ERROR;

    /* Test in all configurations */
    for (configuration = 0; configuration <= MAX_CONFIGURATION; configuration++) {
        hid_t src_fapl;
        hid_t dst_fapl;
        hid_t fcpl_src;
        hid_t fcpl_dst;

        /* No need to test dense attributes with old format */
        if (!(configuration & CONFIG_SRC_NEW_FORMAT) && (configuration & CONFIG_DENSE))
            continue;

        /* TODO Region references currently do not support copy from new format to old format
         * (this may be fixed once H5Sencode/decode and H5CXis fixed) */
        if ((configuration & CONFIG_SRC_NEW_FORMAT) && !(configuration & CONFIG_DST_NEW_FORMAT))
            continue;

        /* Test with and without shared messages */
        if (configuration & CONFIG_SHARE_SRC) {
            puts("\nTesting with shared src messages:");
            fcpl_src = fcpl_shared;
        }
        else {
            puts("\nTesting without shared src messages:");
            fcpl_src = H5P_DEFAULT;
        }
        if (configuration & CONFIG_SHARE_DST) {
            puts("Testing with shared dst messages:");
            fcpl_dst = fcpl_shared;
        }
        else {
            puts("Testing without shared dst messages:");
            fcpl_dst = H5P_DEFAULT;
        }

        /* Set the FAPL for the source file's type of format */
        if (configuration & CONFIG_SRC_NEW_FORMAT) {
            puts("Testing with latest format for source file:");
            src_fapl = fapl2;

            /* Test with and without dense attributes */
            if (configuration & CONFIG_DENSE) {
                puts("Testing with dense attributes:");
                num_attributes_g = max_compact + 1;
            }
            else {
                puts("Testing without dense attributes:");
                num_attributes_g = MAX(min_dense, 2) - 2;
            }
        } /* end if */
        else {
            puts("Testing with oldest file format for source file:");
            src_fapl         = fapl;
            num_attributes_g = 4;
        } /* end else */

        /* Set the FAPL for the destination file's type of format */
        if (configuration & CONFIG_DST_NEW_FORMAT) {
            puts("Testing with latest format for destination file:");
            dst_fapl = fapl2;
        } /* end if */
        else {
            puts("Testing with oldest file format for destination file:");
            dst_fapl = fapl;
        } /* end else */

        /* The tests... */
        nerrors += test_copy_option(fcpl_src, fcpl_dst, src_fapl, dst_fapl, H5O_COPY_EXPAND_REFERENCE_FLAG,
                                    false, "H5Ocopy(): expand object reference");
    } /* end for */

    /* Reset file token checking info */
    token_reset();

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    /* Results */
    if (nerrors) {
        printf("***** %d OBJECT COPY TEST%s FAILED! *****\n", nerrors, (1 == nerrors ? "" : "S"));
        exit(EXIT_FAILURE);
    } /* end if */

    puts("All object copying tests passed.");

    /* close property list.
     * NOTE: if this property list is not closed and the test is
     *          run with the split or multi driver, an interesting
     *          problem is exposed in the property list shutdown code.
     *
     *          Namely, since the split/multi driver copies property
     *          lists for internal use, there's a (high) chance that
     *          leaving the FAPL open and having the library's shutdown
     *          code close it will cause the underlying property lists
     *          to be cleaned up first, causing the actual property list
     *          close operation to fail (since it won't be able to close
     *          the already closed underlying property list).
     *
     *          The could be addressed by converting the split/multi to
     *          use non-public API routines, or putting some way into the
     *          public H5I routines to indicate ordering at shutdown.
     *
     *          For now, we just make certain to close the property list.
     *          (QAK - 2016/04/06)
     *
     */
    H5Pclose(fapl2);

    h5_cleanup(FILENAME, fapl);

    exit(EXIT_SUCCESS);

error:
    exit(EXIT_FAILURE);
} /* main */
