/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/***********************************************************
*
* Test program:	 tsohm
*
* Test Shared Object Header Messages
*
*************************************************************/

#include "testhdf5.h"

/* Maximum number of SOHM indexes in a file.  Should correspond
 * to H5SM_MAX_NUM_INDEXES
 */
/* JAMES: get these three from default fcpl */
#define MAX_INDEXES 6
/* Default SOHM values */
#define DEF_NUM_INDEXES 0
const unsigned def_type_flags[MAX_INDEXES] = {0,0,0,0,0,0};
#define DEF_L2B 50
#define DEF_B2L 40

/* Non-default SOHM values for testing */
#define TEST_NUM_INDEXES 4
const unsigned test_type_flags[MAX_INDEXES] =
                {H5SM_FILL_FLAG,
                 H5SM_DTYPE_FLAG | H5SM_ATTR_FLAG,
                 H5SM_SDSPACE_FLAG,
                 H5SM_PLINE_FLAG,
                 0, 0};
#define TEST_L2B 65
#define TEST_B2L 64

#define FILENAME   "tsohm.h5"


/****************************************************************
**
**  check_fcpl_values(): Helper function for test_sohm_fcpl.
**         Verifies that the *_in and *_out parameters are equal.
**
****************************************************************/
static void check_fcpl_values(hid_t fcpl_id, const unsigned nindexes_in,
               const unsigned *flags_in, size_t l2b, size_t b2l)
{
    unsigned    num_indexes;
    unsigned    index_flags[MAX_INDEXES];
    size_t      size;
    unsigned     x;
    herr_t      ret;

    /* Verify number of indexes is set to default */
    ret = H5Pget_shared_nindexes(fcpl_id, &num_indexes);
    CHECK_I(ret, "H5Pget_shared_nindexes");
    VERIFY(num_indexes, nindexes_in, "H5Pget_shared_nindexes");

    /* Verify index flags are set to default */
    ret = H5Pget_shared_mesg_types(fcpl_id, MAX_INDEXES, index_flags);
    CHECK_I(ret, "H5Pget_shared_mesg_types");

    for(x=0; x<num_indexes; ++x)
        VERIFY(index_flags[x], flags_in[x], "H5Pget_shared_indexes");
/* JAMES: can other values be undefined?
    for(x=0; x<MAX_INDEXES; ++x)
        VERIFY(index_flags[x], flags_in[x], "H5Pget_shared_indexes");
*/

    /* Check list-to-btree and btree-to-list values */
    ret = H5Pget_sohm_list_max(fcpl_id, &size);
    CHECK_I(ret, "H5Pget_sohm_list_max");
    VERIFY(size, l2b, "H5Pget_sohm_list_max");
    ret = H5Pget_sohm_btree_min(fcpl_id, &size);
    CHECK_I(ret, "H5Pget_sohm_btree_min");
    VERIFY(size, b2l, "H5Pget_sohm_btree_min");
}


/****************************************************************
**
**  test_sohm_fcpl(): Test File Creation Property Lists.
**
****************************************************************/
static void test_sohm_fcpl(void)
{
    hid_t       fid = -1;
    hid_t       fcpl_id = -1;
    hid_t       fcpl2_id = -1;
    unsigned    x;
    unsigned    bad_flags[MAX_INDEXES];
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing File Creation Properties for Shared Messages\n"));

    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl_id, DEF_NUM_INDEXES, def_type_flags, DEF_L2B, DEF_B2L);

    /* Create a file with this fcpl and make sure that all the values can be
     * retrieved.
     */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(fid, "H5Fcreate");
    fcpl2_id = H5Fget_create_plist(fid);
    CHECK_I(fcpl2_id, "H5Fcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl2_id, DEF_NUM_INDEXES, def_type_flags, DEF_L2B, DEF_B2L);

    ret = H5Pclose(fcpl2_id);
    CHECK_I(ret, "H5Pclose");

    /* Close and re-open the file.  Make sure that fcpl values are still
     * correct.
     */
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(fid, "H5Fopen");

    fcpl2_id = H5Fget_create_plist(fid);
    CHECK_I(ret, "H5Fcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl_id, DEF_NUM_INDEXES, def_type_flags, DEF_L2B, DEF_B2L);

    /* Clean up */
    ret = H5Pclose(fcpl2_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");


    /* Start over with a non-default fcpl */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK_I(fcpl_id, "H5Pcreate");
    ret = H5Pset_shared_mesgs(fcpl_id, TEST_NUM_INDEXES, test_type_flags);
    CHECK_I(ret, "H5Pset_shared_mesgs");

    ret = H5Pset_sohm_list_max(fcpl_id, TEST_L2B);
    CHECK_I(ret, "H5Pset_sohm_list_max");
    ret = H5Pset_sohm_btree_min(fcpl_id, TEST_B2L);
    CHECK_I(ret, "H5Pset_sohm_btree_min");

    check_fcpl_values(fcpl_id, TEST_NUM_INDEXES, test_type_flags, TEST_L2B, TEST_B2L);

    /* Use the fcpl to create a file and get it back again */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(fid, "H5Fcreate");
    fcpl2_id = H5Fget_create_plist(fid);
    CHECK_I(fcpl2_id, "H5Fcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl_id, TEST_NUM_INDEXES, test_type_flags, TEST_L2B, TEST_B2L);

    ret = H5Pclose(fcpl2_id);
    CHECK_I(ret, "H5Pclose");

    /* Close and re-open the file.  Make sure that fcpl values are still
     * correct.
     */
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(fid, "H5Fopen");

    fcpl2_id = H5Fget_create_plist(fid);
    CHECK_I(ret, "H5Fcreate");

    /* Verify fcpl values */
    check_fcpl_values(fcpl_id, TEST_NUM_INDEXES, test_type_flags, TEST_L2B, TEST_B2L);

    /* Clean up */
    ret = H5Pclose(fcpl2_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");

    /* Test giving bogus values to H5P* functions */
    H5E_BEGIN_TRY {
        ret = H5Pset_shared_mesgs(fcpl_id, -1, def_type_flags);
        VERIFY(ret, -1, "H5Pset_shared_mesgs");
        ret = H5Pset_shared_mesgs(fcpl_id, MAX_INDEXES + 1, test_type_flags);
        VERIFY(ret, -1, "H5Pset_shared_mesgs");

        /* Initialize bad_flags */
        for(x=0; x<MAX_INDEXES; ++x)
            bad_flags[x] = H5SM_NONE_FLAG;

        ret = H5Pset_shared_mesgs(fcpl_id, 1, bad_flags);
        VERIFY(ret, -1, "H5Pset_shared_mesgs");

        bad_flags[0]=H5SM_ALL_FLAG + 1;
        ret = H5Pset_shared_mesgs(fcpl_id, 1, bad_flags);
        VERIFY(ret, -1, "H5Pset_shared_mesgs");

        bad_flags[0]=H5SM_DTYPE_FLAG;
        bad_flags[1]=H5SM_FILL_FLAG;
        bad_flags[2]=H5SM_ATTR_FLAG | H5SM_DTYPE_FLAG;
        ret = H5Pset_shared_mesgs(fcpl_id, 3, bad_flags);
        VERIFY(ret, -1, "H5Pset_shared_mesgs");

        bad_flags[1] = H5SM_ALL_FLAG;
        ret = H5Pset_shared_mesgs(fcpl_id, 2, bad_flags);
        VERIFY(ret, -1, "H5Pset_shared_mesgs");

        bad_flags[1] = H5SM_FILL_FLAG;
        bad_flags[2] = H5SM_NONE_FLAG;
        bad_flags[3] = H5SM_ATTR_FLAG;
        ret = H5Pset_shared_mesgs(fcpl_id, 4, bad_flags);
        VERIFY(ret, -1, "H5Pset_shared_mesgs");

        /* Test list/btree cutoffs.  We can set these to any positive value,
         * but if the list max is less than the btree min we'll get an error
         * when the file is created.
         */
        ret = H5Pset_sohm_list_max(fcpl_id, 10);
        CHECK_I(ret, "H5Pset_sohm_list_max");
        ret = H5Pset_sohm_btree_min(fcpl_id, 12);
        CHECK_I(ret, "H5Pset_sohm_btree_min");
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
        VERIFY(fid, -1, "H5Fcreate");
    } H5E_END_TRY

    /* Actually, the list max can be exactly 1 greater than the
     * btree min, but no more.  Also, the errors above shouldn't
     * have corrupted the fcpl.
     */
    ret = H5Pset_sohm_list_max(fcpl_id, 10);
    CHECK_I(ret, "H5Pset_sohm_list_max");
    ret = H5Pset_sohm_btree_min(fcpl_id, 11);
    CHECK_I(ret, "H5Pset_sohm_btree_min");
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT);
    CHECK_I(fid, "H5Fcreate");

    /* Clean up */
    ret = H5Pclose(fcpl_id);
    CHECK_I(ret, "H5Pclose");
    ret = H5Fclose(fid);
    CHECK_I(ret, "H5Fclose");
}



/****************************************************************
**
**  test_sohm(): Main Shared Object Header Message testing routine.
**
****************************************************************/
void
test_sohm(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Shared Object Header Messages\n"));

    test_sohm_fcpl();		/* Test SOHMs and file creation plists */
    /* JAMES: test SOHMs and H5*copy (especially when file SOHM properties differ */
} /* test_sohm() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_sohm
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	James Laird
 *              October 9, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_sohm(void)
{
    remove(FILENAME);
}
