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
                {H5O_MESG_FILL_FLAG,
                 H5O_MESG_DTYPE_FLAG | H5O_MESG_ATTR_FLAG,
                 H5O_MESG_SDSPACE_FLAG,
                 H5O_MESG_PLINE_FLAG,
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
    unsigned    index_flags, min_mesg_size;
    size_t      list_size, btree_size;
    unsigned     x;
    herr_t      ret;

    /* Verify number of indexes is set to default */
    ret = H5Pget_shared_mesg_nindexes(fcpl_id, &num_indexes);
    CHECK_I(ret, "H5Pget_shared_mesg_nindexes");
    VERIFY(num_indexes, nindexes_in, "H5Pget_shared_mesg_nindexes");

    /* Verify index flags are set to default */
    for(x=1; x<=num_indexes; ++x)
    {
        ret = H5Pget_shared_mesg_index(fcpl_id, x, &index_flags, &min_mesg_size);
        CHECK_I(ret, "H5Pget_shared_mesg_index");
        VERIFY(index_flags, flags_in[x-1], "H5Pget_shared_mesg_index");
        /* JAMES: check min_mesg_size here */
    }

    /* Check list-to-btree and btree-to-list values */
    ret = H5Pget_shared_mesg_phase_change(fcpl_id, &list_size, &btree_size);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
    VERIFY(list_size, l2b, "H5Pset_shared_mesg_phase_change");
    VERIFY(btree_size, b2l, "H5Pset_shared_mesg_phase_change");
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

    /* Set up index values */
    ret = H5Pset_shared_mesg_nindexes(fcpl_id, TEST_NUM_INDEXES);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    for(x=1; x<=TEST_NUM_INDEXES; ++x)
    {
        ret = H5Pset_shared_mesg_index(fcpl_id, x, test_type_flags[x-1], 15 /* JAMES */);
        CHECK_I(ret, "H5Pset_shared_mesg_index");
    }

    ret = H5Pset_shared_mesg_phase_change(fcpl_id, TEST_L2B, TEST_B2L);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");

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
        /* Trying to set index 0 or an index higher than the current number
         * of indexes should fail.
         */
        ret = H5Pset_shared_mesg_index(fcpl_id, 0, 0, 15 /* JAMES */);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");
        ret = H5Pset_shared_mesg_index(fcpl_id, MAX_INDEXES + 1, 0, 15);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");
        ret = H5Pset_shared_mesg_index(fcpl_id, TEST_NUM_INDEXES + 1, 0, 15);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");

        /* Setting an unknown flag (all flags + 1) should fail */
        ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_MESG_ALL_FLAG + 1, 15);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");

        /* Try setting two different indexes to hold fill messages */
        ret = H5Pset_shared_mesg_index(fcpl_id, 1, H5O_MESG_FILL_FLAG, 15 /* JAMES */);
        CHECK_I(ret, "H5Pset_shared_mesg_index");
        ret = H5Pset_shared_mesg_index(fcpl_id, 2, H5O_MESG_FILL_FLAG, 15 /* JAMES */);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");
        ret = H5Pset_shared_mesg_index(fcpl_id, 2, H5O_MESG_DTYPE_FLAG | H5O_MESG_FILL_FLAG, 15 /* JAMES */);
        VERIFY(ret, -1, "H5Pset_shared_mesg_index");

        /* Test list/btree cutoffs.  We can set these to any positive value,
         * but if the list max is less than the btree min we'll get an error
         * when the file is created.
         */
        ret = H5Pset_shared_mesg_phase_change(fcpl_id, 10, 12);
        VERIFY(ret, -1, "H5Pset_shared_mesg_phase_change");
    } H5E_END_TRY

    /* Actually, the list max can be exactly 1 greater than the
     * btree min, but no more.  Also, the errors above shouldn't
     * have corrupted the fcpl.
     */
    ret = H5Pset_shared_mesg_phase_change(fcpl_id, 10, 11);
    CHECK_I(ret, "H5Pset_shared_mesg_phase_change");
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
