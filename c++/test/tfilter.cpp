/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*****************************************************************************
   FILE
   tfilter.cpp - HDF5 C++ testing various filters and their combination.

 ***************************************************************************/
#include <iostream>
using std::cerr;
using std::endl;

#include <cstdint>
#include <string>
#include "H5Cpp.h" // C++ API header file
using namespace H5;

#include "h5test.h"
#include "h5cpputil.h" // C++ utilility header file

#ifdef H5_HAVE_FILTER_SZIP
#define DSET_DIM1 100
#define DSET_DIM2 200
#endif
#define FILTER_CHUNK_DIM1 2
#define FILTER_CHUNK_DIM2 25

// will do this function later or use it as guideline - BMR - 2007/01/26
#if 0
static herr_t test_filter_internal(hid_t fid, const char *name, hid_t dcpl,
                int if_fletcher32, int corrupted, hsize_t *dset_size)
{
    cerr << "do nothing right now" << endl;
    return(0);
}
#endif

/* Temporary filter IDs used for testing */
const int H5Z_FILTER_BOGUS = 305;

static size_t filter_bogus(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                           size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_BOGUS[1] = {{
    H5Z_CLASS_T_VERS, /* H5Z_class_t version */
    H5Z_FILTER_BOGUS, /* Filter id number             */
    1, 1,             /* Encoding and decoding enabled */
    "bogus",          /* Filter name for debugging    */
    NULL,             /* The "can apply" callback     */
    NULL,             /* The "set local" callback     */
    filter_bogus,     /* The actual filter function   */
}};

/*-------------------------------------------------------------------------
 * Function:    filter_bogus
 *
 * Purpose      A bogus compression method that doesn't do anything.
 *
 * Return       Success: Data chunk size
 *
 *              Failure: 0
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
             size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;

    return nbytes;
}

/*-------------------------------------------------------------------------
 * Function:    test_null_filter
 *
 * Purpose      Test null I/O filter by itself.
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
const hsize_t chunk_size[2] = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2};

static void
test_null_filter()
{
    // Output message about test being performed
    SUBTEST("'Null' filter");
    try {
        // hsize_t  null_size;          // Size of dataset with null filter

        // Prepare dataset create property list
        DSetCreatPropList dsplist;
        dsplist.setChunk(2, chunk_size);

        if (H5Zregister(H5Z_BOGUS) < 0)
            throw Exception("test_null_filter", "H5Zregister failed");

        // Set some pretent filter
        dsplist.setFilter(H5Z_FILTER_BOGUS);

        // this function is just a stub right now; will work on it later - BMR
        // if(test_filter_internal(file,DSET_BOGUS_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&null_size)<0)
        //  throw Exception("test_null_filter", "test_filter_internal failed");

        // Close objects.
        dsplist.close();
        PASSED();
    } // end of try

    // catch all other exceptions
    catch (Exception &E) {
        issue_fail_msg("test_null_filter()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_null_filter

/*-------------------------------------------------------------------------
 * Function:    test_szip_filter
 *
 * Purpose      Test SZIP filter by itself.
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
const H5std_string DSET_SZIP_NAME("szipped dataset");

static void
test_szip_filter(H5File &file1)
{
#ifdef H5_HAVE_FILTER_SZIP
    unsigned szip_options_mask     = H5_SZIP_NN_OPTION_MASK;
    unsigned szip_pixels_per_block = 4;

    // Output message about test being performed
    SUBTEST("szip filter (with encoder)");

    if (h5_szip_can_encode() == 1) {
        char *tconv_buf = new char[1000];
        auto  points    = new int[DSET_DIM1][DSET_DIM2];
        auto  check     = new int[DSET_DIM1][DSET_DIM2];

        try {
            const hsize_t size[2] = {DSET_DIM1, DSET_DIM2};

            // Create the data space
            DataSpace space1(2, size, NULL);

            // Create a small conversion buffer to test strip mining (?)
            DSetMemXferPropList xfer;
            xfer.setBuffer(1000, tconv_buf, NULL);

            // Prepare dataset create property list
            DSetCreatPropList dsplist;
            dsplist.setChunk(2, chunk_size);

            // Set up for szip compression
            dsplist.setSzip(szip_options_mask, szip_pixels_per_block);

            // Create a dataset with szip compression
            DataSpace space2(2, size, NULL);
            DataSet   dataset(file1.createDataSet(DSET_SZIP_NAME, PredType::NATIVE_INT, space2, dsplist));

            hsize_t i, j, n;
            for (i = n = 0; i < size[0]; i++) {
                for (j = 0; j < size[1]; j++) {
                    points[i][j] = static_cast<int>(n++);
                }
            }

            // Write to the dataset then read back the values
            dataset.write(static_cast<void *>(points), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL,
                          xfer);
            dataset.read(static_cast<void *>(check), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL,
                         xfer);

            // Check that the values read are the same as the values written
            for (i = 0; i < size[0]; i++)
                for (j = 0; j < size[1]; j++) {
                    int status = check_values(i, j, points[i][j], check[i][j]);
                    if (status == -1)
                        throw Exception("test_szip_filter", "Failed in testing szip method");
                }
            dsplist.close();
            PASSED();
        } // end of try

        // catch all other exceptions
        catch (Exception &E) {
            issue_fail_msg("test_szip_filter()", __LINE__, __FILE__, E.getCDetailMsg());
        }

        delete[] tconv_buf;
        delete[] points;
        delete[] check;
    } // if szip presents
    else {
        SKIPPED();
    }

#else  /* H5_HAVE_FILTER_SZIP */
    SUBTEST("szip filter");
    SKIPPED();
    H5std_string fname = file1.getFileName();
    cerr << "    Szip filter not enabled for file '" << fname << "'" << endl;
#endif /* H5_HAVE_FILTER_SZIP */
} // test_szip_filter

/*-------------------------------------------------------------------------
 * Function:    test_append_filter
 *
 * Purpose      Smoke test for DSetCreatPropList::appendFilter and
 *              H5FilterParam::config_get_param.
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
static void
test_append_filter()
{
    SUBTEST("appendFilter (raw cd_values, shuffle)");
    try {
        DSetCreatPropList dcpl;
        dcpl.appendFilter(H5Z_FILTER_SHUFFLE, 0, 0, nullptr);
        if (dcpl.getNfilters() != 1)
            throw Exception("test_append_filter", "expected 1 filter after appendFilter");
        PASSED();
    }
    catch (Exception &E) {
        issue_fail_msg("test_append_filter()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    SUBTEST("appendFilter (string params, deflate if available)");
    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) > 0) {
        try {
            DSetCreatPropList dcpl;
            dcpl.appendFilter(H5Z_FILTER_DEFLATE, 0, H5std_string("level=6"));
            if (dcpl.getNfilters() != 1)
                throw Exception("test_append_filter", "expected 1 filter after appendFilter");
            H5std_string params = dcpl.getFilterParams(0);
            if (params.empty())
                throw Exception("test_append_filter", "getFilterParams returned empty string");
            PASSED();
        }
        catch (Exception &E) {
            issue_fail_msg("test_append_filter()", __LINE__, __FILE__, E.getCDetailMsg());
        }
    }
    else {
        SKIPPED();
    }

    SUBTEST("H5FilterParam::config_get_param (int64_t)");
    try {
        int64_t val   = 0;
        bool    found = FilterParam::config_get_param("level = 6, mode = 2", "level", val);
        if (!found)
            throw Exception("test_append_filter", "config_get_param: key not found");
        if (val != 6)
            throw Exception("test_append_filter", "config_get_param: wrong value");
        PASSED();
    }
    catch (Exception &E) {
        issue_fail_msg("test_append_filter()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    SUBTEST("H5FilterParam::config_get_param (double)");
    try {
        double val   = 0.0;
        bool   found = FilterParam::config_get_param("rate = 1.5, mode = 2", "rate", val);
        if (!found)
            throw Exception("test_append_filter", "config_get_param: key not found");
        if (val != 1.5)
            throw Exception("test_append_filter", "config_get_param: wrong value");
        /* Absent key returns false and does not throw. */
        double absent_val = 0.0;
        if (FilterParam::config_get_param("rate = 1.5", "missing", absent_val))
            throw Exception("test_append_filter", "config_get_param: expected key not found");
        PASSED();
    }
    catch (Exception &E) {
        issue_fail_msg("test_append_filter()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    SUBTEST("H5FilterParam::config_get_param (bool)");
    try {
        bool val   = false;
        bool found = FilterParam::config_get_param("lossless = true, mode = 2", "lossless", val);
        if (!found)
            throw Exception("test_append_filter", "config_get_param: key not found");
        if (!val)
            throw Exception("test_append_filter", "config_get_param: wrong value");
        PASSED();
    }
    catch (Exception &E) {
        issue_fail_msg("test_append_filter()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    SUBTEST("H5FilterParam::config_get_param (H5std_string)");
    try {
        H5std_string val;
        bool         found = FilterParam::config_get_param("name = \"zlib\", mode = 2", "name", val);
        if (!found)
            throw Exception("test_append_filter", "config_get_param: key not found");
        if (val != "zlib")
            throw Exception("test_append_filter", "config_get_param: wrong value");
        PASSED();
    }
    catch (Exception &E) {
        issue_fail_msg("test_append_filter()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    SUBTEST("H5FilterParam::config_get_param (type mismatch throws)");
    try {
        int64_t val   = 0;
        bool    threw = false;
        try {
            /* "level" holds a string value here, not an integer. */
            FilterParam::config_get_param("level = \"six\"", "level", val);
        }
        catch (LibraryIException &) {
            threw = true;
        }
        if (!threw)
            throw Exception("test_append_filter", "config_get_param: expected type-mismatch exception");
        PASSED();
    }
    catch (Exception &E) {
        issue_fail_msg("test_append_filter()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}

/*-------------------------------------------------------------------------
 * Function:    test_filters
 *
 * Purpose      Main routine for testing filters
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
const H5std_string FILE1("tfilters.h5");
extern "C" void
test_filters(void *params)
{
    (void)params;

    // Output message about test being performed
    MESSAGE(5, ("Testing Various Filters\n"));

    hid_t fapl_id;
    fapl_id = h5_fileaccess(); // in h5test.c, returns a file access template

    try {
        // Use the file access template id to create a file access prop. list
        FileAccPropList fapl(fapl_id);

        H5File file1(FILE1, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Test basic VL string datatype
        test_null_filter();
        test_szip_filter(file1);
        test_append_filter();
    }
    catch (Exception &E) {
        issue_fail_msg("test_filters()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_filters()

/*-------------------------------------------------------------------------
 * Function:    cleanup_filters
 *
 * Purpose      Cleanup temporary test files
 *
 * Return       none
 *-------------------------------------------------------------------------
 */
extern "C" void
cleanup_filters(void *params)
{
    (void)params;

    if (GetTestCleanup()) {
        HDremove(FILE1.c_str());
    }
}
