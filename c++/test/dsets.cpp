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

/*****************************************************************************
   FILE
   dsets.cpp - HDF5 C++ testing the functionalities associated with the
               C dataset interface (H5D)

   EXTERNAL ROUTINES/VARIABLES:
     These routines are in the test directory of the C library:
        h5_reset() -- in h5test.c, resets the library by closing it
        h5_fileaccess() -- in h5test.c, returns a file access template

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
using std::cerr;
using std::endl;

#include <string>
#include "H5Cpp.h"        // C++ API header file
using namespace H5;

#include "h5test.h"
#include "h5cpputil.h"    // C++ utilility header file

const H5std_string    FILE1("dataset.h5");
const H5std_string    DSET_DEFAULT_NAME("default");
const H5std_string    DSET_DEFAULT_NAME_PATH("/default");
const H5std_string    DSET_CHUNKED_NAME("chunked");
const H5std_string    DSET_SIMPLE_IO_NAME("simple_io");
const H5std_string    DSET_TCONV_NAME("tconv");
const H5std_string    DSET_COMPRESS_NAME("compressed");
const H5std_string    DSET_BOGUS_NAME("bogus");

/* Temporary filter IDs used for testing */
const int H5Z_FILTER_BOGUS = 305;

static size_t filter_bogus(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
// H5_ATTR_UNUSED variables caused warning, but taking them out caused failure.


/*-------------------------------------------------------------------------
 * Function:    test_create
 *
 * Purpose      Attempts to create a dataset.
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler (using C version)
 *              Friday, January 5, 2001
 *-------------------------------------------------------------------------
 */
static herr_t test_create( H5File& file)
{
    SUBTEST("Create, open, close");

    // Setting this to NULL for cleaning up in failure situations
    DataSet *dataset = NULL;
    try {
        // Create a data space
        hsize_t     dims[2];
        dims[0] = 256;
        dims[1] = 512;
        DataSpace space (2, dims, NULL);

        // Create a dataset using the default dataset creation properties.
        // We're not sure what they are, so we won't check.
        dataset = new DataSet (file.createDataSet
                (DSET_DEFAULT_NAME, PredType::NATIVE_DOUBLE, space));


        // Add a comment to the dataset
        file.setComment (DSET_DEFAULT_NAME, "This is a dataset");

        // Close the dataset
        delete dataset;
        dataset = NULL;

        // Try creating a dataset that already exists.  This should fail since a
        // dataset can only be created once.  If an exception is not thrown for
        // this action by createDataSet, then throw an invalid action exception.
        try {
            dataset = new DataSet (file.createDataSet
                          (DSET_DEFAULT_NAME, PredType::NATIVE_DOUBLE, space));

            // continuation here, that means no exception has been thrown
            throw InvalidActionException("H5File::createDataSet", "Library allowed overwrite of existing dataset");
        }
        catch (FileIException& E)        // catching invalid creating dataset
        {} // do nothing, exception expected

        // Open the dataset we created above and then close it.  This is one
        // way to open an existing dataset for accessing.
        dataset = new DataSet (file.openDataSet (DSET_DEFAULT_NAME));

        // Get and verify the name of this dataset, using
        // H5std_string getObjName()
        H5std_string ds_name = dataset->getObjName();
        verify_val(ds_name, DSET_DEFAULT_NAME_PATH, "DataSet::getObjName", __LINE__, __FILE__);

        // Get and verify the comment from this dataset, using
        // H5std_string getComment(const H5std_string& name, <buf_size=0, by default>)
        H5std_string comment = file.getComment(DSET_DEFAULT_NAME);
        verify_val(comment, "This is a dataset", "DataSet::getComment", __LINE__, __FILE__);

        // Close the dataset when accessing is completed
        delete dataset;

        // This is another way to open an existing dataset for accessing.
        DataSet another_dataset(file.openDataSet (DSET_DEFAULT_NAME));

        // Try opening a non-existent dataset.  This should fail so if an
        // exception is not thrown for this action by openDataSet, then
        // display failure information and throw an exception.
        try {
            dataset = new DataSet (file.openDataSet( "does_not_exist" ));

            // continuation here, that means no exception has been thrown
            throw InvalidActionException("H5File::openDataSet", "Attempted to open a non-existent dataset");
        }
        catch (FileIException& E ) // catching creating non-existent dataset
        {} // do nothing, exception expected

        // Create a new dataset that uses chunked storage instead of the default
        // layout.
        DSetCreatPropList create_parms;
        hsize_t     csize[2];
        csize[0] = 5;
        csize[1] = 100;
        create_parms.setChunk( 2, csize );

        dataset = new DataSet (file.createDataSet(DSET_CHUNKED_NAME,
                               PredType::NATIVE_DOUBLE, space, create_parms));
        // Note: this one has no error message in C when failure occurs?

        // clean up and return with success
        delete dataset;

        PASSED();
        return 0;
    }        // outer most try block

    catch (InvalidActionException& E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;

        // clean up and return with failure
        if (dataset != NULL)
            delete dataset;
        return -1;
    }
    // catch all other exceptions
    catch (Exception& E)
    {
        issue_fail_msg("test_create", __LINE__, __FILE__);

        // clean up and return with failure
        if (dataset != NULL)
            delete dataset;
        return -1;
    }
}   // test_create


/*-------------------------------------------------------------------------
 * Function:    test_simple_io
 *
 * Purpose      Tests simple I/O.  That is, reading and writing a complete
 *              multi-dimensional array without data type or data space
 *              conversions, without compression, and stored contiguously.
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler (using C version)
 *              Friday, January 5, 2001
 *-------------------------------------------------------------------------
 */
static herr_t test_simple_io( H5File& file)
{

    SUBTEST("Simple I/O");

    int        points[100][200];
    int        check[100][200];
    int i, j, n;

    // Initialize the dataset
    for (i = n = 0; i < 100; i++)
    {
        for (j = 0; j < 200; j++) {
            points[i][j] = n++;
        }
    }

    char* tconv_buf = new char [1000];
    try
    {
        // Create the data space
        hsize_t dims[2];
        dims[0] = 100;
        dims[1] = 200;
        DataSpace space (2, dims, NULL);

        // Create a small conversion buffer to test strip mining
        DSetMemXferPropList xfer;

        xfer.setBuffer (1000, tconv_buf, NULL);

        // Create the dataset
        DataSet dataset (file.createDataSet (DSET_SIMPLE_IO_NAME, PredType::NATIVE_INT, space));

        // Write the data to the dataset
        dataset.write(static_cast<void*>(points), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        // Read the dataset back
        dataset.read (static_cast<void*>(check), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        // Check that the values read are the same as the values written
        for (i = 0; i < 100; i++)
            for (j = 0; j < 200; j++)
            {
                int status = check_values (i, j, points[i][j], check[i][j]);
                if (status == -1)
                    throw Exception("DataSet::read");
            }

        // clean up and return with success
        delete [] tconv_buf;
        PASSED();
        return 0;
    }  // end try

    // catch all dataset, space, plist exceptions
    catch (Exception& E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;

        // clean up and return with failure
        if (tconv_buf)
            delete [] tconv_buf;
        return -1;
    }
}   // test_simple_io


/*-------------------------------------------------------------------------
 * Function:    test_datasize
 *
 * Purpose      Tests DataSet::getInMemDataSize().  
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler
 *              Thursday, March 22, 2012
 *-------------------------------------------------------------------------
 */
static herr_t test_datasize(FileAccPropList &fapl)
{
    SUBTEST("DataSet::getInMemDataSize()");
    try
    {
        // Open FILE1.
        H5File file(FILE1, H5F_ACC_RDWR, FileCreatPropList::DEFAULT, fapl);

        // Open dataset DSET_SIMPLE_IO_NAME.
        DataSet dset = file.openDataSet (DSET_SIMPLE_IO_NAME);

        // Get the dataset's dataspace to calculate the size for verification.
        DataSpace space(dset.getSpace());

        // Get the dimension sizes.
        hsize_t dims[2];
        int n_dims = space.getSimpleExtentDims(dims);
        if (n_dims < 0)
        {
            throw Exception("test_compression", "DataSpace::getSimpleExtentDims() failed");
        }

        // Calculate the supposed size.  Size of each value is int (4), from
        // test_simple_io.
        size_t expected_size = 4 * dims[0] * dims[1];

        // getInMemDataSize() returns the in memory size of the data.
        size_t ds_size = dset.getInMemDataSize();

        // Verify the data size.
        if (ds_size != expected_size)
        {
            H5_FAILED();
            cerr << " Expected data size = " << expected_size;
            cerr << " but dset.getInMemDataSize() returned " << ds_size << endl;
            throw Exception("test_compression", "Failed in testing DataSet::getInMemDataSize()");
        }

        PASSED();
        return 0;
    }  // end try

    // catch all dataset, space, plist exceptions
    catch (Exception& E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;

        return -1;
    }
}   // test_datasize


/*-------------------------------------------------------------------------
 * Function:    test_tconv
 *
 * Purpose      Test some simple data type conversion stuff.
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler (using C version)
 *              Friday, January 5, 2001
 *-------------------------------------------------------------------------
 */
static herr_t test_tconv(H5File& file)
{
    // Prepare buffers for input/output
    char        *out=NULL, *in=NULL;
    out = new char [4*1000000];
    // assert (out); - should use exception handler for new - BMR
    in = new char [4*1000000];
    //assert (in);

    SUBTEST("Data type conversion");

    // Initialize the dataset
    for (int i = 0; i < 1000000; i++) {
        out[i*4+0] = 0x11;
        out[i*4+1] = 0x22;
        out[i*4+2] = 0x33;
        out[i*4+3] = 0x44;
    }

    try
    {
        // Create the data space
        hsize_t dims[1];
        dims[0] = 1000000;
        DataSpace space (1, dims, NULL);

        // Create the data set
        DataSet dataset (file.createDataSet (DSET_TCONV_NAME, PredType::STD_I32LE, space));

        // Write the data to the dataset
        dataset.write (static_cast<void*>(out), PredType::STD_I32LE);

        // Read data with byte order conversion
        dataset.read (static_cast<void*>(in), PredType::STD_I32BE);

        // Check
        for (int i = 0; i < 1000000; i++) {
            if (in[4*i+0]!=out[4*i+3] ||
                in[4*i+1]!=out[4*i+2] ||
                in[4*i+2]!=out[4*i+1] ||
                in[4*i+3]!=out[4*i+0])
            {
                throw Exception("DataSet::read", "Read with byte order conversion failed");
            }
        }

        // clean up and return with success
        delete [] out;
        delete [] in;
        PASSED();
        return 0;
    }  // end try

    // catch all dataset and space exceptions
    catch (Exception& E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;

        // clean up and return with failure
        delete [] out;
        delete [] in;
        return -1;
    }
}   // test_tconv

/* This message derives from H5Z */
const H5Z_class2_t H5Z_BOGUS[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version number   */
    H5Z_FILTER_BOGUS,           /* Filter id number             */
    1, 1,                       /* Encode and decode enabled    */
    "bogus",                    /* Filter name for debugging        */
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    (H5Z_func_t)filter_bogus,   /* The actual filter function        */
}};


/*-------------------------------------------------------------------------
 * Function:    bogus
 *
 * Purpose      A bogus compression method that doesn't do anything.
 *
 * Return       Success: Data chunk size
 *
 *              Failure: 0
 *
 * Programmer   Robb Matzke
 *              Tuesday, April 21, 1998
 *-------------------------------------------------------------------------
 */
static size_t filter_bogus(unsigned int flags, size_t cd_nelmts,
      const unsigned int cd_values[], size_t nbytes,
      size_t *buf_size, void **buf)
// H5_ATTR_UNUSED variables caused warning, but taking them out caused failure.
{
    return nbytes;
}


/*-------------------------------------------------------------------------
 * Function:    test_compression
 *
 * Purpose      Tests dataset compression. If compression is requested when
 *              it hasn't been compiled into the library (such as when
 *              updating an existing compressed dataset) then data is sent to
 *              the file uncompressed but no errors are returned.
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler (using C version)
 *              Friday, January 5, 2001
 *-------------------------------------------------------------------------
 */
static herr_t test_compression(H5File& file)
{
#ifndef H5_HAVE_FILTER_DEFLATE
    const char *not_supported;
    not_supported = "    Deflate compression is not enabled.";
#endif /* H5_HAVE_FILTER_DEFLATE */
    int points[100][200];
    int check[100][200];
    hsize_t        i, j, n;

    // Initialize the dataset
    for (i = n = 0; i < 100; i++)
    {
        for (j = 0; j < 200; j++) {
            points[i][j] = static_cast<int>(n++);
        }
    }

    char* tconv_buf = new char [1000];
    DataSet* dataset = NULL;
    try
    {
        const hsize_t size[2] = {100, 200};
        // Create the data space
        DataSpace space1(2, size, NULL);

        // Create a small conversion buffer to test strip mining
        DSetMemXferPropList xfer;

        xfer.setBuffer (1000, tconv_buf, NULL);

        // Use chunked storage with compression
        DSetCreatPropList dscreatplist;

        const hsize_t chunk_size[2] = {2, 25};
        dscreatplist.setChunk (2, chunk_size);
        dscreatplist.setDeflate (6);

#ifdef H5_HAVE_FILTER_DEFLATE
        SUBTEST("Compression (setup)");

        // Create the dataset
        dataset = new DataSet (file.createDataSet
            (DSET_COMPRESS_NAME, PredType::NATIVE_INT, space1, dscreatplist));

        PASSED();

        /*----------------------------------------------------------------------
        * STEP 1: Read uninitialized data.  It should be zero.
        *----------------------------------------------------------------------
        */
        SUBTEST("Compression (uninitialized read)");

        dataset->read (static_cast<void*>(check), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        for (i=0; i<size[0]; i++) {
            for (j=0; j<size[1]; j++) {
                if (0!=check[i][j]) {
                    H5_FAILED();
                    cerr << "    Read a non-zero value." << endl;
                    cerr << "    At index " << static_cast<unsigned long>(i)
                         << "," << static_cast<unsigned long>(j) << endl;
                    throw Exception("test_compression", "Failed in uninitialized read");
                }
            }
        }
        PASSED();

        /*----------------------------------------------------------------------
        * STEP 2: Test compression by setting up a chunked dataset and writing
        * to it.
        *----------------------------------------------------------------------
        */
        SUBTEST("Compression (write)");

        for (i=n=0; i<size[0]; i++)
        {
            for (j=0; j<size[1]; j++)
            {
                points[i][j] = static_cast<int>(n++);
            }
        }

        dataset->write (static_cast<void*>(points), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        PASSED();

        /*----------------------------------------------------------------------
        * STEP 3: Try to read the data we just wrote.
        *----------------------------------------------------------------------
        */
        SUBTEST("Compression (read)");

        // Read the dataset back
        dataset->read (static_cast<void*>(check), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        // Check that the values read are the same as the values written
        for (i = 0; i < size[0]; i++)
            for (j = 0; j < size[1]; j++)
            {
                int status = check_values (i, j, points[i][j], check[i][j]);
                if (status == -1)
                    throw Exception("test_compression", "Failed in read");
            }

        PASSED();

        /*----------------------------------------------------------------------
        * STEP 4: Write new data over the top of the old data.  The new data is
        * random thus not very compressible, and will cause the chunks to move
        * around as they grow.  We only change values for the left half of the
        * dataset although we rewrite the whole thing.
        *----------------------------------------------------------------------
        */
        SUBTEST("Compression (modify)");

        for (i=0; i<size[0]; i++)
        {
            for (j=0; j<size[1]/2; j++)
            {
                    points[i][j] = rand ();
            }
        }
        dataset->write (static_cast<void*>(points), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        // Read the dataset back and check it
        dataset->read (static_cast<void*>(check), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        // Check that the values read are the same as the values written
        for (i = 0; i < size[0]; i++)
            for (j = 0; j < size[1]; j++)
            {
                int status = check_values (i, j, points[i][j], check[i][j]);
                if (status == -1)
                    throw Exception("test_compression", "Failed in modify");
            }

        PASSED();

        /*----------------------------------------------------------------------
        * STEP 5: Close the dataset and then open it and read it again.  This
        * insures that the compression message is picked up properly from the
        * object header.
        *----------------------------------------------------------------------
        */
        SUBTEST("Compression (re-open)");

        // close this dataset to reuse the var
        delete dataset;

        dataset = new DataSet (file.openDataSet (DSET_COMPRESS_NAME));
        dataset->read (static_cast<void*>(check), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        // Check that the values read are the same as the values written
        for (i = 0; i < size[0]; i++)
            for (j = 0; j < size[1]; j++)
            {
                int status = check_values (i, j, points[i][j], check[i][j]);
                if (status == -1)
                    throw Exception("test_compression", "Failed in re-open");
            }

        PASSED();


        /*----------------------------------------------------------------------
        * STEP 6: Test partial I/O by writing to and then reading from a
        * hyperslab of the dataset.  The hyperslab does not line up on chunk
        * boundaries (we know that case already works from above tests).
        *----------------------------------------------------------------------
        */
        SUBTEST("Compression (partial I/O)");

        const hsize_t hs_size[2] = {4, 50};
        const hsize_t hs_offset[2] = {7, 30};
        for (i = 0; i < hs_size[0]; i++) {
            for (j = 0; j < hs_size[1]; j++) {
                points[hs_offset[0]+i][hs_offset[1]+j] = rand ();
            }
        }
        space1.selectHyperslab( H5S_SELECT_SET, hs_size, hs_offset );
        dataset->write (static_cast<void*>(points), PredType::NATIVE_INT, space1, space1, xfer);
        dataset->read (static_cast<void*>(check), PredType::NATIVE_INT, space1, space1, xfer);

        // Check that the values read are the same as the values written
        for (i=0; i<hs_size[0]; i++) {
        for (j=0; j<hs_size[1]; j++) {
            if (points[hs_offset[0]+i][hs_offset[1]+j] !=
                check[hs_offset[0]+i][hs_offset[1]+j]) {
                H5_FAILED();
                cerr << "    Read different values than written.\n" << endl;
                cerr << "    At index " << static_cast<unsigned long>((hs_offset[0]+i)) <<
                    "," << static_cast<unsigned long>((hs_offset[1]+j)) << endl;

                cerr << "    At original: " << static_cast<int>(points[hs_offset[0]+i][hs_offset[1]+j]) << endl;
                cerr << "    At returned: " << static_cast<int>(check[hs_offset[0]+i][hs_offset[1]+j]) << endl;
                throw Exception("test_compression", "Failed in partial I/O");
            }
        } // for j
        } // for i

        delete dataset;
        dataset = NULL;

        PASSED();

#else
        SUBTEST("deflate filter");
        SKIPPED();
        cerr << not_supported << endl;
#endif

        /*----------------------------------------------------------------------
        * STEP 7: Register an application-defined compression method and use it
        * to write and then read the dataset.
        *----------------------------------------------------------------------
        */
        SUBTEST("Compression (app-defined method)");

        if (H5Zregister (H5Z_BOGUS)<0)
                throw Exception("test_compression", "Failed in app-defined method");
        if (H5Pset_filter (dscreatplist.getId(), H5Z_FILTER_BOGUS, 0, 0, NULL)<0)
            throw Exception("test_compression", "Failed in app-defined method");
        dscreatplist.setFilter (H5Z_FILTER_BOGUS, 0, 0, NULL);

        DataSpace space2 (2, size, NULL);
        dataset = new DataSet (file.createDataSet (DSET_BOGUS_NAME, PredType::NATIVE_INT, space2, dscreatplist));

        dataset->write (static_cast<void*>(points), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);
        dataset->read (static_cast<void*>(check), PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

        // Check that the values read are the same as the values written
        for (i = 0; i < size[0]; i++)
            for (j = 0; j < size[1]; j++)
            {
                int status = check_values (i, j, points[i][j], check[i][j]);
                if (status == -1)
                    throw Exception("test_compression", "Failed in app-defined method");
            }

        PASSED();

        /*----------------------------------------------------------------------
        * Cleanup
        *----------------------------------------------------------------------
        */
        delete dataset;
        delete [] tconv_buf;
        return 0;
    } // end try

    // catch all dataset, file, space, and plist exceptions
    catch (Exception& E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;

        // clean up and return with failure
        if (dataset != NULL)
            delete dataset;
        if (tconv_buf)
            delete [] tconv_buf;
        return -1;
    }
}   // test_compression


/*-------------------------------------------------------------------------
 * Function:    test_nbit_methods
 *
 * Purpose      Tests setting nbit compression methods.
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler
 *              Friday, April 22, 2016
 *
 *-------------------------------------------------------------------------
 */
const H5std_string DSET_NBIT_NAME("nbit_dataset");
const hsize_t DIM1 = 2;
const hsize_t DIM2 = 5;

static herr_t test_nbit_compression(H5File& file)
{
    typedef struct {
        int i;
        char c;
        short s;
    } s1_t;

    const hsize_t size[2] = {DIM1, DIM2};
    const hsize_t chunk_size[2] = {DIM1, DIM2};
    s1_t          orig_data[DIM1][DIM2];
    s1_t          new_data[DIM1][DIM2];
    hsize_t       i, j;

    SUBTEST("N-bit compression (setup)");

    try
    {
        // Define datatypes of members of compound datatype
        IntType i_type(PredType::NATIVE_INT);
        IntType c_type(PredType::NATIVE_CHAR);
        IntType s_type(PredType::NATIVE_SHORT);

        // Create a dataset compound datatype
        CompType cmpd(sizeof(s1_t));
        cmpd.insertMember("i", HOFFSET(s1_t, i), i_type);
        cmpd.insertMember("c", HOFFSET(s1_t, c), c_type);
        cmpd.insertMember("s", HOFFSET(s1_t, s), s_type);

        // Create a memory compound datatype
        CompType mem_cmpd(sizeof(s1_t));
        mem_cmpd.insertMember("i", HOFFSET(s1_t, i), i_type);
        mem_cmpd.insertMember("c", HOFFSET(s1_t, c), c_type);
        mem_cmpd.insertMember("s", HOFFSET(s1_t, s), s_type);

        // Set order of dataset compound datatype
        //cmpd.setOrder(H5T_ORDER_BE); only for atomic type?

        // Create the data space
        DataSpace space(2, size);

        // Use nbit filter
        DSetCreatPropList dscreat;
        dscreat.setChunk(2, chunk_size);
        dscreat.setNbit();

        // Create the dataset
        DataSet dataset(file.createDataSet(DSET_NBIT_NAME, cmpd, space, dscreat));

        // Initialize data, assuming size of long long >= size of member datatypes
        for (i = 0; i < size[0]; i++)
            for (j = 0; j < size[1]; j++)
            {
                orig_data[i][j].i = static_cast<int>(i * j);
                orig_data[i][j].c = static_cast<char>('a' + i);
                orig_data[i][j].s = static_cast<short>(i + j);

                // Some even-numbered integer values are negative
                if ((i*size[1]+j+1)%2 == 0) {
                    orig_data[i][j].i = -orig_data[i][j].i;
                    orig_data[i][j].s = static_cast<short>(-orig_data[i][j].s);
                }
            }

        // Write to the dataset
        dataset.write(static_cast<void*>(orig_data), mem_cmpd);

        // Read the dataset back */
        dataset.read(static_cast<void*>(new_data), mem_cmpd);

        // Check that the values read are the same as the values written.
        for (i = 0; i < size[0]; i++)
            for (j = 0; j < size[1]; j++)
            {
                if((new_data[i][j].i != orig_data[i][j].i) ||
                   (new_data[i][j].c != orig_data[i][j].c) ||
                   (new_data[i][j].s != orig_data[i][j].s))
                {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    At index %lu,%lu\n", static_cast<unsigned long>(i), static_cast<unsigned long>(j));
                }
            }

        PASSED();
        return 0;
    } // end try block

    // catch all dataset, file, space, and plist exceptions
    catch (Exception& E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;

        return -1;
    }
} // test_nbit_compression


/*-------------------------------------------------------------------------
 * Function:    test_multiopen
 *
 * Purpose      Tests that a bug no longer exists.  If a dataset is opened
 *              twice and one of the handles is used to extend the dataset,
 *              then the other handle should return the new size when
 *              queried.
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler (using C version)
 *              Saturday, February 17, 2001
 *-------------------------------------------------------------------------
 */
static herr_t test_multiopen (H5File& file)
{

    SUBTEST("Multi-open with extending");

    DataSpace* space = NULL;
    try {

        // Create a dataset creation property list
        DSetCreatPropList dcpl;

        // Set chunk size to given size
        hsize_t cur_size[1] = {10};
        dcpl.setChunk (1, cur_size);

        // Create a simple data space with unlimited size
        hsize_t max_size[1] = {H5S_UNLIMITED};
        space = new DataSpace (1, cur_size, max_size);

        // Create first dataset
        DataSet dset1 = file.createDataSet ("multiopen", PredType::NATIVE_INT, *space, dcpl);

        // Open again the first dataset from the file to another DataSet object.
        DataSet dset2 = file.openDataSet ("multiopen");

        // Relieve the dataspace
        delete space;
        space = NULL;

        // Extend the dimensionality of the first dataset
        cur_size[0] = 20;
        dset1.extend (cur_size);

        // Get the size from the second handle
        space = new DataSpace (dset2.getSpace());

        hsize_t tmp_size[1];
        space->getSimpleExtentDims (tmp_size);
        if (cur_size[0]!=tmp_size[0])
        {
            cerr << "    Got " << static_cast<int>(tmp_size[0])
                 << " instead of " << static_cast<int>(cur_size[0]) 
                 << "!" << endl;
            throw Exception("test_multiopen", "Failed in multi-open with extending");
        }

        // clean up and return with success
        delete space;
        PASSED();
        return 0;
    } // end try block

    // catch all dataset, file, space, and plist exceptions
    catch (Exception& E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;

        // clean up and return with failure
        if (space != NULL)
            delete space;
        return -1;
    }
}   // test_multiopen


/*-------------------------------------------------------------------------
 * Function:    test_types
 *
 * Purpose      Test various types - should be moved to dtypes.cpp
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler (using C version)
 *              February 17, 2001
 *-------------------------------------------------------------------------
 */
static herr_t test_types(H5File& file)
{
    SUBTEST("Various datatypes");

    size_t i;
    DataSet* dset = NULL;
    try {

        // Create a group in the file that was passed in from the caller
        Group grp = file.createGroup ("typetests");

        /* bitfield_1 */
        unsigned char buf[32];
        hsize_t nelmts = sizeof(buf);
        DataType type;
        try { // block of bitfield_1
            // test copying a predefined type
            type.copy (PredType::STD_B8LE);

            // Test copying a user-defined type using DataType::copy
            DataType copied_type;
            copied_type.copy(type);

            // Test copying a user-defined type using DataType::operator=
            DataType another_copied_type;
            another_copied_type = type;

            // Test copying a user-defined int type using DataType::operator=
            IntType orig_int(PredType::STD_B8LE);
            DataType generic_type;
            generic_type = orig_int;

            // Test copying an integer predefined type
            IntType new_int_type(PredType::STD_B8LE);

            // Test copying an int predefined type using DataType::operator=
            IntType another_int_type;
            another_int_type = new_int_type;

            DataSpace space (1, &nelmts);
            dset = new DataSet(grp.createDataSet("bitfield_1", type, space));

            // Fill buffer
            for (i=0; i<sizeof buf; i++)
                    buf[i] = static_cast<unsigned char>(0xff) ^ static_cast<unsigned char>(i);

            // Write data from buf using all default dataspaces and property list
            dset->write (buf, type);

            // no failure in bitfield_1, close this dataset
            delete dset;
        } // end try block of bitfield_1

        // catch exceptions thrown in try block of bitfield_1
        catch (Exception& E)
        {
            cerr << " FAILED" << endl;
            cerr << "    <<<  " << "bitfield_1: " << E.getFuncName()
                 << " - " << E.getDetailMsg() << "  >>>" << endl << endl;
            if (dset != NULL)
                delete dset;
            return -1;
        }

        /* bitfield_2 */
        nelmts = sizeof(buf)/2;
        try { // bitfield_2 block
            type.copy (PredType::STD_B16LE);
            DataSpace space (1, &nelmts);
            dset = new DataSet(grp.createDataSet("bitfield_2", type, space));

            // Fill buffer
            for (i=0; i<sizeof(buf); i++)
                buf[i] = static_cast<unsigned char>(0xff) ^ static_cast<unsigned char>(i);

            // Write data from buf using all default dataspaces and property
            // list; if writing fails, deallocate dset and return.
            dset->write (buf, type);

            // no failure in bitfield_2, close this dataset and reset for
            // variable reuse
            delete dset;
            dset = NULL;
        } // end try block of bitfield_2

        // catch exceptions thrown in try block of bitfield_2
        catch (Exception& E)
        {
            cerr << " FAILED" << endl;
            cerr << "    <<<  " << "bitfield_2: " << E.getFuncName()
                 << " - " << E.getDetailMsg() << "  >>>" << endl << endl;
            if (dset != NULL)
                delete dset;
            throw E; // propagate the exception
        }

        /* opaque_1 */
        DataType* optype = NULL;
        try { // opaque_1 block
            optype = new DataType(H5T_OPAQUE, 1);
            nelmts = sizeof(buf);
            DataSpace space (1, &nelmts);
            optype->setTag ("testing 1-byte opaque type");
            dset = new DataSet(grp.createDataSet("opaque_1", *optype, space));

            // Fill buffer
            for (i=0; i<sizeof buf; i++)
                buf[i] = static_cast<unsigned char>(0xff) ^ static_cast<unsigned char>(i);

            // Write data from buf using all default dataspaces and property
            // list; if writing fails, deallocate dset and return.
            dset->write (buf, *optype);

            // no failure in opaque_1
            delete dset; dset = NULL;
            delete optype; optype = NULL;
        } // end try block of opaque_1

        // catch exceptions thrown in try block of opaque_1
        catch (Exception& E)
        {
            cerr << " FAILED" << endl;
            cerr << "    <<<  " << "opaque_1: " << E.getFuncName()
                 << " - " << E.getDetailMsg() << "  >>>" << endl << endl;
            if (dset != NULL)
                delete dset;
            if (optype != NULL)
                delete optype;
            throw E; // propagate the exception
        }

        /* opaque_2 */
        try { // block opaque_2
            nelmts = sizeof(buf)/4;
            DataSpace space (1, &nelmts);
            optype = new DataType(H5T_OPAQUE, 4);
            optype->setTag ("testing 4-byte opaque type");
            dset = new DataSet(grp.createDataSet("opaque_2", *optype, space));

            // Fill buffer
            for (i=0; i<sizeof(buf); i++)
                buf[i] = static_cast<unsigned char>(0xff) ^ static_cast<unsigned char>(i);

            // Write data from buf using all default dataspaces and property
            // list; if writing fails, deallocate dset and return.
            dset->write (buf, *optype);

            // no failure in opaque_1
            delete dset; dset = NULL;
            delete optype; optype = NULL;
        } //end try block of opaque_2

        // catch exceptions thrown in try block of opaque_2
        catch (Exception& E)
        {
            cerr << " FAILED" << endl;
            cerr << "    <<<  " << "opaque_2: " << E.getFuncName()
                 << " - " << E.getDetailMsg() << "  >>>" << endl << endl;
            if (dset != NULL)
                delete dset;
            if (optype != NULL)
                delete optype;
            throw E; // propagate the exception
        }

        PASSED();
        return 0;
    } // end top try block

    catch (Exception& E)
    {
        return -1;
    }
}   // test_types


/*-------------------------------------------------------------------------
 * Function:    test_getObjinfo
 *
 * Purpose      Tests getObjinfo()
 *
 * Return       Success: 0
 *              Failure: -1
 *
 * July, 2018
 *-------------------------------------------------------------------------
 */
static herr_t test_getinfo(H5File& file)
{
    SUBTEST("Getting object information");

    try {
        // Create a data space
        hsize_t     dims[2];
        dims[0] = 256;
        dims[1] = 512;
        DataSpace space (2, dims, NULL);

        // Create a dataset using the default dataset creation properties.
        // We're not sure what they are, so we won't check.
        DataSet dataset(file.openDataSet(DSET_CHUNKED_NAME));

        // Get dataset header info
        H5O_info_t oinfo;
        HDmemset(&oinfo, 0, sizeof(oinfo));
        dataset.getObjinfo(oinfo, H5O_INFO_HDR);
        verify_val(oinfo.hdr.nchunks, 1, "DataSet::getObjinfo", __LINE__, __FILE__);
        dataset.close();

        // Open the dataset we created above and then close it.  This is one
        // way to open an existing dataset for accessing.
        dataset = file.openDataSet(DSET_DEFAULT_NAME);
        HDmemset(&oinfo, 0, sizeof(oinfo));
        dataset.getObjinfo(oinfo, H5O_INFO_ALL);
        verify_val(oinfo.hdr.nchunks, 1, "DataSet::getObjinfo", __LINE__, __FILE__);
        dataset.close();

        PASSED();
        return 0;
    }   // outer most try block

    catch (InvalidActionException& E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;
        return -1;
    }
    // catch all other exceptions
    catch (Exception& E)
    {
        issue_fail_msg("test_getinfo", __LINE__, __FILE__);
        return -1;
    }
}   // test_getinfo


/*-------------------------------------------------------------------------
 * Function:    test_chunk_cache
 *
 * Purpose      Tests setting rdcc info on a DAPL, and interaction
 *              with the corresponding properties in the file structure.
 *
 * Return       Success:        0
 *              Failure:        number of errors
 *
 * July 2018
 *-------------------------------------------------------------------------
 */
const int RANK1 = 1;
const H5std_string FILE_ACCPLIST("test_accplist.h5");

static herr_t test_chunk_cache(FileAccPropList fapl)
{
    SUBTEST("DSetAccPropList::set/getChunkCache");

    try {
        // Create a default dataset access and file access property lists
        FileAccPropList fapl_def;
        DSetAccPropList dapl;

        // Verify that chunk cache parameters are the same
        int mdc_nelmts = 0;
        size_t nslots_1 = 0, nslots_4 = 0, nbytes_1 = 0, nbytes_4 = 0;
        double w0_1 = 0.0F, w0_4 = 0.0F;
        fapl_def.getCache(mdc_nelmts, nslots_1, nbytes_1, w0_1);
        dapl.getChunkCache(nslots_4, nbytes_4, w0_4);
        verify_val(nslots_1, nslots_4, "DSetAccPropList::getChunkCache",  __LINE__, __FILE__);
        verify_val(nbytes_1, nbytes_4, "DSetAccPropList::getChunkCache",  __LINE__, __FILE__);
        verify_val(w0_1, w0_4, "DSetAccPropList::getChunkCache",  __LINE__, __FILE__);

        // Set a link access property on dapl to verify property list inheritance
        dapl.setNumLinks((size_t)134);
        size_t nlinks = dapl.getNumLinks();
        verify_val(nlinks, (size_t)134, "DSetAccPropList::getNumLinks", __LINE__, __FILE__);

        // Make a copy of the external fapl
        FileAccPropList fapl_local(fapl);

        // Set new rdcc settings on fapl local
        size_t nslots_2 = nslots_1 * 2;
        size_t nbytes_2 = nbytes_1 * 2;
        double w0_2 = w0_1 / (double)2.0F;
        fapl_local.getCache(mdc_nelmts, nslots_2, nbytes_2, w0_2);

        // Create a new file using default fcpl and the passed-in fapl
        H5File file(FILE_ACCPLIST, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl_local);

        // Create dataset creation property list
        DSetCreatPropList dcpl;

        // Set chunk dimensions
        hsize_t cdims[RANK1];
        cdims[0] = 10;
        dcpl.setChunk(RANK1, cdims);

        // Create memory space
        hsize_t mdims[RANK1];
        mdims[0] = 10;
        DataSpace mspace(RANK1, mdims);

        // Create a dataset using that dataset creation properties
        DataSet dataset(file.createDataSet(DSET_CHUNKED_NAME, PredType::NATIVE_INT, mspace, dcpl, dapl));

        // Get the dataset access property list
        DSetAccPropList dapl2 = dataset.getAccessPlist();

        // Retrieve and verify the raw data chunk cache parameters
        nslots_4 = nbytes_4 = 0;
        w0_4 = 0.0F;
        dapl2.getChunkCache(nslots_4, nbytes_4, w0_4);
        verify_val(nslots_2, nslots_4, "DSetCreatPropList::getChunkCache", __LINE__, __FILE__);
        verify_val(nbytes_2, nbytes_4, "DSetCreatPropList::getChunkCache", __LINE__, __FILE__);
        verify_val(H5_DBL_ABS_EQUAL(w0_2, w0_4), 1, "DSetCreatPropList::getChunkCache", __LINE__, __FILE__);


        // Set new values on original dapl
        size_t nslots_3 = nslots_1 * 2;
        size_t nbytes_3 = H5D_CHUNK_CACHE_NBYTES_DEFAULT;
        double w0_3 = w0_2 / 2;
        dapl.getChunkCache(nslots_3, nbytes_3, w0_3);

        // Close dataset
        dataset.close();

        // Reopen dataset
        DataSet dataset2(file.openDataSet(DSET_CHUNKED_NAME, dapl));

        // Get the dataset access property list
        DSetAccPropList dapl3 = dataset2.getAccessPlist();

        // Retrieve and verify the raw data chunk cache parameters
        dapl3.getChunkCache(nslots_4, nbytes_4, w0_4);
        verify_val(nslots_3, nslots_4, "DSetCreatPropList::getLayout", __LINE__, __FILE__);
        verify_val(nbytes_3, nbytes_4, "DSetCreatPropList::getLayout", __LINE__, __FILE__);
        verify_val(H5_DBL_ABS_EQUAL(w0_3, w0_4), 1, "DSetCreatPropList::getLayout", __LINE__, __FILE__);


        PASSED();
        return 0;
    } // end top try block

    catch (Exception& E)
    {
        return -1;
    }
} // test_chunk_cache


/*-------------------------------------------------------------------------
 * Function:    test_virtual
 *
 * Purpose      Tests fixed, unlimited, and printf selections in the same
 *              VDS
 *
 * Return       Success:        0
 *              Failure:        number of errors
 *
 * Programmer   Binh-Minh Ribler
 *              Friday, March 10, 2017
 *
 *-------------------------------------------------------------------------
 */
const int RANK = 2;

static herr_t test_virtual()
{
    SUBTEST("DSetCreatPropList::setVirtual");

    try {
        // Create DCPLs
        DSetCreatPropList dcpl;
        DSetCreatPropList srcdcpl;

        // Set fill value
        char *fill = NULL;
        dcpl.setFillValue(PredType::NATIVE_INT, &fill);

        // Set chunk dimensions
        hsize_t cdims[RANK];
        cdims[0] = 2;
        cdims[1] = 2;
        srcdcpl.setChunk(RANK, cdims);

        // Create memory space
        hsize_t mdims[RANK];
        mdims[0] = 10;
        mdims[1] = 10;
        DataSpace memspace(RANK, mdims);

        // Get the current layout, should be default, H5D_CONTIGUOUS
        H5D_layout_t layout = dcpl.getLayout();
        verify_val(layout, H5D_CONTIGUOUS, "DSetCreatPropList::getLayout", __LINE__, __FILE__);

        // Create fixed mapping
        hsize_t dims[RANK];
        dims[0] = 6;
        dims[1] = 6;
        DataSpace vspace(RANK, dims, mdims);

        hsize_t start[RANK];       // Hyperslab start
        hsize_t count[RANK];       // Hyperslab count
        start[0] = start[1] = 3;
        count[0] = count[1] = 3;
        vspace.selectHyperslab(H5S_SELECT_SET, count, start);

        DataSpace srcspace(RANK, count);

        H5std_string src_file = "src_file_map.h5";
        H5std_string src_dset = "src_dset_fixed";
        dcpl.setVirtual(vspace, src_file, src_dset, srcspace);

        // Get and verify the new layout
        layout = dcpl.getLayout();
        verify_val(layout, H5D_VIRTUAL, "DSetCreatPropList::getLayout", __LINE__, __FILE__);

        PASSED();
        return 0;
    } // end top try block

    catch (Exception& E)
    {
        return -1;
    }
} // test_virtual


/*-------------------------------------------------------------------------
 * Function:    test_dset
 *
 * Purpose      Tests the dataset interface (H5D)
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler (using C version)
 *              Friday, January 5, 2001
 *
 * Modifications:
 *        Nov 12, 01:
 *              - moved h5_cleanup to outside of try block because
 *                dataset.h5 cannot be removed until "file" is out of
 *                scope and dataset.h5 is closed.
 *        Feb 20, 05:
 *              - cleanup_dsets took care of the cleanup now.
 *
 *-------------------------------------------------------------------------
 */
extern "C"
void test_dset()
{
    hid_t fapl_id;
    fapl_id = h5_fileaccess(); // in h5test.c, returns a file access template
    int nerrors=0; // keep track of number of failures occur

    try
    {
        // Use the file access template id to create a file access prop.
        // list object to pass in H5File::H5File
        FileAccPropList fapl(fapl_id);

        H5File file(FILE1, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Cause the library to emit initial messages
        Group grp = file.createGroup( "emit diagnostics", 0);
        grp.setComment("Causes diagnostic messages to be emitted");

        nerrors += test_create(file) < 0 ? 1:0;
        nerrors += test_simple_io(file) < 0 ? 1:0;
        nerrors += test_getinfo(file) < 0 ? 1:0;
        nerrors += test_tconv(file) < 0 ? 1:0;
        nerrors += test_compression(file) < 0 ? 1:0;
        nerrors += test_nbit_compression(file) < 0 ? 1:0;
        nerrors += test_multiopen (file) < 0 ? 1:0;
        nerrors += test_types(file) < 0 ? 1:0;
        nerrors += test_virtual() < 0 ? 1:0;
        nerrors += test_chunk_cache(fapl) < 0 ? 1:0;

        // Close group "emit diagnostics".
        grp.close();

        // Close the file before testing data size.
        file.close();

        nerrors += test_datasize(fapl) <0 ? 1:0;
    }
    catch (Exception& E)
    {
        test_report(nerrors, H5std_string(" Dataset"));
    }

    // Clean up data file
    cleanup_dsets();
}   // test_dset


/*-------------------------------------------------------------------------
 * Function:    cleanup_dsets
 *
 * Purpose      Cleanup temporary test files
 *
 * Return       None
 *
 * Programmer   (use C version)
 *-------------------------------------------------------------------------
 */
extern "C"
void cleanup_dsets()
{
    HDremove(FILE1.c_str());
} // cleanup_dsets

