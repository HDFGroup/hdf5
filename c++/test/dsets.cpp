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
* Test program:  dsets
*
* Test the dataset interface
*
*************************************************************/

#include <iostream>
#include "H5Cpp.h"
#include "h5test.h"
#include "testhdf5.h"

#ifndef H5_NO_NAMESPACE
using namespace H5;
#endif

const char *FILENAME[] = {
    "dataset",
    NULL
};

#define DSET_DEFAULT_NAME	"default"
#define DSET_CHUNKED_NAME	"chunked"
#define DSET_SIMPLE_IO_NAME	"simple_io"
#define DSET_TCONV_NAME		"tconv"
#define DSET_COMPRESS_NAME	"compressed"
#define DSET_BOGUS_NAME		"bogus"

#define H5Z_BOGUS		305


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Attempts to create a dataset.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *		Friday, January 5, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create( H5File& file)
{
   TESTING("create, open, close");

    try {
	/* Create the data space */
	hsize_t     dims[2];
	dims[0] = 256;
	dims[1] = 512;
	DataSpace space (2, dims, NULL);

	/*
	* Create a dataset using the default dataset creation properties.	
	* We're not sure what they are, so we won't check.
	*/
	DataSet *dataset = new DataSet (file.createDataSet 
		(DSET_DEFAULT_NAME, PredType::NATIVE_DOUBLE, space));

	/* Close the dataset */
	delete dataset;

	/* Add a comment to the dataset */
	file.setComment (DSET_DEFAULT_NAME, "This is a dataset");

	/*
	* Try creating a dataset that already exists.  This should fail since a
	* dataset can only be created once.  If an exception is not thrown
	* for this action by createDataSet, then display failure information
	* and jump to label error: to return.
	*/
	try {
	    dataset = new DataSet (file.createDataSet 
			(DSET_DEFAULT_NAME, PredType::NATIVE_DOUBLE, space));
	    // continuation here, that means no exception has been thrown
	    H5_FAILED();
	    cout << "    Library allowed overwrite of existing dataset." << endl;
	    goto error;
        }
        catch (FileIException E ) // catching invalid creating dataset
        {
	    // Exception is expected.  Do nothing here.
        }
	/*
	* Open the dataset we created above and then close it.  This is how
	* existing datasets are accessed.
	*/
	dataset = new DataSet (file.openDataSet (DSET_DEFAULT_NAME));
	delete dataset;
    
	/*
	* Try opening a non-existent dataset.  This should fail so if an
	* exception is not thrown for this action by openDataSet, then 
	* display failure information and jump to label error: to return.
	*/
	try {
	    dataset = new DataSet (file.openDataSet( "does_not_exist" ));
	    // continuation here, that means no exception has been thrown
	    H5_FAILED();
	    cout << "    Opened a non-existent dataset." << endl;
	    goto error;
	}
	catch (FileIException E ) // catching creating non-existent dataset
	{
	    // Exception is expected.  Do nothing here.
	}

	/*
	* Create a new dataset that uses chunked storage instead of the default
	* layout.
	*/
	DSetCreatPropList create_parms;
	hsize_t     csize[2];
	csize[0] = 5;
	csize[1] = 100;
	create_parms.setChunk( 2, csize );

	dataset = new DataSet (file.createDataSet 
		(DSET_CHUNKED_NAME, PredType::NATIVE_DOUBLE, space, create_parms));
	// Note: this one has no error message in C when failure occurs?

	/*
	* Close the chunked dataset.
	*/
	delete dataset;

	PASSED();
	return 0;
   }	// outer most try block

   // catch all dataset, file, space, plist exceptions
   catch (Exception E) { goto error; }
   
 error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	check_values
 *
 * Purpose:	Checks a read value against the written value.  If they are 
 *		different, the function will
 *		print out a message and the different values.  This function
 *		is made to reuse the code segment that is used in various
 *		places throughout test_compression and in test_simple_io.  
 *		Where the C version
 *		of this code segment "goto error," this function will
 *		return -1, so that the caller can "goto error."
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler (using C code segment for checking values)
 *		Friday, February 6, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int 
check_values (hsize_t i, hsize_t j, int apoint, int acheck)
{
    if (apoint != acheck)
    {
	H5_FAILED();
	cout << "    Read different values than written.\n" << endl;
	cout << "    At index " << (unsigned long)i << "," << 
   	(unsigned long)j << endl;
	return -1;
    }
    return 0;
} // check_values

/*-------------------------------------------------------------------------
 * Function:	test_simple_io
 *
 * Purpose:	Tests simple I/O.  That is, reading and writing a complete
 *		multi-dimensional array without data type or data space
 *		conversions, without compression, and stored contiguously.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *		Friday, January 5, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_simple_io( H5File& file)
{

    TESTING("simple I/O");

    int	points[100][200];
    int	check[100][200];
    int		i, j, n;

    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++)
    {
	for (j = 0; j < 200; j++) {
	    points[i][j] = n++;
	}
    }

    char* tconv_buf = new char [1000];
    try 
    {
	/* Create the data space */
	hsize_t	dims[2];
	dims[0] = 100;
	dims[1] = 200;
	DataSpace space (2, dims, NULL);

	/* Create a small conversion buffer to test strip mining */
	DSetMemXferPropList xfer;

	//if (H5Pset_buffer (xfer, 1000, tconv_buf, NULL)<0) goto error;
	xfer.setBuffer (1000, tconv_buf, NULL);

	/* Create the dataset */
	DataSet dataset (file.createDataSet (DSET_SIMPLE_IO_NAME, PredType::NATIVE_INT, space));

	/* Write the data to the dataset */
	dataset.write ((void*) points, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

	/* Read the dataset back */
	dataset.read ((void*) check, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

	/* Check that the values read are the same as the values written */
	for (i = 0; i < 100; i++)
	    for (j = 0; j < 200; j++)
	    {
		int status = check_values (i, j, points[i][j], check[i][j]);
		if (status == -1) goto error;
	    }

	delete [] tconv_buf;
	PASSED();
	return 0;
    }  // end try

    // catch all dataset, space, plist exceptions
    catch (Exception E) { goto error; }
   
  error:
    // cleaning up
    if (tconv_buf)
	delete [] tconv_buf;
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_tconv
 *
 * Purpose:	Test some simple data type conversion stuff.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *		Friday, January 5, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_tconv( H5File& file)
{
    // Prepare buffers for input/output
    char	*out=NULL, *in=NULL;
    out = new char [4*1000000];
    // assert (out); - should use exception handler for new - BMR
    in = new char [4*1000000];
    //assert (in);

    TESTING("data type conversion");
    
    /* Initialize the dataset */
    for (int i = 0; i < 1000000; i++) {
	out[i*4+0] = 0x11;
	out[i*4+1] = 0x22;
	out[i*4+2] = 0x33;
	out[i*4+3] = 0x44;
    }

    try
    {
	/* Create the data space */
	hsize_t	dims[1];
	dims[0] = 1000000;
	DataSpace space (1, dims, NULL);

	/* Create the data set */
	DataSet dataset (file.createDataSet (DSET_TCONV_NAME, PredType::STD_I32LE, space));

	/* Write the data to the dataset */
	dataset.write ((void*) out, PredType::STD_I32LE);

	/* Read data with byte order conversion */
	dataset.read ((void*) in, PredType::STD_I32BE);

	/* Check */
	for (int i = 0; i < 1000000; i++) {
	    if (in[4*i+0]!=out[4*i+3] ||
		in[4*i+1]!=out[4*i+2] ||
		in[4*i+2]!=out[4*i+1] ||
		in[4*i+3]!=out[4*i+0]) 
	    {
		H5_FAILED();
		cout << "    Read with byte order conversion failed." << endl;
		goto error;
	    }
	}

	delete [] out;
	delete [] in;
	cout << " PASSED" << endl;
	return 0;
    }  // end try

    // catch all dataset and space exceptions
    catch (Exception E) { goto error; }
   
  error:
	delete [] out;
	delete [] in;
	return -1;
}

/*-------------------------------------------------------------------------
 * Function:	bogus
 *
 * Purpose:	A bogus compression method that doesn't do anything.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
/*bogus(unsigned int UNUSED flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED cd_values[], size_t nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf)
BMR: removed UNUSED for now until asking Q. or R. to pass compilation*/
bogus(unsigned int flags, size_t cd_nelmts,
      const unsigned int cd_values[], size_t nbytes,
      size_t *buf_size, void **buf)
{
    return nbytes;
}


/*-------------------------------------------------------------------------
 * Function:	test_compression
 *
 * Purpose:	Tests dataset compression. If compression is requested when
 *		it hasn't been compiled into the library (such as when
 *		updating an existing compressed dataset) then data is sent to
 *		the file uncompressed but no errors are returned.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *		Friday, January 5, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
test_compression(H5File& file)
{
    const char		*not_supported;
    not_supported = "    Deflate compression is not supported.\n"
		    "    The zlib was not found when hdf5 was configured.";
    
    TESTING("compression (setup)");
    
    int		points[100][200];
    int		check[100][200];
    hsize_t	i, j, n;

    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++)
    {
	for (j = 0; j < 200; j++) {
	    points[i][j] = n++;
	}
    }
    char* tconv_buf = new char [1000];

    try 
    {
	const hsize_t	size[2] = {100, 200};
	/* Create the data space */
	DataSpace space1(2, size, NULL);

	/*
	* Create a small conversion buffer to test strip mining. We
	* might as well test all we can!
	*/
	DSetMemXferPropList xfer;

	xfer.setBuffer (1000, tconv_buf, NULL);
  
	/* Use chunked storage with compression */
	DSetCreatPropList dscreatplist;

	const hsize_t	chunk_size[2] = {2, 25};
	dscreatplist.setChunk (2, chunk_size);
	dscreatplist.setDeflate (6);
  
	/* Create the dataset */
	DataSet* dataset = new DataSet (file.createDataSet 
	    (DSET_COMPRESS_NAME, PredType::NATIVE_INT, space1, dscreatplist));
  
#if defined(H5_HAVE_COMPRESS2) && defined(H5_HAVE_ZLIB_H) && defined(H5_HAVE_LIBZ)
    PASSED();
#else
    SKIPPED();
    cout << not_supported << endl;
#endif

	/*----------------------------------------------------------------------
	* STEP 1: Read uninitialized data.  It should be zero.
	*---------------------------------------------------------------------- 
	*/
	TESTING("compression (uninitialized read)");
  
	dataset->read ((void*) check, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);
    
	for (i=0; i<size[0]; i++) {
	    for (j=0; j<size[1]; j++) {
		if (0!=check[i][j]) {
		    H5_FAILED();
		    cout << "    Read a non-zero value." << endl;
		    cout << "    At index " << (unsigned long)i << "," << 
		   (unsigned long)j << endl;
		    goto error;
		}
	    }
	}
#if defined(H5_HAVE_COMPRESS2) && defined(H5_HAVE_ZLIB_H) && defined(H5_HAVE_LIBZ)
    PASSED();
#else
    SKIPPED();
    cout << not_supported << endl;
#endif

	/*----------------------------------------------------------------------
	* STEP 2: Test compression by setting up a chunked dataset and writing
	* to it.
	*---------------------------------------------------------------------- 
	*/
	TESTING("compression (write)");
    
	for (i=n=0; i<size[0]; i++)
	{
	    for (j=0; j<size[1]; j++) 
	    {
		points[i][j] = n++;
	    }
	}

	//if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, points)<0) goto error;
	dataset->write ((void*) points, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

#if defined(H5_HAVE_COMPRESS2) && defined(H5_HAVE_ZLIB_H) && defined(H5_HAVE_LIBZ)
    PASSED();
#else
    SKIPPED();
    cout << not_supported << endl;
#endif

	/*----------------------------------------------------------------------
	* STEP 3: Try to read the data we just wrote.
	*---------------------------------------------------------------------- 
	*/
	TESTING("compression (read)");

	/* Read the dataset back */
	dataset->read ((void*)check, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

	/* Check that the values read are the same as the values written */
	for (i = 0; i < size[0]; i++)
	    for (j = 0; j < size[1]; j++)
	    {
		int status = check_values (i, j, points[i][j], check[i][j]);
		if (status == -1) goto error;
	    }

#if defined(H5_HAVE_COMPRESS2) && defined(H5_HAVE_ZLIB_H) && defined(H5_HAVE_LIBZ)
    PASSED();
#else
    SKIPPED();
    cout << not_supported << endl;
#endif

	/*----------------------------------------------------------------------
	* STEP 4: Write new data over the top of the old data.  The new data is
	* random thus not very compressible, and will cause the chunks to move
	* around as they grow.  We only change values for the left half of the
	* dataset although we rewrite the whole thing.
	*---------------------------------------------------------------------- 
	*/
	TESTING("compression (modify)");
    
	for (i=0; i<size[0]; i++)
	{
	    for (j=0; j<size[1]/2; j++) 
	    {
	    	points[i][j] = rand ();
	    }
	}
	dataset->write ((void*)points, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

	/* Read the dataset back and check it */
	dataset->read ((void*)check, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

	/* Check that the values read are the same as the values written */
	for (i = 0; i < size[0]; i++)
	    for (j = 0; j < size[1]; j++)
	    {
		int status = check_values (i, j, points[i][j], check[i][j]);
		if (status == -1) goto error;
	    }

#if defined(H5_HAVE_COMPRESS2) && defined(H5_HAVE_ZLIB_H) && defined(H5_HAVE_LIBZ)
    PASSED();
#else
    SKIPPED();
    cout << not_supported << endl;
#endif

	/*----------------------------------------------------------------------
	* STEP 5: Close the dataset and then open it and read it again.  This
	* insures that the compression message is picked up properly from the
	* object header.
	*---------------------------------------------------------------------- 
	*/
	TESTING("compression (re-open)");
    
	delete dataset;
    
	//if ((dataset = H5Dopen (file, DSET_COMPRESS_NAME))<0) goto error;
	dataset = new DataSet (file.openDataSet (DSET_COMPRESS_NAME));
	dataset->read ((void*)check, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);

	/* Check that the values read are the same as the values written */
	for (i = 0; i < size[0]; i++)
	    for (j = 0; j < size[1]; j++)
	    {
		int status = check_values (i, j, points[i][j], check[i][j]);
		if (status == -1) goto error;
	    }

#if defined(H5_HAVE_COMPRESS2) && defined(H5_HAVE_ZLIB_H) && defined(H5_HAVE_LIBZ)
    PASSED();
#else
    SKIPPED();
    cout << not_supported << endl;
#endif
    

	/*----------------------------------------------------------------------
	* STEP 6: Test partial I/O by writing to and then reading from a
	* hyperslab of the dataset.  The hyperslab does not line up on chunk
	* boundaries (we know that case already works from above tests).
	*---------------------------------------------------------------------- 
	*/
	TESTING("compression (partial I/O)");

	const hsize_t	hs_size[2] = {4, 50};
	const hssize_t	hs_offset[2] = {7, 30};
	for (i = 0; i < hs_size[0]; i++) {
	    for (j = 0; j < hs_size[1]; j++) {
		points[hs_offset[0]+i][hs_offset[1]+j] = rand ();
	    }
	}
	space1.selectHyperslab( H5S_SELECT_SET, hs_size, hs_offset );
	dataset->write ((void*)points, PredType::NATIVE_INT, space1, space1, xfer);
	dataset->read ((void*)check, PredType::NATIVE_INT, space1, space1, xfer);
    
	/* Check that the values read are the same as the values written */
	for (i=0; i<hs_size[0]; i++) {
	for (j=0; j<hs_size[1]; j++) {
	    if (points[hs_offset[0]+i][hs_offset[1]+j] !=
		check[hs_offset[0]+i][hs_offset[1]+j]) {
		H5_FAILED();
		cout << "    Read different values than written.\n" << endl;
		cout << "    At index " << (unsigned long)(hs_offset[0]+i) << 
		   "," << (unsigned long)(hs_offset[1]+j) << endl;
		
		cout << "    At original: " << (int)points[hs_offset[0]+i][hs_offset[1]+j] << endl;
		cout << "    At returned: " << (int)check[hs_offset[0]+i][hs_offset[1]+j] << endl;
		goto error;
	    }
	}
	}
#if defined(H5_HAVE_COMPRESS2) && defined(H5_HAVE_ZLIB_H) && defined(H5_HAVE_LIBZ)
    PASSED();
#else
    SKIPPED();
    cout << not_supported << endl;
#endif

	/*----------------------------------------------------------------------
	* STEP 7: Register an application-defined compression method and use it
	* to write and then read the dataset.
	*---------------------------------------------------------------------- 
	*/
	TESTING("compression (app-defined method)");

	// BMR: not sure how to handle this yet
	if (H5Zregister (H5Z_BOGUS, DSET_BOGUS_NAME, bogus)<0) goto error;
	if (H5Pset_filter (dscreatplist.getId(), H5Z_BOGUS, 0, 0, NULL)<0) goto error;
	dscreatplist.setFilter (H5Z_BOGUS, 0, 0, NULL);
	delete dataset;

	DataSpace space2 (2, size, NULL);
	dataset = new DataSet (file.createDataSet (DSET_BOGUS_NAME, PredType::NATIVE_INT, space2, dscreatplist));

	dataset->write ((void*)points, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);
	dataset->read ((void*)check, PredType::NATIVE_INT, DataSpace::ALL, DataSpace::ALL, xfer);
    
	/* Check that the values read are the same as the values written */
	for (i = 0; i < size[0]; i++)
	    for (j = 0; j < size[1]; j++)
	    {
		int status = check_values (i, j, points[i][j], check[i][j]);
		if (status == -1) goto error;
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
    catch (Exception E) { goto error; }
   
  error:
    // cleaning up
    if (tconv_buf)
	delete [] tconv_buf;
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_multiopen
 *
 * Purpose:	Tests that a bug no longer exists.  If a dataset is opened
 *		twice and one of the handles is used to extend the dataset,
 *		then the other handle should return the new size when
 *		queried.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *		Saturday, February 17, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multiopen (H5File& file)
{

    TESTING("multi-open with extending");
    try {

	// Create a dataset creation property list
	DSetCreatPropList dcpl;

	// Set chunk size to given size
	hsize_t		cur_size[1] = {10};
	dcpl.setChunk (1, cur_size);

	// Create a simple data space with unlimited size
	static hsize_t	max_size[1] = {H5S_UNLIMITED};
	DataSpace* space = new DataSpace (1, cur_size, max_size);

	// Create first dataset
	DataSet dset1 = file.createDataSet ("multiopen", PredType::NATIVE_INT, *space, dcpl);
    
	//BMR: Quincey said Rob gave a tweak to have dataset being the first
	// argument in this call but actually shouldn't be valid, so just 
	// ignore the argument dset1.  Just open the first dataset again
	// from the file to another DataSet object.
	// if ((dset2=H5Dopen (dset1, "."))<0) goto error;
	DataSet dset2 = file.openDataSet ("multiopen");

	// Relieve the dataspace
	delete space;

	// Extend the dimensionality of the first dataset
	cur_size[0] = 20;
	dset1.extend (cur_size);

	/* Get the size from the second handle */
	//if ((space = H5Dget_space (dset2))<0) goto error;
	space = new DataSpace (dset2.getSpace());

	hsize_t		tmp_size[1];
	//if (H5Sget_simple_extent_dims (space, tmp_size, NULL)<0) goto error;
	space->getSimpleExtentDims (tmp_size);
	if (cur_size[0]!=tmp_size[0]) 
	{
	    H5_FAILED();
	    cout << "    Got " << (int)tmp_size[0] << " instead of " 
		    << (int)cur_size[0] << "!" << endl;
	    delete space;
	    goto error;
	}
    
	delete space;
	PASSED();
	return 0;
    } // end try block

    // catch all dataset, file, space, and plist exceptions
    catch (Exception E)
    { goto error; }

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_types
 *
 * Purpose:	Make some datasets with various types so we can test h5ls.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:  Binh-Minh Ribler (using C version)
 *              February 17, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_types(H5File& file)
{
    size_t		i;

    TESTING("various datatypes");
    try {

	// Create a group in the file that was passed in from the caller
	Group grp = file.createGroup ("typetests");

	/* bitfield_1 */
	unsigned char	buf[32];
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
	    DataSet* dset = new DataSet(grp.createDataSet("bitfield_1", type, space));

	    // Fill buffer
	    for (i=0; i<sizeof buf; i++) 
	    	buf[i] = (unsigned char)0xff ^ (unsigned char)i;

	    // Write data from buf using all default dataspaces and property 
	    // list; if writing fails, deallocate dset and return.
	    try { dset->write (buf, type); }
	    catch(DataSetIException E) 
	    {
		delete dset;
		goto error;
	    }
	    delete dset;

	} // end try block of bitfield_1

	// catch exceptions thrown in try block of bitfield_1
	catch (Exception E) { 
	    cout << "Failure in " << E.getFuncName() << " - " 
		 << E.getDetailMsg() << endl;
	    goto error;
	}

	/* bitfield_2 */
	nelmts = sizeof(buf)/2;
	try { // bitfield_2 block
	    type.copy (PredType::STD_B16LE);
	    DataSpace space (1, &nelmts);
	    DataSet* dset = new DataSet(grp.createDataSet("bitfield_2", type, space));

	    // Fill buffer
	    for (i=0; i<sizeof(buf); i++) 
		buf[i] = (unsigned char)0xff ^ (unsigned char)i;

	    // Write data from buf using all default dataspaces and property 
	    // list; if writing fails, deallocate dset and return.
	    try { dset->write (buf, type); }
	    catch(DataSetIException E) 
	    {
	    	cout << "Failure in " << E.getFuncName() << " - " 
		 << E.getDetailMsg() << endl;
		delete dset;
		goto error;
	    }
	    delete dset;
	} // end try block of bitfield_2

	// catch exceptions thrown in try block of bitfield_2
	catch (Exception E) {
	    cout << "Failure in " << E.getFuncName() << " - " 
		 << E.getDetailMsg() << endl;
	    goto error;
	}

        /* opaque_1 */
	DataType* optype = new DataType(H5T_OPAQUE, 1);
	try { // opaque_1 block
	    nelmts = sizeof(buf);
	    DataSpace space (1, &nelmts);
	    optype->setTag ("testing 1-byte opaque type");
	    DataSet* dset = new DataSet(grp.createDataSet("opaque_1", *optype, space));

	    // Fill buffer
	    for (i=0; i<sizeof buf; i++) 
		buf[i] = (unsigned char)0xff ^ (unsigned char)i;

	    // Write data from buf using all default dataspaces and property 
	    // list; if writing fails, deallocate dset and return.
	    try { dset->write (buf, *optype); }
	    catch(DataSetIException E) 
	    {
		delete dset;
		goto error;
	    }
	    delete dset;
	    delete optype;
	} // end try block of opaque_1

	// catch exceptions thrown in try block of opaque_1
	catch (DataSetIException E) { 
	    delete optype;
	    cout << "Failure in " << E.getFuncName() << " - " 
		 << E.getDetailMsg() << endl;
	    goto error;
	}
	catch (Exception E) {
	    cout << "Failure in " << E.getFuncName() << " - " 
		 << E.getDetailMsg() << endl;
	    goto error;
	}
    
	/* opaque_2 */
	try { // block opaque_2
	    nelmts = sizeof(buf)/4;
	    DataSpace space (1, &nelmts);
	    optype = new DataType(H5T_OPAQUE, 4);
	    optype->setTag ("testing 4-byte opaque type");
	    DataSet* dset = new DataSet(grp.createDataSet("opaque_2", *optype, space));

	    // Fill buffer
	    for (i=0; i<sizeof(buf); i++) 
	        buf[i] = (unsigned char)0xff ^ (unsigned char)i;

	    // Write data from buf using all default dataspaces and property 
	    // list; if writing fails, deallocate dset and return.
	    try { dset->write (buf, *optype); }
	    catch(DataSetIException E) 
	    {
		delete dset;
		goto error;
	    }
	    delete dset;
	    delete optype;
	} //end try block of opaque_2
	catch (DataSetIException E) { 
	    delete optype;
	    cout << "Failure in " << E.getFuncName() << " - " 
		 << E.getDetailMsg() << endl;
	    goto error;
	}
	catch (Exception E) {
	    cout << "Failure in " << E.getFuncName() << " - " 
		 << E.getDetailMsg() << endl;
	    goto error;
	}
    
    PASSED();
    return 0;
    } // end top try block

    catch (Exception E) { // Group and DataType exceptions
	    cout << "Failure in " << E.getFuncName() << " - " 
		 << E.getDetailMsg() << endl;
	    goto error;
    }

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_report
 *
 * Purpose:	Prints out the number of errors for dataset tests if there  
 *		were any failures occurred.  If no failure, test_report
 *		prints out the "All dataset tests passed" message 
 *
 * Return:	if any failure has occurred:	1
 *
 *		if no failure occurs:	0
 *
 * Programmer:	Binh-Minh Ribler (using C code segment for reporting tests)
 *		Friday, February 6, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int test_report( int nerrors )
{
   if (nerrors)
   {
      nerrors = MAX(1, nerrors);
	if (1 == nerrors)
	    cout << "***** " << nerrors << " DATASET TEST" 
					<< " FAILED! *****" << endl;
	else
	    cout << "***** " << nerrors << " DATASET TESTS" 
					<< " FAILED! *****" << endl;
      return 1;
   }
   else 
   {
      cout << "All dataset tests passed." << endl;
      return 0;
   }
}

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests the dataset interface (H5D)
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(1)
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *		Friday, January 5, 2001
 *
 * Modifications:
 *	Nov 12, 01:
 *		- moved h5_cleanup to outside of try block because
 *		  dataset.h5 cannot be removed until "file" is out of
 *		  scope and dataset.h5 is closed. 
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    h5_reset(); // in h5test.c, resets the library by closing it

    hid_t	fapl_id;
    fapl_id = h5_fileaccess(); // in h5test.c, returns a file access template
    
    char        filename[1024];
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    int		nerrors=0;	// keep track of number of failures occurr
    try 
    {
	// Turn of the auto-printing when failure occurs so that we can 
	// handle the errors appropriately since sometime failures are
	// caused deliberately and expected.
	Exception::dontPrint();

	// Use the file access template id to create a file access prop.
	// list object to pass in H5File::H5File
	FileAccPropList fapl(fapl_id);

	H5File file( filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);
       
	/* Cause the library to emit initial messages */
	Group grp = file.createGroup( "emit diagnostics", 0);
	grp.setComment( ".", "Causes diagnostic messages to be emitted");

	nerrors += test_create(file)<0 	?1:0;
	nerrors += test_simple_io(file)<0	?1:0;
	nerrors += test_tconv(file)<0	?1:0;
	nerrors += test_compression(file)<0	?1:0;
	nerrors += test_multiopen (file)<0	?1:0;
	nerrors += test_types(file)<0       ?1:0;

	// increment the ref count of this property list so that the 
	// property list id won't be closed when fapl goes out of scope.
	// This is a bad hack, but I want to use existing routine h5_cleanup!
	fapl.incRefCount();
    }
    catch (Exception E) 
    {
	return( test_report( nerrors ));
    }
    /* use C test utility routine to clean up data files */
    h5_cleanup(FILENAME, fapl_id);

    /* print out dsets test results */
    return( test_report( nerrors ));
}
