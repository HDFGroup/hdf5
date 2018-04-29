/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.						     *
 * Copyright by the Board of Trustees of the University of Illinois.	     *
 * All rights reserved.							     *
 *	                                                                     *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  This example illustrates how to create a compressed dataset.
 *  It is used in the HDF5 Tutorial.
 */

#include <iostream>
using std::cout;
using std::endl;

#include <string>
#include "H5Cpp.h"
using namespace H5;

const H5std_string	FILE_NAME("h5tutr_cmprss.h5");
const H5std_string	DATASET_NAME("Compressed_Data");
const int	DIM0 = 100;
const int	DIM1 = 20;

int main (void)
{
    hsize_t dims[2] = { DIM0, DIM1 };	// dataset dimensions
    hsize_t chunk_dims[2] = { 20, 20 };	// chunk dimensions
    int     i,j, buf[DIM0][DIM1];

    // Try block to detect exceptions raised by any of the calls inside it
    try
    {
	// Turn off the auto-printing when failure occurs so that we can
	// handle the errors appropriately
	Exception::dontPrint();

	// Create a new file using the default property lists. 
	H5File file(FILE_NAME, H5F_ACC_TRUNC);

	// Create the data space for the dataset.
	DataSpace *dataspace = new DataSpace(2, dims);

	// Modify dataset creation property to enable chunking
	DSetCreatPropList  *plist = new  DSetCreatPropList;
	plist->setChunk(2, chunk_dims);

	// Set ZLIB (DEFLATE) Compression using level 6.
	// To use SZIP compression comment out this line.
	plist->setDeflate(6);

	// Uncomment these lines to set SZIP Compression
	// unsigned szip_options_mask = H5_SZIP_NN_OPTION_MASK;
	// unsigned szip_pixels_per_block = 16;
	// plist->setSzip(szip_options_mask, szip_pixels_per_block);
     
	// Create the dataset.      
	DataSet *dataset = new DataSet(file.createDataSet( DATASET_NAME, 
	                        PredType::STD_I32BE, *dataspace, *plist) );

	for (i = 0; i< DIM0; i++)
	  for (j=0; j<DIM1; j++)
	      buf[i][j] = i+j;

	// Write data to dataset.
	dataset->write(buf, PredType::NATIVE_INT);

	// Close objects and file.  Either approach will close the HDF5 item.
	delete dataspace;
	delete dataset;
	delete plist;
	file.close();

	// -----------------------------------------------
	// Re-open the file and dataset, retrieve filter 
	// information for dataset and read the data back.
	// -----------------------------------------------
	
	int        rbuf[DIM0][DIM1];
	int        numfilt;
	size_t     nelmts={1}, namelen={1};
	unsigned  flags, filter_info, cd_values[1], idx;
	char       name[1];
	H5Z_filter_t filter_type;

	// Open the file and the dataset in the file.
	file.openFile(FILE_NAME, H5F_ACC_RDONLY);
	dataset = new DataSet(file.openDataSet( DATASET_NAME));

	// Get the create property list of the dataset.
	plist = new DSetCreatPropList(dataset->getCreatePlist ());

	// Get the number of filters associated with the dataset.
	numfilt = plist->getNfilters();
	cout << "Number of filters associated with dataset: " << numfilt << endl;

	for (idx=0; idx < numfilt; idx++) {
	    nelmts = 0;

	    filter_type = plist->getFilter(idx, flags, nelmts, cd_values, namelen, name , filter_info);

	    cout << "Filter Type: ";

	    switch (filter_type) {
	      case H5Z_FILTER_DEFLATE:
	           cout << "H5Z_FILTER_DEFLATE" << endl;
	           break;
	      case H5Z_FILTER_SZIP:
	           cout << "H5Z_FILTER_SZIP" << endl; 
	           break;
	      default:
	           cout << "Other filter type included." << endl;
	      }
	}

	// Read data.
	dataset->read(rbuf, PredType::NATIVE_INT);

	delete plist; 
	delete dataset;
	file.close();	// can be skipped

    }  // end of try block

    // catch failure caused by the H5File operations
    catch(FileIException error)
    {
	error.printErrorStack();
	return -1;
    }

    // catch failure caused by the DataSet operations
    catch(DataSetIException error)
    {
	error.printErrorStack();
	return -1;
    }

    // catch failure caused by the DataSpace operations
    catch(DataSpaceIException error)
    {
	error.printErrorStack();
	return -1;
    }

    return 0;  // successfully terminated
}

