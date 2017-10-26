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
 *  This example illustrates how to create a dataset that is a 4 x 6
 *  array. It is used in the HDF5 Tutorial.
 */

#include <iostream>
#include <string>
#include "H5Cpp.h"
using namespace H5;

const H5std_string	FILE_NAME("h5tutr_dset.h5");
const H5std_string	DATASET_NAME("dset");
const int	 NX = 4;                     // dataset dimensions
const int	 NY = 6;
const int	 RANK = 2;

int main (void)
{
    // Try block to detect exceptions raised by any of the calls inside it
    try
    {
	// Turn off the auto-printing when failure occurs so that we can
	// handle the errors appropriately
	Exception::dontPrint();

	// Create a new file using the default property lists. 
	H5File file(FILE_NAME, H5F_ACC_TRUNC);

	// Create the data space for the dataset.
	hsize_t dims[2];               // dataset dimensions
	dims[0] = NX;
	dims[1] = NY;
	DataSpace dataspace(RANK, dims);

	// Create the dataset.      
	DataSet dataset = file.createDataSet(DATASET_NAME, PredType::STD_I32BE, dataspace);

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

