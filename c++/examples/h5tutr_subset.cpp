/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.						     *
 * Copyright by the Board of Trustees of the University of Illinois.	     *
 * All rights reserved.							     *
 *	                                                                     *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have	     *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  This example illustrates how to read/write a subset of data (a slab)
 *  from/to a dataset in an HDF5 file. It is used in the HDF5 Tutorial.
 */

#include <iostream>
using std::cout;
using std::endl;

#include <string>
#include "H5Cpp.h"
using namespace H5;

const H5std_string	FILE_NAME("h5tutr_subset.h5");
const H5std_string	DATASET_NAME("IntArray");

const int     RANK = 2;
const int     DIM0_SUB = 3;	// subset dimensions
const int     DIM1_SUB = 4;
const int     DIM0 = 8;		// size of dataset
const int     DIM1 = 10;

int main (void)
{
    int     i,j;
    int     data[DIM0][DIM1], sdata[DIM0_SUB][DIM1_SUB], rdata[DIM0][DIM1];

    // Try block to detect exceptions raised by any of the calls inside it
    try
    {
	// Turn off the auto-printing when failure occurs so that we can
	// handle the errors appropriately
	Exception::dontPrint();

	// ---------------------------------------------------
	// Create a new file using the default property lists. 
	// Then create a dataset and write data to it. 
	// Close the file and dataset.
	// ---------------------------------------------------
      
	H5File file(FILE_NAME, H5F_ACC_TRUNC);

	hsize_t dims[2];              
	dims[0] = DIM0;
	dims[1] = DIM1;
	DataSpace dataspace = DataSpace (RANK, dims);

	DataSet dataset(file.createDataSet( DATASET_NAME, 
	                                 PredType::STD_I32BE, dataspace) );


	for (j = 0; j < DIM0; j++) {
	     for (i = 0; i < DIM1; i++)
	         if (i< (DIM1/2))
	            data[j][i] = 1;
	         else
	            data[j][i] = 2;
	      }

	dataset.write(data, PredType::NATIVE_INT);

	cout << endl << "Data Written to File:" << endl;
	for (j = 0; j < DIM0; j++) {
	    for (i = 0; i < DIM1; i++)
	       cout << " " <<  data[j][i];
	    cout << endl;
	}

	dataspace.close();
	dataset.close();
	file.close();

	// ---------------------------------------------------
	// Reopen the file and dataset and write a subset of
	// values to the dataset.
	// ---------------------------------------------------

	hsize_t offset[2], count[2], stride[2], block[2];
	hsize_t dimsm[2];

	file.openFile(FILE_NAME, H5F_ACC_RDWR);
	dataset = file.openDataSet(DATASET_NAME);

	// Specify size and shape of subset to write. 

	offset[0] = 1;
	offset[1] = 2;

	count[0]  = DIM0_SUB;
	count[1]  = DIM1_SUB;

	stride[0] = 1;
	stride[1] = 1;

	block[0] = 1;
	block[1] = 1;
  
	// Define Memory Dataspace. Get file dataspace and select
	// a subset from the file dataspace.

	dimsm[0] = DIM0_SUB;
	dimsm[1] = DIM1_SUB;

	DataSpace memspace(RANK, dimsm, NULL);

	dataspace = dataset.getSpace();
	dataspace.selectHyperslab(H5S_SELECT_SET, count, offset, stride, block); 

	// Write a subset of data to the dataset, then read the
	// entire dataset back from the file.

	cout << endl << "Write subset to file specifying: " << endl;
	cout << "  offset=1x2 stride=1x1 count=3x4 block=1x1" << endl;
	for (j = 0; j < DIM0_SUB; j++) {
	    for (i = 0; i < DIM1_SUB; i++)
	       sdata[j][i] = 5;
	} 
	
	dataset.write(sdata, PredType::NATIVE_INT, memspace, dataspace);
	dataset.read(rdata, PredType::NATIVE_INT);

 
	cout << endl << "Data in File after Subset is Written:" << endl;
	for (i = 0; i < DIM0; i++) {
	    for (j = 0; j < DIM1; j++) 
	       cout << " " <<  rdata[i][j];
	    cout << endl;
	}
	cout << endl;

	// It is not necessary to close these objects because close() will
	// be called when the object instances are going out of scope.
	dataspace.close();
	memspace.close();
	dataset.close();
	file.close();

    }  // end of try block

    // catch failure caused by the H5File operations
    catch(FileIException error)
    {
	error.printError();
	return -1;
    }

    // catch failure caused by the DataSet operations
    catch(DataSetIException error)
    {
	error.printError();
	return -1;
    }

    // catch failure caused by the DataSpace operations
    catch(DataSpaceIException error)
    {
	error.printError();
	return -1;
    }

    return 0;  // successfully terminated
}

