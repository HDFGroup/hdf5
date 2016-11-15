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
 *  This example illustrates how to create an attribute attached to a
 *  dataset. It is used in the HDF5 Tutorial.
 */

#include <iostream>
#include <string>
#include "H5Cpp.h"
using namespace H5;

const H5std_string	FILE_NAME( "h5tutr_dset.h5" );
const H5std_string	DATASET_NAME( "dset" );
const H5std_string	ATTR_NAME( "Units" );

const int	DIM1 = 2;

int main (void)
{
   int attr_data[2] = { 100, 200};
   hsize_t dims[1] = { DIM1 };
   

   // Try block to detect exceptions raised by any of the calls inside it
   try
   {
	// Turn off the auto-printing when failure occurs so that we can
	// handle the errors appropriately
	Exception::dontPrint();

	// Open an existing file and dataset.
	H5File file( FILE_NAME, H5F_ACC_RDWR );
	DataSet dataset = file.openDataSet( DATASET_NAME );

	// Create the data space for the attribute.
	DataSpace attr_dataspace = DataSpace (1, dims );

	// Create a dataset attribute. 
	Attribute attribute = dataset.createAttribute( ATTR_NAME, PredType::STD_I32BE, 
	                                          attr_dataspace);
     
	// Write the attribute data. 
	attribute.write( PredType::NATIVE_INT, attr_data);

   }  // end of try block

   // catch failure caused by the H5File operations
   catch( DataSpaceIException error )
   {
	error.printError();
	return -1;
   }

   // catch failure caused by the H5File operations
   catch( AttributeIException error )
   {
	error.printError();
	return -1;
   }

   // catch failure caused by the H5File operations
   catch( FileIException error )
   {
	error.printError();
	return -1;
   }

   // catch failure caused by the DataSet operations
   catch( DataSetIException error )
   {
	error.printError();
	return -1;
   }

   return 0;  // successfully terminated
}

