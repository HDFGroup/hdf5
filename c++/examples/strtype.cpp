/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* (wrong, need to change to this example!!!)

 *  This program shows how the select_hyperslab and select_elements
 *  functions are used to write selected data from memory to the file.
 *  Program takes 48 elements from the linear buffer and writes them into
 *  the matrix using 3x2 blocks, (4,3) stride and (2,4) count.
 *  Then four elements  of the matrix are overwritten with the new values and
 *  file is closed. Program reopens the file and reads and displays the result.
 */

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cout;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "H5Cpp.h"      // C++ API header file

#ifndef H5_NO_NAMESPACE
using namespace H5;
#endif

const H5std_string      FILE_NAME( "varlen.h5");
const H5std_string DSET_VLSTR_NAME( "vlstr_type" );
const int   SPACE1_DIM1 = 4;
const int   SPACE1_RANK = 1;

int main()
{
#ifdef SKIP_UNTIL_APRIL_2009
    const char *wdata[SPACE1_DIM1]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure."
        };   /* Information to write */
    char *rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		native_type;       /* Datatype ID */
    hsize_t		dims1[] = {SPACE1_DIM1};

   // Try block to detect exceptions raised by any of the calls inside it
   try
   {
      /*
       * Turn off the auto-printing when failure occurs so that we can
       * handle the errors appropriately
       */
      Exception::dontPrint();

      /*
       * Create a new file using H5F_ACC_TRUNC access,
       * default file creation properties, and default file
       * access properties.
       */
      H5File file( FILE_NAME, H5F_ACC_TRUNC );

      /* Create dataspace for datasets */
      DataSpace sid1(SPACE1_RANK, dims1);

      /* Create a variable-length datatype */
      StrType tid1(0, H5T_VARIABLE);

      /*
       * Create a new dataset within the file using defined dataspace and
       * datatype and default dataset creation properties.
       */
      DataSet dataset = file.createDataSet(DSET_VLSTR_NAME, tid1, sid1);

      /*
       * Write the data to the dataset using default memory space, file
       * space, and transfer properties.
       */
      dataset.write((void*)wdata, tid1);

      /* Close Dataset */
      dataset.close();

      /* Re-open the dataset */
      dataset = file.openDataSet(DSET_VLSTR_NAME);

      /* Get datatype for dataset */
      DataType dtype = dataset.getDataType();

      /* Read dataset from disk */
      dataset.read((void*)rdata, dtype);

      /* Print data read in */
      unsigned i;
      cout << "data read:" << endl;
      for(i=0; i<SPACE1_DIM1; i++)
	  cout << wdata[i] << endl;
      cout << endl;

      /* Free memory for rdata */
      dataset.vlenReclaim(rdata, dtype, sid1);
   } // end of try block

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

   // catch failure caused by the DataSpace operations
   catch( DataSpaceIException error )
   {
      error.printError();
      return -1;
   }

   // catch failure caused by the DataSpace operations
   catch( DataTypeIException error )
   {
      error.printError();
      return -1;
   }
#endif
   return 0;  // successfully terminated
}
