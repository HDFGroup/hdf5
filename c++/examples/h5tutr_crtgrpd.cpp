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

/*
 *  This example illustrates how to create a dataset in a group.
 *  It is used in the HDF5 Tutorial.
 */

#include <iostream>
using std::cout;
using std::endl;

#include <string>
#include "H5Cpp.h"
using namespace H5;

const H5std_string FILE_NAME("h5tutr_groups.h5");
const H5std_string DATASET_NAME1("/MyGroup/dset1");
const H5std_string DATASET_NAME2("dset2");
const int       RANK = 2;
const int       D1DIM1 = 3;
const int       D1DIM2 = 3;
const int       D2DIM1 = 2;
const int       D2DIM2 = 10;

int main(void)
{
    int dset1_data[D1DIM1][D1DIM2], dset2_data[D2DIM1][D2DIM2]; // data buffers
    int i, j;

    // Try block to catch exceptions raised by any of the calls inside it
    try
    {
        // Turn off the auto-printing when failure occurs so that we can
        // handle the errors appropriately
        Exception::dontPrint();

        // Initialize the first dataset. 
        for (i = 0; i < D1DIM1; i++)
            for (j = 0; j < D1DIM2; j++)
               dset1_data[i][j] = j + 1;

        //  Initialize the second dataset. 
        for (i = 0; i < D2DIM1; i++)
          for (j = 0; j < D2DIM2; j++)
              dset2_data[i][j] = j + 1;

        // Open an existing file and dataset.
        H5File file(FILE_NAME, H5F_ACC_RDWR);

        // Create the data space for the first dataset.  Note the use of
        // pointer for the instance 'dataspace'.  It can be deleted and
        // used again later for another data space.  An HDF5 identifier is
        // closed by the destructor or the method 'close()'.
        hsize_t dims[RANK];               // dataset dimensions
        dims[0] = D1DIM1;
        dims[1] = D1DIM2;
        DataSpace *dataspace = new DataSpace (RANK, dims);

        // Create the dataset in group "MyGroup".  Same note as for the
        // dataspace above.
        DataSet *dataset = new DataSet (file.createDataSet(DATASET_NAME1, 
                                         PredType::STD_I32BE, *dataspace));

        // Write the data to the dataset using default memory space, file
        // space, and transfer properties.
        dataset->write(dset1_data, PredType::NATIVE_INT);

        // Close the current dataset and data space.
        delete dataset;
        delete dataspace;

        // Create the data space for the second dataset.
        dims[0] = D2DIM1;
        dims[1] = D2DIM2;
        dataspace = new DataSpace (RANK, dims);

        // Create group "Group_A" in group "MyGroup".
        Group group(file.openGroup("/MyGroup/Group_A"));

        // Create the second dataset in group "Group_A".
        dataset = new DataSet (group.createDataSet(DATASET_NAME2, 
                                         PredType::STD_I32BE, *dataspace));

        // Write the data to the dataset using default memory space, file
        // space, and transfer properties.
        dataset->write(dset2_data, PredType::NATIVE_INT);

        // Close all objects.
        delete dataspace;
        delete dataset;
        group.close();
    
    } // end of try block

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

    // catch failure caused by the Group operations
    catch(GroupIException error)
    {
        error.printErrorStack();
        return -1;
    }
 
     return 0;
}
