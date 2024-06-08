/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  This example illustrates how to write to and read from an existing
 *  dataset. It is used in the HDF5 Tutorial.
 */

#include <iostream>
#include <string>
#include "H5Cpp.h"
using namespace H5;

const H5std_string FILE_NAME("h5tutr_dset.h5");
const H5std_string DATASET_NAME("dset");
const int          DIM0 = 4; // dataset dimensions
const int          DIM1 = 6;

int
main(void)
{

    // Data initialization.

    int i, j;
    int data[DIM0][DIM1]; // buffer for data to write

    for (j = 0; j < DIM0; j++)
        for (i = 0; i < DIM1; i++)
            data[j][i] = i * 6 + j + 1;

    // Try block to detect exceptions raised by any of the calls inside it
    try {
        // Turn off the auto-printing when failure occurs so that we can
        // handle the errors appropriately
        Exception::dontPrint();

        // Open an existing file and dataset.
        H5File  file(FILE_NAME, H5F_ACC_RDWR);
        DataSet dataset = file.openDataSet(DATASET_NAME);

        // Write the data to the dataset using default memory space, file
        // space, and transfer properties.
        dataset.write(data, PredType::NATIVE_INT);

    } // end of try block

    // catch failure caused by the H5File operations
    catch (FileIException error) {
        error.printErrorStack();
        return -1;
    }

    // catch failure caused by the DataSet operations
    catch (DataSetIException error) {
        error.printErrorStack();
        return -1;
    }

    return 0; // successfully terminated
}
