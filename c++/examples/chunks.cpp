/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 *   This example shows how to read data from a chunked dataset.
 *   We will read from the file created by extend.cpp
 */

#include <iostream>
using std::cout;
using std::endl;

#include <string>
#include "H5Cpp.h"
using namespace H5;

const H5std_string FILE_NAME("SDSextendible.h5");
const H5std_string DATASET_NAME("ExtendibleArray");
const int          NX    = 10;
const int          NY    = 5;
const int          RANK  = 2;
const int          RANKC = 1;

int
main(void)
{
    hsize_t i, j;

    // Try block to detect exceptions raised by any of the calls inside it
    try {
        /*
         * Turn off the auto-printing when failure occurs so that we can
         * handle the errors appropriately
         */
        Exception::dontPrint();

        /*
         * Open the file and the dataset.
         */
        H5File  file(FILE_NAME, H5F_ACC_RDONLY);
        DataSet dataset = file.openDataSet(DATASET_NAME);

        /*
         * Get filespace for rank and dimension
         */
        DataSpace filespace = dataset.getSpace();

        /*
         * Get number of dimensions in the file dataspace
         */
        int rank = filespace.getSimpleExtentNdims();

        /*
         * Get and print the dimension sizes of the file dataspace
         */
        hsize_t dims[2]; // dataset dimensions
        rank = filespace.getSimpleExtentDims(dims);
        cout << "dataset rank = " << rank << ", dimensions " << (unsigned long)(dims[0]) << " x "
             << (unsigned long)(dims[1]) << endl;

        /*
         * Define the memory space to read dataset.
         */
        DataSpace mspace1(RANK, dims);

        /*
         * Read dataset back and display.
         */
        int data_out[NX][NY]; // buffer for dataset to be read
        dataset.read(data_out, PredType::NATIVE_INT, mspace1, filespace);

        cout << "\n";
        cout << "Dataset: \n";
        for (j = 0; j < dims[0]; j++) {
            for (i = 0; i < dims[1]; i++)
                cout << data_out[j][i] << " ";
            cout << endl;
        }

        /*
         *          dataset rank 2, dimensions 10 x 5
         *          chunk rank 2, dimensions 2 x 5

         *          Dataset:
         *          1 1 1 3 3
         *          1 1 1 3 3
         *          1 1 1 0 0
         *          2 0 0 0 0
         *          2 0 0 0 0
         *          2 0 0 0 0
         *          2 0 0 0 0
         *          2 0 0 0 0
         *          2 0 0 0 0
         *          2 0 0 0 0
         */

        /*
         * Read the third column from the dataset.
         * First define memory dataspace, then define hyperslab
         * and read it into column array.
         */
        hsize_t col_dims[1];
        col_dims[0] = 10;
        DataSpace mspace2(RANKC, col_dims);

        /*
         * Define the column (hyperslab) to read.
         */
        hsize_t offset[2] = {0, 2};
        hsize_t count[2]  = {10, 1};
        int     column[10]; // buffer for column to be read

        /*
         * Define hyperslab and read.
         */
        filespace.selectHyperslab(H5S_SELECT_SET, count, offset);
        dataset.read(column, PredType::NATIVE_INT, mspace2, filespace);

        cout << endl;
        cout << "Third column: " << endl;
        for (i = 0; i < 10; i++)
            cout << column[i] << endl;

        /*
         *          Third column:
         *          1
         *          1
         *          1
         *          0
         *          0
         *          0
         *          0
         *          0
         *          0
         *          0
         */

        /*
         * Get creation properties list.
         */
        DSetCreatPropList cparms = dataset.getCreatePlist();

        /*
         * Check if dataset is chunked.
         */
        hsize_t chunk_dims[2];
        int     rank_chunk;
        if (H5D_CHUNKED == cparms.getLayout()) {
            /*
             * Get chunking information: rank and dimensions
             */
            rank_chunk = cparms.getChunk(2, chunk_dims);
            cout << "chunk rank " << rank_chunk << "dimensions " << (unsigned long)(chunk_dims[0]) << " x "
                 << (unsigned long)(chunk_dims[1]) << endl;

            /*
             * Define the memory space to read a chunk.
             */
            DataSpace mspace3(rank_chunk, chunk_dims);

            /*
             * Define chunk in the file (hyperslab) to read.
             */
            offset[0] = 2;
            offset[1] = 0;
            count[0]  = chunk_dims[0];
            count[1]  = chunk_dims[1];
            filespace.selectHyperslab(H5S_SELECT_SET, count, offset);

            /*
             * Read chunk back and display.
             */
            int chunk_out[2][5]; // buffer for chunk to be read
            dataset.read(chunk_out, PredType::NATIVE_INT, mspace3, filespace);
            cout << endl;
            cout << "Chunk:" << endl;
            for (j = 0; j < chunk_dims[0]; j++) {
                for (i = 0; i < chunk_dims[1]; i++)
                    cout << chunk_out[j][i] << " ";
                cout << endl;
            }
            /*
             *   Chunk:
             *   1 1 1 0 0
             *   2 0 0 0 0
             */
        }
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

    // catch failure caused by the DataSpace operations
    catch (DataSpaceIException error) {
        error.printErrorStack();
        return -1;
    }
    return 0;
}
