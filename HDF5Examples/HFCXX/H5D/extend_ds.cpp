/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *   This example shows how to work with extendible dataset.
 *   In the current version of the library dataset MUST be
 *   chunked.
 *
 */

#include <iostream>
#include <string>
#include <highfive/highfive.hpp>
using namespace HighFive;

const std::string FILE_NAME("SDSextendible.h5");
const std::string DATASET_NAME("ExtendibleArray");
const int         NX   = 10;
const int         NY   = 5;
const int         RANK = 2;

int
main(void)
{
    /*
     * Try block to detect exceptions raised by any of the calls inside it
     */
    try {
        /*
         * Create the data space with unlimited dimensions.
         */
        hsize_t   dims[2]    = {3, 3}; // dataset dimensions at creation
        hsize_t   maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
        DataSpace mspace1(RANK, dims, maxdims);

        /*
         * Create a new file. If file exists its contents will be overwritten.
         */
        File file(FILE_NAME, File::Truncate);

        /*
         * Modify dataset creation properties, i.e. enable chunking.
         */
        auto cparms = FileCreateProps();

        hsize_t chunk_dims[2] = {2, 5};
        cparms.setChunk(RANK, chunk_dims);

        /*
         * Set fill value for the dataset
         */
        int fill_val = 0;
        cparms.setFillValue(create_datatype<int>(), &fill_val);

        /*
         * Create a new dataset within the file using cparms
         * creation properties.
         */
        DataSet dataset = file.createDataSet(DATASET_NAME, create_datatype<int>(), mspace1, cparms);

        /*
         * Extend the dataset. This call assures that dataset is at least 3 x 3.
         */
        hsize_t size[2];
        size = {3, 3};
        dataset.extend(size);

        /*
         * Select a hyperslab.
         */
        DataSpace fspace1 = dataset.getSpace();

        std::vector<size_t> offset[2];
        offset = {0, 0};

        std::vector<size_t> dims1[2] = {3, 3}; /* data1 dimensions */
        fspace1.select(dims1, offset);

        /*
         * Write the data to the hyperslab.
         */
        int data1[3][3] = {{1, 1, 1}, /* data to write */
                           {1, 1, 1},
                           {1, 1, 1}};
        dataset.write(data1, create_datatype<int>(), mspace1, fspace1);

        /*
         * Extend the dataset. Dataset becomes 10 x 3.
         */
        std::vector<size_t> dims2[2] = {7, 1}; /* data2 dimensions */
        dims[0]                      = dims1[0] + dims2[0];

        size = {dims[0], dims[1]};
        dataset.extend(size);

        /*
         * Select a hyperslab.
         */
        DataSpace fspace2 = dataset.getSpace();
        offset            = {3, 0};
        fspace2.select(dims2, offset);

        /*
         * Define memory space
         */
        DataSpace mspace2(RANK, dims2);

        /*
         * Write the data to the hyperslab.
         */
        int data2[7] = {2, 2, 2, 2, 2, 2, 2};
        dataset.write(data2, create_datatype<int>(), mspace2, fspace2);

        /*
         * Extend the dataset. Dataset becomes 10 x 5.
         */
        std::vector<size_t> dims3[2] = {2, 2}; /* data3 dimensions */
        dims[1]                      = dims1[1] + dims3[1];
        size                         = {dims[0], dims[1]};
        dataset.extend(size);

        /*
         * Select a hyperslab
         */
        DataSpace fspace3 = dataset.getSpace();
        offset            = {0, 3};
        fspace3.select(dims3, offset);

        /*
         * Define memory space.
         */
        DataSpace mspace3(RANK, dims3);

        /*
         * Write the data to the hyperslab.
         */
        int data3[2][2] = {{3, 3}, {3, 3}};
        dataset.write(data3, create_datatype<int>(), mspace3, fspace3);

        /*
         * Read the data from this dataset and display it.
         */
        int i, j;
        int data_out[NX][NY];
        for (i = 0; i < NX; i++) {
            for (j = 0; j < NY; j++)
                data_out[i][j] = 0;
        }
        dataset.read(data_out, create_datatype<int>());
        /*
         * Resulting dataset
         *
         *         1 1 1 3 3
         *         1 1 1 3 3
         *         1 1 1 0 0
         *         2 0 0 0 0
         *         2 0 0 0 0
         *         2 0 0 0 0
         *         2 0 0 0 0
         *         2 0 0 0 0
         *         2 0 0 0 0
         *         2 0 0 0 0
         */
        /*
         * Display the result.
         */
        for (i = 0; i < NX; i++) {
            for (j = 0; j < NY; j++)
                std::cout << data_out[i][j] << "  ";
            std::cout << std::endl;
        }
    } // end of try block
    catch (const Exception &err) {
        // catch and print any HDF5 error
        std::cerr << err.what() << std::endl;
        return -1;
    }

    return 0; // successfully terminated
}
