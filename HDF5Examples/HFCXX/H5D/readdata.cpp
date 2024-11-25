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

//
//      This example reads hyperslab from the SDS.h5 file into
//      two-dimensional plane of a three-dimensional array.  Various
//      information about the dataset in the SDS.h5 file is obtained.
//

#include <iostream>

#include <string>
#include <highfive/highfive.hpp>
using namespace HighFive;

const std::string FILE_NAME("SDS.h5");
const std::string DATASET_NAME("IntArray");
const int         NX_SUB   = 3; // hyperslab dimensions
const int         NY_SUB   = 4;
const int         NX       = 7; // output buffer dimensions
const int         NY       = 7;
const int         NZ       = 3;
const int         RANK_OUT = 3;

int
main(void)
{
    /*
     * Output buffer initialization.
     */
    int i, j, k;
    int data_out[NX][NY][NZ]; /* output buffer */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            for (k = 0; k < NZ; k++)
                data_out[j][i][k] = 0;
        }
    }

    try {
        // we open the existing hdf5 file we created before
        File file(FILE_NAME, File::ReadOnly);

        std::vector<int> read_data;

        // we get the dataset
        DataSet dataset = file.getDataSet(DATASET_NAME);

        // Get the class of the datatype that is used by the dataset.
        DataTypeClass type_class = dataset.getDataType().getClass();

        // Get class of datatype and print message if it's an integer.
        if (type_class == DataTypeClass::Integer) {
            std::cout << "Data set has INTEGER type" << std::endl;

            // Get the integer datatype
            DataType intype = dataset.getDataType();

            /*
             * Get order of datatype and print message if it's a little endian.
             */
            // H5std_string order_string;
            //(void)intype.getOrder(order_string);
            // std::cout << order_string << std::endl;

            // Get size of the data element stored in file and print it.
            size_t size = intype.getSize();
            std::cout << "Data size is " << size << std::endl;
        }

        // Get dataspace of the dataset.
        DataSpace dataspace = dataset.getSpace();

        // Get the number of dimensions in the dataspace.
        size_t rank = dataspace.getNumberDimensions();

        // Get the dimension size of each dimension in the dataspace and
        // display them.
        auto dims_out = dspace.getDimensions() std::cout << "rank " << rank << ", dimensions "
                                                          << (unsigned long)(dims_out[0]) << " x "
                                                          << (unsigned long)(dims_out[1]) << std::endl;

        /*
         * Define hyperslab in the dataset; implicitly giving strike and
         * block NULL.
         */
        hsize_t offset[2]; // hyperslab offset in the file
        hsize_t count[2];  // size of the hyperslab in the file
        offset[0] = 1;
        offset[1] = 2;
        count[0]  = NX_SUB;
        count[1]  = NY_SUB;
        dataspace.selectHyperslab(H5S_SELECT_SET, count, offset);

        /*
         * Define the memory dataspace.
         */
        hsize_t dimsm[3]; /* memory space dimensions */
        dimsm[0] = NX;
        dimsm[1] = NY;
        dimsm[2] = NZ;
        DataSpace memspace(RANK_OUT, dimsm);

        /*
         * Define memory hyperslab.
         */
        hsize_t offset_out[3]; // hyperslab offset in memory
        hsize_t count_out[3];  // size of the hyperslab in memory
        offset_out[0] = 3;
        offset_out[1] = 0;
        offset_out[2] = 0;
        count_out[0]  = NX_SUB;
        count_out[1]  = NY_SUB;
        count_out[2]  = 1;
        memspace.selectHyperslab(H5S_SELECT_SET, count_out, offset_out);

        // we convert the hdf5 dataset to a single dimension vector
        dataset.read(read_data);

        /*
         * Read data from hyperslab in the file into the hyperslab in
         * memory and display the data.
         */
        dataset.read(data_out, PredType::NATIVE_INT, memspace, dataspace);

        for (j = 0; j < NX; j++) {
            for (i = 0; i < NY; i++)
                std::cout << data_out[j][i][0] << " ";
            std::cout << std::endl;
        }

        for (size_t i = 0; i < read_data.size(); ++i) {
            std::cout << read_data[i] << " ";
        }
        std::cout << "\n";
        /*
         * 0 0 0 0 0 0 0
         * 0 0 0 0 0 0 0
         * 0 0 0 0 0 0 0
         * 3 4 5 6 0 0 0
         * 4 5 6 7 0 0 0
         * 5 6 7 8 0 0 0
         * 0 0 0 0 0 0 0
         */
    }
    catch (const Exception &err) {
        // catch and print any HDF5 error
        std::cerr << err.what() << std::endl;
        return -1;
    }

    return 0; // successfully terminated
}
