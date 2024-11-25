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
//  This example writes a dataset to a new HDF5 file.
//

#include <iostream>
#include <string>
#include <highfive/highfive.hpp>
using namespace HighFive;

const std::string FILE_NAME("SDS.h5");
const std::string DATASET_NAME("IntArray");
const int         NX   = 5; // dataset dimensions
const int         NY   = 6;
const int         RANK = 2;

int
main(void)
{
    /*
     * Data initialization.
     */
    int i, j;
    int data[NX][NY]; // buffer for data to write
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = i + j;
    }
    /*
     * 0 1 2 3 4 5
     * 1 2 3 4 5 6
     * 2 3 4 5 6 7
     * 3 4 5 6 7 8
     * 4 5 6 7 8 9
     */

    try {
        // Create a new file using the default property lists. Note that
        // `File::Truncate` will, if present, truncate the file before opening
        // it for reading and writing.
        File file(FILE_NAME, File::Truncate);

        // Define the size of our dataset: NXxNY
        std::vector<size_t> dims{NX, NY};

        // Create the dataset
        DataSet dataset = file.createDataSet<int>(DATASET_NAME, DataSpace(dims));

        // write it
        dataset.write(data);
    }
    catch (const Exception &err) {
        // catch and print any HDF5 error
        std::cerr << err.what() << std::endl;
        return -1;
    }

    return 0; // successfully terminated
}
