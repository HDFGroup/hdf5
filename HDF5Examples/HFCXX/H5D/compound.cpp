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
 * This example shows how to create a compound datatype,
 * write an array which has the compound datatype to the file,
 * and read back fields' subsets.
 */

#include <iostream>
#include <string>
#include <highfive/highfive.hpp>
using namespace HighFive;

const std::string FILE_NAME("SDScompound.h5");
const std::string DATASET_NAME("ArrayOfStructures");
const std::string MEMBER1("a_name");
const std::string MEMBER2("b_name");
const std::string MEMBER3("c_name");
const int         LENGTH = 10;
const int         RANK   = 1;

int
main(void)
{
    /* First structure and dataset*/
    typedef struct s1_t {
        int    a;
        float  b;
        double c;
    } s1_t;

    // Tell HighFive how to create the HDF5 datatype for this base type by
    // using the HIGHFIVE_REGISTER_TYPE macro
    CompoundType create_compound_s1_t()
    {
        return {
            {"a", create_datatype<int>{}}, {"b", create_datatype<float>{}}, {"c", create_datatype<double>{}}};
    }
    HIGHFIVE_REGISTER_TYPE(s1_t, create_compound_s1_t)

    /* Second structure (subset of s1_t) and dataset*/
    typedef struct s2_t {
        double c;
        int    a;
    } s2_t;

    // Tell HighFive how to create the HDF5 datatype for this base type by
    // using the HIGHFIVE_REGISTER_TYPE macro
    CompoundType create_compound_s2_t()
    {
        return {{"c", create_datatype<float>{}}, {"a", create_datatype<int>{}}};
    }
    HIGHFIVE_REGISTER_TYPE(s2_t, create_compound_s2_t)

    try {
        // Create a new file using the default property lists. Note that
        // `File::Truncate` will, if present, truncate the file before opening
        // it for reading and writing.
        File file(FILE_NAME, File::Truncate);

        auto mtype1 = create_compound_s1_t();
        mtype1.commit(file, "s1_t");

        // Define the size of our dataset: LENGTH
        std::vector<s1_t> dims{LENGTH};

        // Initialize the data
        std::vector<s1_t> data;
        int               i;
        for (i = 0; i < LENGTH; i++) {
            data.push_back({i, i * i, 1. / (i + 1)});
        }

        // Create the dataset
        DataSet dataset = file.createDataSet<mtype1>(DATASET_NAME, DataSpace(dims));

        // write it
        dataset.write(data);

        // flush everything
        file.flush();

        // we get the dataset
        DataSet dataset = file.getDataSet(dataset_name);

        // we convert the hdf5 dataset to a single dimension vector
        dataset.read(read_data);

        /*
         * Read two fields c and a from s1 dataset. Fields in the file
         * are found by their names "c_name" and "a_name".
         */
        s2_t s2[LENGTH];
        dataset->read(s2, mtype2);

        /*
         * Display the fields
         */
        std::cout << endl << "Field c : " << endl;
        for (i = 0; i < LENGTH; i++)
            std::cout << s2[i].c << " ";
        std::cout << endl;

        std::cout << endl << "Field a : " << endl;
        for (i = 0; i < LENGTH; i++)
            std::cout << s2[i].a << " ";
        std::cout << endl;

        /*
         * Create a datatype for s3.
         */
        CompType mtype3(sizeof(float));

        mtype3.insertMember(MEMBER2, 0, create_datatype<float>{});

        /*
         * Read field b from s1 dataset. Field in the file is found by its name.
         */
        float s3[LENGTH]; // Third "structure" - used to read float field of s1
        dataset->read(s3, mtype3);

        /*
         * Display the field
         */
        std::cout << endl << "Field b : " << endl;
        for (i = 0; i < LENGTH; i++)
            std::cout << s3[i] << " ";
        std::cout << endl;
    }
    catch (const Exception &err) {
        // catch and print any HDF5 error
        std::cerr << err.what() << std::endl;
        return -1;
    }

    return 0; // successfully terminated
}
