/* -*- c-file-style: "stroustrup" -*- */

//! <!-- [first_declare] -->

DATASET "DSET1" {DATATYPE H5T_STD_I32LE DATASPACE SIMPLE{(4, 13) / (60, 100)} : : }

//! <!-- [first_declare] -->

//! <!-- [first_reading] -->

/* open the HDF5 file */
fid = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);

/* open the dataset */
did = H5Dopen2(fid, "DSET1", H5P_DEFAULT);
        :
        :
    /* define hsize_t dims[2]; */
    /* define hsize_t new_dims[2]; */
    /* get the dataset's current dimension sizes */
    H5LDget_dset_dims(did, dims);

        /* extend the dataset by 2 */
        new_dims[0] = dims[0] + 2;
        new_dims[1] = dims[1] + 2;
        H5Dset_extent(did, new_dims)

            /* write data to the extended dataset */
            : :
            /* get the size of the dataset's data type */
            type_size = H5LDget_dset_type_size(did, NULL);
        :
        :
    /* allocate buffer for storing selected data elements from the dataset */
    /* calculate # of selected elements from dims & new_dims */
    /* buffer size = type_size * number of selected elements */
        :
        :
    /* read the selected elements from the dataset into buf */
    H5LDget_dset_elmts(did, dims, new_dims, NULL, buf);
        :
        :
    H5Dclose(did);
        H5Fclose(fid);

        //! <!-- [first_reading] -->

        //! <!-- [first_output] -->

    data for elements (0, 13), (0, 14) 
    data for elements (1, 13), (1, 14) 
    data for elements (2, 13), (2, 14) 
    data for elements (3, 13), (3, 14)
    data for elements (4, 0), (4, 1), (4, 2)......................(4, 13), (4, 14)
    data for elements (5, 0), (5, 1), (5, 2)......................(5, 13), (5, 14)

//! <!-- [first_output] -->


//! <!-- [second_declare] -->

    DATASET "DSET2" {
    DATATYPE  H5T_COMPOUND {
        H5T_STD_I32LE "a";
    H5T_STD_I32LE "b";
    H5T_ARRAY
    {
        [4] H5T_STD_I32LE
    }
    "c";
    H5T_STD_I32LE "d";
    H5T_STD_I32LE "e";
    H5T_COMPOUND
    {
        H5T_STD_I32LE "a";
        H5T_STD_I32LE "b";
        H5T_ARRAY
        {
            [4] H5T_STD_I32LE
        }
        "c";
        H5T_STD_I32LE "d";
        H5T_STD_I32LE "e";
    }
    "s2";
    }
    DATASPACE SIMPLE
    {
        (5) / (5)
    }
    ::
    }

    //! <!-- [second_declare] -->

    //! <!-- [second_reading] -->

    /* open the HDF5 file */
    fid = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);

    /* open the dataset */
    did = H5Dopen2(fid, "DSET2", H5P_DEFAULT);

    /* define hsize_t dims[1]; */
    /* define hsize_t new_dims[1]; */
        :
        :
    /* get the dataset's current dimension size */
    H5LDget_dset_dims(did, dims);

        /* extend the dataset by 2 */
        new_dims[0] = dims[0] + 2;
        H5Dset_extent(did, new_dims);
        :
        :
    /* write data to the extended part of the dataset */
        :
        :
    /* #define fields "d,s2.c" */
    /* get the size of the dataset's data type for the selected fields */
    type_size = H5LDget_dset_type_size(did, fields);
        :
        :
    /* allocate buffer for storing selected data elements from the dataset */
    /* calculate # of selected elements from dims & new_dims */
    /* buffer size = type_size * number of selected elements */
        :        
        :
    /* read the selected elements from the dataset into buf */
    H5LD_get_dset_elmts(did, dims, new_dims, fields, buf);
        :
        :
    H5Dclose(did);
        H5Fclose(fid);

        //! <!-- [second_reading] -->

        //! <!-- [second_output] -->

    Data for element (5): integer value for "d", 4 integer values for array "s2.c"
    Data for element (6): integer value for "d", 4 integer values for array "s2.c"

//! <!-- [second_output] -->
