#include "hdf5.h"


/*    This program shows how to create and store references to the objects.
 *    Program creates a file, two groups, a dataset to store integer data and 
 *    a dataset to store references to the objects. 
 *    Stored references are used to open the objects they are point to.
 *    Data is written to the dereferenced dataset, and class type is 
 *    displayed for the shared datatype.
 */

#define filename "REF_OBJ.h5"         /* file name */
#define dsetnamei "INTEGERS"          /* dataset with integer data */
#define dsetnamer "OBJECT_REFERENCES" /* dataset with object references */
#define groupname1 "GROUP1"           /* groups in the file */
#define groupname2 "GROUP2" 

int main()
{
    hid_t fileid;        /* file identifier */
    hid_t grp1_id;       /* group identifiers */
    hid_t grp2_id;
    hid_t dset_id;       /* dataset identifiers */
    hid_t dsetr_id;
    hid_t type_id;       /* datatype identifiers */
    hid_t space_id;      /* data space identifiers */
    hid_t spacer_id;     
    hsize_t dims1[1] =  {5};
    hsize_t dimsr[1] =  {4};
    hsize_t my_maxdims[1] = {5}; 
    int rank = 1;
    int rankr =1;
    H5T_class_t class;
    herr_t status;
    hobj_ref_t  ref[4], ref_out[4];
    int data[5] = {1, 2, 3, 4, 5};

    /* 
     * Create file with default file access and file creation properties. 
     */
    fileid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* 
     * Create a group in the file. 
     */
    grp1_id = H5Gcreate(fileid, groupname1, -1);

    /* 
     * Create a group inside the created gorup. 
     */
    grp2_id = H5Gcreate(grp1_id, groupname2, -1);

    /* 
     * Create dataspaces for datasets. 
     */
    space_id = H5Screate_simple(rank, dims1, my_maxdims);
    spacer_id = H5Screate_simple(rankr, dimsr, NULL);

    /*
     * Create integer dataset.
     */
    dset_id = H5Dcreate(fileid, dsetnamei, H5T_NATIVE_INT, space_id, H5P_DEFAULT);

    /*
     * Create dataset to store references to the objects.
     */
    dsetr_id = H5Dcreate(fileid, dsetnamer, H5T_STD_REF_OBJ, spacer_id, H5P_DEFAULT);

    /* 
     * Create a datatype and store in the file.
     */
    type_id = H5Tcopy(H5T_NATIVE_FLOAT);
    status = H5Tcommit(fileid, "MYTYPE", type_id);

    /*
     * Close dataspaces, groups and integer dataset.
     */
    status = H5Sclose(space_id);
    status = H5Sclose(spacer_id);
    status = H5Tclose(type_id);
    status = H5Dclose(dset_id);
    status = H5Gclose(grp1_id);
    status = H5Gclose(grp2_id);

    /*
     * Create references to two groups, integer dataset and shared datatype
     * and write it to the dataset in the file.
     */
    status = H5Rcreate(&ref[0], fileid, groupname1, H5R_OBJECT, -1);
    status = H5Rcreate(&ref[1], fileid, "/GROUP1/GROUP2", H5R_OBJECT, -1);
    status = H5Rcreate(&ref[2], fileid, dsetnamei, H5R_OBJECT, -1);
    status = H5Rcreate(&ref[3], fileid, "MYTYPE", H5R_OBJECT, -1);

    status = H5Dwrite(dsetr_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL,
                  H5P_DEFAULT, ref);
    /* 
     * Close the dataset. 
     */
    status =  H5Dclose(dsetr_id);

    /*
     * Reopen the dataset with object references and read references to the buffer.
     */
    dsetr_id = H5Dopen (fileid, dsetnamer);
    status = H5Dread(dsetr_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, 
                   H5P_DEFAULT, ref_out);

    /* 
     * Dereference the third reference. We know that it is a dataset. On practice
     * one should use H5Rget_object_type function to find out
     * the type of an object the reference points to.
     */
    dset_id = H5Rdereference(dsetr_id, H5R_OBJECT, &ref[2]);

    /*
     * Write data to the dataset.
     */
    status = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data);
    if (status >= 0) 
        printf("Data has been successfully written to the dataset\n"); 

    /*
     * Dereference the fourth reference. We know that it is a datatype. On practice
     * one should use H5Rget_object_type function to find out
     * the type of an object the reference points to.
     */
    type_id = H5Rdereference(dsetr_id, H5R_OBJECT, &ref[3]);

    /*
     * Get datatype class and display it if it is of a FLOAT class.
     */
    class = H5Tget_class(type_id);
    if (class == H5T_FLOAT)
        printf("Stored datatype is of a FLOAT class\n");

    /*
     * Close all objects.
     */
    status = H5Dclose(dset_id);
    status = H5Dclose(dsetr_id);
    status = H5Tclose(type_id);
    status = H5Fclose(fileid);

    return 0;

}

   

