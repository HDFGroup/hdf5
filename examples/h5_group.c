/*
 * This example creates a group in the file and dataset in the group. 
 * Hard link to the group object is created and the dataset is accessed
 * under different names. 
 * Iterator function is used to find the object names in the root group.
 */ 


#include "hdf5.h"


#define FILE    "group.h5"
#define RANK    2

 
herr_t file_info(hid_t loc_id, const char *name, void *opdata);
                                     /* Operator function */
int
main(void)
{

    hid_t    file;
    hid_t    grp;
    hid_t    dataset, dataspace;
    hid_t    plist; 

    herr_t   status;
    hsize_t  dims[2];
    hsize_t  cdims[2];
 
    int      idx;

    /*
     * Create a file.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a group in the file. 
     */
    grp = H5Gcreate(file, "/Data", 0);

    /*
     * Create dataset "Compressed Data" in the group using absolute
     * name. Dataset creation property list is modified to use 
     * GZIP compression with the compression effort set to 6. 
     * Note that compression can be used only when dataset is chunked. 
     */
    dims[0] = 1000;
    dims[1] = 20;
    cdims[0] = 20;
    cdims[1] = 20;
    dataspace = H5Screate_simple(RANK, dims, NULL);
    plist     = H5Pcreate(H5P_DATASET_CREATE);
                H5Pset_chunk(plist, 2, cdims);
                H5Pset_deflate( plist, 6); 
    dataset = H5Dcreate(file, "/Data/Compressed_Data", H5T_NATIVE_INT, 
                        dataspace, plist); 
 
    /* 
     * Close the dataset and the file.
     */
    H5Sclose(dataspace);
    H5Dclose(dataset);
    H5Fclose(file);

    /*
     * Now reopen the file and group in the file. 
     */
    file = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    grp  = H5Gopen(file, "Data");

    /* 
     * Access "Compressed_Data" dataset in the group. 
     */
    dataset = H5Dopen(grp, "Compressed_Data");
    if( dataset < 0) printf(" Dataset is not found. \n");
    printf("\"/Data/Compressed_Data\" dataset is open \n");

    /*
     * Close the dataset.
     */
    status = H5Dclose(dataset);

    /*
     * Create hard link to the Data group.
     */
    status = H5Glink(file, H5G_LINK_HARD, "Data", "Data_new");

    /* 
     * We can access "Compressed_Data" dataset using created
     * hard link "Data_new". 
     */
    dataset = H5Dopen(file, "/Data_new/Compressed_Data");
    if( dataset < 0) printf(" Dataset is not found. \n");
    printf("\"/Data_new/Compressed_Data\" dataset is open \n");

    /*
     * Close the dataset.
     */
    status = H5Dclose(dataset);

    /* 
     * Use iterator to see the names of the objects in the file
     * root directory.
     */
    idx = H5Giterate(file, "/", NULL, file_info, NULL);

    /*
     * Unlink  name "Data" and use iterator to see the names
     * of the objects in the file root direvtory.
     */
    if (H5Gunlink(file, "Data") < 0)  
      printf(" H5Gunlink failed \n");
    else  
      printf("\"Data\" is unlinked \n");

    idx = H5Giterate(file, "/", NULL, file_info, NULL);
    

    /*
     * Close the file.
     */
     
    status = H5Fclose(file);

    return 0;
}
/*
 * Operator function.
 */
herr_t
file_info(hid_t loc_id, const char *name, void *opdata)
{
    hid_t grp;
    /*
     * Open the group using its name.
     */
    grp = H5Gopen(loc_id, name);
 
    /*
     * Display group name.
     */
    printf("\n");
    printf("Name : ");
    puts(name);
    
    H5Gclose(grp);
    return 0;
 }
