#include <h5test.h>

#ifndef H5_HAVE_SRB
int main(void)
{
    printf("Test skipped because SRB driver not available\n");
    return 0;
}
#else

#define fileName "/projects/mdas/srb/SRBVault/slu.ncsa/a.h5"
#define DATASETNAME "Int1Array"
#define NX     5                      /* dataset dimensions */
#define NY     6
#define RANK   2

int main(void)
{
    SRB_Info srb_info={ NULL,
                        NULL,
                        NULL,
                        0,
                        0600,
                        -1
                      };
    hid_t         fapl =-1, file;
    hid_t         dataspace, datatype, dataset;
    hsize_t       dimsf[2];
  
    herr_t        status = 0;
    int           data[NX][NY];          /* data to write */
    int           i, j;

    /* 
     * Data  and output buffer initialization. 
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = i*i + j*j;
    }     
    /*
     *  0   1  4  9 16 25 
     *  1   2  5 10 17 26
     *  4   5  8 13 20 29
     *  9  10 13 18 25 34
     * 16  17 20 25 32 41
     */
  
    /* Create access property list and set the driver to GASS */
    fapl = H5Pcreate (H5P_FILE_ACCESS);
         if (fapl < 0) {
             printf (" H5Pcreate failed. \n");
             return -1;
    }
    status = H5Pset_fapl_srb (fapl, srb_info);
    if (status < 0) {
         printf ("H5Pset_fapl_gass failed. \n");
         return -1;
    }

    /*
     * Open an existing file using H5F_ACC_RDWR access,
     * and srb file access properties.
     */
    file = H5Fopen(fileName, H5F_ACC_RDWR, fapl);
    if (file < 0) {
        printf ("H5Fopen failed. \n");
        return -1;
    }
  
    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset. 
     */
    dimsf[0] = NX;
    dimsf[1] = NY;
    dataspace = H5Screate_simple(RANK, dimsf, NULL); 
    if (dataspace < 0) {
      printf ("H5Screate failed. \n");
      return -1;
    }

    /* 
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    if (datatype < 0) {
      printf ("H5Tcopy failed. \n");
      return -1;
    }
    
    status = H5Tset_order(datatype, H5T_ORDER_LE);
    if (status < 0) {
      printf ("H5Tset_order failed. \n");
      return -1;
    }

    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dataset = H5Dcreate(file, DATASETNAME, datatype, dataspace,
			H5P_DEFAULT);
    if (dataset < 0) {
      printf ("H5Dcreate failed. \n");
      return -1;
    }

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);
    if (status < 0) {
      printf ("H5Dwrite failed. \n");
      return -1;
    }

    /*
     * Close/release resources.
     */
    H5Sclose(dataspace);
    H5Tclose(datatype);
    H5Dclose(dataset);
    H5Fclose(file);
    H5Pclose(fapl);
    
    printf("Test finished!\n");
    return 0;
}

#endif
