#include <h5test.h>

#ifndef H5_HAVE_SRB
int main(void)
{
    printf("Test skipped because SRB driver not available\n");
    return 0;
}
#else

#define fileName "/projects/mdas/srb/SRBVault/slu.ncsa/a.h5"
#define DATASETNAME "IntArray"
#define NX     5                      /* dataset dimensions */
#define NY     6
#define RANK   2

int main(void)
{
    hid_t fapl=-1, fid = -1;     
    hid_t         dataspace, datatype, dataset;
    hsize_t       dimsf[2];
    herr_t        status = 0;
    int           data[NX][NY];          /* data to write */
    int           i, j;
    SRB_Info srb_info={ "ghidorah.sdsc.edu",  /* SRB host address of server  */
                        "5557",               /* SRB host port number        */
                        NULL,                 /* SRB Authentication-password */
                        0,                    /* Storage Type: 0=Unix, 
                                               *                1=UniTree,
                                               *                2=HPSS,
                                               *                3=FTP,
                                               *                4=HTTP       */
                        0600,                 /* File mode-Unix access mode  */
                        -1                    /* File Size-Only valid for HPSS,
                                               *  -1 is default.             */
                      };

    /* 
     * Data  and output buffer initialization. 
     */
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
   
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    if (fapl < 0) {
        printf (" H5Pcreate failed. \n");
        return -1;
    }
 
    status = H5Pset_fapl_srb(fapl, srb_info);
    if (status < 0) {
        printf ("H5Pset_fapl_srb failed. \n");
        return -1;
    }
    
    fid = H5Fcreate(fileName, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    /*fid = H5Fcreate(fileName, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);*/
    if (fid < 0) {
        printf ("H5Fcreate failed. \n");
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
    dataset = H5Dcreate(fid, DATASETNAME, datatype, dataspace,
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

    H5Sclose(dataspace);
    H5Tclose(datatype);
    H5Dclose(dataset);
    H5Fclose(fid);
    H5Pclose(fapl);

    printf("Test finished!\n");
    return 0;
}

#endif
