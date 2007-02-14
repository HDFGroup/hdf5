/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer: Raymond Lu <slu@ncsa.uiuc.edu>
 *             April 14, 2000
 *
 * Purpose:    Test HDF-SRB if it is configured and compiled.  Read a chunk of
 *             dataset of an HDF5 file with an integer array on SRB server,
 *             using hyperslab.
 * Usage:      The HDF5 is built on the top of SRB.  So you just need to
 *             activate a connection to SRB server by calling function
 *             H5Pset_fapl_srb()(after creating file property list by calling
 *             H5Pcreate()).  All server information is passed in through its
 *             parameter SRB_Info(a structure).  Its fields are
 *                 char *srbHost: SRB host address of server.  If the input is
 *                     NULL, it will use the environment varible "srbHost" if
 *                     it is defined.  If it is not defined, the
 *                     ~/.srb/.MdasEnd file will be checked next.  If not, it
 *                     will use the hostname of the client machine.
 *                 char *srbPort: SRB host port number of server.  If the
 *                     input value is NULL, it will use the env variable
 *                     "srbPort" if it is defined.  If not, it defaults to
 *                     5558.
 *                 char *srbAuth: SRB Authentication-password.  It is used to
 *                     define password for MDAS or SEA authentication.
 *                     For SEA authentication, this is the password used by
 *                     the SEA library to decrypt the encrypted private key
 *                     stored in the file ~/.SEAuuuuu@ddddd(where uuuuu is
 *                     the user ID and ddddd is the user domain name).  This
 *                     input is not needed if an unencrypted private key is
 *                     available in the /tmp directory(generated using the
 *                     'seaauth auto' command).  To provide additional
 *                     flexibility, a client may also use the environment
 *                     variable "srbAuth" to specify the password.  A client
 *                     may also supply the password in the ~/.srb/.MdasAuth
 *                     file.  If a client uses more than one method to specfy
 *                     the password, the value given in this function call
 *                     will take precedent, then the environment variable
 *                     "srbAuth", and lastly, the ~/.srb/.MdasAuth file.
 *                 int  storSysType: Storage system type on SRB server.
 *                     0=Unix, 1=UniTree, 2=HPSS, 3=FTP, 4=HTTP
 *                 int  mode: File access mode, same definition with Unix.
 *                 int  size: File Size, only valid for HPSS, -1 is default
 *
 *             Then you can open an HDF5 file by calling H5Fopen().  When
 *             you pass in the file name, it has to be file name with an
 *             absolute path.  It you use SDSC server(ghidorah.sdsc.edu), your
 *             home directory is possibly in /projects/mdas/srb/SRBVault/.
 */
#include "h5test.h"

#ifndef H5_HAVE_SRB
int main(void)
{
    printf("Test skipped because SRB driver not available\n");
    return 0;
}
#else

#define fileName "/projects/mdas/srb/SRBVault/slu.ncsa/a.h5"
#define DATASETNAME "IntArray"
#define NX_SUB  3           /* hyperslab dimensions */
#define NY_SUB  4
#define NX 7           /* output buffer dimensions */
#define NY 7
#define NZ  3
#define RANK         2
#define RANK_OUT     3

int main(void)
{
    hid_t       fapl=-1, fid = -1, dataset;
    hid_t       datatype, dataspace;
    hid_t       memspace;
    H5T_class_t class;                 /* data type class */
    H5T_order_t order;                 /* data order */
    size_t      size;                  /*
				        * size of the data element
				        * stored in file
				        */
    hsize_t     dimsm[3];              /* memory space dimensions */
    hsize_t     dims_out[2];           /* dataset dimensions */
    herr_t      status;

    int         data_out[NX][NY][NZ ]; /* output buffer */

    hsize_t      count[2];              /* size of the hyperslab in the file */
    hssize_t     offset[2];             /* hyperslab offset in the file */
    hsize_t      count_out[3];          /* size of the hyperslab in memory */
    hssize_t     offset_out[3];         /* hyperslab offset in memory */
    int          i, j, k, status_n, rank;
    SRB_Info srb_info={ NULL,  /* Use host name in ~/.srb/.MdasEnv           */
                        NULL,  /* Use environment variable "srbPort"         */
                        NULL,  /* Use password in  ~/.srb/.MdasEnv           */
                        0,     /* Use Unix storage system.                   */
                        0600,  /* File was created for read and write for
                                * owner                                      */
                        -1     /* default                                    */
                      };

    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++) {
	    for (k = 0; k < NZ ; k++)
		data_out[j][i][k] = 0;
	}
    }

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

    fid = H5Fopen(fileName, H5F_ACC_RDONLY, fapl);
    if (fid < 0) {
        printf ("H5Fopen failed. \n");
        return -1;
    }
    dataset = H5Dopen(fid, DATASETNAME);
    if(dataset<0) {
        printf ("H5Dopen failed. \n");
        return -1;
    }
    /*
     * Get datatype and dataspace handles and then query
     * dataset class, order, size, rank and dimensions.
     */
    datatype  = H5Dget_type(dataset);     /* datatype handle */
    class     = H5Tget_class(datatype);
    if (class == H5T_INTEGER) printf("Data set has INTEGER type \n");
    order     = H5Tget_order(datatype);
    if (order == H5T_ORDER_LE) printf("Little endian order \n");

    size  = H5Tget_size(datatype);
    printf(" Data size is %d \n", size);

    dataspace = H5Dget_space(dataset);    /* dataspace handle */
    rank      = H5Sget_simple_extent_ndims(dataspace);
    status_n  = H5Sget_simple_extent_dims(dataspace, dims_out, NULL);
    printf("rank %d, dimensions %lu x %lu \n", rank,
	   (unsigned long)(dims_out[0]), (unsigned long)(dims_out[1]));

    /*
     * Define hyperslab in the dataset.
     */
    offset[0] = 1;
    offset[1] = 2;
    count[0]  = NX_SUB;
    count[1]  = NY_SUB;
    status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL,
				 count, NULL);

    /*
     * Define the memory dataspace.
     */
    dimsm[0] = NX;
    dimsm[1] = NY;
    dimsm[2] = NZ ;
    memspace = H5Screate_simple(RANK_OUT,dimsm,NULL);

    /*
     * Define memory hyperslab.
     */
    offset_out[0] = 3;
    offset_out[1] = 0;
    offset_out[2] = 0;
    count_out[0]  = NX_SUB;
    count_out[1]  = NY_SUB;
    count_out[2]  = 1;
    status = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset_out, NULL,
				 count_out, NULL);

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    status = H5Dread(dataset, H5T_NATIVE_INT, memspace, dataspace,
		     H5P_DEFAULT, data_out);
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++) printf("%d ", data_out[j][i][0]);
	printf("\n");
    }
    /*
     * 0 0 0 0 0 0 0
     * 0 0 0 0 0 0 0
     * 0 0 0 0 0 0 0
     * 3 4 5 6 0 0 0
     * 4 5 6 7 0 0 0
     * 5 6 7 8 0 0 0
     * 0 0 0 0 0 0 0
     */

    /*
     * Close/release resources.
     */
    H5Tclose(datatype);
    H5Dclose(dataset);
    H5Sclose(dataspace);
    H5Sclose(memspace);
    H5Fclose(fid);
    H5Pclose(fapl);

    printf("Test finished!\n");
    return 0;
}

#endif
