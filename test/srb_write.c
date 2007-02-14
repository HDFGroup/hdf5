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
 * Purpose:    Test HDF-SRB if it is configured and compiled.  Write an HDF5
 *             file with an integer array to SRB server.
 *
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
 *             Then you can create an HDF5 file by calling H5Fcreate().  When
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
    SRB_Info srb_info={ "ghidorah.sdsc.edu",  /* Using SDSC SRB server,
                                               * don't append port number.   */
                        NULL,                 /* SRB host default port number*/
                        NULL,                 /* SRB Authentication-password,
                                               * using the one in
                                               * ~/.srb/.MdasAuth            */
                        0,                    /* Unix storage system.        */
                        0600,                 /* Read and write only for
                                               * owner                       */
                        -1                    /* -1 is default.              */
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
