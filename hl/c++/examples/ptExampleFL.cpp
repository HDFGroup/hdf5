/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5PacketTable.h"

/*-------------------------------------------------------------------------
 * Packet Table Fixed-Length Example
 *
 * Example program that creates a packet table and performs
 * writes and reads.
 *
 *-------------------------------------------------------------------------
 */

const char *FILE_NAME("PTcppexampleFL.h5");
const char *PT_NAME("/examplePacketTable");

int
main(void)
{
    herr_t  err;     /* Return value from function calls */
    hid_t   fileID;  /* HDF5 identifier for file */
    hid_t   plistID; /* HDF5 identifier for property list to use compression */
    hsize_t count;   /* Number of records in table */
    int     x;       /* Temporary counter variable */

    /* Buffers to hold data */
    int readBuffer[5];
    int writeBuffer[5];

    /* Initialize buffers */
    for (x = 0; x < 5; x++) {
        writeBuffer[x] = x;
        readBuffer[x]  = -1;
    }

    /* Create a new HDF5 file */
    fileID = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fileID < 0)
        fprintf(stderr, "Couldn't create file.\n");

    /* Prepare property list to set compression, randomly use deflate
       with compression level 5. */
    plistID = H5Pcreate(H5P_DATASET_CREATE);
    err     = H5Pset_deflate(plistID, 5);
    if (err < 0)
        fprintf(stderr, "Error setting compression level.");

    /* Create a fixed-length packet table. */
    FL_PacketTable ptable(fileID, plistID, PT_NAME, H5T_NATIVE_INT, 100);
    if (!ptable.IsValid())
        fprintf(stderr, "Unable to create packet table.");

    /* Append five packets to the packet table, one at a time */
    for (x = 0; x < 5; x++) {
        err = ptable.AppendPacket(&(writeBuffer[x]));
        if (err < 0)
            fprintf(stderr, "Error adding record.");
    }

    /* Get the number of packets in the packet table.  This should be five. */
    count = ptable.GetPacketCount(err);
    if (err < 0)
        fprintf(stderr, "Error getting packet count.");

    printf("Number of packets in packet table after five appends: %" PRIuHSIZE "\n", count);

    /* Initialize packet table's "current record" */
    ptable.ResetIndex();

    /* Iterate through packets, read each one back */
    for (x = 0; x < 5; x++) {
        err = ptable.GetNextPacket(&(readBuffer[x]));
        if (err < 0)
            fprintf(stderr, "Error reading record.");

        printf("Packet %d's value is %d.\n", x, readBuffer[x]);
    }

    /* The packet table will close automatically when its object goes */
    /* out of scope.  */

    err = H5Fclose(fileID);
    if (err < 0)
        fprintf(stderr, "Failed to close file.\n");

    return 0;
}
