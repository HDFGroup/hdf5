/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING f.                                                           *
 *                                                                          *
 ****************************************************************************/

#include "H5PT.h"
#include <stdlib.h>

/*-------------------------------------------------------------------------
 * Packet Table Fixed-Length Example
 *
 * Example program that creates a packet table and performs
 * writes and reads.
 *
 *-------------------------------------------------------------------------
 */

int main(void)
{
 hid_t          fid;        /* File identifier */
 hid_t          ptable;     /* Packet table identifier */

 herr_t         err;        /* Function return status */
 hsize_t        count;      /* Number of records in the table */

 int            x;          /* Loop variable */

    /* Buffers to hold data */
 int writeBuffer[5];
 int readBuffer[5];

   /* Initialize buffers */
 for(x=0; x<5; x++)
 {
     writeBuffer[x]=x;
     readBuffer[x] = -1;
 }

    /* Create a file using default properties */
 fid=H5Fcreate("packet_table_FLexample.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);

    /* Create a fixed-length packet table within the file */
    /* This table's "packets" will be simple integers. */
 ptable = H5PTcreate_fl(fid, "Packet Test Dataset", H5T_NATIVE_INT, 1);
 if(ptable == H5I_INVALID_HID)
     goto out;

    /* Write one packet to the packet table */
 err = H5PTappend(ptable, 1, &(writeBuffer[0]) );
 if(err < 0)
     goto out;

    /* Write several packets to the packet table */
 err = H5PTappend(ptable, 4, &(writeBuffer[1]) );
 if(err < 0)
     goto out;

    /* Get the number of packets in the packet table.  This should be five. */
 err = H5PTget_num_packets(ptable, &count);
 if(err < 0)
     goto out;

 printf("Number of packets in packet table after five appends: %d\n", count);

    /* Initialize packet table's "current record" */
 err = H5PTcreate_index(ptable);
 if(err < 0)
     goto out;

    /* Iterate through packets, read each one back */
 for(x=0; x<5; x++)
 {
    err = H5PTget_next(ptable, 1, &(readBuffer[x]) );
    if(err < 0)
	goto out;

    printf("Packet %d's value is %d\n", x, readBuffer[x]);
 }

    /* Close the packet table */
 err = H5PTclose(ptable);
 if(err < 0)
     goto out;

    /* Close the file */
 H5Fclose(fid);

 return 0;

 out: /* An error has occurred.  Clean up and exit. */
    H5PTclose(ptable);
    H5Fclose(fid);
    return -1;
}

