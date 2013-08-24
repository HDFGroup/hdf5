/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

/***********************************************************
*
* Test program: twriteorder
*
* Test to verify that the write order is strictly consistent.
* The SWMR feature requires that the order of write is strictly consistent.
* <<design requirements of SWMR>>
*************************************************************/

/***********************************************************
*
* Algorithm
*
* The test simulates what SWMR does by writing chained blocks and see if
* they can be read back correctly.
* There is a writer process and multiple read processes.
* The file is divided into 2KB partitions. Then writer writes 1 chained
* block, each of 1KB big, in each partition after the first partition.
* Each chained block has this structure:
* Byte 0-7: offset address of its child block. The last child uses 0 as NULL.
* Byte 8-1023: some artificial data.
* The child block address of Block 1 is NULL (0).
* The child block address of Block 2 is the offset address of Block 1.
* The child block address of Block n is the offset address of Block n-1.
* After all n blocks are written, the offset address of Block n is written
* to the offset 0 of the first partition.
* Therefore, by the time the offset address of Block n is written to this
* position, all n chain-linked blocks have been written.
*
* The other reader processes will try to read the address value at the
* offset 0. The value is initially NULL(0). When it changes to non-zero,
* it signifies the writer process has written all the chain-link blocks
* and they are ready for the reader processes to access.
*
* If the system, in which the writer and reader processes run, the readers
* will always get all chain-linked blocks correctly. If the order of write
* is not maintained, some reader processes may found unexpect block data.
*
*************************************************************/

#include "testhdf5.h"

#include "hdf5.h"

#define DATAFILE   "twriteorder.dat"

int
main(int ac, char **av)
{
    if (ac > 0 && ac < 10){	/* just to shut up the warnings */
	printf("This is a dummy of %s with datafile %s\n", *av, DATAFILE);
    }
    return(0);
}
