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

/*-------------------------------------------------------------------------
 *
 * Created:             debug.c
 *                      Jul 18 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Debugs an existing HDF5 file at a low level.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

#include "H5private.h"
#include "H5Bprivate.h"
#include "H5Dprivate.h"
#include "H5Fpkg.h"
#include "H5Gprivate.h"
#include "H5HGprivate.h"
#include "H5HLprivate.h"
#include "H5Iprivate.h"
#include "H5Opkg.h"
#include "H5Pprivate.h"

/* File drivers */
#include "H5FDfamily.h"

#define INDENT  3
#define VCOL    50


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Usage:       debug FILENAME [OFFSET]
 *
 * Return:      Success:        exit (0)
 *
 *              Failure:        exit (non-zero)
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t	fid, fapl, dxpl;
    H5F_t       *f;
    haddr_t     addr=0, extra=0;
    uint8_t     sig[16];
    int         i;
    unsigned    ndims;
    herr_t      status = SUCCEED;

    if (argc == 1) {
	fprintf(stderr,
		"Usage: %s filename [signature addr [extra]]\n", argv[0]);
	HDexit(1);
    }

    /* Initialize the library */
    if(H5open ()<0) {
        fprintf(stderr, "cannot initialize the library\n");
        HDexit(1);
    }

    /*
     * Open the file and get the file descriptor.
     */
    if((dxpl = H5Pcreate (H5P_DATASET_XFER))<0) {
        fprintf(stderr, "cannot create dataset transfer property list\n");
        HDexit(1);
    }
    if((fapl = H5Pcreate (H5P_FILE_ACCESS))<0) {
        fprintf(stderr, "cannot create file access property list\n");
        HDexit(1);
    }
    if (strchr (argv[1], '%')) {
	H5Pset_fapl_family (fapl, (hsize_t)0, H5P_DEFAULT);
    }
    if ((fid = H5Fopen(argv[1], H5F_ACC_RDONLY, fapl)) < 0) {
        fprintf(stderr, "cannot open file\n");
        HDexit(1);
    }
    if (NULL == (f = H5I_object(fid))) {
        fprintf(stderr, "cannot obtain H5F_t pointer\n");
        HDexit(2);
    }

    /*
     * Parse command arguments.
     */
    if (argc > 2) {
        printf("New address: %s\n", argv[2]);
        addr = HDstrtoll(argv[2], NULL, 0);
    }
    if (argc > 3) {
        extra = HDstrtoll(argv[3], NULL, 0);
    }
    /*
     * Read the signature at the specified file position.
     */
    HDfprintf(stdout, "Reading signature at address %a (rel)\n", addr);
    if (H5F_block_read(f, H5FD_MEM_SUPER, addr, sizeof(sig), dxpl, sig)<0) {
        fprintf(stderr, "cannot read signature\n");
        HDexit(3);
    }
    if (!HDmemcmp(sig, H5F_SIGNATURE, H5F_SIGNATURE_LEN)) {
        /*
         * Debug the boot block.
         */
        status = H5F_debug(f, H5P_DATASET_XFER_DEFAULT, stdout, 0, VCOL);

    } else if (!HDmemcmp(sig, H5HL_MAGIC, H5HL_SIZEOF_MAGIC)) {
        /*
         * Debug a local heap.
         */
        status = H5HL_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL);

    } else if (!HDmemcmp (sig, H5HG_MAGIC, H5HG_SIZEOF_MAGIC)) {
	/*
	 * Debug a global heap collection.
	 */
	status = H5HG_debug (f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL);

    } else if (!HDmemcmp(sig, H5G_NODE_MAGIC, H5G_NODE_SIZEOF_MAGIC)) {
        /*
         * Debug a symbol table node.
         */
        status = H5G_node_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, extra);

    } else if (!HDmemcmp(sig, H5B_MAGIC, H5B_SIZEOF_MAGIC)) {
        /*
         * Debug a B-tree.  B-trees are debugged through the B-tree
         * subclass.  The subclass identifier is the byte immediately
         * after the B-tree signature.
         */
        H5B_subid_t subtype = (H5B_subid_t)sig[H5B_SIZEOF_MAGIC];

        switch (subtype) {
        case H5B_SNODE_ID:
            status = H5G_node_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, extra);
            break;

	case H5B_ISTORE_ID:
	    ndims = (unsigned)extra;
	    status = H5D_istore_debug (f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, ndims);
	    break;

        default:
            fprintf(stderr, "Unknown B-tree subtype %u\n", (unsigned)(subtype));
            HDexit(4);
        }

    } else if (sig[0] == H5O_VERSION) {
        /*
         * This could be an object header.  Since they don't have a signature
         * it's a somewhat "ify" detection.
         */
        status = H5O_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL);

    } else {
        /*
         * Got some other unrecognized signature.
         */
        printf("%-*s ", VCOL, "Signature:");
        for (i = 0; i < 8; i++) {
            if (sig[i] > ' ' && sig[i] <= '~' && '\\' != sig[i]) {
                HDputchar(sig[i]);
            } else if ('\\' == sig[i]) {
                HDputchar('\\');
                HDputchar('\\');
            } else {
                printf("\\%03o", sig[i]);
            }
        }
        HDputchar('\n');

        fprintf(stderr, "unknown signature\n");
        HDexit(4);
    }

    if (status < 0) {
        fprintf(stderr, "An error occurred\n");
        H5Eprint(stderr);
        HDexit(5);
    }
    H5Pclose(dxpl);
    H5Pclose(fapl);
    H5Fclose(fid);
    return 0;
}
