/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
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
#include <H5private.h>
#include <H5Iprivate.h>
#include <H5Bprivate.h>
#include <H5Pprivate.h>
#include <H5Fprivate.h>
#include <H5Gprivate.h>
#include <H5HGprivate.h>
#include <H5HLprivate.h>
#include <H5Oprivate.h>

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
    hid_t                   fid, plist=H5P_DEFAULT;
    H5F_t                  *f;
    haddr_t                 addr;
    uint8                   sig[16];
    intn                    i;
    herr_t                  status = SUCCEED;
    haddr_t                 extra;

    /*
     * Open the file and get the file descriptor.
     */
    if (strchr (argv[1], '%')) {
	plist = H5Pcreate (H5P_FILE_ACCESS);
	H5Pset_family (plist, 0, H5P_DEFAULT);
    }
    if ((fid = H5Fopen(argv[1], H5F_ACC_RDONLY, plist)) < 0) {
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
    H5F_addr_reset(&addr);
    H5F_addr_reset(&extra);
    if (argc > 2) {
        printf("New address: %s\n", argv[2]);
        addr.offset = HDstrtoll(argv[2], NULL, 0);
    }
    if (argc > 3) {
        extra.offset = HDstrtoll(argv[3], NULL, 0);
    }
    /*
     * Read the signature at the specified file position.
     */
    printf("Reading signature at address ");
    H5F_addr_print(stdout, &addr);
    printf(" (rel)\n");
    if (H5F_block_read(f, &addr, (hsize_t)sizeof(sig), sig) < 0) {
        fprintf(stderr, "cannot read signature\n");
        HDexit(3);
    }
    if (!HDmemcmp(sig, H5F_SIGNATURE, H5F_SIGNATURE_LEN)) {
        /*
         * Debug the boot block.
         */
        status = H5F_debug(f, &addr, stdout, 0, VCOL);

    } else if (!HDmemcmp(sig, H5HL_MAGIC, H5HL_SIZEOF_MAGIC)) {
        /*
         * Debug a local heap.
         */
        status = H5HL_debug(f, &addr, stdout, 0, VCOL);
	
    } else if (!HDmemcmp (sig, H5HG_MAGIC, H5HG_SIZEOF_MAGIC)) {
	/*
	 * Debug a global heap collection.
	 */
	status = H5HG_debug (f, &addr, stdout, 0, VCOL);

    } else if (!HDmemcmp(sig, H5G_NODE_MAGIC, H5G_NODE_SIZEOF_MAGIC)) {
        /*
         * Debug a symbol table node.
         */
        status = H5G_node_debug(f, &addr, stdout, 0, VCOL, &extra);

    } else if (!HDmemcmp(sig, H5B_MAGIC, H5B_SIZEOF_MAGIC)) {
        /*
         * Debug a B-tree.  B-trees are debugged through the B-tree
         * subclass.  The subclass identifier is the byte immediately
         * after the B-tree signature.
         */
        H5B_subid_t             subtype = (H5B_subid_t)sig[H5B_SIZEOF_MAGIC];
        switch (subtype) {
        case H5B_SNODE_ID:
            status = H5G_node_debug(f, &addr, stdout, 0, VCOL, &extra);
            break;

        default:
            fprintf(stderr, "Unknown B-tree subtype %u\n", (unsigned) (subtype));
            HDexit(4);
        }

    } else if (sig[0] == H5O_VERSION) {
        /*
         * This could be an object header.  Since they don't have a signature
         * it's a somewhat "ify" detection.
         */
        status = H5O_debug(f, &addr, stdout, 0, VCOL);

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
        HDexit(5);
    }
    H5Fclose(fid);
    return 0;
}
