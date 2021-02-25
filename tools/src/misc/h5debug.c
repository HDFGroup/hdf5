/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             debug.c
 *                      Jul 18 1997
 *                      Robb Matzke
 *
 * Purpose:             Debugs an existing HDF5 file at a low level.
 *
 *-------------------------------------------------------------------------
 */
#define H5A_FRIEND   /*suppress error about including H5Apkg  */
#define H5B2_FRIEND  /*suppress error about including H5B2pkg */
#define H5B2_TESTING /*suppress warning about H5B2 testing funcs*/
#define H5D_FRIEND   /*suppress error about including H5Dpkg  */
#define H5EA_FRIEND  /*suppress error about including H5EApkg */
#define H5EA_TESTING /*suppress warning about H5EA testing funcs*/
#define H5FA_FRIEND  /*suppress error about including H5FApkg */
#define H5FA_TESTING /*suppress warning about H5FA testing funcs*/
#define H5F_FRIEND   /*suppress error about including H5Fpkg  */
#define H5G_FRIEND   /*suppress error about including H5Gpkg  */
#define H5HF_FRIEND  /*suppress error about including H5HFpkg */
#define H5O_FRIEND   /*suppress error about including H5Opkg  */
#define H5SM_FRIEND  /*suppress error about including H5SMpkg */

#include "H5private.h"   /* Generic Functions    */
#include "H5Apkg.h"      /* Attributes           */
#include "H5B2pkg.h"     /* v2 B-trees           */
#include "H5CXprivate.h" /* API Contexts        */
#include "H5Dpkg.h"      /* Datasets             */
#include "H5Eprivate.h"  /* Error handling       */
#include "H5EApkg.h"     /* Extensible Arrays    */
#include "H5FApkg.h"     /* Fixed Arrays         */
#include "H5Fpkg.h"      /* File access          */
#include "H5FSprivate.h" /* Free space manager  */
#include "H5Gpkg.h"      /* Groups               */
#include "H5HFpkg.h"     /* Fractal heaps        */
#include "H5HGprivate.h" /* Global Heaps        */
#include "H5Iprivate.h"  /* IDs                  */
#include "H5Opkg.h"      /* Object headers       */
#include "H5SMpkg.h"     /* Implicitly shared messages    */

/* File drivers */
#include "H5FDfamily.h"

#define VCOL 50

/*-------------------------------------------------------------------------
 * Function:    get_H5B2_class
 *
 * Purpose:  Determine the v2 B-tree class from the buffer read in.
 *              B-trees are debugged through the B-tree subclass.  The subclass
 *              identifier is two bytes after the B-tree signature.
 *
 * Return:  Non-NULL on success/NULL on failure
 *
 * Programmer:  Quincey Koziol
 *    Sep 11 2008
 *
 *-------------------------------------------------------------------------
 */
static const H5B2_class_t *
get_H5B2_class(const uint8_t *sig)
{
    H5B2_subid_t        subtype = (H5B2_subid_t)sig[H5_SIZEOF_MAGIC + 1];
    const H5B2_class_t *cls     = NULL;

    switch (subtype) {
        case H5B2_TEST_ID:
            cls = H5B2_TEST;
            break;

        case H5B2_FHEAP_HUGE_INDIR_ID:
            cls = H5HF_HUGE_BT2_INDIR;
            break;

        case H5B2_FHEAP_HUGE_FILT_INDIR_ID:
            cls = H5HF_HUGE_BT2_FILT_INDIR;
            break;

        case H5B2_FHEAP_HUGE_DIR_ID:
            cls = H5HF_HUGE_BT2_DIR;
            break;

        case H5B2_FHEAP_HUGE_FILT_DIR_ID:
            cls = H5HF_HUGE_BT2_FILT_DIR;
            break;

        case H5B2_GRP_DENSE_NAME_ID:
            cls = H5G_BT2_NAME;
            break;

        case H5B2_GRP_DENSE_CORDER_ID:
            cls = H5G_BT2_CORDER;
            break;

        case H5B2_SOHM_INDEX_ID:
            cls = H5SM_INDEX;
            break;

        case H5B2_ATTR_DENSE_NAME_ID:
            cls = H5A_BT2_NAME;
            break;

        case H5B2_ATTR_DENSE_CORDER_ID:
            cls = H5A_BT2_CORDER;
            break;

        case H5B2_CDSET_ID:
            cls = H5D_BT2;
            break;

        case H5B2_CDSET_FILT_ID:
            cls = H5D_BT2_FILT;
            break;

        case H5B2_TEST2_ID:
            cls = H5B2_TEST2;
            break;

        case H5B2_NUM_BTREE_ID:
        default:
            HDfprintf(stderr, "Unknown v2 B-tree subtype %u\n", (unsigned)(subtype));
    } /* end switch */

    return (cls);
} /* end get_H5B2_class() */

/*-------------------------------------------------------------------------
 * Function:    get_H5EA_class
 *
 * Purpose:  Determine the extensible array class from the buffer read in.
 *              Extensible arrays are debugged through the array subclass.
 *              The subclass identifier is two bytes after the signature.
 *
 * Return:  Non-NULL on success/NULL on failure
 *
 * Programmer:  Quincey Koziol
 *    Sep 11 2008
 *
 *-------------------------------------------------------------------------
 */
static const H5EA_class_t *
get_H5EA_class(const uint8_t *sig)
{
    H5EA_cls_id_t       clsid = (H5EA_cls_id_t)sig[H5_SIZEOF_MAGIC + 1];
    const H5EA_class_t *cls   = NULL;

    switch (clsid) {
        case H5EA_CLS_TEST_ID:
            cls = H5EA_CLS_TEST;
            break;

        case H5EA_CLS_CHUNK_ID:
            cls = H5EA_CLS_CHUNK;
            break;

        case H5EA_CLS_FILT_CHUNK_ID:
            cls = H5EA_CLS_FILT_CHUNK;
            break;

        case H5EA_NUM_CLS_ID:
        default:
            HDfprintf(stderr, "Unknown extensible array class %u\n", (unsigned)(clsid));
    } /* end switch */

    return (cls);
} /* end get_H5EA_class() */

/*-------------------------------------------------------------------------
 * Function:    get_H5FA_class
 *
 * Purpose:  Determine the fixed array class from the buffer read in.
 *              Extensible arrays are debugged through the array subclass.
 *              The subclass identifier is two bytes after the signature.
 *
 * Return:  Non-NULL on success/NULL on failure
 *
 * Programmer:  Quincey Koziol
 *    Sep 11 2008
 *
 *-------------------------------------------------------------------------
 */
static const H5FA_class_t *
get_H5FA_class(const uint8_t *sig)
{
    H5FA_cls_id_t       clsid = (H5FA_cls_id_t)sig[H5_SIZEOF_MAGIC + 1];
    const H5FA_class_t *cls   = NULL;

    switch (clsid) {
        case H5FA_CLS_TEST_ID:
            cls = H5FA_CLS_TEST;
            break;

        case H5FA_CLS_CHUNK_ID:
            cls = H5FA_CLS_CHUNK;
            break;

        case H5FA_CLS_FILT_CHUNK_ID:
            cls = H5FA_CLS_FILT_CHUNK;
            break;

        case H5FA_NUM_CLS_ID:
        default:
            HDfprintf(stderr, "Unknown fixed array class %u\n", (unsigned)(clsid));
    } /* end switch */

    return (cls);
} /* end get_H5FA_class() */

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
 *              Jul 18 1997
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t          fid  = H5I_INVALID_HID;
    hid_t          fapl = H5I_INVALID_HID;
    H5VL_object_t *vol_obj;
    H5F_t *        f;
    haddr_t        addr        = 0;
    int            extra_count = 0; /* Number of extra arguments */
    haddr_t        extra[10];
    uint8_t        sig[H5F_SIGNATURE_LEN];
    size_t         u;
    H5E_auto2_t    func           = NULL;
    void *         edata          = NULL;
    hbool_t        api_ctx_pushed = FALSE; /* Whether API context pushed */
    herr_t         status         = SUCCEED;
    int            exit_value     = 0;

    if (argc == 1) {
        HDfprintf(stderr, "Usage: %s filename [signature-addr [extra]*]\n", argv[0]);
        exit_value = 1;
        goto done;
    } /* end if */

    /* Initialize the library */
    if (H5open() < 0) {
        HDfprintf(stderr, "cannot initialize the library\n");
        exit_value = 1;
        goto done;
    } /* end if */

    /* Disable error reporting */
    H5Eget_auto2(H5E_DEFAULT, &func, &edata);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /*
     * Open the file and get the file descriptor.
     */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        HDfprintf(stderr, "cannot create file access property list\n");
        exit_value = 1;
        goto done;
    } /* end if */
    if (HDstrchr(argv[1], '%'))
        if (H5Pset_fapl_family(fapl, (hsize_t)0, H5P_DEFAULT) < 0) {
            HDfprintf(stderr, "cannot set file access property list\n");
            exit_value = 1;
            goto done;
        }
    if ((fid = H5Fopen(argv[1], H5F_ACC_RDONLY, fapl)) < 0) {
        HDfprintf(stderr, "cannot open file\n");
        exit_value = 1;
        goto done;
    } /* end if */

    /* Push API context */
    if (H5CX_push() < 0) {
        HDfprintf(stderr, "cannot set API context\n");
        exit_value = 1;
        goto done;
    }
    api_ctx_pushed = TRUE;

    if (NULL == (vol_obj = (H5VL_object_t *)H5VL_vol_object(fid))) {
        HDfprintf(stderr, "cannot obtain vol_obj pointer\n");
        exit_value = 2;
        goto done;
    } /* end if */

    if (NULL == (f = (H5F_t *)H5VL_object_data(vol_obj))) {
        HDfprintf(stderr, "cannot obtain H5F_t pointer\n");
        exit_value = 2;
        goto done;
    } /* end if */

    /* Ignore metadata tags while using h5debug */
    if (H5AC_ignore_tags(f) < 0) {
        HDfprintf(stderr, "cannot ignore metadata tags\n");
        exit_value = 1;
        goto done;
    }

    /*
     * Parse command arguments.
     */

    /* Primary data structure to dump */
    if (argc > 2)
        addr = (haddr_t)HDstrtoll(argv[2], NULL, 0);

    /* Extra arguments for primary data structure */
    HDmemset(extra, 0, sizeof(extra));
    if (argc > 3) {
        /* Number of extra arguments */
        extra_count = argc - 3;

        /* Range check against 'extra' array size */
        if (extra_count > (int)(sizeof(extra) / sizeof(haddr_t))) {
            HDfprintf(stderr, "\nWARNING: Only using first %d extra parameters\n\n",
                      (int)(sizeof(extra) / sizeof(haddr_t)));
            extra_count = (int)(sizeof(extra) / sizeof(haddr_t));
        } /* end if */

        for (u = 0; u < (size_t)extra_count; u++)
            extra[u] = (haddr_t)HDstrtoll(argv[u + 3], NULL, 0);
    } /* end if */

    /*
     * Read the signature at the specified file position.
     */
    HDfprintf(stdout, "Reading signature at address %" PRIuHADDR " (rel)\n", addr);
    if (H5F_block_read(f, H5FD_MEM_SUPER, addr, sizeof(sig), sig) < 0) {
        HDfprintf(stderr, "cannot read signature\n");
        exit_value = 3;
        goto done;
    }
    if (!HDmemcmp(sig, H5F_SIGNATURE, (size_t)H5F_SIGNATURE_LEN)) {
        /*
         * Debug the file's super block.
         */
        status = H5F_debug(f, stdout, 0, VCOL);
    }
    else if (!HDmemcmp(sig, H5HL_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a local heap.
         */
        status = H5HL_debug(f, addr, stdout, 0, VCOL);
    }
    else if (!HDmemcmp(sig, H5HG_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a global heap collection.
         */
        status = H5HG_debug(f, addr, stdout, 0, VCOL);
    }
    else if (!HDmemcmp(sig, H5G_NODE_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a symbol table node.
         */

        /* Check for extra parameters */
        if (extra_count == 0 || extra[0] == 0) {
            HDfprintf(stderr,
                      "\nWarning: Providing the group's local heap address will give more information\n");
            HDfprintf(stderr, "Symbol table node usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <Symbol table node address> <address of local heap>\n\n");
        } /* end if */

        status = H5G_node_debug(f, addr, stdout, 0, VCOL, extra[0]);
    }
    else if (!HDmemcmp(sig, H5B_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a B-tree.  B-trees are debugged through the B-tree
         * subclass.  The subclass identifier is the byte immediately
         * after the B-tree signature.
         */
        H5B_subid_t subtype = (H5B_subid_t)sig[H5_SIZEOF_MAGIC];
        unsigned    ndims;
        uint32_t    dim[H5O_LAYOUT_NDIMS];

        switch (subtype) {
            case H5B_SNODE_ID:
                /* Check for extra parameters */
                if (extra_count == 0 || extra[0] == 0) {
                    HDfprintf(
                        stderr,
                        "\nWarning: Providing the group's local heap address will give more information\n");
                    HDfprintf(stderr, "B-tree symbol table node usage:\n");
                    HDfprintf(stderr,
                              "\th5debug <filename> <B-tree node address> <address of local heap>\n\n");
                    exit_value = 4;
                    goto done;
                } /* end if */

                status = H5G_node_debug(f, addr, stdout, 0, VCOL, extra[0]);
                break;

            case H5B_CHUNK_ID:
                /* Check for extra parameters */
                if (extra_count == 0 || extra[0] == 0) {
                    HDfprintf(
                        stderr,
                        "ERROR: Need number of dimensions of chunk in order to dump chunk B-tree node\n");
                    HDfprintf(stderr, "B-tree chunked storage node usage:\n");
                    HDfprintf(stderr, "\th5debug <filename> <B-tree node address> <# of dimensions> <slowest "
                                      "chunk dim>...<fastest chunk dim>\n");
                    exit_value = 4;
                    goto done;
                } /* end if */

                /* Set # of dimensions */
                ndims = (unsigned)extra[0];

                /* Check for dimension error */
                if (ndims > 9) {
                    HDfprintf(stderr, "ERROR: Only 9 dimensions support currently (fix h5debug)\n");
                    HDfprintf(stderr, "B-tree chunked storage node usage:\n");
                    HDfprintf(stderr, "\th5debug <filename> <B-tree node address> <# of dimensions> <slowest "
                                      "chunk dim>...<fastest chunk dim>\n");
                    exit_value = 4;
                    goto done;
                } /* end for */

                /* Build array of chunk dimensions */
                for (u = 0; u < ndims; u++)
                    dim[u] = (uint32_t)extra[u + 1];

                /* Check for dimension error */
                for (u = 0; u < ndims; u++)
                    if (0 == dim[u]) {
                        HDfprintf(stderr, "ERROR: Chunk dimensions should be >0\n");
                        HDfprintf(stderr, "B-tree chunked storage node usage:\n");
                        HDfprintf(stderr, "\th5debug <filename> <B-tree node address> <# of dimensions> "
                                          "<slowest chunk dim>...<fastest chunk dim>\n");
                        exit_value = 4;
                        goto done;
                    } /* end if */

                /* Set the last dimension (the element size) to zero */
                dim[ndims] = 0;

                status = H5D_btree_debug(f, addr, stdout, 0, VCOL, ndims, dim);
                break;

            case H5B_NUM_BTREE_ID:
            default:
                HDfprintf(stderr, "Unknown v1 B-tree subtype %u\n", (unsigned)(subtype));
                exit_value = 4;
                goto done;
        }
    }
    else if (!HDmemcmp(sig, H5B2_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a v2 B-tree header.
         */
        const H5B2_class_t *cls = get_H5B2_class(sig);
        HDassert(cls);

        if ((cls == H5D_BT2 || cls == H5D_BT2_FILT) && (extra_count == 0 || extra[0] == 0)) {
            HDfprintf(stderr, "ERROR: Need v2 B-tree header address and object header address containing the "
                              "layout message in order to dump header\n");
            HDfprintf(stderr, "v2 B-tree hdr usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <v2 B-tree header address> <object header address>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5B2__hdr_debug(f, addr, stdout, 0, VCOL, cls, (haddr_t)extra[0]);
    }
    else if (!HDmemcmp(sig, H5B2_INT_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a v2 B-tree internal node.
         */
        const H5B2_class_t *cls = get_H5B2_class(sig);
        HDassert(cls);

        /* Check for enough valid parameters */
        if ((cls == H5D_BT2 || cls == H5D_BT2_FILT) &&
            (extra_count == 0 || extra[0] == 0 || extra[1] == 0 || extra[2] == 0 || extra[3] == 0)) {
            HDfprintf(stderr,
                      "ERROR: Need v2 B-tree header address, the node's number of records, depth, and object "
                      "header address containing the layout message in order to dump internal node\n");
            HDfprintf(stderr,
                      "NOTE: Leaf nodes are depth 0, the internal nodes above them are depth 1, etc.\n");
            HDfprintf(stderr, "v2 B-tree internal node usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <internal node address> <v2 B-tree header address> "
                              "<number of records> <depth> <object header address>\n");
            exit_value = 4;
            goto done;
        }
        else if (extra_count == 0 || extra[0] == 0 || extra[1] == 0 || extra[2] == 0) {
            HDfprintf(stderr, "ERROR: Need v2 B-tree header address and the node's number of records and "
                              "depth in order to dump internal node\n");
            HDfprintf(stderr,
                      "NOTE: Leaf nodes are depth 0, the internal nodes above them are depth 1, etc.\n");
            HDfprintf(stderr, "v2 B-tree internal node usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <internal node address> <v2 B-tree header address> "
                              "<number of records> <depth>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5B2__int_debug(f, addr, stdout, 0, VCOL, cls, extra[0], (unsigned)extra[1],
                                 (unsigned)extra[2], (haddr_t)extra[3]);
    }
    else if (!HDmemcmp(sig, H5B2_LEAF_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a v2 B-tree leaf node.
         */
        const H5B2_class_t *cls = get_H5B2_class(sig);
        HDassert(cls);

        /* Check for enough valid parameters */
        if ((cls == H5D_BT2 || cls == H5D_BT2_FILT) &&
            (extra_count == 0 || extra[0] == 0 || extra[1] == 0 || extra[2] == 0)) {

            HDfprintf(stderr, "ERROR: Need v2 B-tree header address, number of records, and object header "
                              "address containing the layout message in order to dump leaf node\n");
            HDfprintf(stderr, "v2 B-tree leaf node usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <leaf node address> <v2 B-tree header address> <number "
                              "of records> <object header address>\n");
            exit_value = 4;
            goto done;
        }
        else if (extra_count == 0 || extra[0] == 0 || extra[1] == 0) {
            HDfprintf(
                stderr,
                "ERROR: Need v2 B-tree header address and number of records in order to dump leaf node\n");
            HDfprintf(stderr, "v2 B-tree leaf node usage:\n");
            HDfprintf(
                stderr,
                "\th5debug <filename> <leaf node address> <v2 B-tree header address> <number of records>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status =
            H5B2__leaf_debug(f, addr, stdout, 0, VCOL, cls, extra[0], (unsigned)extra[1], (haddr_t)extra[2]);
    }
    else if (!HDmemcmp(sig, H5HF_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a fractal heap header.
         */
        status = H5HF_hdr_debug(f, addr, stdout, 0, VCOL);
    }
    else if (!HDmemcmp(sig, H5HF_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a fractal heap direct block.
         */

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0 || extra[1] == 0) {
            HDfprintf(stderr, "ERROR: Need fractal heap header address and size of direct block in order to "
                              "dump direct block\n");
            HDfprintf(stderr, "Fractal heap direct block usage:\n");
            HDfprintf(
                stderr,
                "\th5debug <filename> <direct block address> <heap header address> <size of direct block>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5HF_dblock_debug(f, addr, stdout, 0, VCOL, extra[0], (size_t)extra[1]);
    }
    else if (!HDmemcmp(sig, H5HF_IBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a fractal heap indirect block.
         */

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0 || extra[1] == 0) {
            HDfprintf(stderr, "ERROR: Need fractal heap header address and number of rows in order to dump "
                              "indirect block\n");
            HDfprintf(stderr, "Fractal heap indirect block usage:\n");
            HDfprintf(
                stderr,
                "\th5debug <filename> <indirect block address> <heap header address> <number of rows>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5HF_iblock_debug(f, addr, stdout, 0, VCOL, extra[0], (unsigned)extra[1]);
    }
    else if (!HDmemcmp(sig, H5FS_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a free space header.
         */

        status = H5FS_debug(f, addr, stdout, 0, VCOL);
    }
    else if (!HDmemcmp(sig, H5FS_SINFO_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug free space serialized sections.
         */

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0 || extra[1] == 0) {
            HDfprintf(stderr, "ERROR: Need free space header address and client address in order to dump "
                              "serialized sections\n");
            HDfprintf(stderr, "Free space serialized sections usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <serialized sections address> <free space header "
                              "address> <client address>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5FS_sects_debug(f, addr, stdout, 0, VCOL, extra[0], extra[1]);
    }
    else if (!HDmemcmp(sig, H5SM_TABLE_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug shared message master table.
         */

        status = H5SM_table_debug(f, addr, stdout, 0, VCOL, (unsigned)UFAIL, (unsigned)UFAIL);
    }
    else if (!HDmemcmp(sig, H5SM_LIST_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug shared message list index.
         */

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0) {
            HDfprintf(stderr, "ERROR: Need shared message header address in order to shared message list\n");
            HDfprintf(stderr, "Shared message list usage:\n");
            HDfprintf(stderr,
                      "\th5debug <filename> <shared message list address> <shared message header address>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5SM_list_debug(f, addr, stdout, 0, VCOL, (haddr_t)extra[0]);
    }
    else if (!HDmemcmp(sig, H5EA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug an extensible aray header.
         */
        const H5EA_class_t *cls = get_H5EA_class(sig);
        HDassert(cls);

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0) {
            HDfprintf(
                stderr,
                "ERROR: Need object header address containing the layout message in order to dump header\n");
            HDfprintf(stderr, "Extensible array header block usage:\n");
            HDfprintf(stderr,
                      "\th5debug <filename> <Extensible Array header address> <object header address>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5EA__hdr_debug(f, addr, stdout, 0, VCOL, cls, extra[0]);
    }
    else if (!HDmemcmp(sig, H5EA_IBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug an extensible aray index block.
         */
        const H5EA_class_t *cls = get_H5EA_class(sig);
        HDassert(cls);

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0 || extra[1] == 0) {
            HDfprintf(stderr, "ERROR: Need extensible array header address and object header address "
                              "containing the layout message in order to dump index block\n");
            HDfprintf(stderr, "Extensible array index block usage:\n");
            HDfprintf(
                stderr,
                "\th5debug <filename> <index block address> <array header address> <object header address\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5EA__iblock_debug(f, addr, stdout, 0, VCOL, cls, extra[0], extra[1]);
    }
    else if (!HDmemcmp(sig, H5EA_SBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug an extensible aray super block.
         */
        const H5EA_class_t *cls = get_H5EA_class(sig);
        HDassert(cls);

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0 || extra[1] == 0 || extra[2] == 0) {
            HDfprintf(stderr, "ERROR: Need extensible array header address, super block index and object "
                              "header address containing the layout message in order to dump super block\n");
            HDfprintf(stderr, "Extensible array super block usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <super block address> <array header address> <super "
                              "block index> <object header address>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5EA__sblock_debug(f, addr, stdout, 0, VCOL, cls, extra[0], (unsigned)extra[1], extra[2]);
    }
    else if (!HDmemcmp(sig, H5EA_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug an extensible aray data block.
         */
        const H5EA_class_t *cls = get_H5EA_class(sig);
        HDassert(cls);

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0 || extra[1] == 0 || extra[2] == 0) {
            HDfprintf(stderr,
                      "ERROR: Need extensible array header address, # of elements in data block and object "
                      "header address containing the layout message in order to dump data block\n");
            HDfprintf(stderr, "Extensible array data block usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <data block address> <array header address> <# of "
                              "elements in data block> <object header address\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5EA__dblock_debug(f, addr, stdout, 0, VCOL, cls, extra[0], (size_t)extra[1], extra[2]);
    }
    else if (!HDmemcmp(sig, H5FA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a fixed array header.
         */
        const H5FA_class_t *cls = get_H5FA_class(sig);
        HDassert(cls);

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0) {
            HDfprintf(
                stderr,
                "ERROR: Need object header address containing the layout message in order to dump header\n");
            HDfprintf(stderr, "Fixed array header block usage:\n");
            HDfprintf(stderr, "\th5debug <filename> <Fixed Array header address> <object header address>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5FA__hdr_debug(f, addr, stdout, 0, VCOL, cls, extra[0]);
    }
    else if (!HDmemcmp(sig, H5FA_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug a fixed array data block.
         */
        const H5FA_class_t *cls = get_H5FA_class(sig);
        HDassert(cls);

        /* Check for enough valid parameters */
        if (extra_count == 0 || extra[0] == 0 || extra[1] == 0) {
            HDfprintf(stderr, "ERROR: Need fixed array header address and object header address containing "
                              "the layout message in order to dump data block\n");
            HDfprintf(stderr, "fixed array data block usage:\n");
            HDfprintf(
                stderr,
                "\th5debug <filename> <data block address> <array header address> <object header address>\n");
            exit_value = 4;
            goto done;
        } /* end if */

        status = H5FA__dblock_debug(f, addr, stdout, 0, VCOL, cls, extra[0], extra[1]);
    }
    else if (!HDmemcmp(sig, H5O_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /*
         * Debug v2 object header (which have signatures).
         */

        status = H5O_debug(f, addr, stdout, 0, VCOL);
    }
    else if (sig[0] == H5O_VERSION_1) {
        /*
         * This could be a v1 object header.  Since they don't have a signature
         * it's a somewhat "ify" detection.
         */
        status = H5O_debug(f, addr, stdout, 0, VCOL);
    }
    else {
        /*
         * Got some other unrecognized signature.
         */
        HDprintf("%-*s ", VCOL, "Signature:");
        for (u = 0; u < sizeof(sig); u++) {
            if (sig[u] > ' ' && sig[u] <= '~' && '\\' != sig[u])
                HDputchar(sig[u]);
            else if ('\\' == sig[u]) {
                HDputchar('\\');
                HDputchar('\\');
            }
            else
                HDprintf("\\%03o", sig[u]);
        }
        HDputchar('\n');

        HDfprintf(stderr, "unknown signature\n");
        exit_value = 4;
        goto done;
    } /* end else */

    /* Check for an error when dumping information */
    if (status < 0) {
        HDfprintf(stderr, "An error occurred!\n");
        H5Eprint2(H5E_DEFAULT, stderr);
        exit_value = 5;
        goto done;
    } /* end if */

done:
    if (fapl > 0)
        H5Pclose(fapl);
    if (fid > 0)
        H5Fclose(fid);

    /* Pop API context */
    if (api_ctx_pushed)
        H5CX_pop(FALSE);

    H5Eset_auto2(H5E_DEFAULT, func, edata);

    return exit_value;
} /* main() */
