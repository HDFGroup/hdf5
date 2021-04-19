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

/*
 * Serial tests for encoding/decoding plists
 */

#include "testhdf5.h"
#include "H5ACprivate.h"
#include "H5Pprivate.h"

#define SRC_FNAME "source_file.h5"
#define SRC_DSET  "src_dset"

static int
test_encode_decode(hid_t orig_pl, H5F_libver_t low, H5F_libver_t high, hbool_t support_virtual)
{
    hid_t  pl        = (-1); /* Decoded property list */
    hid_t  fapl      = -1;   /* File access property list */
    void * temp_buf  = NULL; /* Pointer to encoding buffer */
    size_t temp_size = 0;    /* Size of encoding buffer */
    herr_t ret;              /* Return value */

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR

    /* Set library version bounds */
    if (H5Pset_libver_bounds(fapl, low, high) < 0)
        TEST_ERROR

    H5E_BEGIN_TRY
    {
        ret = H5Pencode2(orig_pl, NULL, &temp_size, fapl);
    }
    H5E_END_TRY;

    if (support_virtual && high < H5F_LIBVER_V110)
        VERIFY(ret, FAIL, "H5Pencode2");
    else {

        VERIFY(ret, SUCCEED, "H5Pencode2");

        /* Allocate the buffer for encoding */
        if (NULL == (temp_buf = (void *)HDmalloc(temp_size)))
            TEST_ERROR

        /* Encode the property list to the buffer */
        if (H5Pencode2(orig_pl, temp_buf, &temp_size, fapl) < 0)
            TEST_ERROR

        /* Decode the buffer */
        if ((pl = H5Pdecode(temp_buf)) < 0)
            STACK_ERROR

        /* Check if the original and the decoded property lists are equal */
        if (!H5Pequal(orig_pl, pl))
            PUTS_ERROR("encoding-decoding cycle failed\n")

        /* Close the decoded property list */
        if ((H5Pclose(pl)) < 0)
            TEST_ERROR

        /* Free the buffer */
        if (temp_buf)
            HDfree(temp_buf);

#ifndef H5_NO_DEPRECATED_SYMBOLS
        /* Test H5Pencode1() */

        /* first call to encode returns only the size of the buffer needed */
        if (H5Pencode1(orig_pl, NULL, &temp_size) < 0)
            STACK_ERROR

        if (NULL == (temp_buf = (void *)HDmalloc(temp_size)))
            TEST_ERROR

        if (H5Pencode1(orig_pl, temp_buf, &temp_size) < 0)
            STACK_ERROR

        if ((pl = H5Pdecode(temp_buf)) < 0)
            STACK_ERROR

        if (!H5Pequal(orig_pl, pl))
            PUTS_ERROR("encoding-decoding cycle failed\n")

        if ((H5Pclose(pl)) < 0)
            STACK_ERROR

        if (temp_buf)
            HDfree(temp_buf);
#endif /* H5_NO_DEPRECATED_SYMBOLS */
    }

    if ((H5Pclose(fapl)) < 0)
        TEST_ERROR

    /* Success */
    return (0);

error:
    if (temp_buf)
        HDfree(temp_buf);

    H5E_BEGIN_TRY
    {
        H5Pclose(pl);
        H5Pclose(fapl);
    }
    H5E_END_TRY;

    return (-1);
} /* end test_encode_decode() */

int
main(void)
{
    hid_t        dcpl;                       /* dataset create prop. list */
    hid_t        dapl;                       /* dataset access prop. list */
    hid_t        dxpl;                       /* dataset xfer prop. list */
    hid_t        gcpl;                       /* group create prop. list */
    hid_t        ocpypl;                     /* object copy prop. list */
    hid_t        ocpl;                       /* object create prop. list */
    hid_t        lcpl;                       /* link create prop. list */
    hid_t        lapl;                       /* link access prop. list */
    hid_t        fapl;                       /* file access prop. list */
    hid_t        fcpl;                       /* file create prop. list */
    hid_t        strcpl;                     /* string create prop. list */
    hid_t        acpl;                       /* attribute create prop. list */
    hid_t        srcspace      = -1;         /* Source dataspaces */
    hid_t        vspace        = -1;         /* Virtual dset dataspaces */
    hsize_t      dims[1]       = {3};        /* Data space current size */
    hsize_t      chunk_size[2] = {16384, 4}; /* chunk size */
    double       fill          = 2.7f;       /* Fill value */
    hsize_t      max_size[1];                /* data space maximum size */
    size_t       nslots = 521 * 2;
    size_t       nbytes = 1048576 * 10;
    double       w0     = 0.5f;
    unsigned     max_compact;
    unsigned     min_dense;
    const char * c_to_f = "x+32";
    H5F_libver_t low, high; /* Low and high bounds */

    H5AC_cache_config_t my_cache_config = {H5AC__CURR_CACHE_CONFIG_VERSION,
                                           TRUE,
                                           FALSE,
                                           FALSE,
                                           "temp",
                                           TRUE,
                                           FALSE,
                                           (2 * 2048 * 1024),
                                           0.3f,
                                           (64 * 1024 * 1024),
                                           (4 * 1024 * 1024),
                                           60000,
                                           H5C_incr__threshold,
                                           0.8f,
                                           3.0f,
                                           TRUE,
                                           (8 * 1024 * 1024),
                                           H5C_flash_incr__add_space,
                                           2.0f,
                                           0.25f,
                                           H5C_decr__age_out_with_threshold,
                                           0.997f,
                                           0.8f,
                                           TRUE,
                                           (3 * 1024 * 1024),
                                           3,
                                           FALSE,
                                           0.2f,
                                           (256 * 2048),
                                           H5AC__DEFAULT_METADATA_WRITE_STRATEGY};

    H5AC_cache_image_config_t my_cache_image_config = {H5AC__CURR_CACHE_IMAGE_CONFIG_VERSION, TRUE, FALSE,
                                                       -1};

    /* Loop through all the combinations of low/high version bounds */
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            char        msg[80];     /* Message for file version bounds */
            const char *low_string;  /* The low bound string */
            const char *high_string; /* The high bound string */

            /* Invalid combinations, just continue */
            if (high == H5F_LIBVER_EARLIEST || high < low)
                continue;

            /* Display testing info */
            low_string  = h5_get_version_string(low);
            high_string = h5_get_version_string(high);
            HDsprintf(msg, "Testing ENCODE/DECODE with file version bounds: (%s, %s):", low_string,
                      high_string);
            HDputs(msg);

            if (VERBOSE_MED)
                HDprintf("Encode/Decode DCPLs\n");

            /******* ENCODE/DECODE DCPLS *****/
            TESTING("Default DCPL Encoding/Decoding");
            if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(dcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default DCPL encoding/decoding failed\n")

            PASSED();

            TESTING("DCPL Encoding/Decoding");

            if ((H5Pset_chunk(dcpl, 2, chunk_size)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_fill_value(dcpl, H5T_NATIVE_DOUBLE, &fill)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_dset_no_attrs_hint(dcpl, FALSE)) < 0)
                FAIL_STACK_ERROR

            max_size[0] = 100;
            if ((H5Pset_external(dcpl, "ext1.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) / 4))) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_external(dcpl, "ext2.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) / 4))) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_external(dcpl, "ext3.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) / 4))) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_external(dcpl, "ext4.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) / 4))) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(dcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("DCPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(dcpl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE DCPLS *****/
            TESTING("DCPL Encoding/Decoding for virtual layout");
            if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Set virtual layout */
            if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
                TEST_ERROR

            /* Create source dataspace */
            if ((srcspace = H5Screate_simple(1, dims, NULL)) < 0)
                TEST_ERROR

            /* Create virtual dataspace */
            if ((vspace = H5Screate_simple(1, dims, NULL)) < 0)
                TEST_ERROR

            /* Add virtual layout mapping */
            if (H5Pset_virtual(dcpl, vspace, SRC_FNAME, SRC_DSET, srcspace) < 0)
                TEST_ERROR

            if (test_encode_decode(dcpl, low, high, TRUE) < 0)
                FAIL_PUTS_ERROR("DCPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(dcpl)) < 0)
                FAIL_STACK_ERROR

            /******* ENCODE/DECODE DAPLS *****/
            TESTING("Default DAPL Encoding/Decoding");
            if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(dapl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default DAPL encoding/decoding failed\n")

            PASSED();

            TESTING("DAPL Encoding/Decoding");

            if ((H5Pset_chunk_cache(dapl, nslots, nbytes, w0)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(dapl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("DAPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(dapl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE OCPLS *****/
            TESTING("Default OCPL Encoding/Decoding");
            if ((ocpl = H5Pcreate(H5P_OBJECT_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(ocpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default OCPL encoding/decoding failed\n")

            PASSED();

            TESTING("OCPL Encoding/Decoding");

            if ((H5Pset_attr_creation_order(ocpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED))) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_attr_phase_change(ocpl, 110, 105)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_filter(ocpl, H5Z_FILTER_FLETCHER32, 0, (size_t)0, NULL)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(ocpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("OCPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(ocpl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE DXPLS *****/
            TESTING("Default DXPL Encoding/Decoding");
            if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(dxpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default DXPL encoding/decoding failed\n")

            PASSED();

            TESTING("DXPL Encoding/Decoding");

            if ((H5Pset_btree_ratios(dxpl, 0.2f, 0.6f, 0.2f)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_hyper_vector_size(dxpl, 5)) < 0)
                FAIL_STACK_ERROR

#ifdef H5_HAVE_PARALLEL
            if ((H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_INDIVIDUAL_IO)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_dxpl_mpio_chunk_opt(dxpl, H5FD_MPIO_CHUNK_MULTI_IO)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_dxpl_mpio_chunk_opt_ratio(dxpl, 30)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_dxpl_mpio_chunk_opt_num(dxpl, 40)) < 0)
                FAIL_STACK_ERROR
#endif /* H5_HAVE_PARALLEL */
            if ((H5Pset_edc_check(dxpl, H5Z_DISABLE_EDC)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_data_transform(dxpl, c_to_f)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(dxpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("DXPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(dxpl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE GCPLS *****/
            TESTING("Default GCPL Encoding/Decoding");
            if ((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(gcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default GCPL encoding/decoding failed\n")

            PASSED();

            TESTING("GCPL Encoding/Decoding");

            if ((H5Pset_local_heap_size_hint(gcpl, 256)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_link_phase_change(gcpl, 2, 2)) < 0)
                FAIL_STACK_ERROR

            /* Query the group creation properties */
            if ((H5Pget_link_phase_change(gcpl, &max_compact, &min_dense)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_est_link_info(gcpl, 3, 9)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_link_creation_order(gcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED))) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(gcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("GCPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(gcpl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE LCPLS *****/
            TESTING("Default LCPL Encoding/Decoding");
            if ((lcpl = H5Pcreate(H5P_LINK_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(lcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default LCPL encoding/decoding failed\n")

            PASSED();

            TESTING("LCPL Encoding/Decoding");

            if ((H5Pset_create_intermediate_group(lcpl, TRUE)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(lcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("LCPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(lcpl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE LAPLS *****/
            TESTING("Default LAPL Encoding/Decoding");
            if ((lapl = H5Pcreate(H5P_LINK_ACCESS)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(lapl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default LAPL encoding/decoding failed\n")

            PASSED();

            TESTING("LAPL Encoding/Decoding");

            if ((H5Pset_nlinks(lapl, (size_t)134)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_elink_acc_flags(lapl, H5F_ACC_RDONLY)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_elink_prefix(lapl, "/tmpasodiasod")) < 0)
                FAIL_STACK_ERROR

            /* Create FAPL for the elink FAPL */
            if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_alignment(fapl, 2, 1024)) < 0)
                FAIL_STACK_ERROR

            if ((H5Pset_elink_fapl(lapl, fapl)) < 0)
                FAIL_STACK_ERROR

            /* Close the elink's FAPL */
            if ((H5Pclose(fapl)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(lapl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("LAPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(lapl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE OCPYPLS *****/
            TESTING("Default OCPYPL Encoding/Decoding");
            if ((ocpypl = H5Pcreate(H5P_OBJECT_COPY)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(ocpypl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default OCPYPL encoding/decoding failed\n")

            PASSED();

            TESTING("OCPYPL Encoding/Decoding");

            if ((H5Pset_copy_object(ocpypl, H5O_COPY_EXPAND_EXT_LINK_FLAG)) < 0)
                FAIL_STACK_ERROR

            if ((H5Padd_merge_committed_dtype_path(ocpypl, "foo")) < 0)
                FAIL_STACK_ERROR
            if ((H5Padd_merge_committed_dtype_path(ocpypl, "bar")) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(ocpypl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("OCPYPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(ocpypl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE FAPLS *****/
            TESTING("Default FAPL Encoding/Decoding");
            if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(fapl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default FAPL encoding/decoding failed\n")

            PASSED();

            TESTING("FAPL Encoding/Decoding");

            if ((H5Pset_family_offset(fapl, 1024)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_meta_block_size(fapl, 2098452)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_sieve_buf_size(fapl, 1048576)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_alignment(fapl, 2, 1024)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_cache(fapl, 1024, 128, 10485760, 0.3f)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_elink_file_cache_size(fapl, 10485760)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_gc_references(fapl, 1)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_small_data_block_size(fapl, 2048)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_fclose_degree(fapl, H5F_CLOSE_WEAK)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_multi_type(fapl, H5FD_MEM_GHEAP)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_mdc_config(fapl, &my_cache_config)) < 0)
                FAIL_STACK_ERROR
            if ((H5Pset_mdc_image_config(fapl, &my_cache_image_config)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(fapl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("FAPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(fapl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE FCPLS *****/
            TESTING("Default FCPL Encoding/Decoding");

            if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(fcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default FCPL encoding/decoding failed\n")

            PASSED();

            TESTING("FCPL Encoding/Decoding");

            if ((H5Pset_userblock(fcpl, 1024) < 0))
                FAIL_STACK_ERROR

            if ((H5Pset_istore_k(fcpl, 3) < 0))
                FAIL_STACK_ERROR

            if ((H5Pset_sym_k(fcpl, 4, 5) < 0))
                FAIL_STACK_ERROR

            if ((H5Pset_shared_mesg_nindexes(fcpl, 8) < 0))
                FAIL_STACK_ERROR

            if ((H5Pset_shared_mesg_index(fcpl, 1, H5O_SHMESG_SDSPACE_FLAG, 32) < 0))
                FAIL_STACK_ERROR

            if ((H5Pset_shared_mesg_phase_change(fcpl, 60, 20) < 0))
                FAIL_STACK_ERROR

            if ((H5Pset_sizes(fcpl, 8, 4) < 0))
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(fcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("FCPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(fcpl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE STRCPLS *****/
            TESTING("Default STRCPL Encoding/Decoding");

            if ((strcpl = H5Pcreate(H5P_STRING_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(strcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default STRCPL encoding/decoding failed\n")

            PASSED();

            TESTING("STRCPL Encoding/Decoding");

            if ((H5Pset_char_encoding(strcpl, H5T_CSET_UTF8) < 0))
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(strcpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("STRCPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(strcpl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

            /******* ENCODE/DECODE ACPLS *****/
            TESTING("Default ACPL Encoding/Decoding");

            if ((acpl = H5Pcreate(H5P_ATTRIBUTE_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Test encoding & decoding default property list */
            if (test_encode_decode(acpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("Default ACPL encoding/decoding failed\n")

            PASSED();

            TESTING("ACPL Encoding/Decoding");

            if ((H5Pset_char_encoding(acpl, H5T_CSET_UTF8) < 0))
                FAIL_STACK_ERROR

            /* Test encoding & decoding property list */
            if (test_encode_decode(acpl, low, high, FALSE) < 0)
                FAIL_PUTS_ERROR("ACPL encoding/decoding failed\n")

            /* release resource */
            if ((H5Pclose(acpl)) < 0)
                FAIL_STACK_ERROR

            PASSED();

        } /* end high */
    }     /* end low */

    return 0;

error:
    HDprintf("***** Plist Encode/Decode tests FAILED! *****\n");
    return 1;
}
