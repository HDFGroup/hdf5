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
 * Parallel tests for encoding/decoding plists sent between processes
 */

#include "testphdf5.h"
#include "H5ACprivate.h"
#include "H5Pprivate.h"

static int
test_encode_decode(hid_t orig_pl, int mpi_rank, int recv_proc)
{
    MPI_Request req[2];
    MPI_Status  status;
    hid_t       pl; /* Decoded property list */
    size_t      buf_size = 0;
    void *      sbuf     = NULL;
    herr_t      ret; /* Generic return value */

    if (mpi_rank == 0) {
        int send_size = 0;

        /* first call to encode returns only the size of the buffer needed */
        ret = H5Pencode2(orig_pl, NULL, &buf_size, H5P_DEFAULT);
        VRFY((ret >= 0), "H5Pencode succeeded");

        sbuf = (uint8_t *)HDmalloc(buf_size);

        ret = H5Pencode2(orig_pl, sbuf, &buf_size, H5P_DEFAULT);
        VRFY((ret >= 0), "H5Pencode succeeded");

        /* this is a temp fix to send this size_t */
        send_size = (int)buf_size;

        MPI_Isend(&send_size, 1, MPI_INT, recv_proc, 123, MPI_COMM_WORLD, &req[0]);
        MPI_Isend(sbuf, send_size, MPI_BYTE, recv_proc, 124, MPI_COMM_WORLD, &req[1]);
    } /* end if */

    if (mpi_rank == recv_proc) {
        int   recv_size;
        void *rbuf;

        MPI_Recv(&recv_size, 1, MPI_INT, 0, 123, MPI_COMM_WORLD, &status);
        buf_size = (size_t)recv_size;
        rbuf     = (uint8_t *)HDmalloc(buf_size);
        MPI_Recv(rbuf, recv_size, MPI_BYTE, 0, 124, MPI_COMM_WORLD, &status);

        pl = H5Pdecode(rbuf);
        VRFY((pl >= 0), "H5Pdecode succeeded");

        VRFY(H5Pequal(orig_pl, pl), "Property List Equal Succeeded");

        ret = H5Pclose(pl);
        VRFY((ret >= 0), "H5Pclose succeeded");

        if (NULL != rbuf)
            HDfree(rbuf);
    } /* end if */

    if (0 == mpi_rank)
        MPI_Waitall(2, req, MPI_STATUSES_IGNORE);

    if (NULL != sbuf)
        HDfree(sbuf);

    MPI_Barrier(MPI_COMM_WORLD);
    return (0);
}

void
test_plist_ed(void)
{
    hid_t dcpl;   /* dataset create prop. list */
    hid_t dapl;   /* dataset access prop. list */
    hid_t dxpl;   /* dataset transfer prop. list */
    hid_t gcpl;   /* group create prop. list */
    hid_t lcpl;   /* link create prop. list */
    hid_t lapl;   /* link access prop. list */
    hid_t ocpypl; /* object copy prop. list */
    hid_t ocpl;   /* object create prop. list */
    hid_t fapl;   /* file access prop. list */
    hid_t fcpl;   /* file create prop. list */
    hid_t strcpl; /* string create prop. list */
    hid_t acpl;   /* attribute create prop. list */

    int mpi_size, mpi_rank, recv_proc;

    hsize_t             chunk_size = 16384; /* chunk size */
    double              fill       = 2.7f;  /* Fill value */
    size_t              nslots     = 521 * 2;
    size_t              nbytes     = 1048576 * 10;
    double              w0         = 0.5f;
    unsigned            max_compact;
    unsigned            min_dense;
    hsize_t             max_size[1]; /*data space maximum size */
    const char *        c_to_f          = "x+32";
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

    herr_t ret; /* Generic return value */

    if (VERBOSE_MED)
        HDprintf("Encode/Decode DCPLs\n");

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (mpi_size == 1)
        recv_proc = 0;
    else
        recv_proc = 1;

    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_chunk(dcpl, 1, &chunk_size);
    VRFY((ret >= 0), "H5Pset_chunk succeeded");

    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
    VRFY((ret >= 0), "H5Pset_alloc_time succeeded");

    ret = H5Pset_fill_value(dcpl, H5T_NATIVE_DOUBLE, &fill);
    VRFY((ret >= 0), "set fill-value succeeded");

    max_size[0] = 100;
    ret         = H5Pset_external(dcpl, "ext1.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) / 4));
    VRFY((ret >= 0), "set external succeeded");
    ret = H5Pset_external(dcpl, "ext2.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) / 4));
    VRFY((ret >= 0), "set external succeeded");
    ret = H5Pset_external(dcpl, "ext3.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) / 4));
    VRFY((ret >= 0), "set external succeeded");
    ret = H5Pset_external(dcpl, "ext4.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) / 4));
    VRFY((ret >= 0), "set external succeeded");

    ret = test_encode_decode(dcpl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(dcpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE DAPLS *****/
    dapl = H5Pcreate(H5P_DATASET_ACCESS);
    VRFY((dapl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_chunk_cache(dapl, nslots, nbytes, w0);
    VRFY((ret >= 0), "H5Pset_chunk_cache succeeded");

    ret = test_encode_decode(dapl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(dapl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE OCPLS *****/
    ocpl = H5Pcreate(H5P_OBJECT_CREATE);
    VRFY((ocpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_attr_creation_order(ocpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    VRFY((ret >= 0), "H5Pset_attr_creation_order succeeded");

    ret = H5Pset_attr_phase_change(ocpl, 110, 105);
    VRFY((ret >= 0), "H5Pset_attr_phase_change succeeded");

    ret = H5Pset_filter(ocpl, H5Z_FILTER_FLETCHER32, 0, (size_t)0, NULL);
    VRFY((ret >= 0), "H5Pset_filter succeeded");

    ret = test_encode_decode(ocpl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(ocpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE DXPLS *****/
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_btree_ratios(dxpl, 0.2f, 0.6f, 0.2f);
    VRFY((ret >= 0), "H5Pset_btree_ratios succeeded");

    ret = H5Pset_hyper_vector_size(dxpl, 5);
    VRFY((ret >= 0), "H5Pset_hyper_vector_size succeeded");

    ret = H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    ret = H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_INDIVIDUAL_IO);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio_collective_opt succeeded");

    ret = H5Pset_dxpl_mpio_chunk_opt(dxpl, H5FD_MPIO_CHUNK_MULTI_IO);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio_chunk_opt succeeded");

    ret = H5Pset_dxpl_mpio_chunk_opt_ratio(dxpl, 30);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio_chunk_opt_ratio succeeded");

    ret = H5Pset_dxpl_mpio_chunk_opt_num(dxpl, 40);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio_chunk_opt_num succeeded");

    ret = H5Pset_edc_check(dxpl, H5Z_DISABLE_EDC);
    VRFY((ret >= 0), "H5Pset_edc_check succeeded");

    ret = H5Pset_data_transform(dxpl, c_to_f);
    VRFY((ret >= 0), "H5Pset_data_transform succeeded");

    ret = test_encode_decode(dxpl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(dxpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE GCPLS *****/
    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    VRFY((gcpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_local_heap_size_hint(gcpl, 256);
    VRFY((ret >= 0), "H5Pset_local_heap_size_hint succeeded");

    ret = H5Pset_link_phase_change(gcpl, 2, 2);
    VRFY((ret >= 0), "H5Pset_link_phase_change succeeded");

    /* Query the group creation properties */
    ret = H5Pget_link_phase_change(gcpl, &max_compact, &min_dense);
    VRFY((ret >= 0), "H5Pget_est_link_info succeeded");

    ret = H5Pset_est_link_info(gcpl, 3, 9);
    VRFY((ret >= 0), "H5Pset_est_link_info succeeded");

    ret = H5Pset_link_creation_order(gcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    VRFY((ret >= 0), "H5Pset_link_creation_order succeeded");

    ret = test_encode_decode(gcpl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(gcpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE LCPLS *****/
    lcpl = H5Pcreate(H5P_LINK_CREATE);
    VRFY((lcpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_create_intermediate_group(lcpl, TRUE);
    VRFY((ret >= 0), "H5Pset_create_intermediate_group succeeded");

    ret = test_encode_decode(lcpl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(lcpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE LAPLS *****/
    lapl = H5Pcreate(H5P_LINK_ACCESS);
    VRFY((lapl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_nlinks(lapl, (size_t)134);
    VRFY((ret >= 0), "H5Pset_nlinks succeeded");

    ret = H5Pset_elink_acc_flags(lapl, H5F_ACC_RDONLY);
    VRFY((ret >= 0), "H5Pset_elink_acc_flags succeeded");

    ret = H5Pset_elink_prefix(lapl, "/tmpasodiasod");
    VRFY((ret >= 0), "H5Pset_nlinks succeeded");

    /* Create FAPL for the elink FAPL */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl >= 0), "H5Pcreate succeeded");
    ret = H5Pset_alignment(fapl, 2, 1024);
    VRFY((ret >= 0), "H5Pset_alignment succeeded");

    ret = H5Pset_elink_fapl(lapl, fapl);
    VRFY((ret >= 0), "H5Pset_elink_fapl succeeded");

    /* Close the elink's FAPL */
    ret = H5Pclose(fapl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    ret = test_encode_decode(lapl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(lapl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE OCPYPLS *****/
    ocpypl = H5Pcreate(H5P_OBJECT_COPY);
    VRFY((ocpypl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_copy_object(ocpypl, H5O_COPY_EXPAND_EXT_LINK_FLAG);
    VRFY((ret >= 0), "H5Pset_copy_object succeeded");

    ret = H5Padd_merge_committed_dtype_path(ocpypl, "foo");
    VRFY((ret >= 0), "H5Padd_merge_committed_dtype_path succeeded");

    ret = H5Padd_merge_committed_dtype_path(ocpypl, "bar");
    VRFY((ret >= 0), "H5Padd_merge_committed_dtype_path succeeded");

    ret = test_encode_decode(ocpypl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(ocpypl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE FAPLS *****/
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_family_offset(fapl, 1024);
    VRFY((ret >= 0), "H5Pset_family_offset succeeded");

    ret = H5Pset_meta_block_size(fapl, 2098452);
    VRFY((ret >= 0), "H5Pset_meta_block_size succeeded");

    ret = H5Pset_sieve_buf_size(fapl, 1048576);
    VRFY((ret >= 0), "H5Pset_sieve_buf_size succeeded");

    ret = H5Pset_alignment(fapl, 2, 1024);
    VRFY((ret >= 0), "H5Pset_alignment succeeded");

    ret = H5Pset_cache(fapl, 1024, 128, 10485760, 0.3f);
    VRFY((ret >= 0), "H5Pset_cache succeeded");

    ret = H5Pset_elink_file_cache_size(fapl, 10485760);
    VRFY((ret >= 0), "H5Pset_elink_file_cache_size succeeded");

    ret = H5Pset_gc_references(fapl, 1);
    VRFY((ret >= 0), "H5Pset_gc_references succeeded");

    ret = H5Pset_small_data_block_size(fapl, 2048);
    VRFY((ret >= 0), "H5Pset_small_data_block_size succeeded");

    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    VRFY((ret >= 0), "H5Pset_libver_bounds succeeded");

    ret = H5Pset_fclose_degree(fapl, H5F_CLOSE_WEAK);
    VRFY((ret >= 0), "H5Pset_fclose_degree succeeded");

    ret = H5Pset_multi_type(fapl, H5FD_MEM_GHEAP);
    VRFY((ret >= 0), "H5Pset_multi_type succeeded");

    ret = H5Pset_mdc_config(fapl, &my_cache_config);
    VRFY((ret >= 0), "H5Pset_mdc_config succeeded");

    ret = test_encode_decode(fapl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(fapl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE FCPLS *****/
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    VRFY((fcpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_userblock(fcpl, 1024);
    VRFY((ret >= 0), "H5Pset_userblock succeeded");

    ret = H5Pset_istore_k(fcpl, 3);
    VRFY((ret >= 0), "H5Pset_istore_k succeeded");

    ret = H5Pset_sym_k(fcpl, 4, 5);
    VRFY((ret >= 0), "H5Pset_sym_k succeeded");

    ret = H5Pset_shared_mesg_nindexes(fcpl, 8);
    VRFY((ret >= 0), "H5Pset_shared_mesg_nindexes succeeded");

    ret = H5Pset_shared_mesg_index(fcpl, 1, H5O_SHMESG_SDSPACE_FLAG, 32);
    VRFY((ret >= 0), "H5Pset_shared_mesg_index succeeded");

    ret = H5Pset_shared_mesg_phase_change(fcpl, 60, 20);
    VRFY((ret >= 0), "H5Pset_shared_mesg_phase_change succeeded");

    ret = H5Pset_sizes(fcpl, 8, 4);
    VRFY((ret >= 0), "H5Pset_sizes succeeded");

    ret = test_encode_decode(fcpl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(fcpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE STRCPLS *****/
    strcpl = H5Pcreate(H5P_STRING_CREATE);
    VRFY((strcpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_char_encoding(strcpl, H5T_CSET_UTF8);
    VRFY((ret >= 0), "H5Pset_char_encoding succeeded");

    ret = test_encode_decode(strcpl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(strcpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /******* ENCODE/DECODE ACPLS *****/
    acpl = H5Pcreate(H5P_ATTRIBUTE_CREATE);
    VRFY((acpl >= 0), "H5Pcreate succeeded");

    ret = H5Pset_char_encoding(acpl, H5T_CSET_UTF8);
    VRFY((ret >= 0), "H5Pset_char_encoding succeeded");

    ret = test_encode_decode(acpl, mpi_rank, recv_proc);
    VRFY((ret >= 0), "test_encode_decode succeeded");

    ret = H5Pclose(acpl);
    VRFY((ret >= 0), "H5Pclose succeeded");
}

void
external_links(void)
{
    hid_t lcpl  = H5I_INVALID_HID; /* link create prop. list */
    hid_t lapl  = H5I_INVALID_HID; /* link access prop. list */
    hid_t fapl  = H5I_INVALID_HID; /* file access prop. list */
    hid_t gapl  = H5I_INVALID_HID; /* group access prop. list */
    hid_t fid   = H5I_INVALID_HID; /* file id */
    hid_t group = H5I_INVALID_HID; /* group id */
    int   mpi_size, mpi_rank;

    MPI_Comm comm;
    int      doIO;
    int      i, mrc;

    herr_t ret;        /* Generic return value */
    htri_t tri_status; /* tri return value */

    const char *filename     = "HDF5test.h5";
    const char *filename_ext = "HDF5test_ext.h5";
    const char *group_path   = "/Base/Block/Step";
    const char *link_name    = "link"; /* external link */
    char        link_path[50];

    if (VERBOSE_MED)
        HDprintf("Check external links\n");

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* Check MPI communicator access properties are passed to
       linked external files */

    if (mpi_rank == 0) {

        lcpl = H5Pcreate(H5P_LINK_CREATE);
        VRFY((lcpl >= 0), "H5Pcreate succeeded");

        ret = H5Pset_create_intermediate_group(lcpl, 1);
        VRFY((ret >= 0), "H5Pset_create_intermediate_group succeeded");

        /* Create file to serve as target for external link.*/
        fid = H5Fcreate(filename_ext, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((fid >= 0), "H5Fcreate succeeded");

        group = H5Gcreate2(fid, group_path, lcpl, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((group >= 0), "H5Gcreate succeeded");

        ret = H5Gclose(group);
        VRFY((ret >= 0), "H5Gclose succeeded");

        ret = H5Fclose(fid);
        VRFY((ret >= 0), "H5Fclose succeeded");

        fapl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((fapl >= 0), "H5Pcreate succeeded");

        /* Create a new file using the file access property list. */
        fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
        VRFY((fid >= 0), "H5Fcreate succeeded");

        ret = H5Pclose(fapl);
        VRFY((ret >= 0), "H5Pclose succeeded");

        group = H5Gcreate2(fid, group_path, lcpl, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((group >= 0), "H5Gcreate succeeded");

        /* Create external links to the target files. */
        ret = H5Lcreate_external(filename_ext, group_path, group, link_name, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((ret >= 0), "H5Lcreate_external succeeded");

        /* Close and release resources. */
        ret = H5Pclose(lcpl);
        VRFY((ret >= 0), "H5Pclose succeeded");
        ret = H5Gclose(group);
        VRFY((ret >= 0), "H5Gclose succeeded");
        ret = H5Fclose(fid);
        VRFY((ret >= 0), "H5Fclose succeeded");
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /*
     * For the first  case, use all the  processes. For the second case
     * use  a sub-communicator  to verify  the  correct communicator is
     * being used for the externally linked files.
     * There is no way to determine if MPI info is being used for the
     * externally linked files.
     */

    for (i = 0; i < 2; i++) {

        if (i == 0) {
            doIO = 1;
            comm = MPI_COMM_WORLD;
        }
        else {
            doIO = mpi_rank % 2;
            mrc  = MPI_Comm_split(MPI_COMM_WORLD, doIO, mpi_rank, &comm);
            VRFY((mrc == MPI_SUCCESS), "");
        }

        if (doIO) {
            fapl = H5Pcreate(H5P_FILE_ACCESS);
            VRFY((fapl >= 0), "H5Pcreate succeeded");
            ret = H5Pset_fapl_mpio(fapl, comm, MPI_INFO_NULL);
            VRFY((fapl >= 0), "H5Pset_fapl_mpio succeeded");

            fid = H5Fopen(filename, H5F_ACC_RDWR, fapl);
            VRFY((fid >= 0), "H5Fopen succeeded");

            /* test opening a group that is to an external link, the external linked
               file should inherit the source file's access properties */
            HDsprintf(link_path, "%s%s%s", group_path, "/", link_name);
            group = H5Gopen2(fid, link_path, H5P_DEFAULT);
            VRFY((group >= 0), "H5Gopen succeeded");
            ret = H5Gclose(group);
            VRFY((ret >= 0), "H5Gclose succeeded");

            /* test opening a group that is external link by setting group
               creation property */
            gapl = H5Pcreate(H5P_GROUP_ACCESS);
            VRFY((gapl >= 0), "H5Pcreate succeeded");

            ret = H5Pset_elink_fapl(gapl, fapl);
            VRFY((ret >= 0), "H5Pset_elink_fapl succeeded");

            group = H5Gopen2(fid, link_path, gapl);
            VRFY((group >= 0), "H5Gopen succeeded");

            ret = H5Gclose(group);
            VRFY((ret >= 0), "H5Gclose succeeded");

            ret = H5Pclose(gapl);
            VRFY((ret >= 0), "H5Pclose succeeded");

            /* test link APIs */
            lapl = H5Pcreate(H5P_LINK_ACCESS);
            VRFY((lapl >= 0), "H5Pcreate succeeded");

            ret = H5Pset_elink_fapl(lapl, fapl);
            VRFY((ret >= 0), "H5Pset_elink_fapl succeeded");

            tri_status = H5Lexists(fid, link_path, H5P_DEFAULT);
            VRFY((tri_status == TRUE), "H5Lexists succeeded");

            tri_status = H5Lexists(fid, link_path, lapl);
            VRFY((tri_status == TRUE), "H5Lexists succeeded");

            group = H5Oopen(fid, link_path, H5P_DEFAULT);
            VRFY((group >= 0), "H5Oopen succeeded");

            ret = H5Oclose(group);
            VRFY((ret >= 0), "H5Oclose succeeded");

            group = H5Oopen(fid, link_path, lapl);
            VRFY((group >= 0), "H5Oopen succeeded");

            ret = H5Oclose(group);
            VRFY((ret >= 0), "H5Oclose succeeded");

            ret = H5Pclose(lapl);
            VRFY((ret >= 0), "H5Pclose succeeded");

            /* close the remaining resources */

            ret = H5Pclose(fapl);
            VRFY((ret >= 0), "H5Pclose succeeded");

            ret = H5Fclose(fid);
            VRFY((ret >= 0), "H5Fclose succeeded");

            if (i == 1) {
                mrc = MPI_Comm_free(&comm);
                VRFY((mrc == MPI_SUCCESS), "MPI_Comm_free succeeded");
            }
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* delete the test files */
    if (mpi_rank == 0) {
        MPI_File_delete(filename, MPI_INFO_NULL);
        MPI_File_delete(filename_ext, MPI_INFO_NULL);
    }
}
