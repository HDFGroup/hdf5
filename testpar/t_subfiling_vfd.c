/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * HDF5 Subfiling VFD tests
 */

#include <mpi.h>

#include "testpar.h"
#include "H5srcdir.h"
#include "H5MMprivate.h"

#ifdef H5_HAVE_SUBFILING_VFD

#include "H5FDsubfiling.h"
#include "H5FDioc.h"

#define SUBFILING_TEST_DIR H5FD_SUBFILING_NAME

/* The smallest Subfiling stripe size used for testing */
#define SUBFILING_MIN_STRIPE_SIZE 128

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define ARRAY_SIZE(a) sizeof(a) / sizeof(a[0])

#define CHECK_PASSED()               \
do {                                 \
    if (MAINPROCESS) {               \
        if (nerrors == curr_nerrors) \
            PASSED();                \
        else                         \
            H5_FAILED();             \
    }                                \
} while(0)

static MPI_Comm comm_g = MPI_COMM_WORLD;
static MPI_Info info_g = MPI_INFO_NULL;
static int      mpi_rank;
static int      mpi_size;

static MPI_Comm node_local_comm = MPI_COMM_WORLD;
static int      node_local_rank;
static int      node_local_size;

static MPI_Comm ioc_comm = MPI_COMM_WORLD;
static int      ioc_comm_rank;
static int      ioc_comm_size;

static long long stripe_size_g;
static long      ioc_per_node_g;
static int       ioc_thread_pool_size_g;

int nerrors      = 0;
int curr_nerrors = 0;

/* Function pointer typedef for test functions */
typedef void (*test_func)(void);

/* Utility functions */
static hid_t create_subfiling_ioc_fapl(MPI_Comm comm, MPI_Info info, hbool_t custom_config,
                                       H5FD_subfiling_shared_config_t *custom_cfg, int32_t thread_pool_size);

/* Test functions */
static void test_create_and_close(void);
static void test_config_file(void);
static void test_stripe_sizes(void);
static void test_subfiling_write_many_read_one(void);
static void test_subfiling_write_many_read_few(void);

static test_func tests[] = {
    test_create_and_close,
    test_config_file,
    test_stripe_sizes,
    test_subfiling_write_many_read_one,
    test_subfiling_write_many_read_few,
};

/* ---------------------------------------------------------------------------
 * Function:    create_subfiling_ioc_fapl
 *
 * Purpose:     Create and populate a subfiling FAPL ID.
 *
 * Return:      Success: HID of the top-level (subfiling) FAPL, a non-negative
 *                       value.
 *              Failure: H5I_INVALID_HID, a negative value.
 * ---------------------------------------------------------------------------
 */
static hid_t
create_subfiling_ioc_fapl(MPI_Comm comm, MPI_Info info, hbool_t custom_config,
                          H5FD_subfiling_shared_config_t *custom_cfg, int32_t thread_pool_size)
{
    H5FD_subfiling_config_t *subfiling_conf = NULL;
    H5FD_ioc_config_t       *ioc_conf       = NULL;
    hid_t                    ioc_fapl       = H5I_INVALID_HID;
    hid_t                    ret_value      = H5I_INVALID_HID;

    HDassert(!custom_config || custom_cfg);

    if ((ret_value = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if (H5Pset_mpi_params(ret_value, comm, info) < 0)
        TEST_ERROR;

    if (!custom_config) {
        if (H5Pset_fapl_subfiling(ret_value, NULL) < 0)
            TEST_ERROR;
    }
    else {
        if (NULL == (subfiling_conf = HDcalloc(1, sizeof(*subfiling_conf))))
            TEST_ERROR;

        if ((ioc_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            TEST_ERROR;

        if (H5Pset_mpi_params(ioc_fapl, comm, info) < 0)
            TEST_ERROR;

        /* Get defaults for Subfiling configuration */
        if (H5Pget_fapl_subfiling(ret_value, subfiling_conf) < 0)
            TEST_ERROR;

        /* Set custom configuration */
        subfiling_conf->shared_cfg = *custom_cfg;

        if (subfiling_conf->require_ioc) {
            if (NULL == (ioc_conf = HDcalloc(1, sizeof(*ioc_conf))))
                TEST_ERROR;

            /* Get IOC VFD defaults */
            if (H5Pget_fapl_ioc(ioc_fapl, ioc_conf) < 0)
                TEST_ERROR;

            /* Set custom configuration */
            ioc_conf->subf_config      = *custom_cfg;
            ioc_conf->thread_pool_size = thread_pool_size;

            if (H5Pset_fapl_ioc(ioc_fapl, ioc_conf) < 0)
                TEST_ERROR;
        }
        else {
            if (H5Pset_fapl_sec2(ioc_fapl) < 0)
                TEST_ERROR;
        }

        if (H5Pclose(subfiling_conf->ioc_fapl_id) < 0)
            TEST_ERROR;
        subfiling_conf->ioc_fapl_id = ioc_fapl;

        if (H5Pset_fapl_subfiling(ret_value, subfiling_conf) < 0)
            TEST_ERROR;

        HDfree(ioc_conf);
        ioc_conf = NULL;
        HDfree(subfiling_conf);
        subfiling_conf = NULL;
    }

    return ret_value;

error:
    HDfree(ioc_conf);
    HDfree(subfiling_conf);

    if ((H5I_INVALID_HID != ioc_fapl) && (H5Pclose(ioc_fapl) < 0)) {
        H5_FAILED();
        AT();
    }
    if ((H5I_INVALID_HID != ret_value) && (H5Pclose(ret_value) < 0)) {
        H5_FAILED();
        AT();
    }

    return H5I_INVALID_HID;
}

/*
 * A simple test that creates and closes a file with the
 * subfiling VFD
 */
#define SUBF_FILENAME "test_subfiling_basic_create.h5"
static void
test_create_and_close(void)
{
    hid_t file_id = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("file creation and immediate close");

    /* Get a default Subfiling FAPL */
    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, FALSE, NULL, 0);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    H5E_BEGIN_TRY
    {
        H5Fdelete(SUBF_FILENAME, fapl_id);
    }
    H5E_END_TRY;

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME

/*
 * Test to check that Subfiling configuration file matches
 * what is expected for a given configuration
 */
#define SUBF_FILENAME "test_subfiling_config_file.h5"
static void
test_config_file(void)
{
    H5FD_subfiling_shared_config_t cfg;
    int64_t                        stripe_size;
    int64_t                        read_stripe_size;
    FILE                          *config_file;
    char                          *config_filename = NULL;
    char                          *config_buf      = NULL;
    long                           config_file_len;
    hid_t                          file_id = H5I_INVALID_HID;
    hid_t                          fapl_id = H5I_INVALID_HID;
    int                            read_stripe_count;
    int                            mpi_code;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("subfiling configuration file format");

    /*
     * Choose a random Subfiling stripe size between
     * the smallest allowed value and 32MiB
     */
    stripe_size = (rand() % ((int)(32 * H5_MB) - SUBFILING_MIN_STRIPE_SIZE)) + SUBFILING_MIN_STRIPE_SIZE;

    /* TODO: Subfiling currently does not respect stripe_count setting */
    cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
    cfg.stripe_size   = (stripe_size_g > 0) ? stripe_size_g : stripe_size;

    if (ioc_per_node_g > 0) {
        if (ioc_per_node_g * ioc_comm_size <= mpi_size)
            cfg.stripe_count = (int32_t) (ioc_per_node_g * ioc_comm_size);
        else
            cfg.stripe_count = mpi_size;
    }
    else
        cfg.stripe_count = ioc_comm_size;

    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, TRUE, &cfg, H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    if (MAINPROCESS) {
        h5_stat_t file_info;
        char     *resolved_path;
        char     *subfile_dir;
        char     *subfile_name;
        char     *tmp_buf;
        char     *substr;
        char      scan_format[256];
        int       num_digits;

        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        config_filename = HDmalloc(PATH_MAX);
        VRFY(config_filename, "HDmalloc succeeded");

        HDsnprintf(config_filename, PATH_MAX, H5FD_SUBFILING_CONFIG_FILENAME_TEMPLATE, SUBF_FILENAME,
                   (uint64_t)file_info.st_ino);

        config_file = HDfopen(config_filename, "r");
        VRFY(config_file, "HDfopen succeeded");

        HDfree(config_filename);

        VRFY((HDfseek(config_file, 0, SEEK_END) >= 0), "HDfseek succeeded");

        config_file_len = HDftell(config_file);
        VRFY((config_file_len > 0), "HDftell succeeded");

        VRFY((HDfseek(config_file, 0, SEEK_SET) >= 0), "HDfseek succeeded");

        config_buf = HDmalloc((size_t)config_file_len + 1);
        VRFY(config_buf, "HDmalloc succeeded");

        VRFY((HDfread(config_buf, (size_t)config_file_len, 1, config_file) == 1), "HDfread succeeded");
        config_buf[config_file_len] = '\0';

        /* Check the stripe_size field in the configuration file */
        substr = HDstrstr(config_buf, "stripe_size");
        VRFY(substr, "HDstrstr succeeded");

        VRFY((HDsscanf(substr, "stripe_size=%" PRId64, &read_stripe_size) == 1), "HDsscanf succeeded");
        VRFY((read_stripe_size == cfg.stripe_size), "Stripe size comparison succeeded");

        /* Check the aggregator_count (stripe count) field in the configuration file */
        substr = HDstrstr(config_buf, "aggregator_count");
        VRFY(substr, "HDstrstr succeeded");

        VRFY((HDsscanf(substr, "aggregator_count=%d", &read_stripe_count) == 1), "HDsscanf succeeded");
        VRFY((read_stripe_count == cfg.stripe_count), "Stripe count comparison succeeded");

        /* Check the hdf5_file and subfile_dir fields in the configuration file */
        resolved_path = HDrealpath(SUBF_FILENAME, NULL);
        VRFY(resolved_path, "HDrealpath succeeded");

        VRFY((H5_dirname(resolved_path, &subfile_dir) >= 0), "H5_dirname succeeded");

        tmp_buf = HDmalloc(PATH_MAX);
        VRFY(tmp_buf, "HDmalloc succeeded");

        substr = HDstrstr(config_buf, "hdf5_file");
        VRFY(substr, "HDstrstr succeeded");

        HDsnprintf(scan_format, sizeof(scan_format), "hdf5_file=%%%zus", (size_t)(PATH_MAX - 1));
        VRFY((HDsscanf(substr, scan_format, tmp_buf) == 1), "HDsscanf succeeded");

        VRFY((HDstrcmp(tmp_buf, resolved_path) == 0), "HDstrcmp succeeded");

        substr = HDstrstr(config_buf, "subfile_dir");
        VRFY(substr, "HDstrstr succeeded");

        HDsnprintf(scan_format, sizeof(scan_format), "subfile_dir=%%%zus", (size_t)(PATH_MAX - 1));
        VRFY((HDsscanf(substr, scan_format, tmp_buf) == 1), "HDsscanf succeeded");

        VRFY((HDstrcmp(tmp_buf, subfile_dir) == 0), "HDstrcmp succeeded");

        HDfree(tmp_buf);
        H5MM_free(subfile_dir);
        HDfree(resolved_path);

        subfile_name = HDmalloc(PATH_MAX);
        VRFY(subfile_name, "HDmalloc succeeded");

        /* Verify the name of each subfile is in the configuration file */
        num_digits = (int)(HDlog10(cfg.stripe_count) + 1);
        for (size_t i = 0; i < (size_t)cfg.stripe_count; i++) {
            HDsnprintf(subfile_name, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                       (uint64_t)file_info.st_ino, num_digits, (int)i + 1, cfg.stripe_count);

            substr = HDstrstr(config_buf, subfile_name);
            VRFY(substr, "HDstrstr succeeded");
        }

        HDfree(subfile_name);
        HDfree(config_buf);

        VRFY((HDfclose(config_file) >= 0), "HDfclose on configuration file succeeded");
    }

    mpi_code = MPI_Barrier(comm_g);
    VRFY((mpi_code == MPI_SUCCESS), "MPI_Barrier succeeded");

    H5E_BEGIN_TRY
    {
        H5Fdelete(SUBF_FILENAME, fapl_id);
    }
    H5E_END_TRY;

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME

/*
 * Test a few different Subfiling stripe sizes with a fixed
 * stripe count
 */
/* TODO: Test collective I/O as well when support is implemented */
#define SUBF_FILENAME "test_subfiling_stripe_sizes.h5"
#define SUBF_NITER    10
static void
test_stripe_sizes(void)
{
    H5FD_t *file_ptr     = NULL;
    void   *write_buf    = NULL;
    char   *tmp_filename = NULL;
    hid_t   dxpl_id      = H5I_INVALID_HID;
    int     n_io_concentrators;
    int     num_digits;
    hid_t   fapl_id = H5I_INVALID_HID;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("random subfiling stripe sizes");

    tmp_filename = HDmalloc(PATH_MAX);
    VRFY(tmp_filename, "HDmalloc succeeded");

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "DCPL creation succeeded");

    for (size_t i = 0; i < SUBF_NITER; i++) {
        H5FD_subfiling_shared_config_t cfg;
        h5_stat_size_t                 file_size;
        const void                    *c_write_buf;
        h5_stat_t                      file_info;
        int64_t                        file_size64;
        int64_t                        stripe_size;
        haddr_t                        file_end_addr;
        haddr_t                        write_addr;
        size_t                         nbytes;
        herr_t                         write_status;
        hid_t                          file_id;
        int                            mpi_code;

        /*
         * Choose a random Subfiling stripe size between
         * the smallest allowed value and 32MiB
         */
        stripe_size = (rand() % ((int)(32 * H5_MB) - SUBFILING_MIN_STRIPE_SIZE)) + SUBFILING_MIN_STRIPE_SIZE;

        cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
        cfg.stripe_size   = (stripe_size_g > 0) ? stripe_size_g : stripe_size;
        cfg.stripe_count  = (int32_t) ((ioc_per_node_g > 0) ? ioc_per_node_g * ioc_comm_size : ioc_comm_size);

        /* Start with a single rank for I/O */
        n_io_concentrators = 1;
        num_digits         = (int)(HDlog10(n_io_concentrators) + 1);

        nbytes = (size_t)(stripe_size * n_io_concentrators);

        write_buf = HDmalloc(nbytes);
        VRFY(write_buf, "HDmalloc succeeded");

        HDmemset(write_buf, 255, nbytes);

        c_write_buf = write_buf;

        /* First, try I/O with a single rank */
        if (MAINPROCESS) {
            fapl_id = create_subfiling_ioc_fapl(MPI_COMM_SELF, MPI_INFO_NULL, TRUE, &cfg,
                                                H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
            VRFY((fapl_id >= 0), "FAPL creation succeeded");

            /* Create and close file with H5Fcreate to setup superblock */
            file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
            VRFY((file_id >= 0), "H5Fcreate succeeded");
            VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

            /* Re-open file through H5FDopen for direct writes */
            file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
            VRFY(file_ptr, "H5FDopen succeeded");

            /*
             * Get the current file size to see where we can safely
             * write to in the file without overwriting the superblock
             */
            VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");
            file_size = (h5_stat_size_t)file_info.st_size;

            H5_CHECK_OVERFLOW(file_size, h5_stat_size_t, haddr_t);
            file_end_addr = (haddr_t)file_size;

            c_write_buf = write_buf;

            /* Set independent I/O on DXPL */
            VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_INDEPENDENT) >= 0), "H5Pset_dxpl_mpio succeeded");

            /* Set EOA for following write call */
            VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, file_end_addr + nbytes) >= 0),
                 "H5FDset_eoa succeeded");

            /*
             * Write "number of IOCs" X "stripe size" bytes to the file
             * and ensure that we have "number of IOCs" sub-files, each
             * with a size of at least "stripe size" bytes. The first
             * (few) subfile(s) may be a bit larger due to file metadata.
             */
            write_addr   = file_end_addr;
            write_status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
            VRFY((write_status >= 0), "H5FDwrite succeeded");

            file_end_addr += nbytes;

            for (int j = 0; j < n_io_concentrators; j++) {
                h5_stat_size_t subfile_size;
                h5_stat_t      subfile_info;
                FILE          *subfile_ptr;

                HDsnprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                           (uint64_t)file_info.st_ino, num_digits, j + 1, n_io_concentrators);

                /* Ensure file exists */
                subfile_ptr = HDfopen(tmp_filename, "r");
                VRFY(subfile_ptr, "HDfopen on subfile succeeded");
                VRFY((HDfclose(subfile_ptr) >= 0), "HDfclose on subfile succeeded");

                /* Check file size */
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= stripe_size), "File size verification succeeded");
            }

            /* Set EOA for following write call */
            VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, file_end_addr + nbytes) >= 0),
                 "H5FDset_eoa succeeded");

            /*
             * Write another round of "number of IOCs" X "stripe size"
             * bytes to the file using vector I/O and ensure we have
             * "number of IOCs" sub-files, each with a size of at least
             * 2 * "stripe size" bytes. The first (few) subfile(s) may
             * be a bit larger due to file metadata.
             */
            H5FD_mem_t write_type = H5FD_MEM_DRAW;
            write_addr            = file_end_addr;
            write_status =
                H5FDwrite_vector(file_ptr, dxpl_id, 1, &write_type, &write_addr, &nbytes, &c_write_buf);
            VRFY((write_status >= 0), "H5FDwrite_vector succeeded");

            for (int j = 0; j < n_io_concentrators; j++) {
                h5_stat_size_t subfile_size;
                h5_stat_t      subfile_info;
                FILE          *subfile_ptr;

                HDsnprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                           (uint64_t)file_info.st_ino, num_digits, j + 1, n_io_concentrators);

                /* Ensure file exists */
                subfile_ptr = HDfopen(tmp_filename, "r");
                VRFY(subfile_ptr, "HDfopen on subfile succeeded");
                VRFY((HDfclose(subfile_ptr) >= 0), "HDfclose on subfile succeeded");

                /* Check file size */
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= 2 * stripe_size), "File size verification succeeded");
            }

            H5E_BEGIN_TRY
            {
                H5Fdelete(SUBF_FILENAME, fapl_id);
            }
            H5E_END_TRY;

            VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
            VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");
        }

        mpi_code = MPI_Barrier(comm_g);
        VRFY((mpi_code == MPI_SUCCESS), "MPI_Barrier succeeded");

        /* Next, try I/O with all ranks */

        fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, TRUE, &cfg, H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
        VRFY((fapl_id >= 0), "FAPL creation succeeded");

        /* Create and close file with H5Fcreate to setup superblock */
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /* Re-open file through H5FDopen for direct writes */
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        n_io_concentrators = (int) ((ioc_per_node_g > 0) ? ioc_per_node_g * ioc_comm_size : ioc_comm_size);
        num_digits         = (int)(HDlog10(n_io_concentrators) + 1);

        /*
         * Get the current file size to see where we can safely
         * write to in the file without overwriting the superblock
         */
        if (MAINPROCESS) {
            VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");
            file_size = (h5_stat_size_t)file_info.st_size;

            H5_CHECK_OVERFLOW(file_size, h5_stat_size_t, int64_t);
            file_size64 = (int64_t)file_size;
        }

        if (mpi_size > 1) {
            mpi_code = MPI_Bcast(&file_size64, 1, MPI_INT64_T, 0, comm_g);
            VRFY((mpi_code == MPI_SUCCESS), "MPI_Bcast succeeded");
        }

        H5_CHECK_OVERFLOW(file_size64, int64_t, haddr_t);
        file_end_addr = (haddr_t)file_size64;

        /* Set independent I/O on DXPL */
        VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_INDEPENDENT) >= 0), "H5Pset_dxpl_mpio succeeded");

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, file_end_addr + ((size_t)mpi_size * nbytes)) >= 0),
             "H5FDset_eoa succeeded");

        /*
         * Write "number of IOCs" X "stripe size" bytes to the file
         * from each rank and ensure that we have "number of IOCs"
         * sub-files, each with a size of at least "mpi size" * "stripe size"
         * bytes. The first (few) subfile(s) may be a bit larger
         * due to file metadata.
         */
        write_addr   = file_end_addr + ((size_t)mpi_rank * nbytes);
        write_status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
        VRFY((write_status >= 0), "H5FDwrite succeeded");

        file_end_addr += ((size_t)mpi_size * nbytes);

        mpi_code = MPI_Barrier(comm_g);
        VRFY((mpi_code == MPI_SUCCESS), "MPI_Barrier succeeded");

        if (MAINPROCESS) {
            for (int j = 0; j < n_io_concentrators; j++) {
                h5_stat_size_t subfile_size;
                h5_stat_t      subfile_info;
                FILE          *subfile_ptr;

                HDsnprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                           (uint64_t)file_info.st_ino, num_digits, j + 1, n_io_concentrators);

                /* Ensure file exists */
                subfile_ptr = HDfopen(tmp_filename, "r");
                VRFY(subfile_ptr, "HDfopen on subfile succeeded");
                VRFY((HDfclose(subfile_ptr) >= 0), "HDfclose on subfile succeeded");

                /* Check file size */
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= (mpi_size * stripe_size)), "File size verification succeeded");
            }
        }

        mpi_code = MPI_Barrier(comm_g);
        VRFY((mpi_code == MPI_SUCCESS), "MPI_Barrier succeeded");

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, file_end_addr + ((size_t)mpi_size * nbytes)) >= 0),
             "H5FDset_eoa succeeded");

        /*
         * Write another round of "number of IOCs" X "stripe size"
         * bytes to the file from each rank using vector I/O and
         * ensure we have "number of IOCs" sub-files, each with a
         * size of at least 2 * "mpi size" * "stripe size" bytes.
         * The first (few) subfile(s) may be a bit larger due to
         * file metadata.
         */
        H5FD_mem_t write_type = H5FD_MEM_DRAW;
        write_addr            = file_end_addr + ((size_t)mpi_rank * nbytes);
        write_status =
            H5FDwrite_vector(file_ptr, dxpl_id, 1, &write_type, &write_addr, &nbytes, &c_write_buf);
        VRFY((write_status >= 0), "H5FDwrite_vector succeeded");

        mpi_code = MPI_Barrier(comm_g);
        VRFY((mpi_code == MPI_SUCCESS), "MPI_Barrier succeeded");

        if (MAINPROCESS) {
            for (int j = 0; j < n_io_concentrators; j++) {
                h5_stat_size_t subfile_size;
                h5_stat_t      subfile_info;
                FILE          *subfile_ptr;

                HDsnprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                           (uint64_t)file_info.st_ino, num_digits, j + 1, n_io_concentrators);

                /* Ensure file exists */
                subfile_ptr = HDfopen(tmp_filename, "r");
                VRFY(subfile_ptr, "HDfopen on subfile succeeded");
                VRFY((HDfclose(subfile_ptr) >= 0), "HDfclose on subfile succeeded");

                /* Check file size */
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= (2 * mpi_size * stripe_size)), "File size verification succeeded");
            }
        }

        mpi_code = MPI_Barrier(comm_g);
        VRFY((mpi_code == MPI_SUCCESS), "MPI_Barrier succeeded");

        H5E_BEGIN_TRY
        {
            H5Fdelete(SUBF_FILENAME, fapl_id);
        }
        H5E_END_TRY;

        HDfree(write_buf);

        VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");
    }

    HDfree(tmp_filename);

    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME
#undef SUBF_NITER

/*
 * Test to check that an HDF5 file created with the
 * Subfiling VFD can be read back with a single MPI
 * rank
 */
#define SUBF_FILENAME  "test_subfiling_write_many_read_one.h5"
#define SUBF_HDF5_TYPE H5T_NATIVE_INT
#define SUBF_C_TYPE    int
static void
test_subfiling_write_many_read_one(void)
{
    hsize_t start[1];
    hsize_t count[1];
    hsize_t dset_dims[1];
    size_t  target_size;
    hid_t   file_id   = H5I_INVALID_HID;
    hid_t   fapl_id   = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    void   *buf       = NULL;
    int     mpi_code;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("reading back file with single MPI rank");

    /* Get a default Subfiling FAPL */
    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, FALSE, NULL, 0);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    /* Create file on all ranks */
    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

    /*
     * Attempt to create a dataset that is striped across
     * 2 IOCs
     */
    VRFY(((H5FD_SUBFILING_DEFAULT_STRIPE_SIZE % sizeof(SUBF_C_TYPE)) == 0),
            "I/O type size check succeeded");

    target_size = (2 * H5FD_SUBFILING_DEFAULT_STRIPE_SIZE);
    target_size -= (target_size % (size_t)mpi_size);

    VRFY((target_size >= H5FD_SUBFILING_DEFAULT_STRIPE_SIZE),
            "target size calculation succeeded");
    VRFY(((target_size % sizeof(SUBF_C_TYPE)) == 0),
            "target size check succeeded");

    dset_dims[0] = (hsize_t) (target_size / sizeof(SUBF_C_TYPE));

    fspace_id = H5Screate_simple(1, dset_dims, NULL);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    dset_id = H5Dcreate2(file_id, "DSET", SUBF_HDF5_TYPE, fspace_id,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Select hyperslab */
    count[0] = dset_dims[0] / (hsize_t)mpi_size;
    start[0] = (hsize_t)mpi_rank * count[0];
    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) >= 0),
            "H5Sselect_hyperslab succeeded");

    buf = HDmalloc(count[0] * sizeof(SUBF_C_TYPE));
    VRFY(buf, "HDmalloc succeeded");

    for (size_t i = 0; i < count[0]; i++)
        ((SUBF_C_TYPE *)buf)[i] = (SUBF_C_TYPE) ((size_t)mpi_rank + i);

    VRFY((H5Dwrite(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, fspace_id, H5P_DEFAULT, buf) >= 0),
            "Dataset write succeeded");

    HDfree(buf);
    buf = NULL;

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    if (MAINPROCESS) {
        fapl_id = create_subfiling_ioc_fapl(MPI_COMM_SELF, MPI_INFO_NULL, FALSE, NULL, 0);
        VRFY((fapl_id >= 0), "FAPL creation succeeded");

        file_id = H5Fopen(SUBF_FILENAME, H5F_ACC_RDONLY, fapl_id);
        VRFY((file_id >= 0), "H5Fopen succeeded");

        dset_id = H5Dopen2(file_id, "DSET", H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        buf = HDmalloc(target_size);
        VRFY(buf, "HDmalloc succeeded");

        VRFY((H5Dread(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, H5S_ALL, H5P_DEFAULT, buf) >= 0),
                "Dataset read succeeded");

        for (size_t i = 0; i < (size_t)mpi_size; i++) {
            for (size_t j = 0; j < count[0]; j++) {
                SUBF_C_TYPE buf_value = ((SUBF_C_TYPE *)buf)[(i * count[0]) + j];

                VRFY((buf_value == (SUBF_C_TYPE) (j + i)), "data verification succeeded");
            }
        }

        HDfree(buf);
        buf = NULL;

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

        H5E_BEGIN_TRY
        {
            H5Fdelete(SUBF_FILENAME, fapl_id);
        }
        H5E_END_TRY;

        VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
    }

    mpi_code = MPI_Barrier(comm_g);
    VRFY((mpi_code == MPI_SUCCESS), "MPI_Barrier succeeded");

    VRFY((H5Sclose(fspace_id) >= 0), "FAPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME
#undef SUBF_HDF5_TYPE
#undef SUBF_C_TYPE

#define SUBF_FILENAME  "test_subfiling_write_many_read_few.h5"
#define SUBF_HDF5_TYPE H5T_NATIVE_INT
#define SUBF_C_TYPE    int
static void
test_subfiling_write_many_read_few(void)
{

}

int
main(int argc, char **argv)
{
    time_t seed;
    char  *env_value;
    int    required = MPI_THREAD_MULTIPLE;
    int    provided = 0;
    int    mpi_code;

    /* Initialize MPI */
    if (MPI_SUCCESS != (mpi_code = MPI_Init_thread(&argc, &argv, required, &provided))) {
        HDprintf("MPI_Init_thread failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }

    if (provided != required) {
        HDprintf("MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE\n");
        nerrors++;
        goto exit;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(comm_g, &mpi_size))) {
        HDprintf("MPI_Comm_size failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm_g, &mpi_rank))) {
        HDprintf("MPI_Comm_rank failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }

    /* Split communicator according to node-local ranks */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_split_type(comm_g, MPI_COMM_TYPE_SHARED, mpi_rank, MPI_INFO_NULL,
                                                       &node_local_comm))) {
        HDprintf("MPI_Comm_split_type failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(node_local_comm, &node_local_size))) {
        HDprintf("MPI_Comm_size failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(node_local_comm, &node_local_rank))) {
        HDprintf("MPI_Comm_rank failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }

    /*
     * Split communicator according to rank value across nodes.
     * If the SELECT_IOC_ONE_PER_NODE IOC selection strategy is
     * used, each rank with a node local rank value of 0 will
     * be an IOC in the new communicator.
     */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_split(comm_g, node_local_rank, mpi_rank, &ioc_comm))) {
        HDprintf("MPI_Comm_split failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(ioc_comm, &ioc_comm_size))) {
        HDprintf("MPI_Comm_size failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(ioc_comm, &ioc_comm_rank))) {
        HDprintf("MPI_Comm_rank failed with error code %d\n", mpi_code);
        nerrors++;
        goto exit;
    }

    if (H5dont_atexit() < 0) {
        if (MAINPROCESS)
            HDprintf("Failed to turn off atexit processing. Continue.\n");
    }

    H5open();

    /* Enable selection I/O using internal temporary workaround */
    H5_use_selection_io_g = TRUE;

    if (MAINPROCESS) {
        HDprintf("Testing Subfiling VFD functionality\n");
    }

    TestAlarmOn();

#if 0 /* TODO: No provision for subfiling sub-directory in VFD yet */
    /* Create directories for test-generated .h5 files */
    if ((HDmkdir(SUBFILING_TEST_DIR, (mode_t)0755) < 0) && (errno != EEXIST)) {
        HDprintf("couldn't create subfiling testing directory\n");
        nerrors++;
        goto exit;
    }
#endif

    seed = time(NULL);
    srand((unsigned)seed);

    if (MAINPROCESS)
        HDprintf("Using seed: %lld\n\n", (long long)seed);

    /* Grab values from global variables if set */
    if (NULL != (env_value = HDgetenv(H5FD_SUBFILING_STRIPE_SIZE))) {
        stripe_size_g = HDstrtoll(env_value, NULL, 0);
        if ((ERANGE == errno) || (stripe_size_g <= 0))
            stripe_size_g = -1;
    }

    if (NULL != (env_value = HDgetenv(H5FD_SUBFILING_IOC_PER_NODE))) {
        ioc_per_node_g = HDstrtol(env_value, NULL, 0);
        if ((ERANGE == errno) || (ioc_per_node_g <= 0))
            ioc_per_node_g = -1;
        else if (ioc_per_node_g * ioc_comm_size > mpi_size)
            /*
             * If the number of IOCs per node from the environment
             * causes the total number of IOCs to exceed the number
             * of MPI ranks available, the Subfiling VFD will simply
             * use all of the MPI ranks on a node as IOCs
             */
            ioc_per_node_g = node_local_size;
    }

    if (NULL != (env_value = HDgetenv(H5FD_IOC_THREAD_POOL_SIZE))) {
        ioc_thread_pool_size_g = HDatoi(env_value);
        if (ioc_thread_pool_size_g <= 0)
            ioc_thread_pool_size_g = -1;
    }

    for (size_t i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code = MPI_Barrier(comm_g))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS)
                MESG("MPI_Barrier failed");
            nerrors++;
        }
    }

    if (nerrors)
        goto exit;

    if (MAINPROCESS)
        HDputs("All Subfiling VFD tests passed\n");

exit:
    if (nerrors) {
        if (MAINPROCESS)
            HDprintf("*** %d TEST ERROR%s OCCURRED ***\n", nerrors, nerrors > 1 ? "S" : "");
    }

    TestAlarmOff();

    H5close();

    MPI_Comm_free(&ioc_comm);
    MPI_Comm_free(&node_local_comm);

    MPI_Finalize();

    HDexit(nerrors ? EXIT_FAILURE : EXIT_SUCCESS);
}

#else /* H5_HAVE_SUBFILING_VFD */

int
main(void)
{
    h5_reset();
    HDprintf("Testing Subfiling VFD functionality\n");
    HDprintf("SKIPPED - Subfiling VFD not built\n");
    HDexit(EXIT_SUCCESS);
}

#endif /* H5_HAVE_SUBFILING_VFD */
