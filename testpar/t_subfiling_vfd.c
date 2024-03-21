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

/*
 * HDF5 Subfiling VFD tests
 *
 * NOTE: these tests currently assume that the default I/O concentrator
 *       selection strategy for the Subfiling VFD is to use 1 I/O
 *       concentrator per node. If that changes in the future, some of
 *       these tests will need updating.
 */

#include <mpi.h>

#include "testpar.h"
#include "H5srcdir.h"
#include "H5MMprivate.h"

#ifdef H5_HAVE_SUBFILING_VFD

#include "H5FDsubfiling.h"
#include "H5FDioc.h"

/* The smallest Subfiling stripe size used for testing */
#define SUBFILING_MIN_STRIPE_SIZE 128

/* Temporary test directory */
#define SUBFILING_CONFIG_FILE_DIR "subfiling_config_file_dir"

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define DEFAULT_DEFLATE_LEVEL 4

#define ARRAY_SIZE(a) sizeof(a) / sizeof(a[0])

#define CHECK_PASSED()                                                                                       \
    do {                                                                                                     \
        int err_result = (nerrors > curr_nerrors);                                                           \
                                                                                                             \
        mpi_code_g = MPI_Allreduce(MPI_IN_PLACE, &err_result, 1, MPI_INT, MPI_MAX, comm_g);                  \
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Allreduce succeeded");                                        \
                                                                                                             \
        if (MAINPROCESS) {                                                                                   \
            if (err_result == 0)                                                                             \
                PASSED();                                                                                    \
            else                                                                                             \
                H5_FAILED();                                                                                 \
        }                                                                                                    \
    } while (0)

static MPI_Comm comm_g = MPI_COMM_WORLD;
static MPI_Info info_g = MPI_INFO_NULL;
static int      mpi_rank;
static int      mpi_size;
static int      mpi_code_g;
static int      num_nodes_g;
static int      num_iocs_g;

static MPI_Comm node_local_comm = MPI_COMM_WORLD;
static int      node_local_rank;
static int      node_local_size;

static MPI_Comm ioc_comm = MPI_COMM_WORLD;
static int      ioc_comm_rank;
static int      ioc_comm_size;

static long long stripe_size_g          = -1;
static long      ioc_per_node_g         = -1;
static int       ioc_thread_pool_size_g = -1;

static char *config_dir = NULL;

int nerrors      = 0;
int curr_nerrors = 0;

bool enable_compression = false;

/* Function pointer typedef for test functions */
typedef void (*test_func)(void);

/* Utility functions */
static hid_t create_subfiling_ioc_fapl(MPI_Comm comm, MPI_Info info, bool custom_config,
                                       H5FD_subfiling_params_t *custom_cfg, int32_t thread_pool_size);
static hid_t create_dcpl_id(int rank, const hsize_t dims[], hid_t dxpl_id);

/* Test functions */
static void test_create_and_close(void);
static void test_ioc_only_fail(void);
static void test_config_file(void);
static void test_stripe_sizes(void);
static void test_iovec_translation(void);
static void test_selection_strategies(void);
static void test_read_different_stripe_size(void);
static void test_subfiling_precreate_rank_0(void);
static void test_subfiling_write_many_read_one(void);
static void test_subfiling_write_many_read_few(void);
static void test_subfiling_h5fuse(void);

static test_func tests[] = {
    test_create_and_close,
    test_ioc_only_fail,
    test_config_file,
    test_stripe_sizes,
    test_iovec_translation,
    test_selection_strategies,
    test_read_different_stripe_size,
    test_subfiling_precreate_rank_0,
    test_subfiling_write_many_read_one,
    test_subfiling_write_many_read_few,
    test_subfiling_h5fuse,
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
create_subfiling_ioc_fapl(MPI_Comm comm, MPI_Info info, bool custom_config,
                          H5FD_subfiling_params_t *custom_cfg, int32_t thread_pool_size)
{
    H5FD_subfiling_config_t subfiling_conf;
    H5FD_ioc_config_t       ioc_conf;
    hid_t                   ret_value = H5I_INVALID_HID;

    assert(!custom_config || custom_cfg);

    if ((ret_value = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if (H5Pset_mpi_params(ret_value, comm, info) < 0)
        TEST_ERROR;

    if (!custom_config) {
        if (H5Pset_fapl_subfiling(ret_value, NULL) < 0)
            TEST_ERROR;
    }
    else {
        /* Get defaults for Subfiling configuration */
        if (H5Pget_fapl_subfiling(ret_value, &subfiling_conf) < 0)
            TEST_ERROR;

        /* Set custom configuration */
        subfiling_conf.shared_cfg = *custom_cfg;

        if (subfiling_conf.require_ioc) {
            /* Get IOC VFD defaults */
            if (H5Pget_fapl_ioc(ret_value, &ioc_conf) < 0)
                TEST_ERROR;

            /* Set custom configuration */
            ioc_conf.thread_pool_size = thread_pool_size;

            if (H5Pset_fapl_ioc(subfiling_conf.ioc_fapl_id, &ioc_conf) < 0)
                TEST_ERROR;
        }
        else {
            if (H5Pset_fapl_sec2(subfiling_conf.ioc_fapl_id) < 0)
                TEST_ERROR;
        }

        if (H5Pset_fapl_subfiling(ret_value, &subfiling_conf) < 0)
            TEST_ERROR;
    }

    return ret_value;

error:
    if ((H5I_INVALID_HID != ret_value) && (H5Pclose(ret_value) < 0)) {
        H5_FAILED();
        AT();
    }

    return H5I_INVALID_HID;
}
/* ---------------------------------------------------------------------------
 * Function:    create_dcpl_id
 *
 * Purpose:     Creates dataset creation property list identifier with
 *              chunking and compression, and enforces the
 *              required collective IO.
 *
 * Return:      Success: HID Dataset creation property list identifier,
 *                       a non-negative value.
 *              Failure: H5I_INVALID_HID, a negative value.
 * ---------------------------------------------------------------------------
 */
static hid_t
create_dcpl_id(int rank, const hsize_t dset_dims[], hid_t dxpl_id)
{
    hsize_t chunk_dims[1];
    hid_t   ret_value = H5I_INVALID_HID;

    if ((ret_value = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (enable_compression) {
        if (H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) < 0)
            TEST_ERROR;
        chunk_dims[0] = dset_dims[0] / 2;
        if (H5Pset_chunk(ret_value, rank, chunk_dims) < 0)
            TEST_ERROR;
        if (H5Pset_deflate(ret_value, DEFAULT_DEFLATE_LEVEL) < 0)
            TEST_ERROR;
    }

    return ret_value;

error:
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
    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, false, NULL, 0);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    H5E_BEGIN_TRY
    {
        H5Fdelete(SUBF_FILENAME, fapl_id);
    }
    H5E_END_TRY

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME

/*
 * A simple test that ensures file creation fails when
 * attempting to use the IOC VFD by itself, without it
 * being stacked under the Subfiling VFD. This is
 * currently unsupported.
 */
#define SUBF_FILENAME "test_subfiling_only_ioc_fail.h5"
static void
test_ioc_only_fail(void)
{
    hid_t file_id = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("invalid use of IOC VFD by itself");

    /* Setup a FAPL using only the IOC VFD */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl_id >= 0), "H5Pcreate succeeded");

    VRFY((H5Pset_mpi_params(fapl_id, comm_g, info_g) >= 0), "H5Pset_mpi_params succeeded");

    VRFY((H5Pset_fapl_ioc(fapl_id, NULL) >= 0), "H5Pset_fapl_ioc succeeded");

    H5E_BEGIN_TRY
    {
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    }
    H5E_END_TRY
    VRFY((file_id < 0), "H5Fcreate failed successfully");

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
    H5FD_subfiling_params_t cfg;
    int64_t                 stripe_size;
    int64_t                 read_stripe_size;
    FILE                   *config_file;
    char                   *config_filename = NULL;
    char                   *config_buf      = NULL;
    HDoff_t                 config_file_len;
    hid_t                   file_id = H5I_INVALID_HID;
    hid_t                   fapl_id = H5I_INVALID_HID;
    int                     read_stripe_count;
    int                     read_aggr_count;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("subfiling configuration file format");

    /*
     * Choose a random Subfiling stripe size between
     * the smallest allowed value and 32MiB
     */
    if (mpi_rank == 0) {
        stripe_size = (rand() % (H5FD_SUBFILING_DEFAULT_STRIPE_SIZE - SUBFILING_MIN_STRIPE_SIZE + 1)) +
                      SUBFILING_MIN_STRIPE_SIZE;
    }

    if (mpi_size > 1) {
        mpi_code_g = MPI_Bcast(&stripe_size, 1, MPI_INT64_T, 0, comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Bcast succeeded");
    }

    cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
    cfg.stripe_size   = (stripe_size_g > 0) ? stripe_size_g : stripe_size;
    cfg.stripe_count  = num_iocs_g > 1 ? (num_iocs_g / 2) : 1;

    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, true, &cfg, H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
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

        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        config_filename = malloc(PATH_MAX);
        VRFY(config_filename, "malloc succeeded");

        snprintf(config_filename, PATH_MAX, "%s/" H5FD_SUBFILING_CONFIG_FILENAME_TEMPLATE, config_dir,
                 SUBF_FILENAME, (uint64_t)file_info.st_ino);

        config_file = fopen(config_filename, "r");
        VRFY(config_file, "fopen succeeded");

        free(config_filename);

        VRFY((HDfseek(config_file, 0, SEEK_END) >= 0), "HDfseek succeeded");

        config_file_len = HDftell(config_file);
        VRFY((config_file_len > 0), "HDftell succeeded");

        VRFY((HDfseek(config_file, 0, SEEK_SET) >= 0), "HDfseek succeeded");

        config_buf = malloc((size_t)config_file_len + 1);
        VRFY(config_buf, "malloc succeeded");

        VRFY((fread(config_buf, (size_t)config_file_len, 1, config_file) == 1), "fread succeeded");
        config_buf[config_file_len] = '\0';

        /* Check the stripe_size field in the configuration file */
        substr = strstr(config_buf, "stripe_size");
        VRFY(substr, "strstr succeeded");

        VRFY((sscanf(substr, "stripe_size=%" PRId64, &read_stripe_size) == 1), "sscanf succeeded");
        VRFY((read_stripe_size == cfg.stripe_size), "Stripe size comparison succeeded");

        /* Check the aggregator_count field in the configuration file */
        substr = strstr(config_buf, "aggregator_count");
        VRFY(substr, "strstr succeeded");

        VRFY((sscanf(substr, "aggregator_count=%d", &read_aggr_count) == 1), "sscanf succeeded");
        if (cfg.stripe_count < num_iocs_g)
            VRFY((read_aggr_count == cfg.stripe_count), "Aggregator count comparison succeeded");
        else
            VRFY((read_aggr_count == num_iocs_g), "Aggregator count comparison succeeded");

        /* Check the subfile_count field in the configuration file */
        substr = strstr(config_buf, "subfile_count");
        VRFY(substr, "strstr succeeded");

        VRFY((sscanf(substr, "subfile_count=%d", &read_stripe_count) == 1), "sscanf succeeded");
        VRFY((read_stripe_count == cfg.stripe_count), "Stripe count comparison succeeded");

        /* Check the hdf5_file and subfile_dir fields in the configuration file */
        resolved_path = HDrealpath(SUBF_FILENAME, NULL);
        VRFY(resolved_path, "HDrealpath succeeded");

        VRFY((H5_dirname(resolved_path, &subfile_dir) >= 0), "H5_dirname succeeded");

        tmp_buf = malloc(PATH_MAX);
        VRFY(tmp_buf, "malloc succeeded");

        substr = strstr(config_buf, "hdf5_file");
        VRFY(substr, "strstr succeeded");

        H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
        snprintf(scan_format, sizeof(scan_format), "hdf5_file=%%%zus", (size_t)(PATH_MAX - 1));
        VRFY((sscanf(substr, scan_format, tmp_buf) == 1), "sscanf succeeded");
        H5_GCC_CLANG_DIAG_ON("format-nonliteral")

        VRFY((strcmp(tmp_buf, resolved_path) == 0), "strcmp succeeded");

        substr = strstr(config_buf, "subfile_dir");
        VRFY(substr, "strstr succeeded");

        H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
        snprintf(scan_format, sizeof(scan_format), "subfile_dir=%%%zus", (size_t)(PATH_MAX - 1));
        VRFY((sscanf(substr, scan_format, tmp_buf) == 1), "sscanf succeeded");
        H5_GCC_CLANG_DIAG_ON("format-nonliteral")

        VRFY((strcmp(tmp_buf, subfile_dir) == 0), "strcmp succeeded");

        free(tmp_buf);
        H5MM_free(subfile_dir);
        free(resolved_path);

        subfile_name = malloc(PATH_MAX);
        VRFY(subfile_name, "malloc succeeded");

        /* Verify the name of each subfile is in the configuration file */
        num_digits = (int)(log10(cfg.stripe_count) + 1);
        for (size_t i = 0; i < (size_t)cfg.stripe_count; i++) {
            snprintf(subfile_name, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, (int)i + 1, cfg.stripe_count);

            substr = strstr(config_buf, subfile_name);
            VRFY(substr, "strstr succeeded");
        }

        /* Verify that there aren't too many subfiles */
        snprintf(subfile_name, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                 (uint64_t)file_info.st_ino, num_digits, (int)cfg.stripe_count + 1, cfg.stripe_count);
        substr = strstr(config_buf, subfile_name);
        VRFY(substr == NULL, "strstr correctly failed");

        free(subfile_name);
        free(config_buf);

        VRFY((fclose(config_file) >= 0), "fclose on configuration file succeeded");
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    H5E_BEGIN_TRY
    {
        H5Fdelete(SUBF_FILENAME, fapl_id);
    }
    H5E_END_TRY

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
    int     num_subfiles;
    int     num_digits;
    hid_t   fapl_id = H5I_INVALID_HID;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("random subfiling stripe sizes");

    tmp_filename = malloc(PATH_MAX);
    VRFY(tmp_filename, "malloc succeeded");

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "DXPL creation succeeded");

    /* Set selection I/O mode on DXPL */
    VRFY((H5Pset_selection_io(dxpl_id, H5D_SELECTION_IO_MODE_ON) >= 0), "H5Pset_selection_io succeeded");

    for (size_t i = 0; i < SUBF_NITER; i++) {
        H5FD_subfiling_params_t cfg;
        h5_stat_size_t          file_size;
        const void             *c_write_buf;
        h5_stat_t               file_info;
        int64_t                 file_size64;
        int64_t                 stripe_size;
        haddr_t                 file_end_addr;
        haddr_t                 write_addr;
        size_t                  nbytes;
        herr_t                  write_status;
        hid_t                   file_id;

        /*
         * Choose a random Subfiling stripe size between
         * the smallest allowed value and the default value
         */
        if (mpi_rank == 0) {
            stripe_size = (rand() % (H5FD_SUBFILING_DEFAULT_STRIPE_SIZE - SUBFILING_MIN_STRIPE_SIZE + 1)) +
                          SUBFILING_MIN_STRIPE_SIZE;
        }

        if (mpi_size > 1) {
            mpi_code_g = MPI_Bcast(&stripe_size, 1, MPI_INT64_T, 0, comm_g);
            VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Bcast succeeded");
        }

        cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
        cfg.stripe_size   = (stripe_size_g > 0) ? stripe_size_g : stripe_size;
        cfg.stripe_count  = 1;

        /* First, try I/O with a single rank */
        if (MAINPROCESS) {
            FILE *subfile_ptr;

            num_subfiles = 1;
            num_digits   = (int)(log10(num_subfiles) + 1);

            nbytes = (size_t)(cfg.stripe_size * num_subfiles);

            write_buf = malloc(nbytes);
            VRFY(write_buf, "malloc succeeded");

            memset(write_buf, 255, nbytes);

            c_write_buf = write_buf;

            fapl_id = create_subfiling_ioc_fapl(MPI_COMM_SELF, MPI_INFO_NULL, true, &cfg,
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
            memset(&file_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");
            file_size = (h5_stat_size_t)file_info.st_size;

            H5_CHECK_OVERFLOW(file_size, h5_stat_size_t, haddr_t);
            file_end_addr = (haddr_t)file_size;

            /* Set independent I/O on DXPL */
            VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_INDEPENDENT) >= 0), "H5Pset_dxpl_mpio succeeded");

            /* Set EOA for following write call */
            VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, file_end_addr + nbytes) >= 0),
                 "H5FDset_eoa succeeded");

            /*
             * Write "number of IOCs" X "stripe size" bytes to the file
             * and ensure that we have "number of IOCs" subfiles, each
             * with a size of at least "stripe size" bytes. The first
             * (few) subfile(s) may be a bit larger due to file metadata.
             */
            write_addr   = file_end_addr;
            write_status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
            VRFY((write_status >= 0), "H5FDwrite succeeded");

            file_end_addr += nbytes;

            VRFY((H5FDtruncate(file_ptr, dxpl_id, 0) >= 0), "H5FDtruncate succeeded");

            for (int j = 0; j < num_subfiles; j++) {
                h5_stat_size_t subfile_size;
                h5_stat_t      subfile_info;

                snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                         (uint64_t)file_info.st_ino, num_digits, j + 1, num_subfiles);

                /* Ensure file exists */
                subfile_ptr = fopen(tmp_filename, "r");
                VRFY(subfile_ptr, "fopen on subfile succeeded");
                VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

                /* Check file size */
                memset(&subfile_info, 0, sizeof(h5_stat_t));
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= cfg.stripe_size), "File size verification succeeded");
            }

            /* Verify that there aren't too many subfiles */
            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

            /* Ensure file doesn't exist */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

            /* Set EOA for following write call */
            VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, file_end_addr + nbytes) >= 0),
                 "H5FDset_eoa succeeded");

            /*
             * Write another round of "number of IOCs" X "stripe size"
             * bytes to the file using vector I/O and ensure we have
             * "number of IOCs" subfiles, each with a size of at least
             * 2 * "stripe size" bytes. The first (few) subfile(s) may
             * be a bit larger due to file metadata.
             */
            H5FD_mem_t write_type = H5FD_MEM_DRAW;
            write_addr            = file_end_addr;
            write_status =
                H5FDwrite_vector(file_ptr, dxpl_id, 1, &write_type, &write_addr, &nbytes, &c_write_buf);
            VRFY((write_status >= 0), "H5FDwrite_vector succeeded");

            VRFY((H5FDtruncate(file_ptr, dxpl_id, 0) >= 0), "H5FDtruncate succeeded");

            for (int j = 0; j < num_subfiles; j++) {
                h5_stat_size_t subfile_size;
                h5_stat_t      subfile_info;

                snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                         (uint64_t)file_info.st_ino, num_digits, j + 1, num_subfiles);

                /* Ensure file exists */
                subfile_ptr = fopen(tmp_filename, "r");
                VRFY(subfile_ptr, "fopen on subfile succeeded");
                VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

                /* Check file size */
                memset(&subfile_info, 0, sizeof(h5_stat_t));
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= 2 * cfg.stripe_size), "File size verification succeeded");
            }

            /* Verify that there aren't too many subfiles */
            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

            /* Ensure file doesn't exist */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

            free(write_buf);
            write_buf = NULL;

            VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");

            H5E_BEGIN_TRY
            {
                H5Fdelete(SUBF_FILENAME, fapl_id);
            }
            H5E_END_TRY

            VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
        }

        mpi_code_g = MPI_Barrier(comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

        /* Next, try I/O with all ranks */

        cfg.stripe_count = num_iocs_g;

        fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, true, &cfg, H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
        VRFY((fapl_id >= 0), "FAPL creation succeeded");

        /* Create and close file with H5Fcreate to setup superblock */
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /* Re-open file through H5FDopen for direct writes */
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        num_subfiles = num_iocs_g;
        num_digits   = (int)(log10(num_subfiles) + 1);

        nbytes = (size_t)(cfg.stripe_size * num_subfiles);

        write_buf = malloc(nbytes);
        VRFY(write_buf, "malloc succeeded");

        memset(write_buf, 255, nbytes);

        c_write_buf = write_buf;

        /*
         * Get the current file size to see where we can safely
         * write to in the file without overwriting the superblock
         */
        if (MAINPROCESS) {
            memset(&file_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");
            file_size = (h5_stat_size_t)file_info.st_size;

            H5_CHECK_OVERFLOW(file_size, h5_stat_size_t, int64_t);
            file_size64 = (int64_t)file_size;
        }

        if (mpi_size > 1) {
            mpi_code_g = MPI_Bcast(&file_size64, 1, MPI_INT64_T, 0, comm_g);
            VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Bcast succeeded");
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
         * subfiles, each with a size of at least "mpi size" * "stripe size"
         * bytes. The first (few) subfile(s) may be a bit larger
         * due to file metadata.
         */
        write_addr   = file_end_addr + ((size_t)mpi_rank * nbytes);
        write_status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
        VRFY((write_status >= 0), "H5FDwrite succeeded");

        file_end_addr += ((size_t)mpi_size * nbytes);

        VRFY((H5FDtruncate(file_ptr, dxpl_id, 0) >= 0), "H5FDtruncate succeeded");

        mpi_code_g = MPI_Barrier(comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

        if (MAINPROCESS) {
            FILE *subfile_ptr;

            for (int j = 0; j < num_subfiles; j++) {
                h5_stat_size_t subfile_size;
                h5_stat_t      subfile_info;

                snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                         (uint64_t)file_info.st_ino, num_digits, j + 1, num_subfiles);

                /* Ensure file exists */
                subfile_ptr = fopen(tmp_filename, "r");
                VRFY(subfile_ptr, "fopen on subfile succeeded");
                VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

                /* Check file size */
                memset(&subfile_info, 0, sizeof(h5_stat_t));
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= (mpi_size * cfg.stripe_size)), "File size verification succeeded");
            }

            /* Verify that there aren't too many subfiles */
            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

            /* Ensure file doesn't exist */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");
        }

        mpi_code_g = MPI_Barrier(comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, file_end_addr + ((size_t)mpi_size * nbytes)) >= 0),
             "H5FDset_eoa succeeded");

        /*
         * Write another round of "number of IOCs" X "stripe size"
         * bytes to the file from each rank using vector I/O and
         * ensure we have "number of IOCs" subfiles, each with a
         * size of at least 2 * "mpi size" * "stripe size" bytes.
         * The first (few) subfile(s) may be a bit larger due to
         * file metadata.
         */
        H5FD_mem_t write_type = H5FD_MEM_DRAW;
        write_addr            = file_end_addr + ((size_t)mpi_rank * nbytes);
        write_status =
            H5FDwrite_vector(file_ptr, dxpl_id, 1, &write_type, &write_addr, &nbytes, &c_write_buf);
        VRFY((write_status >= 0), "H5FDwrite_vector succeeded");

        VRFY((H5FDtruncate(file_ptr, dxpl_id, 0) >= 0), "H5FDtruncate succeeded");

        mpi_code_g = MPI_Barrier(comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

        if (MAINPROCESS) {
            FILE *subfile_ptr;

            for (int j = 0; j < num_subfiles; j++) {
                h5_stat_size_t subfile_size;
                h5_stat_t      subfile_info;

                snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                         (uint64_t)file_info.st_ino, num_digits, j + 1, num_subfiles);

                /* Ensure file exists */
                subfile_ptr = fopen(tmp_filename, "r");
                VRFY(subfile_ptr, "fopen on subfile succeeded");
                VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

                /* Check file size */
                memset(&subfile_info, 0, sizeof(h5_stat_t));
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= (2 * mpi_size * cfg.stripe_size)), "File size verification succeeded");
            }

            /* Verify that there aren't too many subfiles */
            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

            /* Ensure file doesn't exist */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");
        }

        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");

        mpi_code_g = MPI_Barrier(comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

        H5E_BEGIN_TRY
        {
            H5Fdelete(SUBF_FILENAME, fapl_id);
        }
        H5E_END_TRY

        free(write_buf);

        VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
    }

    free(tmp_filename);

    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME
#undef SUBF_NITER

/*
 * Test the I/O vector translation code by writing with some
 * different specific I/O patterns
 */
#define SUBF_FILENAME "test_subfiling_iovec_translation.h5"
static void
test_iovec_translation(void)
{
    H5FD_subfiling_params_t cfg;
    const void             *c_write_buf;
    h5_stat_t               file_info;
    int64_t                 stripe_size;
    haddr_t                 write_addr;
    size_t                  nbytes;
    size_t                  buf_size;
    herr_t                  status;
    hid_t                   file_id;
    H5FD_t                 *file_ptr     = NULL;
    FILE                   *subfile_ptr  = NULL;
    void                   *write_buf    = NULL;
    void                   *read_buf     = NULL;
    char                   *tmp_filename = NULL;
    hid_t                   dxpl_id      = H5I_INVALID_HID;
    hid_t                   fapl_id      = H5I_INVALID_HID;
    bool                    skip         = false;
    int                     num_subfiles;
    int                     num_digits;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("I/O vector translation");

    /*
     * Don't run this test if subfiling configuration
     * environment variables have been set since we
     * want to use fixed configurations for testing.
     */
    if (getenv(H5FD_SUBFILING_STRIPE_SIZE) || getenv(H5FD_SUBFILING_IOC_PER_NODE))
        skip = true;

    /* I/O only needs to be done from a single rank */
    if (MAINPROCESS && !skip) {

        /* Use a fixed configuration for these tests */
        stripe_size  = 1048576;
        num_subfiles = 4;
        num_digits   = (int)(log10(num_subfiles) + 1);

        /* Allocate enough buffer space for up to 2 "subfile blocks" of I/O */
        buf_size  = (size_t)(2 * stripe_size * num_subfiles);
        write_buf = malloc(buf_size);
        VRFY(write_buf, "malloc succeeded");
        read_buf = malloc(buf_size);
        VRFY(read_buf, "malloc succeeded");

        c_write_buf = write_buf;

        tmp_filename = malloc(PATH_MAX);
        VRFY(tmp_filename, "malloc succeeded");

        dxpl_id = H5Pcreate(H5P_DATASET_XFER);
        VRFY((dxpl_id >= 0), "DXPL creation succeeded");

        /* Set selection I/O mode on DXPL */
        VRFY((H5Pset_selection_io(dxpl_id, H5D_SELECTION_IO_MODE_ON) >= 0), "H5Pset_selection_io succeeded");

        cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
        cfg.stripe_size   = stripe_size;
        cfg.stripe_count  = 4;

        fapl_id = create_subfiling_ioc_fapl(MPI_COMM_SELF, MPI_INFO_NULL, true, &cfg,
                                            H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
        VRFY((fapl_id >= 0), "FAPL creation succeeded");

        /* Set independent I/O on DXPL */
        VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_INDEPENDENT) >= 0), "H5Pset_dxpl_mpio succeeded");

        /*
         * Test the case where the index value of the last subfile
         * touched by I/O is greater than or equal to the index
         * value of the first subfile touched by I/O, and this results
         * in "thin" I/O segments directed to the subfiles with index
         * values greater than the index values of the first and
         * last subfiles. This might appear as the following I/O
         * pattern:
         *
         *   SUBFILE 0   SUBFILE 1   SUBFILE 2   SUBFILE 3
         *  _______________________________________________
         * |   XXXXX   |   XXXXX   |   XXXXX   |   XXXXX   | ROW 0
         * |   XXXXX   |   XXXXX   |           |           | ROW 1
         * |           |           |           |           | ROW 2
         * |           |           |           |           | ROW ...
         * |           |           |           |           |
         * |           |           |           |           |
         * |           |           |           |           |
         * |___________|___________|___________|___________|
         */

        /* Create/truncate the file */
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /* Retrieve file info to get the file inode for later use */
        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        /* Re-open file through H5FDopen for direct writes */
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        nbytes = (size_t)(6 * stripe_size);
        memset(write_buf, 255, nbytes);
        memset(read_buf, 0, buf_size);

        write_addr = 0;

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Write according to the above pattern */
        status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        /* Close and re-open the file */
        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        /*
         * Set EOA for following read call (since we wrote over any
         * superblock information in the file)
         */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Read the written bytes and verify */
        status = H5FDread(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, read_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        VRFY((0 == memcmp(write_buf, read_buf, nbytes)), "memcmp succeeded");

        /* Verify the size of each subfile */
        for (int i = 0; i < num_subfiles; i++) {
            h5_stat_size_t subfile_size;
            h5_stat_t      subfile_info;

            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, i + 1, num_subfiles);

            /* Ensure file exists */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr, "fopen on subfile succeeded");
            VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

            /* Check file size */
            memset(&subfile_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
            subfile_size = (h5_stat_size_t)subfile_info.st_size;

            if (i <= 1) {
                /*
                 * Subfiles with index values <= 1 should have full
                 * I/O segments (2 * stripe size) written to them.
                 */
                VRFY((subfile_size == 2 * cfg.stripe_size), "File size verification succeeded");
            }
            else {
                /*
                 * Subfiles with index values > 1 should have "thin"
                 * I/O segments (1 * stripe size) written to them.
                 */
                VRFY((subfile_size == cfg.stripe_size), "File size verification succeeded");
            }
        }

        /* Verify that there aren't too many subfiles */
        snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                 (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

        /* Ensure file doesn't exist */
        subfile_ptr = fopen(tmp_filename, "r");
        VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");

        /*
         * Test the case where the index value of the last subfile
         * touched by I/O is greater than or equal to the index
         * value of the first subfile touched by I/O, and this results
         * in "thin" I/O segments directed to the subfiles with index
         * values less than the index values of the first and
         * last subfiles. This might appear as the following I/O
         * pattern:
         *
         *   SUBFILE 0   SUBFILE 1   SUBFILE 2   SUBFILE 3
         *  _______________________________________________
         * |           |   XXXXX   |   XXXXX   |   XXXXX   | ROW 0
         * |   XXXXX   |   XXXXX   |   XXXXX   |   XXXXX   | ROW 1
         * |           |           |           |           | ROW 2
         * |           |           |           |           | ROW ...
         * |           |           |           |           |
         * |           |           |           |           |
         * |           |           |           |           |
         * |___________|___________|___________|___________|
         */

        /* Create/truncate the file */
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /* Retrieve file info to get the file inode for later use */
        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        /* Re-open file through H5FDopen for direct writes */
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        nbytes = (size_t)(7 * stripe_size);
        memset(write_buf, 255, nbytes);
        memset(read_buf, 0, buf_size);

        write_addr = (haddr_t)stripe_size;

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Write according to the above pattern */
        status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        /* Close and re-open the file */
        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        /*
         * Set EOA for following read call (since we wrote over any
         * superblock information in the file)
         */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Read the written bytes and verify */
        status = H5FDread(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, read_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        VRFY((0 == memcmp(write_buf, read_buf, nbytes)), "memcmp succeeded");

        /* Verify the size of each subfile */
        for (int i = 0; i < num_subfiles; i++) {
            h5_stat_size_t subfile_size;
            h5_stat_t      subfile_info;

            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, i + 1, num_subfiles);

            /* Ensure file exists */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr, "fopen on subfile succeeded");
            VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

            /* Check file size */
            memset(&subfile_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
            subfile_size = (h5_stat_size_t)subfile_info.st_size;

            /*
             * Every subfile should be (2 * stripe size) bytes due to
             * space allocated in the file for subfile index 0
             */
            VRFY((subfile_size == 2 * cfg.stripe_size), "File size verification succeeded");
        }

        /* Verify that there aren't too many subfiles */
        snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                 (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

        /* Ensure file doesn't exist */
        subfile_ptr = fopen(tmp_filename, "r");
        VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");

        /*
         * Test the case where the index value of the last subfile
         * touched by I/O is less than the index value of the first
         * subfile touched by I/O, and this results in "thin" I/O
         * segments directed to the subfiles with index values that
         * fall between the values of the first and last subfiles.
         * This might appear as the following I/O pattern:
         *
         *   SUBFILE 0   SUBFILE 1   SUBFILE 2   SUBFILE 3
         *  _______________________________________________
         * |           |           |   XXXXX   |   XXXXX   | ROW 0
         * |   XXXXX   |   XXXXX   |   XXXXX   |   XXXXX   | ROW 1
         * |   XXXXX   |           |           |           | ROW 2
         * |           |           |           |           | ROW ...
         * |           |           |           |           |
         * |           |           |           |           |
         * |           |           |           |           |
         * |___________|___________|___________|___________|
         */

        /* Create/truncate the file */
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /* Retrieve file info to get the file inode for later use */
        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        /* Re-open file through H5FDopen for direct writes */
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        nbytes = (size_t)(7 * stripe_size);
        memset(write_buf, 255, nbytes);
        memset(read_buf, 0, buf_size);

        write_addr = (haddr_t)(2 * stripe_size);

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Write according to the above pattern */
        status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        /* Close and re-open the file */
        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        /*
         * Set EOA for following read call (since we wrote over any
         * superblock information in the file)
         */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Read the written bytes and verify */
        status = H5FDread(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, read_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        VRFY((0 == memcmp(write_buf, read_buf, nbytes)), "memcmp succeeded");

        /* Verify the size of each subfile */
        for (int i = 0; i < num_subfiles; i++) {
            h5_stat_size_t subfile_size;
            h5_stat_t      subfile_info;

            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, i + 1, num_subfiles);

            /* Ensure file exists */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr, "fopen on subfile succeeded");
            VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

            /* Check file size */
            memset(&subfile_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
            subfile_size = (h5_stat_size_t)subfile_info.st_size;

            /*
             * Subfile index 0 should be (3 * stripe size) bytes due to
             * space allocated in the file, while others should be
             * (2 * stripe size) bytes.
             */
            if (i == 0) {
                VRFY((subfile_size == 3 * cfg.stripe_size), "File size verification succeeded");
            }
            else {
                VRFY((subfile_size == 2 * cfg.stripe_size), "File size verification succeeded");
            }
        }

        /* Verify that there aren't too many subfiles */
        snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                 (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

        /* Ensure file doesn't exist */
        subfile_ptr = fopen(tmp_filename, "r");
        VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");

        /*
         * Test the case where I/O is 2 stripe sizes in total, but
         * is offset from a stripe boundary by a single byte, causing
         * the I/O to cross 3 subfiles.
         */

        /* Create/truncate the file */
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /* Retrieve file info to get the file inode for later use */
        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        /* Re-open file through H5FDopen for direct writes */
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        nbytes = (size_t)(2 * stripe_size);
        memset(write_buf, 255, nbytes);
        memset(read_buf, 0, buf_size);

        write_addr = (haddr_t)1;

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Write according to the above pattern */
        status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        /* Close and re-open the file */
        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        /*
         * Set EOA for following read call (since we wrote over any
         * superblock information in the file)
         */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Read the written bytes and verify */
        status = H5FDread(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, read_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        VRFY((0 == memcmp(write_buf, read_buf, nbytes)), "memcmp succeeded");

        /* Verify the size of each subfile */
        for (int i = 0; i < num_subfiles; i++) {
            h5_stat_size_t subfile_size;
            h5_stat_t      subfile_info;

            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, i + 1, num_subfiles);

            /* Ensure file exists */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr, "fopen on subfile succeeded");
            VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

            /* Check file size */
            memset(&subfile_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
            subfile_size = (h5_stat_size_t)subfile_info.st_size;

            /*
             * Subfiles indexed 0 and 1 should both be (1 * stripe size)
             * bytes (Subfile index 0 was written to with an offset of 1
             * byte, but that space will still be allocated in the file).
             * Subfile index 2 should have a single byte written to it and
             * Subfile index 3 should have nothing written to it.
             */
            if (i == 2) {
                VRFY((subfile_size == 1), "File size verification succeeded");
            }
            else if (i == 3) {
                VRFY((subfile_size == 0), "File size verification succeeded");
            }
            else {
                VRFY((subfile_size == cfg.stripe_size), "File size verification succeeded");
            }
        }

        /* Verify that there aren't too many subfiles */
        snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                 (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

        /* Ensure file doesn't exist */
        subfile_ptr = fopen(tmp_filename, "r");
        VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");

        /*
         * Test the case where I/O is 2 stripe sizes in total, but
         * is offset from a stripe boundary by (stripe size - 1) bytes,
         * causing the I/O to start at the last byte of a subfile and
         * cross 3 subfiles.
         */

        /* Create/truncate the file */
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /* Retrieve file info to get the file inode for later use */
        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        /* Re-open file through H5FDopen for direct writes */
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        nbytes = (size_t)(2 * stripe_size);
        memset(write_buf, 255, nbytes);
        memset(read_buf, 0, buf_size);

        write_addr = (haddr_t)(stripe_size - 1);

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Write according to the above pattern */
        status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        /* Close and re-open the file */
        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        /*
         * Set EOA for following read call (since we wrote over any
         * superblock information in the file)
         */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Read the written bytes and verify */
        status = H5FDread(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, read_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        VRFY((0 == memcmp(write_buf, read_buf, nbytes)), "memcmp succeeded");

        /* Verify the size of each subfile */
        for (int i = 0; i < num_subfiles; i++) {
            h5_stat_size_t subfile_size;
            h5_stat_t      subfile_info;

            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, i + 1, num_subfiles);

            /* Ensure file exists */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr, "fopen on subfile succeeded");
            VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

            /* Check file size */
            memset(&subfile_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
            subfile_size = (h5_stat_size_t)subfile_info.st_size;

            /*
             * Subfiles indexed 0 and 1 should both be (1 * stripe size)
             * bytes (Subfile index 0 was written to with an offset of
             * stripe size - 1 bytes, but that space will still be allocated
             * in the file). Subfile index 2 should be (1 * stripe size) - 1
             * bytes. Subfile index 3 should have nothing written to it.
             */
            if (i == 2) {
                VRFY((subfile_size == cfg.stripe_size - 1), "File size verification succeeded");
            }
            else if (i == 3) {
                VRFY((subfile_size == 0), "File size verification succeeded");
            }
            else {
                VRFY((subfile_size == cfg.stripe_size), "File size verification succeeded");
            }
        }

        /* Verify that there aren't too many subfiles */
        snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                 (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

        /* Ensure file doesn't exist */
        subfile_ptr = fopen(tmp_filename, "r");
        VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");

        /*
         * Test the case where I/O is 2 stripe sizes + 1 byte in total
         * and starts aligned to a stripe boundary, causing the I/O
         * to cross 3 subfiles.
         */

        /* Create/truncate the file */
        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /* Retrieve file info to get the file inode for later use */
        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        /* Re-open file through H5FDopen for direct writes */
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        nbytes = (size_t)((2 * stripe_size) + 1);
        memset(write_buf, 255, nbytes);
        memset(read_buf, 0, buf_size);

        write_addr = (haddr_t)0;

        /* Set EOA for following write call */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Write according to the above pattern */
        status = H5FDwrite(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, c_write_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        /* Close and re-open the file */
        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");
        file_ptr = H5FDopen(SUBF_FILENAME, H5F_ACC_RDWR, fapl_id, HADDR_UNDEF);
        VRFY(file_ptr, "H5FDopen succeeded");

        /*
         * Set EOA for following read call (since we wrote over any
         * superblock information in the file)
         */
        VRFY((H5FDset_eoa(file_ptr, H5FD_MEM_DEFAULT, write_addr + nbytes) >= 0), "H5FDset_eoa succeeded");

        /* Read the written bytes and verify */
        status = H5FDread(file_ptr, H5FD_MEM_DRAW, dxpl_id, write_addr, nbytes, read_buf);
        VRFY((status >= 0), "H5FDwrite succeeded");

        VRFY((0 == memcmp(write_buf, read_buf, nbytes)), "memcmp succeeded");

        /* Verify the size of each subfile */
        for (int i = 0; i < num_subfiles; i++) {
            h5_stat_size_t subfile_size;
            h5_stat_t      subfile_info;

            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, i + 1, num_subfiles);

            /* Ensure file exists */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr, "fopen on subfile succeeded");
            VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

            /* Check file size */
            memset(&subfile_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
            subfile_size = (h5_stat_size_t)subfile_info.st_size;

            /*
             * Subfiles indexed 0 and 1 should both be (1 * stripe size)
             * bytes. Subfile index 2 should have a single byte written to
             * it and Subfile index 3 should have nothing written to it.
             */
            if (i == 2) {
                VRFY((subfile_size == 1), "File size verification succeeded");
            }
            else if (i == 3) {
                VRFY((subfile_size == 0), "File size verification succeeded");
            }
            else {
                VRFY((subfile_size == cfg.stripe_size), "File size verification succeeded");
            }
        }

        /* Verify that there aren't too many subfiles */
        snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                 (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

        /* Ensure file doesn't exist */
        subfile_ptr = fopen(tmp_filename, "r");
        VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

        VRFY((H5FDclose(file_ptr) >= 0), "H5FDclose succeeded");

        free(write_buf);
        write_buf = NULL;
        free(read_buf);
        write_buf = NULL;

        free(tmp_filename);

        VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");

        H5E_BEGIN_TRY
        {
            H5Fdelete(SUBF_FILENAME, fapl_id);
        }
        H5E_END_TRY

        VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    if (skip) {
        if (MAINPROCESS)
            SKIPPED();
    }
    else
        CHECK_PASSED();
}
#undef SUBF_FILENAME

/*
 * Test the different I/O Concentator selection strategies
 * for the Subfiling VFD
 */
#define SUBF_FILENAME        "test_subfiling_selection_strategies.h5"
#define NUM_RANKS_CHOICES    2
#define NUM_CRITERIA_FORMATS 2
static void
test_selection_strategies(void)
{
    H5FD_subfiling_params_t cfg;
    hid_t                   file_id      = H5I_INVALID_HID;
    hid_t                   fapl_id      = H5I_INVALID_HID;
    char                   *tmp_filename = NULL;

    curr_nerrors = nerrors;

    if (MAINPROCESS)
        TESTING_2("I/O concentrator selection strategies");

    tmp_filename = malloc(PATH_MAX);
    VRFY(tmp_filename, "malloc succeeded");

    for (H5FD_subfiling_ioc_select_t strategy = 0; strategy < ioc_selection_options; strategy++) {
        /* Skip 1 IOC per node strategy since we assume it's
         * the default strategy tested in this file. Skip
         * "with config" strategy since it isn't supported.
         */
        if (strategy == SELECT_IOC_ONE_PER_NODE || strategy == SELECT_IOC_WITH_CONFIG)
            continue;

        /* Test with 1 MPI rank and then all MPI ranks */
        for (size_t num_ranks_choice = 0; num_ranks_choice < NUM_RANKS_CHOICES; num_ranks_choice++) {
            int num_active_ranks = mpi_size;

            if (num_ranks_choice == 0)
                num_active_ranks = 1;

            /* Test with a selection strategy criteria string
             * in the 'integer:[integer|string]' form and in
             * the form of just a single value.
             */
            for (size_t criteria_format_choice = 0; criteria_format_choice < NUM_CRITERIA_FORMATS;
                 criteria_format_choice++) {
                MPI_Comm file_comm = comm_g;
                char     criteria_buf[256];
                char     sel_criteria[128]; /* Use char buffer for criteria as we may support
                                               the "with config" strategy in the future */
                int expected_num_subfiles = -1;

                cfg.ioc_selection = strategy;
                cfg.stripe_size   = H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;
                cfg.stripe_count  = H5FD_SUBFILING_DEFAULT_STRIPE_COUNT;

                switch (strategy) {
                    case SELECT_IOC_EVERY_NTH_RANK: {
                        int stride;

                        /* Try to select a reasonable stride value */
                        if (num_active_ranks <= 2)
                            stride = 1;
                        else if (num_active_ranks <= 8)
                            stride = 2;
                        else if (num_active_ranks <= 32)
                            stride = 4;
                        else if (num_active_ranks <= 128)
                            stride = 8;
                        else
                            stride = 16;

                        snprintf(sel_criteria, 128, "%d", stride);

                        expected_num_subfiles = ((num_active_ranks - 1) / stride) + 1;

                        break;
                    }

                    case SELECT_IOC_TOTAL: {
                        int n_iocs;

                        /* Try to select a reasonable number of IOCs */
                        if (num_active_ranks <= 2)
                            n_iocs = 1;
                        else if (num_active_ranks <= 8)
                            n_iocs = 2;
                        else if (num_active_ranks <= 32)
                            n_iocs = 4;
                        else if (num_active_ranks <= 128)
                            n_iocs = 8;
                        else
                            n_iocs = 16;

                        snprintf(sel_criteria, 128, "%d", n_iocs);

                        expected_num_subfiles = n_iocs;

                        break;
                    }

                    case SELECT_IOC_ONE_PER_NODE:
                    case SELECT_IOC_WITH_CONFIG:
                    case ioc_selection_options:
                    default:
                        printf("invalid IOC selection strategy\n");
                        MPI_Abort(comm_g, -1);
                }

                if (criteria_format_choice == 0) {
                    snprintf(criteria_buf, 256, "%d:%s", strategy, sel_criteria);
                }
                else if (criteria_format_choice == 1) {
                    snprintf(criteria_buf, 256, "%s", sel_criteria);
                }

                VRFY(HDsetenv(H5FD_SUBFILING_IOC_SELECTION_CRITERIA, criteria_buf, 1) >= 0,
                     "HDsetenv succeeded");

                assert(num_active_ranks == mpi_size || num_active_ranks == 1);

                if ((num_active_ranks == mpi_size) || (mpi_rank == 0)) {
                    h5_stat_t file_info;
                    FILE     *subfile_ptr;
                    int       num_digits;

                    if (num_active_ranks < mpi_size)
                        file_comm = MPI_COMM_SELF;

                    fapl_id = create_subfiling_ioc_fapl(file_comm, info_g, true, &cfg,
                                                        H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
                    VRFY((fapl_id >= 0), "FAPL creation succeeded");

                    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
                    VRFY((file_id >= 0), "H5Fcreate succeeded");

                    /*
                     * Get the file inode value so we can construct the subfile names
                     */
                    memset(&file_info, 0, sizeof(h5_stat_t));
                    VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

                    num_digits = (int)(log10(expected_num_subfiles) + 1);

                    /* Ensure all the subfiles are present */
                    for (int i = 0; i < expected_num_subfiles; i++) {
                        snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                                 (uint64_t)file_info.st_ino, num_digits, i + 1, expected_num_subfiles);

                        /* Ensure file exists */
                        subfile_ptr = fopen(tmp_filename, "r");
                        VRFY(subfile_ptr, "fopen on subfile succeeded");
                        VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");
                    }

                    /* Ensure no extra subfiles are present */
                    snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                             (uint64_t)file_info.st_ino, num_digits, expected_num_subfiles + 1,
                             expected_num_subfiles);

                    /* Ensure file doesn't exist */
                    subfile_ptr = fopen(tmp_filename, "r");
                    VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

                    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

                    mpi_code_g = MPI_Barrier(file_comm);
                    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

                    H5E_BEGIN_TRY
                    {
                        H5Fdelete(SUBF_FILENAME, fapl_id);
                    }
                    H5E_END_TRY

                    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

                    VRFY(HDunsetenv(H5FD_SUBFILING_IOC_SELECTION_CRITERIA) >= 0, "HDunsetenv succeeded");
                }

                mpi_code_g = MPI_Barrier(comm_g);
                VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");
            }
        }
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    free(tmp_filename);

    CHECK_PASSED();
}
#undef SUBF_FILENAME
#undef NUM_RANKS_CHOICES
#undef NUM_CRITERIA_FORMATS

/*
 * Test that opening a file with a different stripe
 * size/count than was used when creating the file
 * results in the original stripe size/count being
 * used. As there is currently no API to check the
 * exact values used, we rely on the assumption that
 * using a different stripe size/count would result
 * in data verification failures.
 */
#define SUBF_FILENAME  "test_subfiling_read_different_stripe_sizes.h5"
#define SUBF_HDF5_TYPE H5T_NATIVE_INT
#define SUBF_C_TYPE    int
static void
test_read_different_stripe_size(void)
{
    H5FD_subfiling_params_t cfg;
    hsize_t                 start[1];
    hsize_t                 count[1];
    hsize_t                 dset_dims[1];
    size_t                  target_size;
    hid_t                   file_id      = H5I_INVALID_HID;
    hid_t                   fapl_id      = H5I_INVALID_HID;
    hid_t                   dset_id      = H5I_INVALID_HID;
    hid_t                   dxpl_id      = H5I_INVALID_HID;
    hid_t                   dcpl_id      = H5I_INVALID_HID;
    hid_t                   fspace_id    = H5I_INVALID_HID;
    char                   *tmp_filename = NULL;
    void                   *buf          = NULL;

    curr_nerrors = nerrors;

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "DXPL creation succeeded");

    /* Set selection I/O mode on DXPL */
    VRFY((H5Pset_selection_io(dxpl_id, H5D_SELECTION_IO_MODE_ON) >= 0), "H5Pset_selection_io succeeded");

    if (MAINPROCESS)
        TESTING_2("file re-opening with different stripe size");

    tmp_filename = malloc(PATH_MAX);
    VRFY(tmp_filename, "malloc succeeded");

    /* Use a 1MiB stripe size and a subfile for each IOC */
    cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
    cfg.stripe_size   = (stripe_size_g > 0) ? stripe_size_g : 1048576;
    cfg.stripe_count  = num_iocs_g;

    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, true, &cfg, H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

    target_size = (size_t)cfg.stripe_size;

    /* Nudge stripe size to be multiple of C type size */
    if ((target_size % sizeof(SUBF_C_TYPE)) != 0)
        target_size += sizeof(SUBF_C_TYPE) - (target_size % sizeof(SUBF_C_TYPE));

    target_size *= (size_t)mpi_size;

    VRFY(((target_size % sizeof(SUBF_C_TYPE)) == 0), "target size check succeeded");

    dset_dims[0] = (hsize_t)(target_size / sizeof(SUBF_C_TYPE));

    fspace_id = H5Screate_simple(1, dset_dims, NULL);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    dcpl_id = create_dcpl_id(1, dset_dims, dxpl_id);
    VRFY((dcpl_id >= 0), "DCPL creation succeeded");

    dset_id = H5Dcreate2(file_id, "DSET", SUBF_HDF5_TYPE, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Select hyperslab */
    count[0] = dset_dims[0] / (hsize_t)mpi_size;
    start[0] = (hsize_t)mpi_rank * count[0];
    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) >= 0),
         "H5Sselect_hyperslab succeeded");

    buf = malloc(count[0] * sizeof(SUBF_C_TYPE));
    VRFY(buf, "malloc succeeded");

    for (size_t i = 0; i < count[0]; i++)
        ((SUBF_C_TYPE *)buf)[i] = (SUBF_C_TYPE)((size_t)mpi_rank + i);

    VRFY((H5Dwrite(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, fspace_id, dxpl_id, buf) >= 0),
         "Dataset write succeeded");

    free(buf);
    buf = NULL;

    VRFY((H5Sclose(fspace_id) >= 0), "File dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Pclose(dcpl_id) >= 0), "DCPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    /* Ensure all the subfiles are present */
    if (MAINPROCESS) {
        h5_stat_t file_info;
        FILE     *subfile_ptr;
        int       num_subfiles = cfg.stripe_count;
        int       num_digits   = (int)(log10(num_subfiles) + 1);

        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        for (int j = 0; j < num_subfiles; j++) {
            h5_stat_size_t subfile_size;
            h5_stat_t      subfile_info;

            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, j + 1, num_subfiles);

            /* Ensure file exists */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr, "fopen on subfile succeeded");
            VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

            /* Check file size */
            if (!enable_compression) {
                memset(&subfile_info, 0, sizeof(h5_stat_t));
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                subfile_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((subfile_size >= cfg.stripe_size), "File size verification succeeded");
            }
        }
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    /* Add a bit to the stripe size and specify a few more subfiles */
    cfg.stripe_size += (cfg.stripe_size / 2);
    cfg.stripe_count *= 2;

    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, true, &cfg, H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    file_id = H5Fopen(SUBF_FILENAME, H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "H5Fopen succeeded");

    dset_id = H5Dopen2(file_id, "DSET", H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    fspace_id = H5Dget_space(dset_id);
    VRFY((fspace_id >= 0), "Dataspace retrieval succeeded");

    /* Select hyperslab */
    count[0] = dset_dims[0] / (hsize_t)mpi_size;
    start[0] = (hsize_t)mpi_rank * count[0];
    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) >= 0),
         "H5Sselect_hyperslab succeeded");

    buf = calloc(1, count[0] * sizeof(SUBF_C_TYPE));
    VRFY(buf, "calloc succeeded");

    VRFY((H5Dread(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, fspace_id, dxpl_id, buf) >= 0),
         "Dataset read succeeded");

    for (size_t i = 0; i < count[0]; i++) {
        SUBF_C_TYPE buf_value = ((SUBF_C_TYPE *)buf)[i];

        VRFY((buf_value == (SUBF_C_TYPE)((size_t)mpi_rank + i)), "data verification succeeded");
    }

    free(buf);
    buf = NULL;

    VRFY((H5Sclose(fspace_id) >= 0), "File dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    /* Ensure only the original subfiles are present */
    if (MAINPROCESS) {
        h5_stat_t file_info;
        FILE     *subfile_ptr;
        int       num_subfiles = cfg.stripe_count;
        int       num_digits   = (int)(log10(num_subfiles / 2) + 1);

        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        for (int j = 0; j < num_subfiles; j++) {
            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, j + 1, num_subfiles / 2);

            if (j < (num_subfiles / 2)) {
                /* Ensure file exists */
                subfile_ptr = fopen(tmp_filename, "r");
                VRFY(subfile_ptr, "fopen on subfile succeeded");
                VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");
            }
            else {
                /* Ensure file doesn't exist */
                subfile_ptr = fopen(tmp_filename, "r");
                VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");
            }
        }
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    H5E_BEGIN_TRY
    {
        H5Fdelete(SUBF_FILENAME, fapl_id);
    }
    H5E_END_TRY

    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");
    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

    free(tmp_filename);

    CHECK_PASSED();
}
#undef SUBF_FILENAME
#undef SUBF_HDF5_TYPE
#undef SUBF_C_TYPE

/*
 * Test that everything works correctly when a file is
 * pre-created on rank 0 with a specified target number
 * of subfiles and then read back on all ranks.
 */
#define SUBF_FILENAME  "test_subfiling_precreate_rank_0.h5"
#define SUBF_HDF5_TYPE H5T_NATIVE_INT
#define SUBF_C_TYPE    int
static void
test_subfiling_precreate_rank_0(void)
{
    hsize_t start[1];
    hsize_t count[1];
    hsize_t dset_dims[1];
    size_t  target_size;
    size_t  n_elements_per_rank;
    hid_t   file_id   = H5I_INVALID_HID;
    hid_t   fapl_id   = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   dxpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    void   *buf       = NULL;

    curr_nerrors = nerrors;

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "DXPL creation succeeded");

    /* Set selection I/O mode on DXPL */
    VRFY((H5Pset_selection_io(dxpl_id, H5D_SELECTION_IO_MODE_ON) >= 0), "H5Pset_selection_io succeeded");

    if (MAINPROCESS)
        TESTING_2("file pre-creation on rank 0");

    /* Calculate target size for dataset to stripe it across available IOCs */
    target_size = (stripe_size_g > 0) ? (size_t)stripe_size_g : H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;

    /* Nudge stripe size to be multiple of C type size */
    if ((target_size % sizeof(SUBF_C_TYPE)) != 0)
        target_size += sizeof(SUBF_C_TYPE) - (target_size % sizeof(SUBF_C_TYPE));

    target_size *= (size_t)mpi_size;

    VRFY(((target_size % sizeof(SUBF_C_TYPE)) == 0), "target size check succeeded");

    if (stripe_size_g > 0) {
        VRFY((target_size >= (size_t)stripe_size_g), "target size check succeeded");
    }
    else {
        VRFY((target_size >= H5FD_SUBFILING_DEFAULT_STRIPE_SIZE), "target size check succeeded");
    }

    dset_dims[0]        = (hsize_t)(target_size / sizeof(SUBF_C_TYPE));
    n_elements_per_rank = (dset_dims[0] / (size_t)mpi_size);

    /* Create and populate file on rank 0 only */
    if (MAINPROCESS) {
        H5FD_subfiling_params_t cfg;
        h5_stat_size_t          file_size;
        h5_stat_t               file_info;
        FILE                   *subfile_ptr;
        char                   *tmp_filename = NULL;
        int                     num_subfiles;
        int                     num_digits;

        /* Create a file consisting of 1 subfile per application I/O concentrator */
        cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
        cfg.stripe_size   = (stripe_size_g > 0) ? stripe_size_g : H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;
        cfg.stripe_count  = num_iocs_g;

        fapl_id = create_subfiling_ioc_fapl(MPI_COMM_SELF, MPI_INFO_NULL, true, &cfg,
                                            H5FD_IOC_DEFAULT_THREAD_POOL_SIZE);
        VRFY((fapl_id >= 0), "FAPL creation succeeded");

        file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "H5Fcreate succeeded");

        fspace_id = H5Screate_simple(1, dset_dims, NULL);
        VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

        dset_id =
            H5Dcreate2(file_id, "DSET", SUBF_HDF5_TYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        buf = malloc(dset_dims[0] * sizeof(SUBF_C_TYPE));
        VRFY(buf, "malloc succeeded");

        for (size_t i = 0; i < dset_dims[0]; i++)
            ((SUBF_C_TYPE *)buf)[i] = (SUBF_C_TYPE)((i / n_elements_per_rank) + (i % n_elements_per_rank));

        VRFY((H5Dwrite(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, fspace_id, dxpl_id, buf) >= 0),
             "Dataset write succeeded");

        free(buf);
        buf = NULL;

        VRFY((H5Sclose(fspace_id) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

        /*
         * Ensure that all the subfiles are present
         */

        num_subfiles = cfg.stripe_count;
        num_digits   = (int)(log10(num_subfiles) + 1);

        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        tmp_filename = malloc(PATH_MAX);
        VRFY(tmp_filename, "malloc succeeded");

        for (int i = 0; i < num_subfiles; i++) {
            h5_stat_t subfile_info;

            snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                     (uint64_t)file_info.st_ino, num_digits, i + 1, num_subfiles);

            /* Ensure file exists */
            subfile_ptr = fopen(tmp_filename, "r");
            VRFY(subfile_ptr, "fopen on subfile succeeded");
            VRFY((fclose(subfile_ptr) >= 0), "fclose on subfile succeeded");

            /* Check file size */
            if (!enable_compression) {
                memset(&subfile_info, 0, sizeof(h5_stat_t));
                VRFY((HDstat(tmp_filename, &subfile_info) >= 0), "HDstat succeeded");
                file_size = (h5_stat_size_t)subfile_info.st_size;

                VRFY((file_size >= cfg.stripe_size), "File size verification succeeded");
            }
        }

        /* Verify that there aren't too many subfiles */
        snprintf(tmp_filename, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME,
                 (uint64_t)file_info.st_ino, num_digits, num_subfiles + 1, num_subfiles);

        /* Ensure file doesn't exist */
        subfile_ptr = fopen(tmp_filename, "r");
        VRFY(subfile_ptr == NULL, "fopen on subfile correctly failed");

        free(tmp_filename);
        tmp_filename = NULL;
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    /* Open the file on all ranks */

    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, false, NULL, 0);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    file_id = H5Fopen(SUBF_FILENAME, H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "H5Fopen succeeded");

    dset_id = H5Dopen2(file_id, "DSET", H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    fspace_id = H5Dget_space(dset_id);
    VRFY((fspace_id >= 0), "Dataset dataspace retrieval succeeded");

    /* Select hyperslab */
    count[0] = n_elements_per_rank;
    start[0] = (hsize_t)mpi_rank * count[0];
    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) >= 0),
         "H5Sselect_hyperslab succeeded");

    buf = calloc(1, count[0] * sizeof(SUBF_C_TYPE));
    VRFY(buf, "calloc succeeded");

    VRFY((H5Dread(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, fspace_id, dxpl_id, buf) >= 0),
         "Dataset read succeeded");

    for (size_t i = 0; i < n_elements_per_rank; i++) {
        SUBF_C_TYPE buf_value = ((SUBF_C_TYPE *)buf)[i];

        VRFY((buf_value == (SUBF_C_TYPE)((size_t)mpi_rank + i)), "data verification succeeded");
    }

    free(buf);
    buf = NULL;

    VRFY((H5Sclose(fspace_id) >= 0), "File dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

    H5E_BEGIN_TRY
    {
        H5Fdelete(SUBF_FILENAME, fapl_id);
    }
    H5E_END_TRY

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME
#undef SUBF_HDF5_TYPE
#undef SUBF_C_TYPE

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
    hid_t   dxpl_id   = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    void   *buf       = NULL;

    curr_nerrors = nerrors;

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "DXPL creation succeeded");

    /* Set selection I/O mode on DXPL */
    VRFY((H5Pset_selection_io(dxpl_id, H5D_SELECTION_IO_MODE_ON) >= 0), "H5Pset_selection_io succeeded");

    if (MAINPROCESS)
        TESTING_2("reading back file with single MPI rank");

    /* Get a default Subfiling FAPL */
    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, false, NULL, 0);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    /* Create file on all ranks */
    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

    /* Calculate target size for dataset to stripe it across available IOCs */
    target_size = (stripe_size_g > 0) ? (size_t)stripe_size_g : H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;

    /* Nudge stripe size to be multiple of C type size */
    if ((target_size % sizeof(SUBF_C_TYPE)) != 0)
        target_size += sizeof(SUBF_C_TYPE) - (target_size % sizeof(SUBF_C_TYPE));

    target_size *= (size_t)mpi_size;

    VRFY(((target_size % sizeof(SUBF_C_TYPE)) == 0), "target size check succeeded");

    if (stripe_size_g > 0) {
        VRFY((target_size >= (size_t)stripe_size_g), "target size check succeeded");
    }
    else {
        VRFY((target_size >= H5FD_SUBFILING_DEFAULT_STRIPE_SIZE), "target size check succeeded");
    }

    dset_dims[0] = (hsize_t)(target_size / sizeof(SUBF_C_TYPE));

    fspace_id = H5Screate_simple(1, dset_dims, NULL);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    dcpl_id = create_dcpl_id(1, dset_dims, dxpl_id);
    VRFY((dcpl_id >= 0), "DCPL creation succeeded");

    dset_id = H5Dcreate2(file_id, "DSET", SUBF_HDF5_TYPE, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Select hyperslab */
    count[0] = dset_dims[0] / (hsize_t)mpi_size;
    start[0] = (hsize_t)mpi_rank * count[0];
    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) >= 0),
         "H5Sselect_hyperslab succeeded");

    buf = malloc(count[0] * sizeof(SUBF_C_TYPE));
    VRFY(buf, "malloc succeeded");

    for (size_t i = 0; i < count[0]; i++)
        ((SUBF_C_TYPE *)buf)[i] = (SUBF_C_TYPE)((size_t)mpi_rank + i);

    VRFY((H5Dwrite(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, fspace_id, dxpl_id, buf) >= 0),
         "Dataset write succeeded");

    free(buf);
    buf = NULL;

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Pclose(dcpl_id) >= 0), "DCPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    if (MAINPROCESS) {
        fapl_id = create_subfiling_ioc_fapl(MPI_COMM_SELF, MPI_INFO_NULL, false, NULL, 0);
        VRFY((fapl_id >= 0), "FAPL creation succeeded");

        file_id = H5Fopen(SUBF_FILENAME, H5F_ACC_RDONLY, fapl_id);
        VRFY((file_id >= 0), "H5Fopen succeeded");

        dset_id = H5Dopen2(file_id, "DSET", H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        buf = calloc(1, target_size);
        VRFY(buf, "calloc succeeded");

        VRFY((H5Dread(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, H5S_ALL, dxpl_id, buf) >= 0),
             "Dataset read succeeded");

        for (size_t i = 0; i < (size_t)mpi_size; i++) {
            for (size_t j = 0; j < count[0]; j++) {
                SUBF_C_TYPE buf_value = ((SUBF_C_TYPE *)buf)[(i * count[0]) + j];

                VRFY((buf_value == (SUBF_C_TYPE)(j + i)), "data verification succeeded");
            }
        }

        free(buf);
        buf = NULL;

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

        H5E_BEGIN_TRY
        {
            H5Fdelete(SUBF_FILENAME, fapl_id);
        }
        H5E_END_TRY

        VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    VRFY((H5Sclose(fspace_id) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME
#undef SUBF_HDF5_TYPE
#undef SUBF_C_TYPE

/*
 * Test to check that an HDF5 file created with the
 * Subfiling VFD can be read back with less MPI ranks
 * than the file was written with
 */
#define SUBF_FILENAME  "test_subfiling_write_many_read_few.h5"
#define SUBF_HDF5_TYPE H5T_NATIVE_INT
#define SUBF_C_TYPE    int
static void
test_subfiling_write_many_read_few(void)
{
    MPI_Comm sub_comm = MPI_COMM_NULL;
    hsize_t  start[1];
    hsize_t  count[1];
    hsize_t  dset_dims[1];
    bool     reading_file = false;
    size_t   target_size;
    hid_t    file_id   = H5I_INVALID_HID;
    hid_t    fapl_id   = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    dxpl_id   = H5I_INVALID_HID;
    hid_t    dcpl_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    void    *buf       = NULL;

    curr_nerrors = nerrors;

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "DXPL creation succeeded");

    /* Set selection I/O mode on DXPL */
    VRFY((H5Pset_selection_io(dxpl_id, H5D_SELECTION_IO_MODE_ON) >= 0), "H5Pset_selection_io succeeded");

    if (MAINPROCESS)
        TESTING_2("reading back file with fewer MPI ranks than written with");

    /*
     * Skip this test for an MPI communicator size of 1,
     * as the test wouldn't really be meaningful
     */
    if (mpi_size == 1) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    /* Get a default Subfiling FAPL */
    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, false, NULL, 0);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    /* Create file on all ranks */
    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");

    /* Calculate target size for dataset to stripe it across available IOCs */
    target_size = (stripe_size_g > 0) ? (size_t)stripe_size_g : H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;

    /* Nudge stripe size to be multiple of C type size */
    if ((target_size % sizeof(SUBF_C_TYPE)) != 0)
        target_size += sizeof(SUBF_C_TYPE) - (target_size % sizeof(SUBF_C_TYPE));

    target_size *= (size_t)mpi_size;

    VRFY(((target_size % sizeof(SUBF_C_TYPE)) == 0), "target size check succeeded");

    if (stripe_size_g > 0) {
        VRFY((target_size >= (size_t)stripe_size_g), "target size check succeeded");
    }
    else {
        VRFY((target_size >= H5FD_SUBFILING_DEFAULT_STRIPE_SIZE), "target size check succeeded");
    }

    dset_dims[0] = (hsize_t)(target_size / sizeof(SUBF_C_TYPE));

    fspace_id = H5Screate_simple(1, dset_dims, NULL);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    dcpl_id = create_dcpl_id(1, dset_dims, dxpl_id);
    VRFY((dcpl_id >= 0), "DCPL creation succeeded");

    dset_id = H5Dcreate2(file_id, "DSET", SUBF_HDF5_TYPE, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Select hyperslab */
    count[0] = dset_dims[0] / (hsize_t)mpi_size;
    start[0] = (hsize_t)mpi_rank * count[0];
    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) >= 0),
         "H5Sselect_hyperslab succeeded");

    buf = malloc(count[0] * sizeof(SUBF_C_TYPE));
    VRFY(buf, "malloc succeeded");

    for (size_t i = 0; i < count[0]; i++)
        ((SUBF_C_TYPE *)buf)[i] = (SUBF_C_TYPE)((size_t)mpi_rank + i);

    VRFY((H5Dwrite(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, fspace_id, dxpl_id, buf) >= 0),
         "Dataset write succeeded");

    free(buf);
    buf = NULL;

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Pclose(dcpl_id) >= 0), "DCPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    /*
     * If only using 1 node, read file back with a
     * few ranks from that node. Otherwise, read file
     * back with 1 MPI rank per node
     */
    if (num_nodes_g == 1) {
        int color;

        if (mpi_size < 2) {
            color = 1;
        }
        else if (mpi_size < 4) {
            color = (mpi_rank < (mpi_size / 2));
        }
        else {
            color = (mpi_rank < (mpi_size / 4));
        }

        if (mpi_size > 1) {
            mpi_code_g = MPI_Comm_split(comm_g, color, mpi_rank, &sub_comm);
            VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Comm_split succeeded");
        }

        if (color)
            reading_file = true;
    }
    else {
        if (node_local_rank == 0) {
            sub_comm     = ioc_comm;
            reading_file = true;
        }
    }

    if (reading_file) {
        fapl_id = create_subfiling_ioc_fapl(sub_comm, MPI_INFO_NULL, false, NULL, 0);
        VRFY((fapl_id >= 0), "FAPL creation succeeded");

        file_id = H5Fopen(SUBF_FILENAME, H5F_ACC_RDONLY, fapl_id);
        VRFY((file_id >= 0), "H5Fopen succeeded");

        dset_id = H5Dopen2(file_id, "DSET", H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        buf = calloc(1, target_size);
        VRFY(buf, "calloc succeeded");

        VRFY((H5Dread(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, H5S_ALL, dxpl_id, buf) >= 0),
             "Dataset read succeeded");

        for (size_t i = 0; i < (size_t)mpi_size; i++) {
            for (size_t j = 0; j < count[0]; j++) {
                SUBF_C_TYPE buf_value = ((SUBF_C_TYPE *)buf)[(i * count[0]) + j];

                VRFY((buf_value == (SUBF_C_TYPE)(j + i)), "data verification succeeded");
            }
        }

        free(buf);
        buf = NULL;

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

        H5E_BEGIN_TRY
        {
            H5Fdelete(SUBF_FILENAME, fapl_id);
        }
        H5E_END_TRY

        VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
    }

    if ((sub_comm != MPI_COMM_NULL) && (num_nodes_g == 1)) {
        mpi_code_g = MPI_Comm_free(&sub_comm);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Comm_free succeeded");
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    VRFY((H5Sclose(fspace_id) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");

    CHECK_PASSED();
}
#undef SUBF_FILENAME
#undef SUBF_HDF5_TYPE
#undef SUBF_C_TYPE

/*
 * Test that the subfiling file can be read with the
 * sec2 driver after being fused back together with
 * the h5fuse utility
 */
#define SUBF_FILENAME  "test_subfiling_h5fuse.h5"
#define SUBF_HDF5_TYPE H5T_NATIVE_INT
#define SUBF_C_TYPE    int
static void
test_subfiling_h5fuse(void)
{
#if defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)
    h5_stat_t file_info;
    uint64_t  file_inode;
    hsize_t   start[1];
    hsize_t   count[1];
    hsize_t   dset_dims[1];
    size_t    target_size;
    hid_t     file_id   = H5I_INVALID_HID;
    hid_t     fapl_id   = H5I_INVALID_HID;
    hid_t     dset_id   = H5I_INVALID_HID;
    hid_t     dxpl_id   = H5I_INVALID_HID;
    hid_t     dcpl_id   = H5I_INVALID_HID;
    hid_t     fspace_id = H5I_INVALID_HID;
    void     *buf       = NULL;
    int       skip_test = 0;
#endif

    curr_nerrors = nerrors;

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "DXPL creation succeeded");

    /* Set selection I/O mode on DXPL */
    VRFY((H5Pset_selection_io(dxpl_id, H5D_SELECTION_IO_MODE_ON) >= 0), "H5Pset_selection_io succeeded");

    if (MAINPROCESS)
        TESTING_2("h5fuse utility");

#if defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)

    /*
     * Check if h5fuse script exists in current directory;
     * Skip test if it doesn't
     */
    if (MAINPROCESS) {
        FILE *h5fuse_script;

        h5fuse_script = fopen("h5fuse", "r");
        if (h5fuse_script)
            fclose(h5fuse_script);
        else
            skip_test = 1;
    }

    if (mpi_size > 1) {
        mpi_code_g = MPI_Bcast(&skip_test, 1, MPI_INT, 0, comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Bcast succeeded");
    }

    if (skip_test) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    /* Get a default Subfiling FAPL */
    fapl_id = create_subfiling_ioc_fapl(comm_g, info_g, false, NULL, 0);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    /* Create file on all ranks */
    file_id = H5Fcreate(SUBF_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    /*
     * Retrieve the HDF5 file's inode number before operating on
     * it, since it might change after fusing
     */
    HDcompile_assert(sizeof(uint64_t) >= sizeof(ino_t));
    if (MAINPROCESS) {
        memset(&file_info, 0, sizeof(h5_stat_t));
        VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");

        file_inode = (uint64_t)file_info.st_ino;
    }

    if (mpi_size > 1) {
        mpi_code_g = MPI_Bcast(&file_inode, 1, MPI_UINT64_T, 0, comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Bcast succeeded");
    }

    /* Calculate target size for dataset to stripe it across available IOCs */
    target_size = (stripe_size_g > 0) ? (size_t)stripe_size_g : H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;

    /* Nudge stripe size to be multiple of C type size */
    if ((target_size % sizeof(SUBF_C_TYPE)) != 0)
        target_size += sizeof(SUBF_C_TYPE) - (target_size % sizeof(SUBF_C_TYPE));

    target_size *= (size_t)mpi_size;

    VRFY(((target_size % sizeof(SUBF_C_TYPE)) == 0), "target size check succeeded");

    if (stripe_size_g > 0) {
        VRFY((target_size >= (size_t)stripe_size_g), "target size check succeeded");
    }
    else {
        VRFY((target_size >= H5FD_SUBFILING_DEFAULT_STRIPE_SIZE), "target size check succeeded");
    }

    dset_dims[0] = (hsize_t)(target_size / sizeof(SUBF_C_TYPE));

    fspace_id = H5Screate_simple(1, dset_dims, NULL);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    dcpl_id = create_dcpl_id(1, dset_dims, dxpl_id);
    VRFY((dcpl_id >= 0), "DCPL creation succeeded");

    dset_id = H5Dcreate2(file_id, "DSET", SUBF_HDF5_TYPE, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Select hyperslab */
    count[0] = dset_dims[0] / (hsize_t)mpi_size;
    start[0] = (hsize_t)mpi_rank * count[0];
    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) >= 0),
         "H5Sselect_hyperslab succeeded");

    buf = malloc(count[0] * sizeof(SUBF_C_TYPE));
    VRFY(buf, "malloc succeeded");

    for (size_t i = 0; i < count[0]; i++)
        ((SUBF_C_TYPE *)buf)[i] = (SUBF_C_TYPE)((size_t)mpi_rank + i);

    VRFY((H5Dwrite(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, fspace_id, dxpl_id, buf) >= 0),
         "Dataset write succeeded");

    free(buf);
    buf = NULL;

    VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_INDEPENDENT) >= 0), "H5Pset_dxpl_mpio succeeded");

    VRFY((H5Sclose(fspace_id) >= 0), "File dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Pclose(dcpl_id) >= 0), "DCPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    if (MAINPROCESS) {
        pid_t pid = 0;
        pid_t tmppid;
        int   status;

        pid = fork();
        VRFY(pid >= 0, "fork succeeded");

        if (pid == 0) {
            char *tmp_filename;
            char *args[7];

            tmp_filename = malloc(PATH_MAX);
            VRFY(tmp_filename, "malloc succeeded");

            /* Generate name for configuration file */
            snprintf(tmp_filename, PATH_MAX, "%s/" H5FD_SUBFILING_CONFIG_FILENAME_TEMPLATE, config_dir,
                     SUBF_FILENAME, file_inode);

            args[0] = strdup("env");
            args[1] = strdup("./h5fuse");
            args[2] = strdup("-q");
            args[3] = strdup("-f");
            args[4] = tmp_filename;
            args[5] = NULL;

            /* Call h5fuse script from MPI rank 0 */
            execvp("env", args);
        }
        else {
            tmppid = waitpid(pid, &status, 0);
            VRFY(tmppid >= 0, "waitpid succeeded");

            if (WIFEXITED(status)) {
                int ret;

                if ((ret = WEXITSTATUS(status)) != 0) {
                    printf("h5fuse process exited with error code %d\n", ret);
                    fflush(stdout);
                    MPI_Abort(comm_g, -1);
                }
            }
            else {
                printf("h5fuse process terminated abnormally\n");
                fflush(stdout);
                MPI_Abort(comm_g, -1);
            }
        }

        /* Verify the size of the fused file */
        if (!enable_compression) {
            memset(&file_info, 0, sizeof(h5_stat_t));
            VRFY((HDstat(SUBF_FILENAME, &file_info) >= 0), "HDstat succeeded");
            VRFY(((size_t)file_info.st_size >= target_size), "File size verification succeeded");
        }

        /* Re-open file with sec2 driver and verify the data */
        file_id = H5Fopen(SUBF_FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
        VRFY((file_id >= 0), "H5Fopen succeeded");

        dset_id = H5Dopen2(file_id, "DSET", H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        buf = calloc(1, target_size);
        VRFY(buf, "calloc succeeded");

        VRFY((H5Dread(dset_id, SUBF_HDF5_TYPE, H5S_BLOCK, H5S_ALL, dxpl_id, buf) >= 0),
             "Dataset read succeeded");

        for (size_t i = 0; i < (size_t)mpi_size; i++) {
            for (size_t j = 0; j < count[0]; j++) {
                SUBF_C_TYPE buf_value = ((SUBF_C_TYPE *)buf)[(i * count[0]) + j];

                VRFY((buf_value == (SUBF_C_TYPE)(j + i)), "data verification succeeded");
            }
        }

        free(buf);
        buf = NULL;

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    /*
     * Since we've now fused the file back together, the Subfiling
     * VFD will no longer be able to work with it. The main HDF5
     * file should be able to be deleted using the sec2 VFD, but we
     * have to delete the extra files manually.
     */
    if (MAINPROCESS) {
        char *filename_buf;
        int   num_subfiles = num_iocs_g;
        int   num_digits   = (int)(log10(num_subfiles) + 1);

        /* Delete the regular HDF5 file */
        H5Pset_fapl_sec2(fapl_id);

        H5E_BEGIN_TRY
        {
            H5Fdelete(SUBF_FILENAME, fapl_id);
        }
        H5E_END_TRY

        filename_buf = malloc(PATH_MAX);
        VRFY(filename_buf, "malloc succeeded");

        /* Generate name for configuration file */
        snprintf(filename_buf, PATH_MAX, "%s/" H5FD_SUBFILING_CONFIG_FILENAME_TEMPLATE, config_dir,
                 SUBF_FILENAME, file_inode);

        /* Delete the configuration file */
        if (HDremove(filename_buf) < 0) {
            printf("couldn't remove Subfiling VFD configuration file '%s'\n", filename_buf);
            nerrors++;
        }

        for (int i = 0; i < num_subfiles; i++) {
            /* Generate name for each subfile */
            snprintf(filename_buf, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE, SUBF_FILENAME, file_inode,
                     num_digits, i + 1, num_subfiles);

            /* Delete the subfile */
            if (HDremove(filename_buf) < 0) {
                printf("couldn't remove subfile '%s'\n", filename_buf);
                nerrors++;
            }
        }

        free(filename_buf);
    }

    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");

    mpi_code_g = MPI_Barrier(comm_g);
    VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Barrier succeeded");

    CHECK_PASSED();
#else
    SKIPPED();
#endif
}
#undef SUBF_FILENAME
#undef SUBF_HDF5_TYPE
#undef SUBF_C_TYPE

static void
parse_subfiling_env_vars(void)
{
    char *env_value;

    if (NULL != (env_value = getenv(H5FD_SUBFILING_STRIPE_SIZE))) {
        stripe_size_g = strtoll(env_value, NULL, 0);
        if ((ERANGE == errno) || (stripe_size_g <= 0))
            stripe_size_g = -1;
    }

    if (NULL != (env_value = getenv(H5FD_SUBFILING_IOC_PER_NODE))) {
        ioc_per_node_g = strtol(env_value, NULL, 0);
        if ((ERANGE == errno) || (ioc_per_node_g <= 0))
            ioc_per_node_g = -1;
        else if (ioc_per_node_g * num_nodes_g > mpi_size)
            /*
             * If the number of IOCs per node from the environment
             * causes the total number of IOCs to exceed the number
             * of MPI ranks available, the Subfiling VFD will simply
             * use all of the MPI ranks on a node as IOCs
             */
            ioc_per_node_g = node_local_size;
    }

    if (NULL != (env_value = getenv(H5FD_IOC_THREAD_POOL_SIZE))) {
        ioc_thread_pool_size_g = atoi(env_value);
        if (ioc_thread_pool_size_g <= 0)
            ioc_thread_pool_size_g = -1;
    }

    if (NULL != (env_value = getenv(H5FD_SUBFILING_CONFIG_FILE_PREFIX))) {
        assert(config_dir);

        strncpy(config_dir, env_value, PATH_MAX);

        /* Just in case.. */
        config_dir[PATH_MAX - 1] = '\0';

        if (*config_dir == '\0') {
            *config_dir       = '.';
            *(config_dir + 1) = '\0';
        }
    }
}

int
main(int argc, char **argv)
{
    unsigned seed;
    bool     must_unset_stripe_size_env      = false;
    bool     must_unset_ioc_per_node_env     = false;
    bool     must_unset_ioc_thread_count_env = false;
    bool     must_unset_config_dir_env       = false;
    int      required                        = MPI_THREAD_MULTIPLE;
    int      provided                        = 0;

    HDcompile_assert(SUBFILING_MIN_STRIPE_SIZE <= H5FD_SUBFILING_DEFAULT_STRIPE_SIZE);

    /* Initialize MPI */
    if (MPI_SUCCESS != (mpi_code_g = MPI_Init_thread(&argc, &argv, required, &provided))) {
        printf("MPI_Init_thread failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }

    if (MPI_SUCCESS != (mpi_code_g = MPI_Comm_rank(comm_g, &mpi_rank))) {
        printf("MPI_Comm_rank failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }

    if (provided != required) {
        if (MAINPROCESS)
            printf("MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE\n");
        nerrors++;
        goto exit;
    }

    if (MPI_SUCCESS != (mpi_code_g = MPI_Comm_size(comm_g, &mpi_size))) {
        if (MAINPROCESS)
            printf("MPI_Comm_size failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }

    /* Split communicator according to node-local ranks */
    if (MPI_SUCCESS != (mpi_code_g = MPI_Comm_split_type(comm_g, MPI_COMM_TYPE_SHARED, mpi_rank,
                                                         MPI_INFO_NULL, &node_local_comm))) {
        if (MAINPROCESS)
            printf("MPI_Comm_split_type failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code_g = MPI_Comm_size(node_local_comm, &node_local_size))) {
        if (MAINPROCESS)
            printf("MPI_Comm_size failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code_g = MPI_Comm_rank(node_local_comm, &node_local_rank))) {
        if (MAINPROCESS)
            printf("MPI_Comm_rank failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }

    /* Get the number of nodes being run on */
    num_nodes_g = (node_local_rank == 0) ? 1 : 0;
    if (MPI_SUCCESS !=
        (mpi_code_g = MPI_Allreduce(MPI_IN_PLACE, &num_nodes_g, 1, MPI_INT, MPI_SUM, comm_g))) {
        if (MAINPROCESS)
            printf("MPI_Allreduce failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }

    /*
     * Split communicator according to rank value across nodes.
     * If the SELECT_IOC_ONE_PER_NODE IOC selection strategy is
     * used, each rank with a node local rank value of 0 will
     * be an IOC in the new communicator.
     */
    if (MPI_SUCCESS != (mpi_code_g = MPI_Comm_split(comm_g, node_local_rank, mpi_rank, &ioc_comm))) {
        if (MAINPROCESS)
            printf("MPI_Comm_split failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code_g = MPI_Comm_size(ioc_comm, &ioc_comm_size))) {
        if (MAINPROCESS)
            printf("MPI_Comm_size failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }
    if (MPI_SUCCESS != (mpi_code_g = MPI_Comm_rank(ioc_comm, &ioc_comm_rank))) {
        if (MAINPROCESS)
            printf("MPI_Comm_rank failed with error code %d\n", mpi_code_g);
        nerrors++;
        goto exit;
    }

    if (H5dont_atexit() < 0) {
        if (MAINPROCESS)
            printf("Failed to turn off atexit processing. Continue.\n");
    }

    H5open();

    if (MAINPROCESS) {
        printf("Testing Subfiling VFD functionality\n");
    }

    TestAlarmOn();

    /*
     * Obtain and broadcast seed value since ranks
     * aren't guaranteed to arrive here at exactly
     * the same time and could end up out of sync
     * with each other in regards to random number
     * generation
     */
    if (mpi_rank == 0)
        seed = (unsigned)time(NULL);

    if (mpi_size > 1) {
        if (MPI_SUCCESS != (mpi_code_g = MPI_Bcast(&seed, 1, MPI_UNSIGNED, 0, comm_g))) {
            if (MAINPROCESS)
                printf("MPI_Bcast failed with error code %d\n", mpi_code_g);
            nerrors++;
            goto exit;
        }
    }

    srand(seed);

    if (MAINPROCESS)
        printf("Using seed: %u\n\n", seed);

    /* Allocate buffer for possible config file directory specified */
    config_dir = malloc(PATH_MAX);
    if (!config_dir) {
        if (MAINPROCESS)
            printf("couldn't allocate space for subfiling config file directory buffer\n");
        nerrors++;
        goto exit;
    }

    /* Initialize to current working directory for now */
    snprintf(config_dir, PATH_MAX, ".");

    /* Grab values from environment variables if set */
    parse_subfiling_env_vars();

    /*
     * Assume that we use the "one IOC per node" selection
     * strategy by default, with a possibly modified
     * number of IOCs per node value
     */
    num_iocs_g = (ioc_per_node_g > 0) ? (int)ioc_per_node_g * num_nodes_g : num_nodes_g;
    if (num_iocs_g > mpi_size)
        num_iocs_g = mpi_size;

    for (size_t i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code_g = MPI_Barrier(comm_g))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS)
                MESG("MPI_Barrier failed");
            nerrors++;
        }
    }

    if (MAINPROCESS)
        puts("");

    if (MAINPROCESS)
        printf(" Re-running tests with compression enabled\n");

#ifdef H5_HAVE_FILTER_DEFLATE
    enable_compression = true;
    for (size_t i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code_g = MPI_Barrier(comm_g))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS)
                MESG("MPI_Barrier failed");
            nerrors++;
        }
    }
    enable_compression = false;
#else
    if (MAINPROCESS)
        SKIPPED();
#endif

    /*
     * Set any unset Subfiling environment variables and re-run
     * the tests as a quick smoke check of whether those are
     * working correctly
     */

    if (stripe_size_g < 0) {
        int64_t stripe_size;
        char    tmp[64];

        /*
         * Choose a random Subfiling stripe size between
         * the smallest allowed value and the default value
         */
        if (mpi_rank == 0) {
            stripe_size = (rand() % (H5FD_SUBFILING_DEFAULT_STRIPE_SIZE - SUBFILING_MIN_STRIPE_SIZE + 1)) +
                          SUBFILING_MIN_STRIPE_SIZE;
        }

        if (mpi_size > 1) {
            mpi_code_g = MPI_Bcast(&stripe_size, 1, MPI_INT64_T, 0, comm_g);
            VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Bcast succeeded");
        }

        snprintf(tmp, sizeof(tmp), "%" PRId64, stripe_size);

        if (HDsetenv(H5FD_SUBFILING_STRIPE_SIZE, tmp, 1) < 0) {
            if (MAINPROCESS)
                printf("HDsetenv failed\n");
            nerrors++;
            goto exit;
        }

        must_unset_stripe_size_env = true;
    }
    if (ioc_per_node_g < 0) {
        const char *ioc_per_node_str;

        if (2 * num_nodes_g <= mpi_size)
            ioc_per_node_str = "2";
        else
            ioc_per_node_str = "1";

        if (HDsetenv(H5FD_SUBFILING_IOC_PER_NODE, ioc_per_node_str, 1) < 0) {
            if (MAINPROCESS)
                printf("HDsetenv failed\n");
            nerrors++;
            goto exit;
        }

        must_unset_ioc_per_node_env = true;
    }
    if (ioc_thread_pool_size_g < 0) {
        if (HDsetenv(H5FD_IOC_THREAD_POOL_SIZE, "2", 1) < 0) {
            if (MAINPROCESS)
                printf("HDsetenv failed\n");
            nerrors++;
            goto exit;
        }

        must_unset_ioc_thread_count_env = true;
    }

    if (NULL == getenv(H5FD_SUBFILING_CONFIG_FILE_PREFIX)) {
        int rand_value = 0;

        if (MAINPROCESS)
            rand_value = rand() % 2;

        mpi_code_g = MPI_Bcast(&rand_value, 1, MPI_INT, 0, comm_g);
        VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Bcast succeeded");

        /* Randomly set config file prefix to either "." or a real
         * directory to test both cases
         */
        if (rand_value == 0) {
            int mkdir_success = 0;

            if (MAINPROCESS) {
                if ((HDmkdir(SUBFILING_CONFIG_FILE_DIR, (mode_t)0755) < 0) && (errno != EEXIST)) {
                    printf("couldn't create temporary testing directory\n");
                    mkdir_success = 0;
                }
                else
                    mkdir_success = 1;
            }

            mpi_code_g = MPI_Bcast(&mkdir_success, 1, MPI_INT, 0, comm_g);
            VRFY((mpi_code_g == MPI_SUCCESS), "MPI_Bcast succeeded");

            if (!mkdir_success) {
                if (MAINPROCESS)
                    printf("HDmkdir failed\n");
                nerrors++;
                goto exit;
            }

            if (HDsetenv(H5FD_SUBFILING_CONFIG_FILE_PREFIX, SUBFILING_CONFIG_FILE_DIR, 1) < 0) {
                if (MAINPROCESS)
                    printf("HDsetenv failed\n");
                nerrors++;
                goto exit;
            }
        }
        else {
            if (HDsetenv(H5FD_SUBFILING_CONFIG_FILE_PREFIX, ".", 1) < 0) {
                if (MAINPROCESS)
                    printf("HDsetenv failed\n");
                nerrors++;
                goto exit;
            }
        }

        must_unset_config_dir_env = true;
    }

    /* Grab values from environment variables */
    parse_subfiling_env_vars();

    /*
     * Assume that we use the "one IOC per node" selection
     * strategy by default, with a possibly modified
     * number of IOCs per node value
     */
    num_iocs_g = (ioc_per_node_g > 0) ? (int)ioc_per_node_g * num_nodes_g : num_nodes_g;
    if (num_iocs_g > mpi_size)
        num_iocs_g = mpi_size;

    if (MAINPROCESS)
        printf("\nRe-running tests with environment variables set\n");

    for (size_t i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code_g = MPI_Barrier(comm_g))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS)
                MESG("MPI_Barrier failed");
            nerrors++;
        }
    }

    if (MAINPROCESS)
        printf("\n Re-running tests with compression enabled\n");

#ifdef H5_HAVE_FILTER_DEFLATE
    enable_compression = true;
    for (size_t i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code_g = MPI_Barrier(comm_g))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS)
                MESG("MPI_Barrier failed");
            nerrors++;
        }
    }
    enable_compression = false;
#else
    if (MAINPROCESS)
        SKIPPED();
#endif

    if (MAINPROCESS)
        printf("\nRe-running tests with environment variables set to the empty string\n");

    HDsetenv("H5FD_SUBFILING_SUBFILE_PREFIX", "", 1);
    HDsetenv("H5FD_SUBFILING_IOC_SELECTION_CRITERIA", "", 1);
    HDsetenv("H5FD_SUBFILING_IOC_PER_NODE", "", 1);
    HDsetenv("H5FD_SUBFILING_STRIPE_SIZE", "", 1);
    HDsetenv("H5FD_SUBFILING_CONFIG_FILE_PREFIX", "", 1);

    /* Grab values from environment variables if set */
    parse_subfiling_env_vars();

    /*
     * Assume that we use the "one IOC per node" selection
     * strategy by default, with a possibly modified
     * number of IOCs per node value
     */
    num_iocs_g = (ioc_per_node_g > 0) ? (int)ioc_per_node_g * num_nodes_g : num_nodes_g;
    if (num_iocs_g > mpi_size)
        num_iocs_g = mpi_size;

    for (size_t i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code_g = MPI_Barrier(comm_g))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS)
                MESG("MPI_Barrier failed");
            nerrors++;
        }
    }

    HDunsetenv("H5FD_SUBFILING_SUBFILE_PREFIX");
    HDunsetenv("H5FD_SUBFILING_IOC_SELECTION_CRITERIA");
    HDunsetenv("H5FD_SUBFILING_IOC_PER_NODE");
    HDunsetenv("H5FD_SUBFILING_STRIPE_SIZE");
    HDunsetenv("H5FD_SUBFILING_CONFIG_FILE_PREFIX");

    if (nerrors)
        goto exit;

    if (MAINPROCESS)
        puts("\nAll Subfiling VFD tests passed\n");

exit:
    if (must_unset_stripe_size_env)
        HDunsetenv(H5FD_SUBFILING_STRIPE_SIZE);
    if (must_unset_ioc_per_node_env)
        HDunsetenv(H5FD_SUBFILING_IOC_PER_NODE);
    if (must_unset_ioc_thread_count_env)
        HDunsetenv(H5FD_IOC_THREAD_POOL_SIZE);
    if (must_unset_config_dir_env)
        HDunsetenv(H5FD_SUBFILING_CONFIG_FILE_PREFIX);

    if (MAINPROCESS) {
        if (HDrmdir(SUBFILING_CONFIG_FILE_DIR) < 0 && (errno != ENOENT)) {
            printf("couldn't remove temporary testing directory\n");
            nerrors++;
        }
    }

    if (nerrors) {
        if (MAINPROCESS)
            printf("*** %d TEST ERROR%s OCCURRED ***\n", nerrors, nerrors > 1 ? "S" : "");
    }

    TestAlarmOff();

    H5close();

    if (MPI_COMM_WORLD != ioc_comm)
        MPI_Comm_free(&ioc_comm);
    if (MPI_COMM_WORLD != node_local_comm)
        MPI_Comm_free(&node_local_comm);

    MPI_Finalize();

    exit(nerrors ? EXIT_FAILURE : EXIT_SUCCESS);
}

#else /* H5_HAVE_SUBFILING_VFD */

int
main(void)
{
    h5_reset();
    printf("Testing Subfiling VFD functionality\n");
    printf("SKIPPED - Subfiling VFD not built\n");
    exit(EXIT_SUCCESS);
}

#endif /* H5_HAVE_SUBFILING_VFD */
