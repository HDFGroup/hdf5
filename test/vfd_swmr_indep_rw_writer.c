/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This program checks independence of writer and reader for VFD SWMR.
 * A writer can also be a reader of another writer. A reader can also be a
 * writer. The program adapts from the "many small" and the "few big" tests.
 * One can test the program as follows:
 * Run ./vfd_swmr_indep_wr_p0 at one terminal
 * Run ./vfd_swmr_indep_wr_p1 at another terminal
 * You should see something like:
 * Successfully write the dataset /dataset-0 for file vfd_swmr_indep_wr_p0.h5.
 * Successfully open the dataset /dataset-1 for file vfd_swmr_indep_wr_p1.h5.
 * Successfully verify a dataset.
 * and
 * Successfully open the dataset /dataset-0 for file vfd_swmr_indep_wr_p0.h5.
 * Successfully verify a dataset.
 * Successfully write the dataset /dataset-1 for file vfd_swmr_indep_wr_p1.h5.
 */

#define H5C_FRIEND /*suppress error about including H5Cpkg   */
#define H5F_FRIEND /*suppress error about including H5Fpkg   */

#include "hdf5.h"
#include "H5Cpkg.h"
#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define DATA_ROWS    256
#define DATA_COLS    512
#define DATA_RANK    2
#define NUM_ATTEMPTS 100

/* Structure for filling the dataset values. Adapted from vfd_swmr_bigset_writer.c. */
typedef struct _mat {
    unsigned rows, cols;
    uint32_t elt[1];
} mat_t;

/* Structure to hold the information of various parameters used in the program. */
typedef struct {
    hid_t        file[2];
    const char * filename[2];
    char         progname[PATH_MAX];
    hid_t        r_dsetid;
    hid_t        dtype, fapl, fcpl;
    unsigned int rows, cols;
    int          rank;
    hsize_t      dims[DATA_RANK];
    uint32_t     max_lag;
    uint32_t     tick_len;
    uint32_t     ps;
    uint32_t     pbs;
    uint32_t     check_interval;
    hbool_t      use_vfd_swmr;
    hbool_t      first_proc;
} state_t;

/* Obtain the data value at index [i][j] for the 2D matrix.
   All the routines related to the matrix are adapted from the vfd_swmr_bigset_writer.c. */
static uint32_t
matget(const mat_t *mat, unsigned i, unsigned j)
{
    return mat->elt[i * mat->cols + j];
}

/* Set the data value at index [i][j] for the 2D matrix. */
static hbool_t
matset(mat_t *mat, unsigned i, unsigned j, uint32_t v)
{
    if (i >= mat->rows || j >= mat->cols) {
        HDfprintf(stderr, "index out of boundary\n");
        TEST_ERROR;
    }

    mat->elt[i * mat->cols + j] = v;

    return TRUE;

error:
    return FALSE;
}

/* Allocate memory for the 2-D matrix. */
static mat_t *
newmat(state_t *s)
{
    mat_t *mat;

    mat = HDmalloc(sizeof(*mat) + (s->rows * s->cols - 1) * sizeof(mat->elt[0]));
    if (mat == NULL) {
        HDfprintf(stderr, "HDmalloc failed\n");
        TEST_ERROR;
    }

    mat->rows = s->rows;
    mat->cols = s->cols;

    return mat;

error:
    return NULL;
}

/* Write or verify the dataset test pattern in the matrix `mat`.
 * If `do_set` is TRUE, write the pattern; otherwise, verify.
 *
 * The basic test pattern consists of increasing
 * integers written in nested corners of the dataset
 * starting at element (0, 0):
 *
 *  0
 *
 *  0  1
 *  3  2
 *
 *  0  1  4
 *  3  2  5
 *  8  7  6
 *
 *  0  1  4  9
 *  3  2  5 10
 *  8  7  6 11
 * 15 14 13 12
 *
 * In an actual pattern, the dataset number, `which`, is added to each integer.
 *
 */
static hbool_t
set_or_verify_matrix(mat_t *mat, unsigned int which, hbool_t do_set)
{
    unsigned row, col;
    hbool_t  ret = TRUE;

    for (row = 0; row < mat->rows; row++) {
        for (col = 0; col < mat->cols; col++) {
            uint32_t v;
            hsize_t  i = row, j = col, u;
            if (j <= i)
                u = (i + 1) * (i + 1) - 1 - j;
            else
                u = j * j + i;

            v = (uint32_t)(u + which);

            if (do_set) {
                if (!matset(mat, row, col, v)) {
                    HDfprintf(stderr, "data initialization failed\n");
                    ret = FALSE;
                    break;
                }
            }
            else if (matget(mat, row, col) != v) {
                /* If the data doesn't match, return FALSE
                 */
                dbgf(1, "vrfy_matrix failed at row %u,col %u\n", row, col);
                dbgf(1, "real value is %u\n", matget(mat, row, col));
                dbgf(1, "expected value is %u\n", v);
                ret = FALSE;
                break;
            }
        }
    }

    return ret;
}

/* Initialize the matrix values. The parameter 'which' determines the data value. */
static hbool_t
init_matrix(mat_t *mat, unsigned int which)
{
    return set_or_verify_matrix(mat, which, TRUE);
}

/* Verify the matrix values at each point. The parameter 'which" determines the data value. */
static hbool_t
verify_matrix(mat_t *mat, unsigned int which)
{
    return set_or_verify_matrix(mat, which, FALSE);
}

/* Usage of this program when running with the -h option */
static void
usage(const char *progname)
{
    HDfprintf(stderr,
              "usage: %s [-S] [-c cols] [-r rows] [-t tick_len] [-m max_lag] \n"
              "    [-B page_buffer_size] [-s page_size] [-u reader wait interval] [-q silent output] \n"
              "\n"
              "-S:	               do not use VFD SWMR\n"
              "-c cols:	       `cols` columns for the dataset\n"
              "                      The default value is 512.\n"
              "-r rows:	       `rows` rows for the dataset\n"
              "                      The default value is 256.\n"
              "-t tick_len:    length of a tick in tenths of a second.\n"
              "-m max_lag:     maximum expected lag(in ticks) between writer and readers\n"
              "-B pbs:         page buffer size in bytes:\n"
              "                The default value is 4K(4096).\n"
              "-s ps:          page size used by page aggregation, page buffer and \n"
              "                the metadata file. The default value is 4K(4096).\n"
              "-u 0.1s:        interval in tenth of seconds to check if a dataset is ready for the reader.\n"
              "-q:             silence printouts, few messages\n"
              "\n",
              progname);
    HDexit(EXIT_FAILURE);
}

/* Initialize the state_t with different options specified by the user. */
static hbool_t
state_init(state_t *s, int argc, char **argv)
{
    unsigned long          tmp;
    int                    opt;
    char *                 tfile = NULL;
    char *                 end;
    const char *           personality;
    const char *           s_opts   = "Sqc:r:t:m:B:s:u:";
    struct h5_long_options l_opts[] = {{NULL, 0, '\0'}};

    s->file[0]        = H5I_INVALID_HID;
    s->file[1]        = H5I_INVALID_HID;
    s->filename[0]    = "";
    s->filename[1]    = "";
    s->r_dsetid       = H5I_INVALID_HID;
    s->dtype          = H5T_NATIVE_UINT32;
    s->fapl           = H5I_INVALID_HID;
    s->fcpl           = H5I_INVALID_HID;
    s->rows           = DATA_ROWS;
    s->cols           = DATA_COLS;
    s->rank           = DATA_RANK;
    s->dims[0]        = DATA_ROWS;
    s->dims[1]        = DATA_COLS;
    s->max_lag        = 7;
    s->tick_len       = 4;
    s->ps             = 4096;
    s->pbs            = 4096;
    s->check_interval = 1;
    s->use_vfd_swmr   = TRUE;
    s->first_proc     = TRUE;

    if (H5_basename(argv[0], &tfile) < 0) {
        HDprintf("H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile) {
        HDfree(tfile);
        tfile = NULL;
    }

    while ((opt = H5_get_option(argc, (const char *const *)argv, s_opts, l_opts)) != EOF) {
        switch (opt) {
            case 'S':
                s->use_vfd_swmr = FALSE;
                break;
            case 'c':
            case 'r':
            case 't':
            case 'm':
            case 'B':
            case 's':
            case 'u':
                errno = 0;
                tmp   = HDstrtoul(H5_optarg, &end, 0);
                if (end == H5_optarg || *end != '\0') {
                    HDfprintf(stderr, "couldn't parse -%c argument %s\n", opt, H5_optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    HDfprintf(stderr, "couldn't parse -%c argument %s\n", opt, H5_optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    HDfprintf(stderr, "-%c argument %lu too large", opt, tmp);
                    TEST_ERROR;
                }
                if ((opt == 'c' || opt == 'r') && tmp == 0) {
                    HDfprintf(stderr, "-%c argument %lu must be >= 1", opt, tmp);
                    TEST_ERROR;
                }

                if (opt == 'c')
                    s->cols = (unsigned)tmp;
                else if (opt == 'r')
                    s->rows = (unsigned)tmp;
                else if (opt == 't')
                    s->tick_len = (unsigned)tmp;
                else if (opt == 'm')
                    s->max_lag = (unsigned)tmp;
                else if (opt == 'B')
                    s->pbs = (unsigned)tmp;
                else if (opt == 's')
                    s->ps = (unsigned)tmp;
                else if (opt == 'u')
                    s->check_interval = (unsigned)tmp;
                break;
            case 'q':
                verbosity = 0;
                break;
            case '?':
            default:
                usage(s->progname);
                break;
        }
    }
    argc -= H5_optind;
    argv += H5_optind;

    if (argc > 0) {
        HDprintf("unexpected command-line arguments\n");
        TEST_ERROR;
    }

    s->filename[0] = "vfd_swmr_indep_wr_p0.h5";
    s->filename[1] = "vfd_swmr_indep_wr_p1.h5";
    s->dims[0]     = s->rows;
    s->dims[1]     = s->cols;

    personality = HDstrstr(s->progname, "vfd_swmr_indep_wr");

    if (personality != NULL && HDstrcmp(personality, "vfd_swmr_indep_wr_p0") == 0)
        s->first_proc = TRUE;
    else if (personality != NULL && HDstrcmp(personality, "vfd_swmr_indep_wr_p1") == 0)
        s->first_proc = FALSE;
    else {
        HDprintf("unknown personality, expected vfd_swmr_indep_wr_{p0,p1}\n");
        TEST_ERROR;
    }

    return TRUE;

error:
    if (tfile)
        HDfree(tfile);
    return FALSE;
}

/* Initialize the configuration and the file creation and access property lists
 *  for the VFD SMWR independence of the reader/writer test.
 */
static hbool_t
indep_init_vfd_swmr_config_plist(state_t *s, hbool_t writer, const char *mdf_name)
{

    H5F_vfd_swmr_config_t *config = NULL;

    if (NULL == (config = HDcalloc(1, sizeof(H5F_vfd_swmr_config_t))))
        TEST_ERROR;

    /* config, tick_len, max_lag, presume_posix_semantics, writer,
     * maintain_metadata_file, generate_updater_files, flush_raw_data, md_pages_reserved,
     * md_file_path, md_file_name, updater_file_path */
    init_vfd_swmr_config(config, s->tick_len, s->max_lag, FALSE, writer, TRUE, FALSE, TRUE, 128, "./",
                         mdf_name, NULL);

    /* Pass the use_vfd_swmr, only_meta_page, page buffer size, config to vfd_swmr_create_fapl().*/
    if ((s->fapl = vfd_swmr_create_fapl(TRUE, s->use_vfd_swmr, TRUE, s->pbs, config)) < 0) {
        HDprintf("vfd_swmr_create_fapl failed\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((s->fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, s->ps)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        TEST_ERROR;
    }

    HDfree(config);

    return TRUE;

error:
    HDfree(config);
    return FALSE;
}

/* Write the matrix mat to a dataset. The dataset name depends on the process it runs. */
static hbool_t
write_dataset(const state_t *s, mat_t *mat)
{

    char         dname[sizeof("/dataset-?")];
    hid_t        dset_id   = H5I_INVALID_HID;
    hid_t        filespace = H5I_INVALID_HID;
    unsigned int which     = 0;

    if (s->first_proc)
        esnprintf(dname, sizeof(dname), "/dataset-%d", 0);
    else {
        esnprintf(dname, sizeof(dname), "/dataset-%d", 1);
        which = 1;
    }

    filespace = H5Screate_simple(s->rank, s->dims, NULL);
    if (filespace < 0) {
        HDfprintf(stderr, "H5Screate_simple failed\n");
        TEST_ERROR;
    }

    if ((dset_id = H5Dcreate2(s->file[which], dname, s->dtype, filespace, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "H5Dcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(filespace) < 0) {
        HDfprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    if (!init_matrix(mat, which)) {
        HDfprintf(stderr, "data initialization failed\n");
        TEST_ERROR;
    }

    if (H5Dwrite(dset_id, H5T_NATIVE_UINT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, mat->elt) < 0) {
        HDfprintf(stderr, "H5Dwrite failed\n");
        TEST_ERROR;
    }

    if (H5Dclose(dset_id) < 0) {
        HDfprintf(stderr, "H5Dclose failed\n");
        TEST_ERROR;
    }

    if (s->first_proc)
        dbgf(1, "Process 0: Successfully write the dataset %s for file %s.\n", dname, s->filename[which]);
    else
        dbgf(1, "Process 1: Successfully write the dataset %s for file %s.\n", dname, s->filename[which]);

    return TRUE;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(filespace);
        H5Dclose(dset_id);
    }
    H5E_END_TRY;

    return FALSE;
}

/* Open the dataset according to the dataset name related to the process.
   The dataset ID is saved in the state_t s */
static hbool_t
open_dset(state_t *s)
{

    char         dname[sizeof("/dataset-?")];
    hid_t        dset_id;
    unsigned int i;
    unsigned int fopen_idx = 0;

    /* The first process reads the dataset from the second process
     * The second process reads the dataset from the first process
     */
    if (s->first_proc) {
        esnprintf(dname, sizeof(dname), "/dataset-%d", 1);
        fopen_idx = 1;
    }
    else {
        esnprintf(dname, sizeof(dname), "/dataset-%d", 0);
        fopen_idx = 0;
    }

    /* Same as the big dset test, tries to open the dataset repeatedly until successful.  After trying
     * NUM_ATTEMPTS times without success, report it as a failure
     */
    for (i = 0; i < NUM_ATTEMPTS; i++) {
        H5E_BEGIN_TRY
        {
            dset_id = H5Dopen2(s->file[fopen_idx], dname, H5P_DEFAULT);
        }
        H5E_END_TRY;

        if (dset_id >= 0)
            break;
        else
            decisleep(s->check_interval);
    }

    s->r_dsetid = dset_id;
    if (i == NUM_ATTEMPTS) {
        HDfprintf(stderr, "dataset opening reaches the maximal number of attempts\n");
        TEST_ERROR;
    }

    if (s->first_proc)
        dbgf(1, "Process 0: Successfully open the dataset %s for file %s.\n", dname, s->filename[fopen_idx]);
    else
        dbgf(1, "Process 1: Successfully open the dataset %s for file %s.\n", dname, s->filename[fopen_idx]);
    return TRUE;

error:
    return FALSE;
}

/* Verify a dataset, this routine must be called after the open_dset().
 * It will use the dataset ID assigned by open_dset().
 */
static hbool_t
vrfy_dset(const state_t *s, mat_t *mat)
{

    unsigned int i;
    unsigned int which = 0;
    herr_t       status;

    /* The approach is adapted from the big set test. A failure to read the data may indicate the data isn't
     * ready yet. For this case, we will not display the error stack, instead sleeping for one tenth of a
     * second, call H5Drefresh and try to call H5Dread again. After NUM_ATTEMPTS times, issue an error.
     */

    for (i = 0; i < NUM_ATTEMPTS; i++) {
        H5E_BEGIN_TRY
        {
            status = H5Dread(s->r_dsetid, H5T_NATIVE_UINT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, mat->elt);
        }
        H5E_END_TRY;
        if (status < 0) {
            decisleep(1);
            /* Refresh the dataset and try it again */
            if (H5Drefresh(s->r_dsetid) < 0) {
                HDfprintf(stderr, "H5Drefresh failed\n");
                TEST_ERROR;
            }
        }
        else
            break;
    }
    if (i == NUM_ATTEMPTS) {
        HDfprintf(stderr, "dataset verification reached the maximal number of attempts\n");
        TEST_ERROR;
    }
    if (s->first_proc)
        which = 1;

    if (verify_matrix(mat, which) == FALSE) {
        HDfprintf(stderr, "dataset verification failed\n");
        TEST_ERROR;
    }

    if (s->first_proc)
        dbgf(1, "Process 0: Successfully verify a dataset.\n");
    else
        dbgf(1, "Process 1: Successfully verify a dataset.\n");

    return TRUE;

error:
    return FALSE;
}

/* The wrapper routine of open_dset() and vrfy_dset() for readers to verify a dataset.  */
static hbool_t
read_vrfy_dataset(state_t *s, mat_t *mat)
{

    if (FALSE == open_dset(s)) {
        HDfprintf(stderr, "Reader: open_dataset() failed\n");
        TEST_ERROR;
    }
    if (FALSE == vrfy_dset(s, mat)) {
        HDfprintf(stderr, "Reader: vrfy_dataset() failed\n");
        TEST_ERROR;
    }

    if (H5Dclose(s->r_dsetid) < 0) {
        HDfprintf(stderr, "Reader: H5Dclose() failed\n");
        TEST_ERROR;
    }
    return TRUE;

error:

    H5E_BEGIN_TRY
    {
        H5Dclose(s->r_dsetid);
    }
    H5E_END_TRY;
    return FALSE;
}

/* Close the file access and creation property lists. */
static hbool_t
close_pl(const state_t *s)
{

    if (H5Pclose(s->fapl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(s->fcpl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    return TRUE;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(s->fapl);
        H5Pclose(s->fcpl);
    }
    H5E_END_TRY;

    return FALSE;
}

int
main(int argc, char **argv)
{
    hbool_t  writer = TRUE;
    state_t *s      = NULL;
    mat_t *  mat    = NULL;
    hbool_t  ret    = FALSE;

    if (NULL == (s = HDcalloc(1, sizeof(state_t))))
        TEST_ERROR;

    if (!state_init(s, argc, argv)) {
        HDfprintf(stderr, "state_init failed\n");
        TEST_ERROR;
    }

    if ((mat = newmat(s)) == NULL) {
        HDfprintf(stderr, "could not allocate memory for dataset \n");
        TEST_ERROR;
    }

    /* The first process writes a dataset in the first file and then reads a dataset from the second file.*/
    if (s->first_proc) {

        writer = TRUE;
        if (FALSE == indep_init_vfd_swmr_config_plist(s, writer, "file1-shadow")) {
            HDfprintf(stderr, "Writer: Cannot initialize file property lists for file %s\n", s->filename[0]);
            TEST_ERROR;
        }
        s->file[0] = H5Fcreate(s->filename[0], H5F_ACC_TRUNC, s->fcpl, s->fapl);
        if (s->file[0] < 0) {
            HDfprintf(stderr, "H5Fcreate failed for the file %s\n", s->filename[0]);
            TEST_ERROR;
        }

        ret = write_dataset(s, mat);
        if (ret == FALSE) {
            HDfprintf(stderr, "write_dataset failed for the file %s\n", s->filename[0]);
            TEST_ERROR;
        }

        /* writer makes repeated HDF5 API calls
         * to trigger EOT at approximately the correct time */
        for (unsigned i = 0; i < s->max_lag + 1; i++) {
            decisleep(s->tick_len);
            H5E_BEGIN_TRY
            {
                H5Aexists(s->file[0], "nonexistent");
            }
            H5E_END_TRY;
        }

        if (FALSE == close_pl(s)) {
            HDfprintf(stderr, "Fail to close file property lists for writing the file %s.\n", s->filename[0]);
            TEST_ERROR;
        }

        writer = FALSE;
        if (FALSE == indep_init_vfd_swmr_config_plist(s, writer, "file2-shadow")) {
            HDfprintf(stderr, "Reader: Cannot initialize file property lists for file %s\n", s->filename[1]);
            TEST_ERROR;
        }
        s->file[1] = H5Fopen(s->filename[1], H5F_ACC_RDONLY, s->fapl);
        if (s->file[1] < 0) {
            HDfprintf(stderr, "H5Fopen failed for the file %s\n", s->filename[1]);
            TEST_ERROR;
        }

        ret = read_vrfy_dataset(s, mat);
        if (ret == FALSE) {
            HDfprintf(stderr, "read and verify dataset failed for file %s\n", s->filename[1]);
            TEST_ERROR;
        }

        if (FALSE == close_pl(s)) {
            HDfprintf(stderr, "Fail to close file property lists for reading the file %s.\n", s->filename[1]);
            TEST_ERROR;
        }

        if (H5Fclose(s->file[0]) < 0) {
            HDfprintf(stderr, "fail to close HDF5 file %s \n", s->filename[0]);
            TEST_ERROR;
        }

        if (H5Fclose(s->file[1]) < 0) {
            HDfprintf(stderr, "fail to close HDF5 file %s \n", s->filename[1]);
            TEST_ERROR;
        }
    }
    else {

        /* The second process reads the dataset of the first file generated by the first process,
         * then writes a dataset in the second file for the first process to read.
         */
        writer = FALSE;
        if (FALSE == indep_init_vfd_swmr_config_plist(s, writer, "file1-shadow")) {
            HDfprintf(stderr, "Reader: Cannot initialize file property lists for file %s\n", s->filename[0]);
            TEST_ERROR;
        }

        s->file[0] = H5Fopen(s->filename[0], H5F_ACC_RDONLY, s->fapl);
        if (s->file[0] < 0) {
            HDfprintf(stderr, "H5Fopen failed for the file %s\n", s->filename[0]);
            TEST_ERROR;
        }
        ret = read_vrfy_dataset(s, mat);
        if (ret == FALSE) {
            HDfprintf(stderr, "read and verify dataset failed for file %s\n", s->filename[0]);
            TEST_ERROR;
        }

        if (FALSE == close_pl(s)) {
            HDfprintf(stderr, "Fail to close file property lists for reading the file %s.\n", s->filename[0]);
            TEST_ERROR;
        }

        writer = TRUE;
        if (FALSE == indep_init_vfd_swmr_config_plist(s, writer, "file2-shadow")) {
            HDfprintf(stderr, "writer: Cannot initialize file property lists for file %s\n", s->filename[1]);
            TEST_ERROR;
        }

        s->file[1] = H5Fcreate(s->filename[1], H5F_ACC_TRUNC, s->fcpl, s->fapl);
        if (s->file[1] < 0) {
            HDfprintf(stderr, "H5Fcreate failed for the file %s\n", s->filename[1]);
            TEST_ERROR;
        }
        ret = write_dataset(s, mat);
        if (ret == FALSE) {
            HDfprintf(stderr, "write_dataset failed for the file %s\n", s->filename[1]);
            TEST_ERROR;
        }

        /* writer makes repeated HDF5 API calls
         * to trigger EOT at approximately the correct time */
        for (unsigned i = 0; i < s->max_lag + 1; i++) {
            decisleep(s->tick_len);
            H5E_BEGIN_TRY
            {
                H5Aexists(s->file[1], "nonexistent");
            }
            H5E_END_TRY;
        }

        if (FALSE == close_pl(s)) {
            HDfprintf(stderr, "Fail to close file property lists for writing the file %s.\n", s->filename[1]);
            TEST_ERROR;
        }

        if (H5Fclose(s->file[0]) < 0) {
            HDfprintf(stderr, "fail to close HDF5 file %s \n", s->filename[0]);
            TEST_ERROR;
        }

        if (H5Fclose(s->file[1]) < 0) {
            HDfprintf(stderr, "fail to close HDF5 file %s \n", s->filename[1]);
            TEST_ERROR;
        }
    }

    HDfree(mat);
    HDfree(s);

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(s->fapl);
        H5Pclose(s->fcpl);
        H5Fclose(s->file[0]);
        H5Fclose(s->file[1]);
    }
    H5E_END_TRY;

    HDfree(mat);
    HDfree(s);

    return EXIT_FAILURE;
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
}

#endif /* H5_HAVE_WIN32_API */
