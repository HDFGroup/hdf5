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
 *      This file is a catchall for parallel VFD tests.
 */

#include "testphdf5.h"

#ifdef H5_HAVE_SUBFILING_VFD
#include "H5FDsubfiling.h"
#include "H5FDioc.h"
#endif

/* Must be a power of 2.  Reducing it below 1024 may cause problems */
#define INTS_PER_RANK 1024

/* global variable declarations: */

static MPI_Comm comm = MPI_COMM_WORLD;
static MPI_Info info = MPI_INFO_NULL;

static bool        pass               = true; /* set to false on error */
static bool        disp_failure_mssgs = true; /* global force display of failure messages */
static const char *failure_mssg       = NULL;

const char *FILENAMES[] = {"mpio_vfd_test_file_0",      /*0*/
                           "mpio_vfd_test_file_1",      /*1*/
                           "mpio_vfd_test_file_2",      /*2*/
                           "mpio_vfd_test_file_3",      /*3*/
                           "mpio_vfd_test_file_4",      /*4*/
                           "mpio_vfd_test_file_5",      /*5*/
                           "mpio_vfd_test_file_6",      /*6*/
                           "mpio_vfd_test_file_7",      /*7*/
                           "subfiling_vfd_test_file_0", /*8*/
                           "subfiling_vfd_test_file_1", /*9*/
                           "subfiling_vfd_test_file_2", /*10*/
                           "subfiling_vfd_test_file_3", /*11*/
                           "subfiling_vfd_test_file_4", /*12*/
                           "subfiling_vfd_test_file_5", /*13*/
                           "subfiling_vfd_test_file_6", /*14*/
                           NULL};

/* File Test Images
 *
 * Pointers to dynamically allocated buffers of size
 * INTS_PER_RANK * sizeof(int32_t) * mpi_size().  These
 * buffers are used to put the test file in a known
 * state, and to test if the test file contains the
 * expected data.
 */

int32_t *increasing_fi_buf = NULL;
int32_t *decreasing_fi_buf = NULL;
int32_t *negative_fi_buf   = NULL;
int32_t *zero_fi_buf       = NULL;
int32_t *read_fi_buf       = NULL;

/* local utility function declarations */

static unsigned alloc_and_init_file_images(int mpi_size);
static void     free_file_images(void);
static void setup_vfd_test_file(int file_name_id, char *file_name, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name, haddr_t eoa,
                                H5FD_t **lf_ptr, hid_t *fapl_id_ptr, hid_t *dxpl_id_ptr);
static void takedown_vfd_test_file(int mpi_rank, char *filename, H5FD_t **lf_ptr, hid_t *fapl_id_ptr,
                                   hid_t *dxpl_id_ptr);

/* test functions */
static unsigned vector_read_test_1(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_read_test_2(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_read_test_3(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_read_test_4(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_read_test_5(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);

static unsigned vector_write_test_1(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_write_test_2(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_write_test_3(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_write_test_4(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_write_test_5(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_write_test_6(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_write_test_7(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
static unsigned vector_write_test_8(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name);
/*
 * Tests for selection I/O:
 * They are derived from test_selection_io() in test/vfd.c and modified for parallel testing.
 */

/*
 * Global declarations for selection I/O tests`
 */

/* Number of errors */
int nerrors      = 0;
int curr_nerrors = 0;

/* Test file name */
#define SELECT_FNAME "mpio_select_test_file"

/* Dimemsion sizes */
#define SEL_IO_DIM0 4
#define SEL_IO_DIM1 8
int sel_dim0 = SEL_IO_DIM0;
int sel_dim1 = SEL_IO_DIM1;

/* Write buffers */
int *wbuf1    = NULL;
int *wbuf2    = NULL;
int *wbufs[2] = {NULL, NULL};

/* File buffers */
int *fbuf1    = NULL;
int *fbuf2    = NULL;
int *fbufs[2] = {NULL, NULL}; /* Array of file buffers     */

/* Expected read buffers */
int *erbuf1    = NULL;
int *erbuf2    = NULL;
int *erbufs[2] = {NULL, NULL}; /* Array of expected read buffers */

/* iotypes for testing:
    H5FD_MPIO_INDEPENDENT
    H5FD_MPIO_COLLECTIVE
    --H5FD_MPIO_COLLECTIVE_IO
    --H5FD_MPIO_INDIVIDUAL_IO
*/
#define iotypes 3

#define P_TEST_ERROR                                                                                         \
    do {                                                                                                     \
        nerrors++;                                                                                           \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
    } while (0)

#define CHECK_PASSED()                                                                                       \
    do {                                                                                                     \
        int err_result = (nerrors > curr_nerrors);                                                           \
                                                                                                             \
        MPI_Allreduce(MPI_IN_PLACE, &err_result, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);                       \
                                                                                                             \
        if (MAINPROCESS) {                                                                                   \
            if (err_result == 0)                                                                             \
                PASSED();                                                                                    \
            else                                                                                             \
                puts("     ***TEST FAILED***");                                                              \
        }                                                                                                    \
    } while (0)

/* Utility functions for selection I/O */
static herr_t test_selection_io_read_verify(hid_t dxpl, int mpi_rank, hsize_t start[], hsize_t block[],
                                            H5FD_t *lf, H5FD_mem_t type, uint32_t count, hid_t mem_spaces[],
                                            hid_t file_spaces[], haddr_t offsets[], size_t element_sizes[],
                                            uint32_t rbufcount, int *erb[], bool shorten_rbufs);

static herr_t test_selection_io_write(hid_t dxpl, H5FD_t *lf, H5FD_mem_t type, uint32_t count,
                                      hid_t mem_spaces[], hid_t file_spaces[], haddr_t offsets[],
                                      size_t element_sizes[], int *wb[]);

/* Test functions for selection I/O */
static void test_selection_io(int mpi_rank, int mpi_size);
static void test_selection_io_real(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl);
static void test_selection_io_types_1d(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl, H5FD_mem_t type,
                                       haddr_t addrs[], size_t element_sizes[], hid_t mem_spaces[],
                                       hid_t file_spaces[], hsize_t dims1[]);
static void test_selection_io_types_2d(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl, H5FD_mem_t type,
                                       haddr_t addrs[], size_t element_sizes[], hid_t mem_spaces[],
                                       hid_t file_spaces[], hsize_t dims2[]);
static void test_selection_io_types_1d_2d(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl, H5FD_mem_t type,
                                          haddr_t addrs[], size_t element_sizes[], hid_t mem_spaces[],
                                          hid_t file_spaces[], hsize_t dims1[], hsize_t dims2[]);
static void test_selection_io_types_shorten(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl,
                                            H5FD_mem_t type, haddr_t addrs[], size_t element_sizes[],
                                            hid_t mem_spaces[], hid_t file_spaces[], hsize_t dims1[],
                                            hsize_t dims2[]);

/****************************************************************************/

/****************************************************************************/
/***************************** Utility Functions ****************************/
/****************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    alloc_and_init_file_images
 *
 * Purpose:     Allocate and initialize the global buffers used to construct,
 *              load and verify test file contents.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

static unsigned
alloc_and_init_file_images(int mpi_size)
{
    const char *fcn_name = "alloc_and_init_file_images()";
    int         cp       = 0;
    int         buf_len;
    size_t      buf_size;
    int         i;
    bool        show_progress = false;

    pass = true;

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* allocate the file image buffers */
    if (pass) {

        buf_len  = INTS_PER_RANK * mpi_size;
        buf_size = sizeof(int32_t) * (size_t)INTS_PER_RANK * (size_t)mpi_size;

        increasing_fi_buf = (int32_t *)malloc(buf_size);
        decreasing_fi_buf = (int32_t *)malloc(buf_size);
        negative_fi_buf   = (int32_t *)malloc(buf_size);
        zero_fi_buf       = (int32_t *)malloc(buf_size);
        read_fi_buf       = (int32_t *)malloc(buf_size);

        if ((!increasing_fi_buf) || (!decreasing_fi_buf) || (!negative_fi_buf) || (!zero_fi_buf) ||
            (!read_fi_buf)) {

            pass         = false;
            failure_mssg = "Can't allocate one or more file image buffers.";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* initialize the file image buffers */
    if (pass) {

        for (i = 0; i < buf_len; i++) {

            increasing_fi_buf[i] = i;
            decreasing_fi_buf[i] = buf_len - i;
            negative_fi_buf[i]   = -i;
            zero_fi_buf[i]       = 0;
            read_fi_buf[i]       = 0;
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* discard file image buffers if there was an error */
    if (!pass) {

        free_file_images();
    }

    return !pass;

} /* alloc_and_init_file_images() */

/*-------------------------------------------------------------------------
 * Function:    free_file_images
 *
 * Purpose:     Deallocate any glogal file image buffers that exist, and
 *              set their associated pointers to NULL.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

static void
free_file_images(void)
{
    if (increasing_fi_buf) {

        free(increasing_fi_buf);
        increasing_fi_buf = NULL;
    }

    if (decreasing_fi_buf) {

        free(decreasing_fi_buf);
        decreasing_fi_buf = NULL;
    }

    if (negative_fi_buf) {

        free(negative_fi_buf);
        negative_fi_buf = NULL;
    }

    if (zero_fi_buf) {

        free(zero_fi_buf);
        zero_fi_buf = NULL;
    }

    if (read_fi_buf) {

        free(read_fi_buf);
        read_fi_buf = NULL;
    }

    return;

} /* free_file_images() */

/*-------------------------------------------------------------------------
 * Function:    setup_vfd_test_file
 *
 * Purpose:     Create / open the specified test file with the specified
 *              VFD, and set the EOA to the specified value.
 *
 *              Setup the dxpl for subsequent I/O via the target VFD.
 *
 *              Return a pointer to the instance of H5FD_t created on
 *              file open in *lf_ptr, and the FAPL and DXPL ids in
 *              *fapl_id_ptr and *dxpl_id_ptr.  Similarly, copy the
 *              "fixed" file name into file_name on exit.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

static void
setup_vfd_test_file(int file_name_id, char *file_name, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name, haddr_t eoa,
                    H5FD_t **lf_ptr, hid_t *fapl_id_ptr, hid_t *dxpl_id_ptr)
{
    const char *fcn_name = "setup_vfd_test_file()";
    char        filename[512];
    int         cp            = 0;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    unsigned    flags         = 0;               /* file open flags              */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */

    assert(vfd_name);
    assert(lf_ptr);
    assert(fapl_id_ptr);
    assert(dxpl_id_ptr);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the file name -- do this now, since setting up the ioc faple requires it.  This will probably
     * change */
    if (pass) {

        if (h5_fixname(FILENAMES[file_name_id], H5P_DEFAULT, filename, sizeof(filename)) == NULL) {

            pass         = false;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setupf fapl for target VFD */
    if (pass) {

        if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0) {

            pass         = false;
            failure_mssg = "Can't create fapl.";
        }
    }

    if (pass) {

        if (strcmp(vfd_name, "mpio") == 0) {

            if (H5Pset_fapl_mpio(fapl_id, comm, info) < 0) {

                pass         = false;
                failure_mssg = "Can't set mpio fapl.";
            }
        }
#ifdef H5_HAVE_SUBFILING_VFD
        else if (strcmp(vfd_name, H5FD_SUBFILING_NAME) == 0) {

            H5FD_subfiling_params_t shared_conf = {
                /* ioc_selection = */ SELECT_IOC_ONE_PER_NODE,
                /* stripe_size   = */ (INTS_PER_RANK / 2),
                /* stripe_count  = */ 0, /* will over write */
            };
            H5FD_subfiling_config_t subfiling_conf = {
                /* magic         = */ H5FD_SUBFILING_FAPL_MAGIC,
                /* version       = */ H5FD_SUBFILING_CURR_FAPL_VERSION,
                /* ioc_fapl_id   = */ H5P_DEFAULT, /* will over write? */
                /* require_ioc   = */ true,
                /* shared_cfg    = */ shared_conf,
            };
            H5FD_ioc_config_t ioc_config = {
                /* magic            = */ H5FD_IOC_FAPL_MAGIC,
                /* version          = */ H5FD_IOC_CURR_FAPL_VERSION,
                /* thread_pool_size = */ H5FD_IOC_DEFAULT_THREAD_POOL_SIZE,
            };
            hid_t ioc_fapl = H5I_INVALID_HID;

            if ((pass) && ((ioc_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)) {

                pass         = false;
                failure_mssg = "Can't create ioc fapl.";
            }

            /* set the MPI communicator and info in the FAPL */
            if (H5Pset_mpi_params(ioc_fapl, comm, info) < 0) {

                pass         = false;
                failure_mssg = "Can't set MPI communicator and info in IOC fapl.";
            }

            /* set the MPI communicator and info in the FAPL */
            if (H5Pset_mpi_params(fapl_id, comm, info) < 0) {

                pass         = false;
                failure_mssg = "Can't set MPI communicator and info in subfiling fapl.";
            }

            memset(&ioc_config, 0, sizeof(ioc_config));
            memset(&subfiling_conf, 0, sizeof(subfiling_conf));

            /* Get subfiling VFD defaults */
            if ((pass) && (H5Pget_fapl_subfiling(fapl_id, &subfiling_conf) == FAIL)) {

                pass         = false;
                failure_mssg = "Can't get sub-filing VFD defaults.";
            }

            if ((pass) && (subfiling_conf.require_ioc)) {

                /* Get IOC VFD defaults */
                if ((pass) && ((H5Pget_fapl_ioc(ioc_fapl, &ioc_config) == FAIL))) {

                    pass         = false;
                    failure_mssg = "Can't get IOC VFD defaults.";
                }

                /* Now we can set the IOC fapl. */
                if ((pass) && ((H5Pset_fapl_ioc(ioc_fapl, &ioc_config) == FAIL))) {

                    pass         = false;
                    failure_mssg = "Can't set IOC fapl.";
                }
            }
            else {

                if ((pass) && ((H5Pset_fapl_sec2(ioc_fapl) == FAIL))) {

                    pass         = false;
                    failure_mssg = "Can't set sec2 fapl.";
                }
            }

            /* Assign the IOC fapl as the underlying VPD */
            subfiling_conf.ioc_fapl_id = ioc_fapl;

            /* Now we can set the SUBFILING fapl before returning. */
            if ((pass) && (H5Pset_fapl_subfiling(fapl_id, &subfiling_conf) == FAIL)) {

                pass         = false;
                failure_mssg = "Can't set subfiling fapl.";
            }
        }
#endif
        else {
            pass         = false;
            failure_mssg = "un-supported VFD";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the file name */
    if (pass) {

        if (h5_fixname(FILENAMES[file_name_id], H5P_DEFAULT, filename, sizeof(filename)) == NULL) {

            pass         = false;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Open the VFD test file with the specified VFD.  */

    if (pass) {

        flags = H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC;

        if (NULL == (lf = H5FDopen(filename, flags, fapl_id, HADDR_UNDEF))) {

            pass         = false;
            failure_mssg = "H5FDopen() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* set eoa as specified */

    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        if (H5FDset_eoa(lf, H5FD_MEM_DEFAULT, eoa) < 0) {

            pass         = false;
            failure_mssg = "H5FDset_eoa() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if (pass) { /* setup dxpl */

        dxpl_id = H5Pcreate(H5P_DATASET_XFER);

        if (dxpl_id < 0) {

            pass         = false;
            failure_mssg = "H5Pcreate(H5P_DATASET_XFER) failed.";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if (pass) {

        if (H5Pset_dxpl_mpio(dxpl_id, xfer_mode) < 0) {

            pass         = false;
            failure_mssg = "H5Pset_dxpl_mpio() failed.";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if (pass) {

        if (H5Pset_dxpl_mpio_collective_opt(dxpl_id, coll_opt_mode) < 0) {

            pass         = false;
            failure_mssg = "H5Pset_dxpl_mpio() failed.";
        }
    }

    if (pass) { /* setup pointers with return values */

        strncpy(file_name, filename, 512);
        *lf_ptr      = lf;
        *fapl_id_ptr = fapl_id;
        *dxpl_id_ptr = dxpl_id;
    }
    else { /* tidy up from failure as possible  */

        if (lf)
            H5FDclose(lf);

        if (fapl_id != -1)
            H5Pclose(fapl_id);

        if (dxpl_id != -1)
            H5Pclose(dxpl_id);
    }

    return;

} /* setup_vfd_test_file() */

/*-------------------------------------------------------------------------
 * Function:    takedown_vfd_test_file
 *
 * Purpose:     Close and delete the specified test file.  Close the
 *              FAPL & DXPL.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

static void
takedown_vfd_test_file(int mpi_rank, char *filename, H5FD_t **lf_ptr, hid_t *fapl_id_ptr, hid_t *dxpl_id_ptr)
{
    const char *fcn_name      = "takedown_vfd_test_file()";
    int         cp            = 0;
    bool        show_progress = false;

    assert(lf_ptr);
    assert(fapl_id_ptr);
    assert(dxpl_id_ptr);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Close the test file if it is open, regardless of the value of pass.
     * This should let the test program shut down more cleanly.
     */

    if (*lf_ptr) {

        if (H5FDclose(*lf_ptr) < 0) {

            pass         = false;
            failure_mssg = "H5FDclose() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On rank 0, delete the test file.
     */

    /* wait for everyone to close the file */
    MPI_Barrier(comm);

    if (pass) {

        if ((mpi_rank == 0) && (HDremove(filename) < 0)) {

            pass         = false;
            failure_mssg = "HDremove() failed.\n";
        }
    }

    /* wait for the file delete to complete */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Close the fapl */
    if (H5Pclose(*fapl_id_ptr) < 0) {

        pass         = false;
        failure_mssg = "can't close fapl.\n";
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Close the dxpl */
    if (H5Pclose(*dxpl_id_ptr) < 0) {

        pass         = false;
        failure_mssg = "can't close dxpl.\n";
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    return;

} /* takedown_vfd_test_file() */

/****************************************************************************/
/******************************* Test Functions *****************************/
/****************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    vector_read_test_1()
 *
 * Purpose:     Simple vector read test:
 *
 *              1) Open the test file with the specified VFD, set the eoa,
 *                 and setup the DXPL.
 *
 *              2) Using rank zero, write the entire increasing_fi_buf to
 *                 the file.
 *
 *              3) Barrier
 *
 *              4) On each rank, zero the read buffer, and then read
 *                 INTS_PER_RANK * sizeof(int32) bytes from the file
 *                 starting at offset mpi_rank * INTS_PER_RANK *
 *                 sizeof(int32_t) in both the file and read_fi_buf.
 *                 Do this with a vector read containing a single
 *                 element.
 *
 *                 Verify that read_fi_buf contains zeros for all
 *                 indices less than mpi_rank * INTS_PER_RANK, or
 *                 greater than or equal to (mpi_rank + 1) * INTS_PER_RANK.
 *                 For all other indices, read_fi_buf should equal
 *                 increasing_fi_buf.
 *
 *              5) Barrier
 *
 *              6) Close the test file.
 *
 *              7) On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_read_test_1(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_read_test_1()";
    char        test_title[120];
    char        filename[512];
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    uint32_t    count;
    H5FD_mem_t  types[1];
    haddr_t     addrs[1];
    size_t      sizes[1];
    void       *bufs[1];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 1 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 1 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector read test 1 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire increasing_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)increasing_fi_buf) <
                0) {

                pass         = false;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, zero the read buffer, and then read
     *    INTS_PER_RANK * sizeof(int32) bytes from the file
     *    starting at offset mpi_rank * INTS_PER_RANK *
     *    sizeof(int32_t) in both the file and read_fi_buf.
     *    Do this with a vector read containing a single
     *    element.
     *
     *    Verify that read_fi_buf contains zeros for all
     *    indices less than mpi_rank * INTS_PER_RANK, or
     *    greater than or equal to (mpi_rank + 1) * INTS_PER_RANK.
     *    For all other indices, read_fi_buf should equal
     *    increasing_fi_buf.
     */
    if (pass) {

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            read_fi_buf[i] = 0;
        }

        count    = 1;
        types[0] = H5FD_MEM_DRAW;
        addrs[0] = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
        sizes[0] = (size_t)INTS_PER_RANK * sizeof(int32_t);
        bufs[0]  = (void *)(&(read_fi_buf[mpi_rank * INTS_PER_RANK]));

        if (H5FDread_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDread_vector() failed.\n";
        }

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            if ((i < mpi_rank * INTS_PER_RANK) || (i >= (mpi_rank + 1) * INTS_PER_RANK)) {

                if (read_fi_buf[i] != 0) {

                    pass         = false;
                    failure_mssg = "Unexpected value in read_fi_buf (1).\n";
                    break;
                }
            }
            else {

                if (read_fi_buf[i] != increasing_fi_buf[i]) {

                    pass         = false;
                    failure_mssg = "Unexpected value in read_fi_buf (2).\n";
                    break;
                }
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_read_test_1() */

/*-------------------------------------------------------------------------
 * Function:    vector_read_test_2()
 *
 * Purpose:     Simple vector read test with only half of ranks
 *              participating in each vector read.
 *
 *               1) Open the test file with the specified VFD, set the eoa,
 *                  and setup the DXPL.
 *
 *               2) Using rank zero, write the entire decreasing_fi_buf to
 *                  the file.
 *
 *               3) Barrier
 *
 *               4) On each rank, zero the read buffer.
 *
 *               5) On even ranks, read INTS_PER_RANK * sizeof(int32)
 *                  bytes from the file starting at offset mpi_rank *
 *                  INTS_PER_RANK * sizeof(int32_t) in both the file and
 *                  read_fi_buf.  Do this with a vector read containing
 *                  a single element.
 *
 *                  Odd ranks perform an empty read.
 *
 *               6) Barrier.
 *
 *               7) On odd ranks, read INTS_PER_RANK * sizeof(int32)
 *                  bytes from the file starting at offset mpi_rank *
 *                  INTS_PER_RANK * sizeof(int32_t) in both the file and
 *                  read_fi_buf.  Do this with a vector read containing
 *                  a single element.
 *
 *                  Even ranks perform an empty read.
 *
 *               8) Verify that read_fi_buf contains zeros for all
 *                  indices less than mpi_rank * INTS_PER_RANK, or
 *                  greater than or equal to (mpi_rank + 1) * INTS_PER_RANK.
 *                  For all other indices, read_fi_buf should equal
 *                  decreasing_fi_buf.
 *
 *               9) Barrier
 *
 *              10) Close the test file.
 *
 *              11) On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_read_test_2(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_read_test_2()";
    char        test_title[120];
    char        filename[512];
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    uint32_t    count;
    H5FD_mem_t  types[1];
    haddr_t     addrs[1];
    size_t      sizes[1];
    void       *bufs[1];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 2 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 2 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector read test 2 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire decreasing_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)decreasing_fi_buf) <
                0) {

                pass         = false;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, zero the read buffer. */
    if (pass) {

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            read_fi_buf[i] = 0;
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) On even ranks, read INTS_PER_RANK * sizeof(int32)
     *    bytes from the file starting at offset mpi_rank *
     *    INTS_PER_RANK * sizeof(int32_t) in both the file and
     *    read_fi_buf.  Do this with a vector read containing
     *    a single element.
     *
     *    Odd ranks perform an empty read.
     */
    if (pass) {

        if (mpi_rank % 2 == 0) {

            count    = 1;
            types[0] = H5FD_MEM_DRAW;
            addrs[0] = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
            sizes[0] = (size_t)INTS_PER_RANK * sizeof(int32_t);
            bufs[0]  = (void *)(&(read_fi_buf[mpi_rank * INTS_PER_RANK]));
        }
        else {

            count = 0;
        }

        if (H5FDread_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDread_vector() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) On odd ranks, read INTS_PER_RANK * sizeof(int32)
     *    bytes from the file starting at offset mpi_rank *
     *    INTS_PER_RANK * sizeof(int32_t) in both the file and
     *    read_fi_buf.  Do this with a vector read containing
     *    a single element.
     *
     *    Even ranks perform an empty read.
     */
    if (pass) {

        if (mpi_rank % 2 == 1) {

            count    = 1;
            types[0] = H5FD_MEM_DRAW;
            addrs[0] = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
            sizes[0] = (size_t)INTS_PER_RANK * sizeof(int32_t);
            bufs[0]  = (void *)(&(read_fi_buf[mpi_rank * INTS_PER_RANK]));
        }
        else {

            count = 0;
        }

        if (H5FDread_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDread_vector() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Verify that read_fi_buf contains zeros for all
     *    indices less than mpi_rank * INTS_PER_RANK, or
     *    greater than or equal to (mpi_rank + 1) * INTS_PER_RANK.
     *    For all other indices, read_fi_buf should equal
     *    decreasing_fi_buf.
     */

    if (pass) {

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            if ((i < mpi_rank * INTS_PER_RANK) || (i >= (mpi_rank + 1) * INTS_PER_RANK)) {

                if (read_fi_buf[i] != 0) {

                    pass         = false;
                    failure_mssg = "Unexpected value in read_fi_buf (1).\n";
                    break;
                }
            }
            else {

                if (read_fi_buf[i] != decreasing_fi_buf[i]) {

                    pass         = false;
                    failure_mssg = "Unexpected value in read_fi_buf (2).\n";
                    break;
                }
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 9) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 10) Close the test file and delete it (on rank 0 only).
     *     Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);
    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_read_test_2() */

/*-------------------------------------------------------------------------
 * Function:    vector_read_test_3()
 *
 * Purpose:     Verify that vector read works with multiple entries in
 *              the vector in each read, and that read buffers need not
 *              be in increasing (memory) address order.
 *
 *               1) Open the test file with the specified VFD, set the eoa,
 *                  and setup the DXPL.
 *
 *               2) Using rank zero, write the entire negative_fi_buf to
 *                  the file.
 *
 *               3) Barrier
 *
 *               4) On each rank, zero the four read buffers.
 *
 *               5) On each rank, do a vector read from the file, with
 *                  each rank's vector having four elements, with each
 *                  element reading INTS_PER_RANK / 4 * sizeof(int32)
 *                  bytes, and the reads starting at address:
 *
 *                      (mpi_rank * INTS_PER_RANK) * sizeof(int32_t)
 *
 *                      (mpi_rank * INTS_PER_RANK + INTS_PER_RANK / 4) *
 *                          sizeof(int32_t)
 *
 *                      (mpi_rank * INTS_PER_RANK + INTS_PER_RANK / 2) *
 *                          sizeof(int32_t)
 *
 *                      (mpi_rank * INTS_PER_RANK + 3 * INTS_PER_RANK / 2) *
 *                          sizeof(int32_t)
 *
 *                  On even ranks, the targets of the reads should be
 *                  buf_0, buf_1, buf_2, and buf_3 respectively.
 *
 *                  On odd ranks, the targets of the reads should be
 *                  buf_3, buf_2, buf_1, and buf_0 respectively.
 *
 *                  This has the effect of ensuring that on at least
 *                  some ranks, the read buffers are not in increasing
 *                  address order.
 *
 *               6) Verify that buf_0, buf_1, buf_2, and buf_3 contain
 *                  the expected data.  Note that this will be different
 *                  on even vs. odd ranks.
 *
 *               7) Barrier.
 *
 *               8) Close the test file.
 *
 *               9) On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_read_test_3(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_read_test_3()";
    char        test_title[120];
    char        filename[512];
    int32_t     buf_0[(INTS_PER_RANK / 4) + 1];
    int32_t     buf_1[(INTS_PER_RANK / 4) + 1];
    int32_t     buf_2[(INTS_PER_RANK / 4) + 1];
    int32_t     buf_3[(INTS_PER_RANK / 4) + 1];
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    uint32_t    count;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    void       *bufs[4];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 3 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 3 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector read test 3 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)negative_fi_buf) <
                0) {

                pass         = false;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, zero the four read buffers. */
    if (pass) {

        for (i = 0; i <= INTS_PER_RANK / 4; i++) {

            buf_0[i] = 0;
            buf_1[i] = 0;
            buf_2[i] = 0;
            buf_3[i] = 0;
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) On each rank, do a vector read from the file, with
     *    each rank's vector having four elements, with each
     *    element reading INTS_PER_RANK / 4 * sizeof(int32)
     *    bytes, and the reads starting at address:
     *
     *        (mpi_rank * INTS_PER_RANK) * sizeof(int32_t)
     *
     *        (mpi_rank * INTS_PER_RANK + INTS_PER_RANK / 4) *
     *            sizeof(int32_t)
     *
     *        (mpi_rank * INTS_PER_RANK + INTS_PER_RANK / 2) *
     *            sizeof(int32_t)
     *
     *        (mpi_rank * INTS_PER_RANK + 3 * INTS_PER_RANK / 2) *
     *            sizeof(int32_t)
     *
     *    On even ranks, the targets of the reads should be
     *    buf_0, buf_1, buf_2, and buf_3 respectively.
     *
     *    On odd ranks, the targets of the reads should be
     *    buf_3, buf_2, buf_1, and buf_0 respectively.
     *
     *    This has the effect of ensuring that on at least
     *    some ranks, the read buffers are not in increasing
     *    address order.
     */
    if (pass) {

        haddr_t base_addr = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        count = 4;

        types[0] = H5FD_MEM_DRAW;
        addrs[0] = base_addr;
        sizes[0] = (size_t)(INTS_PER_RANK / 4) * sizeof(int32_t);

        types[1] = H5FD_MEM_DRAW;
        addrs[1] = base_addr + ((haddr_t)(INTS_PER_RANK / 4) * (haddr_t)(sizeof(int32_t)));
        sizes[1] = (size_t)(INTS_PER_RANK / 4) * sizeof(int32_t);

        types[2] = H5FD_MEM_DRAW;
        addrs[2] = base_addr + ((haddr_t)(INTS_PER_RANK / 2) * (haddr_t)(sizeof(int32_t)));
        sizes[2] = (size_t)(INTS_PER_RANK / 4) * sizeof(int32_t);

        types[3] = H5FD_MEM_DRAW;
        addrs[3] = base_addr + ((haddr_t)(3 * INTS_PER_RANK / 4) * (haddr_t)(sizeof(int32_t)));
        sizes[3] = (size_t)INTS_PER_RANK / 4 * sizeof(int32_t);

        if (mpi_rank % 2 == 0) {

            bufs[0] = (void *)(&(buf_0[0]));
            bufs[1] = (void *)(buf_1);
            bufs[2] = (void *)(buf_2);
            bufs[3] = (void *)(buf_3);
        }
        else {

            bufs[0] = (void *)(&(buf_3[0]));
            bufs[1] = (void *)(buf_2);
            bufs[2] = (void *)(buf_1);
            bufs[3] = (void *)(buf_0);
        }

        if (H5FDread_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDread_vector() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) Verify that buf_0, buf_1, buf_2, and buf_3 contain
     *    the expected data.  Note that this will be different
     *    on even vs. odd ranks.
     */
    if (pass) {

        int base_index = mpi_rank * INTS_PER_RANK;

        for (i = 0; ((pass) && (i < INTS_PER_RANK / 4)); i++) {

            if (((mpi_rank % 2 == 0) && (buf_0[i] != negative_fi_buf[base_index + i])) ||
                ((mpi_rank % 2 == 1) && (buf_3[i] != negative_fi_buf[base_index + i]))) {

                pass         = false;
                failure_mssg = "Unexpected value in buf (1).\n";
            }
        }

        base_index += INTS_PER_RANK / 4;

        for (i = 0; ((pass) && (i < INTS_PER_RANK / 4)); i++) {

            if (((mpi_rank % 2 == 0) && (buf_1[i] != negative_fi_buf[base_index + i])) ||
                ((mpi_rank % 2 == 1) && (buf_2[i] != negative_fi_buf[base_index + i]))) {

                pass         = false;
                failure_mssg = "Unexpected value in buf (2).\n";
            }
        }

        base_index += INTS_PER_RANK / 4;

        for (i = 0; ((pass) && (i < INTS_PER_RANK / 4)); i++) {

            if (((mpi_rank % 2 == 0) && (buf_2[i] != negative_fi_buf[base_index + i])) ||
                ((mpi_rank % 2 == 1) && (buf_1[i] != negative_fi_buf[base_index + i]))) {

                pass         = false;
                failure_mssg = "Unexpected value in buf (3).\n";
            }
        }

        base_index += INTS_PER_RANK / 4;

        for (i = 0; ((pass) && (i < INTS_PER_RANK / 4)); i++) {

            if (((mpi_rank % 2 == 0) && (buf_3[i] != negative_fi_buf[base_index + i])) ||
                ((mpi_rank % 2 == 1) && (buf_0[i] != negative_fi_buf[base_index + i]))) {

                pass         = false;
                failure_mssg = "Unexpected value in buf (4).\n";
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_read_test_3() */

/*-------------------------------------------------------------------------
 * Function:    vector_read_test_4()
 *
 * Purpose:     Test vector I/O reads with vectors of different lengths
 *              and entry sizes across the ranks.  Vectors are not, in
 *              general, sorted in increasing address order.  Further,
 *              reads are not, in general, contiguous.
 *
 *               1) Open the test file with the specified VFD, set the eoa.
 *                  and setup the DXPL.
 *
 *               2) Using rank zero, write the entire increasing_fi_buf to
 *                  the file.
 *
 *               3) Barrier
 *
 *               4) Set all cells of read_fi_buf to zero.
 *
 *               5) For each rank, define base_index equal to:
 *
 *                      mpi_rank * INTS_PER_RANK
 *
 *                  and define base_addr equal to
 *
 *                      base_index * sizeof(int32_t).
 *
 *                  Setup a vector read between base_addr and
 *                  base_addr + INTS_PER_RANK * sizeof(int32_t) - 1
 *                  as follows:
 *
 *                  if ( rank % 4 == 0 ) construct a vector that reads:
 *
 *                       INTS_PER_RANK / 4 * sizeof(int32_t) bytes
 *                       starting at base_addr + INTS_PER_RANK / 2 *
 *                       sizeof(int32_t),
 *
 *                       INTS_PER_RANK / 8 * sizeof(int32_t) bytes
 *                       starting at base_addr + INTS_PER_RANK / 4 *
 *                       sizeof(int32_t), and
 *
 *                       INTS_PER_RANK / 16 * sizeof(int32_t) butes
 *                       starting at base_addr + INTS_PER_RANK / 16 *
 *                       sizeof(int32_t)
 *
 *                   to the equivalent locations in read_fi_buf
 *
 *                  if ( rank % 4 == 1 ) construct a vector that reads:
 *
 *                       ((INTS_PER_RANK / 2) - 2) * sizeof(int32_t)
 *                       bytes starting at base_addr + sizeof(int32_t), and
 *
 *                       ((INTS_PER_RANK / 2) - 2) * sizeof(int32_t) bytes
 *                       starting at base_addr + (INTS_PER_RANK / 2 + 1) *
 *                       sizeof(int32_t).
 *
 *                   to the equivalent locations in read_fi_buf
 *
 *                  if ( rank % 4 == 2 ) construct a vector that reads:
 *
 *                       sizeof(int32_t) bytes starting at base_index +
 *                       (INTS_PER_RANK / 2) * sizeof int32_t.
 *
 *                   to the equivalent locations in read_fi_buf
 *
 *                 if ( rank % 4 == 3 ) construct and read the empty vector
 *
 *               6) On each rank, verify that read_fi_buf contains the
 *                  the expected values -- that is the matching values from
 *                  increasing_fi_buf where ever there was a read, and zero
 *                  otherwise.
 *
 *               7) Barrier.
 *
 *               8) Close the test file.
 *
 *               9) On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_read_test_4(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_read_test_4()";
    char        test_title[120];
    char        filename[512];
    haddr_t     eoa;
    haddr_t     base_addr;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    int         k;
    int         base_index;
    uint32_t    count = 0;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    void       *bufs[4];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 4 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 4 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector read test 4 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)increasing_fi_buf) <
                0) {

                pass         = false;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) Set all cells of read_fi_buf to zero. */
    if (pass) {

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            read_fi_buf[i] = 0;
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) For each rank, define base_index equal to:
     *
     *        mpi_rank * INTS_PER_RANK
     *
     *    and define base_addr equal to
     *
     *        base_index * sizeof(int32_t).
     *
     *    Setup a vector read between base_addr and
     *    base_addr + INTS_PER_RANK * sizeof(int32_t) - 1
     *    as follows:
     */
    if (pass) {

        base_index = mpi_rank * INTS_PER_RANK;
        base_addr  = (haddr_t)base_index * (haddr_t)sizeof(int32_t);

        if ((mpi_rank % 4) == 0) {

            /* if ( rank % 4 == 0 ) construct a vector that reads:
             *
             *      INTS_PER_RANK / 4 * sizeof(int32_t) bytes
             *      starting at base_addr + INTS_PER_RANK / 2 *
             *      sizeof(int32_t),
             *
             *      INTS_PER_RANK / 8 * sizeof(int32_t) bytes
             *      starting at base_addr + INTS_PER_RANK / 4 *
             *      sizeof(int32_t), and
             *
             *      INTS_PER_RANK / 16 * sizeof(int32_t) butes
             *      starting at base_addr + INTS_PER_RANK / 16 *
             *      sizeof(int32_t)
             *
             *  to the equivalent locations in read_fi_buf
             */

            count = 3;

            types[0] = H5FD_MEM_DRAW;
            addrs[0] = base_addr + (haddr_t)((size_t)(INTS_PER_RANK / 2) * sizeof(int32_t));
            sizes[0] = (size_t)(INTS_PER_RANK / 4) * sizeof(int32_t);
            bufs[0]  = (void *)(&(read_fi_buf[base_index + (INTS_PER_RANK / 2)]));

            types[1] = H5FD_MEM_DRAW;
            addrs[1] = base_addr + (haddr_t)((size_t)(INTS_PER_RANK / 4) * sizeof(int32_t));
            sizes[1] = (size_t)(INTS_PER_RANK / 8) * sizeof(int32_t);
            bufs[1]  = (void *)(&(read_fi_buf[base_index + (INTS_PER_RANK / 4)]));

            types[2] = H5FD_MEM_DRAW;
            addrs[2] = base_addr + (haddr_t)((size_t)(INTS_PER_RANK / 16) * sizeof(int32_t));
            sizes[2] = (size_t)(INTS_PER_RANK / 16) * sizeof(int32_t);
            bufs[2]  = (void *)(&(read_fi_buf[base_index + (INTS_PER_RANK / 16)]));
        }
        else if ((mpi_rank % 4) == 1) {

            /* if ( rank % 4 == 1 ) construct a vector that reads:
             *
             *      ((INTS_PER_RANK / 2) - 2) * sizeof(int32_t)
             *      bytes starting at base_addr + sizeof(int32_t), and
             *
             *      ((INTS_PER_RANK / 2) - 2) * sizeof(int32_t) bytes
             *      starting at base_addr + (INTS_PER_RANK / 2 + 1) *
             *      sizeof(int32_t).
             *
             *  to the equivalent locations in read_fi_buf
             */
            count = 2;

            types[0] = H5FD_MEM_DRAW;
            addrs[0] = base_addr + (haddr_t)(sizeof(int32_t));
            sizes[0] = (size_t)((INTS_PER_RANK / 2) - 2) * sizeof(int32_t);
            bufs[0]  = (void *)(&(read_fi_buf[base_index + 1]));

            types[1] = H5FD_MEM_DRAW;
            addrs[1] = base_addr + (haddr_t)((size_t)((INTS_PER_RANK / 2) + 1) * sizeof(int32_t));
            sizes[1] = (size_t)((INTS_PER_RANK / 2) - 2) * sizeof(int32_t);
            bufs[1]  = (void *)(&(read_fi_buf[base_index + (INTS_PER_RANK / 2) + 1]));
        }
        else if ((mpi_rank % 4) == 2) {

            /* if ( rank % 4 == 2 ) construct a vector that reads:
             *
             *      sizeof(int32_t) bytes starting at base_index +
             *      (INTS_PER_RANK / 2) * sizeof int32_t.
             *
             *  to the equivalent locations in read_fi_buf
             */
            count = 1;

            types[0] = H5FD_MEM_DRAW;
            addrs[0] = base_addr + (haddr_t)((size_t)(INTS_PER_RANK / 2) * sizeof(int32_t));
            sizes[0] = sizeof(int32_t);
            bufs[0]  = (void *)(&(read_fi_buf[base_index + (INTS_PER_RANK / 2)]));
        }
        else if ((mpi_rank % 4) == 3) {

            /* if ( rank % 4 == 3 ) construct and read the empty vector */

            count = 0;
        }

        if (H5FDread_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDread_vector() failed (1).\n";
        }
    }

    /* 6) On each rank, verify that read_fi_buf contains the
     *    the expected values -- that is the matching values from
     *    increasing_fi_buf where ever there was a read, and zero
     *    otherwise.
     */
    if (pass) {

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;
#if 1
            for (j = base_index; j < base_index + INTS_PER_RANK; j++) {

                k = j - base_index;
#else
            for (k = 0; k < INTS_PER_RANK; k++) {

                j = k + base_index;
#endif

                if (i == mpi_rank) {

                    switch (i % 4) {

                        case 0:
                            if (((INTS_PER_RANK / 2) <= k) && (k < (3 * (INTS_PER_RANK / 4)))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (1.1)";
                                    fprintf(stdout, "\nread_fi_buf[%d] = %d, increasing_fi_buf[%d] = %d\n", j,
                                            read_fi_buf[j], j, increasing_fi_buf[j]);
                                }
                            }
                            else if (((INTS_PER_RANK / 4) <= k) && (k < (3 * (INTS_PER_RANK / 8)))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (1.2)";
                                }
                            }
                            else if (((INTS_PER_RANK / 16) <= k) && (k < (INTS_PER_RANK / 8))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (1.3)";
                                }
                            }
                            else {

                                if (read_fi_buf[j] != 0) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (1.4)";
                                }
                            }
                            break;

                        case 1:
                            if ((1 <= k) && (k <= ((INTS_PER_RANK / 2) - 2))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (2.1)";
                                }
                            }
                            else if ((((INTS_PER_RANK / 2) + 1) <= k) && (k <= (INTS_PER_RANK - 2))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (2.2)";
                                }
                            }
                            else {

                                if (read_fi_buf[j] != 0) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (2.3)";
                                }
                            }
                            break;

                        case 2:
                            if (k == INTS_PER_RANK / 2) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (3.1)";
                                }
                            }
                            else {

                                if (read_fi_buf[j] != 0) {

                                    pass         = false;
                                    failure_mssg = "unexpected data read from file (3.2)";
                                }
                            }
                            break;

                        case 3:
                            if (read_fi_buf[j] != 0) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (4)";
                            }
                            break;

                        default:
                            assert(false); /* should be un-reachable */
                            break;
                    }
                }
                else if (read_fi_buf[j] != 0) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (5)";
                }
            } /* end for loop */
        }     /* end for loop */
    }         /* end if */

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_read_test_4() */

/*-------------------------------------------------------------------------
 * Function:    vector_read_test_5()
 *
 * Purpose:     Test correct management of the sizes[] array optimization,
 *              where, if sizes[i] == 0, we use sizes[i - 1] as the value
 *              of size[j], for j >= i.
 *
 *               1) Open the test file with the specified VFD, set the eoa.
 *                  and setup the DXPL.
 *
 *               2) Using rank zero, write the entire increasing_fi_buf to
 *                  the file.
 *
 *               3) Barrier
 *
 *               4) Set all cells of read_fi_buf to zero.
 *
 *               5) For each rank, define base_index equal to:
 *
 *                      mpi_rank * INTS_PER_RANK
 *
 *                  and define base_addr equal to
 *
 *                      base_index * sizeof(int32_t).
 *
 *                  Setup a vector read between base_addr and
 *                  base_addr + INTS_PER_RANK * sizeof(int32_t) - 1
 *                  that reads every 16th integer located in that
 *                  that range starting at base_addr.  Use a sizes[]
 *                  array of length 2, with sizes[0] set to sizeof(int32_t),
 *                  and sizes[1] = 0.
 *
 *                  Read the integers into the corresponding locations in
 *                  read_fi_buf.
 *
 *               6) On each rank, verify that read_fi_buf contains the
 *                  the expected values -- that is the matching values from
 *                  increasing_fi_buf where ever there was a read, and zero
 *                  otherwise.
 *
 *               7) Barrier.
 *
 *               8) Close the test file.
 *
 *               9) On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_read_test_5(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                   H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_read_test_5()";
    char        test_title[120];
    char        filename[512];
    haddr_t     eoa;
    haddr_t     base_addr;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    int         base_index;
    uint32_t    count = 0;
    H5FD_mem_t  types[(INTS_PER_RANK / 16) + 1];
    haddr_t     addrs[(INTS_PER_RANK / 16) + 1];
    size_t      sizes[2];
    void       *bufs[(INTS_PER_RANK / 16) + 1];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 5 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector read test 5 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector read test 5 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)increasing_fi_buf) <
                0) {

                pass         = false;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) Set all cells of read_fi_buf to zero. */
    if (pass) {

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            read_fi_buf[i] = 0;
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) For each rank, define base_index equal to:
     *
     *        mpi_rank * INTS_PER_RANK
     *
     *    and define base_addr equal to
     *
     *        base_index * sizeof(int32_t).
     *
     *    Setup a vector read between base_addr and
     *    base_addr + INTS_PER_RANK * sizeof(int32_t) - 1
     *    that reads every 16th integer located in that
     *    that range starting at base_addr.  Use a sizes[]
     *    array of length 2, with sizes[0] set to sizeof(int32_t),
     *    and sizes[1] = 0.
     *
     *    Read the integers into the corresponding locations in
     *    read_fi_buf.
     */
    if (pass) {

        base_index = (mpi_rank * INTS_PER_RANK);
        base_addr  = (haddr_t)base_index * (haddr_t)sizeof(int32_t);

        count    = INTS_PER_RANK / 16;
        sizes[0] = sizeof(int32_t);
        sizes[1] = 0;

        for (i = 0; i < INTS_PER_RANK / 16; i++) {

            types[i] = H5FD_MEM_DRAW;
            addrs[i] = base_addr + ((haddr_t)(16 * i) * (haddr_t)sizeof(int32_t));
            bufs[i]  = (void *)(&(read_fi_buf[base_index + (i * 16)]));
        }

        if (H5FDread_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDread_vector() failed (1).\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On each rank, verify that read_fi_buf contains the
     *    the expected values -- that is the matching values from
     *    increasing_fi_buf where ever there was a read, and zero
     *    otherwise.
     */
    if (pass) {

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;

            for (j = base_index; j < base_index + INTS_PER_RANK; j++) {

                if ((i == mpi_rank) && (j % 16 == 0)) {

                    if (read_fi_buf[j] != increasing_fi_buf[j]) {

                        pass         = false;
                        failure_mssg = "unexpected data read from file (1)";
                    }
                }
                else if (read_fi_buf[j] != 0) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (2)";
                }
            } /* end for loop */
        }     /* end for loop */
    }         /* end if */

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();
            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_read_test_5() */

/*-------------------------------------------------------------------------
 * Function:    vector_write_test_1()
 *
 * Purpose:     Simple vector write test:
 *
 *              1) Open the test file with the specified VFD, set the eoa,
 *                 and setup the DXPL.
 *
 *              2) Write the entire increasing_fi_buf to the file, with
 *                 exactly one buffer per vector per rank.  Use either
 *                 independent or collective I/O as specified.
 *
 *              3) Barrier
 *
 *              4) On each rank, read the entire file into the read_fi_buf,
 *                 and compare against increasing_fi_buf.  Report failure
 *                 if any differences are detected.
 *
 *              5) Close the test file.
 *
 *              6) On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_write_test_1(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_write_test_1()";
    char        test_title[120];
    char        filename[512];
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    uint32_t    count;
    H5FD_mem_t  types[1];
    haddr_t     addrs[1];
    size_t      sizes[1];
    const void *bufs[1];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 1 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 1 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector write test 1 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Write the entire increasing_fi_buf to the file, with
     *    exactly one buffer per vector per rank.  Use either
     *    independent or collective I/O as specified.
     */

    if (pass) {

        count    = 1;
        types[0] = H5FD_MEM_DRAW;
        addrs[0] = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
        sizes[0] = (size_t)INTS_PER_RANK * sizeof(int32_t);
        bufs[0]  = (const void *)(&(increasing_fi_buf[mpi_rank * INTS_PER_RANK]));

        if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf.  Report failure
     *    if any differences are detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = false;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            if (read_fi_buf[i] != increasing_fi_buf[i]) {

                pass         = false;
                failure_mssg = "unexpected data read from file";
                break;
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_1() */

/*-------------------------------------------------------------------------
 * Function:    vector_write_test_2()
 *
 * Purpose:     Test vector I/O writes in which only some ranks participate.
 *              Depending on the collective parameter, these writes will
 *              be either collective or independent.
 *
 *              1) Open the test file with the specified VFD, and set
 *                 the eoa.
 *
 *              2) Write the odd blocks of the increasing_fi_buf to the file,
 *                 with the odd ranks writing the odd blocks, and the even
 *                 ranks writing an empty vector.
 *
 *                 Here, a "block" of the increasing_fi_buf is a sequence
 *                 of integers in increasing_fi_buf of length INTS_PER_RANK,
 *                 and with start index a multiple of INTS_PER_RANK.
 *
 *              3) Write the even blocks of the negative_fi_buf to the file,
 *                 with the even ranks writing the even blocks, and the odd
 *                 ranks writing an empty vector.
 *
 *              4) Barrier
 *
 *              4) On each rank, read the entire file into the read_fi_buf,
 *                 and compare against increasing_fi_buf and negative_fi_buf
 *                 as appropriate.  Report failure if any differences are
 *                 detected.
 *
 *              5) Close the test file.  On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_write_test_2(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_write_test_2()";
    char        test_title[120];
    char        filename[512];
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    uint32_t    count;
    H5FD_mem_t  types[1];
    haddr_t     addrs[1];
    size_t      sizes[1];
    const void *bufs[1];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 2 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 2 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector write test 2 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Write the odd blocks of the increasing_fi_buf to the file,
     *    with the odd ranks writing the odd blocks, and the even
     *    ranks writing an empty vector.
     *
     *    Here, a "block" of the increasing_fi_buf is a sequence
     *    of integers in increasing_fi_buf of length INTS_PER_RANK,
     *    and with start index a multiple of INTS_PER_RANK.
     */
    if (pass) {

        if (mpi_rank % 2 == 1) { /* odd ranks */

            count    = 1;
            types[0] = H5FD_MEM_DRAW;
            addrs[0] = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
            sizes[0] = (size_t)INTS_PER_RANK * sizeof(int32_t);
            bufs[0]  = (const void *)(&(increasing_fi_buf[mpi_rank * INTS_PER_RANK]));

            if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

                pass         = false;
                failure_mssg = "H5FDwrite_vector() failed (1).\n";
            }
        }
        else { /* even ranks */

            if (H5FDwrite_vector(lf, dxpl_id, 0, NULL, NULL, NULL, NULL) < 0) {

                pass         = false;
                failure_mssg = "H5FDwrite_vector() failed (2).\n";
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Write the even blocks of the negative_fi_buf to the file,
     *    with the even ranks writing the even blocks, and the odd
     *    ranks writing an empty vector.
     */
    if (pass) {

        if (mpi_rank % 2 == 1) { /* odd ranks */

            if (H5FDwrite_vector(lf, dxpl_id, 0, NULL, NULL, NULL, NULL) < 0) {

                pass         = false;
                failure_mssg = "H5FDwrite_vector() failed (3).\n";
            }
        }
        else { /* even ranks */

            count    = 1;
            types[0] = H5FD_MEM_DRAW;
            addrs[0] = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
            sizes[0] = (size_t)INTS_PER_RANK * sizeof(int32_t);
            bufs[0]  = (const void *)(&(negative_fi_buf[mpi_rank * INTS_PER_RANK]));

            if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

                pass         = false;
                failure_mssg = "H5FDwrite_vector() failed (4).\n";
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf.  Report failure
     *    if any differences are detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = false;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            if (i % 2 == 1) { /* odd block */

                for (j = i * INTS_PER_RANK; ((pass) && (j < (i + 1) * INTS_PER_RANK)); j++) {

                    if (read_fi_buf[j] != increasing_fi_buf[j]) {

                        pass         = false;
                        failure_mssg = "unexpected data read from file";
                        break;
                    }
                }
            }
            else { /* even block */

                for (j = i * INTS_PER_RANK; ((pass) && (j < (i + 1) * INTS_PER_RANK)); j++) {

                    if (read_fi_buf[j] != negative_fi_buf[j]) {

                        pass         = false;
                        failure_mssg = "unexpected data read from file";
                        break;
                    }
                }
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_2() */

/*-------------------------------------------------------------------------
 * Function:    vector_write_test_3()
 *
 * Purpose:     Test vector I/O writes with vectors of multiple entries.
 *              For now, keep the vectors sorted in increasing address
 *              order.
 *
 *              1) Open the test file with the specified VFD, and set
 *                 the eoa.
 *
 *              2) For each rank, construct a vector with base address
 *                 (mpi_rank * INTS_PER_RANK) and writing all bytes from
 *                 that address to ((mpi_rank + 1) * INTS_PER_RANK) - 1.
 *                 Draw equal parts from increasing_fi_buf,
 *                 decreasing_fi_buf, negative_fi_buf, and zero_fi_buf.
 *
 *                 Write to file.
 *
 *              3) Barrier
 *
 *              4) On each rank, read the entire file into the read_fi_buf,
 *                 and compare against increasing_fi_buf,
 *                 decreasing_fi_buf, negative_fi_buf, and zero_fi_buf as
 *                 appropriate.  Report failure if any differences are
 *                 detected.
 *
 *              5) Close the test file.  On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_write_test_3(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_write_test_3()";
    char        test_title[120];
    char        filename[512];
    haddr_t     base_addr;
    int         base_index;
    int         ints_per_write;
    size_t      bytes_per_write;
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    uint32_t    count;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    const void *bufs[4];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 3 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 3 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector write test 3 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) For each rank, construct a vector with base address
     *    (mpi_rank * INTS_PER_RANK) and writing all bytes from
     *    that address to ((mpi_rank + 1) * INTS_PER_RANK) - 1.
     *    Draw equal parts from increasing_fi_buf,
     *    decreasing_fi_buf, negative_fi_buf, and zero_fi_buf.
     *
     *    Write to file.
     */
    if (pass) {

        count = 4;

        base_addr       = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
        ints_per_write  = INTS_PER_RANK / 4;
        bytes_per_write = (size_t)(ints_per_write) * sizeof(int32_t);

        types[0] = H5FD_MEM_DRAW;
        addrs[0] = base_addr;
        sizes[0] = bytes_per_write;
        bufs[0]  = (const void *)(&(increasing_fi_buf[mpi_rank * INTS_PER_RANK]));

        types[1] = H5FD_MEM_DRAW;
        addrs[1] = addrs[0] + (haddr_t)(bytes_per_write);
        sizes[1] = bytes_per_write;
        bufs[1]  = (const void *)(&(decreasing_fi_buf[(mpi_rank * INTS_PER_RANK) + (INTS_PER_RANK / 4)]));

        types[2] = H5FD_MEM_DRAW;
        addrs[2] = addrs[1] + (haddr_t)(bytes_per_write);
        sizes[2] = bytes_per_write;
        bufs[2]  = (const void *)(&(negative_fi_buf[(mpi_rank * INTS_PER_RANK) + (INTS_PER_RANK / 2)]));

        types[3] = H5FD_MEM_DRAW;
        addrs[3] = addrs[2] + (haddr_t)(bytes_per_write);
        sizes[3] = bytes_per_write;
        bufs[3]  = (const void *)(&(zero_fi_buf[(mpi_rank * INTS_PER_RANK) + (3 * (INTS_PER_RANK / 4))]));

        if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf,
     *    decreasing_fi_buf, negative_fi_buf, and zero_fi_buf as
     *    appropriate.  Report failure if any differences are
     *    detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = false;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (1)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != decreasing_fi_buf[j]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (2)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != negative_fi_buf[j]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (3)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != zero_fi_buf[j]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (3)";
                    break;
                }
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_3() */

/*-------------------------------------------------------------------------
 * Function:    vector_write_test_4()
 *
 * Purpose:     Test vector I/O writes with vectors of multiple entries.
 *              For now, keep the vectors sorted in increasing address
 *              order.
 *
 *              This test differs from vector_write_test_3() in the order
 *              in which the file image buffers appear in the vector
 *              write.  This guarantees that at least one of these
 *              tests will present buffers with non-increasing addresses
 *              in RAM.
 *
 *              1) Open the test file with the specified VFD, and set
 *                 the eoa.
 *
 *              2) For each rank, construct a vector with base address
 *                 (mpi_rank * INTS_PER_RANK) and writing all bytes from
 *                 that address to ((mpi_rank + 1) * INTS_PER_RANK) - 1.
 *                 Draw equal parts from zero_fi_buf, negative_fi_buf,
 *                 decreasing_fi_buf, and increasing_fi_buf.
 *
 *                 Write to file.
 *
 *              3) Barrier
 *
 *              4) On each rank, read the entire file into the read_fi_buf,
 *                 and compare against zero_fi_buf, negative_fi_buf,
 *                 decreasing_fi_buf, and  increasing_fi_buf as
 *                 appropriate.  Report failure if any differences are
 *                 detected.
 *
 *              5) Close the test file.  On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_write_test_4(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_write_test_4()";
    char        test_title[120];
    char        filename[512];
    haddr_t     base_addr;
    int         base_index;
    int         ints_per_write;
    size_t      bytes_per_write;
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    uint32_t    count;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    const void *bufs[4];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 4 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 4 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector write test 4 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) For each rank, construct a vector with base address
     *    (mpi_rank * INTS_PER_RANK) and writing all bytes from
     *    that address to ((mpi_rank + 1) * INTS_PER_RANK) - 1.
     *    Draw equal parts from increasing_fi_buf,
     *    decreasing_fi_buf, negative_fi_buf, and zero_fi_buf.
     *
     *    Write to file.
     */
    if (pass) {

        count = 4;

        base_addr       = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
        ints_per_write  = INTS_PER_RANK / 4;
        bytes_per_write = (size_t)(ints_per_write) * sizeof(int32_t);

        types[0] = H5FD_MEM_DRAW;
        addrs[0] = base_addr;
        sizes[0] = bytes_per_write;
        bufs[0]  = (const void *)(&(zero_fi_buf[mpi_rank * INTS_PER_RANK]));

        types[1] = H5FD_MEM_DRAW;
        addrs[1] = addrs[0] + (haddr_t)(bytes_per_write);
        sizes[1] = bytes_per_write;
        bufs[1]  = (const void *)(&(negative_fi_buf[(mpi_rank * INTS_PER_RANK) + (INTS_PER_RANK / 4)]));

        types[2] = H5FD_MEM_DRAW;
        addrs[2] = addrs[1] + (haddr_t)(bytes_per_write);
        sizes[2] = bytes_per_write;
        bufs[2]  = (const void *)(&(decreasing_fi_buf[(mpi_rank * INTS_PER_RANK) + (INTS_PER_RANK / 2)]));

        types[3] = H5FD_MEM_DRAW;
        addrs[3] = addrs[2] + (haddr_t)(bytes_per_write);
        sizes[3] = bytes_per_write;
        bufs[3] =
            (const void *)(&(increasing_fi_buf[(mpi_rank * INTS_PER_RANK) + (3 * (INTS_PER_RANK / 4))]));

        if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf,
     *    decreasing_fi_buf, negative_fi_buf, and zero_fi_buf as
     *    appropriate.  Report failure if any differences are
     *    detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = false;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != zero_fi_buf[j]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (1)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != negative_fi_buf[j]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (2)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != decreasing_fi_buf[j]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (3)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (3)";
                    break;
                }
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_4() */

/*-------------------------------------------------------------------------
 * Function:    vector_write_test_5()
 *
 * Purpose:     Test vector I/O writes with vectors of different lengths
 *              and entry sizes across the ranks.  Vectors are not, in
 *              general, sorted in increasing address order.  Further,
 *              writes are not, in general, contiguous.
 *
 *              1) Open the test file with the specified VFD, and set
 *                 the eoa.
 *
 *              2) Set the test file in a known state by writing zeros
 *                 to all bytes in the test file.  Since we have already
 *                 tested this, do this via a vector write of zero_fi_buf.
 *
 *              3) Barrier
 *
 *              4) For each rank, define base_index equal to:
 *
 *                     mpi_rank * INTS_PER_RANK
 *
 *                 and define base_addr equal to
 *
 *                     base_index * sizeof(int32_t).
 *
 *                 Setup a vector write between base_addr and
 *                 base_addr + INTS_PER_RANK * sizeof(int32_t) - 1
 *                 as follows:
 *
 *                 if ( rank % 4 == 0 ) construct a vector that writes:
 *
 *                       negative_fi_buf starting at base_index +
 *                       INTS_PER_RANK / 2 and running for INTS_PER_RANK / 4
 *                       entries,
 *
 *                       decreasing_fi_buf starting at base_index +
 *                       INTS_PER_RANK / 4 and running for INTS_PER_RANK / 8
 *                       entries, and
 *
 *                       increasing_fi_buf starting at base_index +
 *                       INTS_PER_RANK / 16 and running for INTS_PER_RANK / 16
 *                       entries
 *
 *                   to the equivalent locations in the file.
 *
 *                 if ( rank % 4 == 1 ) construct a vector that writes:
 *
 *                       increasing_fi_buf starting at base_index + 1 and
 *                       running for (INTS_PER_RANK / 2) - 2 entries, and
 *
 *                       decreasing_fi_buf startomg at base_index +
 *                       INTS_PER_RANK / 2 + 1 and running for (INTS_PER_RANK / 2)
 *                       - 2 entries
 *
 *                 if ( rank % 4 == 2 ) construct a vector that writes:
 *
 *                       negative_fi_buf starting at base_index +
 *                       INTS_PER_RANK / 2 and running for one entry.
 *
 *                 if ( rank % 4 == 3 ) construct and write the empty vector
 *
 *              5) Barrier
 *
 *              6) On each rank, read the entire file into the read_fi_buf,
 *                 and compare against zero_fi_buf, negative_fi_buf,
 *                 decreasing_fi_buf, and  increasing_fi_buf as
 *                 appropriate.  Report failure if any differences are
 *                 detected.
 *
 *              7) Close the test file.  On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_write_test_5(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_write_test_5()";
    char        test_title[120];
    char        filename[512];
    haddr_t     base_addr;
    int         base_index;
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    int         k;
    uint32_t    count;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    const void *bufs[4];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 5 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 5 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector write test 5 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Set the test file in a known state by writing zeros
     *    to all bytes in the test file.  Since we have already
     *    tested this, do this via a vector write of zero_fi_buf.
     */
    if (pass) {

        count    = 1;
        types[0] = H5FD_MEM_DRAW;
        addrs[0] = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
        sizes[0] = (size_t)INTS_PER_RANK * sizeof(int32_t);
        bufs[0]  = (const void *)(&(zero_fi_buf[mpi_rank * INTS_PER_RANK]));

        if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) For each rank, define base_index equal to:
     *
     *        mpi_rank * INTS_PER_RANK
     *
     *    and define base_addr equal to
     *
     *        base_index * sizeof(int32_t).
     *
     *    Setup a vector write between base_addr and
     *    base_addr + INTS_PER_RANK * sizeof(int32_t) - 1
     *    as follows:
     */
    if (pass) {

        base_index = mpi_rank * INTS_PER_RANK;
        base_addr  = (haddr_t)((size_t)base_index * sizeof(int32_t));

        if ((mpi_rank % 4) == 0) {

            /* if ( rank % 4 == 0 ) construct a vector that writes:
             *
             *     negative_fi_buf starting at base_index +
             *     INTS_PER_RANK / 2 and running for INTS_PER_RANK / 4
             *     entries,
             *
             *     decreasing_fi_buf starting at base_index +
             *     INTS_PER_RANK / 4 and running for INTS_PER_RANK / 8
             *     entries, and
             *
             *     increasing_fi_buf starting at base_index +
             *     INTS_PER_RANK / 16 and running for INTS_PER_RANK / 16
             *     entries
             *
             *  to the equivalent locations in the file.
             */
            count = 3;

            types[0] = H5FD_MEM_DRAW;
            addrs[0] = base_addr + (haddr_t)((size_t)(INTS_PER_RANK / 2) * sizeof(int32_t));
            sizes[0] = (size_t)(INTS_PER_RANK / 4) * sizeof(int32_t);
            bufs[0]  = (const void *)(&(negative_fi_buf[base_index + (INTS_PER_RANK / 2)]));

            types[1] = H5FD_MEM_DRAW;
            addrs[1] = base_addr + (haddr_t)((size_t)(INTS_PER_RANK / 4) * sizeof(int32_t));
            sizes[1] = (size_t)(INTS_PER_RANK / 8) * sizeof(int32_t);
            bufs[1]  = (const void *)(&(decreasing_fi_buf[base_index + (INTS_PER_RANK / 4)]));

            types[2] = H5FD_MEM_DRAW;
            addrs[2] = base_addr + (haddr_t)((size_t)(INTS_PER_RANK / 16) * sizeof(int32_t));
            sizes[2] = (size_t)(INTS_PER_RANK / 16) * sizeof(int32_t);
            bufs[2]  = (const void *)(&(increasing_fi_buf[base_index + (INTS_PER_RANK / 16)]));
        }
        else if ((mpi_rank % 4) == 1) {

            /* if ( rank % 4 == 1 ) construct a vector that writes:
             *
             *     increasing_fi_buf starting at base_index + 1 and
             *     running for (INTS_PER_RANK / 2) - 2 entries, and
             *
             *     decreasing_fi_buf startomg at base_addr +
             *     INTS_PER_RANK / 2 + 1 and running for (INTS_PER_RANK / 2)
             *     - 2 entries
             *
             *  to the equivalent locations in the file.
             */
            count = 2;

            types[0] = H5FD_MEM_DRAW;
            addrs[0] = base_addr + (haddr_t)(sizeof(int32_t));
            sizes[0] = (size_t)((INTS_PER_RANK / 2) - 2) * sizeof(int32_t);
            bufs[0]  = (const void *)(&(increasing_fi_buf[base_index + 1]));

            types[1] = H5FD_MEM_DRAW;
            addrs[1] = base_addr + (haddr_t)((size_t)((INTS_PER_RANK / 2) + 1) * sizeof(int32_t));
            sizes[1] = (size_t)((INTS_PER_RANK / 2) - 2) * sizeof(int32_t);
            bufs[1]  = (const void *)(&(decreasing_fi_buf[base_index + (INTS_PER_RANK / 2) + 1]));
        }
        else if ((mpi_rank % 4) == 2) {

            /* if ( rank % 4 == 2 ) construct a vector that writes:
             *
             *     negative_fi_buf starting at base_index +
             *     INTS_PER_RANK / 2 and running for one entry.
             *
             *  to the equivalent location in the file.
             */
            count = 1;

            types[0] = H5FD_MEM_DRAW;
            addrs[0] = base_addr + (haddr_t)((size_t)(INTS_PER_RANK / 2) * sizeof(int32_t));
            sizes[0] = sizeof(int32_t);
            bufs[0]  = (const void *)(&(negative_fi_buf[base_index + (INTS_PER_RANK / 2)]));
        }
        else if ((mpi_rank % 4) == 3) {

            /* if ( rank % 4 == 3 ) construct and write the empty vector */

            count = 0;
        }

        if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf,
     *    decreasing_fi_buf, negative_fi_buf, and zero_fi_buf as
     *    appropriate.  Report failure if any differences are
     *    detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = false;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;

            for (j = base_index; j < base_index + INTS_PER_RANK; j++) {

                k = j - base_index;

                switch (i % 4) {

                    case 0:
                        if (((INTS_PER_RANK / 2) <= k) && (k < (3 * (INTS_PER_RANK / 4)))) {

                            if (read_fi_buf[j] != negative_fi_buf[j]) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (1.1)";

                                printf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                       negative_fi_buf[j]);
                            }
                        }
                        else if (((INTS_PER_RANK / 4) <= k) && (k < (3 * (INTS_PER_RANK / 8)))) {

                            if (read_fi_buf[j] != decreasing_fi_buf[j]) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (1.2)";

                                printf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                       decreasing_fi_buf[j]);
                            }
                        }
                        else if (((INTS_PER_RANK / 16) <= k) && (k < (INTS_PER_RANK / 8))) {

                            if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (1.3)";

                                printf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                       increasing_fi_buf[j]);
                            }
                        }
                        else {

                            if (read_fi_buf[j] != 0) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (1.4)";
                            }
                        }
                        break;

                    case 1:
                        if ((1 <= k) && (k <= ((INTS_PER_RANK / 2) - 2))) {

                            if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (2.1)";

                                printf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                       increasing_fi_buf[j]);
                            }
                        }
                        else if ((((INTS_PER_RANK / 2) + 1) <= k) && (k <= (INTS_PER_RANK - 2))) {

                            if (read_fi_buf[j] != decreasing_fi_buf[j]) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (2.2)";

                                printf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                       decreasing_fi_buf[j]);
                            }
                        }
                        else {

                            if (read_fi_buf[j] != 0) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (2.3)";
                            }
                        }
                        break;

                    case 2:
                        if (k == INTS_PER_RANK / 2) {

                            if (read_fi_buf[j] != negative_fi_buf[j]) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (3.1)";

                                printf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                       negative_fi_buf[j]);
                            }
                        }
                        else {

                            if (read_fi_buf[j] != 0) {

                                pass         = false;
                                failure_mssg = "unexpected data read from file (3.2)";
                            }
                        }
                        break;

                    case 3:
                        if (read_fi_buf[j] != 0) {

                            pass         = false;
                            failure_mssg = "unexpected data read from file (4)";
                        }
                        break;

                    default:
                        assert(false); /* should be un-reachable */
                        break;
                }
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_5() */

/*-------------------------------------------------------------------------
 * Function:    vector_write_test_6()
 *
 * Purpose:     Test correct management of the sizes[] array optimization,
 *              where, if sizes[i] == 0, we use sizes[i - 1] as the value
 *              of size[j], for j >= i.
 *
 *               1) Open the test file with the specified VFD, set the eoa.
 *                  and setup the DXPL.
 *
 *               2) Using rank zero, write the entire zero_fi_buf to
 *                  the file.
 *
 *               3) Barrier
 *
 *               4) For each rank, define base_index equal to:
 *
 *                      mpi_rank * INTS_PER_RANK
 *
 *                  and define base_addr equal to
 *
 *                      base_index * sizeof(int32_t).
 *
 *                  Setup a vector write from increasing_fi_buf between
 *                  base_addr and base_addr + INTS_PER_RANK *
 *                  sizeof(int32_t) - 1 that writes every 16th integer
 *                  located in that range starting at base_addr.
 *                  Use a sizes[] array of length 2, with sizes[0] set
 *                  to sizeof(int32_t), and sizes[1] = 0.
 *
 *                  Write the integers into the corresponding locations in
 *                  the file.
 *
 *               5) Barrier
 *
 *               6) On each rank, read the entire file into the read_fi_buf,
 *                  and compare against zero_fi_buf, and increasing_fi_buf
 *                  as appropriate.  Report failure if any differences are
 *                  detected.
 *
 *               7) Barrier.
 *
 *               8) Close the test file.
 *
 *               9) On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_write_test_6(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_write_test_6()";
    char        test_title[120];
    char        filename[512];
    haddr_t     eoa;
    haddr_t     base_addr;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         base_index;
    uint32_t    count = 0;
    H5FD_mem_t  types[(INTS_PER_RANK / 16) + 1];
    haddr_t     addrs[(INTS_PER_RANK / 16) + 1];
    size_t      sizes[2];
    const void *bufs[(INTS_PER_RANK / 16) + 1];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 6 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 6 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector write test 6 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)zero_fi_buf) < 0) {

                pass         = false;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) For each rank, define base_index equal to:
     *
     *        mpi_rank * INTS_PER_RANK
     *
     *    and define base_addr equal to
     *
     *        base_index * sizeof(int32_t).
     *
     *    Setup a vector write from increasing_fi_buf between
     *    base_addr and base_addr + INTS_PER_RANK *
     *    sizeof(int32_t) - 1 that writes every 16th integer
     *    located in that range starting at base_addr.
     *    Use a sizes[] array of length 2, with sizes[0] set
     *    to sizeof(int32_t), and sizes[1] = 0.
     *
     *    Write the integers into the corresponding locations in
     *    the file.
     */
    if (pass) {

        base_index = (mpi_rank * INTS_PER_RANK);
        base_addr  = (haddr_t)base_index * (haddr_t)sizeof(int32_t);

        count    = INTS_PER_RANK / 16;
        sizes[0] = sizeof(int32_t);
        sizes[1] = 0;

        for (i = 0; i < INTS_PER_RANK / 16; i++) {

            types[i] = H5FD_MEM_DRAW;
            addrs[i] = base_addr + ((haddr_t)(16 * i) * (haddr_t)sizeof(int32_t));
            bufs[i]  = (const void *)(&(increasing_fi_buf[base_index + (i * 16)]));
        }

        if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On each rank, read the entire file into the read_fi_buf,
     *    and compare against zero_fi_buf, and increasing_fi_buf
     *    as appropriate.  Report failure if any differences are
     *    detected.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = false;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size * INTS_PER_RANK)); i++) {

            if (i % 16 == 0) {

                if (read_fi_buf[i] != increasing_fi_buf[i]) {

                    pass         = false;
                    failure_mssg = "unexpected data read from file (1)";
                }
            }
            else if (read_fi_buf[i] != zero_fi_buf[i]) {

                pass         = false;
                failure_mssg = "unexpected data read from file (2)";
            }
        }
    } /* end if */

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_6() */

/*-------------------------------------------------------------------------
 * Function:    vector_write_test_7()
 *
 * Purpose:     Test vector I/O with larger vectors -- 8 elements in each
 *              vector for now.
 *
 *              1) Open the test file with the specified VFD, and set
 *                 the eoa.
 *
 *              2) Set the test file in a known state by writing zeros
 *                 to all bytes in the test file.  Since we have already
 *                 tested this, do this via a vector write of zero_fi_buf.
 *
 *              3) Barrier
 *
 *              4) For each rank, define base_index equal to:
 *
 *                     mpi_rank * INTS_PER_RANK
 *
 *                 and define base_addr equal to
 *
 *                     base_index * sizeof(int32_t).
 *
 *                 Setup a vector of length 8, with each element of
 *                 length INTS_PER_RANK / 16, and base address
 *                 base_addr + i * (INTS_PER_RANK / 8), where i is
 *                 the index of the entry (starting at zero).  Draw
 *                 written data from the equivalent locations in
 *                 increasing_fi_buf.
 *
 *                 Write the vector.
 *
 *              5) Barrier
 *
 *              6) On each rank, read the entire file into the read_fi_buf,
 *                 and compare against zero_fi_buf, and increasing_fi_buf as
 *                 appropriate.  Report failure if any differences are
 *                 detected.
 *
 *              7) Close the test file.  On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
vector_write_test_7(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_write_test_7()";
    char        test_title[120];
    char        filename[512];
    haddr_t     base_addr;
    haddr_t     addr_increment;
    int         base_index;
    haddr_t     eoa;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    int         k;
    uint32_t    count;
    H5FD_mem_t  types[8];
    haddr_t     addrs[8];
    size_t      sizes[8];
    const void *bufs[8];

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 7 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 7 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector write test 7 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Set the test file in a known state by writing zeros
     *    to all bytes in the test file.  Since we have already
     *    tested this, do this via a vector write of zero_fi_buf.
     */
    if (pass) {

        count    = 1;
        types[0] = H5FD_MEM_DRAW;
        addrs[0] = (haddr_t)mpi_rank * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));
        sizes[0] = (size_t)INTS_PER_RANK * sizeof(int32_t);
        bufs[0]  = (void *)(&(zero_fi_buf[mpi_rank * INTS_PER_RANK]));

        if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed.\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if (pass) {

        base_index     = mpi_rank * INTS_PER_RANK;
        base_addr      = (haddr_t)((size_t)base_index * sizeof(int32_t));
        addr_increment = (haddr_t)((INTS_PER_RANK / 8) * sizeof(int32_t));

        count = 8;

        for (i = 0; i < (int)count; i++) {

            types[i] = H5FD_MEM_DRAW;
            addrs[i] = base_addr + ((haddr_t)(i)*addr_increment);
            sizes[i] = (size_t)(INTS_PER_RANK / 16) * sizeof(int32_t);
            bufs[i]  = (void *)(&(increasing_fi_buf[base_index + (i * (INTS_PER_RANK / 8))]));
        }

        if (H5FDwrite_vector(lf, dxpl_id, count, types, addrs, sizes, bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf, and zero_fi_buf as
     *    appropriate.  Report failure if any differences are
     *    detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = false;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;

            for (j = base_index; j < base_index + INTS_PER_RANK; j++) {

                k = j - base_index;

                if ((k % (INTS_PER_RANK / 8)) < (INTS_PER_RANK / 16)) {

                    if (read_fi_buf[j] != increasing_fi_buf[j]) {

                        pass         = false;
                        failure_mssg = "unexpected data read from file (1)";

                        printf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                               increasing_fi_buf[j]);
                    }
                }
                else {

                    if (read_fi_buf[j] != 0) {

                        pass         = false;
                        failure_mssg = "unexpected data read from file (2)";

                        printf("\nread_fi_buf[%d] = %d, 0 expected.\n", j, read_fi_buf[j]);
                    }
                }
            }
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_7() */

/*-------------------------------------------------------------------------
 * Function:    vector_write_test_8()
 *
 * Purpose:     This test is to verify the fix for the following problem
 *              in H5FD__mpio_write_vector when calculating max_addr:
 *              --illegal reference occurs when referencing the s_sizes array
 *                with <count - 1> due to <count> exceeding the length of the
 *                size array which uses the compressed feature.
 *
 *              1) Open the test file with the specified VFD, and set
 *                 the eoa.
 *
 *              2) Set the test file in a known state by writing zeros
 *                 to all bytes in the test file.  Since we have already
 *                 tested this, do this via a vector write of zero_fi_buf.
 *
 *              3) Barrier
 *
 *              4) For each rank, define base_index equal to:
 *
 *                     mpi_rank * INTS_PER_RANK
 *
 *                 and define base_addr equal to
 *
 *                     base_index * sizeof(int32_t).
 *
 *                 Setup a vector of length INTS_PER_RANK - 1.
 *                 Set up the size array with the compressed feature:
 *                  --The first element has size (2 * sizeof(int32_t))
 *                  --The second and third elements are of size sizeof(int32_t)
 *                  --The fourth element is zero.
 *                  Set up addrs and bufs accordingly.
 *
 *                 Write the vector.
 *
 *              5) Barrier
 *
 *              6) On each rank, read the entire file into the read_fi_buf,
 *                 and compare against increasing_fi_buf.
 *                 Report failure if any differences are detected.
 *
 *              7) Close the test file.  On rank 0, delete the test file.
 *
 * Return:      false on success, true if any errors are detected.
 *
 *-------------------------------------------------------------------------
 */
static unsigned
vector_write_test_8(int file_name_id, int mpi_rank, int mpi_size, H5FD_mpio_xfer_t xfer_mode,
                    H5FD_mpio_collective_opt_t coll_opt_mode, const char *vfd_name)
{
    const char *fcn_name = "vector_write_test_8()";
    char        test_title[120];
    char        filename[512];
    haddr_t     eoa;
    haddr_t     base_addr;
    bool        show_progress = false;
    hid_t       fapl_id       = H5I_INVALID_HID; /* file access property list ID */
    hid_t       dxpl_id       = H5I_INVALID_HID; /* data access property list ID */
    H5FD_t     *lf            = NULL;            /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         base_index;
    uint32_t    count = 0;
    size_t      sizes[4];
    H5FD_mem_t  types[2];

    haddr_t     *tt_addrs = NULL; /* For storing addrs */
    const void **tt_bufs  = NULL; /* For storing buf pointers */

    pass = true;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 8 -- %s / independent",
                     vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            snprintf(test_title, sizeof(test_title), "parallel vector write test 8 -- %s / col op / ind I/O",
                     vfd_name);
        }
        else {

            assert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            snprintf(test_title, sizeof(test_title), "parallel vector write test 8 -- %s / col op / col I/O",
                     vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        fprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Allocate local buffers for addrs and bufs,
          open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        tt_addrs = (haddr_t *)malloc((INTS_PER_RANK) * sizeof(haddr_t *));
        tt_bufs  = (const void **)malloc((INTS_PER_RANK) * sizeof(void *));

        if (tt_addrs == NULL || tt_bufs == NULL) {
            pass         = false;
            failure_mssg = "Can't allocate local addrs and bufs buffers.";
        }

        if (pass) {
            eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

            setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa,
                                &lf, &fapl_id, &dxpl_id);
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)zero_fi_buf) < 0) {

                pass         = false;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) For each rank, define base_index equal to:
     *
     *        mpi_rank * INTS_PER_RANK
     *
     *    and define base_addr equal to
     *
     *        base_index * sizeof(int32_t).
     *
     *    Set up the array of sizes and types with the compressed feature
     *    as described in the routine header description.
     */
    if (pass) {

        base_index = (mpi_rank * INTS_PER_RANK);
        base_addr  = (haddr_t)base_index * (haddr_t)sizeof(int32_t);

        count = INTS_PER_RANK - 1;

        types[0] = H5FD_MEM_DRAW;
        types[1] = H5FD_MEM_NOLIST;

        sizes[0] = 2 * sizeof(int32_t);
        sizes[1] = sizeof(int32_t);
        sizes[2] = sizeof(int32_t);
        sizes[3] = 0;

        tt_addrs[0] = base_addr;
        tt_bufs[0]  = (const void *)(&(increasing_fi_buf[base_index]));

        tt_addrs[0] = base_addr;
        base_index += 2;
        base_addr = (haddr_t)base_index * (haddr_t)sizeof(int32_t);

        for (i = 1; i < (INTS_PER_RANK - 1); i++) {

            tt_addrs[i] = base_addr + ((haddr_t)(i - 1) * (haddr_t)sizeof(int32_t));
            tt_bufs[i]  = (const void *)(&(increasing_fi_buf[base_index + (i - 1)]));
        }

        if (H5FDwrite_vector(lf, dxpl_id, count, types, tt_addrs, sizes, tt_bufs) < 0) {

            pass         = false;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf
     *    Report failure if any differences are detected.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = false;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size * INTS_PER_RANK)); i++) {

            if (read_fi_buf[i] != increasing_fi_buf[i]) {

                pass         = false;
                failure_mssg = "unexpected data read from file (1)";
            }
        }
    } /* end if */

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    /* Free the local buffers */
    if (tt_addrs) {
        free(tt_addrs);
        tt_addrs = NULL;
    }

    if (tt_bufs) {
        free(tt_bufs);
        tt_bufs = NULL;
    }

    if (show_progress)
        fprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                fprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_8() */

static void
test_vector_io(int mpi_rank, int mpi_size)
{
    unsigned nerrs = 0;

    nerrs += alloc_and_init_file_images(mpi_size);

    if (!pass) {

        printf("\nAllocation and initialize of file image buffers failed.  Test aborted.\n");
        nerrors += (int)nerrs;
        return;
    }

    MPI_Barrier(comm);

    nerrs +=
        vector_read_test_1(0, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_1(0, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_1(0, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_read_test_2(1, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_2(1, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_2(1, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_read_test_3(2, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_3(2, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_3(2, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_read_test_4(3, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_4(3, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_4(3, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_read_test_5(4, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_5(4, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs += vector_read_test_5(4, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_write_test_1(0, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_1(0, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_1(0, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_write_test_2(1, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_2(1, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_2(1, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_write_test_3(2, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_3(2, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_3(2, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_write_test_4(3, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_4(3, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_4(3, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_write_test_5(4, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_5(4, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_5(4, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_write_test_6(5, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_6(5, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_6(5, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_write_test_7(6, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_7(6, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_7(6, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    nerrs +=
        vector_write_test_8(7, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_8(7, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO, "mpio");
    nerrs +=
        vector_write_test_8(7, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO, "mpio");

    MPI_Barrier(comm);

#ifdef H5_HAVE_SUBFILING_VFD
    if (mpi_rank == 0) {

        printf("\n\n --- TESTING SUBFILING VFD --- \n\n");
    }

    nerrs += vector_read_test_1(7, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_1(7, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_1(7, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                H5FD_SUBFILING_NAME);

    nerrs += vector_read_test_2(8, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_2(8, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_2(8, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                H5FD_SUBFILING_NAME);

    nerrs += vector_read_test_3(9, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_3(9, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_3(9, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                H5FD_SUBFILING_NAME);

    nerrs += vector_read_test_4(10, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_4(10, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_4(10, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                H5FD_SUBFILING_NAME);

    nerrs += vector_read_test_5(11, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_5(11, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                H5FD_SUBFILING_NAME);
    nerrs += vector_read_test_5(11, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                H5FD_SUBFILING_NAME);

    nerrs += vector_write_test_1(7, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_1(7, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_1(7, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                 H5FD_SUBFILING_NAME);

    nerrs += vector_write_test_2(8, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_2(8, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_2(8, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                 H5FD_SUBFILING_NAME);

    nerrs += vector_write_test_3(9, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_3(9, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_3(9, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                 H5FD_SUBFILING_NAME);

    nerrs += vector_write_test_4(10, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_4(10, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_4(10, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                 H5FD_SUBFILING_NAME);

    nerrs += vector_write_test_5(11, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_5(11, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_5(11, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                 H5FD_SUBFILING_NAME);

    nerrs += vector_write_test_6(12, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_6(12, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_6(12, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                 H5FD_SUBFILING_NAME);

    nerrs += vector_write_test_7(13, mpi_rank, mpi_size, H5FD_MPIO_INDEPENDENT, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_7(13, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_INDIVIDUAL_IO,
                                 H5FD_SUBFILING_NAME);
    nerrs += vector_write_test_7(13, mpi_rank, mpi_size, H5FD_MPIO_COLLECTIVE, H5FD_MPIO_COLLECTIVE_IO,
                                 H5FD_SUBFILING_NAME);
#endif

    nerrors += (int)nerrs;

    /* return(nerrs);*/

} /* test_vector_io() */

/*
 * Utility routine to perform the actual selection I/O read
 */
static herr_t
test_selection_io_read_verify(hid_t dxpl, int mpi_rank, hsize_t start[], hsize_t block[], H5FD_t *lf,
                              H5FD_mem_t type, uint32_t count, hid_t mem_spaces[], hid_t file_spaces[],
                              haddr_t offsets[], size_t element_sizes[], uint32_t rbufcount, int *erb[],
                              bool shorten_rbufs)
{
    int   *rbuf1    = NULL;
    int   *rbuf2    = NULL;
    int   *rbufs[2] = {NULL, NULL};
    size_t bufsize;
    int    i;
    int    j;

    bufsize = (hsize_t)(sel_dim0 * sel_dim1) * sizeof(int);
    if ((rbuf1 = malloc(bufsize)) == NULL)
        goto error;
    if ((rbuf2 = malloc(bufsize)) == NULL)
        goto error;
    rbufs[0] = rbuf1;
    rbufs[1] = rbuf2;

    /* Initialize read buffer */
    for (i = 0; i < (int)rbufcount; i++)
        for (j = 0; j < sel_dim0 * sel_dim1; j++)
            rbufs[i][j] = -1;

    /* Handle elements in count that are not part of rbufcount */
    for (i = (int)rbufcount; i < (int)count; i++)
        if (shorten_rbufs)
            rbufs[i] = NULL;
        else
            rbufs[i] = rbufs[rbufcount - 1];

    /* Issue read call */
    if (H5FDread_selection(lf, type, dxpl, count, mem_spaces, file_spaces, offsets, element_sizes,
                           (void **)rbufs) < 0)
        goto error;

    /* Verify result */
    for (i = 0; i < (int)rbufcount; i++) {
        hsize_t endblock = MIN((start[i] + block[i]), (hsize_t)(sel_dim0 * sel_dim1));
        for (j = (int)start[i]; j < (int)endblock; j++)
            if (rbufs[i][j] != erb[i][j]) {
                H5_FAILED();
                AT();
                printf(
                    "data read from file does not match expected values at mapping array location %d: %d\n",
                    i, mpi_rank);
                printf("expected data: \n");
                for (j = 0; j < sel_dim0 * sel_dim1; j++) {
                    printf("%6d", erb[i][j]);
                    if (!((j + 1) % sel_dim1))
                        printf("\n");
                }
                printf("read data: \n");
                for (j = 0; j < (sel_dim0 * sel_dim1); j++) {
                    printf("%6d", rbufs[i][j]);
                    if (!((j + 1) % sel_dim1))
                        printf("\n");
                }
                goto error;
            }
    }

    if (rbuf1)
        free(rbuf1);
    if (rbuf2)
        free(rbuf2);
    return 0;

error:
    if (rbuf1)
        free(rbuf1);
    if (rbuf2)
        free(rbuf2);
    return -1;

} /* end test_selection_io_read_verify() */

/*
 * Utility routine to perform the actual selection I/O write
 */
static herr_t
test_selection_io_write(hid_t dxpl, H5FD_t *lf, H5FD_mem_t type, uint32_t count, hid_t mem_spaces[],
                        hid_t file_spaces[], haddr_t offsets[], size_t element_sizes[], int *wb[])
{
    const void **bufs = NULL; /* Avoids cast/const warnings */
    int          i;
    int          j;

    if (NULL == (bufs = calloc(count, sizeof(void *))))
        goto error;

    /* Update write buffer */
    for (i = 0; i < (int)count; i++) {
        if (wb[i] && (i == 0 || wb[i] != wb[i - 1]))
            for (j = 0; j < (sel_dim0 * sel_dim1); j++)
                wb[i][j] += 2 * (sel_dim0 * sel_dim1);
        bufs[i] = wb[i];
    }

    /* Issue write call */
    if (H5FDwrite_selection(lf, type, dxpl, count, mem_spaces, file_spaces, offsets, element_sizes, bufs) < 0)
        goto error;

    if (bufs)
        free(bufs);

    return 0;

error:
    if (bufs)
        free(bufs);
    return -1;

} /* end test_selection_io_write() */

/*
 * Perform the following tests that use shortened arrays for wbuf and element sizes
 * --Test 1: Strided <> Strided 1D and 2D I/O for both file and memory spaces
 * --Reset selections
 * --Test 2: Strided <> Strided 2D I/O, 2 different selections in the same memory buffer
 * --Reset selections
 */
static void
test_selection_io_types_shorten(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl, H5FD_mem_t type,
                                haddr_t addrs[], size_t element_sizes[], hid_t mem_spaces[],
                                hid_t file_spaces[], hsize_t dims1[], hsize_t dims2[])
{
    hsize_t start[2];                 /* start for hyperslab          */
    hsize_t stride[2];                /* stride for hyperslab         */
    hsize_t count[2];                 /* count for hyperslab          */
    hsize_t block[2];                 /* block for hyperslab          */
    hsize_t verify_start[2] = {0, 0}; /* Starting block for verified data */
    hsize_t verify_block[2] = {0, 0}; /* Block size for verified data */
    int     i;
    int     j;
    int     i2;
    int     j2;

    int shorten_element_sizes; /* Whether to shorten the element sizes array */

    for (shorten_element_sizes = 0; shorten_element_sizes <= 1; shorten_element_sizes++) {
        /*
         * Test 1: Strided <> Strided 1D and 2D I/O
         */
        /* sel_dim1 must be even */
        assert(sel_dim1 / 2 == (sel_dim1 + 1) / 2);

        /* Strided selection in memory (1D) */
        block[0]  = 1;
        count[0]  = (hsize_t)(((sel_dim0 * sel_dim1) / 2) / mpi_size);
        stride[0] = 2;
        start[0]  = (hsize_t)mpi_rank * stride[0] * count[0];
        if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        verify_start[0] = start[0];
        verify_block[0] = (count[0] * stride[0]);

        /* Strided selection in file (1D) */
        start[0] = 1 + ((hsize_t)mpi_rank * stride[0] * count[0]);
        if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        /* Strided selection (across dim 1) in file (2D) */
        block[0]  = 1;
        block[1]  = 1;
        count[0]  = (hsize_t)(sel_dim0 / mpi_size);
        count[1]  = (hsize_t)sel_dim1 / 2;
        stride[0] = 1;
        stride[1] = 2;
        start[0]  = (hsize_t)mpi_rank * count[0];
        start[1]  = 1;
        if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        /* Strided selection (across dim 0) in memory (2D) */
        block[0]  = 1;
        block[1]  = 1;
        count[0]  = (hsize_t)((sel_dim0 / 2) / mpi_size);
        count[1]  = (hsize_t)sel_dim1;
        stride[0] = 2;
        stride[1] = 1;
        start[0]  = 1 + ((hsize_t)mpi_rank * stride[0] * count[0]);
        start[1]  = 0;
        if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        verify_start[1] = start[0] * count[1];
        verify_block[1] = (count[0] * count[1] * stride[0]);

        /* Issue write call */
        if (test_selection_io_write(dxpl, lf, type, 2, mem_spaces, file_spaces, addrs, element_sizes,
                                    (int **)wbufs) < 0)
            P_TEST_ERROR;

        MPI_Barrier(comm);

        /* Update file bufs */
        for (i = 0; i < (sel_dim0 * sel_dim1) / 2; i++)
            fbuf1[(2 * i) + 1] = wbuf1[2 * i];
        for (i = 1, i2 = 0, j2 = 1; i < sel_dim0; i += 2)
            for (j = 0; j < sel_dim1; j++) {
                assert(i2 < sel_dim0);
                fbuf2[i2 * sel_dim1 + j2] = wbuf2[i * sel_dim1 + j];
                j2 += 2;
                if (j2 >= sel_dim1) {
                    i2++;
                    j2 = 1;
                }
            }

        /* Update expected read bufs */
        for (i = 0; i < (sel_dim0 * sel_dim1); i++)
            erbuf1[i] = -1;
        for (i = 0; i < (sel_dim0 * sel_dim1) / 2; i++)
            erbuf1[2 * i] = wbuf1[2 * i];
        for (i = 0; i < sel_dim0; i++)
            for (j = 0; j < sel_dim1; j++)
                erbuf2[i * sel_dim1 + j] = -1;
        for (i = 1; i < sel_dim0; i += 2)
            for (j = 0; j < sel_dim1; j++)
                erbuf2[i * sel_dim1 + j] = wbuf2[i * sel_dim1 + j];

        /* Read and verify */
        if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 2, mem_spaces,
                                          file_spaces, addrs, element_sizes, 2, (int **)erbufs, false) < 0)
            P_TEST_ERROR;

        MPI_Barrier(comm);

        /*
         * Reset selections
         */
        if (H5Sselect_all(mem_spaces[0]) < 0)
            P_TEST_ERROR;
        if (H5Sselect_all(file_spaces[0]) < 0)
            P_TEST_ERROR;

        /* Each process takes x number of elements */
        block[0]  = dims1[0] / (hsize_t)mpi_size;
        count[0]  = 1;
        stride[0] = block[0];
        start[0]  = (hsize_t)mpi_rank * block[0];

        verify_start[0] = start[0];
        verify_block[0] = block[0];

        if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;
        if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        if (H5Sselect_all(mem_spaces[1]) < 0)
            P_TEST_ERROR;
        if (H5Sselect_all(file_spaces[1]) < 0)
            P_TEST_ERROR;

        /* Each process takes x number of elements */
        block[0]  = dims2[0] / (hsize_t)mpi_size;
        block[1]  = dims2[1];
        count[0]  = 1;
        count[1]  = 1;
        stride[0] = block[0];
        stride[1] = block[1];
        start[0]  = (hsize_t)mpi_rank * block[0];
        start[1]  = 0;

        verify_start[1] = start[0] * block[1];
        verify_block[1] = (block[0] * block[1]);

        if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;
        if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        /* Read entire file buffer and verify */
        if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 2, mem_spaces,
                                          file_spaces, addrs, element_sizes, 2, (int **)fbufs, false) < 0)
            P_TEST_ERROR;

        MPI_Barrier(comm);

        /*
         * Test 2: Strided <> Strided 2D I/O, 2 different selections in the same memory buffer
         */
        /* Switch mem and file spaces to both be 2D */
        if (H5Sset_extent_simple(mem_spaces[0], 2, dims2, NULL) < 0)
            P_TEST_ERROR;
        if (H5Sset_extent_simple(file_spaces[0], 2, dims2, NULL) < 0)
            P_TEST_ERROR;

        /* Strided selection in memory (1st) */
        block[0]  = 1;
        block[1]  = 1;
        count[0]  = (hsize_t)((sel_dim0 / 2) / mpi_size);
        count[1]  = (hsize_t)sel_dim1;
        stride[0] = 2;
        stride[1] = 1;
        start[0]  = (hsize_t)mpi_rank * count[0] * stride[0];
        start[1]  = 0;

        if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        verify_start[0] = start[0] * count[1];
        verify_block[0] = (count[0] * count[1] * stride[0]);

        /* Strided selection (across dim 0) in memory (2nd) */
        start[0] = 1 + ((hsize_t)mpi_rank * count[0] * stride[0]);
        if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        verify_start[1] = start[0] * count[1];
        verify_block[1] = (count[0] * count[1] * stride[0]);

        /* Strided selection in file (1st) */
        block[0]  = 1;
        block[1]  = 1;
        count[0]  = (hsize_t)(sel_dim0 / mpi_size);
        count[1]  = (hsize_t)sel_dim1 / 2;
        stride[0] = 1;
        stride[1] = 2;
        start[0]  = (hsize_t)mpi_rank * count[0];
        start[1]  = 0;
        if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        /* Strided selection (across dim 1) in file (2nd) */
        block[0]  = 1;
        block[1]  = 1;
        count[0]  = (hsize_t)(sel_dim0 / mpi_size);
        count[1]  = (hsize_t)sel_dim1 / 2;
        stride[0] = 1;
        stride[1] = 2;
        start[0]  = (hsize_t)mpi_rank * count[0];
        start[1]  = 1;
        if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        /* Use the same memory buffer for both selections */
        wbufs[0] = wbuf2;

        /* Shorten wbuf array */
        if (shorten_element_sizes)
            wbufs[1] = NULL;
        else
            wbufs[1] = wbufs[0];

        /* Issue write call */
        if (test_selection_io_write(dxpl, lf, type, 2, mem_spaces, file_spaces, addrs, element_sizes,
                                    (int **)wbufs) < 0)
            P_TEST_ERROR;

        MPI_Barrier(comm);

        /* Update file bufs - need to reuse 1D array so data stays consistent, so use math to
         * find 1D index into 2D array */
        for (i = 0, i2 = 0, j2 = 0; i < sel_dim0; i += 2)
            for (j = 0; j < sel_dim1; j++) {
                assert(i2 < sel_dim0);
                fbuf1[(i2 * sel_dim1) + j2] = wbuf2[i * sel_dim1 + j];
                j2 += 2;
                if (j2 >= sel_dim1) {
                    i2++;
                    j2 = 0;
                }
            }
        for (i = 1, i2 = 0, j2 = 1; i < sel_dim0; i += 2)
            for (j = 0; j < sel_dim1; j++) {
                assert(i2 < sel_dim0);
                fbuf2[i2 * sel_dim1 + j2] = wbuf2[i * sel_dim1 + j];
                j2 += 2;
                if (j2 >= sel_dim1) {
                    i2++;
                    j2 = 1;
                }
            }

        /* Update expected read buf */
        for (i = 0; i < sel_dim0; i++)
            for (j = 0; j < sel_dim1; j++)
                erbuf2[i * sel_dim1 + j] = -1;
        for (i = 0; i < sel_dim0; i += 2)
            for (j = 0; j < sel_dim1; j++)
                erbuf2[i * sel_dim1 + j] = wbuf2[i * sel_dim1 + j];
        for (i = 1; i < sel_dim0; i += 2)
            for (j = 0; j < sel_dim1; j++)
                erbuf2[i * sel_dim1 + j] = wbuf2[i * sel_dim1 + j];

        /* Read and verify */
        if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 2, mem_spaces,
                                          file_spaces, addrs, element_sizes, 1, (int **)&erbufs[1],
                                          shorten_element_sizes ? true : false) < 0)
            P_TEST_ERROR;

        MPI_Barrier(comm);

        /*
         * Reset selections
         */
        if (H5Sselect_all(mem_spaces[0]) < 0)
            P_TEST_ERROR;
        if (H5Sselect_all(file_spaces[0]) < 0)
            P_TEST_ERROR;

        if (H5Sselect_all(mem_spaces[1]) < 0)
            P_TEST_ERROR;
        if (H5Sselect_all(file_spaces[1]) < 0)
            P_TEST_ERROR;

        /* Each process takes x number of elements */
        block[0]  = dims2[0] / (hsize_t)mpi_size;
        block[1]  = dims2[1];
        count[0]  = 1;
        count[1]  = 1;
        stride[0] = block[0];
        stride[1] = block[1];
        start[0]  = (hsize_t)mpi_rank * block[0];
        start[1]  = 0;

        if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;
        if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;
        if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;

        /* Read entire file buffer and verify */
        verify_start[0] = start[0] * block[1];
        verify_block[0] = (block[0] * block[1]);
        verify_start[1] = start[0] * block[1];
        verify_block[1] = (block[0] * block[1]);
        if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 2, mem_spaces,
                                          file_spaces, addrs, element_sizes, 2, (int **)fbufs, false) < 0)
            P_TEST_ERROR;

        MPI_Barrier(comm);

        /* Reset first spaces to 1D */
        if (H5Sset_extent_simple(mem_spaces[0], 1, dims1, NULL) < 0)
            P_TEST_ERROR;
        if (H5Sset_extent_simple(file_spaces[0], 1, dims1, NULL) < 0)
            P_TEST_ERROR;

        /* Reset write buffer array */
        wbufs[0] = wbuf1;
        wbufs[1] = wbuf2;

        /* Change to shortened element sizes array */
        element_sizes[1] = 0;

        MPI_Barrier(comm);
    }

    /* Reset element sizes array */
    element_sizes[1] = element_sizes[0];

    return;

} /* test_selection_io_types_shorten() */

/*
 * Perform the following tests for 1 & 2 dimensional spaces:
 * --Test 1: Strided 1D (memory) <> Strided 2D (file) I/O
 * --Reset selections
 * --Test 2: Strided 2D (memory) <> Strided 1D (file) I/O
 * --Reset selections
 */
static void
test_selection_io_types_1d_2d(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl, H5FD_mem_t type,
                              haddr_t addrs[], size_t element_sizes[], hid_t mem_spaces[],
                              hid_t file_spaces[], hsize_t dims1[], hsize_t dims2[])
{
    hsize_t start[2];                 /* start for hyperslab          */
    hsize_t stride[2];                /* stride for hyperslab         */
    hsize_t count[2];                 /* count for hyperslab          */
    hsize_t block[2];                 /* block for hyperslab          */
    hsize_t verify_start[2] = {0, 0}; /* Starting block for verified data */
    hsize_t verify_block[2] = {0, 0}; /* Block size for verified data */
    int     i;
    int     j;
    int     i2;
    int     j2;

    /*
     * Test 1: Strided 1D (memory) <> Strided 2D (file) I/O
     */
    /* Strided selection (across dim 1) in file */
    block[0]  = 1;
    block[1]  = 1;
    count[0]  = (hsize_t)(sel_dim0 / mpi_size);
    count[1]  = (hsize_t)sel_dim1 / 2;
    stride[0] = 1;
    stride[1] = 2;
    start[0]  = (hsize_t)mpi_rank * count[0];
    start[1]  = 1;
    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Strided selection in memory */
    block[0]  = 1;
    count[0]  = (hsize_t)(((sel_dim0 * sel_dim1) / 2) / mpi_size);
    stride[0] = 2;
    start[0]  = 1 + ((hsize_t)mpi_rank * stride[0] * count[0]);
    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[0], &file_spaces[1], &addrs[1], element_sizes,
                                (int **)&wbufs[0]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 1, i2 = 0, j2 = 1; i < (sel_dim0 * sel_dim1); i += 2) {
        assert(i2 < sel_dim0);
        fbuf2[(i2 * sel_dim1) + j2] = wbuf1[i];
        j2 += 2;
        if (j2 >= sel_dim1) {
            i2++;
            j2 = 1;
        }
    }

    /* Update expected read buf */
    for (i = 0; i < (sel_dim0 * sel_dim1); i++)
        erbuf1[i] = -1;
    for (i = 1; i < (sel_dim0 * sel_dim1); i += 2)
        erbuf1[i] = wbuf1[i];

    /* Read and verify */
    verify_start[0] = start[0];
    verify_block[0] = (count[0] * stride[0]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&erbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Reset selections
     */
    if (H5Sselect_all(file_spaces[1]) < 0)
        P_TEST_ERROR;

    if (H5Sselect_all(mem_spaces[0]) < 0)
        P_TEST_ERROR;

    block[0]  = dims2[0] / (hsize_t)mpi_size;
    block[1]  = dims2[1];
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = block[0];
    stride[1] = block[1];
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;
    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    block[0]  = dims1[0] / (hsize_t)mpi_size;
    count[0]  = 1;
    stride[0] = block[0];
    start[0]  = (hsize_t)mpi_rank * block[0];
    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Read entire file buffer and verify */
    verify_start[0] = start[0];
    verify_block[0] = block[0];
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&fbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Test 2: Strided 2D (memory) <> Strided 1D (file) I/O
     */
    /* Strided selection in file */
    block[0]  = 1;
    count[0]  = (hsize_t)(((sel_dim0 * sel_dim1) / 2) / mpi_size);
    stride[0] = 2;
    start[0]  = (hsize_t)mpi_rank * stride[0] * count[0];
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Strided selection (across dim 0) in memory */
    block[0]  = 1;
    block[1]  = 1;
    count[0]  = (hsize_t)((sel_dim0 / 2) / mpi_size);
    count[1]  = (hsize_t)sel_dim1;
    stride[0] = 2;
    stride[1] = 1;
    start[0]  = (hsize_t)mpi_rank * count[0] * stride[0];
    start[1]  = 0;
    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[1], &file_spaces[0], &addrs[0], element_sizes,
                                (int **)&wbufs[1]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0, i2 = 0; i < sel_dim0; i += 2)
        for (j = 0; j < sel_dim1; j++) {
            assert(i2 < (sel_dim0 * sel_dim1));
            fbuf1[i2] = wbuf2[i * sel_dim1 + j];
            i2 += 2;
        }

    /* Update expected read buf */
    for (i = 0; i < sel_dim0; i++)
        for (j = 0; j < sel_dim1; j++)
            erbuf2[(i * sel_dim1) + j] = -1;
    for (i = 0; i < sel_dim0; i += 2)
        for (j = 0; j < sel_dim1; j++)
            erbuf2[(i * sel_dim1) + j] = wbuf2[i * sel_dim1 + j];

    /* Read and verify */
    verify_start[0] = start[0] * count[1];
    verify_block[0] = (count[0] * count[1] * stride[0]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&erbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Reset selections
     */
    if (H5Sselect_all(file_spaces[0]) < 0)
        P_TEST_ERROR;

    if (H5Sselect_all(mem_spaces[1]) < 0)
        P_TEST_ERROR;

    /* Each process takes x number of elements */
    block[0]  = dims1[0] / (hsize_t)mpi_size;
    count[0]  = 1;
    stride[0] = block[0];
    start[0]  = (hsize_t)mpi_rank * block[0];
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Each process takes x number of elements */
    block[0]  = dims2[0] / (hsize_t)mpi_size;
    block[1]  = dims2[1];
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = block[0];
    stride[1] = block[1];
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;
    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Read entire file buffer and verify */
    verify_start[0] = start[0] * block[1];
    verify_block[0] = (block[0] * block[1]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&fbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    return;

} /* test_selection_io_types_1d_2d() */

/*
 * Perform the following tests for 2 dimensional spaces:
 * --Test 1: Simple 2D contiguous I/O for both file and memory spaces
 * --Test 2: Strided (memory) <> Contiguous(file) 2D I/O
 * --Reset selections
 * --Test 3: Contiguous (memory) <> Strided (file) 2D I/O
 * --Reset selections
 * --Test 4: Strided (memory) <> Strided (file) 2D I/O
 * --Reset selections
 */
static void
test_selection_io_types_2d(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl, H5FD_mem_t type,
                           haddr_t addrs[], size_t element_sizes[], hid_t mem_spaces[], hid_t file_spaces[],
                           hsize_t dims2[])
{
    hsize_t start[2];                 /* start for hyperslab          */
    hsize_t stride[2];                /* stride for hyperslab         */
    hsize_t count[2];                 /* count for hyperslab          */
    hsize_t block[2];                 /* block for hyperslab          */
    hsize_t verify_start[2] = {0, 0}; /* Starting block for verified data */
    hsize_t verify_block[2] = {0, 0}; /* Block size for verified data */
    int     i;
    int     j;
    int     i2;
    int     j2;

    /*
     * Test 1: Simple 2D contiguous I/O
     */

    /* Contiguous selection in file and memory */
    block[0]  = dims2[0] / (hsize_t)mpi_size;
    block[1]  = dims2[1];
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = block[0];
    stride[1] = block[1];
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;
    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[1], &file_spaces[1], &addrs[1], element_sizes,
                                (int **)&wbufs[1]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0; i < sel_dim0; i++)
        for (j = 0; j < sel_dim1; j++)
            fbuf2[(i * sel_dim1) + j] = wbuf2[(i * sel_dim1) + j];

    /* Read and verify */
    verify_start[0] = start[0] * block[1];
    verify_block[0] = (block[0] * block[1]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&fbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Test 2: Strided (memory) <> Contiguous(file) 2D I/O
     */
    /* Contiguous selection in file */
    count[0] = (hsize_t)((sel_dim0 / 2) / mpi_size);
    count[1] = (hsize_t)sel_dim1;
    start[0] = 1 + ((hsize_t)mpi_rank * count[0]);
    start[1] = 0;
    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        P_TEST_ERROR;

    /* Strided selection in memory */
    block[0]  = 1;
    block[1]  = 1;
    stride[0] = 2;
    stride[1] = 1;
    start[0]  = 1 + ((hsize_t)mpi_rank * stride[0] * count[0]);
    start[1]  = 0;
    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[1], &file_spaces[1], &addrs[1], element_sizes,
                                (int **)&wbufs[1]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0; i < sel_dim0 / 2; i++)
        for (j = 0; j < sel_dim1; j++) {
            fbuf2[((i + 1) * sel_dim1) + j] = wbuf2[(((2 * i) + 1) * sel_dim1) + j];
        }

    /* Update expected read buf */
    for (i = 0; i < sel_dim0; i++)
        for (j = 0; j < sel_dim1; j++)
            erbuf2[(i * sel_dim1) + j] = -1;
    for (i = 0; i < sel_dim0 / 2; i++)
        for (j = 0; j < sel_dim1; j++)
            erbuf2[(((2 * i) + 1) * sel_dim1) + j] = wbuf2[(((2 * i) + 1) * sel_dim1) + j];

    /* Read and verify */
    verify_start[0] = start[0] * count[1];
    verify_block[0] = (count[0] * count[1] * stride[0]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&erbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Reset selections
     */
    if (H5Sselect_all(mem_spaces[1]) < 0)
        P_TEST_ERROR;
    if (H5Sselect_all(file_spaces[1]) < 0)
        P_TEST_ERROR;

    block[0]  = dims2[0] / (hsize_t)mpi_size;
    block[1]  = dims2[1];
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = block[0];
    stride[1] = block[1];
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;
    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Read entire file buffer and verify */
    verify_start[0] = start[0] * block[1];
    verify_block[0] = (block[0] * block[1]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&fbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Test 3: Contiguous (memory) <> Strided (file) 2D I/O
     */

    /* Strided selection in file */
    block[0]  = 1;
    block[1]  = 1;
    count[0]  = (hsize_t)(sel_dim0 / mpi_size);
    count[1]  = (hsize_t)sel_dim1 / 2;
    stride[0] = 1;
    stride[1] = 2;
    start[0]  = (hsize_t)mpi_rank * count[0];
    start[1]  = 1;
    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Contiguous selection in memory */
    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[1], &file_spaces[1], &addrs[1], element_sizes,
                                (int **)&wbufs[1]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0; i < sel_dim0; i++)
        for (j = 0; j < sel_dim1 / 2; j++)
            fbuf2[i * sel_dim1 + (2 * j) + 1] = wbuf2[i * sel_dim1 + (j + 1)];

    /* Update expected read buf */
    for (i = 0; i < sel_dim0; i++)
        for (j = 0; j < sel_dim1; j++)
            erbuf2[i * sel_dim1 + j] = -1;
    for (i = 0; i < sel_dim0; i++)
        for (j = 0; j < sel_dim1 / 2; j++)
            erbuf2[i * sel_dim1 + (j + 1)] = wbuf2[i * sel_dim1 + (j + 1)];

    /* Read and verify */
    verify_start[0] = start[0] * count[1] * stride[1];
    verify_block[0] = (count[0] * count[1] * stride[1]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&erbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Reset selections
     */
    if (H5Sselect_all(mem_spaces[1]) < 0)
        P_TEST_ERROR;
    if (H5Sselect_all(file_spaces[1]) < 0)
        P_TEST_ERROR;

    block[0]  = dims2[0] / (hsize_t)mpi_size;
    block[1]  = dims2[1];
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = block[0];
    stride[1] = block[1];
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;
    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Read entire file buffer and verify */
    verify_start[0] = start[0] * block[1];
    verify_block[0] = (block[0] * block[1]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&fbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Test 4: Strided (memory) <> Strided (file) 2D I/O
     */
    /* sel_dim0 and sel_dim1 must be even */
    assert(sel_dim0 / 2 == (sel_dim0 + 1) / 2);
    assert(sel_dim1 / 2 == (sel_dim1 + 1) / 2);

    /* Strided selection (across dim 0) in file */
    block[0]  = 1;
    block[1]  = 1;
    count[0]  = (hsize_t)((sel_dim0 / 2) / mpi_size);
    count[1]  = (hsize_t)sel_dim1;
    stride[0] = 2;
    stride[1] = 1;
    start[0]  = 1 + ((hsize_t)mpi_rank * count[0] * stride[0]);
    start[1]  = 0;
    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Strided selection (across dim 1) in memory */
    block[0]  = 1;
    block[1]  = 1;
    count[0]  = (hsize_t)(sel_dim0 / mpi_size);
    count[1]  = (hsize_t)sel_dim1 / 2;
    stride[0] = 1;
    stride[1] = 2;
    start[0]  = (hsize_t)mpi_rank * count[0];
    start[1]  = 1;
    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[1], &file_spaces[1], &addrs[1], element_sizes,
                                (int **)&wbufs[1]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0, i2 = 1, j2 = 0; i < sel_dim0; i++)
        for (j = 1; j < sel_dim1; j += 2) {
            assert(i2 < sel_dim0);
            fbuf2[i2 * sel_dim1 + j2] = wbuf2[i * sel_dim1 + j];
            if (++j2 == sel_dim1) {
                i2 += 2;
                j2 = 0;
            }
        }

    /* Update expected read buf */
    for (i = 0; i < sel_dim0; i++)
        for (j = 0; j < sel_dim1; j++)
            erbuf2[i * sel_dim1 + j] = -1;
    for (i = 0; i < sel_dim0; i++)
        for (j = 1; j < sel_dim1; j += 2)
            erbuf2[i * sel_dim1 + j] = wbuf2[i * sel_dim1 + j];
    /* Read and verify */
    verify_start[0] = start[0] * count[1] * stride[1];
    verify_block[0] = (count[0] * count[1] * stride[1]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&erbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Reset selections
     */
    if (H5Sselect_all(file_spaces[1]) < 0)
        P_TEST_ERROR;
    if (H5Sselect_all(mem_spaces[1]) < 0)
        P_TEST_ERROR;

    block[0]  = dims2[0] / (hsize_t)mpi_size;
    block[1]  = dims2[1];
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = block[0];
    stride[1] = block[1];
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (H5Sselect_hyperslab(file_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;
    if (H5Sselect_hyperslab(mem_spaces[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Read entire file buffer and verify */
    verify_start[0] = start[0] * block[1];
    verify_block[0] = (block[0] * block[1]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[1],
                                      &file_spaces[1], &addrs[1], element_sizes, 1, (int **)&fbufs[1],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    return;

} /* test_selection_io_types_2d() */

/*
 * Perform the following tests for 1 dimensional spaces:
 * --Test 1: Simple 1D contiguous I/O in both file and memory spaces
 * --Test 2: Strided (memory) <> Contiguous (file) 1D I/O
 * --Reset selections
 * --Test 3: Contiguous (memory) <> Strided (file) 1D I/O
 * --Reset selections
 * --Test 4: Strided (memory) <> Strided 1D (file) I/O
 * --Reset selections
 */
static void
test_selection_io_types_1d(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl, H5FD_mem_t type,
                           haddr_t addrs[], size_t element_sizes[], hid_t mem_spaces[], hid_t file_spaces[],
                           hsize_t dims1[])
{
    hsize_t start[2];                 /* start for hyperslab          */
    hsize_t stride[2];                /* stride for hyperslab         */
    hsize_t count[2];                 /* count for hyperslab          */
    hsize_t block[2];                 /* block for hyperslab          */
    hsize_t verify_start[2] = {0, 0}; /* Starting block for verified data */
    hsize_t verify_block[2] = {0, 0}; /* Block size for verified data */
    int     i;

    /*
     * Test 1: Simple 1D contiguous I/O
     */

    /* Contiguous selection in file and memory */
    block[0]  = dims1[0] / (hsize_t)mpi_size;
    count[0]  = 1;
    stride[0] = block[0];
    start[0]  = (hsize_t)mpi_rank * block[0];

    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[0], &file_spaces[0], &addrs[0], element_sizes,
                                (int **)&wbufs[0]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0; i < sel_dim0 * sel_dim1; i++)
        fbuf1[i] = wbuf1[i];

    /* Read and verify */
    verify_start[0] = start[0];
    verify_block[0] = block[0];

    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&fbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Test 2: Strided (memory) <> Contiguous (file) 1D I/O
     */
    /* sel_dim1 must be even */
    assert(sel_dim1 / 2 == (sel_dim1 + 1) / 2);

    /* Contiguous selection in file */
    count[0] = (hsize_t)(((sel_dim0 * sel_dim1) / 2) / mpi_size);
    start[0] = 1 + ((hsize_t)mpi_rank * count[0]);
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        P_TEST_ERROR;

    /* Strided selection in memory */
    block[0]  = 1;
    stride[0] = 2;
    start[0]  = 1 + ((hsize_t)mpi_rank * stride[0] * count[0]);
    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[0], &file_spaces[0], &addrs[0], element_sizes,
                                (int **)&wbufs[0]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0; i < (sel_dim0 * sel_dim1) / 2; i++)
        fbuf1[i + 1] = wbuf1[(2 * i) + 1];

    /* Update expected read buf */
    for (i = 0; i < (sel_dim0 * sel_dim1); i++)
        erbuf1[i] = -1;
    for (i = 0; i < (sel_dim0 * sel_dim1) / 2; i++)
        erbuf1[(2 * i) + 1] = wbuf1[(2 * i) + 1];

    /* Read and verify */
    verify_start[0] = start[0];
    verify_block[0] = (count[0] * stride[0]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&erbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Reset selections
     */
    if (H5Sselect_all(mem_spaces[0]) < 0)
        P_TEST_ERROR;
    if (H5Sselect_all(file_spaces[0]) < 0)
        P_TEST_ERROR;

    block[0]  = dims1[0] / (hsize_t)mpi_size;
    count[0]  = 1;
    stride[0] = block[0];
    start[0]  = (hsize_t)mpi_rank * block[0];

    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Read entire file buffer and verify */
    verify_start[0] = start[0];
    verify_block[0] = block[0];
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&fbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Test 3: Contiguous (memory) <> Strided (file) 1D I/O
     */
    /* sel_dim1 must be even */
    assert(sel_dim1 / 2 == (sel_dim1 + 1) / 2);

    /* Strided selection in file */
    block[0]  = 1;
    count[0]  = (hsize_t)(((sel_dim0 * sel_dim1) / 2) / mpi_size); /* count is this value from twice above */
    stride[0] = 2;                                                 /* stride is this value from twice above */
    start[0]  = 1 + ((hsize_t)mpi_rank * stride[0] * count[0]);
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Contiguous selection in memory */
    start[0] = 1 + ((hsize_t)mpi_rank * count[0]);
    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[0], &file_spaces[0], &addrs[0], element_sizes,
                                (int **)&wbufs[0]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0; i < (sel_dim0 * sel_dim1) / 2; i++)
        fbuf1[(2 * i) + 1] = wbuf1[i + 1];

    /* Update expected read buf */
    for (i = 0; i < (sel_dim0 * sel_dim1); i++)
        erbuf1[i] = -1;
    for (i = 0; i < (sel_dim0 * sel_dim1) / 2; i++)
        erbuf1[i + 1] = wbuf1[i + 1];

    /* Read and verify */
    verify_start[0] = start[0];
    verify_block[0] = count[0];
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&erbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Reset selections
     */
    if (H5Sselect_all(mem_spaces[0]) < 0)
        P_TEST_ERROR;
    if (H5Sselect_all(file_spaces[0]) < 0)
        P_TEST_ERROR;

    block[0]  = dims1[0] / (hsize_t)mpi_size;
    count[0]  = 1;
    stride[0] = block[0];
    start[0]  = (hsize_t)mpi_rank * block[0];

    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Read entire file buffer and verify */
    verify_start[0] = start[0];
    verify_block[0] = block[0];
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&fbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Test 4: Strided (memory) <> Strided 1D (file) I/O
     */
    /* sel_dim1 must be even */
    assert(sel_dim1 / 2 == (sel_dim1 + 1) / 2);

    /* Strided selection in file */
    block[0]  = 1;
    count[0]  = (hsize_t)(((sel_dim0 * sel_dim1) / 2) / mpi_size);
    stride[0] = 2;
    start[0]  = 0 + ((hsize_t)mpi_rank * stride[0] * count[0]);
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Strided selection in memory */
    start[0] = 1 + ((hsize_t)mpi_rank * stride[0] * count[0]);
    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Issue write call */
    if (test_selection_io_write(dxpl, lf, type, 1, &mem_spaces[0], &file_spaces[0], &addrs[0], element_sizes,
                                (int **)&wbufs[0]) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /* Update file buf */
    for (i = 0; i < (sel_dim0 * sel_dim1) / 2; i++)
        fbuf1[2 * i] = wbuf1[(2 * i) + 1];

    /* Update expected read buf */
    for (i = 0; i < (sel_dim0 * sel_dim1); i++)
        erbuf1[i] = -1;
    for (i = 0; i < (sel_dim0 * sel_dim1) / 2; i++)
        erbuf1[(2 * i) + 1] = wbuf1[(2 * i) + 1];

    /* Read and verify */
    verify_start[0] = start[0];
    verify_block[0] = (count[0] * stride[0]);
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&erbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    /*
     * Reset selections
     */
    if (H5Sselect_all(mem_spaces[0]) < 0)
        P_TEST_ERROR;
    if (H5Sselect_all(file_spaces[0]) < 0)
        P_TEST_ERROR;

    block[0]  = dims1[0] / (hsize_t)mpi_size;
    count[0]  = 1;
    stride[0] = block[0];
    start[0]  = (hsize_t)mpi_rank * block[0];

    if (H5Sselect_hyperslab(mem_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;
    if (H5Sselect_hyperslab(file_spaces[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Read entire file buffer and verify */
    verify_start[0] = start[0];
    verify_block[0] = block[0];
    if (test_selection_io_read_verify(dxpl, mpi_rank, verify_start, verify_block, lf, type, 1, &mem_spaces[0],
                                      &file_spaces[0], &addrs[0], element_sizes, 1, (int **)&fbufs[0],
                                      false) < 0)
        P_TEST_ERROR;

    MPI_Barrier(comm);

    return;

} /* test_selection_io_types_1d() */

/*
 *  Perform the following tests for selection I/O:
 *
 *  test_selection_io_types_1d():
 *      ---Selection I/O tests for 1 dimensional spaces
 *  test_selection_io_types_2d()
 *      ---Selection I/O tests for 2 dimensional spaces
 *  test_selection_io_types_1d_2d()
 *      ---Selection I/O tests for 1 & 2 dimensional spaces
 *  test_selection_io_types_shorten()
 *      --Selection I/O tests that use shortened arrays for wbuf and element sizes
 */
static void
test_selection_io_real(int mpi_rank, int mpi_size, H5FD_t *lf, hid_t dxpl)
{
    hid_t   mem_spaces[2]  = {H5I_INVALID_HID, H5I_INVALID_HID}; /* memory dataspaces vector */
    hid_t   file_spaces[2] = {H5I_INVALID_HID, H5I_INVALID_HID}; /* file dataspaces vector */
    hsize_t dims1[1];                                            /* 1d dimension sizes */
    hsize_t dims2[2];                                            /* 2d dimension sizes */

    H5FD_mem_t type;                                          /* File type */
    haddr_t    addrs[2];                                      /* File allocation address */
    size_t     element_sizes[2] = {sizeof(int), sizeof(int)}; /* Element size */
    size_t     bufsize;                                       /* Buffer size */
    int        i;
    int        j;

    curr_nerrors = nerrors;

    /*
     *  Default dimension sizes for mpi_size 1 or 2:
     *  int sel_dim0 = SELECT_IO_DIM0;
     *  int sel_dim1 = SELECT_IO_DIM1;
     */
    if (mpi_size >= 3) {
        sel_dim0 = mpi_size * 2;
        sel_dim1 = mpi_size * 4;
    }

    dims1[0] = (hsize_t)(sel_dim0 * sel_dim1);
    dims2[0] = (hsize_t)sel_dim0, dims2[1] = (hsize_t)sel_dim1;

    /* Create dataspaces - location 0 will be 1D and location 1 will be 2D */
    if ((mem_spaces[0] = H5Screate_simple(1, dims1, NULL)) < 0)
        P_TEST_ERROR;
    if ((mem_spaces[1] = H5Screate_simple(2, dims2, NULL)) < 0)
        P_TEST_ERROR;
    if ((file_spaces[0] = H5Screate_simple(1, dims1, NULL)) < 0)
        P_TEST_ERROR;
    if ((file_spaces[1] = H5Screate_simple(2, dims2, NULL)) < 0)
        P_TEST_ERROR;

    /* Initialize global buffers:
     * --wbuf1, wbuf2: write buffers
     * --fbuf1, fbuf1: expected file buffers
     * --erbuf1, erbuf2: expected read buffers
     */
    bufsize = (size_t)(sel_dim0 * sel_dim1) * sizeof(int);

    if ((wbuf1 = malloc(bufsize)) == NULL)
        P_TEST_ERROR;

    if ((wbuf2 = malloc(bufsize)) == NULL)
        P_TEST_ERROR;

    wbufs[0] = wbuf1;
    wbufs[1] = wbuf2;

    if ((fbuf1 = malloc(bufsize)) == NULL)
        P_TEST_ERROR;

    if ((fbuf2 = malloc(bufsize)) == NULL)
        P_TEST_ERROR;

    fbufs[0] = fbuf1;
    fbufs[1] = fbuf2;

    if ((erbuf1 = malloc(bufsize)) == NULL)
        P_TEST_ERROR;

    if ((erbuf2 = malloc(bufsize)) == NULL)
        P_TEST_ERROR;

    erbufs[0] = erbuf1;
    erbufs[1] = erbuf2;

    /* Initialize data */
    for (i = 0; i < sel_dim0; i++)
        for (j = 0; j < sel_dim1; j++) {
            wbuf1[(i * sel_dim1) + j] = (i * sel_dim1) + j;
            wbuf2[(i * sel_dim1) + j] = (i * sel_dim1) + j + (sel_dim0 * sel_dim1);
        }

    /* Loop over memory types */
    for (type = 1; type < H5FD_MEM_NTYPES; type++) {

        addrs[0] = H5FDalloc(lf, type, H5P_DEFAULT, (sizeof(int) * (hsize_t)sel_dim0 * (hsize_t)sel_dim1));
        addrs[1] = H5FDalloc(lf, type, H5P_DEFAULT, (sizeof(int) * (hsize_t)sel_dim0 * (hsize_t)sel_dim1));

        test_selection_io_types_1d(mpi_rank, mpi_size, lf, dxpl, type, addrs, element_sizes, mem_spaces,
                                   file_spaces, dims1);
        test_selection_io_types_2d(mpi_rank, mpi_size, lf, dxpl, type, addrs, element_sizes, mem_spaces,
                                   file_spaces, dims2);
        test_selection_io_types_1d_2d(mpi_rank, mpi_size, lf, dxpl, type, addrs, element_sizes, mem_spaces,
                                      file_spaces, dims1, dims2);
        test_selection_io_types_shorten(mpi_rank, mpi_size, lf, dxpl, type, addrs, element_sizes, mem_spaces,
                                        file_spaces, dims1, dims2);

    } /* end for */

    /* Close dataspaces */
    for (i = 0; i < 2; i++) {
        if (H5Sclose(mem_spaces[i]) < 0)
            P_TEST_ERROR;
        if (H5Sclose(file_spaces[i]) < 0)
            P_TEST_ERROR;
    }

    /* Free the buffers */
    if (wbuf1)
        free(wbuf1);
    if (wbuf2)
        free(wbuf2);
    if (fbuf1)
        free(fbuf1);
    if (fbuf2)
        free(fbuf2);
    if (erbuf1)
        free(erbuf1);
    if (erbuf2)
        free(erbuf2);

    CHECK_PASSED();

    return;

} /* test_selection_io_real() */

/*
 * These tests for selection I/O are derived from test_selection_io() in
 * test/vfd.c and modified for parallel testing.
 */
static void
test_selection_io(int mpi_rank, int mpi_size)
{
    H5FD_t  *lf   = NULL;            /* VFD struct ptr */
    hid_t    fapl = H5I_INVALID_HID; /* File access property list */
    char     filename[1024];         /* Test file name */
    unsigned flags = 0;              /* File access flags */

    unsigned collective;                      /* Types of I/O for testing */
    hid_t    dxpl          = H5I_INVALID_HID; /* Dataset transfer property list */
    hid_t    def_dxpl      = H5I_INVALID_HID; /* dxpl: independent access */
    hid_t    col_xfer_dxpl = H5I_INVALID_HID; /* dxpl: collective access with collective I/O */
    hid_t    ind_io_dxpl   = H5I_INVALID_HID; /* dxpl: collective access with individual I/O */

    /* If I use fapl in this call, I got an environment printout */
    h5_fixname(SELECT_FNAME, H5P_DEFAULT, filename, sizeof(filename));

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        P_TEST_ERROR;

    if (H5Pset_fapl_mpio(fapl, comm, info) < 0)
        P_TEST_ERROR;

    /* Create file */
    flags = H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC;

    if (NULL == (lf = H5FDopen(filename, flags, fapl, HADDR_UNDEF)))
        P_TEST_ERROR;

    /* Default dxpl which will be H5FD_MPIO_INDEPENDENT by default */
    def_dxpl = H5Pcreate(H5P_DATASET_XFER);

    /* Set dxpl for collective access which will have H5FD_MPIO_COLLECTIVE_IO as default */
    if ((col_xfer_dxpl = H5Pcopy(def_dxpl)) < 0)
        P_TEST_ERROR;
    if (H5Pset_dxpl_mpio(col_xfer_dxpl, H5FD_MPIO_COLLECTIVE) < 0)
        P_TEST_ERROR;

    /* Set dxpl for H5FD_MPIO_INDIVIDUAL_IO */
    if ((ind_io_dxpl = H5Pcopy(col_xfer_dxpl)) < 0)
        P_TEST_ERROR;
    if (H5Pset_dxpl_mpio_collective_opt(ind_io_dxpl, H5FD_MPIO_INDIVIDUAL_IO) < 0)
        P_TEST_ERROR;

    for (collective = 0; collective < iotypes; collective++) {
        // for (collective = 0; collective < 1; collective++) {
        if (collective)
            dxpl = collective == 1 ? col_xfer_dxpl : ind_io_dxpl;
        else
            dxpl = def_dxpl;

        if (MAINPROCESS) {
            if (collective) {
                if (collective == 1)
                    printf("     Testing with Collective access: collective I/O         ");
                else
                    printf("     Testing with Collective_access: Individual I/O         ");
            }
            else
                printf("     Testing with Independent access                        ");
        }

        /* Perform the actual tests */
        test_selection_io_real(mpi_rank, mpi_size, lf, dxpl);
    }

    /* Close file */
    if (H5FDclose(lf) < 0)
        P_TEST_ERROR;

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        P_TEST_ERROR;

    if (H5Pclose(def_dxpl) < 0)
        P_TEST_ERROR;
    if (H5Pclose(col_xfer_dxpl) < 0)
        P_TEST_ERROR;
    if (H5Pclose(ind_io_dxpl) < 0)
        P_TEST_ERROR;

    // if (MAINPROCESS && HDremove(filename) < 0)
    //    P_TEST_ERROR;

} /* test_selection_io() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run parallel VFD tests.
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 *-------------------------------------------------------------------------
 */

int
main(int argc, char **argv)
{

#ifdef H5_HAVE_SUBFILING_VFD
    int required = MPI_THREAD_MULTIPLE;
    int provided = 0;
#endif
    int mpi_size;
    int mpi_rank = 0;
    int ret;

#ifdef H5_HAVE_SUBFILING_VFD
    if (MPI_SUCCESS != MPI_Init_thread(&argc, &argv, required, &provided)) {
        printf("    MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE. Exiting\n");
        goto finish;
    }

    if (provided != required) {
        printf("    MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE. Exiting\n");
        goto finish;
    }
#else
    if (MPI_SUCCESS != MPI_Init(&argc, &argv)) {
        printf("    MPI_Init failed. Exiting\n");
        goto finish;
    }
#endif

    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    /* Attempt to turn off atexit post processing so that in case errors
     * occur during the test and the process is aborted, it will not hang
     * in the atexit post processing.  If it does, it may try to make MPI
     * calls which may not work.
     */
    if (H5dont_atexit() < 0)
        printf("%d:Failed to turn off atexit processing. Continue.\n", mpi_rank);

    H5open();

    if (mpi_rank == 0) {
        printf("=========================================\n");
        printf("Parallel virtual file driver (VFD) tests\n");
        printf("        mpi_size     = %d\n", mpi_size);
        printf("=========================================\n");
    }

    MPI_Barrier(comm);

    if (mpi_rank == 0)
        printf("\n --- TESTING MPIO VFD: selection I/O --- \n");

    test_selection_io(mpi_rank, mpi_size);

    if (mpi_rank == 0)
        printf("\n --- TESTING MPIO VFD: vector I/O --- \n");

    if (mpi_size < 2) {
        if (mpi_rank == 0) {
            printf("     Need at least 2 processes to run tests for vector I/O.");
            SKIPPED();
        }
        printf("\n");
        goto finish;
    }

    test_vector_io(mpi_rank, mpi_size);

#ifdef H5_HAVE_SUBFILING_VFD

    if (mpi_rank == 0)
        printf("\n --- TESTING SUBFILING VFD: environment variables set to empty --- \n");

    HDsetenv("H5FD_SUBFILING_SUBFILE_PREFIX", "", 1);
    HDsetenv("H5FD_SUBFILING_IOC_SELECTION_CRITERIA", "", 1);
    HDsetenv("H5FD_SUBFILING_IOC_PER_NODE", "", 1);
    HDsetenv("H5FD_SUBFILING_STRIPE_SIZE", "", 1);
    HDsetenv("H5FD_SUBFILING_CONFIG_FILE_PREFIX", "", 1);

    MPI_Barrier(comm);

    if (mpi_rank == 0)
        printf("\n --- TESTING MPIO VFD: selection I/O --- \n");

    test_selection_io(mpi_rank, mpi_size);

    if (mpi_rank == 0)
        printf("\n --- TESTING MPIO VFD: vector I/O --- \n");

    if (mpi_size < 2) {
        if (mpi_rank == 0) {
            printf("     Need at least 2 processes to run tests for vector I/O.");
            SKIPPED();
        }
        printf("\n");
        goto finish;
    }

    test_vector_io(mpi_rank, mpi_size);

    HDunsetenv("H5FD_SUBFILING_SUBFILE_PREFIX");
    HDunsetenv("H5FD_SUBFILING_IOC_SELECTION_CRITERIA");
    HDunsetenv("H5FD_SUBFILING_IOC_PER_NODE");
    HDunsetenv("H5FD_SUBFILING_STRIPE_SIZE");
    HDunsetenv("H5FD_SUBFILING_CONFIG_FILE_PREFIX");

#endif

finish:
    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(comm);

    /* Gather errors from all processes */
    MPI_Allreduce(&nerrors, &ret, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
    nerrors = ret;

    if (MAINPROCESS) {
        printf("\n===================================\n");
        if (nerrors)
            printf("***Parallel vfd tests detected %d errors***\n", nerrors);
        else
            printf("Parallel vfd tests finished with no errors\n");
        printf("===================================\n");
    }

    /* discard the file image buffers */
    free_file_images();

    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrs) because exit code is limited to 1byte */
    return (nerrors != 0);

} /* main() */
