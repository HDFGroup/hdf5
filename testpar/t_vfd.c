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

/* Programmer:  John Mainzer
 *
 *              This file is a catchall for parallel VFD tests.
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

hbool_t     pass               = TRUE; /* set to FALSE on error */
hbool_t     disp_failure_mssgs = TRUE; /* global force display of failure messages */
const char *failure_mssg       = NULL;

const char *FILENAMES[] = {"mpio_vfd_test_file_0",      /*0*/
                           "mpio_vfd_test_file_1",      /*1*/
                           "mpio_vfd_test_file_2",      /*2*/
                           "mpio_vfd_test_file_3",      /*3*/
                           "mpio_vfd_test_file_4",      /*4*/
                           "mpio_vfd_test_file_5",      /*5*/
                           "mpio_vfd_test_file_6",      /*6*/
                           "subfiling_vfd_test_file_0", /*7*/
                           "subfiling_vfd_test_file_1", /*8*/
                           "subfiling_vfd_test_file_2", /*9*/
                           "subfiling_vfd_test_file_3", /*10*/
                           "subfiling_vfd_test_file_4", /*11*/
                           "subfiling_vfd_test_file_5", /*12*/
                           "subfiling_vfd_test_file_6", /*13*/
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
 * Programmer:  John Mainzer
 *              3/25/26
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;

    pass = TRUE;

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* allocate the file image buffers */
    if (pass) {

        buf_len  = INTS_PER_RANK * mpi_size;
        buf_size = sizeof(int32_t) * (size_t)INTS_PER_RANK * (size_t)mpi_size;

        increasing_fi_buf = (int32_t *)HDmalloc(buf_size);
        decreasing_fi_buf = (int32_t *)HDmalloc(buf_size);
        negative_fi_buf   = (int32_t *)HDmalloc(buf_size);
        zero_fi_buf       = (int32_t *)HDmalloc(buf_size);
        read_fi_buf       = (int32_t *)HDmalloc(buf_size);

        if ((!increasing_fi_buf) || (!decreasing_fi_buf) || (!negative_fi_buf) || (!zero_fi_buf) ||
            (!read_fi_buf)) {

            pass         = FALSE;
            failure_mssg = "Can't allocate one or more file image buffers.";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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
 * Programmer:  John Mainzer
 *              1/25/17
 *
 * Modifications:
 *
 *        None.
 *
 *-------------------------------------------------------------------------
 */

static void
free_file_images(void)
{
    if (increasing_fi_buf) {

        HDfree(increasing_fi_buf);
        increasing_fi_buf = NULL;
    }

    if (decreasing_fi_buf) {

        HDfree(decreasing_fi_buf);
        decreasing_fi_buf = NULL;
    }

    if (negative_fi_buf) {

        HDfree(negative_fi_buf);
        negative_fi_buf = NULL;
    }

    if (zero_fi_buf) {

        HDfree(zero_fi_buf);
        zero_fi_buf = NULL;
    }

    if (read_fi_buf) {

        HDfree(read_fi_buf);
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
 * Programmer:  John Mainzer
 *              3/25/26
 *
 * Modifications:
 *
 *              Updated for subfiling VFD                  9/29/30
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    unsigned    flags         = 0;    /* file open flags              */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */

    HDassert(vfd_name);
    HDassert(lf_ptr);
    HDassert(fapl_id_ptr);
    HDassert(dxpl_id_ptr);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the file name -- do this now, since setting up the ioc faple requires it.  This will probably
     * change */
    if (pass) {

        if (h5_fixname(FILENAMES[file_name_id], H5P_DEFAULT, filename, sizeof(filename)) == NULL) {

            pass         = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setupf fapl for target VFD */
    if (pass) {

        if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0) {

            pass         = FALSE;
            failure_mssg = "Can't create fapl.";
        }
    }

    if (pass) {

        if (HDstrcmp(vfd_name, "mpio") == 0) {

            if (H5Pset_fapl_mpio(fapl_id, comm, info) < 0) {

                pass         = FALSE;
                failure_mssg = "Can't set mpio fapl.";
            }
        }
#ifdef H5_HAVE_SUBFILING_VFD
        else if (HDstrcmp(vfd_name, H5FD_SUBFILING_NAME) == 0) {

            H5FD_subfiling_params_t shared_conf = {
                /* ioc_selection = */ SELECT_IOC_ONE_PER_NODE,
                /* stripe_size   = */ (INTS_PER_RANK / 2),
                /* stripe_count  = */ 0, /* will over write */
            };
            H5FD_subfiling_config_t subfiling_conf = {
                /* magic         = */ H5FD_SUBFILING_FAPL_MAGIC,
                /* version       = */ H5FD_SUBFILING_CURR_FAPL_VERSION,
                /* ioc_fapl_id   = */ H5P_DEFAULT, /* will over write? */
                /* require_ioc   = */ TRUE,
                /* shared_cfg    = */ shared_conf,
            };
            H5FD_ioc_config_t ioc_config = {
                /* magic            = */ H5FD_IOC_FAPL_MAGIC,
                /* version          = */ H5FD_IOC_CURR_FAPL_VERSION,
                /* thread_pool_size = */ H5FD_IOC_DEFAULT_THREAD_POOL_SIZE,
            };
            hid_t ioc_fapl = H5I_INVALID_HID;

            if ((pass) && ((ioc_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)) {

                pass         = FALSE;
                failure_mssg = "Can't create ioc fapl.";
            }

            /* set the MPI communicator and info in the FAPL */
            if (H5Pset_mpi_params(ioc_fapl, comm, info) < 0) {

                pass         = FALSE;
                failure_mssg = "Can't set MPI communicator and info in IOC fapl.";
            }

            /* set the MPI communicator and info in the FAPL */
            if (H5Pset_mpi_params(fapl_id, comm, info) < 0) {

                pass         = FALSE;
                failure_mssg = "Can't set MPI communicator and info in subfiling fapl.";
            }

            HDmemset(&ioc_config, 0, sizeof(ioc_config));
            HDmemset(&subfiling_conf, 0, sizeof(subfiling_conf));

            /* Get subfiling VFD defaults */
            if ((pass) && (H5Pget_fapl_subfiling(fapl_id, &subfiling_conf) == FAIL)) {

                pass         = FALSE;
                failure_mssg = "Can't get sub-filing VFD defaults.";
            }

            if ((pass) && (subfiling_conf.require_ioc)) {

                /* Get IOC VFD defaults */
                if ((pass) && ((H5Pget_fapl_ioc(ioc_fapl, &ioc_config) == FAIL))) {

                    pass         = FALSE;
                    failure_mssg = "Can't get IOC VFD defaults.";
                }

                /* Now we can set the IOC fapl. */
                if ((pass) && ((H5Pset_fapl_ioc(ioc_fapl, &ioc_config) == FAIL))) {

                    pass         = FALSE;
                    failure_mssg = "Can't set IOC fapl.";
                }
            }
            else {

                if ((pass) && ((H5Pset_fapl_sec2(ioc_fapl) == FAIL))) {

                    pass         = FALSE;
                    failure_mssg = "Can't set sec2 fapl.";
                }
            }

            /* Assign the IOC fapl as the underlying VPD */
            subfiling_conf.ioc_fapl_id = ioc_fapl;

            /* Now we can set the SUBFILING fapl before returning. */
            if ((pass) && (H5Pset_fapl_subfiling(fapl_id, &subfiling_conf) == FAIL)) {

                pass         = FALSE;
                failure_mssg = "Can't set subfiling fapl.";
            }
        }
#endif
        else {
            pass         = FALSE;
            failure_mssg = "un-supported VFD";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the file name */
    if (pass) {

        if (h5_fixname(FILENAMES[file_name_id], H5P_DEFAULT, filename, sizeof(filename)) == NULL) {

            pass         = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Open the VFD test file with the specified VFD.  */

    if (pass) {

        flags = H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC;

        if (NULL == (lf = H5FDopen(filename, flags, fapl_id, HADDR_UNDEF))) {

            pass         = FALSE;
            failure_mssg = "H5FDopen() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* set eoa as specified */

    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        if (H5FDset_eoa(lf, H5FD_MEM_DEFAULT, eoa) < 0) {

            pass         = FALSE;
            failure_mssg = "H5FDset_eoa() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if (pass) { /* setup dxpl */

        dxpl_id = H5Pcreate(H5P_DATASET_XFER);

        if (dxpl_id < 0) {

            pass         = FALSE;
            failure_mssg = "H5Pcreate(H5P_DATASET_XFER) failed.";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if (pass) {

        if (H5Pset_dxpl_mpio(dxpl_id, xfer_mode) < 0) {

            pass         = FALSE;
            failure_mssg = "H5Pset_dxpl_mpio() failed.";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if (pass) {

        if (H5Pset_dxpl_mpio_collective_opt(dxpl_id, coll_opt_mode) < 0) {

            pass         = FALSE;
            failure_mssg = "H5Pset_dxpl_mpio() failed.";
        }
    }

    if (pass) { /* setup pointers with return values */

        HDstrncpy(file_name, filename, 512);
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
 * Programmer:  John Mainzer
 *              3/25/26
 *
 * Modifications:
 *
 *        None.
 *
 *-------------------------------------------------------------------------
 */

static void
takedown_vfd_test_file(int mpi_rank, char *filename, H5FD_t **lf_ptr, hid_t *fapl_id_ptr, hid_t *dxpl_id_ptr)
{
    const char *fcn_name      = "takedown_vfd_test_file()";
    int         cp            = 0;
    hbool_t     show_progress = FALSE;

    HDassert(lf_ptr);
    HDassert(fapl_id_ptr);
    HDassert(dxpl_id_ptr);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Close the test file if it is open, regardless of the value of pass.
     * This should let the test program shut down more cleanly.
     */

    if (*lf_ptr) {

        if (H5FDclose(*lf_ptr) < 0) {

            pass         = FALSE;
            failure_mssg = "H5FDclose() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On rank 0, delete the test file.
     */

    /* wait for everyone to close the file */
    MPI_Barrier(comm);

    if (pass) {

        if ((mpi_rank == 0) && (HDremove(filename) < 0)) {

            pass         = FALSE;
            failure_mssg = "HDremove() failed.\n";
        }
    }

    /* wait for the file delete to complete */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Close the fapl */
    if (H5Pclose(*fapl_id_ptr) < 0) {

        pass         = FALSE;
        failure_mssg = "can't close fapl.\n";
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Close the dxpl */
    if (H5Pclose(*dxpl_id_ptr) < 0) {

        pass         = FALSE;
        failure_mssg = "can't close dxpl.\n";
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/26/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    uint32_t    count;
    H5FD_mem_t  types[1];
    haddr_t     addrs[1];
    size_t      sizes[1];
    void       *bufs[1];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 1 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 1 -- %s / col op / ind I/O",
                       vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 1 -- %s / col op / col I/O",
                       vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire increasing_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)increasing_fi_buf) <
                0) {

                pass         = FALSE;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDread_vector() failed.\n";
        }

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            if ((i < mpi_rank * INTS_PER_RANK) || (i >= (mpi_rank + 1) * INTS_PER_RANK)) {

                if (read_fi_buf[i] != 0) {

                    pass         = FALSE;
                    failure_mssg = "Unexpected value in read_fi_buf (1).\n";
                    break;
                }
            }
            else {

                if (read_fi_buf[i] != increasing_fi_buf[i]) {

                    pass         = FALSE;
                    failure_mssg = "Unexpected value in read_fi_buf (2).\n";
                    break;
                }
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/26/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    uint32_t    count;
    H5FD_mem_t  types[1];
    haddr_t     addrs[1];
    size_t      sizes[1];
    void       *bufs[1];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 2 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 2 -- %s / col op / ind I/O",
                       vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 2 -- %s / col op / col I/O",
                       vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire decreasing_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)decreasing_fi_buf) <
                0) {

                pass         = FALSE;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, zero the read buffer. */
    if (pass) {

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            read_fi_buf[i] = 0;
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDread_vector() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDread_vector() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

                    pass         = FALSE;
                    failure_mssg = "Unexpected value in read_fi_buf (1).\n";
                    break;
                }
            }
            else {

                if (read_fi_buf[i] != decreasing_fi_buf[i]) {

                    pass         = FALSE;
                    failure_mssg = "Unexpected value in read_fi_buf (2).\n";
                    break;
                }
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 9) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 10) Close the test file and delete it (on rank 0 only).
     *     Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);
    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/26/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    uint32_t    count;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    void       *bufs[4];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 3 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 3 -- %s / col op / ind I/O",
                       vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 3 -- %s / col op / col I/O",
                       vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)negative_fi_buf) <
                0) {

                pass         = FALSE;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDread_vector() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) Verify that buf_0, buf_1, buf_2, and buf_3 contain
     *    the expected data.  Note that this will be different
     *    on even vs. odd ranks.
     */
    if (pass) {

        int base_index = mpi_rank * INTS_PER_RANK;

        for (i = 0; ((pass) && (i < INTS_PER_RANK / 4)); i++) {

            if (((mpi_rank % 2 == 0) && (buf_0[i] != negative_fi_buf[base_index + i])) ||
                ((mpi_rank % 2 == 1) && (buf_3[i] != negative_fi_buf[base_index + i]))) {

                pass         = FALSE;
                failure_mssg = "Unexpected value in buf (1).\n";
            }
        }

        base_index += INTS_PER_RANK / 4;

        for (i = 0; ((pass) && (i < INTS_PER_RANK / 4)); i++) {

            if (((mpi_rank % 2 == 0) && (buf_1[i] != negative_fi_buf[base_index + i])) ||
                ((mpi_rank % 2 == 1) && (buf_2[i] != negative_fi_buf[base_index + i]))) {

                pass         = FALSE;
                failure_mssg = "Unexpected value in buf (2).\n";
            }
        }

        base_index += INTS_PER_RANK / 4;

        for (i = 0; ((pass) && (i < INTS_PER_RANK / 4)); i++) {

            if (((mpi_rank % 2 == 0) && (buf_2[i] != negative_fi_buf[base_index + i])) ||
                ((mpi_rank % 2 == 1) && (buf_1[i] != negative_fi_buf[base_index + i]))) {

                pass         = FALSE;
                failure_mssg = "Unexpected value in buf (3).\n";
            }
        }

        base_index += INTS_PER_RANK / 4;

        for (i = 0; ((pass) && (i < INTS_PER_RANK / 4)); i++) {

            if (((mpi_rank % 2 == 0) && (buf_3[i] != negative_fi_buf[base_index + i])) ||
                ((mpi_rank % 2 == 1) && (buf_0[i] != negative_fi_buf[base_index + i]))) {

                pass         = FALSE;
                failure_mssg = "Unexpected value in buf (4).\n";
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/26/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
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

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 4 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 4 -- %s / col op / ind I/O",
                       vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 4 -- %s / col op / col I/O",
                       vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)increasing_fi_buf) <
                0) {

                pass         = FALSE;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) Set all cells of read_fi_buf to zero. */
    if (pass) {

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            read_fi_buf[i] = 0;
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
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

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (1.1)";
                                    HDfprintf(stdout, "\nread_fi_buf[%d] = %d, increasing_fi_buf[%d] = %d\n",
                                              j, read_fi_buf[j], j, increasing_fi_buf[j]);
                                }
                            }
                            else if (((INTS_PER_RANK / 4) <= k) && (k < (3 * (INTS_PER_RANK / 8)))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (1.2)";
                                }
                            }
                            else if (((INTS_PER_RANK / 16) <= k) && (k < (INTS_PER_RANK / 8))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (1.3)";
                                }
                            }
                            else {

                                if (read_fi_buf[j] != 0) {

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (1.4)";
                                }
                            }
                            break;

                        case 1:
                            if ((1 <= k) && (k <= ((INTS_PER_RANK / 2) - 2))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (2.1)";
                                }
                            }
                            else if ((((INTS_PER_RANK / 2) + 1) <= k) && (k <= (INTS_PER_RANK - 2))) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (2.2)";
                                }
                            }
                            else {

                                if (read_fi_buf[j] != 0) {

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (2.3)";
                                }
                            }
                            break;

                        case 2:
                            if (k == INTS_PER_RANK / 2) {

                                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (3.1)";
                                }
                            }
                            else {

                                if (read_fi_buf[j] != 0) {

                                    pass         = FALSE;
                                    failure_mssg = "unexpected data read from file (3.2)";
                                }
                            }
                            break;

                        case 3:
                            if (read_fi_buf[j] != 0) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (4)";
                            }
                            break;

                        default:
                            HDassert(FALSE); /* should be un-reachable */
                            break;
                    }
                }
                else if (read_fi_buf[j] != 0) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (5)";
                }
            } /* end for loop */
        }     /* end for loop */
    }         /* end if */

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/26/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    int         base_index;
    uint32_t    count = 0;
    H5FD_mem_t  types[(INTS_PER_RANK / 16) + 1];
    haddr_t     addrs[(INTS_PER_RANK / 16) + 1];
    size_t      sizes[2];
    void       *bufs[(INTS_PER_RANK / 16) + 1];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 5 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 5 -- %s / col op / ind I/O",
                       vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title), "parallel vector read test 5 -- %s / col op / col I/O",
                       vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)increasing_fi_buf) <
                0) {

                pass         = FALSE;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) Set all cells of read_fi_buf to zero. */
    if (pass) {

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            read_fi_buf[i] = 0;
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDread_vector() failed (1).\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

                        pass         = FALSE;
                        failure_mssg = "unexpected data read from file (1)";
                    }
                }
                else if (read_fi_buf[j] != 0) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (2)";
                }
            } /* end for loop */
        }     /* end for loop */
    }         /* end if */

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();
            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/26/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    uint32_t    count;
    H5FD_mem_t  types[1];
    haddr_t     addrs[1];
    size_t      sizes[1];
    const void *bufs[1];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector write test 1 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 1 -- %s / col op / ind I/O", vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 1 -- %s / col op / col I/O", vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDwrite_vector() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf.  Report failure
     *    if any differences are detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = FALSE;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; i < mpi_size * INTS_PER_RANK; i++) {

            if (read_fi_buf[i] != increasing_fi_buf[i]) {

                pass         = FALSE;
                failure_mssg = "unexpected data read from file";
                break;
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/28/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    uint32_t    count;
    H5FD_mem_t  types[1];
    haddr_t     addrs[1];
    size_t      sizes[1];
    const void *bufs[1];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector write test 2 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 2 -- %s / col op / ind I/O", vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 2 -- %s / col op / col I/O", vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

                pass         = FALSE;
                failure_mssg = "H5FDwrite_vector() failed (1).\n";
            }
        }
        else { /* even ranks */

            if (H5FDwrite_vector(lf, dxpl_id, 0, NULL, NULL, NULL, NULL) < 0) {

                pass         = FALSE;
                failure_mssg = "H5FDwrite_vector() failed (2).\n";
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Write the even blocks of the negative_fi_buf to the file,
     *    with the even ranks writing the even blocks, and the odd
     *    ranks writing an empty vector.
     */
    if (pass) {

        if (mpi_rank % 2 == 1) { /* odd ranks */

            if (H5FDwrite_vector(lf, dxpl_id, 0, NULL, NULL, NULL, NULL) < 0) {

                pass         = FALSE;
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

                pass         = FALSE;
                failure_mssg = "H5FDwrite_vector() failed (4).\n";
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf.  Report failure
     *    if any differences are detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = FALSE;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            if (i % 2 == 1) { /* odd block */

                for (j = i * INTS_PER_RANK; ((pass) && (j < (i + 1) * INTS_PER_RANK)); j++) {

                    if (read_fi_buf[j] != increasing_fi_buf[j]) {

                        pass         = FALSE;
                        failure_mssg = "unexpected data read from file";
                        break;
                    }
                }
            }
            else { /* even block */

                for (j = i * INTS_PER_RANK; ((pass) && (j < (i + 1) * INTS_PER_RANK)); j++) {

                    if (read_fi_buf[j] != negative_fi_buf[j]) {

                        pass         = FALSE;
                        failure_mssg = "unexpected data read from file";
                        break;
                    }
                }
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/31/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    uint32_t    count;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    const void *bufs[4];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector write test 3 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 3 -- %s / col op / ind I/O", vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 3 -- %s / col op / col I/O", vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf,
     *    decreasing_fi_buf, negative_fi_buf, and zero_fi_buf as
     *    appropriate.  Report failure if any differences are
     *    detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = FALSE;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (1)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != decreasing_fi_buf[j]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (2)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != negative_fi_buf[j]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (3)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != zero_fi_buf[j]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (3)";
                    break;
                }
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/31/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    uint32_t    count;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    const void *bufs[4];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector write test 4 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 4 -- %s / col op / ind I/O", vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 4 -- %s / col op / col I/O", vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 4) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf,
     *    decreasing_fi_buf, negative_fi_buf, and zero_fi_buf as
     *    appropriate.  Report failure if any differences are
     *    detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = FALSE;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != zero_fi_buf[j]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (1)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != negative_fi_buf[j]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (2)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != decreasing_fi_buf[j]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (3)";
                    break;
                }
            }

            base_index += (INTS_PER_RANK / 4);

            for (j = base_index; j < base_index + (INTS_PER_RANK / 4); j++) {

                if (read_fi_buf[j] != increasing_fi_buf[j]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (3)";
                    break;
                }
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/31/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    int         k;
    uint32_t    count;
    H5FD_mem_t  types[4];
    haddr_t     addrs[4];
    size_t      sizes[4];
    const void *bufs[4];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector write test 5 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 5 -- %s / col op / ind I/O", vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 5 -- %s / col op / col I/O", vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDwrite_vector() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf,
     *    decreasing_fi_buf, negative_fi_buf, and zero_fi_buf as
     *    appropriate.  Report failure if any differences are
     *    detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = FALSE;
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

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (1.1)";

                                HDprintf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                         negative_fi_buf[j]);
                            }
                        }
                        else if (((INTS_PER_RANK / 4) <= k) && (k < (3 * (INTS_PER_RANK / 8)))) {

                            if (read_fi_buf[j] != decreasing_fi_buf[j]) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (1.2)";

                                HDprintf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                         decreasing_fi_buf[j]);
                            }
                        }
                        else if (((INTS_PER_RANK / 16) <= k) && (k < (INTS_PER_RANK / 8))) {

                            if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (1.3)";

                                HDprintf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                         increasing_fi_buf[j]);
                            }
                        }
                        else {

                            if (read_fi_buf[j] != 0) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (1.4)";
                            }
                        }
                        break;

                    case 1:
                        if ((1 <= k) && (k <= ((INTS_PER_RANK / 2) - 2))) {

                            if (read_fi_buf[j] != increasing_fi_buf[j]) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (2.1)";

                                HDprintf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                         increasing_fi_buf[j]);
                            }
                        }
                        else if ((((INTS_PER_RANK / 2) + 1) <= k) && (k <= (INTS_PER_RANK - 2))) {

                            if (read_fi_buf[j] != decreasing_fi_buf[j]) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (2.2)";

                                HDprintf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                         decreasing_fi_buf[j]);
                            }
                        }
                        else {

                            if (read_fi_buf[j] != 0) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (2.3)";
                            }
                        }
                        break;

                    case 2:
                        if (k == INTS_PER_RANK / 2) {

                            if (read_fi_buf[j] != negative_fi_buf[j]) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (3.1)";

                                HDprintf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                         negative_fi_buf[j]);
                            }
                        }
                        else {

                            if (read_fi_buf[j] != 0) {

                                pass         = FALSE;
                                failure_mssg = "unexpected data read from file (3.2)";
                            }
                        }
                        break;

                    case 3:
                        if (read_fi_buf[j] != 0) {

                            pass         = FALSE;
                            failure_mssg = "unexpected data read from file (4)";
                        }
                        break;

                    default:
                        HDassert(FALSE); /* should be un-reachable */
                        break;
                }
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              3/26/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         base_index;
    uint32_t    count = 0;
    H5FD_mem_t  types[(INTS_PER_RANK / 16) + 1];
    haddr_t     addrs[(INTS_PER_RANK / 16) + 1];
    size_t      sizes[2];
    const void *bufs[(INTS_PER_RANK / 16) + 1];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsnprintf(test_title, sizeof(test_title), "parallel vector write test 6 -- %s / independent",
                       vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 6 -- %s / col op / ind I/O", vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsnprintf(test_title, sizeof(test_title),
                       "parallel vector write test 6 -- %s / col op / col I/O", vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 2) Using rank zero, write the entire negative_fi_buf to
     *    the file.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (mpi_rank == 0) {

            if (H5FDwrite(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)zero_fi_buf) < 0) {

                pass         = FALSE;
                failure_mssg = "H5FDwrite() on rank 0 failed.\n";
            }
        }
    }

    /* 3) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On each rank, read the entire file into the read_fi_buf,
     *    and compare against zero_fi_buf, and increasing_fi_buf
     *    as appropriate.  Report failure if any differences are
     *    detected.
     */
    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = FALSE;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size * INTS_PER_RANK)); i++) {

            if (i % 16 == 0) {

                if (read_fi_buf[i] != increasing_fi_buf[i]) {

                    pass         = FALSE;
                    failure_mssg = "unexpected data read from file (1)";
                }
            }
            else if (read_fi_buf[i] != zero_fi_buf[i]) {

                pass         = FALSE;
                failure_mssg = "unexpected data read from file (2)";
            }
        }
    } /* end if */

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 8) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
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
 * Return:      FALSE on success, TRUE if any errors are detected.
 *
 * Programmer:  John Mainzer
 *              10/10/21
 *
 * Modifications:
 *
 *        None.
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
    hbool_t     show_progress = FALSE;
    hid_t       fapl_id       = -1;   /* file access property list ID */
    hid_t       dxpl_id       = -1;   /* data access property list ID */
    H5FD_t     *lf            = NULL; /* VFD struct ptr               */
    int         cp            = 0;
    int         i;
    int         j;
    int         k;
    uint32_t    count;
    H5FD_mem_t  types[8];
    haddr_t     addrs[8];
    size_t      sizes[8];
    const void *bufs[8];

    pass = TRUE;

    if (mpi_rank == 0) {

        if (xfer_mode == H5FD_MPIO_INDEPENDENT) {

            HDsprintf(test_title, "parallel vector write test 7 -- %s / independent", vfd_name);
        }
        else if (coll_opt_mode == H5FD_MPIO_INDIVIDUAL_IO) {

            HDsprintf(test_title, "parallel vector write test 7 -- %s / col op / ind I/O", vfd_name);
        }
        else {

            HDassert(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO);

            HDsprintf(test_title, "parallel vector write test 7 -- %s / col op / col I/O", vfd_name);
        }

        TESTING(test_title);
    }

    show_progress = ((show_progress) && (mpi_rank == 0));

    if (show_progress)
        HDfprintf(stdout, "\n%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 1) Open the test file with the specified VFD, set the eoa, and setup the dxpl */
    if (pass) {

        eoa = (haddr_t)mpi_size * (haddr_t)INTS_PER_RANK * (haddr_t)(sizeof(int32_t));

        setup_vfd_test_file(file_name_id, filename, mpi_size, xfer_mode, coll_opt_mode, vfd_name, eoa, &lf,
                            &fapl_id, &dxpl_id);
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDwrite_vector() failed.\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 3) Barrier
     */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

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

            pass         = FALSE;
            failure_mssg = "H5FDwrite_vector() failed (1).\n";
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 5) Barrier */
    MPI_Barrier(comm);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 6) On each rank, read the entire file into the read_fi_buf,
     *    and compare against increasing_fi_buf, and zero_fi_buf as
     *    appropriate.  Report failure if any differences are
     *    detected.
     */

    if (pass) {

        size_t image_size = (size_t)mpi_size * (size_t)INTS_PER_RANK * sizeof(int32_t);

        if (H5FDread(lf, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, image_size, (void *)read_fi_buf) < 0) {

            pass         = FALSE;
            failure_mssg = "H5FDread() failed.\n";
        }

        for (i = 0; ((pass) && (i < mpi_size)); i++) {

            base_index = i * INTS_PER_RANK;

            for (j = base_index; j < base_index + INTS_PER_RANK; j++) {

                k = j - base_index;

                if ((k % (INTS_PER_RANK / 8)) < (INTS_PER_RANK / 16)) {

                    if (read_fi_buf[j] != increasing_fi_buf[j]) {

                        pass         = FALSE;
                        failure_mssg = "unexpected data read from file (1)";

                        HDprintf("\nread_fi_buf[%d] = %d, %d expected.\n", j, read_fi_buf[j],
                                 increasing_fi_buf[j]);
                    }
                }
                else {

                    if (read_fi_buf[j] != 0) {

                        pass         = FALSE;
                        failure_mssg = "unexpected data read from file (2)";

                        HDprintf("\nread_fi_buf[%d] = %d, 0 expected.\n", j, read_fi_buf[j]);
                    }
                }
            }
        }
    }

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 7) Close the test file and delete it (on rank 0 only).
     *    Close FAPL and DXPL.
     */
    takedown_vfd_test_file(mpi_rank, filename, &lf, &fapl_id, &dxpl_id);

    if (show_progress)
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* report results */
    if (mpi_rank == 0) {

        if (pass) {

            PASSED();
        }
        else {

            H5_FAILED();

            if ((disp_failure_mssgs) || (show_progress)) {
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", fcn_name, failure_mssg);
            }
        }
    }

    return (!pass);

} /* vector_write_test_7() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run parallel VFD tests.
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 * Programmer:  John Mainzer
 *              3/2621/
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main(int argc, char **argv)
{
    unsigned nerrs = 0;
#ifdef H5_HAVE_SUBFILING_VFD
    int required = MPI_THREAD_MULTIPLE;
    int provided = 0;
#endif
    int mpi_size;
    int mpi_rank;

#ifdef H5_HAVE_SUBFILING_VFD
    if (MPI_SUCCESS != MPI_Init_thread(&argc, &argv, required, &provided)) {
        HDprintf("    MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE. Exiting\n");
        goto finish;
    }

    if (provided != required) {
        HDprintf("    MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE. Exiting\n");
        goto finish;
    }
#else
    if (MPI_SUCCESS != MPI_Init(&argc, &argv)) {
        HDprintf("    MPI_Init failed. Exiting\n");
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
        HDprintf("%d:Failed to turn off atexit processing. Continue.\n", mpi_rank);

    H5open();

    if (mpi_rank == 0) {
        HDprintf("=========================================\n");
        HDprintf("Parallel virtual file driver (VFD) tests\n");
        HDprintf("        mpi_size     = %d\n", mpi_size);
        HDprintf("=========================================\n");
    }

    if (mpi_size < 2) {
        if (mpi_rank == 0)
            HDprintf("    Need at least 2 processes.  Exiting.\n");
        goto finish;
    }

    alloc_and_init_file_images(mpi_size);

    if (!pass) {

        HDprintf("\nAllocation and initialize of file image buffers failed.  Test aborted.\n");
    }

    MPI_Barrier(comm);

    if (mpi_rank == 0) {

        HDprintf("\n\n --- TESTING MPIO VFD --- \n\n");
    }

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

    MPI_Barrier(comm);

#ifdef H5_HAVE_SUBFILING_VFD
    if (mpi_rank == 0) {

        HDprintf("\n\n --- TESTING SUBFILING VFD --- \n\n");
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

finish:
    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(comm);

    if (mpi_rank == 0) { /* only process 0 reports */
        HDprintf("===================================\n");
        if (nerrs > 0)
            HDprintf("***vfd tests detected %d failures***\n", nerrs);
        else
            HDprintf("vfd tests finished with no failures\n");
        HDprintf("===================================\n");
    }

    /* discard the file image buffers */
    free_file_images();

    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrs) because exit code is limited to 1byte */
    return (nerrs > 0);

} /* main() */
