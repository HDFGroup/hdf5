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

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Use Case 1.8 Appending a hyperslab of multiple chunks.
 * Description:
 *     Appending a hyperslab that spans several chunks of a dataset with
 *     unlimited dimensions within a pre-created file and reading the new
 *     data back.
 * Goal:
 *     Read data appended by the Writer to a pre-existing dataset in a
 *     file. The dataset has one or more unlimited dimensions. The data
 *     is appended by a hyperslab that is contained in several chunks (for
 *     example, appending 2-dim planes along the slowest changing dimension
 *     in the 3-dim dataset and each plane is covered by 4 chunks).
 * Level:
 *     User Level
 * Guarantees:
 *   o Readers will see the modified dimension sizes after the Writer
 *     finishes HDF5 metadata updates and issues H5Fflush or H5Oflush calls.
 *   o Readers will see newly appended data after the Writer finishes
 *     the flush operation.
 *
 * Preconditions:
 *   o Readers are not allowed to modify the file.
 *   o All datasets that are modified by the Writer exist when the
 *     Writer opens the file.
 *   o All datasets that are modified by the Writer exist when a Reader
 *     opens the file.
 *
 * Main Success Scenario:
 *  1. An application creates a file with required objects (groups,
 *     datasets, and attributes).
 *  2. The Writer opens the file and datasets in the file and starts
 *     adding data using H5Dwrite call with a hyperslab selection that
 *     spans several chunks.
 *  3. A Reader opens the file and a dataset in a file; if the size of
 *     the unlimited dimension has changed, reads the appended data back.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5test.h"

/* This test uses many POSIX things that are not available on
 * Windows.
 */
#if defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)

#include "use.h"

#define USE_APPEND_MCHUNKS_PROGNAME "use_append_mchunks"

static options_t UC_opts; /* Use Case Options */

/* Setup parameters for the use case.
 * Return: 0 succeed; -1 fail.
 */
int
setup_parameters(int argc, char *const argv[], options_t *opts)
{
    /* use case defaults */
    memset(opts, 0, sizeof(options_t));
    opts->chunksize   = Chunksize_DFT;
    opts->use_swmr    = 1; /* use swmr open */
    opts->iterations  = 1;
    opts->chunkplanes = 1;
    opts->progname    = USE_APPEND_MCHUNKS_PROGNAME;
    opts->fapl_id     = H5I_INVALID_HID;

    if (parse_option(argc, argv, opts) < 0) {
        return (-1);
    }

    opts->chunkdims[0] = (hsize_t)opts->chunkplanes;
    opts->chunkdims[1] = opts->chunkdims[2] = (hsize_t)opts->chunksize;

    opts->dims[0]     = 0;
    opts->max_dims[0] = H5S_UNLIMITED;
    opts->dims[1] = opts->dims[2] = opts->max_dims[1] = opts->max_dims[2] = 2 * (hsize_t)opts->chunksize;

    if (opts->nplanes == 0)
        opts->nplanes = 2 * (hsize_t)opts->chunksize;

    show_parameters(opts);
    return (0);
} /* end setup_parameters() */

/* Overall Algorithm:
 * Parse options from user;
 * Generate/pre-created test files needed and close it;
 * fork: child process becomes the reader process;
 *       while parent process continues as the writer process;
 * both run till ending conditions are met.
 */
int
main(int argc, char *argv[])
{
    pid_t childpid = 0;
    pid_t mypid, tmppid;
    int   child_status;
    int   child_wait_option = 0;
    int   ret_value         = 0;
    int   child_ret_value;
    bool  send_wait = 0;
    hid_t fapl      = H5I_INVALID_HID; /* File access property list */
    hid_t fid       = H5I_INVALID_HID; /* File ID */

    if (setup_parameters(argc, argv, &UC_opts) < 0) {
        Hgoto_error(1);
    }

    /* Determine the need to send/wait message file*/
    if (UC_opts.launch == UC_READWRITE) {
        HDunlink(WRITER_MESSAGE);
        send_wait = 1;
    }

    /* ==============================================================*/
    /* UC_READWRITE: create datafile, launch both reader and writer. */
    /* UC_WRITER:    create datafile, skip reader, launch writer.    */
    /* UC_READER:    skip create, launch reader, exit.               */
    /* ==============================================================*/
    /* =========== */
    /* Create file */
    /* =========== */
    if (UC_opts.launch != UC_READER) {
        printf("Creating skeleton data file for test...\n");
        if ((UC_opts.fapl_id = h5_fileaccess()) < 0) {
            fprintf(stderr, "can't create creation FAPL\n");
            Hgoto_error(1);
        }
        if (H5Pset_libver_bounds(UC_opts.fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
            fprintf(stderr, "can't set creation FAPL libver bounds\n");
            Hgoto_error(1);
        }
        if (create_uc_file(&UC_opts) < 0) {
            fprintf(stderr, "***encounter error\n");
            Hgoto_error(1);
        }
        else {
            printf("File created.\n");
        }
        /* Close FAPL to prevent issues with forking later */
        if (H5Pclose(UC_opts.fapl_id) < 0) {
            fprintf(stderr, "can't close creation FAPL\n");
            Hgoto_error(1);
        }
        UC_opts.fapl_id = H5I_INVALID_HID;
    }

    /* ============ */
    /* Fork process */
    /* ============ */
    if (UC_opts.launch == UC_READWRITE) {
        if ((childpid = fork()) < 0) {
            perror("fork");
            Hgoto_error(1);
        }
    }
    mypid = HDgetpid();

    /* ============= */
    /* launch reader */
    /* ============= */
    if (UC_opts.launch != UC_WRITER) {
        /* child process launch the reader */
        if (0 == childpid) {
            printf("%d: launch reader process\n", mypid);
            if ((UC_opts.fapl_id = h5_fileaccess()) < 0) {
                fprintf(stderr, "can't create read FAPL\n");
                exit(EXIT_FAILURE);
            }
            if (read_uc_file(send_wait, &UC_opts) < 0) {
                fprintf(stderr, "read_uc_file encountered error\n");
                exit(EXIT_FAILURE);
            }
            if (H5Pclose(UC_opts.fapl_id) < 0) {
                fprintf(stderr, "can't close read FAPL\n");
                exit(EXIT_FAILURE);
            }
            exit(EXIT_SUCCESS);
        }
    }

    /* ============= */
    /* launch writer */
    /* ============= */
    /* this process continues to launch the writer */
    printf("%d: continue as the writer process\n", mypid);

    /* Set the file access property list */
    if ((fapl = h5_fileaccess()) < 0) {
        fprintf(stderr, "can't get write FAPL\n");
        Hgoto_error(1);
    }

    if (UC_opts.use_swmr) {
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
            fprintf(stderr, "can't set write FAPL libver bounds\n");
            Hgoto_error(1);
        }
    }

    if ((fid = H5Fopen(UC_opts.filename, H5F_ACC_RDWR | (UC_opts.use_swmr ? H5F_ACC_SWMR_WRITE : 0), fapl)) <
        0) {
        fprintf(stderr, "H5Fopen failed\n");
        Hgoto_error(1);
    }

    if (write_uc_file(send_wait, fid, &UC_opts) < 0) {
        fprintf(stderr, "write_uc_file encountered error\n");
        Hgoto_error(1);
    }

    if (H5Fclose(fid) < 0) {
        fprintf(stderr, "Failed to close file id\n");
        Hgoto_error(1);
    }

    if (H5Pclose(fapl) < 0) {
        fprintf(stderr, "can't close write FAPL\n");
        Hgoto_error(1);
    }

    /* ================================================ */
    /* If readwrite, collect exit code of child process */
    /* ================================================ */
    if (UC_opts.launch == UC_READWRITE) {
        if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0) {
            perror("waitpid");
            Hgoto_error(1);
        }

        if (WIFEXITED(child_status)) {
            if ((child_ret_value = WEXITSTATUS(child_status)) != 0) {
                printf("%d: child process exited with non-zero code (%d)\n", mypid, child_ret_value);
                Hgoto_error(1);
            }
        }
        else {
            printf("%d: child process terminated abnormally\n", mypid);
            Hgoto_error(2);
        }
    }

done:
    if (ret_value != 0) {
        printf("Error(s) encountered\n");
    }
    else {
        printf("All passed\n");
    }

    return (ret_value);
} /* end main() */

#else /* defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID) */

int
main(void)
{
    fprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID) */
