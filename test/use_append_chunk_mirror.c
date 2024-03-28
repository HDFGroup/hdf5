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

/* HACKED VERSION
 * Demonstrate SWMR with a mirrored file.
 *
 * Must be built with SERVER_IP as the IP address of the target system
 * with a running mirror server, and SERVER_PORT as the primary server port.
 *
 * In addition to the local file, 'shinano.h5' will be created on the remote
 * system, mirroring the local file. The file location will be local to
 * Server's/Writer's invocation directory.
 *
 * Template for demonstration purposes:
 *
 * # Launch mirror server on remote machine (in foreground to easily stop)
 * REMOTE(1)$ ./mirror_server /path/to/mirror_worker
 *
 * # Launch chunk writer with plenty of chunks.
 * LOCAL(1)$ ./use_append_chunk_mirror -l w -n 10000
 *
 * # Wait one second for files to be created.
 *
 * # Launch chunk readers on both files.
 * LOCAL(2)$ ./use_append_chunk_mirror -l r -n 10000
 * REMOTE(2)$ ./use_append_chunk_mirror -l r -n 10000 -f shinano.h5
 *
 * # Hard-stop the server.
 * REMOTE(1)$ ^C
 * # alt, softer shutdown using echo and nc
 * echo "SHUTDOWN" | nc localhost 3000
 */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Use Case 1.7 Appending a single chunk
 * Description:
 *     Appending a single chunk of raw data to a dataset along an unlimited
 *     dimension within a pre-created file and reading the new data back.
 * Goal:
 *     Read data appended by the Writer to a pre-existing dataset in a
 *     file. The dataset has one or more unlimited dimensions. The data is
 *     appended by a hyperslab that is contained in one chunk (for example,
 *     appending 2-dim planes along the slowest changing dimension in the
 *     3-dim dataset).
 * Level:
 *     User Level
 * Guarantees:
 *     o Readers will see the modified dimension sizes after the Writer
 *       finishes HDF5 metadata updates and issues H5Fflush or H5Oflush calls.
 *     o Readers will see newly appended data after the Writer finishes
 *       the flush operation.
 *
 * Preconditions:
 *     o Readers are not allowed to modify the file.
 *     o All datasets that are modified by the Writer exist when the Writer
 *       opens the file.
 *     o All datasets that are modified by the Writer exist when a Reader
 *       opens the file.
 *     o Data is written by a hyperslab contained in one chunk.
 *
 * Main Success Scenario:
 *     1. An application creates a file with required objects (groups,
 *        datasets, and attributes).
 *     2. The Writer application opens the file and datasets in the file
 *        and starts adding data along the unlimited dimension using a hyperslab
 *        selection that corresponds to an HDF5 chunk.
 *     3. A Reader opens the file and a dataset in a file, and queries
 *        the sizes of the dataset; if the extent of the dataset has changed,
 *        reads the appended data back.
 *
 * Discussion points:
 *     1. Since the new data is written to the file, and metadata update
 *        operation of adding pointer to the newly written chunk is atomic and
 *        happens after the chunk is on the disk, only two things may happen
 *        to the Reader:
 *         o The Reader will not see new data.
 *         o The Reader will see all new data written by Writer.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "use.h"

/* This test uses many POSIX things that are not available on
 * Windows.
 */
#ifdef H5_HAVE_UNISTD_H

#ifdef H5_HAVE_MIRROR_VFD

#define THIS_PROGNAME "use_append_chunk_mirror"

#define CONNECT_WITH_JELLY 0

#if CONNECT_WITH_JELLY
#define SERVER_IP "10.10.10.248" /* hard-coded IP address */
#else
#define SERVER_IP "127.0.0.1"         /* localhost */
#endif                                /* CONNECT_WITH_JELLY */
#define SERVER_PORT      3000         /* hard-coded port number */
#define MIRROR_FILE_NAME "shinano.h5" /* hard-coded duplicate/mirror filename */

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
    opts->use_swmr    = true;
    opts->iterations  = 1;
    opts->chunkplanes = 1;
    opts->progname    = THIS_PROGNAME;

    if (parse_option(argc, argv, opts) < 0)
        return (-1);

    opts->chunkdims[0] = opts->chunkplanes;
    opts->chunkdims[1] = opts->chunkdims[2] = opts->chunksize;

    opts->dims[0]     = 0;
    opts->max_dims[0] = H5S_UNLIMITED;
    opts->dims[1] = opts->dims[2] = opts->max_dims[1] = opts->max_dims[2] = opts->chunksize;

    if (opts->nplanes == 0)
        opts->nplanes = (hsize_t)opts->chunksize;

    show_parameters(opts);
    return 0;
} /* setup_parameters() */

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
    pid_t                       childpid = 0;
    pid_t                       mypid, tmppid;
    int                         child_status;
    int                         child_wait_option = 0;
    int                         ret_value         = 0;
    int                         child_ret_value;
    bool                        send_wait = false;
    hid_t                       fid       = H5I_INVALID_HID;
    H5FD_mirror_fapl_t          mirr_fa;
    H5FD_splitter_vfd_config_t *split_fa     = NULL;
    hid_t                       mirr_fapl_id = H5I_INVALID_HID;

    if (setup_parameters(argc, argv, &UC_opts) < 0) {
        Hgoto_error(1);
    }

    mirr_fa.magic          = H5FD_MIRROR_FAPL_MAGIC;
    mirr_fa.version        = H5FD_MIRROR_CURR_FAPL_T_VERSION;
    mirr_fa.handshake_port = SERVER_PORT;
    strncpy(mirr_fa.remote_ip, SERVER_IP, H5FD_MIRROR_MAX_IP_LEN);

    if (NULL == (split_fa = calloc(1, sizeof(H5FD_splitter_vfd_config_t)))) {
        fprintf(stderr, "can't allocate memory for splitter config\n");
        Hgoto_error(1);
    }

    split_fa->wo_fapl_id       = H5I_INVALID_HID;
    split_fa->rw_fapl_id       = H5I_INVALID_HID;
    split_fa->magic            = H5FD_SPLITTER_MAGIC;
    split_fa->version          = H5FD_CURR_SPLITTER_VFD_CONFIG_VERSION;
    split_fa->log_file_path[0] = '\0'; /* none */
    split_fa->ignore_wo_errs   = false;
    strncpy(split_fa->wo_path, MIRROR_FILE_NAME, H5FD_SPLITTER_PATH_MAX);

    /* Determine the need to send/wait message file*/
    if (UC_opts.launch == UC_READWRITE) {
        HDunlink(WRITER_MESSAGE);
        send_wait = true;
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

        /* Prepare mirror child driver */
        mirr_fapl_id = H5Pcreate(H5P_FILE_ACCESS);
        if (mirr_fapl_id == H5I_INVALID_HID) {
            fprintf(stderr, "can't create creation mirror FAPL\n");
            Hgoto_error(1);
        }
        if (H5Pset_fapl_mirror(mirr_fapl_id, &mirr_fa) < 0) {
            fprintf(stderr, "can't set creation mirror FAPL\n");
            H5Eprint2(H5E_DEFAULT, stdout);
            Hgoto_error(1);
        }

        /* Prepare parent "splitter" driver in UC_opts */
        split_fa->wo_fapl_id = mirr_fapl_id;
        split_fa->rw_fapl_id = H5P_DEFAULT;
        UC_opts.fapl_id      = H5Pcreate(H5P_FILE_ACCESS);
        if (UC_opts.fapl_id == H5I_INVALID_HID) {
            fprintf(stderr, "can't create creation FAPL\n");
            Hgoto_error(1);
        }
        if (H5Pset_fapl_splitter(UC_opts.fapl_id, split_fa) < 0) {
            fprintf(stderr, "can't set creation FAPL\n");
            H5Eprint2(H5E_DEFAULT, stdout);
            Hgoto_error(1);
        }

        if (H5Pset_libver_bounds(UC_opts.fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
            fprintf(stderr, "can't set creation FAPL libver bounds\n");
            Hgoto_error(1);
        }

        /* Create file */
        if (create_uc_file(&UC_opts) < 0) {
            fprintf(stderr, "***encounter error\n");
            Hgoto_error(1);
        }
        else {
            printf("File created.\n");
        }

        /* Close FAPLs to prevent issues with forking later */
        if (H5Pclose(UC_opts.fapl_id) < 0) {
            fprintf(stderr, "can't close creation FAPL\n");
            Hgoto_error(1);
        }
        UC_opts.fapl_id = H5I_INVALID_HID;
        if (H5Pclose(mirr_fapl_id) < 0) {
            fprintf(stderr, "can't close creation mirror FAPL\n");
            Hgoto_error(1);
        }
        mirr_fapl_id = H5I_INVALID_HID;
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
        /* child process -- launch the reader */
        /* reader only opens the one file -- separate reader needed for mirrored file 'shinano.h5' */
        if (0 == childpid) {
            printf("%d: launch reader process\n", mypid);

            UC_opts.fapl_id = H5P_DEFAULT;
            if (read_uc_file(send_wait, &UC_opts) < 0) {
                fprintf(stderr, "read_uc_file encountered error (%d)\n", mypid);
                exit(1);
            }

            exit(0);
        }
    }

    /* ============= */
    /* launch writer */
    /* ============= */
    /* this process continues to launch the writer */
    printf("%d: continue as the writer process\n", mypid);

    /* Prepare mirror child driver */
    mirr_fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (mirr_fapl_id == H5I_INVALID_HID) {
        fprintf(stderr, "can't create creation mirror FAPL\n");
        Hgoto_error(1);
    }
    if (H5Pset_fapl_mirror(mirr_fapl_id, &mirr_fa) < 0) {
        fprintf(stderr, "can't set creation mirror FAPL\n");
        H5Eprint2(H5E_DEFAULT, stdout);
        Hgoto_error(1);
    }

    /* Prepare parent "splitter" driver in UC_opts */
    split_fa->wo_fapl_id = mirr_fapl_id;
    split_fa->rw_fapl_id = H5P_DEFAULT;
    UC_opts.fapl_id      = H5Pcreate(H5P_FILE_ACCESS);
    if (UC_opts.fapl_id == H5I_INVALID_HID) {
        fprintf(stderr, "can't create creation FAPL\n");
        Hgoto_error(1);
    }
    if (H5Pset_fapl_splitter(UC_opts.fapl_id, split_fa) < 0) {
        fprintf(stderr, "can't set creation FAPL\n");
        H5Eprint2(H5E_DEFAULT, stdout);
        Hgoto_error(1);
    }

    if (UC_opts.use_swmr) {
        if (H5Pset_libver_bounds(UC_opts.fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
            fprintf(stderr, "can't set write FAPL libver bounds\n");
            Hgoto_error(1);
        }
    }

    if ((fid = H5Fopen(UC_opts.filename, H5F_ACC_RDWR | (UC_opts.use_swmr ? H5F_ACC_SWMR_WRITE : 0),
                       UC_opts.fapl_id)) < 0) {
        fprintf(stderr, "H5Fopen failed\n");
        Hgoto_error(1);
    }

    if (write_uc_file(send_wait, fid, &UC_opts) < 0) {
        fprintf(stderr, "write_uc_file encountered error\n");
        Hgoto_error(1);
    }

    if (H5Fclose(fid) < 0) {
        fprintf(stderr, "Failed to close write\n");
        Hgoto_error(1);
    }

    if (H5Pclose(UC_opts.fapl_id) < 0) {
        fprintf(stderr, "can't close write FAPL\n");
        Hgoto_error(1);
    }

    if (H5Pclose(mirr_fapl_id) < 0) {
        fprintf(stderr, "can't close write mirror FAPL\n");
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
                Hgoto_error(2);
            }
        }
        else {
            printf("%d: child process terminated abnormally\n", mypid);
            Hgoto_error(2);
        }
    }

done:
    free(split_fa);

    if (ret_value != 0) {
        printf("Error(s) encountered\n");
    }
    else {
        printf("All passed\n");
    }

    return ret_value;
}

#else /* H5_HAVE_MIRROR_VFD */

int
main(void)
{
    fprintf(stderr, "Mirror VFD is not built. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_MIRROR_VFD */

#else /* H5_HAVE_UNISTD_H */

int
main(void)
{
    fprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_UNISTD_H */
