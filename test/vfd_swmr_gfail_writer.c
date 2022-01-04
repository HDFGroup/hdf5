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
 *  Purpose: To demostrate the four issues uncovered during the test plan
 *           1.4 part 1.
 *
 *           Setup:
 *           Writer:
 *            1) Create an HDF5 file
 *            2) Create many groups
 *            3) Use the named pipe to send the reader a message to start verifying
 *            4) Call H5Fvfd_swmr_end_tick immeidately after sending out the message to the reader
 *            5) Sleep for two ticks before receiving a message from the reader that informs the reader
 *               started verifying
 *            6) If the option to delete the group is off(by default), just sleep for a relatively long time,
 *                  then close the HDF5 file.
 *               Else delete the last 1000 groups that are just created if the total number of
 *                  created groups is greater than 1000.
 *                  Call H5Fvfd_swmr_end_tick sleep for a relatively long time,
 *                  then close HDF5 file.
 *          Reader:
 *            1) Open the HDF5 file
 *            2) Wait for the writer's message that informs the writer finished creating groups.
 *            3) After receiving the message from the writer, send a message back to the writer,
 *               then call H5Literate to iterate through all the groups. The callback function just checks if
 *               the group name's prefix is valid.
 *               An #if 0 #endif block can help the user easily tune to check the iterated group names.
 *            4) HDclock_gettime is used to check the total time of H5Literate call.
 *            5) Close the HDF5 file.
 *
 *          The number of groups, the tick length, the max lag, the page buffer size, the page size and the
 *          sleep duration on the writer side before closing the file are configurable.
 *          Users can also choose an option to delete 1000 groups after creating a larger number of groups.
 *          We only test to creat the groups with the latest file format. The option to create a
 *          group via the earliest file format is still there.
 *
 *          Issues and expected design fail
 *
 *          The parameter numbers that can reproduce the issues are tested at jelly.
 * To duplicate the issues at other machines, the number of groups to be created should be different.
 *
 * Issue 1: HDassert(oent->length == nent->length) error.
 * Need to UNCOMMENT out the HDassert(oent->length == nent->length) at ../src/H5Fvfd_swmr.c.
 * May also modify the group number a bit to see the assertion error.
 *
 * GROUP_n=340000
 *  ./vfd_swmr_gfail_writer -q -n $GROUP_n &
 *  ./vfd_swmr_gfail_reader -n $GROUP_n  &
 *
 * Issue 2: H5C__load_entry(): incorrect metadata checksum after all read attempts addr
 * Sometimes the expected
 * "Reader's API time exceeds max_lag of ticks, may increase the value of max_lag." may appear
 * You may need to modify the group number a bit to see the unexpected error message.
 *
 * GROUP_n=420000
 *  ./vfd_swmr_gfail_writer -q -n $GROUP_n &
 *  ./vfd_swmr_gfail_reader -n $GROUP_n  &
 *
 * Issue 3: Assertion `length == cache_ptr->page_size || page ......' failed
 * This failure occurs when page size and page buffer size change from 4K to 8K.
 * This issue seems to be always repeatable with the following number of groups.
 *
 * GROUP_n=320000
 *  ./vfd_swmr_gfail_writer -q -B 8192 -s 8192 -n $GROUP_n &
 *  ./vfd_swmr_gfail_reader -B 8192 -s 8192 -n $GROUP_n  &

 * Issue 4: not enough space to copy index
 * To duplicate this failure, the number of groups should be much larger, 2 millions.
 * The max_lag and tick_len should also be set to  big numbers.
 * This issue seems to be always repeatable with the following number of groups.
 *
 * GROUP_n=2000000
 *  ./vfd_swmr_gfail_writer -q -m 40 -t 10 -n $GROUP_n &
 *  ./vfd_swmr_gfail_reader -m 40 -t 10 -n $GROUP_n  &

 * Expected design fail
 *
 * Writer creates a large number of groups, then deletes the last 1000 groups,
 * With the following settings, the expected
 * "Reader's API time exceeds max_lag of ticks, may increase the value of max_lag."
 * should appear. If not, increases the GROUP_n.
 *
 * GROUP_n=320000
 *  ./vfd_swmr_gfail_writer -q -d -n $GROUP_n &
 *  ./vfd_swmr_gfail_reader -n $GROUP_n  &
 *
 * When increasing the max_lag, we may see the program run normally since
 * the reader can finish iterating all the groups within the max_lag of ticks.
 * The following program should end normally. If not, increase the max_lag.
 *  ./vfd_swmr_gfail_writer -m 9 -q -d -n $GROUP_n &
 *  ./vfd_swmr_gfail_reader -m 9 -n $GROUP_n  &

 */

#define H5F_FRIEND /*suppress error about including H5Fpkg   */

#include "hdf5.h"

#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define TIME_PASSED(X, Y)                                                                                    \
    ((double)((Y.tv_sec - X.tv_sec) * 1000000000 + (Y.tv_nsec - X.tv_nsec))) / 1000000000.0

typedef struct {
    hid_t        file, filetype, one_by_one_sid;
    char         filename[PATH_MAX];
    char         progname[PATH_MAX];
    unsigned int nsteps;
    bool         use_vfd_swmr;
    bool         old_style_grp;
    bool         use_named_pipes;
    uint32_t     w_sleep_len;
    uint32_t     max_lag;
    uint32_t     tick_len;
    unsigned int ps;
    unsigned int pbs;
    bool         del_grp;
    int          np_fd_w_to_r;
    int          np_fd_r_to_w;
    int          np_notify;
    int          np_verify;
} state_t;

#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .file = H5I_INVALID_HID, .one_by_one_sid = H5I_INVALID_HID, .filename = "",                          \
        .filetype = H5T_NATIVE_UINT32, .nsteps = 10000, .use_vfd_swmr = true, .old_style_grp = false,        \
        .use_named_pipes = true, .w_sleep_len = 112, .tick_len = 4, .max_lag = 7, .ps = 4096, .pbs = 4096,   \
        .del_grp = false, .np_fd_w_to_r = -1, .np_fd_r_to_w = -1, .np_notify = 0, .np_verify = 0             \
    }

/*
 * Operator function to be called by H5Literate.
 */
herr_t op_func(hid_t loc_id, const char *name, const H5L_info_t *info, void *operator_data);

static void
usage(const char *progname)
{
    HDfprintf(stderr,
              "usage: %s [-S] [-G] [-n number_of_groups] \n"
              "    [-N] [-d] [-q] [-T w_sleep_len] [-t tick_len] [-m max_lag][-B pbs] [-s ps]\n"
              "\n"
              "-S:             do not use VFD SWMR\n"
              "-G:             old-style type of group\n"
              "-n ngroups:     the number of groups\n"
              "-N:             do not use named pipes, \n"
              "                mainly for running the writer and reader seperately\n"
              "-t tick_len:    length of a tick in tenths of a second.\n"
              "-m max_lag:     maximum expected lag(in ticks) between writer and readers\n"
              "-B pbs:         page buffer size in bytes:\n"
              "                The default value is 4K(4096).\n"
              "-s ps:          page size used by page aggregation, page buffer and \n"
              "                the metadata file. The default value is 4K(4096).\n"
              "-T w_sleep_len: Before closing the file, the sleep length in tenths of a second \n"
              "                on the writer side. The default is 112 tenths of a second \n"
              "                That is 4*max_lag*tick_len if tick_len is 4 and max_lag is 7. \n"
              "-d del_grp:     true: delete 1000 groups after creating >1000 groups. \n"
              "-q:             silence printouts, few messages\n"
              "\n",
              progname);
    HDexit(EXIT_FAILURE);
}

static bool
state_init(state_t *s, int argc, char **argv)
{
    unsigned long tmp;
    int           ch;
    const hsize_t dims  = 1;
    char *        tfile = NULL;
    char *        end;

    *s = ALL_HID_INITIALIZER;

    if (H5_basename(argv[0], &tfile) < 0) {
        HDprintf("H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile) {
        HDfree(tfile);
        tfile = NULL;
    }

    while ((ch = getopt(argc, argv, "SGNn:T:t:m:B:s:dq")) != -1) {
        switch (ch) {
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'G':
                s->old_style_grp = true;
                break;
            case 'N':
                s->use_named_pipes = false;
                break;
            case 'd':
                s->del_grp = true;
                break;
            case 'n':
            case 'T':
            case 't':
            case 'm':
            case 'B':
            case 's':

                errno = 0;
                tmp   = HDstrtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    HDprintf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    HDprintf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    HDprintf("`-%c` argument `%lu` too large\n", ch, tmp);
                    TEST_ERROR;
                }

                if (ch == 'n')
                    s->nsteps = (unsigned)tmp;
                else if (ch == 'T')
                    s->w_sleep_len = (unsigned)tmp;
                else if (ch == 't')
                    s->tick_len = (unsigned)tmp;
                else if (ch == 'm')
                    s->max_lag = (unsigned)tmp;
                else if (ch == 'B')
                    s->pbs = (unsigned)tmp;
                else if (ch == 's')
                    s->ps = (unsigned)tmp;
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
    argc -= optind;
    argv += optind;

    if (argc > 0) {
        HDprintf("unexpected command-line arguments\n");
        TEST_ERROR;
    }

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        HDprintf("H5Screate_simple failed\n");
        TEST_ERROR;
    }

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_group.h5");

    return true;

error:
    if (tfile)
        HDfree(tfile);
    return false;
}

/* Named Pipe Subroutine: np_wr_send_receive
 * Description:
 *   The writer sends a message to the reader,
 *   then waits for max_lag ticks,
 *   then checks the returned message from the reader.
 * Return:
 *   True  if succeed
 *   False if an error occurs in any step above.
 *         An error is mostly caused by an unexpected
 *         notification number from the message sent
 *         by the reader.
 */
static bool
np_wr_send_receive(state_t *s)
{

    /* Bump up the value of notify to notice the reader to start to read */
    s->np_notify++;
    if (HDwrite(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0) {
        HDprintf("HDwrite failed\n");
        TEST_ERROR;
    }

    /* Call the end tick */
    if (H5Fvfd_swmr_end_tick(s->file) < 0)
        TEST_ERROR;

    /* Sleep for two ticks to wait for the reader's message */
    decisleep(2 * s->tick_len);

    /* Receive the same value from the reader and verify it before
     * going to the next step */
    (s->np_verify)++;
    if (HDread(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0) {
        HDprintf("HDread failed\n");
        TEST_ERROR;
    }

    if (s->np_notify == -1) {
        HDprintf("reader failed to verify group or attribute operation.\n");
        TEST_ERROR;
    }

    if (s->np_notify != s->np_verify) {
        HDprintf("received message %d, expecting %d\n", s->np_notify, s->np_verify);
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

/* Named Pipe Subroutine: np_rd_receive
 * Description:
 *   The reader receives a message from the writer,
 *   then checks if the notification number from
 *   the writer is expected.
 * Return:
 *   True  if succeed
 *   False if an error occurs in any step above.
 *         An error is mostly caused by an unexpected
 *         notification number from the message sent
 *         by the writer.
 */
static bool
np_rd_receive(state_t *s)
{

    /* The writer should have bumped up the value of notify.
     * Do the same with verify and confirm it */
    s->np_verify++;

    /* Receive the notify that the writer bumped up the value */
    if (HDread(s->np_fd_w_to_r, &(s->np_notify), sizeof(int)) < 0) {
        HDprintf("HDread failed\n");
        TEST_ERROR;
    }

    if (s->np_notify == -1) {
        HDprintf("writer failed to create group or carry out an attribute operation.\n");
        TEST_ERROR;
    }

    if (s->np_notify != s->np_verify) {
        HDprintf("received message %d, expecting %d\n", s->np_notify, s->np_verify);
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

/* Named Pipe Subroutine: np_rd_send
 * Description:
 *   The reader sends an acknowledgement to the writer
 * Return:
 *   True  if succeed
 *   False if an error occurs in sending the message.
 */
static bool
np_rd_send(state_t *s)
{

    if (HDwrite(s->np_fd_r_to_w, &(s->np_notify), sizeof(int)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDwrite failed\n");
        return false;
    }
    else
        return true;
}

/* Named Pipe Subroutine: np_send_error
 * Description:
 *   An error (notification number is 1) message is sent
 *   either from the reader or the writer.
 *   A boolean input parameter is used to choose
 *   either reader or writer.
 * Return:
 *     None
 */
static void
np_send_error(state_t *s, bool writer)
{
    s->np_notify = -1;
    if (writer)
        HDwrite(s->np_fd_w_to_r, &(s->np_notify), sizeof(int));
    else
        HDwrite(s->np_fd_r_to_w, &(s->np_notify), sizeof(int));
}

/*-------------------------------------------------------------------------
 * Function:    create_group
 *
 * Purpose:     Create a group and then close it.
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static bool
create_group(state_t *s, unsigned int which)
{
    char  name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;

    esnprintf(name, sizeof(name), "/group-%u", which);

    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDprintf("H5Gcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Gclose(g) < 0) {
        HDprintf("H5Gclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(g);
    }
    H5E_END_TRY;

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    delete_group
 *
 * Purpose:     Delete a group
 *
 * Parameters:  state_t *s
 *              The struct that stores information of HDF5 file, named pipe
 *              and some VFD SWMR configuration parameters
 *
 *              unsigned int which
 *              The number of iterations for group creation
 *
 *
 * Return:      Success:    true
 *              Failure:    false
 *
 * Note:        This is called by the main() function.
 *-------------------------------------------------------------------------
 */

static bool
delete_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];

    esnprintf(name, sizeof(name), "/group-%u", which);

    if (H5Ldelete(s->file, name, H5P_DEFAULT) < 0) {
        HDprintf("H5Ldelete failed\n");
        TEST_ERROR;
    }

    return true;

error:

    return false;
}

int
main(int argc, char **argv)
{
    hid_t                 fapl = H5I_INVALID_HID, fcpl = H5I_INVALID_HID;
    unsigned              step;
    bool                  writer = false;
    state_t               s;
    const char *          personality;
    H5F_vfd_swmr_config_t config;
    const char *          fifo_writer_to_reader = "./fifo_group_writer_to_reader";
    const char *          fifo_reader_to_writer = "./fifo_group_reader_to_writer";
    int                   fd_writer_to_reader = -1, fd_reader_to_writer = -1;
    int                   notify = 0, verify = 0;
    bool                  wg_ret = false;

    struct timespec start_time, end_time;
    double          temp_time;

    if (!state_init(&s, argc, argv)) {
        HDprintf("state_init failed\n");
        TEST_ERROR;
    }

    personality = HDstrstr(s.progname, "vfd_swmr_gfail_");

    if (personality != NULL && HDstrcmp(personality, "vfd_swmr_gfail_writer") == 0)
        writer = true;
    else if (personality != NULL && HDstrcmp(personality, "vfd_swmr_gfail_reader") == 0)
        writer = false;
    else {
        HDprintf("unknown personality, expected vfd_swmr_gfail_{reader,writer}\n");
        TEST_ERROR;
    }

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
     * flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(&config, s.tick_len, s.max_lag, writer, TRUE, FALSE, TRUE, 128, "./group-shadow",
                         NULL);

    /* If old-style option is chosen, use the earliest file format(H5F_LIBVER_EARLIEST)
     * as the second parameter of H5Pset_libver_bound() that is called by
     * vfd_swmr_create_fapl. Otherwise, the latest file format(H5F_LIBVER_LATEST)
     * should be used as the second parameter of H5Pset_libver_bound().
     * Also pass the use_vfd_swmr, only_meta_page, page_buf_size, config to vfd_swmr_create_fapl().*/
    if ((fapl = vfd_swmr_create_fapl(!s.old_style_grp, s.use_vfd_swmr, true, s.pbs, &config)) < 0) {
        HDprintf("vfd_swmr_create_fapl failed\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, s.ps)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        TEST_ERROR;
    }

    if (writer)
        s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl);
    else
        s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl);

    if (s.file < 0) {
        HDprintf("H5Fcreate/open failed\n");
        TEST_ERROR;
    }

    /* Use two named pipes(FIFO) to coordinate the writer and reader for
     * two-way communication.
     * One is for the writer to write to the reader.
     * The other one is for the reader to signal the writer.  */
    if (s.use_named_pipes && writer) {
        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(fifo_writer_to_reader, 0600) < 0) {
            HDprintf("HDmkfifo failed\n");
            TEST_ERROR;
        }

        if (HDmkfifo(fifo_reader_to_writer, 0600) < 0) {
            HDprintf("HDmkfifo failed\n");
            TEST_ERROR;
        }
    }

    /* Both the writer and reader open the pipes */
    if (s.use_named_pipes && (fd_writer_to_reader = HDopen(fifo_writer_to_reader, O_RDWR)) < 0) {
        HDprintf("HDopen failed\n");
        TEST_ERROR;
    }

    if (s.use_named_pipes && (fd_reader_to_writer = HDopen(fifo_reader_to_writer, O_RDWR)) < 0) {
        HDprintf("HDopen failed\n");
        TEST_ERROR;
    }

    /* Pass the named pipe information to the struct of state_t s, for attribute tests.*/
    if (s.use_named_pipes) {
        s.np_fd_w_to_r = fd_writer_to_reader;
        s.np_fd_r_to_w = fd_reader_to_writer;
        s.np_notify    = notify;
        s.np_verify    = verify;
    }

    if (writer) {

        for (step = 0; step < s.nsteps; step++) {
            dbgf(2, "writer: step %d\n", step);

            wg_ret = create_group(&s, step);
            if (wg_ret == false) {
                if (s.use_named_pipes)
                    np_send_error(&s, true);
                HDprintf("create groups failed\n");
                TEST_ERROR;
            }
        }
        if (s.use_named_pipes && np_wr_send_receive(&s) == false) {
            HDprintf("writer: write group - verification failed.\n");
            TEST_ERROR;
        }

        /* Delete 1000 groups if the del_grp option is true.  */
        if (s.del_grp && s.nsteps > 1000) {
            printf("Deleting groups. \n");
            for (step = s.nsteps - 1; step >= (s.nsteps - 1000); step--) {
                dbgf(2, "writer: deleting step %d\n", step);

                wg_ret = delete_group(&s, step);
                if (wg_ret == false) {
                    HDprintf("delete group_operations failed\n");
                    TEST_ERROR;
                }
            }
            /* end tick,may be not necessary. */
            if (H5Fvfd_swmr_end_tick(s.file) < 0)
                TEST_ERROR;
        }

        /* May be necessary for the writer to wait a longer time before
           closing the file.
           The default value is 112 tenths of a second(4*s.tick_len*s.max_lag)
           if tick_len is 4 and max_lag is 7.
        */
        decisleep(s.w_sleep_len);
    }
    else { /*Reader */
        if (s.use_named_pipes) {
            if (false == np_rd_receive(&s)) {
                TEST_ERROR;
            }

            dbgf(2, "reader receives the message.\n");
            if (np_rd_send(&s) == false) {
                TEST_ERROR;
            }
            dbgf(2, "reader sends the message.\n ");
        }

        if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {
            fprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }

        printf("Reader: call back function: check group names.\n");
        if (H5Literate(s.file, H5_INDEX_NAME, H5_ITER_NATIVE, NULL, op_func, NULL) < 0) {
            printf("H5Literate failed \n");
            TEST_ERROR;
        }
        if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {
            fprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }
        temp_time = TIME_PASSED(start_time, end_time);
        fprintf(stdout, "H5Literate: temp time                           = %lf\n", temp_time);
    }

    if (H5Pclose(fapl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fcpl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(s.one_by_one_sid) < 0) {
        HDprintf("H5Sclose failed\n");
        TEST_ERROR;
    }

    if (H5Fclose(s.file) < 0) {
        HDprintf("H5Fclose failed\n");
        TEST_ERROR;
    }

    /* Both the writer and reader close the named pipes */
    if (s.use_named_pipes && HDclose(fd_writer_to_reader) < 0) {
        HDprintf("HDclose failed\n");
        TEST_ERROR;
    }

    if (s.use_named_pipes && HDclose(fd_reader_to_writer) < 0) {
        HDprintf("HDclose failed\n");
        TEST_ERROR;
    }

    /* Reader finishes last and deletes the named pipes */
    if (s.use_named_pipes && !writer) {
        if (HDremove(fifo_writer_to_reader) != 0) {
            HDprintf("HDremove failed\n");
            TEST_ERROR;
        }

        if (HDremove(fifo_reader_to_writer) != 0) {
            HDprintf("HDremove failed\n");
            TEST_ERROR;
        }
    }

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Sclose(s.one_by_one_sid);
        H5Fclose(s.file);
    }
    H5E_END_TRY;

    if (s.use_named_pipes && fd_writer_to_reader >= 0)
        HDclose(fd_writer_to_reader);

    if (s.use_named_pipes && fd_reader_to_writer >= 0)
        HDclose(fd_reader_to_writer);

    if (s.use_named_pipes && !writer) {
        HDremove(fifo_writer_to_reader);
        HDremove(fifo_reader_to_writer);
    }

    return EXIT_FAILURE;
}
/************************************************************

  Operator function.  Prints the name and type of the object
  being examined.

 ************************************************************/
herr_t
op_func(hid_t loc_id, const char *name, const H5L_info_t *info, void *operator_data)
{

    /* avoid compiler warnings */
    (void)loc_id;
    (void)info;
    (void)operator_data;

#if 0 /* Kent for debugging purpose. */
    char *     subname;
    int        grp_num;
#endif

    if (strncmp(name, "group", (size_t)5) != 0) {
        printf("Iteration failed:  group name is %s\n", name);
        return -1;
    }
    else {
#if 0 /* Kent for debugging purpose. */
        subname = name + 6;
        grp_num = atoi((const char *)subname);
        if (grp_num > 1450000 && grp_num % 5000 == 0)
            dbgf(2, "Group name is %s\n", name);
#endif
        return 0;
    }
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
