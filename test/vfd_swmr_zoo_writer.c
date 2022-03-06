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

#define H5C_FRIEND /* suppress error about including H5Cpkg */
#define H5F_FRIEND /* suppress error about including H5Fpkg */

#include "hdf5.h"

#include "H5private.h"
#include "H5retry_private.h"
#include "H5Cpkg.h"
#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "genall5.h"
#include "vfd_swmr_common.h"

#define MAX_READ_LEN_IN_SECONDS 2
#define TICK_LEN                4

typedef struct _shared_ticks {
    uint64_t reader_tick;
} shared_ticks_t;

int                          fd_writer_to_reader = -1, fd_reader_to_writer = -1;
const char *                 fifo_writer_to_reader = "./fifo_writer_to_reader";
const char *                 fifo_reader_to_writer = "./fifo_reader_to_writer";
bool                         use_vfd_swmr          = true;
bool                         use_named_pipe        = true;
bool                         print_estack          = false;
static H5F_vfd_swmr_config_t swmr_config;
static bool                  writer;
struct timespec ival = {MAX_READ_LEN_IN_SECONDS, 0}; /* Expected maximal time for reader's validation */

zoo_config_t config = {.proc_num        = 0,
                       .skip_compact    = false,
                       .skip_varlen     = true,
                       .max_pause_msecs = 0,
                       .msgival         = {.tv_sec = 0, .tv_nsec = 0}};

static void
#ifndef H5C_COLLECT_CACHE_STATS
print_cache_hits(H5C_t *cache)
{
    int i;

    for (i = 0; i < H5AC_NTYPES; i++) {
        dbgf(3, "type-%d cache hits %" PRId64 "%s\n", i, cache->hits[i], (i == H5AC_GHEAP_ID) ? " *" : "");
    }
    dbgf(3, "\n");
}
#else
print_cache_hits(H5C_t H5_ATTR_UNUSED *cache)
{
    return;
}
#endif

void
zoo_create_hook(hid_t H5_ATTR_UNUSED fid)
{
    dbgf(3, "%s: enter\n", __func__);
    if (writer)
        decisleep(1);
}

/* Print out the menu for the command-line options */
static void
usage(const char *progname)
{
    HDfprintf(stderr, "usage: %s [-C] [-S] [-a] [-e] [-p] [-q] [-v]\n", progname);
    HDfprintf(stderr, "\n  -C: skip compact dataset tests\n");
    HDfprintf(stderr, "  -S: do not use VFD SWMR\n");
    HDfprintf(stderr, "  -a: run all tests, including variable-length data\n");
    HDfprintf(stderr, "  -e: print error stacks\n");
    HDfprintf(stderr, "  -l tick_num: expected maximal number of ticks from \n");
    HDfprintf(stderr,
              "     the writer's finishing zoo creation or deletion to the reader's finishing validation\n");
    HDfprintf(stderr, "  -N: do not use named pipes\n");
    HDfprintf(stderr, "  -q: be quiet: few/no progress messages\n");
    HDfprintf(stderr, "  -v: be verbose: most progress messages\n");
    HDexit(EXIT_FAILURE);
}

/* Private function to help parsing command-line options */
static int
parse_command_line_options(int argc, char **argv)
{
    int           ch;
    unsigned long tmpl;
    char *        end;

    while ((ch = getopt(argc, argv, "CSael:Nqv")) != -1) {
        switch (ch) {
            case 'C':
                config.skip_compact = true;
                break;
            case 'S':
                use_vfd_swmr = false;
                break;
            case 'a':
                config.skip_varlen = false;
                break;
            case 'e':
                print_estack = true;
                break;
            case 'l':
                /* Expected maximal number of ticks from the writer's finishing zoo creation or deletion
                 * to the reader's finishing validation of zoo creation or deletion */
                errno = 0;
                tmpl  = HDstrtoul(optarg, &end, 0);

                if (end == optarg || *end != '\0') {
                    HDprintf("couldn't parse `-l` argument `%s`", optarg);
                    goto error;
                }
                else if (errno != 0) {
                    HDprintf("couldn't parse `-l` argument `%s`", optarg);
                    goto error;
                }
                else if (tmpl > UINT_MAX) {
                    HDprintf("`-l` argument `%lu` too large", tmpl);
                    goto error;
                }

                {
                    /* Translate the tick number to time represented by the timespec struct */
                    float    time = (float)(((unsigned)tmpl * TICK_LEN) / 10.0);
                    unsigned sec  = (unsigned)time;
                    unsigned nsec = (unsigned)((time - sec) * 10 * 1000 * 1000);

                    ival.tv_sec  = sec;
                    ival.tv_nsec = nsec;
                }
                break;
            case 'N':
                /* Disable named pipes, mainly for running the writer and reader separately */
                use_named_pipe = false;
                break;
            case 'q':
                verbosity = 1;
                break;
            case 'v':
                verbosity = 3;
                break;
            default:
                usage(argv[0]);
                break;
        }
    }
    argv += optind;
    argc -= optind;

    if (argc > 0) {
        H5_FAILED();
        AT();
        HDprintf("unexpected command-line arguments");
        goto error;
    }

    return 0;

error:
    return -1;
}

/* Writer creates two named pipes(FIFO) to coordinate two-way communication
 * between the writer and the reader.  Both the writer and reader open the named pipes */
static int
create_open_named_pipes(void)
{
    /* Writer creates two named pipes(FIFO) to coordinate two-way communication */
    if (writer) {
        if (HDmkfifo(fifo_writer_to_reader, 0600) < 0) {
            H5_FAILED();
            AT();
            HDprintf("HDmkfifo failed");
            goto error;
        }

        if (HDmkfifo(fifo_reader_to_writer, 0600) < 0) {
            H5_FAILED();
            AT();
            HDprintf("HDmkfifo failed");
            goto error;
        }
    }

    /* Both the writer and reader open the pipes */
    if ((fd_writer_to_reader = HDopen(fifo_writer_to_reader, O_RDWR)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("fifo_writer_to_reader open failed");
        goto error;
    }

    if ((fd_reader_to_writer = HDopen(fifo_reader_to_writer, O_RDWR)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("fifo_reader_to_writer open failed");
        goto error;
    }

    return 0;

error:
    return -1;
}

/* Notify the reader of finishing zoo creation by sending the timestamp
 * and wait for the reader to finish validation before proceeding */
static int
notify_and_wait_for_reader(hid_t fid, int verify)
{
    int             notify;
    unsigned int    i;
    struct timespec last = {0, 0};

    /* Get the time when finishing zoo creation */
    if (HDclock_gettime(CLOCK_MONOTONIC, &last) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDclock_gettime failed");
        goto error;
    }

    /* Notify the reader of finishing zoo creation by sending the timestamp */
    if (HDwrite(fd_writer_to_reader, &last, sizeof(last)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDwrite failed");
        goto error;
    }

    /* During the wait, writer makes repeated HDF5 API calls so as to trigger
     * EOT at approximately the correct time */
    for (i = 0; i < swmr_config.max_lag + 1; i++) {
        decisleep(swmr_config.tick_len);

        H5E_BEGIN_TRY
        {
            H5Aexists(fid, "nonexistent");
        }
        H5E_END_TRY;
    }

    /* Wait until the reader finishes validating zoo creation */
    if (HDread(fd_reader_to_writer, &notify, sizeof(int)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDread failed");
        goto error;
    }

    if (notify != verify) {
        H5_FAILED();
        AT();
        HDprintf("expected %d but read %d", verify, notify);
        goto error;
    }

    return 0;

error:
    return -1;
}

/* Notify the reader of finishing zoo deletion by sending the timestamp */
static int
notify_reader(void)
{
    struct timespec last = {0, 0};

    /* Get the time when finishing zoo deletion */
    if (HDclock_gettime(CLOCK_MONOTONIC, &last) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDclock_gettime failed");
        goto error;
    }

    /* Notify the reader about finishing zoo deletion by sending the timestamp */
    if (HDwrite(fd_writer_to_reader, &last, sizeof(last)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDwrite failed");
        goto error;
    }

    return 0;

error:
    return -1;
}

/* Wait for the writer's notice before starting to zoo validation */
static int
reader_verify(int verify)
{
    int notify;

    if (HDread(fd_writer_to_reader, &notify, sizeof(int)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDread failed");
        goto error;
    }

    if (notify != verify) {
        H5_FAILED();
        AT();
        HDprintf("expected %d but read %d", verify, notify);
        goto error;
    }

    return 0;

error:
    return -1;
}

/* Receive the notice of the writer finishing zoo creation (timestamp)
 * Make sure the zoo validation doesn't take longer than the expected time.
 * This time period is from the writer finishing zoo creation to the reader finishing
 * the validation of zoo creation */
static int
reader_check_time_and_notify_writer(int notify)
{
    struct timespec last = {0, 0};

    /* Receive the notice of the writer finishing zoo creation (timestamp) */
    if (HDread(fd_writer_to_reader, &last, sizeof(last)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDread failed");
        goto error;
    }

    /* Make sure the zoo validation doesn't take longer than the expected time.
     * This time period is from the writer finishing zoo creation to the reader finishing
     * the validation of zoo creation */
    if (below_speed_limit(&last, &ival)) {
        AT();
        HDfprintf(stderr, "validate_zoo took too long to finish");
    }

    /* Notify the writer that zoo validation is finished */
    if (HDwrite(fd_reader_to_writer, &notify, sizeof(int)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDwrite failed");
        goto error;
    }

    return 0;

error:
    return -1;
}

/* Receive the finish notice (timestamp) from the writer.
 * Make sure validation of zoo deletion doesn't take longer than the expected time.
 * This time period is from the writer finishing zoo deletion to the reader finishing
 * the validation of zoo deletion */
static int
reader_check_time_after_verify_deletion(void)
{
    struct timespec last = {0, 0};

    if (HDread(fd_writer_to_reader, &last, sizeof(last)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDread failed");
        goto error;
    }

    if (below_speed_limit(&last, &ival)) {
        AT();
        HDfprintf(stderr, "validate_deleted_zoo took too long to finish");
    }

    return 0;

error:
    return -1;
}

/* Close and remove the named pipes */
static int
close_named_pipes(void)
{
    /* Close the named pipes */
    if (HDclose(fd_writer_to_reader) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDclose failed\n");
        goto error;
    }

    if (HDclose(fd_reader_to_writer) < 0) {
        H5_FAILED();
        AT();
        HDprintf("HDclose failed\n");
        goto error;
    }

    /* Reader finishes last and deletes the named pipes */
    if (!writer) {
        if (HDremove(fifo_writer_to_reader) != 0) {
            H5_FAILED();
            AT();
            HDprintf("HDremove failed\n");
            goto error;
        }

        if (HDremove(fifo_reader_to_writer) != 0) {
            H5_FAILED();
            AT();
            HDprintf("HDremove failed\n");
            goto error;
        }
    }

    return 0;

error:
    return -1;
}

int
main(int argc, char **argv)
{
    hid_t                 fapl = H5I_INVALID_HID, fcpl = H5I_INVALID_HID, fid = H5I_INVALID_HID;
    H5F_t *               f;
    H5C_t *               cache;
    struct timespec       lastmsgtime = {.tv_sec = 0, .tv_nsec = 0};
    char *                progname    = NULL;
    char *                personality;
    estack_state_t        es;
    H5F_vfd_swmr_config_t vfd_swmr_config;
    int                   notify = 0, verify = 0;

    if (H5_basename(argv[0], &progname) < 0) {
        H5_FAILED();
        AT();
        HDprintf("H5_basename failed\n");
        goto error;
    }

    personality = HDstrstr(progname, "vfd_swmr_zoo_");

    if (personality != NULL && HDstrcmp(personality, "vfd_swmr_zoo_writer") == 0)
        writer = true;
    else if (personality != NULL && HDstrcmp(personality, "vfd_swmr_zoo_reader") == 0)
        writer = false;
    else {
        H5_FAILED();
        AT();
        HDprintf("unknown personality, expected vfd_swmr_zoo_{reader,writer}");
        goto error;
    }

    parse_command_line_options(argc, argv);

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
     * flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(&vfd_swmr_config, TICK_LEN, 7, writer, TRUE, FALSE, TRUE, 128, "./zoo-shadow", NULL);

    /* ? turn off use latest format argument via 1st argument? since later on it reset to early format */
    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    if ((fapl = vfd_swmr_create_fapl(true, use_vfd_swmr, true, 4096, &vfd_swmr_config)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("vfd_swmr_create_fapl");
        goto error;
    }

    if (use_vfd_swmr && H5Pget_vfd_swmr_config(fapl, &swmr_config) < 0) {
        H5_FAILED();
        AT();
        HDprintf("H5Pget_vfd_swmr_config failed");
        goto error;
    }

    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0) {
        H5_FAILED();
        AT();
        HDprintf("H5Pset_libver_bounds failed");
        goto error;
    }

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        H5_FAILED();
        AT();
        HDprintf("vfd_swmr_create_fcpl() failed");
        goto error;
    }

    if (writer)
        fid = H5Fcreate("vfd_swmr_zoo.h5", H5F_ACC_TRUNC, fcpl, fapl);
    else
        fid = H5Fopen("vfd_swmr_zoo.h5", H5F_ACC_RDONLY, fapl);

    if (fid < 0) {
        H5_FAILED();
        AT();
        HDprintf(writer ? "H5Fcreate failed" : "H5Fopen failed");
        goto error;
    }

    if ((f = H5VL_object_verify(fid, H5I_FILE)) == NULL) {
        H5_FAILED();
        AT();
        HDprintf("H5VL_object_verify failed");
        goto error;
    }

    cache = f->shared->cache;

    /* Writer creates two named pipes(FIFO) to coordinate two-way communication
     * between the writer and the reader.  Both the writer and reader open the named pipes */
    if (use_named_pipe && create_open_named_pipes() < 0) {
        H5_FAILED();
        AT();
        HDprintf("create_open_named_pipes failed");
        goto error;
    }

    print_cache_hits(cache);

    es = print_estack ? estack_get_state() : disable_estack();
    if (writer) {
        dbgf(2, "Writing zoo...\n");

        /* Writer tells reader to start */
        notify = 1;
        if (use_named_pipe && HDwrite(fd_writer_to_reader, &notify, sizeof(int)) < 0) {
            H5_FAILED();
            AT();
            HDprintf("HDwrite failed");
            goto error;
        }

        /* Start to create the zoo */
        if (!create_zoo(fid, ".", &lastmsgtime, config)) {
            H5_FAILED();
            AT();
            HDprintf("create_zoo failed");
            goto error;
        }

        /* Notify the reader of finishing zoo creation by sending the timestamp
         * and wait for the reader to finish validation before proceeding */
        verify = 2;
        if (use_named_pipe && notify_and_wait_for_reader(fid, verify) < 0) {
            H5_FAILED();
            AT();
            HDprintf("notify_and_wait_for_reader failed");
            goto error;
        }

        /* Start to delete the zoo */
        if (!delete_zoo(fid, ".", &lastmsgtime, config)) {
            H5_FAILED();
            AT();
            HDprintf("delete_zoo failed");
            goto error;
        }

        /* Notify the reader of finishing zoo deletion by sending the timestamp */
        if (use_named_pipe && notify_reader() < 0) {
            H5_FAILED();
            AT();
            HDprintf("notify_reader failed");
            goto error;
        }
    }
    else {
        dbgf(2, "Reading zoo...\n");

        /* Wait for the writer's notice before starting to zoo validation */
        verify = 1;
        if (use_named_pipe && reader_verify(verify) < 0) {
            H5_FAILED();
            AT();
            HDprintf("reader_verify failed");
            goto error;
        }

        /* Validate the zoo creation */
        while (!validate_zoo(fid, ".", &lastmsgtime, config))
            ;

        /* Receive the notice of the writer finishing zoo creation (timestamp)
         * Make sure the zoo validation doesn't take longer than the expected time.
         * This time period is from the writer finishing zoo creation to the reader finishing
         * the validation of zoo creation */
        notify = 2;
        if (use_named_pipe && reader_check_time_and_notify_writer(notify) < 0) {
            H5_FAILED();
            AT();
            HDprintf("reader_check_time_and_notify_writer failed");
            goto error;
        }

        /* Start to validate the zoo deletion */
        while (!validate_deleted_zoo(fid, ".", &lastmsgtime, config))
            ;

        /* Receive the finish notice (timestamp) from the writer.
         * Make sure validation of zoo deletion doesn't take longer than the expected time.
         * This time period is from the writer finishing zoo deletion to the reader finishing
         * the validation of zoo deletion */
        if (use_named_pipe && reader_check_time_after_verify_deletion() < 0) {
            H5_FAILED();
            AT();
            HDprintf("reader_check_time_and_notify_writer failed");
            goto error;
        }
    }
    restore_estack(es);

    if (H5Pclose(fapl) < 0) {
        H5_FAILED();
        AT();
        HDprintf("H5Pclose failed");
        goto error;
    }

    if (H5Pclose(fcpl) < 0) {
        H5_FAILED();
        AT();
        HDprintf("H5Pclose failed");
        goto error;
    }

    if (H5Fclose(fid) < 0) {
        H5_FAILED();
        AT();
        HDprintf("H5Fclose failed");
        goto error;
    }

    if (progname)
        HDfree(progname);

    if (use_named_pipe && close_named_pipes() < 0) {
        H5_FAILED();
        AT();
        HDprintf("close_named_pipes failed");
        goto error;
    }

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    if (use_named_pipe && fd_writer_to_reader >= 0)
        HDclose(fd_writer_to_reader);

    if (use_named_pipe && fd_reader_to_writer >= 0)
        HDclose(fd_reader_to_writer);

    if (use_named_pipe && !writer) {
        HDremove(fifo_writer_to_reader);
        HDremove(fifo_reader_to_writer);
    }

    return EXIT_FAILURE;
}
