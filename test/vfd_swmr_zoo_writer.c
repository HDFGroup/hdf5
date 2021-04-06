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

#include <err.h>

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

#ifndef _arraycount
#define _arraycount(_a) (sizeof(_a) / sizeof(_a[0]))
#endif

#define MAX_READ_LEN_IN_SECONDS 2
#define TICK_LEN                4

typedef struct _shared_ticks {
    uint64_t reader_tick;
} shared_ticks_t;

static H5F_vfd_swmr_config_t swmr_config;
static bool                  writer;

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

static void
usage(const char *progname)
{
    fprintf(stderr, "usage: %s [-C] [-S] [-a] [-e] [-p] [-q] [-v]\n", progname);
    fprintf(stderr, "\n  -C: skip compact dataset tests\n");
    fprintf(stderr,   "  -S: do not use VFD SWMR\n");
    fprintf(stderr,   "  -a: run all tests, including variable-length data\n");
    fprintf(stderr,   "  -e: print error stacks\n");
    fprintf(stderr,   "  -l secs: maximal seconds for reader to validate the zoo\n");
    fprintf(stderr,   "  -p: do not use named pipes\n");
    fprintf(stderr,   "  -q: be quiet: few/no progress messages\n");
    fprintf(stderr,   "  -v: be verbose: most progress messages\n");
    exit(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
    hid_t fapl = H5I_INVALID_HID, fcpl = H5I_INVALID_HID, fid = H5I_INVALID_HID;
    H5F_t *f;
    H5C_t *cache;
    zoo_config_t config = {
          .proc_num = 0
        , .skip_compact = false
        , .skip_varlen = true
        , .max_pause_msecs = 0
        , .msgival = {.tv_sec = 0, .tv_nsec = 0}
    };
    struct timespec lastmsgtime = {.tv_sec = 0, .tv_nsec = 0};
    bool wait_for_signal;
    int ch;
    unsigned long tmpl;
    char *end;
    bool use_vfd_swmr = true;
    bool use_named_pipe = true;
    char *progname = NULL;
    char *personality;
    estack_state_t es;
    bool print_estack = false;
    H5F_vfd_swmr_config_t vfd_swmr_config;
    int fd_writer_to_reader = -1, fd_reader_to_writer = -1;
    const char *fifo_writer_to_reader = "./fifo_writer_to_reader";
    const char *fifo_reader_to_writer = "./fifo_reader_to_writer";
    int notify = 0;
    unsigned int i;
    struct timespec last = {0, 0};
    struct timespec ival = {MAX_READ_LEN_IN_SECONDS, 0}; /* Expected maximal time for reader's validation */

    if (H5_basename(argv[0], &progname) < 0) {
        H5_FAILED(); AT();
        printf("H5_basename failed\n");
        goto error;
    }

    personality = HDstrstr(progname, "vfd_swmr_zoo_");

    if (personality != NULL && strcmp(personality, "vfd_swmr_zoo_writer") == 0)
        writer = wait_for_signal = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_zoo_reader") == 0)
        writer = false;
    else {
        H5_FAILED(); AT();
        printf("unknown personality, expected vfd_swmr_zoo_{reader,writer}");
        goto error;
    }

    while ((ch = getopt(argc, argv, "CSael:pqv")) != -1) {
        switch(ch) {
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
            /* Expected maximal number of ticks for reader's validation of zoo creation or deletion */
            errno = 0;
            tmpl = HDstrtoul(optarg, &end, 0);
            if (end == optarg || *end != '\0')
                errx(EXIT_FAILURE, "couldn't parse `-l` argument `%s`", optarg);
            else if (errno != 0)
                err(EXIT_FAILURE, "couldn't parse `-l` argument `%s`", optarg);
            else if (tmpl > UINT_MAX)
                errx(EXIT_FAILURE, "`-l` argument `%lu` too large", tmpl);
            {
                /* Translate the tick number to time represented by the timespec struct */
                float time = (float)(((unsigned)tmpl * TICK_LEN) / 10.0);
                unsigned sec = (unsigned)time;
                unsigned nsec = (unsigned)((time - sec) * 10 * 1000 * 1000);

                ival.tv_sec = sec;
                ival.tv_nsec = nsec;
            }
            break;
        case 'p':
            /* Disable named pipes, mainly for running the writer and reader seperately */
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
        H5_FAILED(); AT();
        printf("unexpected command-line arguments");
        goto error;
    }

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&vfd_swmr_config, TICK_LEN, 7, writer, FALSE, 128, "./zoo-shadow");

    /* ? turn off use latest format argument via 1st argument? since later on it reset to early format */
    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    if ((fapl = vfd_swmr_create_fapl(true, use_vfd_swmr, true, &vfd_swmr_config)) < 0) {
        H5_FAILED(); AT();
        printf("vfd_swmr_create_fapl");
        goto error;
    }

    if (use_vfd_swmr && H5Pget_vfd_swmr_config(fapl, &swmr_config) < 0) {
        H5_FAILED(); AT();
        printf("H5Pget_vfd_swmr_config failed");
        goto error;
    }

    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0) {
        H5_FAILED(); AT();
        printf("H5Pset_libver_bounds failed");
        goto error;
    }

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
        H5_FAILED(); AT();
        printf("H5Pcreate failed");
        goto error;
    }

    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1) < 0) {
        H5_FAILED(); AT();
        printf("H5Pset_file_space_strategy failed");
        goto error;
    }

    if (writer)
        fid = H5Fcreate("vfd_swmr_zoo.h5", H5F_ACC_TRUNC, fcpl, fapl);
    else
        fid = H5Fopen("vfd_swmr_zoo.h5", H5F_ACC_RDONLY, fapl);

    if (fid < 0) {
        errx(EXIT_FAILURE, writer ? "H5Fcreate" : "H5Fopen");
        H5_FAILED(); AT();
        printf(writer ? "H5Fcreate failed" : "H5Fopen failed");
        goto error;
    }

    if ((f = H5VL_object_verify(fid, H5I_FILE)) == NULL) {
        H5_FAILED(); AT();
        printf("H5VL_object_verify failed");
        goto error;
    }

    cache = f->shared->cache;

    /* Writer creates two named pipes(FIFO) to coordinate two-way communication */
    if (use_named_pipe && writer) {
        if (HDmkfifo(fifo_writer_to_reader, 0600) < 0) {
            H5_FAILED(); AT();
            printf("HDmkfifo failed");
            goto error;
        }

        if (HDmkfifo(fifo_reader_to_writer, 0600) < 0) {
            H5_FAILED(); AT();
            printf("HDmkfifo failed");
            goto error;
        }
    }

    /* Both the writer and reader open the pipes */
    if (use_named_pipe && (fd_writer_to_reader = HDopen(fifo_writer_to_reader, O_RDWR)) < 0) {
        H5_FAILED(); AT();
        printf("fifo_writer_to_reader open failed");
        goto error;
    }

    if (use_named_pipe && (fd_reader_to_writer = HDopen(fifo_reader_to_writer, O_RDWR)) < 0) {
        H5_FAILED(); AT();
        printf("fifo_reader_to_writer open failed");
        goto error;
    }

    print_cache_hits(cache);

    es = print_estack ? estack_get_state() : disable_estack();
    if (writer) {
        dbgf(2, "Writing zoo...\n");

        /* Writer tells reader to start */
        notify = 1;
        if (use_named_pipe && HDwrite(fd_writer_to_reader, &notify, sizeof(int)) < 0) {
            H5_FAILED(); AT();
            printf("HDwrite failed");
            goto error;
        }

        if (!create_zoo(fid, ".", &lastmsgtime, config)) {
            H5_FAILED(); AT();
            printf("create_zoo failed");
            goto error;
        }

        if (use_named_pipe) {
            /* Get the time when finishing zoo creation */
            if (HDclock_gettime(CLOCK_MONOTONIC, &last) < 0) {
                H5_FAILED(); AT();
                printf("HDclock_gettime failed");
                goto error;
            }

            /* Notify the reader of finishing zoo creation by sending the timestamp */
            if (HDwrite(fd_writer_to_reader, &last, sizeof(last)) < 0) {
                H5_FAILED(); AT();
                printf("HDwrite failed");
                goto error;
            }

            /* During the wait, writer makes repeated HDF5 API calls so as to trigger
             * EOT at approximately the correct time */
            for(i = 0; i < swmr_config.max_lag + 1; i++) {
                decisleep(swmr_config.tick_len);

                H5E_BEGIN_TRY {
                    H5Aexists(fid, "nonexistent");
                } H5E_END_TRY;
            }

            /* Wait until the reader finishes validating zoo creation */
            if (HDread(fd_reader_to_writer, &notify, sizeof(int)) < 0) {
                H5_FAILED(); AT();
                printf("HDread failed");
                goto error;
            }

            if (notify != 2) {
                H5_FAILED(); AT();
                printf("expected 2 but read %d", notify);
                goto error;
            }
        }

        /* Start to delete the zoo */
        if (!delete_zoo(fid, ".", &lastmsgtime, config)) {
            H5_FAILED(); AT();
            printf("delete_zoo failed");
            goto error;
        }

        if (use_named_pipe) {
            /* Get the time when finishing zoo deletion */
            if (HDclock_gettime(CLOCK_MONOTONIC, &last) < 0) {
                H5_FAILED(); AT();
                printf("HDclock_gettime failed");
                goto error;
            }

            /* Notify the reader about finishing zoo deletion by sending the timestamp */
            if (HDwrite(fd_writer_to_reader, &last, sizeof(last)) < 0) {
                H5_FAILED(); AT();
                printf("HDwrite failed");
                goto error;
            }
        }
    } else {
        dbgf(2, "Reading zoo...\n");

        if (use_named_pipe) {
            /* Start to validate zoo creation after receiving the writer's notice */
            if (HDread(fd_writer_to_reader, &notify, sizeof(int)) < 0) {
                H5_FAILED(); AT();
                printf("HDread failed");
                goto error;
            }

            if (notify != 1) {
                H5_FAILED(); AT();
                printf("expected 1 but read %d", notify);
                goto error;
            }
        }

        while (!validate_zoo(fid, ".", &lastmsgtime, config))
            ;

        if (use_named_pipe) {
            /* Receive the notice of the writer finishing zoo creation (timestamp) */
            if (HDread(fd_writer_to_reader, &last, sizeof(last)) < 0) {
                H5_FAILED(); AT();
                printf("HDread failed");
                goto error;
            }

            /* Make sure the zoo validation doesn't take longer than the expected time.
             * This time period is from the writer finishing zoo creation to the reader finishing
             * the validation of zoo creation */
            if (below_speed_limit(&last, &ival)) {
                AT();
                warnx("validate_zoo took too long to finish");
            }

            /* Notify the writer that zoo validation is finished */
            notify = 2;
            if (HDwrite(fd_reader_to_writer, &notify, sizeof(int)) < 0) {
                H5_FAILED(); AT();
                printf("HDwrite failed");
                goto error;
            }
        }

        while (!validate_deleted_zoo(fid, ".", &lastmsgtime, config))
            ;

        if (use_named_pipe) {
            /* Receive the finish notice (timestamp) from the writer */
            if (HDread(fd_writer_to_reader, &last, sizeof(last)) < 0) {
                H5_FAILED(); AT();
                printf("HDread failed");
                goto error;
            }

            /* Make sure validation of zoo deletion doesn't take longer than the expected time.
             * This time period is from the writer finishing zoo deletion to the reader finishing
             * the validation of zoo deletion */
            if (below_speed_limit(&last, &ival)) {
                AT();
                warnx("validate_deleted_zoo took too long to finish");
            }
        }
    }
    restore_estack(es);

    if (H5Pclose(fapl) < 0) {
        H5_FAILED(); AT();
        printf("H5Pclose failed");
        goto error;
    }

    if (H5Pclose(fcpl) < 0) {
        H5_FAILED(); AT();
        printf("H5Pclose failed");
        goto error;
    }

    if (H5Fclose(fid) < 0) {
        H5_FAILED(); AT();
        printf("H5Fclose failed");
        goto error;
    }

    if (progname)
        HDfree(progname);

    /* Close the named pipes */
    if (use_named_pipe && HDclose(fd_writer_to_reader) < 0) {
        H5_FAILED(); AT();
        printf("HDclose failed\n");
        goto error;
    }

    if (use_named_pipe && HDclose(fd_reader_to_writer) < 0) {
        H5_FAILED(); AT();
        printf("HDclose failed\n");
        goto error;
    }

    /* Reader finishes last and deletes the named pipes */
    if(use_named_pipe && !writer) {
        if(HDremove(fifo_writer_to_reader) != 0) {
            H5_FAILED(); AT();
            printf("HDremove failed\n");
            goto error;
        }

        if(HDremove(fifo_reader_to_writer) != 0) {
            H5_FAILED(); AT();
            printf("HDremove failed\n");
            goto error;
        }
    }

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(fid);
    } H5E_END_TRY;

    if (use_named_pipe && fd_writer_to_reader >= 0)
        HDclose(fd_writer_to_reader);

    if (use_named_pipe && fd_reader_to_writer >= 0)
        HDclose(fd_reader_to_writer);

    if(use_named_pipe && !writer) {
        HDremove(fifo_writer_to_reader);
        HDremove(fifo_reader_to_writer);
    }

    return EXIT_FAILURE;
}
