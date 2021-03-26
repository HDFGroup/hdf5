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

#define H5F_FRIEND /*suppress error about including H5Fpkg   */

#include "hdf5.h"

#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#include <err.h>
#include <libgen.h>

typedef struct {
    hid_t        file, filetype, one_by_one_sid;
    char         filename[PATH_MAX];
    char         progname[PATH_MAX];
    unsigned int asteps;
    unsigned int csteps;
    unsigned int nsteps;
    bool         wait_for_signal;
    bool         use_vfd_swmr;
} state_t;

#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .file = H5I_INVALID_HID, .one_by_one_sid = H5I_INVALID_HID, .filename = "",                          \
        .filetype = H5T_NATIVE_UINT32, .asteps = 10, .csteps = 10, .nsteps = 100, .wait_for_signal = true,   \
        .use_vfd_swmr = true                                                                                 \
    }

static void state_init(state_t *, int, char **);

static const hid_t badhid = H5I_INVALID_HID;

static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: %s [-S] [-W] [-a steps] [-b] [-c]\n"
            "    [-n iterations]\n"
            "\n"
            "-S:	               do not use VFD SWMR\n"
            "-W:	               do not wait for a signal before\n"
            "                      exiting\n"
            "-a steps:	       `steps` between adding attributes\n"
            "-b:	               write data in big-endian byte order\n"
            "-c steps:	       `steps` between communication between the writer and reader\n"
            "-n ngroups:           the number of groups\n"
            "\n",
            progname);
    exit(EXIT_FAILURE);
}

static void
state_init(state_t *s, int argc, char **argv)
{
    unsigned long tmp;
    int           ch;
    const hsize_t dims = 1;
    char          tfile[PATH_MAX];
    char *        end;

    *s = ALL_HID_INITIALIZER;
    esnprintf(tfile, sizeof(tfile), "%s", argv[0]);
    esnprintf(s->progname, sizeof(s->progname), "%s", basename(tfile));

    while ((ch = getopt(argc, argv, "SWa:bc:n:q")) != -1) {
        switch (ch) {
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'W':
                s->wait_for_signal = false;
                break;
            case 'a':
            case 'c':
            case 'n':
                errno = 0;
                tmp   = strtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    errx(EXIT_FAILURE, "couldn't parse `-%c` argument `%s`", ch, optarg);
                }
                else if (errno != 0) {
                    err(EXIT_FAILURE, "couldn't parse `-%c` argument `%s`", ch, optarg);
                }
                else if (tmp > UINT_MAX)
                    errx(EXIT_FAILURE, "`-%c` argument `%lu` too large", ch, tmp);

                if (ch == 'a')
                    s->asteps = (unsigned)tmp;
                else if (ch == 'c')
                    s->csteps = (unsigned)tmp;
                else if (ch == 'n')
                    s->nsteps = (unsigned)tmp;
                break;
            case 'b':
                s->filetype = H5T_STD_U32BE;
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

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0)
        errx(EXIT_FAILURE, "H5Screate_simple failed");

    if (s->csteps < 1 || s->csteps > s->nsteps)
        errx(EXIT_FAILURE, "communication interval is out of bounds");

    if (s->asteps < 1 || s->asteps > s->nsteps)
        errx(EXIT_FAILURE, "attribute interval is out of bounds");

    if (argc > 0)
        errx(EXIT_FAILURE, "unexpected command-line arguments");

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_group.h5");
}

static void
add_group_attribute(const state_t *s, hid_t g, hid_t sid, unsigned int which)
{
    hid_t aid;
    char  name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    if ((aid = H5Acreate2(g, name, s->filetype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        errx(EXIT_FAILURE, "H5Acreate2 failed");

    if (H5Awrite(aid, H5T_NATIVE_UINT, &which) < 0)
        errx(EXIT_FAILURE, "H5Awrite failed");
    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");
}

static void
write_group(state_t *s, unsigned int which)
{
    char  name[sizeof("/group-9999999999")];
    hid_t g;

    assert(which < s->nsteps);

    esnprintf(name, sizeof(name), "/group-%d", which);

    g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (g < 0)
        errx(EXIT_FAILURE, "H5Gcreate(, \"%s\", ) failed", name);

    if (s->asteps != 0 && which % s->asteps == 0)
        add_group_attribute(s, g, s->one_by_one_sid, which);

    if (H5Gclose(g) < 0)
        errx(EXIT_FAILURE, "H5Gclose failed");
}

static bool
verify_group_attribute(hid_t g, unsigned int which)
{
    estack_state_t es;
    unsigned int   read_which;
    hid_t          aid;
    char           name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which, which);

    es = disable_estack();
    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        restore_estack(es);
        return false;
    }

    if (H5Aread(aid, H5T_NATIVE_UINT, &read_which) < 0) {
        restore_estack(es);
        if (H5Aclose(aid) < 0)
            errx(EXIT_FAILURE, "H5Aclose failed");
        return false;
    }

    restore_estack(es);

    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");

    return read_which == which;
}

static bool
verify_group(state_t *s, unsigned int which)
{
    char           name[sizeof("/group-9999999999")];
    hid_t          g;
    estack_state_t es;
    bool           result;

    assert(which < s->nsteps);

    esnprintf(name, sizeof(name), "/group-%d", which);

    es = disable_estack();
    g  = H5Gopen2(s->file, name, H5P_DEFAULT);
    restore_estack(es);

    if (g < 0)
        return false;

    if (s->asteps != 0 && which % s->asteps == 0)
        result = verify_group_attribute(g, which);
    else
        result = true;

    if (H5Gclose(g) < 0)
        errx(EXIT_FAILURE, "H5Gclose failed");

    return result;
}

/* Sleep for `tenths` tenths of a second */
static void
decisleep(uint32_t tenths)
{
    uint64_t nsec = tenths * 100 * 1000 * 1000;

    H5_nanosleep(nsec);
}

int
main(int argc, char **argv)
{
    hid_t                 fapl, fcpl;
    herr_t                ret;
    unsigned              step;
    bool                  writer;
    state_t               s;
    const char *          personality;
    H5F_vfd_swmr_config_t config;
    const char *          fifo_writer_to_reader = "./fifo_group_writer_to_reader";
    const char *          fifo_reader_to_writer = "./fifo_group_reader_to_writer";
    int                   fd_writer_to_reader, fd_reader_to_writer;
    int                   notify = 0, verify = 0;
    unsigned int          i;

    state_init(&s, argc, argv);

    personality = strstr(s.progname, "vfd_swmr_group_");

    if (personality != NULL && strcmp(personality, "vfd_swmr_group_writer") == 0)
        writer = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_group_reader") == 0)
        writer = false;
    else {
        errx(EXIT_FAILURE, "unknown personality, expected vfd_swmr_group_{reader,writer}");
    }

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&config, 4, 7, writer, FALSE, 128, "./group-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, &config);

    if (fapl < 0)
        errx(EXIT_FAILURE, "vfd_swmr_create_fapl");

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        errx(EXIT_FAILURE, "H5Pcreate");

    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1);
    if (ret < 0)
        errx(EXIT_FAILURE, "H5Pset_file_space_strategy");

    if (writer)
        s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl);
    else
        s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl);

    if (s.file == badhid)
        errx(EXIT_FAILURE, writer ? "H5Fcreate" : "H5Fopen");

    /* Use two named pipes(FIFO) to coordinate the writer and reader for
     * two-way communication so that the two sides can move forward together.
     * One is for the writer to write to the reader.
     * The other one is for the reader to signal the writer.  */
    if (writer) {
        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(fifo_writer_to_reader, 0600) < 0)
            errx(EXIT_FAILURE, "HDmkfifo");

        if (HDmkfifo(fifo_reader_to_writer, 0600) < 0)
            errx(EXIT_FAILURE, "HDmkfifo");
    }

    /* Both the writer and reader open the pipes */
    if ((fd_writer_to_reader = HDopen(fifo_writer_to_reader, O_RDWR)) < 0)
        errx(EXIT_FAILURE, "fifo_writer_to_reader open failed");

    if ((fd_reader_to_writer = HDopen(fifo_reader_to_writer, O_RDWR)) < 0)
        errx(EXIT_FAILURE, "fifo_reader_to_writer open failed");

    if (writer) {
        for (step = 0; step < s.nsteps; step++) {
            dbgf(2, "writer: step %d\n", step);

            write_group(&s, step);

            /* At communication interval, notifies the reader and waits for its response */
            if (step % s.csteps == 0) {
                /* Bump up the value of notify to notice the reader to start to read */
                notify++;
                if (HDwrite(fd_writer_to_reader, &notify, sizeof(int)) < 0)
                    err(EXIT_FAILURE, "write failed");

                /* During the wait, writer makes repeated HDF5 API calls
                 * to trigger EOT at approximately the correct time */
                for (i = 0; i < config.max_lag + 1; i++) {
                    decisleep(config.tick_len);
                    H5Aexists(s.file, "nonexistent");
                }

                /* Receive the same value from the reader and verify it before
                 * going to the next step */
                verify++;
                if (HDread(fd_reader_to_writer, &notify, sizeof(int)) < 0)
                    err(EXIT_FAILURE, "read failed");

                if (notify != verify)
                    errx(EXIT_FAILURE, "received message %d, expecting %d", notify, verify);
            }
        }
    }
    else {
        for (step = 0; step < s.nsteps; step++) {
            dbgf(2, "reader: step %d\n", step);

            /* At communication interval, waits for the writer to finish creation before starting verification
             */
            if (step % s.csteps == 0) {
                /* The writer should have bumped up the value of notify.
                 * Do the same with verify and confirm it */
                verify++;

                /* Receive the notify that the writer bumped up the value */
                if (HDread(fd_writer_to_reader, &notify, sizeof(int)) < 0)
                    err(EXIT_FAILURE, "read failed");

                if (notify != verify)
                    errx(EXIT_FAILURE, "received message %d, expecting %d", notify, verify);
            }

            while (!verify_group(&s, step))
                ;

            if (step % s.csteps == 0) {
                /* Send back the same nofity value for acknowledgement to tell the writer
                 * move to the next step. */
                if (HDwrite(fd_reader_to_writer, &notify, sizeof(int)) < 0)
                    err(EXIT_FAILURE, "write failed");
            }
        }
    }

    if (H5Pclose(fapl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fapl)");

    if (H5Pclose(fcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fcpl)");

    if (H5Fclose(s.file) < 0)
        errx(EXIT_FAILURE, "H5Fclose");

    /* Both the writer and reader close the named pipes */
    if (HDclose(fd_writer_to_reader) < 0)
        errx(EXIT_FAILURE, "HDclose");

    if (HDclose(fd_reader_to_writer) < 0)
        errx(EXIT_FAILURE, "HDclose");

    /* Reader finishes last and deletes the named pipes */
    if (!writer) {
        if (HDremove(fifo_writer_to_reader) != 0)
            errx(EXIT_FAILURE, "fifo_writer_to_reader deletion failed");

        if (HDremove(fifo_reader_to_writer) != 0)
            errx(EXIT_FAILURE, "fifo_reader_to_writer deletion failed");
    }

    return EXIT_SUCCESS;
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
