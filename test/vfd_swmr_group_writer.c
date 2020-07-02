/*
 * Copyright by The HDF Group.
 * Copyright by the Board of Trustees of the University of Illinois.
 * All rights reserved.
 *
 * This file is part of HDF5.  The full HDF5 copyright notice, including
 * terms governing use, modification, and redistribution, is contained in
 * the COPYING file, which can be found at the root of the source code
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
 * If you do not have access to either file, you may request a copy from
 * help@hdfgroup.org.
 */

#include <err.h>
#include <libgen.h>
#include <time.h> /* nanosleep(2) */
#include <unistd.h> /* getopt(3) */

#define H5F_FRIEND              /*suppress error about including H5Fpkg   */

#include "hdf5.h"

#include "H5Fpkg.h"
// #include "H5Iprivate.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

typedef struct {
        hid_t file, filetype, one_by_one_sid;
	char filename[PATH_MAX];
	char progname[PATH_MAX];
	struct timespec update_interval;
	unsigned int asteps;
	unsigned int nsteps;
        bool wait_for_signal;
        bool use_vfd_swmr;
} state_t;

#define ALL_HID_INITIALIZER (state_t){					\
	  .file = H5I_INVALID_HID					\
	, .one_by_one_sid = H5I_INVALID_HID				\
	, .filename = ""						\
	, .filetype = H5T_NATIVE_UINT32					\
	, .asteps = 10							\
	, .nsteps = 100							\
        , .wait_for_signal = true                                       \
        , .use_vfd_swmr = true                                          \
	, .update_interval = (struct timespec){				\
		  .tv_sec = 0						\
		, .tv_nsec = 1000000000UL / 30 /* 1/30 second */}}

static void state_init(state_t *, int, char **);

static const hid_t badhid = H5I_INVALID_HID;

static void
usage(const char *progname)
{
	fprintf(stderr, "usage: %s [-S] [-W] [-a steps] [-b]\n"
                "    [-n iterations] [-u milliseconds]\n"
		"\n"
		"-S:	               do not use VFD SWMR\n"
		"-W:	               do not wait for a signal before\n"
                "                      exiting\n"
		"-a steps:	       `steps` between adding attributes\n"
		"-b:	               write data in big-endian byte order\n"
		"-n iterations:        how many times to expand each dataset\n"
		"-u ms:                milliseconds interval between updates\n"
                "                      to %s.h5\n"
		"\n",
		progname, progname);
	exit(EXIT_FAILURE);
}

static void
state_init(state_t *s, int argc, char **argv)
{
    unsigned long tmp;
    int ch;
    const hsize_t dims = 1;
    char tfile[PATH_MAX];
    char *end;
    unsigned long millis;

    *s = ALL_HID_INITIALIZER;
    esnprintf(tfile, sizeof(tfile), "%s", argv[0]);
    esnprintf(s->progname, sizeof(s->progname), "%s", basename(tfile));

    while ((ch = getopt(argc, argv, "SWa:bn:qu:")) != -1) {
        switch (ch) {
        case 'S':
            s->use_vfd_swmr = false;
            break;
        case 'W':
            s->wait_for_signal = false;
            break;
        case 'a':
        case 'n':
            errno = 0;
            tmp = strtoul(optarg, &end, 0);
            if (end == optarg || *end != '\0') {
                errx(EXIT_FAILURE, "couldn't parse `-%c` argument `%s`", ch,
                    optarg);
            } else if (errno != 0) {
                err(EXIT_FAILURE, "couldn't parse `-%c` argument `%s`", ch,
                    optarg);
            } else if (tmp > UINT_MAX)
                errx(EXIT_FAILURE, "`-%c` argument `%lu` too large", ch, tmp);

            if (ch == 'a')
                s->asteps = (unsigned)tmp;
            else
                s->nsteps = (unsigned)tmp;
            break;
        case 'b':
            s->filetype = H5T_STD_U32BE;
            break;
        case 'q':
            verbosity = 0;
            break;
        case 'u':
            errno = 0;
            millis = strtoul(optarg, &end, 0);
            if (millis == ULONG_MAX && errno == ERANGE) {
                    err(EXIT_FAILURE,
                        "option -p argument \"%s\"", optarg);
            } else if (*end != '\0') {
                    errx(EXIT_FAILURE,
                        "garbage after -p argument \"%s\"", optarg);
            }
            s->update_interval.tv_sec = (time_t)(millis / 1000UL);
            s->update_interval.tv_nsec =
                (long)((millis * 1000000UL) % 1000000000UL);
            dbgf(1, "%lu milliseconds between updates\n", millis);
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

    if (argc > 0)
        errx(EXIT_FAILURE, "unexpected command-line arguments");

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_group.h5");
}

static void
add_group_attribute(const state_t *s, hid_t g, hid_t sid, unsigned int which)
{
    hid_t aid;
    char name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    if ((aid = H5Acreate2(g, name, s->filetype, sid, H5P_DEFAULT,
            H5P_DEFAULT)) < 0)
        errx(EXIT_FAILURE, "H5Acreate2 failed");

    if (H5Awrite(aid, H5T_NATIVE_UINT, &which) < 0)
        errx(EXIT_FAILURE, "H5Awrite failed");
    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");
}


static void
write_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
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

static void
verify_group_attribute(hid_t g, unsigned int which)
{
    unsigned int read_which;
    hid_t aid;
    char name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which,
        which);

    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0)
        errx(EXIT_FAILURE, "H5Acreate2 failed");

    if (H5Aread(aid, H5T_NATIVE_UINT, &read_which) < 0)
        errx(EXIT_FAILURE, "H5Aread failed");

    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");

    if (read_which != which)
        errx(EXIT_FAILURE, "expected %u read %u", which, read_which);
}

static bool
verify_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g;
    estack_state_t es;

    assert(which < s->nsteps);

    esnprintf(name, sizeof(name), "/group-%d", which);

    es = disable_estack();
    g = H5Gopen(s->file, name, H5P_DEFAULT);
    restore_estack(es);

    if (g < 0)
        return false;

    if (s->asteps != 0 && which % s->asteps == 0)
        verify_group_attribute(g, which);

    if (H5Gclose(g) < 0)
        errx(EXIT_FAILURE, "H5Gclose failed");

    return true;
}

int
main(int argc, char **argv)
{
    hid_t fapl, fcpl;
    sigset_t oldsigs;
    herr_t ret;
    unsigned step;
    bool writer;
    state_t s;
    const char *personality;

    state_init(&s, argc, argv);

    personality = strstr(s.progname, "vfd_swmr_group_");

    if (personality != NULL &&
        strcmp(personality, "vfd_swmr_group_writer") == 0)
        writer = true;
    else if (personality != NULL &&
             strcmp(personality, "vfd_swmr_group_reader") == 0)
        writer = false;
    else {
        errx(EXIT_FAILURE,
             "unknown personality, expected vfd_swmr_group_{reader,writer}");
    }

    fapl = vfd_swmr_create_fapl(writer, true, s.use_vfd_swmr);

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

    block_signals(&oldsigs);

    if (writer) {
        for (step = 0; step < s.nsteps; step++) {
            dbgf(2, "step %d\n", step);
            write_group(&s, step);
            nanosleep(&s.update_interval, NULL);
        }
    } else {
        for (step = 0; step < s.nsteps;) {
            dbgf(2, "step %d\n", step);
            if (verify_group(&s, step))
                step++;
            nanosleep(&s.update_interval, NULL);
        }
    }

    if (s.use_vfd_swmr && s.wait_for_signal)
        await_signal(s.file);

    restore_signals(&oldsigs);

    if (H5Pclose(fapl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fapl)");

    if (H5Pclose(fcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fcpl)");

    if (H5Fclose(s.file) < 0)
        errx(EXIT_FAILURE, "H5Fclose");

    return EXIT_SUCCESS;
}
