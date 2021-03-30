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

#define READER_WAIT_TICKS	3

typedef struct {
        hid_t file, filetype, one_by_one_sid;
	char filename[PATH_MAX];
	char progname[PATH_MAX];
	unsigned int asteps;
	unsigned int csteps;
	unsigned int nsteps;
        unsigned int update_interval;
        bool use_vfd_swmr;
} state_t;

#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .file = H5I_INVALID_HID, .one_by_one_sid = H5I_INVALID_HID, .filename = "",                          \
        .filetype = H5T_NATIVE_UINT32, .asteps = 10, .csteps = 10, .nsteps = 100, .update_interval = READER_WAIT_TICKS,   \
        .use_vfd_swmr = true                                                                                 \
    }

static void
usage(const char *progname)
{
	fprintf(stderr, "usage: %s [-S] [-a steps] [-b] [-c]\n"
                "    [-n iterations] [-u numb_ticks]\n"
		"\n"
		"-S:	               do not use VFD SWMR\n"
		"-a steps:	       `steps` between adding attributes\n"
		"-b:	               write data in big-endian byte order\n"
		"-c steps:	       `steps` between communication between the writer and reader\n"
                "-n ngroups:           the number of groups\n"
                "-u numb_tcks:         `numb_ticks` for the reader to wait before verification\n"
		"\n",
		progname);
	exit(EXIT_FAILURE);
}

static bool
state_init(state_t *s, int argc, char **argv)
{
    unsigned long tmp;
    int           ch;
    const hsize_t dims = 1;
    char          *tfile = NULL;
    char *        end;

    *s = ALL_HID_INITIALIZER;

    if (H5_basename(argv[0], &tfile) < 0) {
        H5_FAILED(); AT();
        printf("H5_basename failed\n");
        goto error;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile)
        HDfree(tfile);

    while ((ch = getopt(argc, argv, "SWa:bc:n:qu:")) != -1) {
        switch (ch) {
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'a':
            case 'c':
            case 'n':
            case 'u':
                errno = 0;
                tmp = strtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    H5_FAILED(); AT();
                    printf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    goto error;
                } else if (errno != 0) {
                    H5_FAILED(); AT();
                    printf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    goto error;
                } else if (tmp > UINT_MAX) {
                    H5_FAILED(); AT();
                    printf("`-%c` argument `%lu` too large\n", ch, tmp);
                    goto error;
                }

                if (ch == 'a')
                    s->asteps = (unsigned)tmp;
                else if (ch == 'c')
                    s->csteps = (unsigned)tmp;
                else if (ch == 'n')
                    s->nsteps = (unsigned)tmp;
                else if (ch == 'u')
                    s->update_interval = (unsigned)tmp;
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
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        H5_FAILED(); AT();
        printf("H5Screate_simple failed\n");
        goto error;
    }

    if( s->csteps < 1 || s->csteps > s->nsteps) {
        H5_FAILED(); AT();
        printf("communication interval is out of bounds\n");
        goto error;
    }

    if( s->asteps < 1 || s->asteps > s->nsteps) {
        H5_FAILED(); AT();
        printf("attribute interval is out of bounds\n");
        goto error;
    }

    if (argc > 0) {
        H5_FAILED(); AT();
        printf("unexpected command-line arguments\n");
        goto error;
    }

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_group.h5");

    return true;

error:
    if (tfile)
        HDfree(tfile);

    return false;
}

static bool
add_group_attribute(const state_t *s, hid_t g, hid_t sid, unsigned int which)
{
    hid_t aid;
    char  name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "setting attribute %s on group %u to %u\n", name, which, which);

    if ((aid = H5Acreate2(g, name, s->filetype, sid, H5P_DEFAULT,
            H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Acreate2 failed\n");
        goto error;
    }

    if (H5Awrite(aid, H5T_NATIVE_UINT, &which) < 0) {
        H5_FAILED(); AT();
        printf("H5Awrite failed\n");
        goto error;
    }

    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        printf("H5Aclose failed\n");
        goto error;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
    } H5E_END_TRY;

    return false;
}

static bool
write_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;
    bool result = true;

    if (which >= s->nsteps) {
        H5_FAILED(); AT();
        printf("group order is out of bounds\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "/group-%d", which);

    if ((g = H5Gcreate2(s->file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Gcreate2 failed\n");
        goto error;
    }

    if (s->asteps != 0 && which % s->asteps == 0)
        result = add_group_attribute(s, g, s->one_by_one_sid, which);

    if (H5Gclose(g) < 0) {
        H5_FAILED(); AT();
        printf("H5Gclose failed\n");
        goto error;
    }

    return result;

error:
    H5E_BEGIN_TRY {
        H5Gclose(g);
    } H5E_END_TRY;

    return false;
}

static bool
verify_group_attribute(hid_t g, unsigned int which)
{
    unsigned int read_which;
    hid_t aid;
    char name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    dbgf(1, "verifying attribute %s on group %u equals %u\n", name, which, which);

    if ((aid = H5Aopen(g, name, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Aopen failed\n");
        goto error;
    }

    if (H5Aread(aid, H5T_NATIVE_UINT, &read_which) < 0) {
        H5_FAILED(); AT();
        printf("H5Aread failed\n");
        goto error;
    }

    if (read_which != which) {
        H5_FAILED(); AT();
        printf("H5Aread wrong value\n");
        goto error;
    }

    if (H5Aclose(aid) < 0) {
        H5_FAILED(); AT();
        printf("H5Aread failed\n");
        goto error;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
    } H5E_END_TRY;

    return false;
}

static bool
verify_group(state_t *s, unsigned int which)
{
    char name[sizeof("/group-9999999999")];
    hid_t g = H5I_INVALID_HID;
    bool result = true;

    if (which >= s->nsteps) {
        H5_FAILED(); AT();
        printf("Group order is out of bounds\n");
        goto error;
    }

    esnprintf(name, sizeof(name), "/group-%d", which);

    if ((g = H5Gopen(s->file, name, H5P_DEFAULT)) < 0) {
        H5_FAILED(); AT();
        printf("H5Gopen failed\n");
        goto error;
    }

    if (s->asteps != 0 && which % s->asteps == 0)
        result = verify_group_attribute(g, which);
    else
        result = true;

    if (H5Gclose(g) < 0) {
        H5_FAILED(); AT();
        printf("H5Gclose failed\n");
        goto error;
    }

    return result;

error:
    H5E_BEGIN_TRY {
        H5Gclose(g);
    } H5E_END_TRY;

    return false;
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
    hid_t fapl = H5I_INVALID_HID, fcpl = H5I_INVALID_HID;
    unsigned step;
    bool writer = false;
    state_t s;
    const char *personality;
    H5F_vfd_swmr_config_t config;
    const char *fifo_writer_to_reader = "./fifo_group_writer_to_reader";
    const char *fifo_reader_to_writer = "./fifo_group_reader_to_writer";
    int fd_writer_to_reader = -1, fd_reader_to_writer = -1;
    int notify = 0, verify = 0;
    unsigned int i;

    if (!state_init(&s, argc, argv)) {
        H5_FAILED(); AT();
        printf("state_init failed\n");
        goto error;
    }

    personality = strstr(s.progname, "vfd_swmr_group_");

    if (personality != NULL && strcmp(personality, "vfd_swmr_group_writer") == 0)
        writer = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_group_reader") == 0)
        writer = false;
    else {
        H5_FAILED(); AT();
        printf("unknown personality, expected vfd_swmr_group_{reader,writer}\n");
        goto error;
    }

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&config, 4, 7, writer, FALSE, 128, "./group-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    if ((fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, &config)) < 0) {
        H5_FAILED(); AT();
        printf("vfd_swmr_create_fapl failed\n");
        goto error;
    }

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
        H5_FAILED(); AT();
        printf("H5Pcreate failed\n");
        goto error;
    }

    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1) < 0) {
        H5_FAILED(); AT();
        printf("H5Pset_file_space_strategy failed\n");
        goto error;
    }

    if (writer)
        s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl);
    else
        s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl);

    if (s.file < 0) {
        H5_FAILED(); AT();
        printf("H5Fcreate/open failed\n");
        goto error;
    }

    /* Use two named pipes(FIFO) to coordinate the writer and reader for
     * two-way communication so that the two sides can move forward together.
     * One is for the writer to write to the reader.
     * The other one is for the reader to signal the writer.  */
    if (writer) {
        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(fifo_writer_to_reader, 0600) < 0) {
            H5_FAILED(); AT();
            printf("HDmkfifo failed\n");
            goto error;
        }

        if (HDmkfifo(fifo_reader_to_writer, 0600) < 0) {
            H5_FAILED(); AT();
            printf("HDmkfifo failed\n");
            goto error;
        }

    }

    /* Both the writer and reader open the pipes */
    if ((fd_writer_to_reader = HDopen(fifo_writer_to_reader, O_RDWR)) < 0) {
        H5_FAILED(); AT();
        printf("HDopen failed\n");
        goto error;
    }

    if ((fd_reader_to_writer = HDopen(fifo_reader_to_writer, O_RDWR)) < 0) {
        H5_FAILED(); AT();
        printf("HDopen failed\n");
        goto error;
    }

    if (writer) {
        for (step = 0; step < s.nsteps; step++) {
            dbgf(2, "writer: step %d\n", step);

            if (!write_group(&s, step)) {
                H5_FAILED(); AT();
                printf("write_group failed\n");

                /* At communication interval, notifies the reader about the failture and quit */
                if (step % s.csteps == 0) {
                    notify = -1;
                    HDwrite(fd_writer_to_reader, &notify, sizeof(int));
                    goto error;
                }
            } else {
                /* At communication interval, notifies the reader and waits for its response */
                if (step % s.csteps == 0) {
                    /* Bump up the value of notify to notice the reader to start to read */
                    notify++;
                    if (HDwrite(fd_writer_to_reader, &notify, sizeof(int)) < 0) {
                        H5_FAILED(); AT();
                        printf("HDwrite failed\n");
                        goto error;
                    }

                    /* During the wait, writer makes repeated HDF5 API calls
                     * to trigger EOT at approximately the correct time */
                    for(i = 0; i < config.max_lag + 1; i++) {
                        decisleep(config.tick_len);
                        H5E_BEGIN_TRY {
                            H5Aexists(s.file, "nonexistent");
                        } H5E_END_TRY;
                    }

                    /* Receive the same value from the reader and verify it before
                     * going to the next step */
                    verify++;
                    if (HDread(fd_reader_to_writer, &notify, sizeof(int)) < 0) {
                        H5_FAILED(); AT();
                        printf("HDread failed\n");
                        goto error;
                    }

                    if (notify == -1) {
                        H5_FAILED(); AT();
                        printf("reader failed to verify group\n");
                        goto error;
                    }

                    if (notify != verify) {
                        H5_FAILED(); AT();
                        printf("received message %d, expecting %d\n", notify, verify);
                        goto error;
                    }
                }
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
                if (HDread(fd_writer_to_reader, &notify, sizeof(int)) < 0) {
                    H5_FAILED(); AT();
                    printf("HDread failed\n");
                    goto error;
                }

                if (notify == -1) {
                    H5_FAILED(); AT();
                    printf("writer failed to create group\n");
                    goto error;
                }

                if (notify != verify) {
                    H5_FAILED(); AT();
                    printf("received message %d, expecting %d\n", notify, verify);
                    goto error;
                }
            }

            /* Wait for a few ticks for the update to happen */
            decisleep(config.tick_len * s.update_interval);

            /* Start to verify group */
            if (!verify_group(&s, step)) {
                H5_FAILED(); AT();
                printf("verify_group failed\n");

                /* At communication interval, tell the writer about the failure and exit */
                if (step % s.csteps == 0) {
                    notify = -1;
                    HDwrite(fd_reader_to_writer, &notify, sizeof(int));
                    goto error;
                }
            } else {
                if (step % s.csteps == 0) {
                    /* Send back the same nofity value for acknowledgement to tell the writer
                     * move to the next step */
                    if (HDwrite(fd_reader_to_writer, &notify, sizeof(int)) < 0) {
                        H5_FAILED(); AT();
                        printf("HDwrite failed\n");
                        goto error;
                    }
                }
            }
        }
    }

    if (H5Pclose(fapl) < 0) {
        H5_FAILED(); AT();
        printf("H5Pclose failed\n");
        goto error;
    }

    if (H5Pclose(fcpl) < 0) {
        H5_FAILED(); AT();
        printf("H5Pclose failed\n");
        goto error;
    }

    if (H5Fclose(s.file) < 0) {
        H5_FAILED(); AT();
        printf("H5Fclose failed\n");
        goto error;
    }

    /* Both the writer and reader close the named pipes */
    if (HDclose(fd_writer_to_reader) < 0) {
        H5_FAILED(); AT();
        printf("HDclose failed\n");
        goto error;
    }

    if (HDclose(fd_reader_to_writer) < 0) {
        H5_FAILED(); AT();
        printf("HDclose failed\n");
        goto error;
    }

    /* Reader finishes last and deletes the named pipes */
    if(!writer) {
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
        H5Fclose(s.file);
    } H5E_END_TRY;

    if (fd_writer_to_reader >= 0)
        HDclose(fd_writer_to_reader);

    if (fd_reader_to_writer >= 0)
        HDclose(fd_reader_to_writer);

    if(!writer) {
        HDremove(fifo_writer_to_reader);
        HDremove(fifo_reader_to_writer);
    }

    return EXIT_FAILURE;
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
