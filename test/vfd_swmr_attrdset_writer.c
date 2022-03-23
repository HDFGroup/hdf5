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

/*
 *  Purpose: To test attribute handling for different dataset types.
 *      Dataset types:
 *      --dataset with compact layout
 *      --dataset with contiguous layout
 *      --dataset with chunked layout:
 *              1. single indexing type
 *              2. implicit indexing type
 *              3. fixed array indexing type
 *              4. extensible array indexing type
 *              5. version btree 2 indexing type
 *      Attribute handling:
 *      -- Add attribute
 *      -- Delete attribute
 *      -- Modify attribute
 *      -- Add variable length attribute
 *      -- Delete variable length attribute
 *      -- Modify variable length attribute
 *      -- Add sufficient attributes to force creation of object header continuation block
 *      -- Remove sufficient attributes to allow deletion of object header continuation block
 *      -- Transition from compact to dense attribute storage
 *      -- Transition from dense to compact attribute storage
 *
 * Please see verify_storage_cont() on verification of
 * compact<->dense storage and with/without continuation block.
 *
 */

#include "hdf5.h"
#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define READER_WAIT_TICKS 4

/* Structure to hold info for options specified */
typedef struct {
    hid_t        file;               /* File ID */
    hid_t        filetype;           /* ID for default datatype */
    hid_t        one_by_one_sid;     /* ID for default dataspace */
    char         filename[PATH_MAX]; /* File name */
    char         progname[PATH_MAX]; /* Program name */
    unsigned int update_interval;    /* For -u option */
    unsigned int asteps;             /* For -a <nattrs> option */
    unsigned int csteps;             /* For -c <csteps> option */
    unsigned int dattrs;             /* For -d <dattrs> option */
    hbool_t      compact;            /* For -p option */
    hbool_t      contig;             /* For -g option */
    hbool_t      chunked;            /* For -k option */
    hbool_t      vl_attr;            /* For -v option */
    hbool_t      mod_attr;           /* For -m option */
    hbool_t      use_np;             /* For -N option */
    hbool_t      use_vfd_swmr;       /* For -S option */
} state_t;

/* Initializations for state_t */
#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .file = H5I_INVALID_HID, .one_by_one_sid = H5I_INVALID_HID, .filename = "",                          \
        .filetype = H5T_NATIVE_UINT32, .asteps = 0, .csteps = 1, .dattrs = 0, .use_np = TRUE,                \
        .use_vfd_swmr = TRUE, .compact = FALSE, .contig = FALSE, .chunked = FALSE, .vl_attr = FALSE,         \
        .mod_attr = FALSE, .update_interval = READER_WAIT_TICKS                                              \
    }

/* Structure to hold info for different dataset types */
typedef struct {
    hid_t    compact_did;          /* ID for compact dataset */
    hid_t    contig_did;           /* ID for contiguous dataset */
    hid_t    single_did;           /* ID for chunked dataset: single index */
    hid_t    implicit_did;         /* ID for chunked dataset: implicit index */
    hid_t    fa_did;               /* ID for chunked dataset: fixed array index  */
    hid_t    ea_did;               /* ID for chunked dataset: extensible array index */
    hid_t    bt2_did;              /* ID for chunked dataset: version 2 btree index */
    unsigned p_max_compact;        /* Value of max_compact storage for -p */
    unsigned g_max_compact;        /* Value of max_compact storage for -g */
    unsigned single_max_compact;   /* Value of max_compact storage for -k: single index */
    unsigned implicit_max_compact; /* Value of max_compact storage for -k: implicit index */
    unsigned fa_max_compact;       /* Value of max_compact storage for -k: fixed array index */
    unsigned ea_max_compact;       /* Value of max_compact storage for -k: extensible array index */
    unsigned bt2_max_compact;      /* Value of max_compact storage for -k: version 2 btree index */
    unsigned p_min_dense;          /* Value of min_dense storage for -p */
    unsigned g_min_dense;          /* Value of min_dense storage for -g */
    unsigned single_min_dense;     /* Value of min_dense storage for -k: single index */
    unsigned implicit_min_dense;   /* Value of min_dense storage for -k: implicit index */
    unsigned fa_min_dense;         /* Value of min_dense storage for -k: fixed array index */
    unsigned ea_min_dense;         /* Value of min_dense storage for -k: extensible array index */
    unsigned bt2_min_dense;        /* Value of min_dense storage for -k: version 2 btree index */
} dsets_state_t;

/* Initializations for dsets_state_t */
#define DSETS_INITIALIZER                                                                                    \
    (dsets_state_t)                                                                                          \
    {                                                                                                        \
        .compact_did = H5I_INVALID_HID, .contig_did = H5I_INVALID_HID, .single_did = H5I_INVALID_HID,        \
        .implicit_did = H5I_INVALID_HID, .fa_did = H5I_INVALID_HID, .ea_did = H5I_INVALID_HID,               \
        .bt2_did = H5I_INVALID_HID, .p_max_compact = 0, .g_max_compact = 0, .single_max_compact = 0,         \
        .implicit_max_compact = 0, .fa_max_compact = 0, .ea_max_compact = 0, .bt2_max_compact = 0            \
    }

/* Structure to hold info for named pipes */
typedef struct {
    const char *fifo_writer_to_reader; /* Name of fifo for writer to reader */
    const char *fifo_reader_to_writer; /* Name of fifo for reader to writer */
    int         fd_writer_to_reader;   /* File ID for fifo from writer to reader */
    int         fd_reader_to_writer;   /* File ID for fifo from reader to writer */
    int         notify;                /* Value to notify between writer and reader */
    int         verify;                /* Value to verify between writer and reader */
} np_state_t;

/* Initializations for np_state_t */
#define NP_INITIALIZER                                                                                       \
    (np_state_t)                                                                                             \
    {                                                                                                        \
        .fifo_writer_to_reader = "./fifo_attrdset_writer_to_reader",                                         \
        .fifo_reader_to_writer = "./fifo_attrdset_reader_to_writer", .fd_writer_to_reader = -1,              \
        .fd_reader_to_writer = -1, .notify = 0, .verify = 0                                                  \
    }

static herr_t state_init(state_t *s, int argc, const char *const *argv);

static herr_t np_init(np_state_t *np, hbool_t writer);
static herr_t np_close(np_state_t *np, hbool_t writer);
static herr_t np_writer(hbool_t result, unsigned step, const state_t *s, np_state_t *np,
                        H5F_vfd_swmr_config_t *config);
static herr_t np_reader(hbool_t result, unsigned step, const state_t *s, np_state_t *np);
static herr_t np_confirm_verify_notify(int fd, unsigned step, const state_t *s, np_state_t *np);
static herr_t np_reader_no_verification(const state_t *s, np_state_t *np, H5F_vfd_swmr_config_t *config);

static herr_t create_dsets(const state_t *s, dsets_state_t *ds);
static herr_t open_dsets(const state_t *s, dsets_state_t *ds);
static herr_t open_dset_real(hid_t fid, hid_t *did, const char *name, unsigned *max_compact,
                             unsigned *min_dense);
static herr_t close_dsets(const dsets_state_t *ds);

static herr_t perform_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config,
                                       np_state_t *np);
static herr_t attr_dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which);
static herr_t attr_action(unsigned action, const state_t *s, hid_t did, unsigned which);
static herr_t add_attr(const state_t *s, hid_t did, unsigned int which);
static herr_t modify_attr(const state_t *s, hid_t did, unsigned int which);
static herr_t delete_attr(hid_t did, unsigned int which);

static herr_t verify_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config,
                                      np_state_t *np);
static herr_t verify_attr_dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds,
                                       unsigned which);
static herr_t verify_attr_action(unsigned action, hid_t did, unsigned which);
static herr_t verify_add_or_modify_attr(unsigned action, hid_t did, char *attr_name, unsigned int which);
static herr_t verify_delete_attr(hid_t did, char *attr_name);
static herr_t verify_storage_cont(unsigned action, hid_t did, unsigned int which, unsigned max_compact,
                                  unsigned min_dense, unsigned asteps);
static herr_t verify_storage_cont_real(hid_t did, unsigned int which, unsigned cut_point);

/* Names for datasets */
#define DSET_COMPACT_NAME  "compact_dset"
#define DSET_CONTIG_NAME   "contig_dset"
#define DSET_SINGLE_NAME   "chunked_single"
#define DSET_IMPLICIT_NAME "chunked_implicit"
#define DSET_FA_NAME       "chunked_fa"
#define DSET_EA_NAME       "chunked_ea"
#define DSET_BT2_NAME      "chunked_bt2"

/* Action for attribute handling */
#define ADD_ATTR    1
#define MODIFY_ATTR 2
#define DELETE_ATTR 3

/* Test program usage info */
static void
usage(const char *progname)
{
    HDfprintf(stderr,
              "usage: %s -a nattrs [-p] [-g] [-k] [-v] [-m]\n"
              "    [-d dattrs] [-u nticks] [-c csteps] [-S] [-N]\n"
              "\n"
              "-p:	           create a dataset with compact layout\n"
              "-g:	           create a dataset with contiguous layout\n"
              "-k:	           create datasets with chunked layout for the 5 indexing types\n"
              "-m:	           modify attributes to all datasets after addition\n"
              "-v:	           add variable length attribute to datasets\n"
              "                   (default is H5T_NATIVE_UINT32)\n"
              "-a nattrs:	   add `nattrs` attributes to all datasets\n"
              "-d dattrs:	   delete `dattrs` attributes to all datasets after addition\n"
              "-u nticks:         `nticks` ticks for the reader to wait before verification\n"
              "                   (default is 4)\n"
              "-c csteps:         `csteps` steps communication interval between reader and writer\n"
              "                   (default is 1)\n"
              "-S:	           do not use VFD SWMR\n"
              "-N:	           do not use named pipes for test synchronization\n"
              "-b:	           write data in big-endian byte order if no -v option\n"
              "                   (default is H5T_NATIVE_UINT32)\n\n"
              "Note:\n"
              "1. Require to specify at least -p, -g or -k option\n"
              "2. -c <csteps> option cannot exceed -a <nattrs> option\n"
              "3. -d <dattrs> option cannot exceed -a <nattrs> option\n"
              "\n",
              progname);
    HDexit(EXIT_FAILURE);
} /* usage() */

/*
 * Initialize option info in state_t
 */
static herr_t
state_init(state_t *s, int argc, const char *const *argv)
{
    unsigned long          tmp;
    int                    opt;
    const hsize_t          dims  = 1;
    char *                 tfile = NULL;
    char *                 end;
    const char *           s_opts   = "pgkvmbqSNa:d:u:c:";
    struct h5_long_options l_opts[] = {{NULL, 0, '\0'}};

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

    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != -1) {
        switch (opt) {

            case 'p':
                s->compact = TRUE;
                break;

            case 'g':
                s->contig = TRUE;
                break;

            case 'k':
                s->chunked = TRUE;
                break;

            case 'v':
                s->vl_attr = TRUE;
                break;

            case 'm':
                s->mod_attr = TRUE;
                break;
            case 'b':
                s->filetype = H5T_STD_U32BE;
                break;
            case 'q':
                verbosity = 0;
                break;
            case 'S':
                s->use_vfd_swmr = FALSE;
                break;
            case 'N':
                s->use_np = FALSE;
                break;
            case 'a':
            case 'd':
            case 'u':
            case 'c':
                errno = 0;
                tmp   = HDstrtoul(H5_optarg, &end, 0);
                if (end == H5_optarg || *end != '\0') {
                    HDprintf("couldn't parse `-%c` argument `%s`\n", opt, H5_optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    HDprintf("couldn't parse `-%c` argument `%s`\n", opt, H5_optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    HDprintf("`-%c` argument `%lu` too large\n", opt, tmp);
                    TEST_ERROR;
                }

                if (opt == 'a')
                    s->asteps = (unsigned)tmp;
                else if (opt == 'd')
                    s->dattrs = (unsigned)tmp;
                else if (opt == 'u')
                    s->update_interval = (unsigned)tmp;
                else if (opt == 'c')
                    s->csteps = (unsigned)tmp;
                break;

            case '?':
            default:
                usage(s->progname);
                break;
        }
    }
    argc -= H5_optind;
    argv += H5_optind;

    /* Require to specify at least -p, -g or -k option */
    if (!s->compact && !s->contig && !s->chunked) {
        HDprintf("Require to specify at least -p, -g or -k option\n");
        usage(s->progname);
        goto error;
    }

    /* -c <csteps> cannot be zero */
    if (!s->csteps) {
        HDprintf("communication interval cannot be zero\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -a <nattrs> options */
    if (s->asteps && s->csteps > s->asteps) {
        HDprintf("communication interval is out of bounds\n");
        TEST_ERROR;
    }

    /* -d and -a */
    if (s->dattrs > s->asteps) {
        HDprintf("# of attributes to be deleted exceeds # of attributes created\n");
        TEST_ERROR;
    }

    /* Dataspace for attributes added to datasets */
    /* Dataspace for compact and contiguous datasets */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        HDprintf("H5Screate_simple failed\n");
        TEST_ERROR;
    }

    /* The test file name */
    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_attrdset.h5");

    return SUCCEED;

error:
    if (tfile)
        HDfree(tfile);

    return FAIL;

} /* state_init() */

/*
 *  Create the datasets as specified on the command line.
 */
static herr_t
create_dsets(const state_t *s, dsets_state_t *ds)
{
    hid_t dcpl      = H5I_INVALID_HID;
    hid_t dtid      = H5I_INVALID_HID;
    hid_t tmp_did   = H5I_INVALID_HID;
    hid_t cmpd_tid  = H5I_INVALID_HID;
    hid_t array_tid = H5I_INVALID_HID;
    hid_t vl_tid    = H5I_INVALID_HID;
    hid_t sid       = H5I_INVALID_HID;

    *ds = DSETS_INITIALIZER;

    /* Dataset with compact layout, compound datatype */
    if (s->compact) {
        const hsize_t dims = 2;
        typedef struct {
            int a;
            int b[2];
        } cmpd;
        cmpd wdata; /* Data for compact dataset */

        wdata.a    = 1;
        wdata.b[0] = 2;
        wdata.b[1] = 3;

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }
        if (H5Pset_layout(dcpl, H5D_COMPACT) < 0) {
            HDprintf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        /* Create compound datatype */
        if ((cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(cmpd))) < 0) {
            HDprintf("H5Tcreate failed\n");
            TEST_ERROR;
        }

        /* Create the array for the second element in the compound type */
        if ((array_tid = H5Tarray_create2(H5T_NATIVE_INT, 1, &dims)) < 0) {
            HDprintf("H5Tarray_create2 failed\n");
            TEST_ERROR;
        }

        /* First element in the compound type */
        if (H5Tinsert(cmpd_tid, "a", HOFFSET(cmpd, a), H5T_NATIVE_INT) < 0) {
            HDprintf("H5Tinsert failed\n");
            TEST_ERROR;
        }
        /* Second element in the compound type */
        if (H5Tinsert(cmpd_tid, "b", HOFFSET(cmpd, b), array_tid) < 0) {
            HDprintf("H5Tinsert failed\n");
            TEST_ERROR;
        }

        /* Create the compact dataset with compound datatype */
        if ((ds->compact_did = H5Dcreate2(s->file, DSET_COMPACT_NAME, cmpd_tid, s->one_by_one_sid,
                                          H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 compact dataset failed\n");
            TEST_ERROR;
        }

        /* Write data to the dataset */
        if (H5Dwrite(ds->compact_did, cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0) {
            HDprintf("H5Dwrite to compact dataset failed\n");
            TEST_ERROR;
        }

        /* In order to trigger continuation block if -p is used alone by itself */
        if ((tmp_did = H5Dcreate2(s->file, "JUNK_IGNORE", cmpd_tid, s->one_by_one_sid, H5P_DEFAULT,
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 failed\n");
            TEST_ERROR;
        }
        if (H5Dclose(tmp_did) < 0) {
            HDprintf("H5Dclose failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with contiguous layout, early allocation, non-default attr phase change, named datatype */
    if (s->contig) {
        int      wdata1          = 9; /* Data for contiguous dataset */
        unsigned def_max_compact = 0;
        unsigned def_min_dense   = 0;

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_layout(dcpl, H5D_CONTIGUOUS) < 0) {
            HDprintf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) {
            HDprintf("H5Pset_alloc_time failed\n");
            TEST_ERROR;
        }

        if (H5Pget_attr_phase_change(dcpl, &def_max_compact, &def_min_dense) < 0) {
            HDprintf("H5Pget_attr_phase_change failed\n");
            TEST_ERROR;
        }

        if (H5Pset_attr_phase_change(dcpl, def_max_compact + 2, def_min_dense + 2) < 0) {
            HDprintf("H5Pset_attr_phase_change failed\n");
            TEST_ERROR;
        }

        /* Create the named datatype */
        if ((dtid = H5Tcopy(H5T_NATIVE_INT)) < 0) {
            HDprintf("H5Tcopy failed\n");
            TEST_ERROR;
        }

        if (H5Tcommit2(s->file, "named_dtype", dtid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
            HDprintf("H5Tcommit2 failed\n");
            TEST_ERROR;
        }

        /* Create the contiguous dataset with the named datatype */
        if ((ds->contig_did = H5Dcreate2(s->file, DSET_CONTIG_NAME, dtid, s->one_by_one_sid, H5P_DEFAULT,
                                         dcpl, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 contiguous dataset failed\n");
            TEST_ERROR;
        }

        /* Write to the dataset */
        if (H5Dwrite(ds->contig_did, dtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata1) < 0) {
            HDprintf("H5Dwrite to contiguous dataset failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }

        if (H5Tclose(dtid) < 0) {
            HDprintf("H5Tclose failed\n");
            TEST_ERROR;
        }
    }

    /* Datasets with the 5 indexes: single, implicit, fa, ea, bt2 */
    /* All with variable length datatype */
    if (s->chunked) {

        /* For index: single, implicit and fa */
        hsize_t dims1[1]       = {5};
        hsize_t max_dims1[1]   = {100};
        hsize_t chunk_dims1[1] = {2};

        /* The variable length data */
        const char *vdata[5] = {"one", "two", "three", "four", "five"};

        /* For index: ea and bt2 */
        hsize_t     dims2[2]       = {5, 5};
        hsize_t     max_dims2[2]   = {100, H5S_UNLIMITED};
        hsize_t     chunk_dims2[2] = {2, 2};
        const char *vdata2[5][5]   = {{"one", "two", "three", "four", "five"},
                                    {"two", "three", "four", "five", "six"},
                                    {"three", "four", "five", "six", "seven"},
                                    {"four", "five", "six", "seven", "eight"},
                                    {"five", "six", "seven", "eight", "nine"}};

        /* Create variable length datatype */
        if ((vl_tid = H5Tcopy(H5T_C_S1)) < 0) {
            HDprintf("H5Tcopy failed\n");
            TEST_ERROR;
        }

        if (H5Tset_size(vl_tid, H5T_VARIABLE) < 0) {
            HDprintf("H5Tset_size failed\n");
            TEST_ERROR;
        }

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0) {
            HDprintf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl, 1, dims1) < 0) {
            HDprintf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        /* Create 1-D chunked dataset with single index */
        /* Chunked, dims=max_dims=chunk_dims */

        if ((sid = H5Screate_simple(1, dims1, dims1)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((ds->single_did =
                 H5Dcreate2(s->file, DSET_SINGLE_NAME, vl_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 chunked dataset: single index failed\n");
            TEST_ERROR;
        }

        if (H5Dwrite(ds->single_did, vl_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &vdata) < 0) {
            HDprintf("H5Dwrite to chunked dataset: single index failed\n");
            TEST_ERROR;
        }

        /* Create 1-D chunked dataset with implicit index */
        /* Chunked, dims=max_dims, early allocation */

        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) {
            HDprintf("H5Pset_alloc_time\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl, 1, chunk_dims1) < 0) {
            HDprintf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        if ((ds->implicit_did =
                 H5Dcreate2(s->file, DSET_IMPLICIT_NAME, vl_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 chunked dataset: implicit index failed\n");
            TEST_ERROR;
        }

        if (H5Dwrite(ds->implicit_did, vl_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &vdata) < 0) {
            HDprintf("H5Dwrite to chunked dataset: implicit index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            HDprintf("H5Sclose failed\n");
            TEST_ERROR;
        }

        /* Create 1-D chunked dataset with fixed array index */
        /* Chunked, fixed max_dims */

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl, 1, chunk_dims1) < 0) {
            HDprintf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        if ((sid = H5Screate_simple(1, dims1, max_dims1)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((ds->fa_did = H5Dcreate2(s->file, DSET_FA_NAME, vl_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) <
            0) {
            HDprintf("H5Dcreaet2 chunked dataset: fa index failed\n");
            TEST_ERROR;
        }

        if (H5Dwrite(ds->fa_did, vl_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &vdata) < 0) {
            HDprintf("H5Dwrite to chunked dataset: fa index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            HDprintf("H5Sclose failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with extensible array index */
        /* Chunked, 1 unlimited max_dims */

        if ((sid = H5Screate_simple(2, dims2, max_dims2)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl, 2, chunk_dims2) < 0) {
            HDprintf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        if ((ds->ea_did = H5Dcreate2(s->file, DSET_EA_NAME, vl_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) <
            0) {
            HDprintf("H5Dcreate2 chunked dataset: ea index failed\n");
            TEST_ERROR;
        }

        if (H5Dwrite(ds->ea_did, vl_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &vdata2) < 0) {
            HDprintf("H5Dwrite to chunked dataset: ea index failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with bt2 index */
        /* Chunked, 2 unlimited max_dims */
        max_dims2[0] = H5S_UNLIMITED;

        if ((sid = H5Screate_simple(2, dims2, max_dims2)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((ds->bt2_did = H5Dcreate2(s->file, DSET_BT2_NAME, vl_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) <
            0) {
            HDprintf("H5Dcreate2 chunked dataset: bt2 index failed\n");
            TEST_ERROR;
        }

        if (H5Dwrite(ds->bt2_did, vl_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &vdata2) < 0) {
            HDprintf("H5Dwrite to chunked dataset: bt2 index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            HDprintf("H5Sclose failed\n");
            TEST_ERROR;
        }

        if (H5Tclose(vl_tid) < 0) {
            HDprintf("H5Tclose failed\n");
            TEST_ERROR;
        }
    }

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Tclose(cmpd_tid);
        H5Tclose(array_tid);
        H5Tclose(dtid);
        H5Tclose(vl_tid);
        H5Sclose(sid);
        H5Dclose(ds->compact_did);
        H5Dclose(tmp_did);
        H5Dclose(ds->contig_did);
        H5Dclose(ds->single_did);
        H5Dclose(ds->implicit_did);
        H5Dclose(ds->fa_did);
        H5Dclose(ds->ea_did);
        H5Dclose(ds->bt2_did);
    }
    H5E_END_TRY;

    return FAIL;

} /* create_dsets() */

/*
 * Open the datasets as specified.
 */
static herr_t
open_dsets(const state_t *s, dsets_state_t *ds)
{
    *ds = DSETS_INITIALIZER;

    if (s->compact) {
        if (!open_dset_real(s->file, &ds->compact_did, DSET_COMPACT_NAME, &ds->p_max_compact,
                            &ds->p_min_dense)) {
            HDprintf("open_dset_real() for compact dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->contig) {
        if (!open_dset_real(s->file, &ds->contig_did, DSET_CONTIG_NAME, &ds->g_max_compact,
                            &ds->g_min_dense)) {
            HDprintf("open_dset_real() for contiguous dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->chunked) {
        if (!open_dset_real(s->file, &ds->single_did, DSET_SINGLE_NAME, &ds->single_max_compact,
                            &ds->single_min_dense)) {
            HDprintf("open_dset_real() for chunked dataset: single failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s->file, &ds->implicit_did, DSET_IMPLICIT_NAME, &ds->implicit_max_compact,
                            &ds->implicit_min_dense)) {
            HDprintf("open_dset_real() for chunked dataset: implicit failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s->file, &ds->fa_did, DSET_FA_NAME, &ds->fa_max_compact, &ds->fa_min_dense)) {
            HDprintf("open_dset_real() for chunked dataset: fa failed\n");
            TEST_ERROR;
        }
        if (!open_dset_real(s->file, &ds->ea_did, DSET_FA_NAME, &ds->ea_max_compact, &ds->ea_min_dense)) {
            HDprintf("open_dset_real() for chunked dataset: ea failed\n");
            TEST_ERROR;
        }
        if (!open_dset_real(s->file, &ds->bt2_did, DSET_BT2_NAME, &ds->bt2_max_compact, &ds->bt2_min_dense)) {
            HDprintf("open_dset_real() for chunked dataset: bt2 failed\n");
            TEST_ERROR;
        }
    }

    return SUCCEED;

error:
    return FAIL;

} /* open_dsets() */

/*
 * Do the real work of opening the dataset.
 * Retrieve the max_compact and min_dense values for the dataset.
 */
static herr_t
open_dset_real(hid_t fid, hid_t *did, const char *name, unsigned *max_compact, unsigned *min_dense)
{
    hid_t dcpl = H5I_INVALID_HID;

    if ((*did = H5Dopen2(fid, name, H5P_DEFAULT)) < 0) {
        HDprintf("H5Dopen dataset failed\n");
        TEST_ERROR;
    }

    if ((dcpl = H5Dget_create_plist(*did)) < 0) {
        HDprintf("H5Dget_create_plist failed\n");
        TEST_ERROR;
    }

    if (H5Pget_attr_phase_change(dcpl, max_compact, min_dense) < 0) {
        HDprintf("H5Dget_attr_phase_change failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(dcpl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(*did);
        H5Pclose(dcpl);
    }
    H5E_END_TRY;

    return FAIL;
} /* open_dset_real() */

/*
 * Close all the datasets as specified.
 */
static herr_t
close_dsets(const dsets_state_t *ds)
{
    if (ds->compact_did != H5I_INVALID_HID && H5Dclose(ds->compact_did) < 0) {
        HDprintf("H5Dclose compact dataset failed\n");
        TEST_ERROR;
    }

    if (ds->contig_did != H5I_INVALID_HID && H5Dclose(ds->contig_did) < 0) {
        HDprintf("H5Dclose contig dataset failed\n");
        TEST_ERROR;
    }

    if (ds->single_did != H5I_INVALID_HID && H5Dclose(ds->single_did) < 0) {
        HDprintf("H5Dclose chunked dataset: single index failed\n");
        TEST_ERROR;
    }

    if (ds->implicit_did != H5I_INVALID_HID && H5Dclose(ds->implicit_did) < 0) {
        HDprintf("H5Dclose chunked dataset: implicit index failed\n");
        TEST_ERROR;
    }

    if (ds->fa_did >= 0 && H5Dclose(ds->fa_did) < 0) {
        HDprintf("H5Dclose chunked dataset: fa index failed\n");
        TEST_ERROR;
    }

    if (ds->ea_did >= 0 && H5Dclose(ds->ea_did) < 0) {
        HDprintf("H5Dclose chunked dataset: ea index failed\n");
        TEST_ERROR;
    }

    if (ds->bt2_did >= 0 && H5Dclose(ds->bt2_did) < 0) {
        HDprintf("H5Dclose chunked dataset: bt2 index failed\n");
        TEST_ERROR;
    }

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(ds->compact_did);
        H5Dclose(ds->contig_did);
        H5Dclose(ds->single_did);
        H5Dclose(ds->implicit_did);
        H5Dclose(ds->fa_did);
        H5Dclose(ds->ea_did);
        H5Dclose(ds->bt2_did);
    }
    H5E_END_TRY;

    return FAIL;
} /* close_dsets() */

/*
 *  Writer
 */

/*
 * Perform the attribute operations specified on the command line.
 *      ADD_ATTR    : -a <nattrs> option
 *      MODIFY_ATTR : -m option
 *      DELETE_ATTR : -d <dattrs> option
 */
static herr_t
perform_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np)
{
    unsigned step;
    hbool_t  result;
    unsigned dd;

    for (step = 0; step < s->asteps; step++) {
        dbgf(2, "Adding attribute %d\n", step);

        result = attr_dsets_action(ADD_ATTR, s, ds, step);

        if (s->use_np && !np_writer(result, step, s, np, config)) {
            HDprintf("np_writer() for addition failed\n");
            TEST_ERROR;
        }
    }

    if (s->mod_attr) {

        /* Need to sync up writer/reader before moving onto the next phase */
        if (s->use_np && !np_writer(TRUE, 0, s, np, config)) {
            HDprintf("np_writer() for modification failed\n");
            TEST_ERROR;
        }

        /* Start modification */
        for (step = 0; step < s->asteps; step++) {
            dbgf(2, "Modifying attribute %d\n", step);

            result = attr_dsets_action(MODIFY_ATTR, s, ds, step);

            if (s->use_np && !np_writer(result, step, s, np, config)) {
                HDprintf("np_writer() for modification failed\n");
                TEST_ERROR;
            }
        }
    }

    if (s->dattrs) {

        /* Need to sync up writer/reader before moving onto the next phase */
        if (s->use_np && !np_writer(TRUE, 0, s, np, config)) {
            HDprintf("np_writer() for deletion failed\n");
            TEST_ERROR;
        }

        /* Start deletion */
        for (dd = 0, step = s->asteps - 1; dd < s->dattrs; dd++, --step) {
            dbgf(2, "Deleting attribute %d\n", step);

            result = attr_dsets_action(DELETE_ATTR, s, ds, step);

            if (s->use_np && !np_writer(result, step, s, np, config)) {
                HDprintf("np_writer() for deletion failed\n");
                TEST_ERROR;
            }
        }
    }

    return SUCCEED;

error:
    return FAIL;

} /* perform_dsets_operations() */

/*
 * Perform the "action" for each of the datasets specified on the command line.
 *      -p: compact dataset
 *      -g: contiguous dataset
 *      -k: 5 chunked datasets with 5 indexing types
 */
static herr_t
attr_dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which)
{
    int     nerrors = 0;
    hbool_t ret     = TRUE;

    if (s->compact) {
        HDassert(ds->compact_did != H5I_INVALID_HID);
        dbgf(2, "to compact dataset\n");
        if (!attr_action(action, s, ds->compact_did, which))
            ++nerrors;
    }

    if (s->contig) {
        HDassert(ds->contig_did != H5I_INVALID_HID);
        dbgf(2, "to contiguous dataset\n");
        if (!attr_action(action, s, ds->contig_did, which))
            ++nerrors;
    }

    if (s->chunked) {
        HDassert(ds->single_did != H5I_INVALID_HID);
        dbgf(2, "to chunked dataset: single index\n");
        if (!attr_action(action, s, ds->single_did, which))
            ++nerrors;

        HDassert(ds->implicit_did != H5I_INVALID_HID);
        dbgf(2, "to chunked dataset: implicit index\n");
        if (!attr_action(action, s, ds->implicit_did, which))
            ++nerrors;

        HDassert(ds->fa_did != H5I_INVALID_HID);
        dbgf(2, "to chunked dataset: fixed array index\n");
        if (!attr_action(action, s, ds->fa_did, which))
            ++nerrors;

        HDassert(ds->ea_did != H5I_INVALID_HID);
        dbgf(2, "to chunked dataset: extensible array index\n");
        if (!attr_action(action, s, ds->ea_did, which))
            ++nerrors;

        HDassert(ds->bt2_did != H5I_INVALID_HID);
        dbgf(2, "to chunked dataset: version 2 btree index\n");
        if (!attr_action(action, s, ds->bt2_did, which))
            ++nerrors;
    }

    if (nerrors)
        ret = FALSE;

    return (ret);

} /* attr_dsets_action() */

/*
 * Perform the action on the specified dataset.
 *      ADD_ATTR    : add `which` attribute
 *      MODIFY_ATTR : modify `which` attribute
 *      DELETE_ATTR : delete `which` attribute
 */
static herr_t
attr_action(unsigned action, const state_t *s, hid_t did, unsigned which)
{
    hbool_t ret;

    switch (action) {
        case ADD_ATTR:
            ret = add_attr(s, did, which);
            break;

        case MODIFY_ATTR:
            ret = modify_attr(s, did, which);
            break;

        case DELETE_ATTR:
            ret = delete_attr(did, which);
            break;

        default:
            HDassert(0 && "Unknown action?!?");
    } /* end switch */

    return ret;

} /* attr_action() */

/*
 * Add an attribute to the specified dataset.
 * The datatype can be:
 *      variable length (-v) or
 *      H5T_NATIVE_UINT32 (-b) or
 *      H5T_NATIVE_UINT32 (default)
 */
static herr_t
add_attr(const state_t *s, hid_t did, unsigned int which)
{
    hid_t aid    = H5I_INVALID_HID;
    hid_t tid    = H5I_INVALID_HID;
    hid_t vl_tid = H5I_INVALID_HID;
    char  name[sizeof("attr-9999999999")];
    char *val = NULL;

    esnprintf(name, sizeof(name), "attr-%u", which);

    if (s->vl_attr) {
        if ((vl_tid = H5Tcopy(H5T_C_S1)) < 0) {
            HDprintf("H5Tcopy failed\n");
            TEST_ERROR;
        }

        if (H5Tset_size(vl_tid, H5T_VARIABLE) < 0) {
            HDprintf("H5Tset_size failed\n");
            TEST_ERROR;
        }

        if ((val = HDmalloc(sizeof("9999999999"))) == NULL) {
            HDprintf("H5Dmalloc failed\n");
            TEST_ERROR;
        }

        HDsprintf(val, "%u", which);

        tid = vl_tid;
    }
    else
        tid = s->filetype;

    /* Attach the attribute to the dataset */
    if ((aid = H5Acreate2(did, name, tid, s->one_by_one_sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDprintf("H5Acreate2 failed\n");
        TEST_ERROR;
    }

    /* Write to the attribute */
    if (H5Awrite(aid, tid, s->vl_attr ? &val : (const void *)&which) < 0) {
        HDprintf("H5Awrite failed\n");
        TEST_ERROR;
    }

    /* Close the attribute */
    if (H5Aclose(aid) < 0) {
        HDprintf("H5Aclose failed\n");
        TEST_ERROR;
    }

    if (vl_tid >= 0 && H5Tclose(vl_tid) < 0) {
        HDprintf("H5Tclose failed\n");
        TEST_ERROR;
    }

    if (val)
        HDfree(val);

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(vl_tid);
        H5Aclose(aid);
    }
    H5E_END_TRY;

    if (val)
        HDfree(val);

    return FAIL;

} /* add_attr() */

/*
 * Modify the attribute data.
 */
static herr_t
modify_attr(const state_t *s, hid_t did, unsigned int which)
{
    hid_t    aid    = H5I_INVALID_HID;
    hid_t    tid    = H5I_INVALID_HID;
    hid_t    vl_tid = H5I_INVALID_HID;
    char     name[sizeof("attr-9999999999")];
    char *   val     = NULL;
    unsigned tmp_val = 0;

    esnprintf(name, sizeof(name), "attr-%u", which);

    if (s->vl_attr) {
        if ((vl_tid = H5Tcopy(H5T_C_S1)) < 0) {
            HDprintf("H5Tcopy failed\n");
            TEST_ERROR;
        }

        if (H5Tset_size(vl_tid, H5T_VARIABLE) < 0) {
            HDprintf("H5Tset_size failed\n");
            TEST_ERROR;
        }

        /* Needs to fit "%u %c", below */
        if ((val = HDmalloc(10 + 3)) == NULL) {
            HDprintf("HDmalloc failed\n");
            TEST_ERROR;
        }

        HDsprintf(val, "%u %c", which, 'M');

        tid = vl_tid;
    }
    else {
        tid     = s->filetype;
        tmp_val = which + 1;
    }

    /* Open the attribute to the dataset */
    if ((aid = H5Aopen(did, name, H5P_DEFAULT)) < 0) {
        HDprintf("H5Aopen failed\n");
        TEST_ERROR;
    }

    /* Write to the attribute */
    if (H5Awrite(aid, tid, s->vl_attr ? &val : (const void *)&tmp_val) < 0) {
        HDprintf("H5Awrite failed\n");
        TEST_ERROR;
    }

    /* Close the attribute */
    if (H5Aclose(aid) < 0) {
        HDprintf("H5Aclose failed\n");
        TEST_ERROR;
    }

    if (vl_tid >= 0 && H5Tclose(vl_tid) < 0) {
        HDprintf("H5Tclose failed\n");
        TEST_ERROR;
    }

    if (val)
        HDfree(val);

    return SUCCEED;
error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(vl_tid);
    }
    H5E_END_TRY;

    if (val)
        HDfree(val);

    return FAIL;
} /* modify_attr() */

/*
 * Delete the attribute
 */
static herr_t
delete_attr(hid_t did, unsigned int which)
{
    char name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", which);

    /* Delete the attribute to the dataset */
    if (H5Adelete(did, name) < 0) {
        HDprintf("H5Adelete failed\n");
        TEST_ERROR;
    }

    return SUCCEED;

error:
    return FAIL;

} /* delete_attr() */

/*
 * Verification by the reader
 */

/*
 * Verify the attribute operations specified on the command line:
 *      ADD_ATTR    : -a <nattrs> option
 *      MODIFY_ATTR : -m option
 *      DELETE_ATTR : -d <dattrs> option
 *
 * Also verify continuation block and compact<->dense storage if:
 *      --[-c <csteps>] is 1
 *      --not applicable for -m option
 */
static herr_t
verify_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np)
{
    unsigned step;
    hbool_t  result;
    unsigned dd;

    /* Start verifying addition */
    for (step = 0; step < s->asteps; step++) {
        dbgf(2, "Verifying...attribute %d\n", step);

        if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
            HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
            TEST_ERROR;
        }

        /* Wait for a few ticks for the update to happen */
        decisleep(config->tick_len * s->update_interval);

        result = verify_attr_dsets_action(ADD_ATTR, s, ds, step);

        if (s->use_np && !np_reader(result, step, s, np)) {
            HDprintf("np_reader() for verifying addition failed\n");
            TEST_ERROR;
        }
    }

    if (s->mod_attr) {
        /* Need to sync up writer/reader before moving onto the next phase */
        if (!np_reader_no_verification(s, np, config)) {
            HDprintf("np_reader_no_verification() for verifying modification failed\n");
            TEST_ERROR;
        }

        /* Start verifying modification */
        for (step = 0; step < s->asteps; step++) {
            dbgf(2, "Verifying...modify attribute %d\n", step);

            if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }

            /* Wait for a few ticks for the update to happen */
            decisleep(config->tick_len * s->update_interval);

            result = verify_attr_dsets_action(MODIFY_ATTR, s, ds, step);

            if (s->use_np && !np_reader(result, step, s, np)) {
                HDprintf("np_reader() for verifying modification failed\n");
                TEST_ERROR;
            }
        }
    }

    if (s->dattrs) {

        /* Need to sync up writer/reader before moving onto the next phase */
        if (!np_reader_no_verification(s, np, config)) {
            HDprintf("np_reader_no_verification() for verifying modification failed\n");
            TEST_ERROR;
        }

        /* Start verifying deletion */
        for (dd = 0, step = s->asteps - 1; dd < s->dattrs; dd++, --step) {
            dbgf(2, "Verifying...delete attribute %d\n", step);

            if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }

            /* Wait for a few ticks for the update to happen */
            decisleep(config->tick_len * s->update_interval);

            result = verify_attr_dsets_action(DELETE_ATTR, s, ds, step);

            if (s->use_np && !np_reader(result, step, s, np)) {
                HDprintf("np_reader() for verifying deletion failed\n");
                TEST_ERROR;
            }
        }
    }

    return SUCCEED;

error:

    return FAIL;
} /* verify_dsets_operations() */

/*
 * Verify the "action" for each of the datasets specified on the command line.
 *      -p: compact dataset
 *      -g: contiguous dataset
 *      -k: 5 chunked datasets with 5 indexing types
 */
static herr_t
verify_attr_dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which)
{
    int     nerrors = 0;
    hbool_t ret     = TRUE;

    if (s->compact) {
        HDassert(ds->compact_did != H5I_INVALID_HID);
        dbgf(2, "Verifying attribute to compact dataset\n");
        if (!verify_attr_action(action, ds->compact_did, which))
            ++nerrors;
        if (s->csteps == 1 && (action != MODIFY_ATTR)) {
            if (!verify_storage_cont(action, ds->compact_did, which, ds->p_max_compact, ds->p_min_dense,
                                     s->asteps))
                ++nerrors;
        }
    }

    if (s->contig) {
        HDassert(ds->contig_did != H5I_INVALID_HID);
        dbgf(2, "Verifying attribute to contiguous dataset\n");
        if (!verify_attr_action(action, ds->contig_did, which))
            ++nerrors;
        if (s->csteps == 1 && (action != MODIFY_ATTR)) {
            if (!verify_storage_cont(action, ds->contig_did, which, ds->g_max_compact, ds->g_min_dense,
                                     s->asteps))
                ++nerrors;
        }
    }

    if (s->chunked) {
        HDassert(ds->single_did != H5I_INVALID_HID);
        dbgf(2, "Verifying attribute to chunked dataset: single indedx\n");
        if (!verify_attr_action(action, ds->single_did, which))
            ++nerrors;
        if (s->csteps == 1 && (action != MODIFY_ATTR)) {
            if (!verify_storage_cont(action, ds->single_did, which, ds->single_max_compact,
                                     ds->single_min_dense, s->asteps))
                ++nerrors;
        }

        HDassert(ds->implicit_did != H5I_INVALID_HID);
        dbgf(2, "Verifying attribute to chunked dataset: implicit index\n");
        if (!verify_attr_action(action, ds->implicit_did, which))
            ++nerrors;
        if (s->csteps == 1 && (action != MODIFY_ATTR)) {
            if (!verify_storage_cont(action, ds->implicit_did, which, ds->implicit_max_compact,
                                     ds->implicit_min_dense, s->asteps))
                ++nerrors;
        }

        HDassert(ds->fa_did != H5I_INVALID_HID);
        dbgf(2, "Verifying attribute to chunked dataset: fa index\n");
        if (!verify_attr_action(action, ds->fa_did, which))
            ++nerrors;
        if (s->csteps == 1 && (action != MODIFY_ATTR)) {
            if (!verify_storage_cont(action, ds->fa_did, which, ds->fa_max_compact, ds->fa_min_dense,
                                     s->asteps))
                ++nerrors;
        }

        HDassert(ds->ea_did != H5I_INVALID_HID);
        dbgf(2, "Verifying attribute to chunked dataset: ea index\n");
        if (!verify_attr_action(action, ds->ea_did, which))
            ++nerrors;
        if (s->csteps == 1 && (action != MODIFY_ATTR)) {
            if (!verify_storage_cont(action, ds->ea_did, which, ds->ea_max_compact, ds->ea_min_dense,
                                     s->asteps))
                ++nerrors;
        }

        HDassert(ds->bt2_did != H5I_INVALID_HID);
        dbgf(2, "Verifying attribute to chunked dataset: bt2 index\n");
        if (!verify_attr_action(action, ds->bt2_did, which))
            ++nerrors;
        if (s->csteps == 1 && (action != MODIFY_ATTR)) {
            if (!verify_storage_cont(action, ds->bt2_did, which, ds->bt2_max_compact, ds->bt2_min_dense,
                                     s->asteps))
                ++nerrors;
        }
    }

    if (nerrors)
        ret = FALSE;

    return (ret);

} /* verify_attr_dsets_action() */

/*
 * Verify the attribute action on the specified dataset.
 */
static herr_t
verify_attr_action(unsigned action, hid_t did, unsigned which)
{
    char    name[sizeof("attr-9999999999")];
    hbool_t ret;

    esnprintf(name, sizeof(name), "attr-%u", which);

    switch (action) {
        case ADD_ATTR:
            ret = verify_add_or_modify_attr(action, did, name, which);
            break;

        case MODIFY_ATTR:
            ret = verify_add_or_modify_attr(action, did, name, which);
            break;

        case DELETE_ATTR:
            ret = verify_delete_attr(did, name);
            break;

        default:
            HDassert(0 && "Unknown action?!?");
    } /* end switch */

    return ret;
} /* verify_attr_action() */

/*
 * Verify the attribute is added or modified
 */
static herr_t
verify_add_or_modify_attr(unsigned action, hid_t did, char *attr_name, unsigned int which)
{
    unsigned int read_which;
    unsigned int tmp_val;
    char         tmp_vl_val[sizeof("attr-9999999999")];
    char *       read_vl_which;
    hbool_t      is_vl = FALSE;
    hid_t        aid   = H5I_INVALID_HID;
    hid_t        atid  = H5I_INVALID_HID;
    hbool_t      ret   = FALSE;

    HDassert(did != H5I_INVALID_HID);
    HDassert(action == ADD_ATTR || action == MODIFY_ATTR);

    if ((aid = H5Aopen(did, attr_name, H5P_DEFAULT)) < 0) {
        HDprintf("H5Aopen failed\n");
        TEST_ERROR;
    }

    if ((atid = H5Aget_type(aid)) < 0) {
        HDprintf("H5Aget_type failed\n");
        TEST_ERROR;
    }

    if ((is_vl = H5Tis_variable_str(atid))) {
        if (action == ADD_ATTR)
            HDsprintf(tmp_vl_val, "%u", which);
        else
            HDsprintf(tmp_vl_val, "%u %c", which, 'M');
    }
    else {
        if (action == MODIFY_ATTR)
            tmp_val = which + 1;
        else
            tmp_val = which;
    }

    if (H5Aread(aid, atid, is_vl ? &read_vl_which : (void *)&read_which) < 0) {
        HDprintf("H5Aread failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        HDprintf("H5Aclose failed\n");
        TEST_ERROR;
    }

    if (H5Tclose(atid) < 0) {
        HDprintf("H5Tclose failed\n");
        TEST_ERROR;
    }

    if (is_vl) {
        dbgf(2, "read_vl_which = %s, tmp_vl_val= %s\n", read_vl_which, tmp_vl_val);
        if (!HDstrcmp(read_vl_which, tmp_vl_val))
            ret = TRUE;
    }
    else {
        dbgf(2, "read_which = %u, tmp_val = %u\n", read_which, tmp_val);
        ret = (read_which == tmp_val);
    }

    if (is_vl)
        H5free_memory(read_vl_which);

    return ret;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Tclose(atid);
    }
    H5E_END_TRY;

    if (is_vl)
        H5free_memory(read_vl_which);

    return FAIL;

} /* verify_add_or_modify_attr() */

/*
 * Verify the attribute does not exist.
 */
static herr_t
verify_delete_attr(hid_t did, char *attr_name)
{
    int ret;

    if ((ret = H5Aexists(did, attr_name)) < 0) {
        HDprintf("H5Aexists failed\n");
        TEST_ERROR;
    }
    else if (!ret) /* attribute does not exist */
        ret = TRUE;
    else /* attribute exist */
        ret = FALSE;

    return ret;

error:
    return FAIL;

} /* verify_delete_attr() */

/*
 * `which` is indexed by 0, 1, 2, 3...
 *
 * Checkpoints:
 *      --`which` is 0: no continuation block
 *
 *      For addition:
 *      For deletion but [-a <nattrs>] is not yet to dense storage:
 *          --`which` is at (max_compact - 1): compact storage, continuation block exists
 *          --`which` is at max_compact: dense storage, no continuation block
 *      For deletion:
 *          --`which` is at min_dense: dense storage, no continuation block
 *          --`which` is at (min_dense - 1): compact storage, continuation block exists
 */
static herr_t
verify_storage_cont(unsigned action, hid_t did, unsigned int which, unsigned max_compact, unsigned min_dense,
                    unsigned asteps)
{
    hbool_t ret = TRUE;

    HDassert(action == ADD_ATTR || action == DELETE_ATTR);

    /* Verify no cont */
    if (!which)
        ret = verify_storage_cont_real(did, which, max_compact);

    /* For addition: */
    /* For deletion, if [-a <nattrs>] is not yet to dense storage */
    else if (action == ADD_ATTR || (action == DELETE_ATTR && asteps <= max_compact)) {

        /* Verify compact storage & cont */
        if (which == (max_compact - 1))
            ret = verify_storage_cont_real(did, which, max_compact);

        /* Verify dense storage & no cont */
        else if (which == max_compact)
            ret = verify_storage_cont_real(did, which, max_compact);
    }
    /* For deletion */
    else if (action == DELETE_ATTR) {

        /* Verify compact storage & cont */
        if (which == (min_dense - 1))
            ret = verify_storage_cont_real(did, which, min_dense);

        /* Verify dense storage & no cont */
        else if (which == min_dense)
            ret = verify_storage_cont_real(did, which, min_dense);
    }

    return ret;

} /* verify_storage_cont() */

/*
 * Verify the storage condition at the specific checkpoint
 */
static herr_t
verify_storage_cont_real(hid_t did, unsigned int which, unsigned cut_point)
{
    H5O_native_info_t ninfo;

    /* Get the object information */
    if (H5Oget_native_info(did, &ninfo, H5O_NATIVE_INFO_HDR | H5O_NATIVE_INFO_META_SIZE) < 0) {
        HDprintf("H5Oget_native_info failed\n");
        TEST_ERROR;
    }

    if (!which) {
        dbgf(2, "Verifying no cont\n");
        return (ninfo.hdr.nchunks == 1);
    }
    else if (which < cut_point) {
        dbgf(2, "Verifying storage compact & cont\n");
        return (ninfo.meta_size.attr.index_size == 0 && ninfo.meta_size.attr.heap_size == 0 &&
                ninfo.hdr.nchunks > 1);
    }
    else {
        dbgf(2, "Verifying storage dense & no cont\n");
        return (ninfo.meta_size.attr.index_size != 0 && ninfo.meta_size.attr.heap_size != 0 &&
                ninfo.hdr.nchunks == 1);
    }

error:
    return FAIL;

} /* verify_storage_cont_real() */

/*
 * Named pipes handling
 */

/*
 * Initialize the named pipes for test synchronization.
 */
static herr_t
np_init(np_state_t *np, hbool_t writer)
{
    *np = NP_INITIALIZER;

    /*
     * Use two named pipes(FIFO) to coordinate the writer and reader for
     * two-way communication so that the two sides can move forward together.
     * One is for the writer to write to the reader.
     * The other one is for the reader to signal the writer.
     */
    if (writer) {
        /* If the named pipes are present at the start of the test, remove them */
        if (HDaccess(np->fifo_writer_to_reader, F_OK) == 0)
            if (HDremove(np->fifo_writer_to_reader) != 0) {
                HDprintf("HDremove fifo_writer_to_reader failed\n");
                TEST_ERROR;
            }

        if (HDaccess(np->fifo_reader_to_writer, F_OK) == 0)
            if (HDremove(np->fifo_reader_to_writer) != 0) {
                HDprintf("HDremove fifo_reader_to_writer failed\n");
                TEST_ERROR;
            }

        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(np->fifo_writer_to_reader, 0600) < 0) {
            HDprintf("HDmkfifo fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDmkfifo(np->fifo_reader_to_writer, 0600) < 0) {
            HDprintf("HDmkfifo fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }

    /* Both the writer and reader open the pipes */
    if ((np->fd_writer_to_reader = HDopen(np->fifo_writer_to_reader, O_RDWR)) < 0) {
        HDprintf("HDopen fifo_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if ((np->fd_reader_to_writer = HDopen(np->fifo_reader_to_writer, O_RDWR)) < 0) {
        HDprintf("HDopen fifo_reader_to_writer failed\n");
        TEST_ERROR;
    }

    return SUCCEED;

error:
    return FAIL;

} /* np_init() */

/*
 * Close the named pipes.
 */
static herr_t
np_close(np_state_t *np, hbool_t writer)
{
    /* Both the writer and reader close the named pipes */
    if (HDclose(np->fd_writer_to_reader) < 0) {
        HDprintf("HDclose fd_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if (HDclose(np->fd_reader_to_writer) < 0) {
        HDprintf("HDclose fd_reader_to_writer failed\n");
        TEST_ERROR;
    }

    /* Reader finishes last and deletes the named pipes */
    if (!writer) {
        if (HDremove(np->fifo_writer_to_reader) != 0) {
            HDprintf("HDremove fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDremove(np->fifo_reader_to_writer) != 0) {
            HDprintf("HDremove fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }
    return SUCCEED;

error:
    return FAIL;
} /* np_close() */

/*
 *  Writer synchronization depending on the result from the attribute action performed.
 */
static herr_t
np_writer(hbool_t result, unsigned step, const state_t *s, np_state_t *np, H5F_vfd_swmr_config_t *config)
{
    unsigned int i;

    /* The action fails */
    if (!result) {
        HDprintf("attribute action failed\n");
        H5_FAILED();
        AT();

        /* At communication interval, notify the reader about the failure and quit */
        if (step % s->csteps == 0) {
            np->notify = -1;
            if (HDwrite(np->fd_writer_to_reader, &np->notify, sizeof(int)) < 0) {
                HDprintf("HDwrite failed\n");
                TEST_ERROR;
            }
            goto error;
        }
        /* The action succeeds */
    }
    else {
        /* At communication interval, notify the reader and wait for its response */
        if (step % s->csteps == 0) {
            /* Bump up the value of notify to tell the reader to start reading */
            np->notify++;
            if (HDwrite(np->fd_writer_to_reader, &np->notify, sizeof(int)) < 0) {
                HDprintf("HDwrite failed\n");
                TEST_ERROR;
            }

            /* During the wait, writer makes repeated HDF5 API calls
             * to trigger EOT at approximately the correct time */
            for (i = 0; i < config->max_lag + 1; i++) {
                decisleep(config->tick_len);
                H5E_BEGIN_TRY
                {
                    H5Aexists(s->file, "nonexistent");
                }
                H5E_END_TRY;
            }

            /* Handshake between writer and reader */
            if (!np_confirm_verify_notify(np->fd_reader_to_writer, step, s, np)) {
                HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }
        }
    }
    return SUCCEED;

error:
    return FAIL;

} /* np_writer() */

/*
 *
 *  Reader synchronization depending on the result from the verification.
 */
static herr_t
np_reader(hbool_t result, unsigned step, const state_t *s, np_state_t *np)
{
    /* The verification fails */
    if (!result) {
        HDprintf("verify action failed\n");
        H5_FAILED();
        AT();

        /* At communication interval, tell the writer about the failure and exit */
        if (step % s->csteps == 0) {
            np->notify = -1;
            if (HDwrite(np->fd_reader_to_writer, &np->notify, sizeof(int)) < 0) {
                HDprintf("HDwrite failed\n");
                TEST_ERROR;
            }
            goto error;
        }
        /* The verification succeeds */
    }
    else {
        if (step % s->csteps == 0) {
            /* Send back the same notify value for acknowledgement:
             *   --inform the writer to move to the next step */
            if (HDwrite(np->fd_reader_to_writer, &np->notify, sizeof(int)) < 0) {
                HDprintf("HDwrite failed\n");
                TEST_ERROR;
            }
        }
    }
    return SUCCEED;

error:
    return FAIL;

} /* np_reader() */

/*
 *  Handshake between writer and reader:
 *      Confirm `verify` is same as `notify`.
 */
static herr_t
np_confirm_verify_notify(int fd, unsigned step, const state_t *s, np_state_t *np)
{
    if (step % s->csteps == 0) {
        np->verify++;
        if (HDread(fd, &np->notify, sizeof(int)) < 0) {
            HDprintf("HDread failed\n");
            TEST_ERROR;
        }

        if (np->notify == -1) {
            HDprintf("reader/writer failed to verify\n");
            TEST_ERROR;
        }

        if (np->notify != np->verify) {
            HDprintf("received message %d, expecting %d\n", np->notify, np->verify);
            TEST_ERROR;
        }
    }

    return SUCCEED;

error:
    return FAIL;
} /* confirm_verify_notify() */

/*
 * Synchronization done by the reader before moving onto the
 * next verification phase.
 */
static herr_t
np_reader_no_verification(const state_t *s, np_state_t *np, H5F_vfd_swmr_config_t *config)
{
    if (s->use_np) {
        if (!np_confirm_verify_notify(np->fd_writer_to_reader, 0, s, np)) {
            HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
            TEST_ERROR;
        }
    }

    /* Wait for a few ticks for the update to happen */
    decisleep(config->tick_len * s->update_interval);

    if (s->use_np) {
        /* Send back the same notify value for acknowledgement:
         *   --inform the writer to move to the next step */
        if (HDwrite(np->fd_reader_to_writer, &np->notify, sizeof(int)) < 0) {
            HDprintf("HDwrite failed\n");
            TEST_ERROR;
        }
    }

    return SUCCEED;

error:
    return FAIL;

} /* np_reader_no_verification() */

int
main(int argc, char **argv)
{
    hid_t                 fapl   = H5I_INVALID_HID;
    hid_t                 fcpl   = H5I_INVALID_HID;
    hbool_t               writer = FALSE;
    state_t               s;
    const char *          personality;
    H5F_vfd_swmr_config_t config;
    np_state_t            np;
    dsets_state_t         ds;

    if (!state_init(&s, argc, (const char *const *)argv)) {
        HDprintf("state_init() failed\n");
        TEST_ERROR;
    }

    personality = HDstrstr(s.progname, "vfd_swmr_attrdset_");

    if (personality != NULL && HDstrcmp(personality, "vfd_swmr_attrdset_writer") == 0)
        writer = TRUE;
    else if (personality != NULL && HDstrcmp(personality, "vfd_swmr_attrdset_reader") == 0)
        writer = FALSE;
    else {
        HDprintf("unknown personality, expected vfd_swmr_attrdset_{reader,writer}\n");
        TEST_ERROR;
    }

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
     * flush_raw_data, md_pages_reserved, md_file_path, updater_file_path
     */
    init_vfd_swmr_config(&config, 4, 7, writer, TRUE, FALSE, TRUE, 128, "./attrdset-shadow", NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    if ((fapl = vfd_swmr_create_fapl(TRUE, s.use_vfd_swmr, TRUE, 4096, &config)) < 0) {
        HDprintf("vfd_swmr_create_fapl() failed\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        TEST_ERROR;
    }

    if (writer) {
        if ((s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) {
            HDprintf("H5Fcreate failed\n");
            TEST_ERROR;
        }

        if (!create_dsets(&s, &ds)) {
            HDprintf("create_dsets() failed\n");
            TEST_ERROR;
        }
    }
    else {
        if ((s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl)) < 0) {
            HDprintf("H5Fopen failed\n");
            TEST_ERROR;
        }
        if (!open_dsets(&s, &ds)) {
            HDprintf("open_dsets() failed\n");
            TEST_ERROR;
        }
    }

    /* Initiailze named pipes */
    if (s.use_np && !np_init(&np, writer)) {
        HDprintf("np_init() failed\n");
        TEST_ERROR;
    }

    if (writer) {
        if (!perform_dsets_operations(&s, &ds, &config, &np)) {
            HDprintf("perform_dsets_operations() failed\n");
            TEST_ERROR;
        }
    }
    else {
        if (!verify_dsets_operations(&s, &ds, &config, &np)) {
            HDprintf("verify_dsets_operations() failed\n");
            TEST_ERROR;
        }
    }

    if (!close_dsets(&ds)) {
        HDprintf("close_dsets() failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fapl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fcpl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Fclose(s.file) < 0) {
        HDprintf("H5Fclose failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(s.one_by_one_sid) < 0) {
        HDprintf("H5Sclose failed\n");
        TEST_ERROR;
    }

    if (s.use_np && !np_close(&np, writer)) {
        HDprintf("np_close() failed\n");
        TEST_ERROR;
    }

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(s.file);
        H5Sclose(s.one_by_one_sid);
    }
    H5E_END_TRY;

    if (s.use_np && np.fd_writer_to_reader >= 0)
        HDclose(np.fd_writer_to_reader);

    if (s.use_np && np.fd_reader_to_writer >= 0)
        HDclose(np.fd_reader_to_writer);

    if (s.use_np && !writer) {
        HDremove(np.fifo_writer_to_reader);
        HDremove(np.fifo_reader_to_writer);
    }

    return EXIT_FAILURE;
} /* main */

#endif /* H5_HAVE_WIN32_API */
