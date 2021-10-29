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
 * Use Case Header file: common definitions for use cases tests.
 */
#include "h5test.h"

/* Macro definitions */
#define Hgoto_error(val)                                                                                     \
    {                                                                                                        \
        ret_value = val;                                                                                     \
        goto done;                                                                                           \
    }
#define Hgoto_done                                                                                           \
    {                                                                                                        \
        goto done;                                                                                           \
    }
#define Chunksize_DFT  256 /* chunksize default */
#define ErrorReportMax 10  /* max number of errors reported */
/* these two definitions must match each other */
#define UC_DATATYPE H5T_NATIVE_SHORT /* use case HDF5 data type */
#define UC_CTYPE    short            /* use case C data type */
#define UC_RANK     3                /* use case dataset rank */

/* Name of message file that is sent by the writer */
#define WRITER_MESSAGE "USE_WRITER_MESSAGE"

/* type declarations */
typedef enum part_t {
    UC_READWRITE = 0, /* both writer and reader */
    UC_WRITER,        /* writer only */
    UC_READER         /* reader only */
} part_t;

typedef struct options_t {
    hsize_t     chunksize;          /* chunks are chunksize^2 planes         */
    hsize_t     chunkplanes;        /* number of planes per chunk, default 1 */
    hsize_t     chunkdims[UC_RANK]; /* chunk dims is (chunkplan, chunksize, chunksize) */
    hsize_t     dims[UC_RANK];      /* dataset initial dims */
    hsize_t     max_dims[UC_RANK];  /* dataset max dims */
    hsize_t     nplanes;            /* number of planes to write, default proportional to chunksize */
    char *      filename;           /* use case data filename               */
    part_t      launch;             /* launch writer, reader or both        */
    hbool_t     use_swmr;           /* use swmr open (1) or not             */
    int         iterations;         /* iterations, default 1                */
    hid_t       fapl_id;            /* instance-specific FAPL ID            */
    const char *progname;           /* Program name (used in usage and dset name) */
} options_t;

/* prototype declarations */
int  parse_option(int argc, char *const argv[], options_t *opts);
int  setup_parameters(int argc, char *const argv[], options_t *opts);
void show_parameters(options_t *opts);
void usage(const char *prog);
int  create_uc_file(options_t *opts);
int  write_uc_file(hbool_t tosend, hid_t file_id, options_t *opts);
int  read_uc_file(hbool_t towait, options_t *opts);
