#ifndef _SWMR_COMMON_H
#define _SWMR_COMMON_H

/* Headers needed */

#define _GNU_SOURCE
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "h5test.h"

/**********/
/* Macros */
/**********/

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

#define NLEVELS         5   /* # of datasets in the SWMR test file */

#define NMAPPING        9   

#define FILENAME        "swmr_data.h5"  /* SWMR test file name */
#define DTYPE_SIZE      150             /* Data size in opaque type */

/* The message sent by writer that the file open is done--releasing the file lock */
#define WRITER_MESSAGE "SWMR_WRITER_MESSAGE"

/************/
/* Typedefs */
/************/

/* Information about a symbol/dataset */
typedef struct {
    char *name;         /* Dataset name for symbol */
    hid_t dsid;         /* Dataset ID for symbol */
    hsize_t nrecords;   /* Number of records for the symbol */
} symbol_info_t;

/* A symbol's record */
typedef struct {
    uint64_t rec_id;    /* ID for this record (unique in symbol) */
    uint8_t info[DTYPE_SIZE];   /* "Other" information for this record */
} symbol_t;

/********************/
/* Global Variables */
/********************/
extern symbol_info_t *symbol_info[NLEVELS];
extern unsigned symbol_count[NLEVELS];

/**************/
/* Prototypes */
/**************/
symbol_info_t * choose_dataset(void);
hid_t create_symbol_datatype(void);
int generate_name(char *name_buf, unsigned level, unsigned count);
int generate_symbols(void);
int shutdown_symbols(void);
int print_metadata_retries_info(hid_t fid);

#endif /* _SWMR_COMMON_H */
