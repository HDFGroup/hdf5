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

#ifndef _SWMR_COMMON_H
#define _SWMR_COMMON_H

/***********/
/* Headers */
/***********/

#include "h5test.h"

/**********/
/* Macros */
/**********/

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
H5TEST_DLLVAR symbol_info_t *symbol_info[NLEVELS];
H5TEST_DLLVAR unsigned symbol_count[NLEVELS];

/**************/
/* Prototypes */
/**************/
#ifdef __cplusplus
extern "C" {
#endif

H5TEST_DLL symbol_info_t * choose_dataset(void);
H5TEST_DLL hid_t create_symbol_datatype(void);
H5TEST_DLL int generate_name(char *name_buf, unsigned level, unsigned count);
H5TEST_DLL int generate_symbols(void);
H5TEST_DLL int shutdown_symbols(void);
H5TEST_DLL int print_metadata_retries_info(hid_t fid);

#ifdef __cplusplus
}
#endif

#endif /* _SWMR_COMMON_H */
