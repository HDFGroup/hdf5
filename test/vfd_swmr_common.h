/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _VFD_SWMR_COMMON_H
#define _VFD_SWMR_COMMON_H

/***********/
/* Headers */
/***********/

#include "h5test.h"

/**********/
/* Macros */
/**********/

/* The maximum # of records to add/remove from the dataset in one step,
 * used by vfd_swmr_addrem_writer and vfd_swmr_remove_reader.
 */
#define MAX_SIZE_CHANGE     10

#define NLEVELS         5   /* # of datasets in the SWMR test file */

#define NMAPPING        9   

#define FILENAME        "vfd_swmr_data.h5"  /* SWMR test file name */
#define DTYPE_SIZE      150             /* Data size in opaque type */

/* The message sent by writer that the file open is done--releasing the file lock */
#define WRITER_MESSAGE "VFD_SWMR_WRITER_MESSAGE"

/************/
/* Typedefs */
/************/

typedef struct _estack_state {
    H5E_auto_t efunc;
    void *edata;
} estack_state_t;

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

H5TEST_DLL estack_state_t disable_estack(void);
H5TEST_DLL void restore_estack(estack_state_t);

H5TEST_DLL symbol_info_t * choose_dataset(unsigned *, unsigned *);
H5TEST_DLL hid_t create_symbol_datatype(void);
H5TEST_DLL int generate_name(char *name_buf, unsigned level, unsigned count);
H5TEST_DLL int generate_symbols(void);
H5TEST_DLL int shutdown_symbols(void);
H5TEST_DLL int print_metadata_retries_info(hid_t fid);

H5TEST_DLL void block_signals(sigset_t *);
H5TEST_DLL void restore_signals(sigset_t *);
H5TEST_DLL void await_signal(hid_t);

#ifdef __cplusplus
}
#endif

#endif /* _SWMR_COMMON_H */
