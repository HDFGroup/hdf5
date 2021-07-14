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

#ifndef VFD_SWMR_COMMON_H
#define VFD_SWMR_COMMON_H

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
#define MAX_SIZE_CHANGE 10

#define VFD_SWMR_FILENAME "vfd_swmr_data.h5" /* SWMR test file name */

/* The message sent by writer that the file open is done--releasing the file lock */
#define VFD_SWMR_WRITER_MESSAGE "VFD_SWMR_WRITER_MESSAGE"

/************/
/* Typedefs */
/************/

typedef struct _estack_state {
    H5E_auto2_t efunc;
    void *      edata;
} estack_state_t;

typedef enum _testsel { TEST_NONE = 0, TEST_NULL, TEST_OOB } testsel_t;

/********************/
/* Global Variables */
/********************/

H5TEST_DLLVAR int verbosity;

/**************/
/* Prototypes */
/**************/
#ifdef __cplusplus
extern "C" {
#endif

H5TEST_DLL bool below_speed_limit(struct timespec *, const struct timespec *);
H5TEST_DLL void decisleep(uint32_t tenths);

H5TEST_DLL estack_state_t estack_get_state(void);
H5TEST_DLL estack_state_t disable_estack(void);
H5TEST_DLL void           restore_estack(estack_state_t);

#ifndef H5_HAVE_WIN32_API
H5TEST_DLL void block_signals(sigset_t *);
H5TEST_DLL void restore_signals(sigset_t *);
H5TEST_DLL void await_signal(hid_t);
#endif /* H5_HAVE_WIN32_API */

H5TEST_DLL hid_t vfd_swmr_create_fapl(bool use_latest_format, bool use_vfd_swmr, bool only_meta_pages,
                                      size_t page_buf_size, H5F_vfd_swmr_config_t *config);

H5TEST_DLL void init_vfd_swmr_config(H5F_vfd_swmr_config_t *config, uint32_t tick_len, uint32_t max_lag,
                                     hbool_t writer, hbool_t flush_raw_data, uint32_t md_pages_reserved,
                                     const char *md_file_fmtstr, ...) H5_ATTR_FORMAT(printf, 7, 8);

H5TEST_DLL hid_t vfd_swmr_create_fcpl(H5F_fspace_strategy_t fs_strategy, hsize_t fs_page_size);

H5TEST_DLL void dbgf(int, const char *, ...) H5_ATTR_FORMAT(printf, 2, 3);
H5TEST_DLL void evsnprintf(char *, size_t, const char *, va_list);
H5TEST_DLL void esnprintf(char *, size_t, const char *, ...) H5_ATTR_FORMAT(printf, 3, 4);

H5TEST_DLL int fetch_env_ulong(const char *, unsigned long, unsigned long *);

#ifdef __cplusplus
}
#endif

#endif /* SWMR_COMMON_H */
