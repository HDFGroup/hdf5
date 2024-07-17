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
 * This header file contains information required for testing the HDF5 library.
 */

#ifndef TTSAFE_H
#define TTSAFE_H

/*
 * Include required headers.  This file tests internal library functions,
 * so we include the private headers here.
 */
#include "testhdf5.h"

/*
 * This file needs to access private datatypes from the H5TS package.
 * This file also needs to access the threadsafety testing code.
 */
#define H5TS_FRIEND /*suppress error about including H5TSpkg      */
#include "H5TSpkg.h"

/* Prototypes for the support routines */
extern char *gen_name(int);

/* Prototypes for the test routines */
void tts_is_threadsafe(void);
#ifdef H5_HAVE_THREADS
void tts_thread_pool(void);
void tts_atomics(void);
#ifdef H5_HAVE_STDATOMIC_H
void tts_semaphore(void);
#endif /* H5_HAVE_STDATOMIC_H */
void tts_rec_rw_lock_smoke_check_1(void);
void tts_rec_rw_lock_smoke_check_2(void);
void tts_rec_rw_lock_smoke_check_3(void);
void tts_rec_rw_lock_smoke_check_4(void);
#ifdef H5_HAVE_THREADSAFE
void tts_dcreate(void);
void tts_error(void);
void tts_cancel(void);
void tts_acreate(void);
void tts_attr_vlen(void);
void tts_thread_id(void);
void tts_develop_api(void);

/* Prototypes for the cleanup routines */
void cleanup_dcreate(void);
void cleanup_error(void);
void cleanup_cancel(void);
void cleanup_acreate(void);
void cleanup_attr_vlen(void);

#endif /* H5_HAVE_THREADSAFE */
#endif /* H5_HAVE_THREADS */
#endif /* TTSAFE_H */
