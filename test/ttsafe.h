/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
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
void tts_is_threadsafe(const void *);
#ifdef H5_HAVE_THREADS
void tts_thread_pool(const void *);
#ifndef H5_HAVE_STDATOMIC_H
/* C11 atomics only tested when emulated */
void tts_atomics(const void *);
#endif /* H5_HAVE_STDATOMIC_H */
void tts_rwlock(const void *);
void tts_semaphore(const void *);
#ifndef H5_HAVE_WIN_THREADS
void tts_rec_rwlock_smoke_check_1(const void *);
void tts_rec_rwlock_smoke_check_2(const void *);
void tts_rec_rwlock_smoke_check_3(const void *);
void tts_rec_rwlock_smoke_check_4(const void *);
#endif /* !H5_HAVE_WIN_THREADS */
#ifdef H5_HAVE_THREADSAFE_API
void tts_dcreate(const void *);
void tts_error(const void *);
void tts_cancel(const void *);
void tts_acreate(const void *);
void tts_attr_vlen(const void *);
void tts_thread_id(const void *);
void tts_develop_api(const void *);
void tts_error_stacks(const void *);

/* Prototypes for the cleanup routines */
void cleanup_dcreate(void *);
void cleanup_error(void *);
void cleanup_cancel(void *);
void cleanup_acreate(void *);
void cleanup_attr_vlen(void *);

#endif /* H5_HAVE_THREADSAFE_API */
#endif /* H5_HAVE_THREADS */
#endif /* TTSAFE_H */
