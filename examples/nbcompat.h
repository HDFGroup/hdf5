/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

#ifndef NB_COMPAT_H
#define NB_COMPAT_H

#include <unistd.h> /* for size_t */

#ifndef __arraycount
#define __arraycount(__a) (sizeof(__a) / sizeof((__a)[0]))
#endif

size_t strlcpy(char *, const char *, size_t);

#define timespeccmp(tsp, usp, cmp)                                                                           \
    (((tsp)->tv_sec == (usp)->tv_sec) ? ((tsp)->tv_nsec cmp(usp)->tv_nsec) : ((tsp)->tv_sec cmp(usp)->tv_sec))
#define timespecadd(tsp, usp, vsp)                                                                           \
    do {                                                                                                     \
        (vsp)->tv_sec  = (tsp)->tv_sec + (usp)->tv_sec;                                                      \
        (vsp)->tv_nsec = (tsp)->tv_nsec + (usp)->tv_nsec;                                                    \
        if ((vsp)->tv_nsec >= 1000000000L) {                                                                 \
            (vsp)->tv_sec++;                                                                                 \
            (vsp)->tv_nsec -= 1000000000L;                                                                   \
        }                                                                                                    \
    } while (/* CONSTCOND */ 0)
#define timespecsub(tsp, usp, vsp)                                                                           \
    do {                                                                                                     \
        (vsp)->tv_sec  = (tsp)->tv_sec - (usp)->tv_sec;                                                      \
        (vsp)->tv_nsec = (tsp)->tv_nsec - (usp)->tv_nsec;                                                    \
        if ((vsp)->tv_nsec < 0) {                                                                            \
            (vsp)->tv_sec--;                                                                                 \
            (vsp)->tv_nsec += 1000000000L;                                                                   \
        }                                                                                                    \
    } while (/* CONSTCOND */ 0)
#define timespec2ns(x) (((uint64_t)(x)->tv_sec) * 1000000000L + (x)->tv_nsec)

#endif /* NB_COMPAT_H */
