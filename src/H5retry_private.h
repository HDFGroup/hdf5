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

#ifndef H5retry_private_H
#define H5retry_private_H

/*
 * Data types and functions for retry loops.
 */

/* State for a retry loop.  No user-serviceable parts inside. */
typedef struct h5_retry_t {
    uint64_t maxival;  /* maximum sleep interval (nanoseconds) */
    unsigned maxtries; /* maximum permissible tries */
    unsigned tries;    /* remaining tries */
    uint64_t ival;     /* nanoseconds sleep interval before clamping to
                        * maxival
                        */
} h5_retry_t;

/* Default minimum/maximum retry intervals: 1/10s minimum, 1s maximum. */
#define H5_RETRY_DEFAULT_MINIVAL (100ULL * 1000ULL * 1000ULL)
#define H5_RETRY_DEFAULT_MAXIVAL (1000ULL * 1000ULL * 1000ULL)
/* One hour: */
#define H5_RETRY_ONE_SECOND (1000ULL * 1000ULL * 1000ULL)
#define H5_RETRY_ONE_HOUR   (3600ULL * H5_RETRY_ONE_SECOND)

/* If any tries remain, decrease the number of remaining tries and
 * return true.  Otherwise, return false.
 *
 * XXX This is not part of the API. XXX
 */
static inline bool
h5_retry_decrement(struct h5_retry_t *r)
{
    if (r->tries == 0)
        return false;
    --r->tries;
    return true;
}

/* Establish state for a retry loop in `r`.  The loop will retry no
 * more than `maxtries` times, sleeping for no fewer than `minival`
 * nanoseconds between tries.  After each try, the sleep time will
 * increase to `maxival` nanoseconds or twice the previous sleep time,
 * whichever is less.
 *
 * `h5_retry_init` always returns true.  This is to help one use
 * it in a loop like this:
 *
 * for (do_try = h5_retry_init(&r, 100, H5_RETRY_DEFAULT_MINIVAL,
 *                             H5_RETRY_DEFAULT_MAXIVAL);
 *      do_try;
 *      do_try = h5_retry_next(&r)) {
 *      .
 *      .
 *      .
 * }
 *
 * Note well: the program will enter the body of the loop, above, no more
 * than 101 times: once for an initial try, and then 100 times for retries.
 */
static inline bool
h5_retry_init(struct h5_retry_t *r, unsigned int maxtries, uint64_t minival, uint64_t maxival)
{
    memset(r, '\0', sizeof(*r));
    assert(0 < maxtries);
    assert(0 < minival && minival <= maxival);
    r->tries = r->maxtries = maxtries;
    r->ival                = minival;
    r->maxival             = maxival;
    return h5_retry_decrement(r);
}

/* If any tries remain, sleep for the mininum interval, or twice the
 * previous sleep time, and return true.  If no tries remain, return false.
 */
static inline bool
h5_retry_next(struct h5_retry_t *r)
{
    uint64_t ival;

    if (!h5_retry_decrement(r))
        return false;
    ival = r->ival;
    if (r->maxival < ival)
        ival = r->maxival;
    else if (UINT64_MAX - ival >= ival)
        r->ival += ival;

    H5_nanosleep(ival);

    return true;
}

/* Return the number of tries performed since `h5_retry_init()`
 * was called on `r`.
 */
static inline unsigned
h5_retry_tries(struct h5_retry_t *r)
{
    return r->maxtries - r->tries;
}

#endif /* H5retry_private_H */
