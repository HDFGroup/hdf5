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

/* Check that a thread ID returned by H5TS_thread_id() possesses the
 * following properties:
 *
 * 1 ID >= 1.
 * 2 The ID is constant over the thread's lifetime.
 * 3 No two threads share an ID during their lifetimes.
 * 4 A thread's ID is available for reuse as soon as it is joined.
 */

/*
 * Include required headers.  This file tests internal library functions,
 * so we include the private headers here.
 */
#include "testhdf5.h"

#if defined(H5_HAVE_THREADSAFE) && !defined(H5_HAVE_WIN_THREADS)

static void my_errx(int, const char *, ...) H5_ATTR_FORMAT(printf, 2, 3);

static void
my_errx(int code, const char *fmt, ...)
{
    va_list ap;

    (void)fprintf(stderr, "thread_id: ");
    va_start(ap, fmt);
    (void)vfprintf(stderr, fmt, ap);
    va_end(ap);
    (void)fputc('\n', stderr);
    exit(code);
}

#if defined(H5_HAVE_DARWIN)

typedef struct _pthread_barrierattr {
    uint8_t unused;
} pthread_barrierattr_t;

typedef struct _pthread_barrier {
    uint32_t        magic;
    unsigned int    count;
    uint64_t        nentered;
    pthread_cond_t  cv;
    pthread_mutex_t mtx;
} pthread_barrier_t;

int pthread_barrier_init(pthread_barrier_t *, const pthread_barrierattr_t *, unsigned int);
int pthread_barrier_wait(pthread_barrier_t *);
int pthread_barrier_destroy(pthread_barrier_t *);

static const uint32_t barrier_magic = 0xf00dd00f;

int
pthread_barrier_init(pthread_barrier_t *barrier, const pthread_barrierattr_t *attr, unsigned int count)
{
    int rc;

    if (count == 0)
        return EINVAL;

    if (attr != NULL)
        return EINVAL;

    memset(barrier, 0, sizeof(*barrier));

    barrier->count = count;

    if ((rc = pthread_cond_init(&barrier->cv, NULL)) != 0)
        return rc;

    if ((rc = pthread_mutex_init(&barrier->mtx, NULL)) != 0) {
        (void)pthread_cond_destroy(&barrier->cv);
        return rc;
    }

    barrier->magic = barrier_magic;

    return 0;
}

static void
barrier_lock(pthread_barrier_t *barrier)
{
    int rc;

    if ((rc = pthread_mutex_lock(&barrier->mtx)) != 0) {
        my_errx(EXIT_FAILURE, "%s: pthread_mutex_lock: %s", __func__, strerror(rc));
    }
}

static void
barrier_unlock(pthread_barrier_t *barrier)
{
    int rc;

    if ((rc = pthread_mutex_unlock(&barrier->mtx)) != 0) {
        my_errx(EXIT_FAILURE, "%s: pthread_mutex_unlock: %s", __func__, strerror(rc));
    }
}

int
pthread_barrier_destroy(pthread_barrier_t *barrier)
{
    int rc;

    barrier_lock(barrier);
    if (barrier->magic != barrier_magic)
        rc = EINVAL;
    else if (barrier->nentered % barrier->count != 0)
        rc = EBUSY;
    else {
        rc             = 0;
        barrier->magic = ~barrier->magic;
    }
    barrier_unlock(barrier);

    if (rc != 0)
        return rc;

    (void)pthread_cond_destroy(&barrier->cv);
    (void)pthread_mutex_destroy(&barrier->mtx);

    return 0;
}

int
pthread_barrier_wait(pthread_barrier_t *barrier)
{
    int      rc;
    uint64_t threshold;

    if (barrier == NULL)
        return EINVAL;

    barrier_lock(barrier);
    if (barrier->magic != barrier_magic) {
        rc = EINVAL;
        goto out;
    }
    /* Compute the release `threshold`.  All threads entering with count = 5
     * and `nentered` in [0, 4] should be released once `nentered` reaches 5:
     * call 5 the release `threshold`.  All threads entering with count = 5
     * and `nentered` in [5, 9] should be released once `nentered` reaches 10.
     */
    threshold = (barrier->nentered / barrier->count + 1) * barrier->count;
    barrier->nentered++;
    while (barrier->nentered < threshold) {
        if ((rc = pthread_cond_wait(&barrier->cv, &barrier->mtx)) != 0)
            goto out;
    }
    rc = pthread_cond_broadcast(&barrier->cv);
out:
    barrier_unlock(barrier);
    return rc;
}

#endif /* H5_HAVE_DARWIN */

static void my_err(int, const char *, ...) H5_ATTR_FORMAT(printf, 2, 3);

static void
my_err(int code, const char *fmt, ...)
{
    va_list ap;
    int     errno_copy = errno;

    (void)fprintf(stderr, "thread_id: ");
    va_start(ap, fmt);
    (void)vfprintf(stderr, fmt, ap);
    va_end(ap);
    (void)fprintf(stderr, ": %s\n", strerror(errno_copy));
    exit(code);
}

#define threads_failure(_call, _result)                                                                      \
    do {                                                                                                     \
        my_errx(EXIT_FAILURE, "%s.%d: " #_call ": %s", __func__, __LINE__, strerror(_result));               \
    } while (false)

#define NTHREADS 5

static volatile bool     failed = false;
static pthread_barrier_t barrier;
static bool              used[NTHREADS];
static pthread_mutex_t   used_lock;

static void
atomic_printf(const char *fmt, ...)
{
    char    buf[80];
    va_list ap;
    ssize_t nprinted, nwritten;

    va_start(ap, fmt);
    nprinted = vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);

    if (nprinted == -1)
        my_err(EXIT_FAILURE, "%s.%d: vsnprintf", __func__, __LINE__);
    else if (nprinted >= (ssize_t)sizeof(buf))
        my_errx(EXIT_FAILURE, "%s.%d: vsnprintf overflowed", __func__, __LINE__);

    nwritten = HDwrite(STDOUT_FILENO, buf, (size_t)nprinted);
    if (nwritten < nprinted) {
        my_errx(EXIT_FAILURE, "%s.%d: write error or short write", __func__, __LINE__);
    }
}

/* Each thread runs this routine.  The routine fetches the current
 * thread's ID, makes sure that it is in the expected range, makes
 * sure that in this round of testing, no two threads shared the
 * same ID, and checks that each thread's ID is constant over its lifetime.
 *
 * main() checks that every ID in [1, NTHREADS] is used in each round
 * of testing.  All NTHREADS threads synchronize on a barrier after each
 * has fetched its ID.  The barrier guarantees that all threads' lifetimes
 * overlap at least momentarily, so the IDs will be unique, and there
 * will be NTHREADS of them.  Further, since thread IDs are assigned
 * starting with 1, and the number of threads with IDs alive never exceeds
 * NTHREADS, the least ID has to be 1 and the greatest, NTHREADS.
 */
static void *
thread_main(void H5_ATTR_UNUSED *arg)
{
    uint64_t ntid, tid;

    tid = H5TS_thread_id();

    if (tid < 1 || NTHREADS < tid) {
        atomic_printf("unexpected tid %" PRIu64 " FAIL\n", tid);
        goto pre_barrier_error;
    }
    pthread_mutex_lock(&used_lock);
    if (used[tid - 1]) {
        atomic_printf("reused tid %" PRIu64 " FAIL\n", tid);
        pthread_mutex_unlock(&used_lock);
        goto pre_barrier_error;
    }
    used[tid - 1] = true;
    pthread_mutex_unlock(&used_lock);

    atomic_printf("tid %" PRIu64 " in [1, %d] PASS\n", tid, NTHREADS);
    pthread_barrier_wait(&barrier);

    ntid = H5TS_thread_id();
    if (ntid != tid) {
        atomic_printf("tid changed from %" PRIu64 " to %" PRIu64 " FAIL\n", tid, ntid);
        failed = true;
    }
    return NULL;
pre_barrier_error:
    pthread_barrier_wait(&barrier);
    failed = true;
    return NULL;
}

int
main(void)
{
    int       i, rc, times;
    pthread_t threads[NTHREADS];

    /* Run H5open() to initialize the library's thread-ID freelist,
     * mutex, etc.
     */
    if (H5open() != SUCCEED)
        my_errx(EXIT_FAILURE, "%s.%d: H5open failed", __func__, __LINE__);

    if ((rc = pthread_mutex_init(&used_lock, NULL)) == -1)
        threads_failure(pthread_mutex_init, rc);

    if ((rc = pthread_barrier_init(&barrier, NULL, NTHREADS)) != 0)
        threads_failure(pthread_barrier_init, rc);

    /* Start the test threads and join them twice to make sure that
     * the thread IDs are recycled in the second round.
     */
    for (times = 0; times < 2; times++) {

        for (i = 0; i < NTHREADS; i++)
            used[i] = false; // access synchronized by thread create/join

        for (i = 0; i < NTHREADS; i++) {
            rc = pthread_create(&threads[i], NULL, thread_main, NULL);
            if (rc != 0)
                threads_failure(pthread_create, rc);
        }

        for (i = 0; i < NTHREADS; i++) {
            rc = pthread_join(threads[i], NULL);
            if (rc != 0)
                threads_failure(pthread_join, rc);
        }

        for (i = 0; i < NTHREADS; i++) {
            if (!used[i]) // access synchronized by thread create/join
                my_errx(EXIT_FAILURE, "thread ID %d did not run.", i + 1);
        }
    }
    if ((rc = pthread_barrier_destroy(&barrier)) != 0)
        threads_failure(pthread_barrier_destroy, rc);
    return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}

#else  /*H5_HAVE_THREADSAFE && !H5_HAVE_WIN_THREADS*/
int
main(void)
{
    fprintf(stderr, "not implemented in this configuration.\n");
    return EXIT_SUCCESS;
}
#endif /*H5_HAVE_THREADSAFE && !H5_HAVE_WIN_THREADS*/
