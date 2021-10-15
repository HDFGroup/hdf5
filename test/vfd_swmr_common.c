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
 * Utility functions for the VFD SWMR tests.
 */

/***********/
/* Headers */
/***********/

#include "h5test.h"
#include "vfd_swmr_common.h"
#include "swmr_common.h"

/* Only need the pthread solution if sigtimedwait(2) isn't available.
 * There's currently no Windows solution, so ignore that for now.
 */
#if !defined(H5_HAVE_SIGTIMEDWAIT) && !defined(H5_HAVE_WIN32_API)
#include <pthread.h>
#endif

int verbosity = 2;

/* Return true no more than once in any `ival` interval of time,
 * as measured by the system's monotonically increasing timer, to
 * help rate-limit activities.
 *
 * Read the system's current time and compare it with the time stored in
 * `last`.  If the difference between `last` and the current time is
 * greater than the duration `ival`, then record the current time at
 * `last` and return true.  Otherwise, return false.
 */
hbool_t
below_speed_limit(struct timespec *last, const struct timespec *ival)
{
    struct timespec now;
    hbool_t         result;

    HDassert(0 <= last->tv_nsec && last->tv_nsec < 1000000000L);
    HDassert(0 <= ival->tv_nsec && ival->tv_nsec < 1000000000L);

    /* NOTE: timespec_get() is C11. This may need further tweaks. */
#ifdef H5_HAVE_WIN32_API
    if (timespec_get(&now, TIME_UTC) != TIME_UTC) {
#else
    if (HDclock_gettime(CLOCK_MONOTONIC, &now) == -1) {
#endif
        HDfprintf(stderr, "%s: clock_gettime", __func__);
        HDexit(EXIT_FAILURE);
    }

    if (now.tv_sec - last->tv_sec > ival->tv_sec)
        result = true;
    else if (now.tv_sec - last->tv_sec < ival->tv_sec)
        result = false;
    else
        result = (now.tv_nsec - last->tv_nsec >= ival->tv_nsec);

    if (result)
        *last = now;

    return result;
}

/* Sleep for `tenths` tenths of a second. */
void
decisleep(uint32_t tenths)
{
    uint64_t nsec = tenths * 100 * 1000 * 1000;

    H5_nanosleep(nsec);
}

/* Like vsnprintf(3), but abort the program with an error message on
 * `stderr` if the buffer is too small or some other error occurs.
 */
void
evsnprintf(char *buf, size_t bufsz, const char *fmt, va_list ap)
{
    int rc;

    rc = HDvsnprintf(buf, bufsz, fmt, ap);

    if (rc < 0) {
        HDfprintf(stderr, "%s: HDvsnprintf", __func__);
        HDexit(EXIT_FAILURE);
    }
    else if ((size_t)rc >= bufsz) {
        HDfprintf(stderr, "%s: buffer too small", __func__);
        HDexit(EXIT_FAILURE);
    }
}

/* Like snprintf(3), but abort the program with an error message on
 * `stderr` if the buffer is too small or some other error occurs.
 */
void
esnprintf(char *buf, size_t bufsz, const char *fmt, ...)
{
    va_list ap;

    HDva_start(ap, fmt);
    evsnprintf(buf, bufsz, fmt, ap);
    HDva_end(ap);
}

void
dbgf(int level, const char *fmt, ...)
{
    va_list ap;

    if (verbosity < level)
        return;

    HDva_start(ap, fmt);
    (void)HDvfprintf(stderr, fmt, ap);
    HDva_end(ap);
}

/* Disable HDF5 error-stack printing and return the previous state
 * of error-stack printing.
 */
estack_state_t
disable_estack(void)
{
    estack_state_t es = estack_get_state();

    (void)H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    return es;
}

/* Return the current state of HDF5 error-stack printing. */
estack_state_t
estack_get_state(void)
{
    estack_state_t es;

    (void)H5Eget_auto2(H5E_DEFAULT, &es.efunc, &es.edata);

    return es;
}

/* Restore HDF5 error-stack printing to a state returned previously by
 * `disable_estack` or `estack_get_state`.
 */
void
restore_estack(estack_state_t es)
{
    (void)H5Eset_auto2(H5E_DEFAULT, es.efunc, es.edata);
}

#ifndef H5_HAVE_WIN32_API
/* Store the signal mask at `oldset` and then block all signals. */
void
block_signals(sigset_t *oldset)
{
    sigset_t fullset;

    if (sigfillset(&fullset) == -1) {
        HDfprintf(stderr, "%s.%d: could not initialize signal masks", __func__, __LINE__);
        HDexit(EXIT_FAILURE);
    }

    if (sigprocmask(SIG_BLOCK, &fullset, oldset) == -1) {
        HDfprintf(stderr, "%s.%d: sigprocmask", __func__, __LINE__);
        HDexit(EXIT_FAILURE);
    }
}

/* Restore the signal mask in `oldset`. */
void
restore_signals(sigset_t *oldset)
{
    if (sigprocmask(SIG_SETMASK, oldset, NULL) == -1) {
        HDfprintf(stderr, "%s.%d: sigprocmask", __func__, __LINE__);
        HDexit(EXIT_FAILURE);
    }
}

#if 0
static const char *
strsignal(int signum)
{
    switch (signum) {
    case SIGUSR1:
        return "SIGUSR1";
    case SIGINT:
        return "SIGINT";
    case SIGPIPE:
        return "SIGPIPE";
    default:
        return "<unknown>";
    }
}
#endif

#ifndef H5_HAVE_SIGTIMEDWAIT

typedef struct timer_params_t {
    struct timespec *tick;
    hid_t            fid;
} timer_params_t;

pthread_mutex_t timer_mutex;
hbool_t         timer_stop = FALSE;

static void *
timer_function(void *arg)
{
    timer_params_t *params = (timer_params_t *)arg;
    sigset_t        sleepset;
    hbool_t         done = FALSE;

    /* Ignore any signals */
    sigfillset(&sleepset);
    pthread_sigmask(SIG_SETMASK, &sleepset, NULL);

    for (;;) {
        estack_state_t es;

        nanosleep(params->tick, NULL);

        /* Check the mutex */
        pthread_mutex_lock(&timer_mutex);
        done = timer_stop;
        pthread_mutex_unlock(&timer_mutex);
        if (done)
            break;

        /* Avoid deadlock with peer: periodically enter the API so that
         * tick processing occurs and data is flushed so that the peer
         * can see it.
         *
         * The call we make will fail, but that's ok,
         * so squelch errors.
         */
        es = disable_estack();
        (void)H5Aexists_by_name(params->fid, "nonexistent", "nonexistent", H5P_DEFAULT);
        restore_estack(es);
    }

    return NULL;
}
#endif /* H5_HAVE_SIGTIMEDWAIT */

/* Wait for any signal to occur and then return.  Wake periodically
 * during the wait to perform API calls: in this way, the
 * VFD SWMR tick number advances and recent changes do not languish
 * in HDF5 library buffers where readers cannot see them.
 */
void
await_signal(hid_t fid)
{
    struct timespec tick = {.tv_sec = 0, .tv_nsec = 1000000000 / 100};
    sigset_t        sleepset;

    if (sigfillset(&sleepset) == -1) {
        HDfprintf(stderr, "%s.%d: could not initialize signal mask", __func__, __LINE__);
        HDexit(EXIT_FAILURE);
    }

    /* Avoid deadlock: flush the file before waiting for the reader's
     * message.
     */
    if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0) {
        HDfprintf(stderr, "%s: H5Fflush failed", __func__);
        HDexit(EXIT_FAILURE);
    }

    dbgf(1, "waiting for signal\n");

#ifndef H5_HAVE_SIGTIMEDWAIT
    {
        /* Use an alternative scheme for platforms like MacOS that do not have
         * sigtimedwait(2)
         */
        timer_params_t params;
        int            rc;
        pthread_t      timer;

        params.tick = &tick;
        params.fid  = fid;

        pthread_mutex_init(&timer_mutex, NULL);

        pthread_create(&timer, NULL, timer_function, &params);

        rc = sigwait(&sleepset, NULL);

        if (rc != -1) {
            HDfprintf(stderr, "Received signal, wrapping things up.\n");
            pthread_mutex_lock(&timer_mutex);
            timer_stop = TRUE;
            pthread_mutex_unlock(&timer_mutex);
            pthread_join(timer, NULL);
        }
        else {
            HDfprintf(stderr, "%s: sigwait", __func__);
            HDexit(EXIT_FAILURE);
        }
    }
#else
    for (;;) {
        /* Linux and other systems */
        const int rc = sigtimedwait(&sleepset, NULL, &tick);

        if (rc != -1) {
            HDfprintf(stderr, "Received %s, wrapping things up.\n", strsignal(rc));
            break;
        }
        else if (rc == -1 && errno == EAGAIN) {
            estack_state_t es;

            /* Avoid deadlock with peer: periodically enter the API so that
             * tick processing occurs and data is flushed so that the peer
             * can see it.
             *
             * The call we make will fail, but that's ok,
             * so squelch errors.
             */
            es = disable_estack();
            (void)H5Aexists_by_name(fid, "nonexistent", "nonexistent", H5P_DEFAULT);
            restore_estack(es);
        }
        else if (rc == -1) {
            HDfprintf(stderr, "%s: sigtimedwait", __func__);
            HDexit(EXIT_FAILURE);
        }
    }
#endif /* H5_HAVE_SIGTIMEDWAIT */
}

#endif /* H5_HAVE_WIN32_API */

/* Revised support routines that can be used for all VFD SWMR integration tests
 */
/* Initialize fields in config with the input parameters */
void
init_vfd_swmr_config(H5F_vfd_swmr_config_t *config, uint32_t tick_len, uint32_t max_lag, hbool_t writer,
                     hbool_t flush_raw_data, uint32_t md_pages_reserved, const char *md_file_fmtstr, ...)
{
    va_list ap;

    HDmemset(config, 0, sizeof(H5F_vfd_swmr_config_t));

    config->version                = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config->pb_expansion_threshold = 0;

    config->tick_len          = tick_len;
    config->max_lag           = max_lag;
    config->writer            = writer;
    config->flush_raw_data    = flush_raw_data;
    config->md_pages_reserved = md_pages_reserved;

    HDva_start(ap, md_file_fmtstr);
    evsnprintf(config->md_file_path, sizeof(config->md_file_path), md_file_fmtstr, ap);
    HDva_end(ap);

} /* init_vfd_swmr_config() */

void
init_vfd_swmr_log(H5F_vfd_swmr_config_t *config, const char *log_file_fmtstr,...)

{
    va_list ap;

    HDva_start(ap, log_file_fmtstr);
    evsnprintf(config->log_file_path, sizeof(config->log_file_path), log_file_fmtstr, ap);
    HDva_end(ap);

} /* init_vfd_swmr_log() */


/* Perform common VFD SWMR configuration on the file-access property list:
 * configure page buffering, set reasonable VFD SWMR defaults.
 */
/* Set up the file-access property list:
 * --configure for latest format or not
 * --configure the page buffer size to page_buf_size
 * --configure page buffering with only_meta_pages or not
 * --configure for VFD SWMR or not
 */
hid_t
vfd_swmr_create_fapl(bool use_latest_format, bool use_vfd_swmr, bool only_meta_pages, size_t page_buf_size,
                     H5F_vfd_swmr_config_t *config)
{
    hid_t fapl = H5I_INVALID_HID;

    /* Create file access property list */
    if ((fapl = h5_fileaccess()) < 0)
        return H5I_INVALID_HID;

    if (use_latest_format) {
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            return H5I_INVALID_HID;
    }
    else { /* Currently this is used only for old-styled group implementation tests.*/
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
            return H5I_INVALID_HID;
    }

    /* Enable page buffering */
    if (H5Pset_page_buffer_size(fapl, page_buf_size, only_meta_pages ? 100 : 0, 0) < 0)
        return H5I_INVALID_HID;

    /*
     * Set up to open the file with VFD SWMR configured.
     */
    /* Enable VFD SWMR configuration */
    if (use_vfd_swmr && H5Pset_vfd_swmr_config(fapl, config) < 0)
        return H5I_INVALID_HID;

    return fapl;

} /* vfd_swmr_create_fapl() */

/* Create the file creation property list:
 * --Set the file space strategy to fs_strategy
 * --Set the file space page size to fs_page_size
 */
hid_t
vfd_swmr_create_fcpl(H5F_fspace_strategy_t fs_strategy, hsize_t fs_page_size)
{
    hid_t fcpl = H5I_INVALID_HID;

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        return H5I_INVALID_HID;

    if (H5Pset_file_space_strategy(fcpl, fs_strategy, false, 1) < 0)
        return H5I_INVALID_HID;

    if (H5Pset_file_space_page_size(fcpl, fs_page_size) < 0)
        return H5I_INVALID_HID;

    return fcpl;

} /* vfd_swmr_create_fcpl() */

/* Fetch a variable from the environment and parse it for unsigned long
 * content.  Return 0 if the variable is not present, -1 if it is present
 * but it does not parse and compare less than `limit`, 1 if it's present,
 * parses, and is in-bounds.
 */
int
fetch_env_ulong(const char *varname, unsigned long limit, unsigned long *valp)
{
    char *        end;
    unsigned long ul;
    char *        tmp;

    if ((tmp = HDgetenv(varname)) == NULL)
        return 0;

    errno = 0;
    ul    = HDstrtoul(tmp, &end, 0);
    if (ul == ULONG_MAX && errno != 0) {
        HDfprintf(stderr, "could not parse %s: %s\n", varname, HDstrerror(errno));
        return -1;
    }
    if (end == tmp || *end != '\0') {
        HDfprintf(stderr, "could not parse %s\n", varname);
        return -1;
    }
    if (ul > limit) {
        HDfprintf(stderr, "%s (%lu) out of range\n", varname, ul);
        return -1;
    }
    *valp = ul;
    return 1;
}
