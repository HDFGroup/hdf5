/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_EVENT_H
#define MERCURY_EVENT_H

#include "mercury_util_config.h"

#ifdef _WIN32

#else
#include <errno.h>
#include <string.h>
#include <unistd.h>
#if defined(HG_UTIL_HAS_SYSEVENTFD_H)
#include <sys/eventfd.h>
#ifndef HG_UTIL_HAS_EVENTFD_T
typedef uint64_t eventfd_t;
#endif
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
#include <sys/event.h>
#define HG_EVENT_IDENT 42 /* User-defined ident */
#endif
#endif

/**
 * Purpose: define an event object that can be used as an event
 * wait/notify mechanism.
 */

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Create a new event object.
 *
 * \return file descriptor on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_event_create(void);

/**
 * Destroy an event object.
 *
 * \param fd [IN]               event file descriptor
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_event_destroy(int fd);

/**
 * Notify for event.
 *
 * \param fd [IN]               event file descriptor
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int hg_event_set(int fd);

/**
 * Get event notification.
 *
 * \param fd [IN]               event file descriptor
 * \param notified [IN]         boolean set to true if event received
 *
 * \return Non-negative on success or negative on failure
 */
static HG_UTIL_INLINE int hg_event_get(int fd, bool *notified);

/*---------------------------------------------------------------------------*/
#if defined(_WIN32)
static HG_UTIL_INLINE int
hg_event_set(int fd)
{
    return HG_UTIL_FAIL; /* TODO */
}
#elif defined(HG_UTIL_HAS_SYSEVENTFD_H)
#ifdef HG_UTIL_HAS_EVENTFD_T
static HG_UTIL_INLINE int
hg_event_set(int fd)
{
    return (eventfd_write(fd, 1) == 0) ? HG_UTIL_SUCCESS : HG_UTIL_FAIL;
}
#else
static HG_UTIL_INLINE int
hg_event_set(int fd)
{
    eventfd_t count = 1;
    ssize_t   s     = write(fd, &count, sizeof(eventfd_t));

    return (s == sizeof(eventfd_t)) ? HG_UTIL_SUCCESS : HG_UTIL_FAIL;
}
#endif
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
static HG_UTIL_INLINE int
hg_event_set(int fd)
{
    struct kevent   kev;
    struct timespec timeout = {0, 0};
    int             rc;

    EV_SET(&kev, HG_EVENT_IDENT, EVFILT_USER, 0, NOTE_TRIGGER, 0, NULL);

    /* Trigger user-defined event */
    rc = kevent(fd, &kev, 1, NULL, 0, &timeout);

    return (rc == -1) ? HG_UTIL_FAIL : HG_UTIL_SUCCESS;
}
#else
#error "Not supported on this platform."
#endif

/*---------------------------------------------------------------------------*/
#if defined(_WIN32)
static HG_UTIL_INLINE int
hg_event_get(int fd, bool *signaled)
{
    return HG_UTIL_FAIL; /* TODO */
}
#elif defined(HG_UTIL_HAS_SYSEVENTFD_H)
#ifdef HG_UTIL_HAS_EVENTFD_T
static HG_UTIL_INLINE int
hg_event_get(int fd, bool *signaled)
{
    eventfd_t count = 0;

    if ((eventfd_read(fd, &count) == 0) && count)
        *signaled = true;
    else {
        if (errno == EAGAIN)
            *signaled = false;
        else
            return HG_UTIL_FAIL;
    }

    return HG_UTIL_SUCCESS;
}
#else
static HG_UTIL_INLINE int
hg_event_get(int fd, bool *signaled)
{
    eventfd_t count = 0;
    ssize_t   s     = read(fd, &count, sizeof(eventfd_t));
    if ((s == sizeof(eventfd_t)) && count)
        *signaled = true;
    else {
        if (errno == EAGAIN)
            *signaled = false;
        else
            return HG_UTIL_FAIL;
    }

    return HG_UTIL_SUCCESS;
}
#endif
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
static HG_UTIL_INLINE int
hg_event_get(int fd, bool *signaled)
{
    struct kevent   kev;
    int             nfds;
    struct timespec timeout = {0, 0};

    /* Check user-defined event */
    nfds = kevent(fd, NULL, 0, &kev, 1, &timeout);
    if (nfds == -1)
        return HG_UTIL_FAIL;

    *signaled = ((nfds > 0) && (kev.ident == HG_EVENT_IDENT)) ? true : false;

    return HG_UTIL_SUCCESS;
}
#else
#error "Not supported on this platform."
#endif

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_EVENT_H */
