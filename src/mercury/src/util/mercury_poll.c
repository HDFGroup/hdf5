/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_poll.h"
#include "mercury_event.h"
#include "mercury_thread_mutex.h"
#include "mercury_util_error.h"

#include <stdlib.h>

#if defined(_WIN32)
/* TODO */
#else
#include <errno.h>
#include <string.h>
#include <unistd.h>
#if defined(HG_UTIL_HAS_SYSEPOLL_H)
#include <sys/epoll.h>
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
#include <sys/event.h>
#include <sys/time.h>
#else
#include <poll.h>
#endif
#endif /* defined(_WIN32) */

/****************/
/* Local Macros */
/****************/

#define HG_POLL_INIT_NEVENTS 32
#define HG_POLL_MAX_EVENTS   4096

#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

/************************************/
/* Local Type and Struct Definition */
/************************************/

struct hg_poll_set {
    hg_thread_mutex_t lock;
#if defined(HG_UTIL_HAS_SYSEPOLL_H)
    struct epoll_event *events;
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    struct kevent *events;
#else
    struct pollfd * events;
    hg_poll_data_t *event_data;
#endif
    unsigned int max_events;
    unsigned int nfds;
    int          fd;
};

/********************/
/* Local Prototypes */
/********************/

/*******************/
/* Local Variables */
/*******************/

/*---------------------------------------------------------------------------*/
hg_poll_set_t *
hg_poll_create(void)
{
    struct hg_poll_set *hg_poll_set = NULL;

    hg_poll_set = malloc(sizeof(struct hg_poll_set));
    HG_UTIL_CHECK_ERROR_NORET(hg_poll_set == NULL, error, "malloc() failed (%s)", strerror(errno));

    hg_thread_mutex_init(&hg_poll_set->lock);
    hg_poll_set->nfds       = 0;
    hg_poll_set->max_events = HG_POLL_INIT_NEVENTS;

    /* Preallocate events, size will grow as needed */
    hg_poll_set->events = malloc(sizeof(*hg_poll_set->events) * hg_poll_set->max_events);
    HG_UTIL_CHECK_ERROR_NORET(!hg_poll_set->events, error, "malloc() failed (%s)", strerror(errno));

#if defined(_WIN32)
    /* TODO */
#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
    hg_poll_set->fd = epoll_create1(0);
    HG_UTIL_CHECK_ERROR_NORET(hg_poll_set->fd == -1, error, "epoll_create1() failed (%s)", strerror(errno));
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    hg_poll_set->fd = kqueue();
    HG_UTIL_CHECK_ERROR_NORET(hg_poll_set->fd == -1, error, "kqueue() failed (%s)", strerror(errno));
#else
    hg_poll_set->fd = hg_event_create();
    HG_UTIL_CHECK_ERROR_NORET(hg_poll_set->fd == -1, error, "hg_event_create() failed (%s)", strerror(errno));

    /* Preallocate event_data, size will grow as needed */
    hg_poll_set->event_data = malloc(sizeof(*hg_poll_set->event_data) * hg_poll_set->max_events);
    HG_UTIL_CHECK_ERROR_NORET(!hg_poll_set->events, error, "malloc() failed (%s)", strerror(errno));
#endif
    HG_UTIL_LOG_DEBUG("Created new poll set, fd=%d", hg_poll_set->fd);

    return hg_poll_set;

error:
    if (hg_poll_set) {
        free(hg_poll_set->events);
        hg_thread_mutex_destroy(&hg_poll_set->lock);
        free(hg_poll_set);
    }
    return NULL;
}

/*---------------------------------------------------------------------------*/
int
hg_poll_destroy(hg_poll_set_t *poll_set)
{
    int ret = HG_UTIL_SUCCESS;
    int rc;

    if (!poll_set)
        goto done;

    HG_UTIL_CHECK_ERROR(poll_set->nfds > 0, done, ret, HG_UTIL_FAIL, "Poll set non empty");

    HG_UTIL_LOG_DEBUG("Destroying poll set, fd=%d", poll_set->fd);

#if defined(_WIN32)
    /* TODO */
#elif defined(HG_UTIL_HAS_SYSEPOLL_H) || defined(HG_UTIL_HAS_SYSEVENT_H)
    /* Close poll descriptor */
    rc = close(poll_set->fd);
    HG_UTIL_CHECK_ERROR(rc == -1, done, ret, HG_UTIL_FAIL, "close() failed (%s)", strerror(errno));
#else
    rc = hg_event_destroy(poll_set->fd);
    HG_UTIL_CHECK_ERROR(rc == HG_UTIL_FAIL, done, ret, HG_UTIL_FAIL, "hg_event_destroy() failed (%s)",
                        strerror(errno));
#endif

    hg_thread_mutex_destroy(&poll_set->lock);
#if !defined(_WIN32) && !defined(HG_UTIL_HAS_SYSEPOLL_H) && !defined(HG_UTIL_HAS_SYSEVENT_H)
    free(poll_set->event_data);
#endif
    free(poll_set->events);
    free(poll_set);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
int
hg_poll_get_fd(hg_poll_set_t *poll_set)
{
#if defined(_WIN32)
    /* TODO */
    return -1;
#else
    return poll_set->fd;
#endif
}

/*---------------------------------------------------------------------------*/
int
hg_poll_add(hg_poll_set_t *poll_set, int fd, struct hg_poll_event *event)
{
#if defined(_WIN32)
    /* TODO */
#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
    struct epoll_event ev;
    uint32_t           poll_flags = 0;
    int                rc;
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    struct kevent   ev;
    struct timespec timeout    = {0, 0};
    int16_t         poll_flags = 0;
    int             rc;
#else
    struct pollfd ev;
    short int     poll_flags = 0;
#endif
    int ret = HG_UTIL_SUCCESS;

    HG_UTIL_LOG_DEBUG("Adding fd=%d to poll set (fd=%d)", fd, poll_set->fd);

#if defined(_WIN32)
    /* TODO */
#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
    /* Translate flags */
    if (event->events & HG_POLLIN)
        poll_flags |= EPOLLIN;
    if (event->events & HG_POLLOUT)
        poll_flags |= EPOLLOUT;

    ev.events   = poll_flags;
    ev.data.u64 = (uint64_t)event->data.u64;

    rc = epoll_ctl(poll_set->fd, EPOLL_CTL_ADD, fd, &ev);
    HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "epoll_ctl() failed (%s)", strerror(errno));
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    /* Translate flags */
    if (event->events & HG_POLLIN)
        poll_flags |= EVFILT_READ;
    if (event->events & HG_POLLOUT)
        poll_flags |= EVFILT_WRITE;

    EV_SET(&ev, (uintptr_t)fd, poll_flags, EV_ADD, 0, 0, event->data.ptr);

    rc = kevent(poll_set->fd, &ev, 1, NULL, 0, &timeout);
    HG_UTIL_CHECK_ERROR(rc == -1, done, ret, HG_UTIL_FAIL, "kevent() failed (%s)", strerror(errno));
#else
    /* Translate flags */
    if (event->events & HG_POLLIN)
        poll_flags |= POLLIN;
    if (event->events & HG_POLLOUT)
        poll_flags |= POLLOUT;

    ev.fd      = fd;
    ev.events  = poll_flags;
    ev.revents = 0;
#endif

    hg_thread_mutex_lock(&poll_set->lock);

#if !defined(_WIN32) && !defined(HG_UTIL_HAS_SYSEPOLL_H) && !defined(HG_UTIL_HAS_SYSEVENT_H)
    /* Grow array if reached max number */
    if (poll_set->nfds == poll_set->max_events) {
        HG_UTIL_CHECK_ERROR(poll_set->max_events * 2 > HG_POLL_MAX_EVENTS, unlock, ret, HG_UTIL_FAIL,
                            "reached max number of events for this poll set (%d)", poll_set->max_events);

        poll_set->events = realloc(poll_set->events, sizeof(*poll_set->events) * poll_set->max_events * 2);
        HG_UTIL_CHECK_ERROR(!poll_set->events, unlock, ret, HG_UTIL_FAIL, "realloc() failed (%s)",
                            strerror(errno));

        poll_set->event_data =
            realloc(poll_set->event_data, sizeof(*poll_set->event_data) * poll_set->max_events * 2);
        HG_UTIL_CHECK_ERROR(!poll_set->event_data, unlock, ret, HG_UTIL_FAIL, "realloc() failed (%s)",
                            strerror(errno));

        poll_set->max_events *= 2;
    }
    poll_set->events[poll_set->nfds]     = ev;
    poll_set->event_data[poll_set->nfds] = event->data;
#endif
    poll_set->nfds++;

#if !defined(_WIN32) && !defined(HG_UTIL_HAS_SYSEPOLL_H) && !defined(HG_UTIL_HAS_SYSEVENT_H)
unlock:
#endif
    hg_thread_mutex_unlock(&poll_set->lock);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
int
hg_poll_remove(hg_poll_set_t *poll_set, int fd)
{
#if defined(_WIN32)
    /* TODO */
#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
    int rc;
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    struct kevent   ev;
    struct timespec timeout = {0, 0};
    int             rc;
#else
    int i, found = -1;
#endif
    int ret = HG_UTIL_SUCCESS;

    HG_UTIL_LOG_DEBUG("Removing fd=%d from poll set (fd=%d)", fd, poll_set->fd);

#if defined(_WIN32)
    /* TODO */
#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
    rc = epoll_ctl(poll_set->fd, EPOLL_CTL_DEL, fd, NULL);
    HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "epoll_ctl() failed (%s)", strerror(errno));
    hg_thread_mutex_lock(&poll_set->lock);
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    /* Events which are attached to file descriptors are automatically
     * deleted on the last close of the descriptor. */
    EV_SET(&ev, (uintptr_t)fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
    rc = kevent(poll_set->fd, &ev, 1, NULL, 0, &timeout);
    HG_UTIL_CHECK_ERROR(rc == -1, done, ret, HG_UTIL_FAIL, "kevent() failed (%s)", strerror(errno));
    hg_thread_mutex_lock(&poll_set->lock);
#else
    hg_thread_mutex_lock(&poll_set->lock);
    for (i = 0; i < (int)poll_set->nfds; i++) {
        if (poll_set->events[i].fd == fd) {
            found = i;
            break;
        }
    }
    HG_UTIL_CHECK_ERROR(found < 0, error, ret, HG_UTIL_FAIL, "Could not find fd in poll_set");

    for (i = found; i < (int)poll_set->nfds - 1; i++) {
        poll_set->events[i]     = poll_set->events[i + 1];
        poll_set->event_data[i] = poll_set->event_data[i + 1];
    }
#endif
    poll_set->nfds--;
    hg_thread_mutex_unlock(&poll_set->lock);

done:
    return ret;

#if !defined(_WIN32) && !defined(HG_UTIL_HAS_SYSEPOLL_H) && !defined(HG_UTIL_HAS_SYSEVENT_H)
error:
    hg_thread_mutex_unlock(&poll_set->lock);

    return ret;
#endif
}

/*---------------------------------------------------------------------------*/
int
hg_poll_wait(hg_poll_set_t *poll_set, unsigned int timeout, unsigned int max_events,
             struct hg_poll_event *events, unsigned int *actual_events)
{
    int max_poll_events = (int)MIN(max_events, poll_set->max_events);
    int nfds            = 0, i;
    int ret             = HG_UTIL_SUCCESS;

#if defined(_WIN32)

#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
    nfds = epoll_wait(poll_set->fd, poll_set->events, max_poll_events, (int)timeout);
    HG_UTIL_CHECK_ERROR(nfds == -1 && errno != EINTR, done, ret, HG_UTIL_FAIL, "epoll_wait() failed (%s)",
                        strerror(errno));

    /* Handle signal interrupts */
    if (unlikely(errno == EINTR)) {
        events[0].events |= HG_POLLINTR;
        *actual_events = 1;

        /* Reset errno */
        errno = 0;

        return HG_UTIL_SUCCESS;
    }

    for (i = 0; i < nfds; ++i) {
        events[i].events   = 0;
        events[i].data.u64 = (hg_util_uint64_t)poll_set->events[i].data.u64;

        if (poll_set->events[i].events & EPOLLIN)
            events[i].events |= HG_POLLIN;

        if (poll_set->events[i].events & EPOLLOUT)
            events[i].events |= HG_POLLOUT;

        /* Don't change the if/else order */
        if (poll_set->events[i].events & EPOLLERR)
            events[i].events |= HG_POLLERR;
        else if (poll_set->events[i].events & EPOLLHUP)
            events[i].events |= HG_POLLHUP;
        else if (poll_set->events[i].events & EPOLLRDHUP)
            events[i].events |= HG_POLLHUP;
    }

    /* Grow array if reached max number */
    if ((nfds == (int)poll_set->max_events) && (poll_set->max_events * 2 <= HG_POLL_MAX_EVENTS)) {
        poll_set->events = realloc(poll_set->events, sizeof(*poll_set->events) * poll_set->max_events * 2);
        HG_UTIL_CHECK_ERROR(!poll_set->events, done, ret, HG_UTIL_FAIL, "realloc() failed (%s)",
                            strerror(errno));

        poll_set->max_events *= 2;
    }
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    struct timespec timeout_spec;
    ldiv_t          ld;

    /* Get sec / nsec */
    ld                   = ldiv(timeout, 1000L);
    timeout_spec.tv_sec  = ld.quot;
    timeout_spec.tv_nsec = ld.rem * 1000000L;

    nfds = kevent(poll_set->fd, NULL, 0, poll_set->events, max_poll_events, &timeout_spec);
    HG_UTIL_CHECK_ERROR(nfds == -1 && errno != EINTR, done, ret, HG_UTIL_FAIL, "kevent() failed (%s)",
                        strerror(errno));

    /* Handle signal interrupts */
    if (unlikely(errno == EINTR)) {
        events[0].events |= HG_POLLINTR;
        *actual_events = 1;

        return HG_UTIL_SUCCESS;
    }

    for (i = 0; i < nfds; ++i) {
        events[i].events   = 0;
        events[i].data.ptr = poll_set->events[i].udata;

        if (poll_set->events[i].flags & EVFILT_READ)
            events[i].events |= HG_POLLIN;

        if (poll_set->events[i].flags & EVFILT_WRITE)
            events[i].events |= HG_POLLOUT;
    }

    /* Grow array if reached max number */
    if ((nfds == (int)poll_set->max_events) && (poll_set->max_events * 2 <= HG_POLL_MAX_EVENTS)) {
        poll_set->events = realloc(poll_set->events, sizeof(*poll_set->events) * poll_set->max_events * 2);
        HG_UTIL_CHECK_ERROR(!poll_set->events, done, ret, HG_UTIL_FAIL, "realloc() failed (%s)",
                            strerror(errno));

        poll_set->max_events *= 2;
    }
#else
    int            nevent = 0, rc;
    hg_util_bool_t signaled;

    rc = hg_event_get(poll_set->fd, &signaled);
    HG_UTIL_CHECK_ERROR(rc != HG_UTIL_SUCCESS, done, ret, HG_UTIL_FAIL, "hg_event_get() failed (%s)",
                        strerror(errno));
    if (signaled) {
        /* Should we do anything in that case? */
    }

    hg_thread_mutex_lock(&poll_set->lock);

    /* Reset revents */
    for (i = 0; i < (int)poll_set->nfds; i++)
        poll_set->events[i].revents = 0;

    nfds = poll(poll_set->events, (nfds_t)poll_set->nfds, (int)timeout);
    HG_UTIL_CHECK_ERROR(nfds == -1 && errno != EINTR, unlock, ret, HG_UTIL_FAIL, "poll() failed (%s)",
                        strerror(errno));

    /* Handle signal interrupts */
    if (unlikely(errno == EINTR)) {
        events[0].events |= HG_POLLINTR;
        *actual_events = 1;
        hg_thread_mutex_unlock(&poll_set->lock);

        return HG_UTIL_SUCCESS;
    }

    nfds = (int)MIN(max_poll_events, nfds);

    /* An event on one of the fds has occurred. */
    for (i = 0; i < (int)poll_set->nfds && nevent < nfds; ++i) {
        events[i].events   = 0;
        events[i].data.u64 = (hg_util_uint64_t)poll_set->event_data[i].u64;

        if (poll_set->events[i].revents & POLLIN)
            events[i].events |= HG_POLLIN;

        if (poll_set->events[i].revents & POLLOUT)
            events[i].events |= HG_POLLOUT;

        /* Don't change the if/else order */
        if (poll_set->events[i].revents & POLLERR)
            events[i].events |= HG_POLLERR;
        else if (poll_set->events[i].revents & POLLHUP)
            events[i].events |= HG_POLLHUP;
        else if (poll_set->events[i].events & POLLNVAL)
            events[i].events |= HG_POLLERR;

        nevent++;
    }

    hg_thread_mutex_unlock(&poll_set->lock);

    HG_UTIL_CHECK_ERROR(nevent != nfds, done, ret, HG_UTIL_FAIL, "found only %d events, expected %d", nevent,
                        nfds);

    if (nfds > 0) {
        /* TODO should figure where to call hg_event_get() */
        rc = hg_event_set(poll_set->fd);
        HG_UTIL_CHECK_ERROR(rc != HG_UTIL_SUCCESS, done, ret, HG_UTIL_FAIL, "hg_event_set() failed (%s)",
                            strerror(errno));
    }
#endif

    *actual_events = (unsigned int)nfds;

done:
    return ret;

#if !defined(_WIN32) && !defined(HG_UTIL_HAS_SYSEPOLL_H) && !defined(HG_UTIL_HAS_SYSEVENT_H)
unlock:
    hg_thread_mutex_unlock(&poll_set->lock);

    return ret;
#endif
}
