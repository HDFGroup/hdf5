/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_poll.h"
#include "mercury_atomic.h"
#include "mercury_event.h"
#include "mercury_list.h"
#include "mercury_thread_spin.h"
#include "mercury_util_error.h"

#include <stdlib.h>

#if defined(_WIN32)
/* TODO */
#else
#    include <errno.h>
#    include <string.h>
#    include <unistd.h>
#    if defined(HG_UTIL_HAS_SYSEPOLL_H)
#        include <sys/epoll.h>
#    elif defined(HG_UTIL_HAS_SYSEVENT_H)
#        include <sys/event.h>
#        include <sys/time.h>
#    else
#        include <poll.h>
#    endif
#endif /* defined(_WIN32) */

/****************/
/* Local Macros */
/****************/

#define HG_POLL_MAX_EVENTS 1024

#ifndef MIN
#    define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

/************************************/
/* Local Type and Struct Definition */
/************************************/

struct hg_poll_data {
#if defined(HG_UTIL_HAS_SYSEPOLL_H)
    int fd;
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    struct kevent kev;
#else
    struct pollfd pollfd;
#endif
    hg_poll_cb_t poll_cb;
    void *poll_arg;
    HG_LIST_ENTRY(hg_poll_data) entry;
};

struct hg_poll_set {
    int fd;
    hg_atomic_int32_t nfds;
    hg_poll_try_wait_cb_t try_wait_cb;
    void *try_wait_arg;
    HG_LIST_HEAD(hg_poll_data) poll_data_list;
    hg_thread_spin_t poll_data_list_lock;
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
    HG_UTIL_CHECK_ERROR_NORET(
        hg_poll_set == NULL, error, "malloc() failed (%s)");
#if defined(_WIN32)
    /* TODO */
#else
    HG_LIST_INIT(&hg_poll_set->poll_data_list);
    hg_thread_spin_init(&hg_poll_set->poll_data_list_lock);
    hg_atomic_init32(&hg_poll_set->nfds, 0);
    hg_poll_set->try_wait_cb = NULL;

#    if defined(HG_UTIL_HAS_SYSEPOLL_H)
    hg_poll_set->fd = epoll_create1(0);
    HG_UTIL_CHECK_ERROR_NORET(hg_poll_set->fd == -1, error,
        "epoll_create1() failed (%s)", strerror(errno));
#    elif defined(HG_UTIL_HAS_SYSEVENT_H)
    hg_poll_set->fd = kqueue();
    HG_UTIL_CHECK_ERROR_NORET(
        hg_poll_set->fd == -1, error, "kqueue() failed (%s)", strerror(errno));
#    else
    hg_poll_set->fd = hg_event_create();
    HG_UTIL_CHECK_ERROR_NORET(hg_poll_set->fd == -1, error,
        "hg_event_create() failed (%s)", strerror(errno));
#    endif
#endif /* defined(_WIN32) */

    return hg_poll_set;

error:
    if (hg_poll_set) {
        hg_thread_spin_destroy(&hg_poll_set->poll_data_list_lock);
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

#if defined(_WIN32)
        /* TODO */
#else
    HG_UTIL_CHECK_ERROR(hg_atomic_get32(&poll_set->nfds), done, ret,
        HG_UTIL_FAIL, "Poll set non empty");

#    if defined(HG_UTIL_HAS_SYSEPOLL_H) || defined(HG_UTIL_HAS_SYSEVENT_H)
    /* Close poll descriptor */
    rc = close(poll_set->fd);
    HG_UTIL_CHECK_ERROR(rc == -1, done, ret, HG_UTIL_FAIL,
        "close() failed (%s)", strerror(errno));
#    else
    rc = hg_event_destroy(poll_set->fd);
    HG_UTIL_CHECK_ERROR(rc == HG_UTIL_FAIL, done, ret, HG_UTIL_FAIL,
        "hg_event_destroy() failed (%s)", strerror(errno));
#    endif

    hg_thread_spin_destroy(&poll_set->poll_data_list_lock);
#endif /* defined(_WIN32) */

    free(poll_set);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
int
hg_poll_get_fd(hg_poll_set_t *poll_set)
{
    int fd = -1;

    HG_UTIL_CHECK_ERROR_NORET(!poll_set, done, "NULL poll set");

#if defined(_WIN32)
    /* TODO */
#else
    fd = poll_set->fd;
#endif

done:
    return fd;
}

/*---------------------------------------------------------------------------*/
int
hg_poll_set_try_wait(
    hg_poll_set_t *poll_set, hg_poll_try_wait_cb_t try_wait_cb, void *arg)
{
    int ret = HG_UTIL_SUCCESS;

    HG_UTIL_CHECK_ERROR(!poll_set, done, ret, HG_UTIL_FAIL, "NULL poll set");

    poll_set->try_wait_cb = try_wait_cb;
    poll_set->try_wait_arg = arg;

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
int
hg_poll_add(hg_poll_set_t *poll_set, int fd, unsigned int flags,
    hg_poll_cb_t poll_cb, void *poll_arg)
{
    struct hg_poll_data *hg_poll_data = NULL;
    int ret = HG_UTIL_SUCCESS;

    HG_UTIL_CHECK_ERROR(!poll_set, done, ret, HG_UTIL_FAIL, "NULL poll set");

    /* Allocate poll data that can hold user data and callback */
    hg_poll_data = malloc(sizeof(struct hg_poll_data));
    HG_UTIL_CHECK_ERROR(
        !hg_poll_data, done, ret, HG_UTIL_FAIL, "malloc() failed (%s)");
    memset(hg_poll_data, 0, sizeof(struct hg_poll_data));
    hg_poll_data->poll_cb = poll_cb;
    hg_poll_data->poll_arg = poll_arg;

    if (fd > 0) {
#if defined(_WIN32)
        /* TODO */
#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
        struct epoll_event ev;
        uint32_t poll_flags;
        int rc;

        /* Translate flags */
        switch (flags) {
            case HG_POLLIN:
                poll_flags = EPOLLIN;
                break;
            case HG_POLLOUT:
                poll_flags = EPOLLOUT;
                break;
            default:
                HG_UTIL_GOTO_ERROR(error, ret, HG_UTIL_FAIL, "Invalid flag");
        }

        hg_poll_data->fd = fd;
        ev.events = poll_flags;
        ev.data.ptr = hg_poll_data;

        rc = epoll_ctl(poll_set->fd, EPOLL_CTL_ADD, fd, &ev);
        HG_UTIL_CHECK_ERROR(rc != 0, error, ret, HG_UTIL_FAIL,
            "epoll_ctl() failed (%s)", strerror(errno));
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
        struct timespec timeout = {0, 0};
        int16_t poll_flags;
        int rc;

        /* Translate flags */
        switch (flags) {
            case HG_POLLIN:
                poll_flags = EVFILT_READ;
                break;
            case HG_POLLOUT:
                poll_flags = EVFILT_WRITE;
                break;
            default:
                HG_UTIL_GOTO_ERROR(error, ret, HG_UTIL_FAIL, "Invalid flag");
        }

        EV_SET(&hg_poll_data->kev, (uintptr_t) fd, poll_flags, EV_ADD, 0, 0,
            hg_poll_data);

        rc = kevent(poll_set->fd, &hg_poll_data->kev, 1, NULL, 0, &timeout);
        HG_UTIL_CHECK_ERROR(rc == -1, error, ret, HG_UTIL_FAIL,
            "kevent() failed (%s)", strerror(errno));
#else
        short int poll_flags;

        /* Translate flags */
        switch (flags) {
            case HG_POLLIN:
                poll_flags = POLLIN;
                break;
            case HG_POLLOUT:
                poll_flags = POLLOUT;
                break;
            default:
                HG_UTIL_GOTO_ERROR(error, ret, HG_UTIL_FAIL, "Invalid flag");
        }

        hg_poll_data->pollfd.fd = fd;
        hg_poll_data->pollfd.events = poll_flags;
        hg_poll_data->pollfd.revents = 0;
#endif /* defined(_WIN32) */
    }
    hg_atomic_incr32(&poll_set->nfds);

    hg_thread_spin_lock(&poll_set->poll_data_list_lock);
    HG_LIST_INSERT_HEAD(&poll_set->poll_data_list, hg_poll_data, entry);
    hg_thread_spin_unlock(&poll_set->poll_data_list_lock);

done:
    return ret;

error:
    free(hg_poll_data);

    return HG_UTIL_FAIL;
}

/*---------------------------------------------------------------------------*/
int
hg_poll_remove(hg_poll_set_t *poll_set, int fd)
{
    struct hg_poll_data *hg_poll_data;
    hg_util_bool_t found = HG_UTIL_FALSE;
    int ret = HG_UTIL_SUCCESS;

    HG_UTIL_CHECK_ERROR(!poll_set, done, ret, HG_UTIL_FAIL, "NULL poll set");

    hg_thread_spin_lock(&poll_set->poll_data_list_lock);
    HG_LIST_FOREACH (hg_poll_data, &poll_set->poll_data_list, entry) {
#if defined(_WIN32)
        /* TODO */
#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
        if (hg_poll_data->fd == fd) {
            HG_LIST_REMOVE(hg_poll_data, entry);

            if (fd > 0) {
                int rc = epoll_ctl(poll_set->fd, EPOLL_CTL_DEL, fd, NULL);
                HG_UTIL_CHECK_ERROR(rc != 0, error, ret, HG_UTIL_FAIL,
                    "epoll_ctl() failed (%s)", strerror(errno));
            }
            free(hg_poll_data);
            found = HG_UTIL_TRUE;
            break;
        }
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
        /* Events which are attached to file descriptors are automatically
         * deleted on the last close of the descriptor. */
        if ((int) hg_poll_data->kev.ident == fd) {
            HG_LIST_REMOVE(hg_poll_data, entry);

            if (fd > 0) {
                struct timespec timeout = {0, 0};
                int rc;

                EV_SET(&hg_poll_data->kev, (uintptr_t) fd, EVFILT_READ,
                    EV_DELETE, 0, 0, NULL);
                rc = kevent(
                    poll_set->fd, &hg_poll_data->kev, 1, NULL, 0, &timeout);
                HG_UTIL_CHECK_ERROR(rc == -1, error, ret, HG_UTIL_FAIL,
                    "kevent() failed (%s)", strerror(errno));
            }
            free(hg_poll_data);
            found = HG_UTIL_TRUE;
            break;
        }
#else
        if (hg_poll_data->pollfd.fd == fd) {
            HG_LIST_REMOVE(hg_poll_data, entry);
            free(hg_poll_data);
            found = HG_UTIL_TRUE;
            break;
        }
#endif
    }
    hg_thread_spin_unlock(&poll_set->poll_data_list_lock);

    HG_UTIL_CHECK_ERROR(
        !found, done, ret, HG_UTIL_FAIL, "Could not find fd in poll_set");
    hg_atomic_decr32(&poll_set->nfds);

done:
    return ret;

#if defined(HG_UTIL_HAS_SYSEPOLL_H) || defined(HG_UTIL_HAS_SYSEVENT_H)
error:
    hg_thread_spin_unlock(&poll_set->poll_data_list_lock);

    return ret;
#endif
}

/*---------------------------------------------------------------------------*/
int
hg_poll_wait(hg_poll_set_t *poll_set, unsigned int timeout,
    unsigned int max_events, struct hg_poll_event *events,
    unsigned int *actual_events)
{
    int max_poll_events = (int) MIN(max_events, HG_POLL_MAX_EVENTS);
    int nfds = 0, i;
    int ret = HG_UTIL_SUCCESS;

    HG_UTIL_CHECK_ERROR(!poll_set, done, ret, HG_UTIL_FAIL, "NULL poll set");

    if (timeout && (!poll_set->try_wait_cb ||
                       (poll_set->try_wait_cb &&
                           poll_set->try_wait_cb(poll_set->try_wait_arg)))) {
#if defined(_WIN32)

#elif defined(HG_UTIL_HAS_SYSEPOLL_H)
        struct epoll_event poll_events[HG_POLL_MAX_EVENTS];

        nfds = epoll_wait(
            poll_set->fd, poll_events, max_poll_events, (int) timeout);
        HG_UTIL_CHECK_ERROR(nfds == -1 && errno != EINTR, done, ret,
            HG_UTIL_FAIL, "epoll_wait() failed (%s)", strerror(errno));

        for (i = 0; i < nfds; ++i) {
            struct hg_poll_data *hg_poll_data =
                (struct hg_poll_data *) poll_events[i].data.ptr;
            int error = 0, rc;

            HG_UTIL_CHECK_ERROR(hg_poll_data == NULL, done, ret, HG_UTIL_FAIL,
                "NULL poll data");

            /* Don't change the if/else order */
            if (poll_events[i].events & EPOLLERR)
                error = EPOLLERR;
            else if (poll_events[i].events & EPOLLHUP)
                error = EPOLLHUP;
            else if (poll_events[i].events & EPOLLRDHUP)
                error = EPOLLRDHUP;

            HG_UTIL_CHECK_ERROR(!(poll_events[i].events & (EPOLLIN | EPOLLOUT)),
                done, ret, HG_UTIL_FAIL, "Unsupported events");

            if (!hg_poll_data->poll_cb)
                continue;

            rc = hg_poll_data->poll_cb(
                hg_poll_data->poll_arg, error, &events[i]);
            HG_UTIL_CHECK_ERROR(rc != HG_UTIL_SUCCESS, done, ret, HG_UTIL_FAIL,
                "poll cb failed");
        }
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
        struct kevent poll_events[HG_POLL_MAX_EVENTS];
        struct timespec timeout_spec;
        ldiv_t ld;

        /* Get sec / nsec */
        ld = ldiv(timeout, 1000L);
        timeout_spec.tv_sec = ld.quot;
        timeout_spec.tv_nsec = ld.rem * 1000000L;

        nfds = kevent(
            poll_set->fd, NULL, 0, poll_events, max_events, &timeout_spec);
        HG_UTIL_CHECK_ERROR(nfds == -1 && errno != EINTR, done, ret,
            HG_UTIL_FAIL, "kevent() failed (%s)", strerror(errno));

        for (i = 0; i < nfds; ++i) {
            struct hg_poll_data *hg_poll_data =
                (struct hg_poll_data *) poll_events[i].udata;
            int rc;

            HG_UTIL_CHECK_ERROR(hg_poll_data == NULL, done, ret, HG_UTIL_FAIL,
                "NULL poll data");

            if (!hg_poll_data->poll_cb)
                continue;

            rc = hg_poll_data->poll_cb(hg_poll_data->poll_arg, 0, &events[i]);
            HG_UTIL_CHECK_ERROR(rc != HG_UTIL_SUCCESS, done, ret, HG_UTIL_FAIL,
                "poll cb failed");
        }
#else
        struct pollfd poll_events[HG_POLL_MAX_EVENTS] = {0};
        struct hg_poll_data *poll_data_events[HG_POLL_MAX_EVENTS] = {NULL};
        struct hg_poll_data *hg_poll_data = NULL;
        int nevents = 0;

        /* Reset revents */
        hg_thread_spin_lock(&poll_set->poll_data_list_lock);
        for (hg_poll_data = HG_LIST_FIRST(&poll_set->poll_data_list);
             hg_poll_data && (nevents < max_poll_events);
             hg_poll_data = HG_LIST_NEXT(hg_poll_data, entry), nevents++) {
            poll_events[nevents] = hg_poll_data->pollfd;
            poll_data_events[nevents] = hg_poll_data;
        }
        hg_thread_spin_unlock(&poll_set->poll_data_list_lock);

        nfds = poll(poll_events, nevents, (int) timeout);
        HG_UTIL_CHECK_ERROR(nfds == -1 && errno != EINTR, done, ret,
            HG_UTIL_FAIL, "poll() failed (%s)", strerror(errno));

        /* An event on one of the fds has occurred. */
        for (i = 0; i < nfds; ++i) {
            int rc;

            if (!(poll_events[i].revents & poll_events[i].events))
                continue;

            /* TODO check POLLHUP | POLLERR | POLLNVAL */
            if (!poll_data_events[i]->poll_cb)
                continue;

            rc = poll_data_events[i]->poll_cb(
                poll_data_events[i]->poll_arg, 0, &events[i]);
            HG_UTIL_CHECK_ERROR(rc != HG_UTIL_SUCCESS, done, ret, HG_UTIL_FAIL,
                "poll cb failed");
        }

        if (nfds) {
            /* TODO should figure where to call hg_event_get() */
            int rc = hg_event_set(poll_set->fd);
            HG_UTIL_CHECK_ERROR(rc != HG_UTIL_SUCCESS, done, ret, HG_UTIL_FAIL,
                "hg_event_set() failed (%s)", strerror(errno));
        }
#endif
    } else {
#ifdef _WIN32

#else
        struct hg_poll_data *poll_data_events[HG_POLL_MAX_EVENTS] = {NULL};
        struct hg_poll_data *hg_poll_data;
        int nevents = 0;

        /* Reset revents */
        hg_thread_spin_lock(&poll_set->poll_data_list_lock);
        for (hg_poll_data = HG_LIST_FIRST(&poll_set->poll_data_list);
             hg_poll_data && (nevents < max_poll_events);
             hg_poll_data = HG_LIST_NEXT(hg_poll_data, entry), nevents++)
            poll_data_events[nevents] = hg_poll_data;
        hg_thread_spin_unlock(&poll_set->poll_data_list_lock);

        nfds = nevents;
        for (i = 0; i < nfds; ++i) {
            int rc;

            if (!poll_data_events[i]->poll_cb)
                continue;

            rc = poll_data_events[i]->poll_cb(
                poll_data_events[i]->poll_arg, 0, &events[i]);
            HG_UTIL_CHECK_ERROR(rc != HG_UTIL_SUCCESS, done, ret, HG_UTIL_FAIL,
                "poll cb failed");
        }
#endif
    }

    if (actual_events)
        *actual_events = (unsigned int) nfds;

done:
    return ret;
}
