/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_event.h"

#include "mercury_util_error.h"

/*---------------------------------------------------------------------------*/
int
hg_event_create(void)
{
    int fd = -1;
#if defined(_WIN32)

#elif defined(HG_UTIL_HAS_SYSEVENTFD_H)
    /* Create local signal event on self address */
    fd = eventfd(0, EFD_NONBLOCK | EFD_SEMAPHORE);
    HG_UTIL_CHECK_ERROR_NORET(fd == -1, done, "eventfd() failed (%s)", strerror(errno));
#elif defined(HG_UTIL_HAS_SYSEVENT_H)
    struct kevent   kev;
    struct timespec timeout = {0, 0};
    int             rc;

    /* Create kqueue */
    fd = kqueue();
    HG_UTIL_CHECK_ERROR_NORET(fd == -1, done, "kqueue() failed (%s)", strerror(errno));

    EV_SET(&kev, HG_EVENT_IDENT, EVFILT_USER, EV_ADD | EV_CLEAR, 0, 0, NULL);

    /* Add user-defined event to kqueue */
    rc = kevent(fd, &kev, 1, NULL, 0, &timeout);
    HG_UTIL_CHECK_ERROR_NORET(rc == -1, error, "kevent() failed (%s)", strerror(errno));
#else

#endif
    HG_UTIL_LOG_DEBUG("Created event fd=%d", fd);

done:
    return fd;

#if defined(HG_UTIL_HAS_SYSEVENT_H)
error:
    hg_event_destroy(fd);

    return -1;
#endif
}

/*---------------------------------------------------------------------------*/
int
hg_event_destroy(int fd)
{
    int ret = HG_UTIL_SUCCESS, rc;
#if defined(_WIN32)

#else
    rc = close(fd);
    HG_UTIL_CHECK_ERROR(rc == -1, done, ret, HG_UTIL_FAIL, "close() failed (%s)", strerror(errno));
#endif
    HG_UTIL_LOG_DEBUG("Destroyed event fd=%d", fd);

done:
    return ret;
}
