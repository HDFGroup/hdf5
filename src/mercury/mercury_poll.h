/*
 * Copyright (C) 2013-2019 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_POLL_H
#define MERCURY_POLL_H

#include "mercury_util_config.h"

/**
 * Purpose: define an interface that either polls or allows busy wait
 * without entering system calls.
 */

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

typedef struct hg_poll_set hg_poll_set_t;

struct hg_poll_event {
    hg_util_bool_t progressed; /* Indicates progress */
    void *ptr;                 /* Pointer to user data */
};

/**
 * Callback that can be used to signal when it is safe to block on the
 * poll set or if blocking could hang the application.
 *
 * \param arg [IN]              function argument
 *
 * \return HG_UTIL_TRUE if it is safe to block or HG_UTIL_FALSE otherwise
 */
typedef hg_util_bool_t (*hg_poll_try_wait_cb_t)(void *arg);

/**
 * Polling callback, arg can be used to pass user arguments, event can be used
 * to return user arguments back to hg_poll_wait.
 *
 * \param arg [IN]              pointer to user data
 * \param error [IN]            any error event has occurred
 * \param ptr [OUT]             event data output
 *
 * \return Non-negative on success or negative on failure
 */
typedef int (*hg_poll_cb_t)(void *arg, int error, struct hg_poll_event *event);

/*****************/
/* Public Macros */
/*****************/

/**
 * Polling events.
 */
#define HG_POLLIN  0x001 /* Ready to read.   */
#define HG_POLLOUT 0x004 /* Ready to write.  */

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Create a new poll set.
 *
 * \return Pointer to poll set or NULL in case of failure
 */
HG_UTIL_PUBLIC hg_poll_set_t *
hg_poll_create(void);

/**
 * Destroy a poll set.
 *
 * \param poll_set [IN]         pointer to poll set
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_poll_destroy(hg_poll_set_t *poll_set);

/**
 * Get a file descriptor from an existing poll set.
 *
 * \param poll_set [IN]         pointer to poll set
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_poll_get_fd(hg_poll_set_t *poll_set);

/**
 * Set a callback that can be used to signal when it is safe to block on the
 * poll set or if blocking could hang the application, in which case behavior
 * is the same as passing a timeout of 0.
 *
 * \param poll_set [IN]         pointer to poll set
 * \param try_wait_cb [IN]      function pointer
 * \param try_wait_arg [IN]     function pointer argument
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_poll_set_try_wait(hg_poll_set_t *poll_set, hg_poll_try_wait_cb_t try_wait_cb,
    void *try_wait_arg);

/**
 * Add file descriptor to poll set.
 *
 * \param poll_set [IN]         pointer to poll set
 * \param fd [IN]               file descriptor
 * \param flags [IN]            polling flags (HG_POLLIN, etc)
 * \param poll_cb [IN]          function pointer
 * \param poll_cb_args [IN]     function pointer argument
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_poll_add(hg_poll_set_t *poll_set, int fd, unsigned int flags,
    hg_poll_cb_t poll_cb, void *poll_cb_arg);

/**
 * Remove file descriptor from poll set.
 *
 * \param poll_set [IN]         pointer to poll set
 * \param fd [IN]               file descriptor
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_poll_remove(hg_poll_set_t *poll_set, int fd);

/**
 * Wait on a poll set for timeout ms, progressed indicating whether progress has
 * been made after that call returns. If timeout is 0, progress is performed
 * on all the registered polling callbacks and hg_poll_wait() exits as soon as
 * progress is made. If timeout is non 0, the system dependent polling function
 * call is entered and progress is performed on the list of file descriptors
 * for which an event has occurred.
 *
 * \param poll_set [IN]         pointer to poll set
 * \param timeout [IN]          timeout (in milliseconds)
 * \param progressed [OUT]      pointer to boolean indicating progress made
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int
hg_poll_wait(hg_poll_set_t *poll_set, unsigned int timeout,
    unsigned int max_events, struct hg_poll_event events[],
    unsigned int *actual_events);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_POLL_H */
