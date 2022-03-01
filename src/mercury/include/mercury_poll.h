/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
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

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

typedef struct hg_poll_set hg_poll_set_t;

typedef union hg_poll_data {
    void *           ptr;
    int              fd;
    hg_util_uint32_t u32;
    hg_util_uint64_t u64;
} hg_poll_data_t;

struct hg_poll_event {
    hg_util_uint32_t events; /* Poll events */
    hg_poll_data_t   data;   /* User data variable */
};

/*****************/
/* Public Macros */
/*****************/

/**
 * Polling events.
 */
#define HG_POLLIN   (1 << 0) /* There is data to read. */
#define HG_POLLOUT  (1 << 1) /* Writing now will not block. */
#define HG_POLLERR  (1 << 2) /* Error condition. */
#define HG_POLLHUP  (1 << 3) /* Hung up. */
#define HG_POLLINTR (1 << 4) /* Interrupted. */

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
HG_UTIL_PUBLIC hg_poll_set_t *hg_poll_create(void);

/**
 * Destroy a poll set.
 *
 * \param poll_set [IN/OUT]     pointer to poll set
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_poll_destroy(hg_poll_set_t *poll_set);

/**
 * Get a file descriptor from an existing poll set.
 *
 * \param poll_set [IN]         pointer to poll set
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_poll_get_fd(hg_poll_set_t *poll_set);

/**
 * Add file descriptor to poll set.
 *
 * \param poll_set [IN]         pointer to poll set
 * \param fd [IN]               file descriptor
 * \param event [IN]            pointer to event struct
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_poll_add(hg_poll_set_t *poll_set, int fd, struct hg_poll_event *event);

/**
 * Remove file descriptor from poll set.
 *
 * \param poll_set [IN]         pointer to poll set
 * \param fd [IN]               file descriptor
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_poll_remove(hg_poll_set_t *poll_set, int fd);

/**
 * Wait on a poll set for timeout ms, and return at most max_events.
 *
 * \param poll_set [IN]         pointer to poll set
 * \param timeout [IN]          timeout (in milliseconds)
 * \param max_events [IN]       max number of events
 * \param events [IN/OUT]       array of events to be returned
 * \param actual_events [OUT]   actual number of events returned
 *
 * \return Non-negative on success or negative on failure
 */
HG_UTIL_PUBLIC int hg_poll_wait(hg_poll_set_t *poll_set, unsigned int timeout, unsigned int max_events,
                                struct hg_poll_event events[], unsigned int *actual_events);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_POLL_H */
