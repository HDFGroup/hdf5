/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_request.h"
#include "mercury_thread_condition.h"
#include "mercury_thread_mutex.h"
#include "mercury_time.h"
#include "mercury_util_error.h"

#include <stdlib.h>

/****************/
/* Local Macros */
/****************/

/************************************/
/* Local Type and Struct Definition */
/************************************/

struct hg_request_class {
    hg_request_progress_func_t progress_func;
    hg_request_trigger_func_t  trigger_func;
    void *                     arg;
    hg_util_bool_t             progressing;
    hg_thread_mutex_t          progress_mutex;
    hg_thread_cond_t           progress_cond;
};

/********************/
/* Local Prototypes */
/********************/

/*******************/
/* Local Variables */
/*******************/

/*---------------------------------------------------------------------------*/
hg_request_class_t *
hg_request_init(hg_request_progress_func_t progress_func, hg_request_trigger_func_t trigger_func, void *arg)
{
    struct hg_request_class *hg_request_class = NULL;

    hg_request_class = (struct hg_request_class *)malloc(sizeof(struct hg_request_class));
    HG_UTIL_CHECK_ERROR_NORET(hg_request_class == NULL, done, "Could not allocate hg_request_class");

    hg_request_class->progress_func = progress_func;
    hg_request_class->trigger_func  = trigger_func;
    hg_request_class->arg           = arg;
    hg_request_class->progressing   = HG_UTIL_FALSE;
    hg_thread_mutex_init(&hg_request_class->progress_mutex);
    hg_thread_cond_init(&hg_request_class->progress_cond);

done:
    return hg_request_class;
}

/*---------------------------------------------------------------------------*/
void
hg_request_finalize(hg_request_class_t *request_class, void **arg)
{
    if (!request_class)
        return;

    if (arg)
        *arg = request_class->arg;
    hg_thread_mutex_destroy(&request_class->progress_mutex);
    hg_thread_cond_destroy(&request_class->progress_cond);
    free(request_class);
}

/*---------------------------------------------------------------------------*/
hg_request_t *
hg_request_create(hg_request_class_t *request_class)
{
    struct hg_request *hg_request = NULL;

    hg_request = (struct hg_request *)malloc(sizeof(struct hg_request));
    HG_UTIL_CHECK_ERROR_NORET(hg_request == NULL, done, "Could not allocate hg_request");

    hg_request->request_class = request_class;
    hg_request->data          = NULL;
    hg_atomic_init32(&hg_request->completed, HG_UTIL_FALSE);

done:
    return hg_request;
}

/*---------------------------------------------------------------------------*/
void
hg_request_destroy(hg_request_t *request)
{
    free(request);
}

/*---------------------------------------------------------------------------*/
int
hg_request_wait(hg_request_t *request, unsigned int timeout_ms, unsigned int *flag)
{
    hg_time_t       deadline, remaining = hg_time_from_ms(timeout_ms);
    hg_time_t       now       = hg_time_from_ms(0);
    hg_util_int32_t completed = HG_UTIL_FALSE;
    int             ret       = HG_UTIL_SUCCESS;

    if (timeout_ms != 0)
        hg_time_get_current_ms(&now);
    deadline = hg_time_add(now, remaining);

    do {
        unsigned int trigger_flag = 0;
        int          trigger_ret;

        do {
            trigger_ret = request->request_class->trigger_func(0, &trigger_flag, request->request_class->arg);
        } while ((trigger_ret == HG_UTIL_SUCCESS) && trigger_flag);

        if ((completed = hg_atomic_get32(&request->completed)) == HG_UTIL_TRUE)
            break;

        hg_thread_mutex_lock(&request->request_class->progress_mutex);
        if (request->request_class->progressing) {
            if (hg_thread_cond_timedwait(&request->request_class->progress_cond,
                                         &request->request_class->progress_mutex,
                                         hg_time_to_ms(remaining)) != HG_UTIL_SUCCESS) {
                /* Timeout occurred so leave */
                hg_thread_mutex_unlock(&request->request_class->progress_mutex);
                break;
            }
            /* Continue as request may have completed in the meantime */
            hg_thread_mutex_unlock(&request->request_class->progress_mutex);
            goto next;
        }
        request->request_class->progressing = HG_UTIL_TRUE;
        hg_thread_mutex_unlock(&request->request_class->progress_mutex);

        request->request_class->progress_func(hg_time_to_ms(remaining), request->request_class->arg);

        hg_thread_mutex_lock(&request->request_class->progress_mutex);
        request->request_class->progressing = HG_UTIL_FALSE;
        hg_thread_cond_broadcast(&request->request_class->progress_cond);
        hg_thread_mutex_unlock(&request->request_class->progress_mutex);

next:
        if (timeout_ms != 0)
            hg_time_get_current_ms(&now);
        remaining = hg_time_subtract(deadline, now);
    } while (hg_time_less(now, deadline));

    if (flag)
        *flag = (unsigned int)completed;

    return ret;
}
