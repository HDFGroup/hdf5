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
 * Purpose:    Tests event sets.
 */
#include "h5test.h"
#include "H5srcdir.h"

#define EVENT_SET_NUM_CONNECTOR_IDS 2

static const char *FILENAME[] = {"event_set_1", NULL};

static hid_t connector_ids_g[EVENT_SET_NUM_CONNECTOR_IDS];

herr_t               fake_wait_request_wait(void *req, uint64_t timeout, H5VL_request_status_t *status);
H5_ATTR_CONST herr_t fake_wait_request_free(void *req);

/* A VOL class struct that describes a VOL class with no
 * functionality, other than a wait that returns success.
 */
static const H5VL_class_t fake_wait_vol_g = {
    H5VL_VERSION,              /* VOL class struct version */
    ((H5VL_class_value_t)501), /* value        */
    "fake_wait",               /* name         */
    0,                         /* connector version */
    0,                         /* capability flags */
    NULL,                      /* initialize   */
    NULL,                      /* terminate    */
    {
        /* info_cls */
        (size_t)0, /* size    */
        NULL,      /* copy    */
        NULL,      /* compare */
        NULL,      /* free    */
        NULL,      /* to_str  */
        NULL,      /* from_str */
    },
    {
        /* wrap_cls */
        NULL, /* get_object   */
        NULL, /* get_wrap_ctx */
        NULL, /* wrap_object  */
        NULL, /* unwrap_object */
        NULL, /* free_wrap_ctx */
    },
    {
        /* attribute_cls */
        NULL, /* create       */
        NULL, /* open         */
        NULL, /* read         */
        NULL, /* write        */
        NULL, /* get          */
        NULL, /* specific     */
        NULL, /* optional     */
        NULL  /* close        */
    },
    {
        /* dataset_cls */
        NULL, /* create       */
        NULL, /* open         */
        NULL, /* read         */
        NULL, /* write        */
        NULL, /* get          */
        NULL, /* specific     */
        NULL, /* optional     */
        NULL  /* close        */
    },
    {
        /* datatype_cls */
        NULL, /* commit       */
        NULL, /* open         */
        NULL, /* get          */
        NULL, /* specific     */
        NULL, /* optional     */
        NULL  /* close        */
    },
    {
        /* file_cls */
        NULL, /* create       */
        NULL, /* open         */
        NULL, /* get          */
        NULL, /* specific     */
        NULL, /* optional     */
        NULL  /* close        */
    },
    {
        /* group_cls */
        NULL, /* create       */
        NULL, /* open         */
        NULL, /* get          */
        NULL, /* specific     */
        NULL, /* optional     */
        NULL  /* close        */
    },
    {
        /* link_cls */
        NULL, /* create       */
        NULL, /* copy         */
        NULL, /* move         */
        NULL, /* get          */
        NULL, /* specific     */
        NULL  /* optional     */
    },
    {
        /* object_cls */
        NULL, /* open         */
        NULL, /* copy         */
        NULL, /* get          */
        NULL, /* specific     */
        NULL  /* optional     */
    },
    {
        /* introspect_cls */
        NULL, /* get_conn_cls */
        NULL, /* get_cap_flags */
        NULL, /* opt_query    */
    },
    {
        /* request_cls */
        fake_wait_request_wait, /* wait         */
        NULL,                   /* notify       */
        NULL,                   /* cancel       */
        NULL,                   /* specific     */
        NULL,                   /* optional     */
        fake_wait_request_free  /* free         */
    },
    {
        /* blob_cls */
        NULL, /* put          */
        NULL, /* get          */
        NULL, /* specific     */
        NULL  /* optional     */
    },
    {
        /* token_cls */
        NULL, /* cmp              */
        NULL, /* to_str           */
        NULL  /* from_str         */
    },
    NULL /* optional     */
};

herr_t
fake_wait_request_wait(void H5_ATTR_UNUSED *req, uint64_t H5_ATTR_UNUSED timeout,
                       H5VL_request_status_t *status)
{
    /* Set status if requested */
    if (status)
        *status = H5VL_REQUEST_STATUS_SUCCEED;

    return 0;
} /* end H5_daos_req_wait() */

herr_t
fake_wait_request_free(void H5_ATTR_UNUSED *req)
{
    return 0;
} /* end fake_wait_request_free() */

/*-------------------------------------------------------------------------
 * Function:    test_es_create
 *
 * Purpose:     Tests creating event sets.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_es_create(void)
{
    hid_t    es_id;        /* Event set ID */
    size_t   count;        /* # of events in set */
    size_t   num_errs;     /* # of failed events in set */
    uint64_t num_ops;      /* # of events inserted into set */
    bool     err_occurred; /* Whether an error has occurred */

    TESTING("event set creation");

    /* Create an event set */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Query the # of events in empty event set */
    count = 0;
    if (H5ESget_count(es_id, &count) < 0)
        TEST_ERROR;
    if (count > 0)
        FAIL_PUTS_ERROR("should be empty event set");

    /* Check for errors */
    err_occurred = false;
    if (H5ESget_err_status(es_id, &err_occurred) < 0)
        TEST_ERROR;
    if (err_occurred)
        FAIL_PUTS_ERROR("should not be an error for the event set");

    /* Check errors count */
    num_errs = 0;
    if (H5ESget_err_count(es_id, &num_errs) < 0)
        TEST_ERROR;
    if (num_errs)
        FAIL_PUTS_ERROR("should not be any errors for the event set");

    /* Check errors count */
    num_ops = 0;
    if (H5ESget_op_counter(es_id, &num_ops) < 0)
        TEST_ERROR;
    if (num_ops)
        FAIL_PUTS_ERROR("should not be any operations for the event set yet");

    /* Close the event set */
    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5ESclose(es_id);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_es_none
 *
 * Purpose:     Tests for passing H5ES_NONE to H5ES routines
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_es_none(void)
{
    TESTING("event set H5ES_NONE");

    /* Wait */
    if (H5ESwait(H5ES_NONE, 0, NULL, NULL) < 0)
        TEST_ERROR;

    /* Cancel */
    if (H5EScancel(H5ES_NONE, NULL, NULL) < 0)
        TEST_ERROR;

    /* Get count */
    if (H5ESget_count(H5ES_NONE, NULL) < 0)
        TEST_ERROR;

    /* Get op counter */
    if (H5ESget_op_counter(H5ES_NONE, NULL) < 0)
        TEST_ERROR;

    /* Get error status */
    if (H5ESget_err_status(H5ES_NONE, NULL) < 0)
        TEST_ERROR;

    /* Get error count */
    if (H5ESget_err_count(H5ES_NONE, NULL) < 0)
        TEST_ERROR;

    /* Get error info */
    if (H5ESget_err_info(H5ES_NONE, 0, NULL, NULL) < 0)
        TEST_ERROR;

    /* Register insert function */
    if (H5ESregister_insert_func(H5ES_NONE, NULL, NULL) < 0)
        TEST_ERROR;

    /* Register complete function */
    if (H5ESregister_complete_func(H5ES_NONE, NULL, NULL) < 0)
        TEST_ERROR;

    /* Close */
    if (H5ESclose(H5ES_NONE) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_es_get_requests
 *
 * Purpose:     Tests getting requests from event set.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_es_get_requests(void)
{
    hid_t  es_id;            /* Event set ID */
    hid_t  connector_ids[2]; /* Connector IDs */
    void  *requests[2];      /* Requests */
    int    req_targets[2];   /* Dummy targets for void * requests */
    size_t count;            /* # of events in set */
    bool   op_failed;        /* Whether an operation failed (unused) */

    TESTING("event set get requests");

    /* Create an event set */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Get number of requests in event set */
    count = 3;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, NULL, NULL, 0, &count) < 0)
        TEST_ERROR;
    if (count != 0)
        TEST_ERROR;

    /* Get only connector IDs */
    count            = 3;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, connector_ids, NULL, 2, &count) < 0)
        TEST_ERROR;
    if (count != 0)
        TEST_ERROR;
    if (connector_ids[0] != H5I_INVALID_HID)
        TEST_ERROR;
    if (connector_ids[1] != H5I_INVALID_HID)
        TEST_ERROR;

    /* Get only requests */
    count       = 3;
    requests[0] = NULL;
    requests[1] = NULL;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, NULL, requests, 2, &count) < 0)
        TEST_ERROR;
    if (count != 0)
        TEST_ERROR;
    if (requests[0] != NULL)
        TEST_ERROR;
    if (requests[1] != NULL)
        TEST_ERROR;

    /* Get both */
    count            = 3;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    requests[0]      = NULL;
    requests[1]      = NULL;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, connector_ids, requests, 2, &count) < 0)
        TEST_ERROR;
    if (count != 0)
        TEST_ERROR;
    if (connector_ids[0] != H5I_INVALID_HID)
        TEST_ERROR;
    if (connector_ids[1] != H5I_INVALID_HID)
        TEST_ERROR;
    if (requests[0] != NULL)
        TEST_ERROR;
    if (requests[1] != NULL)
        TEST_ERROR;

    /* Insert event into event set */
    if (H5ESinsert_request(es_id, connector_ids_g[0], &req_targets[0]) < 0)
        TEST_ERROR;

    /* Get number of requests in event set */
    count = 0;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, NULL, NULL, 0, &count) < 0)
        TEST_ERROR;
    if (count != 1)
        TEST_ERROR;

    /* Get only connector IDs */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, connector_ids, NULL, 2, &count) < 0)
        TEST_ERROR;
    if (count != 1)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[0])
        TEST_ERROR;
    if (connector_ids[1] != H5I_INVALID_HID)
        TEST_ERROR;

    /* Get only requests */
    count       = 0;
    requests[0] = NULL;
    requests[1] = NULL;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, NULL, requests, 2, &count) < 0)
        TEST_ERROR;
    if (count != 1)
        TEST_ERROR;
    if (requests[0] != &req_targets[0])
        TEST_ERROR;
    if (requests[1] != NULL)
        TEST_ERROR;

    /* Get both */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    requests[0]      = NULL;
    requests[1]      = NULL;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, connector_ids, requests, 2, &count) < 0)
        TEST_ERROR;
    if (count != 1)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[0])
        TEST_ERROR;
    if (connector_ids[1] != H5I_INVALID_HID)
        TEST_ERROR;
    if (requests[0] != &req_targets[0])
        TEST_ERROR;
    if (requests[1] != NULL)
        TEST_ERROR;

    /* Insert second event into event set */
    if (H5ESinsert_request(es_id, connector_ids_g[1], &req_targets[1]) < 0)
        TEST_ERROR;

    /* Get number of requests in event set */
    count = 0;
    if (H5ESget_requests(es_id, H5_ITER_NATIVE, NULL, NULL, 0, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;

    /* Get only connector IDs */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    if (H5ESget_requests(es_id, H5_ITER_INC, connector_ids, NULL, 2, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[0])
        TEST_ERROR;
    if (connector_ids[1] != connector_ids_g[1])
        TEST_ERROR;

    /* Try with H5_ITER_DEC */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    if (H5ESget_requests(es_id, H5_ITER_DEC, connector_ids, NULL, 2, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[1])
        TEST_ERROR;
    if (connector_ids[1] != connector_ids_g[0])
        TEST_ERROR;

    /* Get only requests */
    count       = 0;
    requests[0] = NULL;
    requests[1] = NULL;
    if (H5ESget_requests(es_id, H5_ITER_INC, NULL, requests, 2, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (requests[0] != &req_targets[0])
        TEST_ERROR;
    if (requests[1] != &req_targets[1])
        TEST_ERROR;

    /* Try with H5_ITER_DEC */
    count       = 0;
    requests[0] = NULL;
    requests[1] = NULL;
    if (H5ESget_requests(es_id, H5_ITER_DEC, NULL, requests, 2, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (requests[0] != &req_targets[1])
        TEST_ERROR;
    if (requests[1] != &req_targets[0])
        TEST_ERROR;

    /* Get both */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    requests[0]      = NULL;
    requests[1]      = NULL;
    if (H5ESget_requests(es_id, H5_ITER_INC, connector_ids, requests, 2, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[0])
        TEST_ERROR;
    if (connector_ids[1] != connector_ids_g[1])
        TEST_ERROR;
    if (requests[0] != &req_targets[0])
        TEST_ERROR;
    if (requests[1] != &req_targets[1])
        TEST_ERROR;

    /* Try with H5_ITER_DEC */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    requests[0]      = NULL;
    requests[1]      = NULL;
    if (H5ESget_requests(es_id, H5_ITER_DEC, connector_ids, requests, 2, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[1])
        TEST_ERROR;
    if (connector_ids[1] != connector_ids_g[0])
        TEST_ERROR;
    if (requests[0] != &req_targets[1])
        TEST_ERROR;
    if (requests[1] != &req_targets[0])
        TEST_ERROR;

    /* Get only first connector ID */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    if (H5ESget_requests(es_id, H5_ITER_INC, connector_ids, NULL, 1, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[0])
        TEST_ERROR;
    if (connector_ids[1] != H5I_INVALID_HID)
        TEST_ERROR;

    /* Try with H5_ITER_DEC */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    if (H5ESget_requests(es_id, H5_ITER_DEC, connector_ids, NULL, 1, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[1])
        TEST_ERROR;
    if (connector_ids[1] != H5I_INVALID_HID)
        TEST_ERROR;

    /* Get only first request */
    count       = 0;
    requests[0] = NULL;
    requests[1] = NULL;
    if (H5ESget_requests(es_id, H5_ITER_INC, NULL, requests, 1, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (requests[0] != &req_targets[0])
        TEST_ERROR;
    if (requests[1] != NULL)
        TEST_ERROR;

    /* Try with H5_ITER_DEC */
    count       = 0;
    requests[0] = NULL;
    requests[1] = NULL;
    if (H5ESget_requests(es_id, H5_ITER_DEC, NULL, requests, 1, &count) < 0)
        TEST_ERROR;
    if (count != 2)
        TEST_ERROR;
    if (requests[0] != &req_targets[1])
        TEST_ERROR;
    if (requests[1] != NULL)
        TEST_ERROR;

    /* Get only first of both */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    requests[0]      = NULL;
    requests[1]      = NULL;
    if (H5ESget_requests(es_id, H5_ITER_INC, connector_ids, requests, 1, &count) < 0)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[0])
        TEST_ERROR;
    if (connector_ids[1] != H5I_INVALID_HID)
        TEST_ERROR;
    if (requests[0] != &req_targets[0])
        TEST_ERROR;
    if (requests[1] != NULL)
        TEST_ERROR;

    /* Try with H5_ITER_DEC */
    count            = 0;
    connector_ids[0] = H5I_INVALID_HID;
    connector_ids[1] = H5I_INVALID_HID;
    requests[0]      = NULL;
    requests[1]      = NULL;
    if (H5ESget_requests(es_id, H5_ITER_DEC, connector_ids, requests, 1, &count) < 0)
        TEST_ERROR;
    if (connector_ids[0] != connector_ids_g[1])
        TEST_ERROR;
    if (connector_ids[1] != H5I_INVALID_HID)
        TEST_ERROR;
    if (requests[0] != &req_targets[1])
        TEST_ERROR;
    if (requests[1] != NULL)
        TEST_ERROR;

    /* Close the event set */
    if (H5ESwait(es_id, 10000000, &count, &op_failed) < 0)
        TEST_ERROR;
    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5ESclose(es_id);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests event sets
 *
 * Return:      Success: EXIT_SUCCESS
 *              Failure: EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fapl_id = H5I_INVALID_HID; /* File access property list */
    int   i;                         /* Local index variable */
    int   nerrors = 0;               /* Error count */

    /* Setup */
    h5_reset();
    fapl_id = h5_fileaccess();

    /* Register dummy connector IDs */
    for (i = 0; i < EVENT_SET_NUM_CONNECTOR_IDS; i++)
        if ((connector_ids_g[i] = H5VLregister_connector(&fake_wait_vol_g, H5P_DEFAULT)) < 0)
            TEST_ERROR;

    /* Tests */
    nerrors += test_es_create();
    nerrors += test_es_none();
    nerrors += test_es_get_requests();

    /* Unregister dummy connectors */
    for (i = 0; i < EVENT_SET_NUM_CONNECTOR_IDS; i++)
        if (H5VLunregister_connector(connector_ids_g[i]) < 0)
            TEST_ERROR;

    /* Cleanup */
    h5_cleanup(FILENAME, fapl_id);

    /* Check for any errors */
    if (nerrors)
        goto error;

    /* Report status */
    puts("All event set tests passed.");

    exit(EXIT_SUCCESS);

error:
    puts("***** EVENT SET TESTS FAILED *****");
    exit(EXIT_FAILURE);
} /* end main() */
