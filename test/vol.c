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
 * Purpose: Tests the virtual object layer (H5VL)
 *
 *          This is a minimal test to ensure VOL usage (setting a VOL, etc.)
 *          works as expected. Actual VOL functionality is tested using
 *          other mechanisms.
 */

/* Headers needed */
#include "h5test.h"
#include "H5Iprivate.h" /* IDs                                  */
#define H5T_FRIEND      /* Suppress error about including H5Tpkg    */
#include "H5Tpkg.h"     /* Datatypes                            */
#define H5VL_FRIEND     /* Suppress error about including H5VLpkg    */
#define H5VL_TESTING
#include "H5VLpkg.h" /* Virtual Object Layer                 */

/* Filename */
static const char *FILENAME[] = {"vol_test_file", NULL};

#define NATIVE_VOL_TEST_GROUP_NAME     "test_group"
#define NATIVE_VOL_TEST_DATASET_NAME   "test_dataset"
#define NATIVE_VOL_TEST_ATTRIBUTE_NAME "test_dataset"
#define NATIVE_VOL_TEST_HARD_LINK_NAME "test_hard_link"
#define NATIVE_VOL_TEST_SOFT_LINK_NAME "test_soft_link"
#define NATIVE_VOL_TEST_MOVE_LINK_NAME "test_move_link"
#define NATIVE_VOL_TEST_COPY_LINK_NAME "test_copy_link"
#define NATIVE_VOL_TEST_DATATYPE_NAME  "test_datatype"

#define N_ELEMENTS 10
#define NAME_LEN   512

/* A VOL class struct to verify registering optional operations */
static int    reg_opt_curr_op_val;
static herr_t reg_opt_op_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id, void **req);
static herr_t reg_opt_link_optional(void *obj, const H5VL_loc_params_t *loc_params,
                                    H5VL_optional_args_t *args, hid_t dxpl_id, void **req);
static herr_t reg_opt_datatype_get(void *obj, H5VL_datatype_get_args_t *args, hid_t dxpl_id, void **req);

#define REG_OPT_VOL_NAME  "reg_opt"
#define REG_OPT_VOL_VALUE ((H5VL_class_value_t)502)
static const H5VL_class_t reg_opt_vol_g = {
    H5VL_VERSION,       /* VOL class struct version */
    REG_OPT_VOL_VALUE,  /* value        */
    REG_OPT_VOL_NAME,   /* name         */
    0,                  /* version      */
    H5VL_CAP_FLAG_NONE, /* capability flags */
    NULL,               /* initialize   */
    NULL,               /* terminate    */
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
        NULL,                /* create       */
        NULL,                /* open         */
        NULL,                /* read         */
        NULL,                /* write        */
        NULL,                /* get          */
        NULL,                /* specific     */
        reg_opt_op_optional, /* optional     */
        NULL                 /* close        */
    },
    {
        /* dataset_cls */
        NULL,                /* create       */
        NULL,                /* open         */
        NULL,                /* read         */
        NULL,                /* write        */
        NULL,                /* get          */
        NULL,                /* specific     */
        reg_opt_op_optional, /* optional     */
        NULL                 /* close        */
    },
    {
        /* datatype_cls */
        NULL,                 /* commit       */
        NULL,                 /* open         */
        reg_opt_datatype_get, /* get          */
        NULL,                 /* specific     */
        reg_opt_op_optional,  /* optional     */
        NULL                  /* close        */
    },
    {
        /* file_cls */
        NULL,                /* create       */
        NULL,                /* open         */
        NULL,                /* get          */
        NULL,                /* specific     */
        reg_opt_op_optional, /* optional     */
        NULL                 /* close        */
    },
    {
        /* group_cls */
        NULL,                /* create       */
        NULL,                /* open         */
        NULL,                /* get          */
        NULL,                /* specific     */
        reg_opt_op_optional, /* optional     */
        NULL                 /* close        */
    },
    {
        /* link_cls */
        NULL,                 /* create       */
        NULL,                 /* copy         */
        NULL,                 /* move         */
        NULL,                 /* get          */
        NULL,                 /* specific     */
        reg_opt_link_optional /* optional     */
    },
    {
        /* object_cls */
        NULL,                 /* open         */
        NULL,                 /* copy         */
        NULL,                 /* get          */
        NULL,                 /* specific     */
        reg_opt_link_optional /* optional     */
    },
    {
        /* introspect_cls */
        NULL, /* get_conn_cls */
        NULL, /* get_cap_flags */
        NULL, /* opt_query    */
    },
    {
        /* request_cls */
        NULL, /* wait         */
        NULL, /* notify       */
        NULL, /* cancel       */
        NULL, /* specific     */
        NULL, /* optional     */
        NULL  /* free         */
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

static herr_t fake_get_cap_flags(const void *info, uint64_t *cap_flags);
static herr_t fake_vol_info_to_str(const void *info, char **str);
static herr_t fake_vol_str_to_info(const char *str, void **info);
static herr_t fake_vol_free_info(void *info);

#define FAKE_VOL_NAME  "fake"
#define FAKE_VOL_VALUE ((H5VL_class_value_t)501)
#define H5VL_FAKE_CAP_FLAGS                                                                                  \
    (H5VL_CAP_FLAG_DATASET_BASIC | H5VL_CAP_FLAG_GROUP_BASIC | H5VL_CAP_FLAG_FILE_BASIC)

/* A VOL class struct that describes a VOL class with no
 * functionality.
 */
static const H5VL_class_t fake_vol_g = {
    H5VL_VERSION,        /* VOL class struct version */
    FAKE_VOL_VALUE,      /* value        */
    FAKE_VOL_NAME,       /* name         */
    0,                   /* connector version */
    H5VL_FAKE_CAP_FLAGS, /* capability flags */
    NULL,                /* initialize   */
    NULL,                /* terminate    */
    {
        /* info_cls */
        (size_t)0,            /* size    */
        NULL,                 /* copy    */
        NULL,                 /* compare */
        fake_vol_free_info,   /* free    */
        fake_vol_info_to_str, /* to_str  */
        fake_vol_str_to_info, /* from_str */
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
        NULL,                 /* commit       */
        NULL,                 /* open         */
        reg_opt_datatype_get, /* get          */
        NULL,                 /* specific     */
        NULL,                 /* optional     */
        NULL                  /* close        */
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
        NULL,               /* get_conn_cls */
        fake_get_cap_flags, /* get_cap_flags */
        NULL,               /* opt_query    */
    },
    {
        /* request_cls */
        NULL, /* wait         */
        NULL, /* notify       */
        NULL, /* cancel       */
        NULL, /* specific     */
        NULL, /* optional     */
        NULL  /* free         */
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

static herr_t fake_async_get_cap_flags(const void *info, uint64_t *cap_flags);

#define FAKE_ASYNC_VOL_NAME  "fake_async"
#define FAKE_ASYNC_VOL_VALUE ((H5VL_class_value_t)503)

/* A VOL class struct that describes a VOL class with no
 * functionality except to set the async capability flag.
 */
static const H5VL_class_t fake_async_vol_g = {
    H5VL_VERSION,         /* VOL class struct version */
    FAKE_ASYNC_VOL_VALUE, /* value        */
    FAKE_ASYNC_VOL_NAME,  /* name         */
    0,                    /* connector version */
    H5VL_CAP_FLAG_ASYNC,  /* capability flags */
    NULL,                 /* initialize   */
    NULL,                 /* terminate    */
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
        NULL,                     /* get_conn_cls */
        fake_async_get_cap_flags, /* get_cap_flags */
        NULL,                     /* opt_query    */
    },
    {
        /* request_cls */
        NULL, /* wait         */
        NULL, /* notify       */
        NULL, /* cancel       */
        NULL, /* specific     */
        NULL, /* optional     */
        NULL  /* free         */
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

/*-------------------------------------------------------------------------
 * Function:    reg_opt_op_optional_verify
 *
 * Purpose:     Common verification routine for dynamic optional operations
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
reg_opt_op_optional_verify(void *obj, H5VL_optional_args_t *args)
{
    int *o = (int *)obj;
    int *op_args;

    /* Check for receiving correct operation value */
    if (args->op_type != reg_opt_curr_op_val)
        return -1;

    /* Check that the object is correct */
    if ((-1) != *o)
        return -1;

    /* Update the object, with the operation value */
    *o = args->op_type;

    /* Check that the argument is correct */
    op_args = args->args;
    if (NULL == op_args)
        return -1;
    if ((-1) != *op_args)
        return -1;

    /* Update the argument return parameter */
    *op_args = args->op_type;

    return 0;
} /* end reg_opt_op_optional_verify() */

/*-------------------------------------------------------------------------
 * Function:    reg_opt_op_optional
 *
 * Purpose:     Common callback to perform a connector-specific operation
 *              on an object
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
reg_opt_op_optional(void *obj, H5VL_optional_args_t *args, hid_t H5_ATTR_UNUSED dxpl_id,
                    void H5_ATTR_UNUSED **req)
{
    /* Invoke the common value verification routine */
    return reg_opt_op_optional_verify(obj, args);
} /* end reg_opt_op_optional() */

/*-------------------------------------------------------------------------
 * Function:    reg_opt_link_optional
 *
 * Purpose:     Callback to perform a connector-specific operation
 *              on a link
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
reg_opt_link_optional(void *obj, const H5VL_loc_params_t *loc_params, H5VL_optional_args_t *args,
                      hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    /* Check for receiving correct loc_params info */
    if (loc_params->type != H5VL_OBJECT_BY_NAME)
        return -1;
    if (loc_params->obj_type != H5I_GROUP)
        return -1;
    if (strcmp(loc_params->loc_data.loc_by_name.name, ".") != 0)
        return -1;
    if (loc_params->loc_data.loc_by_name.lapl_id != H5P_LINK_ACCESS_DEFAULT)
        return -1;

    /* Invoke the common value verification routine */
    return reg_opt_op_optional_verify(obj, args);
} /* end reg_opt_link_optional() */

/*-------------------------------------------------------------------------
 * Function:    reg_opt_datatype_get
 *
 * Purpose:     Handles the datatype get callback
 *
 * Note:        This is _strictly_ a testing fixture to support the
 *              exercise_reg_opt_oper() testing routine.  It fakes just
 *              enough of the named datatype VOL callback for the
 *              H5VL_register_using_vol_id() call in that test routine to
 *              succeed.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
reg_opt_datatype_get(void H5_ATTR_UNUSED *obj, H5VL_datatype_get_args_t *args, hid_t H5_ATTR_UNUSED dxpl_id,
                     void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED; /* Return value */

    if (H5VL_DATATYPE_GET_BINARY_SIZE == args->op_type) {
        if (H5Tencode(H5T_NATIVE_INT, NULL, args->args.get_binary_size.size) < 0)
            ret_value = FAIL;
    } /* end if */
    else if (H5VL_DATATYPE_GET_BINARY == args->op_type) {
        if (H5Tencode(H5T_NATIVE_INT, args->args.get_binary.buf, &args->args.get_binary.buf_size) < 0)
            ret_value = FAIL;
    } /* end if */
    else
        ret_value = FAIL;

    return ret_value;
} /* end reg_opt_datatype_get() */

/*-------------------------------------------------------------------------
 * Function:    fake_vol_info_to_str
 *
 * Purpose:     Convert the fake VOL info to a string
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
fake_vol_info_to_str(const void *info, char **str)
{
    const int    val       = *(const int *)info;
    const size_t str_size  = 16; /* The size of the string */
    herr_t       ret_value = SUCCEED;

    /* Verify the info is correct before continuing */
    if (val != INT_MAX) {
        printf("The value of info (%d) is incorrect\n", val);
        return FAIL;
    }

    /* Allocate the string long enough for the info */
    if (NULL == (*str = (char *)calloc(1, str_size)))
        return FAIL;

    snprintf(*str, str_size, "%d", val);

    return ret_value;
} /* end fake_vol_info_to_str() */

/*-------------------------------------------------------------------------
 * Function:    fake_vol_str_to_info
 *
 * Purpose:     Convert a string to a VOL info
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
fake_vol_str_to_info(const char *str, void **info /*out*/)
{
    herr_t ret_value = SUCCEED; /* Return value */

    *((int **)info) = (int *)malloc(sizeof(int));

    **((int **)info) = atoi(str);

    return ret_value;
} /* end fake_vol_str_to_info() */

/*-------------------------------------------------------------------------
 * Function:    fake_vol_free_info
 *
 * Purpose:     Free the memory of a VOL info
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
fake_vol_free_info(void *info)
{
    herr_t ret_value = SUCCEED; /* Return value */

    if (info)
        free(info);

    return ret_value;
} /* end fake_vol_free_info() */

/*-------------------------------------------------------------------------
 * Function:    fake_get_cap_flags
 *
 * Purpose:     Return the capability flags for the 'fake' connector
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
fake_get_cap_flags(const void H5_ATTR_UNUSED *info, uint64_t *cap_flags)
{
    *cap_flags = fake_vol_g.cap_flags;

    return SUCCEED;
} /* end fake_get_cap_flags() */

/*-------------------------------------------------------------------------
 * Function:    fake_async_get_cap_flags
 *
 * Purpose:     Return the capability flags for the 'fake async' connector
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
fake_async_get_cap_flags(const void H5_ATTR_UNUSED *info, uint64_t *cap_flags)
{
    *cap_flags = fake_async_vol_g.cap_flags;

    return SUCCEED;
} /* end fake_async_get_cap_flags() */

/*-------------------------------------------------------------------------
 * Function:    test_vol_registration()
 *
 * Purpose:     Tests if we can load, register, and close a simple
 *              VOL connector.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_vol_registration(void)
{
    hid_t         native_id          = H5I_INVALID_HID;
    hid_t         lapl_id            = H5I_INVALID_HID;
    hid_t         vipl_id            = H5I_INVALID_HID;
    herr_t        ret                = SUCCEED;
    htri_t        is_registered      = FAIL;
    hid_t         vol_id             = H5I_INVALID_HID;
    hid_t         vol_id2            = H5I_INVALID_HID;
    H5VL_class_t *bad_fake_vol_class = NULL;

    TESTING("VOL registration");

    /* The test/fake VOL connector should not be registered at the start of the test */
    if ((is_registered = H5VLis_connector_registered_by_name(FAKE_VOL_NAME)) < 0)
        TEST_ERROR;
    if (is_registered > 0)
        FAIL_PUTS_ERROR("VOL connector is inappropriately registered");
    if ((is_registered = H5VLis_connector_registered_by_value(FAKE_VOL_VALUE)) < 0)
        TEST_ERROR;
    if (is_registered > 0)
        FAIL_PUTS_ERROR("VOL connector is inappropriately registered");

    /* Test registering a connector with an incorrect property list (SHOULD FAIL) */
    if ((lapl_id = H5Pcreate(H5P_LINK_ACCESS)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        vol_id = H5VLregister_connector(&fake_vol_g, lapl_id);
    }
    H5E_END_TRY
    if (H5I_INVALID_HID != vol_id)
        FAIL_PUTS_ERROR("should not be able to register a connector with an incorrect property list");
    if (H5Pclose(lapl_id) < 0)
        TEST_ERROR;

    /* Test registering a VOL connector with an incompatible version # */
    if (NULL == (bad_fake_vol_class = malloc(sizeof(H5VL_class_t))))
        TEST_ERROR;
    memcpy(bad_fake_vol_class, &fake_vol_g, sizeof(H5VL_class_t));
    bad_fake_vol_class->version = H5VL_VERSION + 1;
    H5E_BEGIN_TRY
    {
        vol_id = H5VLregister_connector(bad_fake_vol_class, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (H5I_INVALID_HID != vol_id)
        FAIL_PUTS_ERROR("should not be able to register a connector with an incompatible version #");
    free(bad_fake_vol_class);
    bad_fake_vol_class = NULL;

    /* Load a VOL interface
     * The vipl_id does nothing without a VOL that needs it, but we do need to
     * test creating a property list of that class and passing it along as a
     * smoke check.
     */
    if ((vipl_id = H5Pcreate(H5P_VOL_INITIALIZE)) < 0)
        TEST_ERROR;
    if ((vol_id = H5VLregister_connector(&fake_vol_g, vipl_id)) < 0)
        TEST_ERROR;
    if (H5Pclose(vipl_id) < 0)
        TEST_ERROR;

    /* The test/fake VOL connector should be registered now */
    if ((is_registered = H5VLis_connector_registered_by_name(FAKE_VOL_NAME)) < 0)
        TEST_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("VOL connector is un-registered");
    if ((is_registered = H5VLis_connector_registered_by_value(FAKE_VOL_VALUE)) < 0)
        TEST_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("VOL connector is un-registered");

    /* Re-register a VOL connector */
    if ((vol_id2 = H5VLregister_connector(&fake_vol_g, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* The test/fake VOL connector should still be registered now */
    if ((is_registered = H5VLis_connector_registered_by_name(FAKE_VOL_NAME)) < 0)
        TEST_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("VOL connector is un-registered");
    if ((is_registered = H5VLis_connector_registered_by_value(FAKE_VOL_VALUE)) < 0)
        TEST_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("VOL connector is un-registered");

    /* Unregister the second test/fake VOL ID */
    if (H5VLunregister_connector(vol_id2) < 0)
        TEST_ERROR;

    /* The test/fake VOL connector should still be registered now */
    if ((is_registered = H5VLis_connector_registered_by_name(FAKE_VOL_NAME)) < 0)
        TEST_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("VOL connector is un-registered");
    if ((is_registered = H5VLis_connector_registered_by_value(FAKE_VOL_VALUE)) < 0)
        TEST_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("VOL connector is un-registered");

    /* Unregister the original test/fake VOL ID */
    if (H5VLunregister_connector(vol_id) < 0)
        TEST_ERROR;

    /* Try to unregister the native VOL connector (should fail) */
    if (H5I_INVALID_HID == (native_id = H5VLget_connector_id_by_name(H5VL_NATIVE_NAME)))
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        ret = H5VLunregister_connector(native_id);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to unregister the native VOL connector");

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5VLunregister_connector(vol_id);
        H5Pclose(lapl_id);
        H5Pclose(vipl_id);
    }
    H5E_END_TRY

    if (bad_fake_vol_class)
        free(bad_fake_vol_class);

    return FAIL;
} /* end test_vol_registration() */

/*-------------------------------------------------------------------------
 * Function:    test_native_vol_init()
 *
 * Purpose:     Tests if the native VOL connector gets initialized.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_native_vol_init(void)
{
    htri_t is_registered;

    TESTING("Native VOL connector initialization");

    /* The native VOL connector should always be registered */
    if ((is_registered = H5VLis_connector_registered_by_name(H5VL_NATIVE_NAME)) < 0)
        TEST_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("native VOL connector is un-registered");

    if ((is_registered = H5VLis_connector_registered_by_value(H5VL_NATIVE_VALUE)) < 0)
        TEST_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("native VOL connector is un-registered");

    PASSED();
    return SUCCEED;

error:
    return FAIL;

} /* end test_native_vol_init() */

/*-------------------------------------------------------------------------
 * Function:    test_basic_file_operation()
 *
 * Purpose:     Uses the native VOL connector to test basic VOL file operations
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_file_operation(const char *driver_name)
{
    hid_t fid        = H5I_INVALID_HID;
    hid_t fid_reopen = H5I_INVALID_HID;
    hid_t fapl_id    = H5I_INVALID_HID;
    hid_t fapl_id2   = H5I_INVALID_HID;
    hid_t fcpl_id    = H5I_INVALID_HID;

    htri_t      use_locking_env     = FAIL;
    htri_t      ignore_disabled_env = FAIL;
    char        filename[1024];
    ssize_t     obj_count;
    hid_t       obj_id_list[1];
    hsize_t     file_size;
    unsigned    intent;
    void       *os_file_handle = NULL;
    H5F_info2_t finfo;
    char        name[32];

    TESTING("Basic VOL file operations");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    /* Set the file close degree to a non-default value, to make the H5Pequal
     *  work out.  This is kinda odd, but the library's current behavior with
     *  a default value is to return the value chosen (H5F_CLOSE_SEMI) instead
     *  of the default value (H5F_CLOSE_DEFAULT) from the property and then
     *  the H5Pequal doesn't detect that the property lists are the same.  Since
     *  this is the documented behavior for file close degree for many years,
     *  I'm not fighting it, just getting the testing to verify that the VOL
     *  connector property is returned correctly.  -QAK, 2018/11/17
     */
    if (H5Pset_fclose_degree(fapl_id, H5F_CLOSE_SEMI) < 0)
        TEST_ERROR;
    if (H5Pset_metadata_read_attempts(fapl_id, 9) < 0)
        TEST_ERROR;

    /* Similar to the above, make sure the FAPL has an appropriate file locking
     * setting if the HDF5_USE_FILE_LOCKING environment variable was set so that
     * the H5Pequal call will work correctly.
     */
    h5_check_file_locking_env_var(&use_locking_env, &ignore_disabled_env);
    if (use_locking_env != FAIL) {
        hbool_t default_use_locking           = true;
        hbool_t default_ignore_disabled_locks = true;

        if (H5Pget_file_locking(H5P_DEFAULT, &default_use_locking, &default_ignore_disabled_locks) < 0)
            TEST_ERROR;

        if (H5Pset_file_locking(fapl_id, (bool)use_locking_env,
                                (ignore_disabled_env == FAIL) ? default_ignore_disabled_locks
                                                              : (bool)ignore_disabled_env) < 0)
            TEST_ERROR;
    }

    /* H5Fcreate */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /* H5Fget_obj_count */
    if ((obj_count = H5Fget_obj_count(fid, H5F_OBJ_FILE)) < 0)
        TEST_ERROR;
    if ((obj_count = H5Fget_obj_count(fid, H5F_OBJ_ALL)) < 0)
        TEST_ERROR;
    if ((obj_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_DATASET)) < 0)
        TEST_ERROR;

    /* H5Fget_obj_ids */
    if ((obj_count = H5Fget_obj_ids(fid, H5F_OBJ_ALL, 2, obj_id_list)) < 0)
        TEST_ERROR;
    if ((obj_count = H5Fget_obj_ids((hid_t)H5F_OBJ_ALL, H5F_OBJ_DATASET, 2, obj_id_list)) < 0)
        TEST_ERROR;

    /* Can't compare VFD properties for several VFDs */
    if ((bool)(strcmp(driver_name, "split") != 0 && strcmp(driver_name, "multi") != 0 &&
               strcmp(driver_name, "family") != 0 && strcmp(driver_name, "direct") != 0 &&
               strcmp(driver_name, "core") != 0 && strcmp(driver_name, "core_paged") != 0 &&
               strcmp(driver_name, "mpio") != 0 && strcmp(driver_name, "splitter") != 0)) {
        /* H5Fget_access_plist */
        if ((fapl_id2 = H5Fget_access_plist(fid)) < 0)
            TEST_ERROR;
        if (H5Pequal(fapl_id, fapl_id2) != true)
            TEST_ERROR;
        if (H5Pclose(fapl_id2) < 0)
            TEST_ERROR;
    } /* end if */

    /* H5Fget_create_plist */
    if ((fcpl_id = H5Fget_create_plist(fid)) < 0)
        TEST_ERROR;
    if (H5Pclose(fcpl_id) < 0)
        TEST_ERROR;

    /* H5Fget_filesize */
    if (H5Fget_filesize(fid, &file_size) < 0)
        TEST_ERROR;

    /* Can't retrieve VFD handle for split / multi / family VFDs */
    if ((bool)(strcmp(driver_name, "split") != 0 && strcmp(driver_name, "multi") != 0 &&
               strcmp(driver_name, "family") != 0)) {
        /* H5Fget_vfd_handle */
        if (H5Fget_vfd_handle(fid, H5P_DEFAULT, &os_file_handle) < 0)
            TEST_ERROR;
    } /* end if */

    /* H5Fget_intent */
    if (H5Fget_intent(fid, &intent) < 0)
        TEST_ERROR;

    /* H5Fget_info2 */
    if (H5Fget_info2(fid, &finfo) < 0)
        TEST_ERROR;

    /* H5Fget_name */
    if (H5Fget_name(fid, name, 32) < 0)
        TEST_ERROR;

    /* H5Fclear_elink_file_cache */
    if (H5Fclear_elink_file_cache(fid) < 0)
        TEST_ERROR;

    /* H5Fflush */
    if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        TEST_ERROR;

    /* H5Fclose */
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* H5Fis_accessible */
    if (H5Fis_accessible(filename, fapl_id) < 0)
        TEST_ERROR;

    /* H5Fopen */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Can't compare VFD properties for several VFDs */
    if ((bool)(strcmp(driver_name, "split") != 0 && strcmp(driver_name, "multi") != 0 &&
               strcmp(driver_name, "family") != 0 && strcmp(driver_name, "direct") != 0 &&
               strcmp(driver_name, "core") != 0 && strcmp(driver_name, "core_paged") != 0 &&
               strcmp(driver_name, "mpio") != 0 && strcmp(driver_name, "splitter") != 0)) {
        /* H5Fget_access_plist */
        if ((fapl_id2 = H5Fget_access_plist(fid)) < 0)
            TEST_ERROR;
        if (H5Pequal(fapl_id, fapl_id2) != true)
            TEST_ERROR;
        if (H5Pclose(fapl_id2) < 0)
            TEST_ERROR;
    } /* end if */

    if ((fid_reopen = H5Freopen(fid)) < 0)
        TEST_ERROR;

    /* Can't compare VFD properties for several VFDs */
    if ((bool)(strcmp(driver_name, "split") != 0 && strcmp(driver_name, "multi") != 0 &&
               strcmp(driver_name, "family") != 0 && strcmp(driver_name, "direct") != 0 &&
               strcmp(driver_name, "core") != 0 && strcmp(driver_name, "core_paged") != 0 &&
               strcmp(driver_name, "mpio") != 0 && strcmp(driver_name, "splitter") != 0)) {
        /* H5Fget_access_plist */
        if ((fapl_id2 = H5Fget_access_plist(fid_reopen)) < 0)
            TEST_ERROR;
        if (H5Pequal(fapl_id, fapl_id2) != true)
            TEST_ERROR;
        if (H5Pclose(fapl_id2) < 0)
            TEST_ERROR;
    } /* end if */

    if (H5Fclose(fid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid_reopen) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    /* H5Pclose */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Fclose(fid_reopen);
        H5Pclose(fapl_id);
        H5Pclose(fapl_id2);
        H5Pclose(fcpl_id);
    }
    H5E_END_TRY

    return FAIL;

} /* end test_basic_file_operation() */

/*-------------------------------------------------------------------------
 * Function:    test_basic_group_operation()
 *
 * Purpose:     Uses the native VOL connector to test basic VOL group operations
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_group_operation(void)
{
    hid_t      fid     = H5I_INVALID_HID;
    hid_t      fapl_id = H5I_INVALID_HID;
    hid_t      gid     = H5I_INVALID_HID;
    hid_t      gid_a   = H5I_INVALID_HID;
    hid_t      gcpl_id = H5I_INVALID_HID;
    char       filename[1024];
    H5G_info_t info;
    bool       driver_is_parallel;

    TESTING("Basic VOL group operations");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /* H5Gcreate */
    if ((gid = H5Gcreate2(fid, NATIVE_VOL_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Gget_create_plist */
    if ((gcpl_id = H5Gget_create_plist(gid)) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
        TEST_ERROR;

    /* H5Gget_info */
    if (H5Gget_info(gid, &info) < 0)
        TEST_ERROR;
    if (H5Gget_info(fid, &info) < 0)
        TEST_ERROR;

    /* H5Gget_info_by_name */
    if (H5Gget_info_by_name(fid, NATIVE_VOL_TEST_GROUP_NAME, &info, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* H5Gget_info_by_idx */
    if (H5Gget_info_by_idx(fid, "/", H5_INDEX_NAME, H5_ITER_NATIVE, 0, &info, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* H5Gflush - skip for parallel file drivers as flush calls cause assertions in the library */
    if (h5_using_parallel_driver(fapl_id, &driver_is_parallel) < 0)
        TEST_ERROR;
    if (!driver_is_parallel)
        if (H5Gflush(gid) < 0)
            TEST_ERROR;

    /* H5Gclose */
    if (H5Gclose(gid) < 0)
        TEST_ERROR;

    /* H5Gopen */
    if ((gid = H5Gopen2(fid, NATIVE_VOL_TEST_GROUP_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Gcreate_anon */
    if ((gid_a = H5Gcreate_anon(fid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Grefresh */
    if (H5Grefresh(gid) < 0)
        TEST_ERROR;

    if (H5Gclose(gid) < 0)
        TEST_ERROR;
    if (H5Gclose(gid_a) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    /* H5Pclose */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Gclose(gid);
        H5Pclose(fapl_id);
        H5Pclose(gcpl_id);
    }
    H5E_END_TRY

    return FAIL;

} /* end test_basic_group_operation() */

/*-------------------------------------------------------------------------
 * Function:    test_basic_dataset_operation()
 *
 * Purpose:     Uses the native VOL connector to test basic VOL dataset operations
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_dataset_operation(void)
{
    hid_t fid     = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;
    hid_t dcpl_id = H5I_INVALID_HID;
    hid_t dapl_id = H5I_INVALID_HID;
    hid_t did     = H5I_INVALID_HID;
    hid_t did_a   = H5I_INVALID_HID;
    hid_t sid     = H5I_INVALID_HID;
    hid_t tid     = H5I_INVALID_HID;

    char filename[1024];

    hsize_t curr_dims = 0;
    hsize_t max_dims  = H5S_UNLIMITED;

    hsize_t            storage_size;
    haddr_t            offset;
    H5D_space_status_t status;

    bool driver_is_parallel;

    int in_buf[N_ELEMENTS];
    int out_buf[N_ELEMENTS];

    int i;

    TESTING("Basic VOL dataset operations");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;
    for (i = 0; i < N_ELEMENTS; i++) {
        in_buf[i]  = i;
        out_buf[i] = 0;
    }

    /* H5Dcreate */
    curr_dims = 0;
    if ((sid = H5Screate_simple(1, &curr_dims, &max_dims)) < 0)
        TEST_ERROR;
    curr_dims = N_ELEMENTS;
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl_id, 1, &curr_dims) < 0)
        TEST_ERROR;
    if ((did = H5Dcreate2(fid, NATIVE_VOL_TEST_DATASET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id,
                          H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Dcreate_anon */
    if ((did_a = H5Dcreate_anon(fid, H5T_NATIVE_INT, sid, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;

    /* H5Dset_extent */
    curr_dims = N_ELEMENTS;
    if (H5Dset_extent(did, &curr_dims) < 0)
        TEST_ERROR;

    /* H5Dflush - skip for parallel file drivers as flush calls cause assertions in the library */
    if (h5_using_parallel_driver(fapl_id, &driver_is_parallel) < 0)
        TEST_ERROR;
    if (!driver_is_parallel)
        if (H5Dflush(did) < 0)
            TEST_ERROR;

    /* H5Dwrite */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, in_buf) < 0)
        TEST_ERROR;

    /* H5Drefresh */
    if (H5Drefresh(did) < 0)
        TEST_ERROR;

    /* H5Dclose */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Dclose(did_a) < 0)
        TEST_ERROR;

    /* H5Dopen */
    if ((did = H5Dopen2(fid, NATIVE_VOL_TEST_DATASET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Dget_space */
    if ((sid = H5Dget_space(did)) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    /* H5Dget_space_status */
    if (H5Dget_space_status(did, &status) < 0)
        TEST_ERROR;

    /* H5Dget_type */
    if ((tid = H5Dget_type(did)) < 0)
        TEST_ERROR;
    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    /* H5Tcopy (when used w/ a dataset, it gets an H5VL struct */
    if ((tid = H5Tcopy(did)) < 0)
        TEST_ERROR;
    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    /* H5Dget_create_plist */
    if ((dcpl_id = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;

    /* H5Dget_access_plist */
    if ((dapl_id = H5Dget_access_plist(did)) < 0)
        TEST_ERROR;
    if (H5Pclose(dapl_id) < 0)
        TEST_ERROR;

    /* H5Dget_storage_size */
    /* XXX: This is a terrible API call that can't truly indicate failure */
    if (0 == (storage_size = H5Dget_storage_size(did)))
        TEST_ERROR;

    /* H5Dget_offset */
    /* XXX: Another bad API call that can't flag error values. Also, this
     *      returns HADDR_UNDEF for chunked datasets, which is bizarre.
     */
    if (HADDR_UNDEF != (offset = H5Dget_offset(did)))
        TEST_ERROR;

    /* H5Dread */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_buf) < 0)
        TEST_ERROR;

    for (i = 0; i < N_ELEMENTS; i++)
        if (in_buf[i] != out_buf[i])
            TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    /* H5Pclose */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Dclose(did);
        H5Dclose(did_a);
        H5Sclose(sid);
        H5Tclose(tid);
        H5Pclose(fapl_id);
        H5Pclose(dapl_id);
        H5Pclose(dcpl_id);
    }
    H5E_END_TRY

    return FAIL;

} /* end test_basic_dataset_operation() */

/*-------------------------------------------------------------------------
 * Function:    test_basic_attribute_operation()
 *
 * Purpose:     Uses the native VOL connector to test basic VOL attribute operations
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_attribute_operation(void)
{
    hid_t fid      = H5I_INVALID_HID;
    hid_t fapl_id  = H5I_INVALID_HID;
    hid_t gid      = H5I_INVALID_HID;
    hid_t aid      = H5I_INVALID_HID;
    hid_t aid_name = H5I_INVALID_HID;
    hid_t sid      = H5I_INVALID_HID;

    char filename[1024];

    hsize_t dims = 1;

    int data_in  = 42;
    int data_out = 0;

    TESTING("Basic VOL attribute operations");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;
    if ((gid = H5Gcreate2(fid, NATIVE_VOL_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    dims = 1;
    if ((sid = H5Screate_simple(1, &dims, &dims)) < 0)
        TEST_ERROR;

    /* H5Acreate */
    if ((aid = H5Acreate2(fid, NATIVE_VOL_TEST_ATTRIBUTE_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Awrite */
    if (H5Awrite(aid, H5T_NATIVE_INT, &data_in) < 0)
        TEST_ERROR;

    /* H5Aread */
    if (H5Aread(aid, H5T_NATIVE_INT, &data_out) < 0)
        TEST_ERROR;
    if (data_in != data_out)
        TEST_ERROR;

    /* H5Aclose */
    if (H5Aclose(aid) < 0)
        TEST_ERROR;

    /* H5Aopen */
    if ((aid = H5Aopen(fid, NATIVE_VOL_TEST_ATTRIBUTE_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Aclose(aid) < 0)
        TEST_ERROR;

    /* H5Adelete */
    if (H5Adelete(fid, NATIVE_VOL_TEST_ATTRIBUTE_NAME) < 0)
        TEST_ERROR;

    /* H5Acreate_by_name */
    if ((aid_name = H5Acreate_by_name(fid, NATIVE_VOL_TEST_GROUP_NAME, NATIVE_VOL_TEST_ATTRIBUTE_NAME,
                                      H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    /* H5Aclose */
    if (H5Aclose(aid_name) < 0)
        TEST_ERROR;

    /* H5Adelete_by_name */
    if (H5Adelete_by_name(fid, NATIVE_VOL_TEST_GROUP_NAME, NATIVE_VOL_TEST_ATTRIBUTE_NAME, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Gclose(gid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    /* H5Pclose */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Pclose(fapl_id);
        H5Gclose(gid);
        H5Sclose(sid);
        H5Aclose(aid);
        H5Aclose(aid_name);
    }
    H5E_END_TRY

    return FAIL;

} /* end test_basic_attribute_operation() */

/*-------------------------------------------------------------------------
 * Function:    test_basic_object_operation()
 *
 * Purpose:     Uses the native VOL connector to test basic VOL object operations
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_object_operation(void)
{
    hid_t fid     = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;
    hid_t gid     = H5I_INVALID_HID;
    hid_t oid     = H5I_INVALID_HID;

    char        filename[1024];
    H5O_info2_t object_info;

    TESTING("Basic VOL object operations");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;
    if ((gid = H5Gcreate2(fid, NATIVE_VOL_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Oget_info */
    if (H5Oget_info3(fid, &object_info, H5O_INFO_ALL) < 0)
        TEST_ERROR;

    //! [H5Oget_info_by_name3_snip]

    /* H5Oget_info_by_name */
    if (H5Oget_info_by_name3(fid, NATIVE_VOL_TEST_GROUP_NAME, &object_info, H5O_INFO_ALL, H5P_DEFAULT) < 0)
        TEST_ERROR;

    //! [H5Oget_info_by_name3_snip]

    /* H5Oexists_by_name */
    if (H5Oexists_by_name(fid, NATIVE_VOL_TEST_GROUP_NAME, H5P_DEFAULT) != true)
        TEST_ERROR;

    /* H5Oopen/close */
    if ((oid = H5Oopen(fid, NATIVE_VOL_TEST_GROUP_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Oclose(oid) < 0)
        TEST_ERROR;

    if (H5Gclose(gid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    /* H5Pclose */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Pclose(fapl_id);
        H5Gclose(gid);
    }
    H5E_END_TRY

    return FAIL;

} /* end test_basic_object_operation() */

/*-------------------------------------------------------------------------
 * Function:    test_basic_link_operation()
 *
 * Purpose:     Uses the native VOL connector to test basic VOL link operations
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_link_operation(void)
{
    hid_t fid     = H5I_INVALID_HID;
    hid_t gid     = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;
    char  filename[1024];

    TESTING("Basic VOL link operations");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;
    if ((gid = H5Gcreate2(fid, NATIVE_VOL_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Lcreate_hard */
    if (H5Lcreate_hard(fid, "/", gid, NATIVE_VOL_TEST_HARD_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* H5Lcreate_soft (to itself) */
    if (H5Lcreate_soft("/", fid, NATIVE_VOL_TEST_SOFT_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* H5Lexists */
    if (H5Lexists(gid, NATIVE_VOL_TEST_HARD_LINK_NAME, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (H5Lexists(fid, NATIVE_VOL_TEST_SOFT_LINK_NAME, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* H5Lcopy */
    if (H5Lcopy(gid, NATIVE_VOL_TEST_HARD_LINK_NAME, fid, NATIVE_VOL_TEST_COPY_LINK_NAME, H5P_DEFAULT,
                H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* H5Lmove */
    if (H5Lmove(fid, NATIVE_VOL_TEST_COPY_LINK_NAME, gid, NATIVE_VOL_TEST_MOVE_LINK_NAME, H5P_DEFAULT,
                H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Gclose(gid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    /* H5Pclose */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Fclose(gid);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;

} /* end test_basic_link_operation() */

/*-------------------------------------------------------------------------
 * Function:    test_basic_datatype_operation()
 *
 * Purpose:     Uses the native VOL connector to test basic VOL datatype operations
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_datatype_operation(void)
{
    hid_t fid      = H5I_INVALID_HID;
    hid_t fapl_id  = H5I_INVALID_HID;
    hid_t tid      = H5I_INVALID_HID;
    hid_t tid_anon = H5I_INVALID_HID;
    hid_t tcpl_id  = H5I_INVALID_HID;
    char  filename[1024];
    bool  driver_is_parallel;

    TESTING("Basic VOL datatype operations");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;
    if ((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;

    /* H5Tcommit */
    if (H5Tcommit2(fid, NATIVE_VOL_TEST_DATATYPE_NAME, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* H5Tflush - skip for parallel file drivers as flush calls cause assertions in the library */
    if (h5_using_parallel_driver(fapl_id, &driver_is_parallel) < 0)
        TEST_ERROR;
    if (!driver_is_parallel)
        if (H5Tflush(tid) < 0)
            TEST_ERROR;

    /* H5Trefresh */
    if (H5Trefresh(tid) < 0)
        TEST_ERROR;

    /* H5Tclose */
    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    /* H5Topen */
    if ((tid = H5Topen2(fid, NATIVE_VOL_TEST_DATATYPE_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* H5Tget_create_plist */
    if ((tcpl_id = H5Tget_create_plist(tid)) < 0)
        TEST_ERROR;

    /* H5Tcommit_anon */
    if ((tid_anon = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;
    if (H5Tcommit_anon(fid, tid_anon, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Pclose(tcpl_id) < 0)
        TEST_ERROR;
    if (H5Tclose(tid) < 0)
        TEST_ERROR;
    if (H5Tclose(tid_anon) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    /* H5Pclose */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(tcpl_id);
        H5Fclose(fid);
        H5Pclose(fapl_id);
        H5Tclose(tid);
        H5Tclose(tid_anon);
    }
    H5E_END_TRY

    return FAIL;

} /* end test_basic_datatype_operation() */

typedef herr_t (*reg_opt_obj_oper_t)(const char *app_file, const char *app_func, unsigned app_line,
                                     hid_t obj_id, H5VL_optional_args_t *args, hid_t dxpl_id, hid_t es_id);
typedef herr_t (*reg_opt_link_oper_t)(const char *app_file, const char *app_func, unsigned app_line,
                                      hid_t obj_id, const char *name, hid_t lapl_id,
                                      H5VL_optional_args_t *args, hid_t dxpl_id, hid_t es_id);
typedef union {
    reg_opt_obj_oper_t  obj_op;
    reg_opt_link_oper_t link_op;
} reg_opt_oper_t;

/*-------------------------------------------------------------------------
 * Function:    exercise_reg_opt_oper()
 *
 * Purpose:     Exercise a particular optional operation for a type.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
exercise_reg_opt_oper(hid_t fake_vol_id, hid_t reg_opt_vol_id, H5VL_subclass_t subcls,
                      const char *subcls_name, H5I_type_t id_type, reg_opt_oper_t reg_opt_op)
{
    char                 op_name[256]; /* Operation name to register */
    hid_t                obj_id = H5I_INVALID_HID;
    H5VL_object_t       *vol_obj;
    H5VL_optional_args_t vol_cb_args;
    int                  fake_obj, fake_arg;
    int                  op_val = -1, op_val2 = -1;
    int                  find_op_val;
    herr_t               ret = SUCCEED;

    /* Test registering optional operation */
    snprintf(op_name, sizeof(op_name), "%s-op1", subcls_name);
    if (H5VLregister_opt_operation(subcls, op_name, &op_val) < 0)
        TEST_ERROR;

    /* Verify that the reserved amount of optional operations is obeyed */
    /* (The first optional operation registered should be at the lower limit) */
    if (op_val < H5VL_RESERVED_NATIVE_OPTIONAL)
        TEST_ERROR;

    /* Look up 1st registered optional operation */
    find_op_val = 0;
    if (H5VLfind_opt_operation(subcls, op_name, &find_op_val) < 0)
        TEST_ERROR;

    /* Verify that the operation was looked up successfully */
    if (op_val != find_op_val)
        TEST_ERROR;

    /* Test registering second optional operation */
    snprintf(op_name, sizeof(op_name), "%s-op2", subcls_name);
    if (H5VLregister_opt_operation(subcls, op_name, &op_val2) < 0)
        TEST_ERROR;

    /* Verify that the reserved amount of optional operations is obeyed */
    /* (The 2nd optional operation registered should be at the lower limit + 1) */
    if (op_val2 < (H5VL_RESERVED_NATIVE_OPTIONAL + 1))
        TEST_ERROR;

    /* Look up 2nd registered optional operation */
    find_op_val = 0;
    if (H5VLfind_opt_operation(subcls, op_name, &find_op_val) < 0)
        TEST_ERROR;

    /* Verify that the operation was looked up successfully */
    if (op_val2 != find_op_val)
        TEST_ERROR;

    /* Push a new API context on the stack */
    /* (Necessary for the named datatype construction routines) */
    if (H5VL_SUBCLS_DATATYPE == subcls)
        H5CX_push();

    /* Create fake object on fake VOL connector */
    if (H5I_INVALID_HID == (obj_id = H5VL_register_using_vol_id(id_type, &fake_obj, fake_vol_id, true)))
        TEST_ERROR;

    /* Pop the API context off the stack */
    if (H5VL_SUBCLS_DATATYPE == subcls)
        H5CX_pop(false);

    /* Attempt to issue operation on fake VOL connector */
    fake_obj            = -1;
    fake_arg            = -1;
    vol_cb_args.op_type = op_val;
    vol_cb_args.args    = &fake_arg;
    H5E_BEGIN_TRY
    {
        if (H5VL_SUBCLS_LINK == subcls || H5VL_SUBCLS_OBJECT == subcls)
            ret = (*reg_opt_op.link_op)(__FILE__, __func__, __LINE__, obj_id, ".", H5P_DEFAULT, &vol_cb_args,
                                        H5P_DEFAULT, H5ES_NONE);
        else
            ret = (*reg_opt_op.obj_op)(__FILE__, __func__, __LINE__, obj_id, &vol_cb_args, H5P_DEFAULT,
                                       H5ES_NONE);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to perform an optional operation with a NULL callback");
    if ((-1) != fake_obj)
        FAIL_PUTS_ERROR("'fake_obj' changed during failed operation?");
    if ((-1) != fake_arg)
        FAIL_PUTS_ERROR("'fake_arg' changed during failed operation?");

    /* Named datatypes must be destroyed differently */
    if (H5VL_SUBCLS_DATATYPE == subcls) {
        H5T_t *dt;

        /* Destroy fake datatype object */
        if (NULL == (dt = H5I_remove(obj_id)))
            TEST_ERROR;
        if (H5VL_free_object(dt->vol_obj) < 0)
            TEST_ERROR;
        dt->vol_obj = NULL;
        if (H5T_close(dt) < 0)
            TEST_ERROR;
    } /* end if */
    else {
        /* Destroy fake object */
        if (NULL == (vol_obj = H5I_remove(obj_id)))
            TEST_ERROR;
        if (H5VL_free_object(vol_obj) < 0)
            TEST_ERROR;
    } /* end else */

    /* Push a new API context on the stack */
    /* (Necessary for the named datatype construction routines) */
    if (H5VL_SUBCLS_DATATYPE == subcls)
        H5CX_push();

    /* Create fake object on reg_opt VOL connector */
    if (H5I_INVALID_HID == (obj_id = H5VL_register_using_vol_id(id_type, &fake_obj, reg_opt_vol_id, true)))
        TEST_ERROR;

    /* Pop the API context off the stack */
    if (H5VL_SUBCLS_DATATYPE == subcls)
        H5CX_pop(false);

    /* Issue first operation */
    fake_obj            = -1;
    fake_arg            = -1;
    reg_opt_curr_op_val = op_val;
    vol_cb_args.op_type = op_val;
    vol_cb_args.args    = &fake_arg;
    if (H5VL_SUBCLS_LINK == subcls || H5VL_SUBCLS_OBJECT == subcls)
        ret = (*reg_opt_op.link_op)(__FILE__, __func__, __LINE__, obj_id, ".", H5P_DEFAULT, &vol_cb_args,
                                    H5P_DEFAULT, H5ES_NONE);
    else
        ret =
            (*reg_opt_op.obj_op)(__FILE__, __func__, __LINE__, obj_id, &vol_cb_args, H5P_DEFAULT, H5ES_NONE);
    if (ret < 0)
        TEST_ERROR;

    /* Verify that fake object & argument were modified correctly */
    if (op_val != fake_obj)
        FAIL_PUTS_ERROR("'fake_obj' not updated");
    if (op_val != fake_arg)
        FAIL_PUTS_ERROR("'fake_arg' not updated");

    /* Issue second operation */
    fake_obj            = -1;
    fake_arg            = -1;
    reg_opt_curr_op_val = op_val2;
    vol_cb_args.op_type = op_val2;
    vol_cb_args.args    = &fake_arg;
    if (H5VL_SUBCLS_LINK == subcls || H5VL_SUBCLS_OBJECT == subcls)
        ret = (*reg_opt_op.link_op)(__FILE__, __func__, __LINE__, obj_id, ".", H5P_DEFAULT, &vol_cb_args,
                                    H5P_DEFAULT, H5ES_NONE);
    else
        ret =
            (*reg_opt_op.obj_op)(__FILE__, __func__, __LINE__, obj_id, &vol_cb_args, H5P_DEFAULT, H5ES_NONE);
    if (ret < 0)
        TEST_ERROR;

    /* Verify that fake object & argument were modified correctly */
    if (op_val2 != fake_obj)
        FAIL_PUTS_ERROR("'fake_obj' not updated");
    if (op_val2 != fake_arg)
        FAIL_PUTS_ERROR("'fake_arg' not updated");

    /* Named datatypes must be destroyed differently */
    if (H5VL_SUBCLS_DATATYPE == subcls) {
        H5T_t *dt;

        /* Destroy fake datatype object */
        if (NULL == (dt = H5I_remove(obj_id)))
            TEST_ERROR;
        if (H5VL_free_object(dt->vol_obj) < 0)
            TEST_ERROR;
        dt->vol_obj = NULL;
        if (H5T_close(dt) < 0)
            TEST_ERROR;
    } /* end if */
    else {
        /* Destroy fake object */
        if (NULL == (vol_obj = H5I_remove(obj_id)))
            TEST_ERROR;
        if (H5VL_free_object(vol_obj) < 0)
            TEST_ERROR;
    } /* end else */

    /* Unregister 2nd registered optional operation */
    if (H5VLunregister_opt_operation(subcls, op_name) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    return FAIL;
} /* end exercise_reg_opt_oper() */

/*-------------------------------------------------------------------------
 * Function:    test_register_opt_operation()
 *
 * Purpose:     Tests if we can load, register, and close a simple
 *              VOL connector.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_register_opt_operation(void)
{
    hid_t fake_vol_id    = H5I_INVALID_HID;
    hid_t reg_opt_vol_id = H5I_INVALID_HID;
    struct {
        H5VL_subclass_t subcls;
        const char     *subcls_name;
        H5I_type_t      id_type;
        reg_opt_oper_t  reg_opt_op;
    } test_params[] = {{H5VL_SUBCLS_ATTR, "attr", H5I_ATTR, {.obj_op = H5VLattr_optional_op}},
                       {H5VL_SUBCLS_DATASET, "dataset", H5I_DATASET, {.obj_op = H5VLdataset_optional_op}},
                       {H5VL_SUBCLS_DATATYPE, "datatype", H5I_DATATYPE, {.obj_op = H5VLdatatype_optional_op}},
                       {H5VL_SUBCLS_FILE, "file", H5I_FILE, {.obj_op = H5VLfile_optional_op}},
                       {H5VL_SUBCLS_GROUP, "group", H5I_GROUP, {.obj_op = H5VLgroup_optional_op}},
                       {H5VL_SUBCLS_LINK, "link", H5I_GROUP, {.link_op = H5VLlink_optional_op}},
                       {H5VL_SUBCLS_OBJECT, "object", H5I_GROUP, {.link_op = H5VLobject_optional_op}}};
    int      op_val = -1;
    unsigned u;
    herr_t   ret = SUCCEED;

    TESTING("dynamically registering optional operations");

    /* Register the VOL connectors for testing */
    if ((fake_vol_id = H5VLregister_connector(&fake_vol_g, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((reg_opt_vol_id = H5VLregister_connector(&reg_opt_vol_g, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Test registering invalid optional VOL subclass operations */
    H5E_BEGIN_TRY
    {
        ret = H5VLregister_opt_operation(H5VL_SUBCLS_NONE, "fail", &op_val);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to register an optional operation for the 'NONE' VOL subclass");
    if ((-1) != op_val)
        FAIL_PUTS_ERROR("'op_val' changed during failed operation?");
    H5E_BEGIN_TRY
    {
        ret = H5VLregister_opt_operation(H5VL_SUBCLS_INFO, "fail2", &op_val);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to register an optional operation for the 'INFO' VOL subclass");
    if ((-1) != op_val)
        FAIL_PUTS_ERROR("'op_val' changed during failed operation?");
    H5E_BEGIN_TRY
    {
        ret = H5VLregister_opt_operation(H5VL_SUBCLS_WRAP, "fail3", &op_val);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to register an optional operation for the 'WRAP' VOL subclass");
    if ((-1) != op_val)
        FAIL_PUTS_ERROR("'op_val' changed during failed operation?");
    H5E_BEGIN_TRY
    {
        ret = H5VLregister_opt_operation(H5VL_SUBCLS_BLOB, "fail4", &op_val);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to register an optional operation for the 'BLOB' VOL subclass");
    if ((-1) != op_val)
        FAIL_PUTS_ERROR("'op_val' changed during failed operation?");
    H5E_BEGIN_TRY
    {
        ret = H5VLregister_opt_operation(H5VL_SUBCLS_TOKEN, "fail5", &op_val);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to register an optional operation for the 'TOKEN' VOL subclass");
    if ((-1) != op_val)
        FAIL_PUTS_ERROR("'op_val' changed during failed operation?");

    /* Test registering valid optional VOL subclass operation with NULL op_val ptr*/
    H5E_BEGIN_TRY
    {
        ret = H5VLregister_opt_operation(H5VL_SUBCLS_FILE, "fail6", NULL);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to register an optional operation with a NULL 'op_val'");

    /* Try finding a non-existent optional VOL subclass operation */
    H5E_BEGIN_TRY
    {
        ret = H5VLfind_opt_operation(H5VL_SUBCLS_DATASET, "fail", &op_val);
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to find a non-existent optional operation");

    /* Try unregistering a non-existent optional VOL subclass operation */
    H5E_BEGIN_TRY
    {
        ret = H5VLunregister_opt_operation(H5VL_SUBCLS_DATASET, "fail");
    }
    H5E_END_TRY
    if (FAIL != ret)
        FAIL_PUTS_ERROR("should not be able to unregister a non-existent optional operation");

    /* Optional operations on requests are supported (but difficult to test further) */
    if (H5VLregister_opt_operation(H5VL_SUBCLS_REQUEST, "req_op", &op_val) < 0)
        TEST_ERROR;

    /* Register & test calling optional operations for each valid VOL subclass */
    /* (Table-driven, with test_params array) */
    for (u = 0; u < NELMTS(test_params); u++)
        /* Exercise appropriate callback, for each VOL subclass */
        if (exercise_reg_opt_oper(fake_vol_id, reg_opt_vol_id, test_params[u].subcls,
                                  test_params[u].subcls_name, test_params[u].id_type,
                                  test_params[u].reg_opt_op) < 0)
            TEST_ERROR;

    /* Unregister the VOL connectors */
    if (H5VLunregister_connector(fake_vol_id) < 0)
        TEST_ERROR;
    if (H5VLunregister_connector(reg_opt_vol_id) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5VLunregister_connector(fake_vol_id);
        H5VLunregister_connector(reg_opt_vol_id);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_register_opt_operation() */

/*-------------------------------------------------------------------------
 * Function:    test_async_vol_props()
 *
 * Purpose:     Test properties related to asynchronous VOL connector operation
 *
 * Note:        Overrides the HDF5_VOL_CONNECTOR environment variable, to
 *              provide stable testing environment.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_async_vol_props(void)
{
    hid_t                    fapl_id = H5I_INVALID_HID;
    hid_t                    vol_id  = H5I_INVALID_HID;
    H5VL_pass_through_info_t passthru_info;
    char                    *conn_env_str = NULL;

    TESTING("Async VOL props");

    vol_cap_flags_g = H5VL_CAP_FLAG_NONE;

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();

    /* Test 'capability flags' property */

    /* Test query w/NULL for cap_flags parameter */
    if (H5Pget_vol_cap_flags(fapl_id, NULL) < 0)
        FAIL_STACK_ERROR;

    /* Override possible environment variable & re-initialize default VOL connector */
    conn_env_str = getenv(HDF5_VOL_CONNECTOR);
    if (conn_env_str) {
        if (NULL == (conn_env_str = strdup(conn_env_str)))
            TEST_ERROR;
        if (HDunsetenv(HDF5_VOL_CONNECTOR) < 0)
            TEST_ERROR;
        if (H5VL__reparse_def_vol_conn_variable_test() < 0)
            TEST_ERROR;
    }

    /* Test query w/default VOL, which should indicate no async, since native connector
     * doesn't support async.
     */
    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g) < 0)
        FAIL_STACK_ERROR;
    if ((vol_cap_flags_g & H5VL_CAP_FLAG_ASYNC) > 0)
        TEST_ERROR;
    if ((vol_cap_flags_g & H5VL_CAP_FLAG_NATIVE_FILES) == 0)
        TEST_ERROR;

    /* Close FAPL */
    if (H5Pclose(fapl_id) < 0)
        FAIL_STACK_ERROR;

    /* Register a fake VOL connector that sets the async capability flag */
    if ((vol_id = H5VLregister_connector(&fake_async_vol_g, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Set environment variable to use 'fake async' connector & re-init default connector */
    if (HDsetenv(HDF5_VOL_CONNECTOR, "fake_async", true) < 0)
        TEST_ERROR;
    if (H5VL__reparse_def_vol_conn_variable_test() < 0)
        TEST_ERROR;

    /* Retrieve the file access property again */
    fapl_id = h5_fileaccess();

    /* Test query w/fake async VOL, which should succeed */
    vol_cap_flags_g = H5VL_CAP_FLAG_NONE;
    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g) < 0)
        FAIL_STACK_ERROR;
    if ((vol_cap_flags_g & H5VL_CAP_FLAG_ASYNC) == 0)
        TEST_ERROR;
    if ((vol_cap_flags_g & H5VL_CAP_FLAG_NATIVE_FILES) > 0)
        TEST_ERROR;

    /* Reset environment variable & re-init default connector */
    if (HDunsetenv(HDF5_VOL_CONNECTOR) < 0)
        TEST_ERROR;
    if (H5VL__reparse_def_vol_conn_variable_test() < 0)
        TEST_ERROR;

    /* Close FAPL */
    if (H5Pclose(fapl_id) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the file access property again */
    fapl_id = h5_fileaccess();

    /* Set the VOL connector for the FAPL to the fake async connector */
    if (H5Pset_vol(fapl_id, vol_id, NULL) < 0)
        FAIL_STACK_ERROR;

    /* Test query w/fake async VOL, which should succeed */
    vol_cap_flags_g = H5VL_CAP_FLAG_NONE;
    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g) < 0)
        FAIL_STACK_ERROR;
    if ((vol_cap_flags_g & H5VL_CAP_FLAG_ASYNC) == 0)
        TEST_ERROR;
    if ((vol_cap_flags_g & H5VL_CAP_FLAG_NATIVE_FILES) > 0)
        TEST_ERROR;

    /* Stack the [internal] passthrough VOL connector on top of the fake async connector */
    passthru_info.under_vol_id   = vol_id;
    passthru_info.under_vol_info = NULL;
    if (H5Pset_vol(fapl_id, H5VL_PASSTHRU, &passthru_info) < 0)
        FAIL_STACK_ERROR;

    /* Test query w/passthru -> fake async VOL, which should succeed */
    vol_cap_flags_g = H5VL_CAP_FLAG_NONE;
    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g) < 0)
        FAIL_STACK_ERROR;
    if ((vol_cap_flags_g & H5VL_CAP_FLAG_ASYNC) == 0)
        TEST_ERROR;
    if ((vol_cap_flags_g & H5VL_CAP_FLAG_NATIVE_FILES) > 0)
        TEST_ERROR;

    /* Unregister the fake async VOL ID */
    if (H5VLunregister_connector(vol_id) < 0)
        TEST_ERROR;

    /* Close FAPL */
    if (H5Pclose(fapl_id) < 0)
        FAIL_STACK_ERROR;

    /* Restore environment variable, if there was one */
    if (conn_env_str) {
        if (HDsetenv(HDF5_VOL_CONNECTOR, conn_env_str, true) < 0)
            TEST_ERROR;
        free(conn_env_str);

        if (H5VL__reparse_def_vol_conn_variable_test() < 0)
            TEST_ERROR;
    }

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5VLunregister_connector(vol_id);
    }
    H5E_END_TRY
    free(conn_env_str);

    return FAIL;
} /* end test_async_vol_props() */

/*-------------------------------------------------------------------------
 * Function:    test_vol_cap_flags()
 *
 * Purpose:     Tests the VOL cap flags
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_vol_cap_flags(void)
{
    hid_t                    fapl_id = H5I_INVALID_HID;
    hid_t                    vol_id  = H5I_INVALID_HID;
    char                    *vol_env = NULL;
    H5VL_pass_through_info_t passthru_info;

    TESTING("VOL capability flags");

    vol_cap_flags_g = H5VL_CAP_FLAG_NONE;

    /* Register a fake VOL */
    if ((vol_id = H5VLregister_connector(&fake_vol_g, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if (H5Pset_vol(fapl_id, vol_id, NULL) < 0)
        TEST_ERROR;

    /* Verify the correctness of the VOL capacity flags */
    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g) < 0)
        TEST_ERROR;

    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC))
        TEST_ERROR;

    if (vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)
        TEST_ERROR;

    /* If using the native VOL by default, check flags again with H5P_DEFAULT */
    vol_env = getenv(HDF5_VOL_CONNECTOR);
    if (!vol_env || (0 == strcmp(vol_env, "native"))) {
        H5VL_class_t *cls;
        hid_t         connector_id;

        if (H5Pget_vol_id(H5P_DEFAULT, &connector_id) < 0)
            TEST_ERROR;
        if (NULL == (cls = H5I_object(connector_id)))
            TEST_ERROR;

        vol_cap_flags_g = H5VL_CAP_FLAG_NONE;

        if (H5Pget_vol_cap_flags(H5P_DEFAULT, &vol_cap_flags_g) < 0)
            TEST_ERROR;

        if (vol_cap_flags_g != cls->cap_flags)
            TEST_ERROR;

        if (H5VLclose(connector_id) < 0)
            TEST_ERROR;
    }

    /* Stack the [internal] passthrough VOL connector on top of the fake connector */
    passthru_info.under_vol_id   = vol_id;
    passthru_info.under_vol_info = NULL;

    if (H5Pset_vol(fapl_id, H5VL_PASSTHRU, &passthru_info) < 0)
        FAIL_STACK_ERROR;

    /* Verify the correctness of the VOL capacity flags */
    vol_cap_flags_g = H5VL_CAP_FLAG_NONE;

    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g) < 0)
        TEST_ERROR;

    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC))
        TEST_ERROR;

    if (vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)
        TEST_ERROR;

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    /* Unregister the fake VOL ID */
    if (H5VLunregister_connector(vol_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5VLunregister_connector(vol_id);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_vol_cap_flags() */

/*-------------------------------------------------------------------------
 * Function:    test_get_vol_name()
 *
 * Purpose:     Tests getting VOL name using H5VLget_connector_name
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_vol_name(void)
{
    hid_t       fapl_id = H5I_INVALID_HID;
    hid_t       file_id = H5I_INVALID_HID;
    char        filename[NAME_LEN];
    char        vol_name[NAME_LEN];
    const char *conn_env_str = NULL;

    TESTING("getting connector name");

    conn_env_str = getenv(HDF5_VOL_CONNECTOR);
    if (NULL == (conn_env_str = getenv("HDF5_VOL_CONNECTOR")))
        conn_env_str = "native";

    /* Skip the connectors other than the native and pass_through connector */
    if (strcmp(conn_env_str, "native") && strcmp(conn_env_str, "pass_through")) {
        SKIPPED();
        printf("    only test the native or internal pass_through connector\n");
        return SUCCEED;
    }

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    if (H5VLget_connector_name(file_id, vol_name, NAME_LEN) < 0)
        TEST_ERROR;

    /* When comparing the pass_through connector, ignore the rest information (under_vol=0;under_info={}) */
    if ((!strcmp(conn_env_str, "native") && strcmp(vol_name, "native")) ||
        (!strcmp(conn_env_str, "pass_through") && strcmp(vol_name, "pass_through")))
        TEST_ERROR;

    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file_id);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_get_vol_name() */

/*-------------------------------------------------------------------------
 * Function:    test_wrap_register()
 *
 * Purpose:     Tests the failure of calling H5VLwrap_register in application
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_wrap_register(void)
{
    hid_t fapl_id  = H5I_INVALID_HID;
    hid_t file_id  = H5I_INVALID_HID;
    hid_t group_id = H5I_INVALID_HID;
    hid_t wrap_id  = H5I_INVALID_HID;
    char  filename[NAME_LEN];
    void *vol_obj;

    TESTING("failure of calling H5VLwrap_register");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    if ((group_id = H5Gcreate2(file_id, "test", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Retrieve the VOL object for the group */
    if (!(vol_obj = H5VLobject(group_id)))
        TEST_ERROR;

    /* Try to register a second ID for the group.  It should fail because this routine is mainly
     * targeted toward wrapping objects for iteration routine callbacks, not for application use
     */
    H5E_BEGIN_TRY
    {
        wrap_id = H5VLwrap_register(vol_obj, H5I_GROUP);
    }
    H5E_END_TRY

    if (H5I_INVALID_HID != wrap_id)
        FAIL_PUTS_ERROR("should not be able to call H5VLwrap_register in an application");

    if (H5Gclose(group_id) < 0)
        TEST_ERROR;

    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(group_id);
        H5Fclose(file_id);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_wrap_register() */

/*-------------------------------------------------------------------------
 * Function:    test_info_to_str()
 *
 * Purpose:     Tests the conversion between a VOL info and a string
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_info_to_str(void)
{
    hid_t fapl_id  = H5I_INVALID_HID;
    hid_t vol_id   = H5I_INVALID_HID;
    int   info     = INT_MAX;
    char *ret_str  = NULL;
    int  *ret_info = NULL;

    TESTING("conversion between a VOL info and a string");

    /* Register a fake VOL */
    if ((vol_id = H5VLregister_connector(&fake_vol_g, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if (H5Pset_vol(fapl_id, vol_id, NULL) < 0)
        TEST_ERROR;

    /* Serialize the VOL info into a string */
    if (H5VLconnector_info_to_str(&info, vol_id, &ret_str) < 0)
        TEST_ERROR;

    /* Parse the string and construct it into a VOL info */
    if (H5VLconnector_str_to_info(ret_str, vol_id, (void **)(&ret_info)) < 0)
        TEST_ERROR;

    if (*ret_info != info)
        FAIL_PUTS_ERROR("the returned VOL info doesn't match the original info");

    /* Free the VOL info being returned */
    if (H5VLfree_connector_info(vol_id, ret_info) < 0)
        TEST_ERROR;

    /* Free the string being returned */
    if (ret_str)
        free(ret_str);

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    /* Unregister the fake VOL ID */
    if (H5VLunregister_connector(vol_id) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5VLunregister_connector(vol_id);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_info_to_str() */

/*-------------------------------------------------------------------------
 * Function:    test_query_optional
 *
 * Purpose:     Tests the bug fix (HDFFV-11208) that a committed datatype
 *              triggered an assertion failure in H5VLquery_optional
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_query_optional(void)
{
    hid_t    fapl_id  = H5I_INVALID_HID;
    hid_t    file_id  = H5I_INVALID_HID;
    hid_t    group_id = H5I_INVALID_HID;
    hid_t    dtype_id = H5I_INVALID_HID;
    char     filename[NAME_LEN];
    uint64_t supported = 0;

    TESTING("H5VLquery_optional");

    /* Retrieve the file access property for testing */
    fapl_id = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    if ((group_id = H5Gcreate2(file_id, "test_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Test H5VLquery_optional with a group */
    if (H5VLquery_optional(group_id, H5VL_SUBCLS_OBJECT, H5VL_NATIVE_OBJECT_GET_COMMENT, &supported) < 0)
        TEST_ERROR;

    if ((dtype_id = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;

    /* Commit the datatype into the file */
    if (H5Tcommit2(file_id, "test_dtype", dtype_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Test H5VLquery_optional with a committed datatype where the assertion failure happened in the past */
    if (H5VLquery_optional(dtype_id, H5VL_SUBCLS_OBJECT, H5VL_NATIVE_OBJECT_GET_COMMENT, &supported) < 0)
        TEST_ERROR;

    if (H5Gclose(group_id) < 0)
        TEST_ERROR;

    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;

    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[0], fapl_id);

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(group_id);
        H5Tclose(dtype_id);
        H5Fclose(file_id);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_query_optional() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the virtual object layer interface (H5VL)
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    const char *driver_name; /* File driver value from environment */
    int         nerrors = 0;

    /* Get the VFD to use */
    driver_name = h5_get_test_driver_name();

    h5_reset();

    puts("Testing basic Virtual Object Layer (VOL) functionality.");

    nerrors += test_vol_registration() < 0 ? 1 : 0;
    nerrors += test_register_opt_operation() < 0 ? 1 : 0;
    nerrors += test_native_vol_init() < 0 ? 1 : 0;
    nerrors += test_basic_file_operation(driver_name) < 0 ? 1 : 0;
    nerrors += test_basic_group_operation() < 0 ? 1 : 0;
    nerrors += test_basic_dataset_operation() < 0 ? 1 : 0;
    nerrors += test_basic_attribute_operation() < 0 ? 1 : 0;
    nerrors += test_basic_object_operation() < 0 ? 1 : 0;
    nerrors += test_basic_link_operation() < 0 ? 1 : 0;
    nerrors += test_basic_datatype_operation() < 0 ? 1 : 0;
    nerrors += test_async_vol_props() < 0 ? 1 : 0;
    nerrors += test_vol_cap_flags() < 0 ? 1 : 0;
    nerrors += test_get_vol_name() < 0 ? 1 : 0;
    nerrors += test_wrap_register() < 0 ? 1 : 0;
    nerrors += test_info_to_str() < 0 ? 1 : 0;
    nerrors += test_query_optional() < 0 ? 1 : 0;

    if (nerrors) {
        printf("***** %d Virtual Object Layer TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        exit(EXIT_FAILURE);
    }

    puts("All Virtual Object Layer (VOL) tests passed.");

    exit(EXIT_SUCCESS);

} /* end main() */
