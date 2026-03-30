/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "H5Ipublic.h"
#include "H5VLpublic.h"
#include "H5VLconnector.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define H5_ATTR_UNUSED __attribute__((unused))
#define H5I_INVALID_HID (-1)
#define TEST_ERROR goto error
#define H5_FAILED() printf("*** FAILED ***\n")
#define TESTING(MSG) printf("Testing %-62s", MSG)
#define PASSED() printf(" PASSED\n")

/* Sequence: H5Iinc_ref,H5VLget_wrap_ctx */

typedef struct H5VL_pass_through_t {
    void *under_object;
    hid_t under_vol_id;
} H5VL_pass_through_t;

typedef struct H5VL_pass_through_wrap_ctx_t {
    hid_t under_vol_id;
    void *under_wrap_ctx;
} H5VL_pass_through_wrap_ctx_t;

static herr_t
H5VL_pass_through_get_wrap_ctx(const void *obj, void **wrap_ctx)
{
    const H5VL_pass_through_t *o = (const H5VL_pass_through_t *)obj;
    H5VL_pass_through_wrap_ctx_t *new_wrap_ctx;
    herr_t ret_value;

    printf("[DEBUG] Entering H5VL_pass_through_get_wrap_ctx\n");
    printf("[DEBUG] Input obj pointer: %p\n", obj);
    printf("[DEBUG] Input wrap_ctx pointer: %p\n", (void*)wrap_ctx);

    printf("[DEBUG] Attempting to allocate wrap context structure\n");
    new_wrap_ctx = (H5VL_pass_through_wrap_ctx_t *)calloc(1, sizeof(H5VL_pass_through_wrap_ctx_t));
    if (!new_wrap_ctx) {
        printf("[ERROR] Failed to allocate wrap context\n");
        return -1;
    }
    printf("[DEBUG] Successfully allocated wrap context at %p\n", (void*)new_wrap_ctx);

    printf("[DEBUG] Setting under_vol_id from o->under_vol_id\n");
    new_wrap_ctx->under_vol_id = o->under_vol_id;
    printf("[DEBUG] new_wrap_ctx->under_vol_id = %lld\n", (long long)new_wrap_ctx->under_vol_id);
    
    printf("[DEBUG] Initializing under_wrap_ctx to NULL\n");
    new_wrap_ctx->under_wrap_ctx = NULL;

    /* Sequence: H5Iinc_ref,H5VLget_wrap_ctx */
    printf("[DEBUG] Calling H5Iinc_ref with vol_id=%lld\n", (long long)new_wrap_ctx->under_vol_id);
    ret_value = H5Iinc_ref(new_wrap_ctx->under_vol_id);
    printf("[DEBUG] H5Iinc_ref returned %d\n", ret_value);
    
    if (ret_value < 0) {
        printf("[ERROR] H5Iinc_ref failed with return value %d\n", ret_value);
        printf("[DEBUG] Freeing new_wrap_ctx before returning error\n");
        free(new_wrap_ctx);
        printf("[DEBUG] Returning -1 from H5VL_pass_through_get_wrap_ctx\n");
        return -1;
    }
    printf("[DEBUG] H5Iinc_ref succeeded\n");

    printf("[DEBUG] Calling H5VLget_wrap_ctx with under_object=%p, under_vol_id=%lld\n", 
           o->under_object, (long long)o->under_vol_id);
    ret_value = H5VLget_wrap_ctx(o->under_object, o->under_vol_id, &new_wrap_ctx->under_wrap_ctx);
    printf("[DEBUG] H5VLget_wrap_ctx executed with return value: %d\n", ret_value);

    /* Validate H5VLget_wrap_ctx behavior according to documentation */
    if (ret_value < 0) {
        printf("[ERROR] H5VLget_wrap_ctx failed - return value is negative: %d\n", ret_value);
        printf("[DEBUG] Calling H5Idec_ref to clean up\n");
        H5Idec_ref(new_wrap_ctx->under_vol_id);
        printf("[DEBUG] Freeing new_wrap_ctx before returning error\n");
        free(new_wrap_ctx);
        printf("[DEBUG] Returning -1 from H5VL_pass_through_get_wrap_ctx\n");
        return -1;
    }
    printf("[DEBUG] H5VLget_wrap_ctx succeeded (return value >= 0)\n");

    /* Check that wrap_ctx output parameter was set (should be non-NULL or NULL depending on VOL connector) */
    printf("[DEBUG] H5VLget_wrap_ctx returned successfully, under_wrap_ctx=%p\n", new_wrap_ctx->under_wrap_ctx);

    /* Verify the output parameter was properly handled */
    if (ret_value >= 0) {
        printf("[DEBUG] H5VLget_wrap_ctx succeeded as expected\n");
    }

    printf("[DEBUG] Setting output parameter *wrap_ctx to %p\n", (void*)new_wrap_ctx);
    *wrap_ctx = new_wrap_ctx;
    printf("[DEBUG] H5VL_pass_through_get_wrap_ctx succeeded, returning 0\n");
    return 0;
}

static void
test_H5VLget_wrap_ctx(void H5_ATTR_UNUSED *params)
{
    H5VL_pass_through_t test_obj;
    void *wrap_ctx = NULL;
    herr_t result = -1;
    hid_t file_id = H5I_INVALID_HID;

    printf("[DEBUG] ========== Entering test_H5VLget_wrap_ctx ==========\n");
    printf("[DEBUG] Initializing test_obj structure\n");
    printf("[DEBUG] Initial wrap_ctx = %p\n", wrap_ctx);
    printf("[DEBUG] Initial result = %d\n", result);
    printf("[DEBUG] Initial file_id = %lld\n", (long long)file_id);

    TESTING("H5VLget_wrap_ctx functionality");

    /* Initialize HDF5 library */
    printf("[DEBUG] Calling H5open to initialize HDF5 library\n");
    if (H5open() < 0) {
        printf("[ERROR] H5open failed\n");
        H5_FAILED();
        printf("    couldn't initialize HDF5 library\n");
        goto error;
    }
    printf("[DEBUG] H5open completed successfully\n");

    /* Create a simple file to get a valid object */
    printf("[DEBUG] Creating test file 'test_file.h5'\n");
    file_id = H5Fcreate("test_file.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    printf("[DEBUG] H5Fcreate returned file_id = %lld\n", (long long)file_id);
    
    if (file_id < 0) {
        printf("[ERROR] H5Fcreate failed, file_id = %lld\n", (long long)file_id);
        H5_FAILED();
        printf("    couldn't create test file\n");
        goto error;
    }
    printf("[DEBUG] Test file created successfully with id=%lld\n", (long long)file_id);

    /* Get the VOL connector ID for the file */
    printf("[DEBUG] Getting VOL connector ID for file_id=%lld\n", (long long)file_id);
    test_obj.under_vol_id = H5VLget_connector_id(file_id);
    printf("[DEBUG] H5VLget_connector_id returned VOL connector ID=%lld\n", (long long)test_obj.under_vol_id);

    if (test_obj.under_vol_id < 0) {
        printf("[ERROR] H5VLget_connector_id failed, returned %lld\n", (long long)test_obj.under_vol_id);
        H5_FAILED();
        printf("    couldn't get VOL connector ID\n");
        goto error;
    }
    printf("[DEBUG] VOL connector ID obtained successfully\n");

    /* Get the underlying object */
    printf("[DEBUG] Getting underlying object for file_id=%lld\n", (long long)file_id);
    test_obj.under_object = H5VLobject(file_id);
    printf("[DEBUG] H5VLobject returned underlying object pointer=%p\n", test_obj.under_object);

    if (test_obj.under_object != NULL) {
        printf("[DEBUG] Underlying object is not NULL, proceeding with test\n");
        printf("[DEBUG] Calling H5VL_pass_through_get_wrap_ctx with test_obj at %p\n", (void*)&test_obj);
        result = H5VL_pass_through_get_wrap_ctx(&test_obj, &wrap_ctx);
        printf("[DEBUG] H5VL_pass_through_get_wrap_ctx returned %d\n", result);
        printf("[DEBUG] wrap_ctx after call = %p\n", wrap_ctx);

        if (result == 0) {
            printf("[DEBUG] H5VL_pass_through_get_wrap_ctx succeeded (result == 0)\n");
            /* Verify wrap_ctx was set */
            if (wrap_ctx == NULL) {
                printf("[ERROR] wrap_ctx is NULL after successful call\n");
                H5_FAILED();
                printf("    wrap_ctx is NULL after successful call\n");
                goto error;
            } else {
                printf("[DEBUG] wrap_ctx is not NULL (%p), test condition satisfied\n", wrap_ctx);
                printf("[DEBUG] Test PASSED: H5VLget_wrap_ctx executed correctly\n");
            }
        } else {
            printf("[ERROR] H5VL_pass_through_get_wrap_ctx returned error: %d\n", result);
            H5_FAILED();
            printf("    H5VL_pass_through_get_wrap_ctx returned error\n");
            goto error;
        }
    } else {
        printf("[ERROR] Underlying object is NULL\n");
        H5_FAILED();
        printf("    couldn't get underlying object\n");
        goto error;
    }

    printf("[DEBUG] Beginning cleanup phase\n");
    if (wrap_ctx) {
        printf("[DEBUG] wrap_ctx is not NULL, proceeding with cleanup\n");
        H5VL_pass_through_wrap_ctx_t *ctx = (H5VL_pass_through_wrap_ctx_t *)wrap_ctx;
        printf("[DEBUG] Cast wrap_ctx to H5VL_pass_through_wrap_ctx_t at %p\n", (void*)ctx);
        
        if (ctx->under_wrap_ctx) {
            printf("[DEBUG] ctx->under_wrap_ctx is not NULL (%p), freeing it\n", ctx->under_wrap_ctx);
            H5VLfree_wrap_ctx(ctx->under_wrap_ctx, ctx->under_vol_id);
            printf("[DEBUG] H5VLfree_wrap_ctx completed\n");
        } else {
            printf("[DEBUG] ctx->under_wrap_ctx is NULL, skipping H5VLfree_wrap_ctx\n");
        }
        
        if (ctx->under_vol_id >= 0) {
            printf("[DEBUG] ctx->under_vol_id is valid (%lld), decrementing reference\n", (long long)ctx->under_vol_id);
            H5Idec_ref(ctx->under_vol_id);
            printf("[DEBUG] H5Idec_ref completed\n");
        } else {
            printf("[DEBUG] ctx->under_vol_id is invalid (%lld), skipping H5Idec_ref\n", (long long)ctx->under_vol_id);
        }
        
        printf("[DEBUG] Freeing wrap_ctx at %p\n", wrap_ctx);
        free(wrap_ctx);
        printf("[DEBUG] wrap_ctx freed\n");
    } else {
        printf("[DEBUG] wrap_ctx is NULL, skipping wrap_ctx cleanup\n");
    }

    if (test_obj.under_vol_id >= 0) {
        printf("[DEBUG] test_obj.under_vol_id is valid (%lld), closing VOL connector\n", (long long)test_obj.under_vol_id);
        H5VLclose(test_obj.under_vol_id);
        printf("[DEBUG] H5VLclose completed\n");
    } else {
        printf("[DEBUG] test_obj.under_vol_id is invalid (%lld), skipping H5VLclose\n", (long long)test_obj.under_vol_id);
    }

    printf("[DEBUG] Closing file with id=%lld\n", (long long)file_id);
    if (H5Fclose(file_id) < 0) {
        printf("[ERROR] H5Fclose failed\n");
        TEST_ERROR;
    }
    printf("[DEBUG] H5Fclose completed successfully\n");

    printf("[DEBUG] Closing HDF5 library\n");
    if (H5close() < 0) {
        printf("[ERROR] H5close failed\n");
        TEST_ERROR;
    }
    printf("[DEBUG] H5close completed successfully\n");

    PASSED();

    printf("[DEBUG] ========== Exiting test_H5VLget_wrap_ctx successfully ==========\n");
    return;

error:
    printf("[DEBUG] ========== Entering error cleanup section ==========\n");
    H5E_BEGIN_TRY
    {
        if (wrap_ctx) {
            printf("[DEBUG] [ERROR PATH] wrap_ctx is not NULL, cleaning up\n");
            H5VL_pass_through_wrap_ctx_t *ctx = (H5VL_pass_through_wrap_ctx_t *)wrap_ctx;
            if (ctx->under_wrap_ctx) {
                printf("[DEBUG] [ERROR PATH] Freeing ctx->under_wrap_ctx\n");
                H5VLfree_wrap_ctx(ctx->under_wrap_ctx, ctx->under_vol_id);
            }
            if (ctx->under_vol_id >= 0) {
                printf("[DEBUG] [ERROR PATH] Decrementing ctx->under_vol_id\n");
                H5Idec_ref(ctx->under_vol_id);
            }
            printf("[DEBUG] [ERROR PATH] Freeing wrap_ctx\n");
            free(wrap_ctx);
        } else {
            printf("[DEBUG] [ERROR PATH] wrap_ctx is NULL\n");
        }
        
        if (test_obj.under_vol_id >= 0) {
            printf("[DEBUG] [ERROR PATH] Closing test_obj.under_vol_id\n");
            H5VLclose(test_obj.under_vol_id);
        }
        
        printf("[DEBUG] [ERROR PATH] Closing file_id\n");
        H5Fclose(file_id);
        
        printf("[DEBUG] [ERROR PATH] Closing HDF5 library\n");
        H5close();
    }
    H5E_END_TRY
    printf("[DEBUG] ========== Exiting test_H5VLget_wrap_ctx via error path ==========\n");

    return;
}

int
main(void)
{
    printf("\n");
    printf("**********************************************\n");
    printf("*                                            *\n");
    printf("*          H5VLget_wrap_ctx Test             *\n");
    printf("*                                            *\n");
    printf("**********************************************\n\n");

    printf("[DEBUG] ========== MAIN: Starting test execution ==========\n");
    test_H5VLget_wrap_ctx(NULL);
    printf("[DEBUG] ========== MAIN: Test execution completed ==========\n");
    printf("[DEBUG] ========== MAIN: Program exiting normally ==========\n");

    return 0;
}
