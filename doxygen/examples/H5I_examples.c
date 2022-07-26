/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_dcpl;
        hid_t dcpl;

        // create an ID of a pre-defined ID type
        if ((dcpl = H5Pcreate(H5P_DATASET_XFER)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // we can reliably predict the ID type
        assert(H5Iget_type(dcpl) == H5I_GENPROP_LST);
        printf("%d\n", H5Iget_type(dcpl));

        H5Pclose(dcpl);
fail_dcpl:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_dcpl;
        hid_t dcpl;

        // create an ID of a pre-defined ID type
        if ((dcpl = H5Pcreate(H5P_DATASET_XFER)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // this better be valid
        assert(H5Iis_valid(dcpl) > 0);

        // this ID is NOT associated with any HDF5 file;
        // see the error message
        assert(H5Iget_file_id(dcpl) == H5I_INVALID_HID);

        H5Pclose(dcpl);
fail_dcpl:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_rc, fail_dcpl;
        hid_t dcpl;
        int   rc;

        // create an ID of a pre-defined ID type
        if ((dcpl = H5Pcreate(H5P_DATASET_XFER)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // retrieve the IDs reference count
        if ((rc = H5Iget_ref(dcpl)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_rc;
        }
        printf("Reference count: %d\n", rc);

        // bump its reference count (by 1)
        if ((rc = H5Iinc_ref(dcpl)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_rc;
        }
        printf("Reference count: %d\n", rc);

fail_rc:
        H5Pclose(dcpl);
fail_dcpl:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_rc, fail_dcpl;
        hid_t dcpl;
        int   rc;

        // create an ID of a pre-defined ID type
        if ((dcpl = H5Pcreate(H5P_DATASET_XFER)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // decrease its reference count (from 1) to 0
        if ((rc = H5Idec_ref(dcpl)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_rc;
        }
        printf("Reference count: %d\n", rc);

        // at this point, the ID is no longer valid
        assert(H5Iis_valid(dcpl) == 0);

        // dropping the ref. count to 0 has the side-effect of closing
        // the associated item, and calling H5Pclose would create an error
        goto fail_dcpl;

fail_rc:
        H5Pclose(dcpl);
fail_dcpl:;
    }
    //! <!-- [delete] -->

    //! <!-- [create_ud] -->
    herr_t free_func(void *obj)
    {
        printf("Calling free_func...\n");
        H5free_memory(obj);
        return 0;
    }

    {
        __label__ fail_id, fail_obj, fail_register;
        H5I_type_t type;
        int       *obj;
        hid_t      obj_id;

        // register a new ID type
        if ((type = H5Iregister_type(128, 1024, &free_func)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_register;
        }
        printf("New identifier type ID: %d\n", type);

        // create a new object
        if ((obj = H5allocate_memory(10 * sizeof(int), 0)) == NULL) {
            ret_val = EXIT_FAILURE;
            goto fail_obj;
        }

        // obtain an ID for the new object
        if ((obj_id = H5Iregister(type, obj)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_id;
        }
        printf("New object identifier: %ld\n", obj_id);

fail_id:
        H5Iclear_type(type, 1);
fail_obj:
        H5Idestroy_type(type);
fail_register:;
    }
    //! <!-- [create_ud] -->

    //! <!-- [read_ud] -->
    {
        __label__ fail_query, fail_register;
        H5I_type_t type;
        hsize_t    count;

        // register a new ID type
        if ((type = H5Iregister_type(128, 1024, NULL)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_register;
        }
        printf("New FOO identifier type ID: %d\n", type);

        // return the number of identifiers of TYPE currently in use
        if (H5Inmembers(type, &count) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_query;
        }
        printf("%llu FOO identifiers in use\n", count);

fail_query:
        H5Idestroy_type(type);
fail_register:;
    }
    //! <!-- [read_ud] -->

    //! <!-- [update_ud] -->
    {
        __label__ fail_id, fail_register;
        H5I_type_t type;
        int        obj = 42;
        hid_t      obj_id;

        // register a new ID type
        if ((type = H5Iregister_type(128, 1024, NULL)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_register;
        }
        printf("New identifier type ID: %d\n", type);

        // obtain an ID for the new object
        if ((obj_id = H5Iregister(type, &obj)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_id;
        }
        printf("New object identifier: %ld\n", obj_id);

        // bump the reference count
        assert(H5Iinc_ref(obj_id) == 2);

        // force the deletion of all IDs regardless of reference count
        H5Iclear_type(type, 1);
fail_id:
        H5Idestroy_type(type);
fail_register:;
    }
    //! <!-- [update_ud] -->

    //! <!-- [delete_ud] -->
    {
        __label__ fail_register;
        H5I_type_t type;

        // register a new ID type
        if ((type = H5Iregister_type(128, 1024, NULL)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_register;
        }
        printf("New identifier type ID: %d\n", type);

        // decrementing the identifier type's ref. count to 0 triggers
        // the type to be destroyed
        assert(H5Idec_type_ref(type) == 0);

fail_register:;
    }
    //! <!-- [delete_ud] -->

    return ret_val;
}
