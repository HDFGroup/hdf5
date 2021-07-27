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

        // every property list is created as an instance of a suitable
        // property list class
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // ...

        H5Pclose(dcpl);
fail_dcpl:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_dcpl, fail_plist_cls_id;
        hid_t dcpl, plist_cls_id;

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }
        // to perform introspection on any kind of property list,
        // we need to determine its property list class by obtaining a handle
        if ((plist_cls_id = H5Pget_class(dcpl)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_plist_cls_id;
        }
        // Compare the handle to the handles of built-in or user-defined
        // property list classes
        assert(H5Pequal(plist_cls_id, H5P_DATASET_CREATE) > 0);

fail_plist_cls_id:
        H5Pclose(dcpl);
fail_dcpl:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_dcpl, fail_prop;
        hid_t dcpl;

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // a property list is updated by adding (or removing) properties
        if (H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }

fail_prop:
        H5Pclose(dcpl);
fail_dcpl:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_dcpl;
        hid_t dcpl;

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // a property list is "deleted" by closing it
        H5Pclose(dcpl);
fail_dcpl:;
    }
    //! <!-- [delete] -->

    //! <!-- [create_class] -->
    {
        __label__ fail_cls, fail_prop;
        hid_t  plist_cls, plist;
        int    izero = 0;
        double dzero = 0.0;

        // create a new property list class
        if ((plist_cls = H5Pcreate_class(H5P_ROOT, "int & double", NULL, NULL, NULL, NULL, NULL, NULL)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_cls;
        }
        // add a permanent integer property
        if (H5Pregister2(plist_cls, "int", sizeof(int), &izero, NULL, NULL, NULL, NULL, NULL, NULL, NULL) <
            0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }
        // add a permanent double property
        if (H5Pregister2(plist_cls, "double", sizeof(double), &dzero, NULL, NULL, NULL, NULL, NULL, NULL,
                         NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }
        // create an instance of this user-defined property list class
        if ((plist = H5Pcreate(plist_cls)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }

        // ...

        H5Pclose(plist);
fail_prop:
        H5Pclose_class(plist_cls);
fail_cls:;
    }
    //! <!-- [create_class] -->

    //! <!-- [read_class] -->
    {
        __label__ fail_cls, fail_prop, fail_plist;
        hid_t  plist_cls, plist;
        size_t nprops;

        if ((plist_cls = H5Pcreate_class(H5P_ROOT, "ud_plist", NULL, NULL, NULL, NULL, NULL, NULL)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_cls;
        }
        if ((plist = H5Pcreate(plist_cls)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }
        if (H5Pinsert2(plist, "temp", 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_plist;
        }
        // count the registered properties of this class
        if (H5Pget_nprops(plist_cls, &nprops) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_plist;
        }
        // this will be 0 as there are no registered properties
        printf("Property list class has %lu registered properties.\n", nprops);

        // count the properties in this property list
        if (H5Pget_nprops(plist, &nprops) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_plist;
        }
        // will be 1 as there is one temporary property
        printf("Property list has %lu property.\n", nprops);

fail_plist:
        H5Pclose(plist);
fail_prop:
        H5Pclose_class(plist_cls);
fail_cls:;
    }
    //! <!-- [read_class] -->

    //! <!-- [update_class] -->
    {
        __label__ fail_cls, fail_prop, fail_plist;
        hid_t plist_cls, plist;

        if ((plist_cls = H5Pcreate_class(H5P_ROOT, "ud_plist", NULL, NULL, NULL, NULL, NULL, NULL)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_cls;
        }
        // create an instance of this user-defined property list class
        if ((plist = H5Pcreate(plist_cls)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }
        // insert a temporary property
        if (H5Pinsert2(plist, "temp", 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_plist;
        }

fail_plist:
        H5Pclose(plist);
fail_prop:
        H5Pclose_class(plist_cls);
fail_cls:;
    }
    //! <!-- [update_class] -->

    //! <!-- [delete_class] -->
    {
        __label__ fail_cls;
        hid_t plist_cls;

        if ((plist_cls = H5Pcreate_class(H5P_ROOT, "int & double", NULL, NULL, NULL, NULL, NULL, NULL)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_cls;
        }
        // ...

        // a user defined property list class is destroyed by closing the handle
        H5Pclose_class(plist_cls);
fail_cls:;
    }
    //! <!-- [delete_class] -->

    return ret_val;
}
