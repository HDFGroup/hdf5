/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_acpl, fail_attr, fail_file;
        hid_t file, acpl, fspace, attr;

        unsigned mode        = H5F_ACC_TRUNC;
        char     file_name[] = "f1.h5";
        // attribute names can be arbitrary Unicode strings
        char attr_name[] = "Χαρακτηριστικό";

        if ((file = H5Fcreate(file_name, mode, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((acpl = H5Pcreate(H5P_ATTRIBUTE_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_acpl;
        }
        // use UTF-8 encoding for the attribute name
        if (H5Pset_char_encoding(acpl, H5T_CSET_UTF8) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_fspace;
        }
        // create a scalar (singleton) attribute
        if ((fspace = H5Screate(H5S_SCALAR)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fspace;
        }
        // create an attribute on the root group
        if ((attr = H5Acreate2(file, attr_name, H5T_STD_I32LE, fspace, acpl, H5P_DEFAULT)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_attr;
        }

        H5Aclose(attr);
fail_attr:
        H5Sclose(fspace);
fail_fspace:
        H5Pclose(acpl);
fail_acpl:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_attr, fail_file;
        hid_t file, attr;

        unsigned mode        = H5F_ACC_RDONLY;
        char     file_name[] = "f1.h5";
        char     attr_name[] = "Χαρακτηριστικό";
        int      value;

        if ((file = H5Fopen(file_name, mode, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((attr = H5Aopen(file, attr_name, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_attr;
        }
        // read the attribute value
        if (H5Aread(attr, H5T_NATIVE_INT, &value) < 0)
            ret_val = EXIT_FAILURE;

        // do something w/ the attribute value

        H5Aclose(attr);
fail_attr:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_attr, fail_file;
        hid_t file, attr;

        unsigned mode        = H5F_ACC_RDWR;
        char     file_name[] = "f1.h5";
        char     attr_name[] = "Χαρακτηριστικό";
        int      value       = 1234;

        if ((file = H5Fopen(file_name, mode, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((attr = H5Aopen(file, attr_name, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_attr;
        }
        // update the attribute value
        if (H5Awrite(attr, H5T_NATIVE_INT, &value) < 0)
            ret_val = EXIT_FAILURE;

        H5Aclose(attr);
fail_attr:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_attr, fail_file;
        hid_t file;

        unsigned mode        = H5F_ACC_RDWR;
        char     file_name[] = "f1.h5";
        char     attr_name[] = "Χαρακτηριστικό";

        if ((file = H5Fopen(file_name, mode, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        // delete the attribute
        if (H5Adelete(file, attr_name) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_attr;
        }

fail_attr:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [delete] -->

    return ret_val;
}
