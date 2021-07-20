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
        __label__ fail_insert, fail_dtype, fail_file;
        hid_t file, dtype;

        if ((file = H5Fcreate("t1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        // create a compound datatype with room for real and imaginary parts
        if ((dtype = H5Tcreate(H5T_COMPOUND, 2 * sizeof(double))) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dtype;
        }
        // add the real part now and the imaginary part later
        if (H5Tinsert(dtype, "re", 0, H5T_IEEE_F64LE) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_insert;
        }
        // commit the datatype definition to the file
        if (H5Tcommit(file, "pre-complex", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
        }

fail_insert:
        H5Tclose(dtype);
fail_dtype:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_dtype, fail_file;
        hid_t file, dtype;

        if ((file = H5Fopen("t1.h5", H5F_ACC_RDONLY, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        // open the datatype object stored in the file
        if ((dtype = H5Topen(file, "pre-complex", H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dtype;
        }

        switch (H5Tget_class(dtype)) { // this time we are only interested in compounds
            case H5T_COMPOUND:
                printf("Record size: %lu bytes\n", H5Tget_size(dtype));
                printf("Record has %d field(s).\n", H5Tget_nmembers(dtype));
                break;
            default:
                break;
        }

        H5Tclose(dtype);
fail_dtype:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_insert, fail_clone, fail_dtype, fail_file;
        hid_t file, dtype, clone;

        if ((file = H5Fopen("t1.h5", H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        // open the datatype object stored in the file
        if ((dtype = H5Topen(file, "pre-complex", H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dtype;
        }
        // the original datatype object is immutable and we need to clone it
        if ((clone = H5Tcopy(dtype)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_clone;
        }
        // remember that the original has enough room for real and imaginary parts
        // add the imaginary part
        if (H5Tinsert(clone, "im", sizeof(double), H5T_IEEE_F64LE) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_insert;
        }
        // commit the "updated" datatype definition to the file
        if (H5Tcommit(file, "complex", clone, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
        }

fail_insert:
        H5Tclose(clone);
fail_clone:
        H5Tclose(dtype);
fail_dtype:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_file;
        hid_t file;

        if ((file = H5Fopen("t1.h5", H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        // delete the "pre-complex" datatype by unlinking
        if (H5Ldelete(file, "pre-complex", H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
        }

        H5Fclose(file);
fail_file:;
    }
    //! <!-- [delete] -->

    return ret_val;
}
