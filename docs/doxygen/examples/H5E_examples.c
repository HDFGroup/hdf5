/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define RESET "\x1b[0m"
#define RED   "\x1b[31m"

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_push, fail_minor, fail_major, fail_class;
        hid_t cls, major, minor;

        // register a new error class for this "application"
        if ((cls = H5Eregister_class("Custom error class", "H5E_examples", "0.1")) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_class;
        }

        // create custom major and minor error codes
        if ((major = H5Ecreate_msg(cls, H5E_MAJOR, "Okay, Houston, we've had a problem here")) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_major;
        }
        if ((minor = H5Ecreate_msg(cls, H5E_MINOR, "Oops!")) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_minor;
        }

        // push a custom error message onto the default stack
        if (H5Epush2(H5E_DEFAULT, __FILE__, __FUNCTION__, __LINE__, cls, major, minor, "%s Hello, error %s\n",
                     RED, RESET) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_push;
        }

        // print the default error stack
        if (H5Eprint(H5E_DEFAULT, stderr) < 0) {
            ret_val = EXIT_FAILURE;
        }

fail_push:
        H5Eclose_msg(minor);
fail_minor:
        H5Eclose_msg(major);
fail_major:
        H5Eunregister_class(cls);
fail_class:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_count;

        // check the number of error messages on the default stack
        // and print what's there
        ssize_t count = H5Eget_num(H5E_DEFAULT);
        if (count < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_count;
        }
        else if (H5Eprint(H5E_DEFAULT, stderr) < 0) {
            ret_val = EXIT_FAILURE;
        }

fail_count:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // pop 10 error messages off the default error stack
        // popping off non-existent messages is OK, but might be confusing
        if (H5Epop(H5E_DEFAULT, 10) < 0)
            ret_val = EXIT_FAILURE;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        // clear the default error stack (for the current thread)
        if (H5Eclear2(H5E_DEFAULT) < 0)
            ret_val = EXIT_FAILURE;
    }
    //! <!-- [delete] -->

    assert(ret_val == EXIT_SUCCESS);

    return ret_val;
}
