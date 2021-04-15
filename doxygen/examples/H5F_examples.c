/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [life_cycle] -->
    {
        __label__ fail_fapl, fail_fcpl, fail_file;
        hid_t fcpl, fapl, file;

        if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fcpl;
        } else {
            // adjust the file creation properties
        }

        if((fapl = H5Pcreate(H5P_FILE_ACCESS)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fapl;
        } else {
            // adjust the file access properties
        }

        unsigned mode = H5F_ACC_EXCL;
        char name[] = "f1.h5";

        if ((file = H5Fcreate(name, mode, fcpl, fapl)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // do something useful with FILE

        H5Fclose(file);
    fail_file:
        H5Pclose(fapl);
    fail_fapl:
        H5Pclose(fcpl);
    fail_fcpl: ;
    }
    //! <!-- [life_cycle] -->

    //! <!-- [life_cycle_w_open] -->
    {
        __label__ fail_fapl, fail_file;
        hid_t fapl, file;

        if((fapl = H5Pcreate(H5P_FILE_ACCESS)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fapl;
        } else {
            // adjust the file access properties
        }

        unsigned mode = H5F_ACC_RDWR;
        char name[] = "f1.h5";

        if ((file = H5Fopen(name, mode, fapl)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // do something useful with FILE

        H5Fclose(file);
    fail_file:
        H5Pclose(fapl);
    fail_fapl: ;
    }
    //! <!-- [life_cycle_w_open] -->

    //! <!-- [minimal] -->
    {
        unsigned mode = H5F_ACC_TRUNC;
        char name[] = "f11.h5";

        hid_t file = H5Fcreate(name, mode, H5P_DEFAULT, H5P_DEFAULT);
        if (file != H5I_INVALID_HID) H5Fclose(file);
        else ret_val = EXIT_FAILURE;
    }
    //! <!-- [minimal] -->

    return ret_val;
}
