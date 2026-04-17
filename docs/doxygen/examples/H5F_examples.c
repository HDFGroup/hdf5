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
        __label__ fail_fapl, fail_fcpl, fail_file;
        hid_t fcpl, fapl, file;

        if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fcpl;
        }
        else {
            // adjust the file creation properties
        }

        if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fapl;
        }
        else {
            // adjust the file access properties
        }

        unsigned mode   = H5F_ACC_EXCL;
        char     name[] = "f1.h5";

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
fail_fcpl:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_fapl, fail_file;
        hid_t   fapl, file;
        hsize_t size;

        if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fapl;
        }
        else {
            // adjust the file access properties
        }

        unsigned mode   = H5F_ACC_RDONLY;
        char     name[] = "f1.h5";

        if ((file = H5Fopen(name, mode, fapl)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        if (H5Fget_filesize(file, &size) < 0) {
            ret_val = EXIT_FAILURE;
        }

        printf("File size: %llu bytes\n", size);

        H5Fclose(file);
fail_file:
        H5Pclose(fapl);
fail_fapl:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_file;
        hid_t file;

        unsigned mode   = H5F_ACC_RDWR;
        char     name[] = "f1.h5";

        if ((file = H5Fopen(name, mode, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // create a cycle by hard linking the root group in the root group
        if (H5Lcreate_hard(file, ".", file, "loopback", H5P_DEFAULT, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
        }

        H5Fclose(file);
fail_file:;
    }
    //! <!-- [update] -->

    //! <!-- [minimal] -->
    {
        unsigned mode   = H5F_ACC_TRUNC;
        char     name[] = "f11.h5";

        hid_t file = H5Fcreate(name, mode, H5P_DEFAULT, H5P_DEFAULT);
        if (file != H5I_INVALID_HID)
            H5Fclose(file);
        else
            ret_val = EXIT_FAILURE;
    }
    //! <!-- [minimal] -->

    //! <!-- [open] -->
    {
        unsigned mode   = H5F_ACC_RDONLY;
        char     name[] = "f11.h5";

        hid_t file = H5Fopen(name, mode, H5P_DEFAULT);
        if (file != H5I_INVALID_HID)
            H5Fclose(file);
        else
            ret_val = EXIT_FAILURE;
    }
    //! <!-- [open] -->

    //! <!-- [flush] -->
    {
        unsigned mode   = H5F_ACC_RDWR;
        char     name[] = "f11.h5";

        hid_t file = H5Fopen(name, mode, H5P_DEFAULT);
        if (file != H5I_INVALID_HID) {
            int step;
            for (step = 0; step < 1000; ++step) {

                // do important work & flush every 20 steps

                if (step % 20 == 0) {
                    if (H5Fflush(file, H5F_SCOPE_LOCAL) < 0) {
                        perror("H5Fflush failed.");
                        ret_val = EXIT_FAILURE;
                        break;
                    }
                }
            }

            if (H5Fclose(file) < 0)
                perror("H5Fclose failed.");
        }
        else
            ret_val = EXIT_FAILURE;
    }
    //! <!-- [flush] -->

    //! <!-- [libver_bounds] -->
    {
        unsigned mode   = H5F_ACC_RDWR;
        char     name[] = "f11.h5";

        hid_t file = H5Fopen(name, mode, H5P_DEFAULT);
        if (file != H5I_INVALID_HID) {
            if (H5Fset_libver_bounds(file, H5F_LIBVER_EARLIEST, H5F_LIBVER_V18) >= 0) {

                // object creation will not exceed HDF5 version 1.8.x
            }
            else
                perror("H5Fset_libver_bounds failed.");

            if (H5Fclose(file) < 0)
                perror("H5Fclose failed.");
        }
        else
            ret_val = EXIT_FAILURE;
    }
    //! <!-- [libver_bounds] -->

    //! <!-- [mount] -->
    {
        hid_t file = H5Fopen("f11.h5", H5F_ACC_RDWR, H5P_DEFAULT);
        if (file != H5I_INVALID_HID) {
            hid_t group, child;
            if ((group = H5Gcreate1(file, "mount_point", H5P_DEFAULT)) != H5I_INVALID_HID) {
                if ((child = H5Fopen("f1.h5", H5F_ACC_RDONLY, H5P_DEFAULT)) != H5I_INVALID_HID) {
                    if (H5Fmount(group, ".", child, H5P_DEFAULT) >= 0) {

                        // do something useful w/ the mounted file
                    }
                    else {
                        ret_val = EXIT_FAILURE;
                        perror("H5Fmount failed.");
                    }
                    H5Fclose(child);
                }
                H5Gclose(group);
            }
            H5Fclose(file);
        }
        else
            ret_val = EXIT_FAILURE;
    }
    //! <!-- [mount] -->

    //! <!-- [delete] -->
    {
#if H5_VERSION_GE(1, 12, 0)

        // this function is only available in HDF5 1.12.x
        if (H5Fdelete("f1.h5", H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
        }

#endif
    }
    //! <!-- [delete] -->

    return ret_val;
}
