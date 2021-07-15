/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [set_libver_bounds] -->
    {
        __label__ fail_fapl, fail_file;
        hid_t fapl, file, aspace, attr;

        if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_fapl;
        }
#if H5_VERSION_GE(1, 10, 0)
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_V18) < 0) {
#elif H5_VERSION_GE(1, 8, 0)
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
#else
#error Only HDF5 1.8.x and later supported.
#endif
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((file = H5Fcreate("set_libver_bounds.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        H5Fclose(file);
fail_file:
        H5Pclose(fapl);
fail_fapl:;
    }
    //! <!-- [set_libver_bounds] -->

    assert(ret_val == EXIT_SUCCESS);

    return ret_val;
}
