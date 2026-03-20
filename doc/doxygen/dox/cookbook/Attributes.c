/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [large_attribute] -->
    {
        __label__ fail_attr, fail_aspace, fail_fapl, fail_file;
        hid_t fapl, file, aspace, attr;

        if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_fapl;
        }
#if H5_VERSION_GE(1, 10, 0)
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_LATEST) < 0) {
#elif H5_VERSION_GE(1, 8, 0)
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
#else
#error Only HDF5 1.8.x and later supported.
#endif
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((file = H5Fcreate("large_attribute.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        if ((aspace = H5Screate_simple(1, (hsize_t[]){1024 * 1024}, NULL)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_aspace;
        }
        if ((attr = H5Acreate(file, "4MiB", H5T_IEEE_F32LE, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_attr;
        }

        H5Aclose(attr);
fail_attr:
        H5Sclose(aspace);
fail_aspace:
        H5Fclose(file);
fail_file:
        H5Pclose(fapl);
fail_fapl:;
    }
    //! <!-- [large_attribute] -->

    assert(ret_val == EXIT_SUCCESS);

    return ret_val;
}
