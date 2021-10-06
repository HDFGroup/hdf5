/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [free_space] -->
    {
        __label__ fail_fcpl, fail_fapl, fail_file;
        hid_t fcpl, fapl, file;

        if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_fcpl;
        }
#if H5_VERSION_GE(1, 10, 1)
        if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, 1, 4096) < 0) {
#else
#error HDF5 1.10.1+ required
#endif
            ret_val = EXIT_FAILURE;
            goto fail_fapl;
        }

        if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_fapl;
        }
#if H5_VERSION_GE(1, 10, 1)
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_V110, H5F_LIBVER_LATEST) < 0) {
#else
#error HDF5 1.10.x+ required
#endif
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        if ((file = H5Fcreate("free_space.h5", H5F_ACC_TRUNC, fcpl, fapl)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        H5Fclose(file);

fail_file:
        H5Pclose(fapl);
fail_fapl:
        H5Pclose(fcpl);
fail_fcpl:;
    }
    //! <!-- [free_space] -->

    //! <!-- [user_block] -->
    {
        __label__ fail_fcpl, fail_file;
        hid_t fcpl, file;

        if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_fcpl;
        }
        if (H5Pset_userblock(fcpl, 8192 * 1024) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((file = H5Fcreate("userblock.h5", H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        H5Fclose(file);

fail_file:
        H5Pclose(fcpl);
fail_fcpl:;
    }
    //! <!-- [user_block] -->

    assert(ret_val == EXIT_SUCCESS);

    return ret_val;
}
