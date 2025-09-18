/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

//! <!-- [closing_shop] -->
void
closing_shop(void *ctx)
{
    printf("GoodBye, Cruel World!\n");
}
//! <!-- [closing_shop] -->

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        // an HDF5 library instance is automatically initialized as
        // part of the first HDF5 API call, but there's no harm in
        // calling H5open().
        if (H5open() < 0) {
            ret_val = EXIT_FAILURE;
        }
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_read;
        unsigned majnum, minnum, relnum;
        hbool_t  flag;

        // retrieve the library version
        if (H5get_libversion(&majnum, &minnum, &relnum) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_read;
        }
        // is this a thread-safe library build?
        if (H5is_library_threadsafe(&flag) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_read;
        }

        printf("Welcome to HDF5 %d.%d.%d\n", majnum, minnum, relnum);
        printf("Thread-safety %s\n", (flag > 0) ? "enabled" : "disabled");

fail_read:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // update the library instance free list limits
        if (H5set_free_list_limits(512 * 1024, 32 * 1024, 2048 * 1024, 128 * 1024, 8192 * 1024, 512 * 1024) <
            0) {
            ret_val = EXIT_FAILURE;
        }
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
#if H5_VERSION_GE(1, 13, 0)
        // install a finalization routine
        if (H5atclose(&closing_shop, NULL) < 0) {
            ret_val = EXIT_FAILURE;
        }
#endif
        // close shop
        if (H5close() < 0) {
            ret_val = EXIT_FAILURE;
        }
    }
    //! <!-- [delete] -->

    assert(ret_val == EXIT_SUCCESS);

    return ret_val;
}
