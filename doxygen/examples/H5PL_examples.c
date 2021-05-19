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
        __label__ fail_set;
        // enable ONLY filter plugins
        if (H5PLset_loading_state(H5PL_FILTER_PLUGIN) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_set;
        }

        // ensure that "/tmp" is at the front of the search path list
        if (H5PLprepend("/tmp") < 0) {
            ret_val = EXIT_FAILURE;
        }

fail_set:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_read;
        unsigned size, mask;
        char     buf[255];

        // retrieve the number of entries in the plugin path list
        if (H5PLsize(&size) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_read;
        }
        printf("Number of stored plugin paths: %d\n", size);

        // check the plugin state mask
        if (H5PLget_loading_state(&mask) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_read;
        }
        printf("Filter plugins %s be loaded.\n", (mask & H5PL_FILTER_PLUGIN) == 1 ? "can" : "can't");
        printf("VOL plugins %s be loaded.\n", (mask & H5PL_VOL_PLUGIN) == 2 ? "can" : "can't");

        // print the paths in the plugin path list
        for (unsigned i = 0; i < size; ++i) {
            if (H5PLget(i, buf, 255) < 0) {
                ret_val = EXIT_FAILURE;
                break;
            }
            printf("%s\n", buf);
        }

fail_read:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // replace "/tmp" with something more sensible
        if (H5PLreplace("/foo/bar", 0) < 0) {
            ret_val = EXIT_FAILURE;
        }
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_delete;
        unsigned size;

        if (H5PLsize(&size) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_delete;
        }

        // clean out the list of plugin paths
        for (int i = size - 1; i >= 0; --i) {
            if (H5PLremove(i) < 0) {
                ret_val = EXIT_FAILURE;
                break;
            }
        }

fail_delete:;
    }
    //! <!-- [delete] -->

    return ret_val;
}
