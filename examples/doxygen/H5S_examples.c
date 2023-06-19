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
        __label__ fail_dspace;
        hid_t dspace;

        // create a 2D chess board shape (8x8)
        if ((dspace = H5Screate_simple(2, (hsize_t[]){8, 8}, NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dspace;
        }

        H5Sclose(dspace);
fail_dspace:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_dspace;
        hid_t dspace;

        if ((dspace = H5Screate(H5S_NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dspace;
        }

        // make changes to the selection on DSPACE
        // ...
        // parse the resulting selection

        switch (H5Sget_select_type(dspace)) {
            case H5S_SEL_HYPERSLABS:
                // we are dealing with a hyperslab selection
                // determine the regularity and block structure
                break;
            case H5S_SEL_POINTS:
                // we are dealing with a point selection
                // for example, retrieve the point list
                break;
            case H5S_SEL_ALL:
                // all dataset elements are selected
                break;
            case H5S_SEL_NONE:
                // the selection is empty
                break;
            default:
                ret_val = EXIT_FAILURE;
                break;
        }

        if (dspace != H5S_ALL)
            H5Sclose(dspace);

fail_dspace:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_select, fail_dspace;
        hid_t dspace;

        // create a 2D chess board shape (8x8)
        if ((dspace = H5Screate_simple(2, (hsize_t[]){8, 8}, NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dspace;
        }
        // select all black fields: do this w/ a hyperslab union in two steps

        // select the black fields on even rows
        if (H5Sselect_hyperslab(dspace, H5S_SELECT_SET, (hsize_t[]){0, 0}, (hsize_t[]){2, 2},
                                (hsize_t[]){4, 4}, NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_select;
        }
        // select the black fields on odd rows
        // notice the H5S_SELECT_OR operator
        if (H5Sselect_hyperslab(dspace, H5S_SELECT_OR, (hsize_t[]){1, 1}, (hsize_t[]){2, 2},
                                (hsize_t[]){4, 4}, NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_select;
        }

        // should be 32
        printf("%lld elements selected.\n", H5Sget_select_npoints(dspace));

fail_select:
        H5Sclose(dspace);
fail_dspace:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_select, fail_dspace;
        hid_t dspace;

        if ((dspace = H5Screate(H5S_NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dspace;
        }

        // make changes to and work with the selection on DSPACE
        // ...
        // finally, clear the selection

        if (H5Sselect_none(dspace) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_select;
        }

fail_select:
        if (dspace != H5S_ALL)
            H5Sclose(dspace);
fail_dspace:;
    }
    //! <!-- [delete] -->

    return ret_val;
}
