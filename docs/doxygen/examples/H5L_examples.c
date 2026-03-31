/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

//! <!-- [iter_cb] -->
herr_t
iter_cb(hid_t group, const char *name, const H5L_info_t *info, void *op_data)
{
    printf("Link \"%s\" is a", name);
    switch (info->type) {
        case H5L_TYPE_HARD:
            printf(" hard link.\n");
            break;
        case H5L_TYPE_SOFT:
            printf(" soft link.\n");
            break;
        case H5L_TYPE_EXTERNAL:
            printf("n external link.\n");
            break;
        default:
            printf(" UFO link.\n");
            break;
    }

    return 0;
}
//! <!-- [iter_cb] -->

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_link, fail_prop, fail_lcpl, fail_create;

        hid_t file, lcpl;

        if ((file = H5Fcreate("l1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_create;
        }

        // make link creation easier by auto-creating intermediate
        // groups and UTF-8 encoding of link names
        if ((lcpl = H5Pcreate(H5P_LINK_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_lcpl;
        }
        if (H5Pset_create_intermediate_group(lcpl, 1) < 0 || H5Pset_char_encoding(lcpl, H5T_CSET_UTF8) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }

        // create a loop by hard linking the root group
        if (H5Lcreate_hard(file, ".", file, "√", lcpl, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_link;
        }
        // create a soft link to nowhere
        if (H5Lcreate_soft("e1 62 80 87 04 09 43 ba 02 d3", file, "/path/to/nowhere", lcpl, H5P_DEFAULT) <
            0) {
            ret_val = EXIT_FAILURE;
            goto fail_link;
        }
        // create an external link to nowhere in a non-existent file
        if (H5Lcreate_external("non-existent-file.h5", "???", file, "external", lcpl, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_link;
        }

fail_link:
fail_prop:
        H5Pclose(lcpl);
fail_lcpl:
        H5Fclose(file);
fail_create:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_iterate, fail_read;
        hid_t   file;
        hsize_t idx = 0;

        if ((file = H5Fopen("l1.h5", H5F_ACC_RDONLY, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_read;
        }

        if (H5Literate(file, H5_INDEX_NAME, H5_ITER_NATIVE, &idx, iter_cb, NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_iterate;
        }

fail_iterate:
        H5Fclose(file);
fail_read:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_move, fail_update;
        hid_t file;

        if ((file = H5Fopen("l1.h5", H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_update;
        }

        // move the "√" link to the group at "/path/to"
        // the cycle remains!
        if (H5Lmove(file, "√", file, "path/to/√", H5P_DEFAULT, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_move;
        }

fail_move:
        H5Fclose(file);
fail_update:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_delete, fail_file;
        hid_t file;

        if ((file = H5Fopen("l1.h5", H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // delete the "external" link
        if (H5Ldelete(file, "external", H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_delete;
        }

fail_delete:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [delete] -->

    assert(ret_val == EXIT_SUCCESS);

    return ret_val;
}
