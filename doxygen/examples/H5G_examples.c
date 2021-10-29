/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_group, fail_prop, fail_lcpl, fail_file;
        hid_t file, lcpl, group;
        char  fname[] = "g1.h5";
        char  path[]  = "/αυτή/είναι/μια/νέα/ομάδα";

        if ((file = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        if ((lcpl = H5Pcreate(H5P_LINK_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_lcpl;
        }
        // ensure that intermediate groups are created automatically
        if (H5Pset_create_intermediate_group(lcpl, 1) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }
        // use UTF-8 encoding for link names
        if (H5Pset_char_encoding(lcpl, H5T_CSET_UTF8) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }
        // create five groups
        if ((group = H5Gcreate(file, path, lcpl, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_group;
        }

        H5Gclose(group);
fail_group:
fail_prop:
        H5Pclose(lcpl);
fail_lcpl:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_file;
        char       fname[] = "g1.h5";
        char       path[]  = "/αυτή/είναι";
        hid_t      file;
        H5G_info_t info;

        if ((file = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        // open one of the intermediate groups
        if (H5Gget_info_by_name(file, path, &info, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_info;
        }

        printf("Link count: %llu\n", info.nlinks);
        switch (info.storage_type) {
            case H5G_STORAGE_TYPE_COMPACT:
                printf("Compact storage\n");
                break;
            case H5G_STORAGE_TYPE_DENSE:
                printf("Compact storage\n");
                break;
            case H5G_STORAGE_TYPE_SYMBOL_TABLE:
                printf("Symbol table\n");
                break;
            default:
                printf("UFO\n");
                break;
        }

fail_info:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_group, fail_prop, fail_lcpl, fail_file;
        hid_t file, lcpl, group;
        char  fname[] = "g1.h5";
        char  path[]  = "/αυτή/είναι/μια/άλλη/νέα/ομάδα";

        if ((file = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        if ((lcpl = H5Pcreate(H5P_LINK_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_lcpl;
        }
        // ensure that intermediate groups are created automatically
        if (H5Pset_create_intermediate_group(lcpl, 1) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }
        // use UTF-8 encoding for link names
        if (H5Pset_char_encoding(lcpl, H5T_CSET_UTF8) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_prop;
        }
        // create an anonymous group
        if ((group = H5Gcreate_anon(file, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_group;
        }
        // link the new group to existing the group at "/αυτή/είναι/μια"
        if (H5Lcreate_hard(group, ".", file, path, lcpl, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
        }

        H5Gclose(group);
fail_group:
fail_prop:
        H5Pclose(lcpl);
fail_lcpl:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_info, fail_object, fail_file;
        hid_t      file, obj;
        char       fname[]       = "g1.h5";
        char       path[]        = "/αυτή/είναι/μια/άλλη/νέα/ομάδα";
        char       delete_path[] = "/αυτή/είναι/μια";
        H5O_info_t info;

        if ((file = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // open a "leaf" group as object
        if ((obj = H5Oopen(file, path, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_object;
        }
        // delete the link to an intermediate group on the path to the leaf
        if (H5Ldelete(file, delete_path, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
        }
        // link deletion will propagate along hard links along all paths
        // reachable from the intermediate group and cause reference counts to
        // be decremented, freeing the objects if the count reaches 0
        if (H5Oget_info(obj, &info, H5O_INFO_BASIC) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_info;
        }

        printf("Leaf reference count: %d\n", info.rc);

fail_info:
        H5Oclose(obj);
fail_object:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [delete] -->

    assert(ret_val == EXIT_SUCCESS);

    return ret_val;
}
