/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

#define H5P_DEFAULTx2 H5P_DEFAULT, H5P_DEFAULT

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_file;
        hid_t file, group;
        char  src_path[] = "/a/few/groups";

        if ((file = H5Fcreate("o1.h5", H5F_ACC_TRUNC, H5P_DEFAULTx2)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // create a few groups
        {
            __label__ fail_group, fail_lcpl;
            hid_t lcpl;
            if ((lcpl = H5Pcreate(H5P_LINK_CREATE)) == H5I_INVALID_HID) {
                ret_val = EXIT_FAILURE;
                goto fail_lcpl;
            }
            if (H5Pset_create_intermediate_group(lcpl, 1) < 0) {
                ret_val = EXIT_FAILURE;
                goto fail_group;
            }
            if ((group = H5Gcreate(file, src_path, lcpl, H5P_DEFAULTx2)) == H5I_INVALID_HID) {
                ret_val = EXIT_FAILURE;
                goto fail_group;
            }

            H5Gclose(group);
fail_group:
            H5Pclose(lcpl);
fail_lcpl:;
        }

        // create a copy
        if (H5Ocopy(file, ".", file, "copy of", H5P_DEFAULTx2) < 0) {
            ret_val = EXIT_FAILURE;
        }

        H5Fclose(file);
fail_file:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_info, fail_file;
        hid_t       file;
        char        path[] = "/a/few/groups";
        H5O_info2_t info;

        if ((file = H5Fopen("o1.h5", H5F_ACC_RDONLY, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // retrieve information about the object
        if (H5Oget_info_by_name(file, path, &info, H5O_INFO_BASIC | H5O_INFO_NUM_ATTRS, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_info;
        }

        // determine the object type
        switch (info.type) {
            case H5O_TYPE_GROUP:
                printf("HDF5 group\n");
                break;
            case H5O_TYPE_DATASET:
                printf("HDF5 dataset\n");
                break;
            case H5O_TYPE_NAMED_DATATYPE:
                printf("HDF5 datatype\n");
                break;
            default:
                printf("UFO?\n");
                break;
        }
        // print basic information
        printf("Reference count: %u\n", info.rc);
        printf("Attribute count: %lld\n", info.num_attrs);

fail_info:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_obj, fail_incr, fail_file;
        hid_t       file, obj;
        char        path[] = "/a/few/groups";
        H5O_info2_t info;

        if ((file = H5Fopen("o1.h5", H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // open an object by path name
        if ((obj = H5Oopen(file, path, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_obj;
        }

        // bump its reference count (by 1)
        if (H5Oincr_refcount(obj) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_incr;
        }

        // confirm the new reference count
        if (H5Oget_info(obj, &info, H5O_INFO_BASIC) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_incr;
        }
        printf("Reference count: %u\n", info.rc);

fail_incr:
        H5Oclose(obj);
fail_obj:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_obj, fail_delete, fail_file;
        hid_t       file, obj;
        char        path[] = "/a/few/groups";
        H5O_info2_t info;

        if ((file = H5Fopen("o1.h5", H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // open an object by path name
        if ((obj = H5Oopen(file, path, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_obj;
        }

        // render it inaccessible from the root group by deleting the one and
        // only link to it; this drops the reference count by 1
        if (H5Ldelete(file, path, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_delete;
        }

        // confirm the new reference count
        if (H5Oget_info(obj, &info, H5O_INFO_BASIC) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_delete;
        }
        printf("Reference count: %u\n", info.rc);

        // if we close the file at this point, we'd be creating a tombstone,
        // an object that cannot be reached and that cannot be reclaimed by the
        // freespace manager; decrement the reference count to zero to prevent that
        if (H5Idec_ref(obj) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_delete;
        }
        else
            // attempting to close the object would be like a double H5Oclose and fail
            goto fail_obj;

fail_delete:
        H5Oclose(obj);
fail_obj:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [delete] -->

    return ret_val;
}
