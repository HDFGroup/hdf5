#include "h5test.h"

int
main(int argc, char **argv)
{
    H5R_ref_t write_ref, read_ref;
    hid_t     fid;
    hid_t     did;
    hid_t     sid;
    int       i;

    if ((fid = H5Fcreate("HDFFV-10992.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        fprintf(stderr, "H5Fcreate failed\n");
        return 1;
    }

    if ((sid = H5Screate(H5S_SCALAR)) < 0) {
        fprintf(stderr, "H5Screate failed\n");
        return 1;
    }

    /* Create a dataset of object references */
    if ((did = H5Dcreate2(fid, "dset", H5T_STD_REF, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        fprintf(stderr, "H5Dcreate failed\n");
        return 1;
    }

    for (i = 0; i < 100; i++) {
        /* Create reference to the root group */
        if (H5Rcreate_object(fid, "/dset", H5P_DEFAULT, &write_ref) < 0) {
            fprintf(stderr, "H5Rcreate_object failed\n");
            return 1;
        }
    }

    /* Write reference to dataset */
    if (H5Dwrite(did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, &write_ref) < 0) {
        fprintf(stderr, "H5Dwrite failed\n");
        return 1;
    }

    for (i = 0; i < 500; i++) {
        /* Read reference back into different reference buffer */
        if (H5Dread(did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, &read_ref) < 0) {
            fprintf(stderr, "H5Dread failed\n");
            return 1;
        }
    }

    /*
     * "Forget" to call H5Rdestroy on reference objects. If H5Rdestroy
     * is called at least once on either reference object, or both
     * objects, the infinite loop goes away. If H5Rdestroy is never
     * called, the infinite loop will appear.
     */
#if 0
    if (H5Rdestroy(&write_ref) < 0) {
        fprintf(stderr, "H5Rdestroy on reference write buffer failed\n");
        return 1;
    }
    if (H5Rdestroy(&read_ref) < 0) {
        fprintf(stderr, "H5Rdestroy on reference read buffer failed\n");
        return 1;
    }
#endif

    if (H5Sclose(sid) < 0) {
        fprintf(stderr, "H5Sclose failed\n");
        return 1;
    }

    if (H5Dclose(did) < 0) {
        fprintf(stderr, "H5Dclose failed\n");
        return 1;
    }

    if (H5Fclose(fid) < 0) {
        fprintf(stderr, "H5Fclose failed\n");
        return 1;
    }

    return 0;
}
