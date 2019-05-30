#include "hdf5.h"
#include "H5private.h"

#define MAX_NAME_SIZE 256
#define FILE_INT32LE "h5repack_int32le"


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
static void
generate_external_int32le(hbool_t external)
{
    char    filename[MAX_NAME_SIZE];
    hid_t   file;
    hid_t   dspace;
    hid_t   dset;
    hid_t   dcpl = H5P_DEFAULT;
    hsize_t dims[] = {8, 8, 8};
    int32_t wdata[512]; /* 8^3 */
    int32_t n;
    int     i;
    int     j;
    int     k;

    /* generate values, alternating positive and negative
     */
    for (i=0, n=0; i < 8; i++)
        for (j=0; j < 8; j++)
            for (k=0; k < 8; k++, n++)
                wdata[n] = (k + j*512 + i*4096) * ((n&1) ? (-1) : (1));

    snprintf(filename,
            MAX_NAME_SIZE,
            "%s%s.h5",
            FILE_INT32LE,
            (external) ? "_ex" : "");

    file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(file >= 0);

    dspace = H5Screate_simple(3, dims, NULL);
    HDassert(dspace >= 0);

    if (external) {
        char name[MAX_NAME_SIZE];
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        HDassert(dcpl >= 0);
        snprintf(name, MAX_NAME_SIZE, "%s_ex-0.dat", FILE_INT32LE);
        HDassert(H5Pset_external(dcpl, name, 0, H5F_UNLIMITED) >= 0);
    }

    dset = H5Dcreate2(
            file,
            "dset",
            H5T_STD_I32LE,
            dspace,
            H5P_DEFAULT,
            dcpl,
            H5P_DEFAULT);
    HDassert(dset >= 0);

    HDassert(H5Dwrite(dset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata)
            >= 0);

    if (external)
        HDassert(H5Pclose(dcpl) >= 0);
    HDassert(H5Dclose(dset) >= 0);
    HDassert(H5Sclose(dspace) >= 0);
    HDassert(H5Fclose(file) >= 0);
} /* end generate_external_int32le() */


/* ----------------------------------------------------------------------------
 */
int
main(void)
{
    generate_external_int32le(FALSE);
    generate_external_int32le(TRUE);
    return 0;
} /* end main() */


