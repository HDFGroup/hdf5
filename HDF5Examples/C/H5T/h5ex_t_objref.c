/************************************************************

  This example shows how to read and write object references
  to a dataset.  The program first creates objects in the
  file and writes references to those objects to a dataset
  with a dataspace of DIM0, then closes the file.  Next, it
  reopens the file, dereferences the references, and outputs
  the names of their targets to the screen.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE    "h5ex_t_objref.h5"
#define DATASET "DS1"
#define DIM0    2
#define RANK    1

int
main(void)
{
    hid_t   file  = H5I_INVALID_HID; /* File Handle */
    hid_t   space = H5I_INVALID_HID; /* Dataspace Handle */
    hid_t   dset  = H5I_INVALID_HID; /* Dataset Handle */
    hid_t   obj   = H5I_INVALID_HID; /* Object Handle */
    herr_t  status;
    hsize_t dims[1] = {DIM0};
    ssize_t size;
    char   *name = NULL;
    int     ndims;
    hsize_t i;

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    hid_t      ref_type = H5T_STD_REF; /* Reference datatype */
    H5R_ref_t  wdata[DIM0];            /* buffer to write to disk */
    H5R_ref_t *rdata = NULL;           /* buffer to read into*/
    H5O_type_t objtype;                /* Reference type */
#else
    hid_t       ref_type = H5T_STD_REF_OBJ; /* Reference datatype */
    hobj_ref_t  wdata[DIM0];                /* Write buffer */
    hobj_ref_t *rdata = NULL;               /* Read buffer */
    H5O_type_t  objtype;
#endif

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a dataset with a null dataspace.
     */
    space  = H5Screate(H5S_NULL);
    obj    = H5Dcreate(file, "DS2", H5T_STD_I32LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dclose(obj);
    status = H5Sclose(space);

    /*
     * Create a group.
     */
    obj    = H5Gcreate(file, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Gclose(obj);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple(RANK, dims, NULL);

    /*
     * Create references to the previously created objects.
     */
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    status = H5Rcreate_object(file, "G1", H5R_OBJECT, &wdata[0]);
    status = H5Rcreate_object(file, "DS2", H5R_OBJECT, &wdata[1]);
#else
    /*
     * Passing -1
     * as space_id causes this parameter to be ignored.  Other values
     * besides valid dataspaces result in an error.
     */
    status = H5Rcreate(&wdata[0], file, "G1", H5R_OBJECT, -1);
    status = H5Rcreate(&wdata[1], file, "DS2", H5R_OBJECT, -1);
#endif

    /*
     * Create the dataset and write the object references to it.
     */
    dset   = H5Dcreate(file, DATASET, ref_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(dset, ref_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

    /*
     * Close and release resources.
     */
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    status = H5Rdestroy(&wdata[0]);
    status = H5Rdestroy(&wdata[1]);
#endif
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Fclose(file);

    /*
     * Now we begin the read section of this example.  Here we assume
     * the dataset has the same name and rank, but can have any size.
     * Therefore we must allocate a new array to read in data using
     * malloc().
     */

    /*
     * Open file and dataset.
     */
    file = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen(file, DATASET, H5P_DEFAULT);

    /*
     * Get dataspace and allocate memory for read buffer.
     */
    space = H5Dget_space(dset);
    ndims = H5Sget_simple_extent_dims(space, dims, NULL);

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    rdata = (H5R_ref_t *)malloc(dims[0] * sizeof(H5R_ref_t));
#else
    rdata  = (hobj_ref_t *)malloc(dims[0] * sizeof(hobj_ref_t));
#endif
    /*
     * Read the data.
     */
    status = H5Dread(dset, ref_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

    /*
     * Output the data to the screen.
     */
    for (i = 0; i < dims[0]; i++) {
        printf("%s[%" PRIuHSIZE "]:\n  ->", DATASET, i);

        /*
         * Open the referenced object, get its name and type.
         */
#if H5_VERSION_GE(1, 10, 0) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
        obj    = H5Ropen_object(&rdata[i], H5P_DEFAULT, H5P_DEFAULT);
        status = H5Rget_obj_type3(&rdata[i], H5P_DEFAULT, &objtype);
#else
        obj    = H5Rdereference(dset, H5P_DEFAULT, H5R_OBJECT, &rdata[i]);
        status = H5Rget_obj_type(dset, H5R_OBJECT, &rdata[i], &objtype);
#endif
#else
        obj    = H5Rdereference(dset, H5R_OBJECT, &rdata[i]);
        status = H5Rget_obj_type(dset, H5R_OBJECT, &rdata[i], &objtype);
#endif

        /*
         * Get the length of the name, allocate space, then retrieve
         * the name.
         */
        size = 1 + H5Iget_name(obj, NULL, 0);
        name = (char *)malloc(size);
        size = H5Iget_name(obj, name, size);

        /*
         * Print the object type and close the object.
         */
        switch (objtype) {
            case H5O_TYPE_GROUP:
                printf("Group");
                break;
            case H5O_TYPE_DATASET:
                printf("Dataset");
                break;
            case H5O_TYPE_NAMED_DATATYPE:
                printf("Named Datatype");
                break;
            case H5O_TYPE_MAP:
                printf("Map Object");
                break;
            case H5O_TYPE_UNKNOWN:
            case H5O_TYPE_NTYPES:
                printf("Unknown");
        }
        status = H5Oclose(obj);

        /*
         * Print the name and deallocate space for the name.
         */
        printf(": %s\n", name);
        free(name);

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
        status = H5Rdestroy(&rdata[i]);
#endif
    }

    /*
     * Close and release resources.
     */
    free(rdata);
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Fclose(file);

    return 0;
}
