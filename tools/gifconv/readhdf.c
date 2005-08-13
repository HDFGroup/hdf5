/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#include "gif.h"

/* just a small cleanup routine before we leave */
void
cleanup(BYTE *ptr)
{
    if (ptr)
        free(ptr);
}

/* Function:    ReadHDF
** Return:      0 on completion without error, -1 on error
** Input:       CHAR *h5_file - HDF file name
**              CHAR *dset_name - Name of the HDF Image dataset
**              CHAR *pal_name - Name of the HDF palette
** Output:      BYTE** data - the HDF Image to be converted
**              BYTE  palette[256][3] - the corresponding palette
**              hsize_t* image_size - the size of each dimension of the image
**
** Future Notes:
** The way readHDF works right now is that it expects the user
** to know the exact path to the HDF image. Thus it does not
** parse the HDF file looking for image datasets and corresponding
** palettes. Also it takes in the default palette for the image
** specified, if the palette is missing, it makes a default greyscale
** palette and throws it in.
**
*/
int ReadHDF(BYTE** data, BYTE palette[256][3], hsize_t *image_size,
            CHAR *h5_file, CHAR *dset_name, CHAR *pal_name)
{
    hid_t fHfile;       /* H5 file to open                              */
    hid_t dspace;       /* dataspace identifier for the the dataset     */
    hid_t dtype;        /* datatype identifier for the the dataset     */
    hid_t dset;	        /* dataset identifier                           */
    hid_t pal_set;      /* dataset for palette                          */
    hid_t pal_space;    /* dataspace for palette                        */
    hid_t pal_dtype;     /* datatype for palette                        */
    hsize_t datasize;	/* size of the image                            */
    int pal_exist = 0;  /* do we have a palette?                        */

    /* check stuff */
    if (!h5_file || !dset_name || !image_size) {
        fprintf(stderr, "NULL is not an acceptable input for HDFread. Aborting.\n");
        return -1;
    }

    /* do we have a palette ? */
    if (pal_name) {
        pal_exist = 1;
    }

    /* try opening the file */
    /* H5 file open calls */
    if ((fHfile = H5Fopen(h5_file , H5F_ACC_RDONLY , H5P_DEFAULT)) < 0) {
        fprintf(stderr , "Unable to open HDF file for input. Aborting.\n");
        return -1;
    }

    /* open the dataset for reading */
    if ((dset = H5Dopen(fHfile , dset_name)) < 0) {
        fprintf(stderr , "Unable to open dataset\n");
        return -1;
    }

    dtype = H5Dget_type(dset);
    if (dtype < 0) {
        fprintf(stderr , "Unable to open datatype\n");
        return -1;
    }
    if (H5Tget_class(dtype) != H5T_INTEGER) {
        fprintf(stderr , "Data is not integer. Cannot convert to GIF\n");
        return -1;
    }
    if (H5Tget_size(dtype) != 1) {
        fprintf(stderr , "Data is %d bytes per pixel. Cannot convert to GIF\n",(int)H5Tget_size(dtype));
        return -1;
    }

    /* get the dataspace */
    if ((dspace = H5Dget_space(dset)) < 0) {
        fprintf(stderr , "Unable to get dataspace\n");
        return -1;
    }

    /* get the dimension size of the image */
    if (H5Sget_simple_extent_dims(dspace , image_size , NULL) <0 ) {
        fprintf(stderr , "Unable to get dimension info\n");
        return -1;
    }

    /* size needed to store the image */
    datasize = image_size[0] * image_size[1];

    /* allocate memory to store the image */
    if ((*data = (BYTE*) malloc((size_t)datasize)) == NULL) {
        fprintf(stderr , "Out of memory, exiting");
        return -1;
    }

    /* get the actual image */
    if (H5Dread(dset , H5Tget_native_type(dtype, H5T_DIR_ASCEND) , H5S_ALL , H5S_ALL , H5P_DEFAULT , *data) < 0) {
        fprintf(stderr , "Unable to read data \n");
        cleanup(*data);
        return -1;
    }

    if (pal_exist) {
        hsize_t loc_pal_size[2];
        hsize_t pal_datasize;
        BYTE *temp_buf;
        hsize_t temp_size;

        /* get the palette dataset */
        if ((pal_set = H5Dopen(fHfile , pal_name)) < 0) {
            fprintf(stderr , "Unable to open dataset\n");
            pal_exist = 0;
            return -1;
        }

        pal_dtype = H5Dget_type(pal_set);
        if (dtype < 0) {
            fprintf(stderr , "Unable to open palette datatype\n");
            return -1;
        }
        if (H5Tget_class(pal_dtype) != H5T_INTEGER) {
            fprintf(stderr , "Palette data is not integer. Cannot convert to GIF\n");
            return -1;
        }
        if (H5Tget_size(pal_dtype) != 1) {
            fprintf(stderr , "Palette data is %d bytes per pixel. Cannot convert to GIF\n",(int)H5Tget_size(pal_dtype));
            return -1;
        }

        /* get the dataspace */
        if ((pal_space = H5Dget_space(pal_set)) < 0) {
            fprintf(stderr , "Unable to get dataspace\n");
            pal_exist = 0;
            return -1;
        }

        /* get the dimension size of the palette. */
        if (H5Sget_simple_extent_dims(pal_space , loc_pal_size , NULL) !=2 ) {
            fprintf(stderr , "Unable to get dimension info\n");
            pal_exist = 0;
            return -1;
        }

        /* size needed to store the image */
        pal_datasize = loc_pal_size[0] * loc_pal_size[1];

        /* copy stuff into a temp buffer and then copy 256*3 elements to palette */
        temp_size = H5Dget_storage_size(pal_set);
        temp_buf = (BYTE*) malloc ((size_t)temp_size * sizeof(BYTE));

        /*
         * make sure that the palette is actually 256 X 3 so that we don't
         * create overflows
         */
        if (pal_datasize > 256 * 3) {
            fprintf(stderr , "Palette seems to be more than 256X3 bytes\n");
            fprintf(stderr , "Truncating palette to 256 colors. This might cause a problem with the final image\n");
            pal_datasize = 256 * 3;
        }

        /* get the actual palette */
        if (H5Dread(pal_set , H5Tget_native_type(pal_dtype, H5T_DIR_ASCEND) , H5S_ALL , H5S_ALL , H5P_DEFAULT , temp_buf) < 0) {
            fprintf(stderr , "Unable to read data \n");
            cleanup(*data);
            cleanup(temp_buf);
            return -1;
        }

        /* copy stuff into the actual palette */
        memcpy(palette , temp_buf , (size_t)pal_datasize);

        /* get rid of the temp memory */
        cleanup(temp_buf);
        /* end of if (pal_exist) */
    } else {
        int i;
        /*
         * if palette does not exist we just go ahead and create a uniform
         * greyscale palette
         */
        for (i = 0 ; i < 256 ; i++) {
            palette[i][0] = 255 - i;
            palette[i][1] = 255 - i;
            palette[i][2] = 255 - i;
        }
    }

    /* close everything */
    H5Dclose(dset);
    H5Sclose(dspace);
    H5Fclose(fHfile);
    return 0;
}
