/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "hdf5_hl.h"

#define WIDTH       400
#define HEIGHT      200
#define PAL_ENTRIES 9
unsigned char buf[WIDTH * HEIGHT];

int
main(void)
{
    hid_t         file_id;
    hsize_t       pal_dims[] = {PAL_ENTRIES, 3};
    size_t        i, j;
    int           n, space;
    unsigned char pal[PAL_ENTRIES * 3] = {               /* create a palette with 9 colors */
                                          0,   0,   168, /* dark blue */
                                          0,   0,   252, /* blue */
                                          0,   168, 252, /* ocean blue */
                                          84,  252, 252, /* light blue */
                                          168, 252, 168, /* light green */
                                          0,   252, 168, /* green */
                                          252, 252, 84,  /* yellow */
                                          252, 168, 0,   /* orange */
                                          252, 0,   0};  /* red */

    /* create an image of 9 values divided evenly by the array */
    space = WIDTH * HEIGHT / PAL_ENTRIES;
    for (i = 0, j = 0, n = 0; i < WIDTH * HEIGHT; i++, j++) {
        buf[i] = n;
        if (j > space) {
            n++;
            j = 0;
        }
        if (n > PAL_ENTRIES - 1)
            n = 0;
    }

    /* create a new HDF5 file using default properties. */
    file_id = H5Fcreate("ex_image1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* make the image */
    H5IMmake_image_8bit(file_id, "image1", (hsize_t)WIDTH, (hsize_t)HEIGHT, buf);

    /* make a palette */
    H5IMmake_palette(file_id, "pallete", pal_dims, pal);

    /* attach the palette to the image */
    H5IMlink_palette(file_id, "image1", "pallete");

    /* close the file. */
    H5Fclose(file_id);

    return 0;
}
