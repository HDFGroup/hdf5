/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * NOTES:
 * 04/01 - 04/10: been working on it a lot. I think it does gif89 images just
 *                fine with palettes. So that's cool. Putting in multiple
 *                images and animation support right now
 * 03/29: For some reason I can write .GIF files which IE will open and see
 *        but kodak imaging does not like. I'm sure its a problem with the GIF
 *        file, I can't figure out what.
 * 03/17: Explicitely deallocate the GIFTOMEM* struct in the main loop.
 *        Check for both success and failure conditions
 */

#include <stdio.h>
#include <assert.h>
#include "gif.h"

#define MAX_FILE_LEN            256
#define MAX_NUMBER_IMAGES       50

int EndianOrder;

#ifdef NOT_USED
static void
PutByte(BYTE b , FILE *fpGif)
{
    if (fputc(b , fpGif) == EOF) {
        printf("File Writing Error, cannot continue");
        exit(-1);
    }
}
#endif /* NOT_USED */

static void
putword(int w, FILE *fp)
{
    /* writes a 16-bit integer in GIF order (LSB first) */
    fputc(w &0xff, fp);
    fputc((w>>8)&0xff,fp);
}

static void
usage(void)
{
    printf("Usage: h52gif <h5_file> <gif_file> -i <h5_image> [-p <h5_palette>]\n");
    printf("h52gif expects *at least* one h5_image.\n");
    printf("You may repeat -i <h5_image> [-p <h5_palette>] at most 50 times\n");
    printf("(maximum of 50 images).\n");
}

FILE *fpGif = NULL;
int main(int argc , char **argv)
{


    hsize_t dim_sizes[2];
    BYTE *Image;

    /* compression structs */
    CHAR *HDFName = NULL;
    CHAR *GIFName = NULL;

    /* reference variables */
    int has_local_palette;  /* treated as a flag */

    BYTE* b;

    BYTE  GlobalPalette[256][3];
    BYTE  Red[256];
    BYTE  Green[256];
    BYTE  Blue[256];

    int   RWidth, RHeight;
#ifdef UNUSED
    int   LeftOfs, TopOfs;
    int   CountDown;
    int   curx , cury;
    int	  w,h;
#endif /* UNUSED */
    int   ColorMapSize, InitCodeSize, Background, BitsPerPixel;
    int   j,nc;
    int	  i;
    int   numcols = 256;
    int   time_out = 0;		/* time between two images in the animation */
    int   n_images , idx;

    BYTE pc2nc[256] , r1[256] , g1[256] , b1[256];

    /* initial stuff */
    int number_of_images = 0;
    int arg_index = 2;
    int bool_is_image = 0; /* 0 = false , 1 = true */
    int bool_is_palette = 0;
    CHAR* image_name_arr[MAX_NUMBER_IMAGES];
    CHAR* pal_name_arr[MAX_NUMBER_IMAGES];

    if (argc < 5) {
        /* they didn't supply at least one image -- bail */
        usage();
        return 0;
    }

    memset(image_name_arr , 0 , (size_t)MAX_NUMBER_IMAGES);
    memset(pal_name_arr , 0 , (size_t)MAX_NUMBER_IMAGES);

    HDFName = (CHAR *)malloc (strlen(argv[1]) + 1);
    GIFName = (CHAR *)malloc (strlen(argv[2]) + 1);

    if (strlen(argv[1] + 1) > MAX_FILE_LEN || strlen(argv[2] + 1) > MAX_FILE_LEN) {
        /* supplied file names are too long. bail. */
        usage();
        printf("Supplied filenames exceed maximum length of 256 bytes\n");
    }

    strcpy(HDFName , argv[1]);
    strcpy(GIFName , argv[2]);

    /* get the options */
    while (arg_index++ < argc - 1 && number_of_images < MAX_NUMBER_IMAGES) {
        if (!strcmp(argv[arg_index] , "-i")) {
            bool_is_image = 1;
            continue;
        }

        if (!strcmp(argv[arg_index] , "-p")) {
            bool_is_palette = 1;
            continue;
        }

        if (!strcmp(argv[arg_index] , "-a")) {
            time_out = 10;
            continue;
        }

        if (bool_is_image) {
            /* this is an image */
            /* allocate space to store the image name */
            image_name_arr[number_of_images] = (CHAR*) malloc(strlen(argv[arg_index] + 1));
            strcpy(image_name_arr[number_of_images] , argv[arg_index]);

            /* make the palette array for this NULL */
            pal_name_arr[number_of_images] = NULL;
            number_of_images++;
            bool_is_image = 0;
            continue;
        }

        if (bool_is_palette) {
            /* this is a palette */
            /* allocate space to store the pal name */
            /* the palette was probably allocated for a previous image */
            pal_name_arr[number_of_images-1] = (CHAR*) malloc(strlen(argv[arg_index] + 1));
            strcpy(pal_name_arr[number_of_images - 1], argv[arg_index]);
            bool_is_palette = 0;
            continue;
        }

        /* oops. This was not meant to happen */
        usage();

#if 0
        while (number_of_images--) {
            cleanup(image_name_arr[number_of_images]);
            cleanup(pal_name_arr[number_of_images]);
        }
#endif  /* 0 */

        return -1;
    }

    /* we shall always have a palette - read hdf will see to that */
    has_local_palette = true;

    /* Do Endian Order testing and set Endian Order */
    idx = 0x0001;
    b = (BYTE *) &idx;
    EndianOrder = (b[0] ? 1:0);

    if (!(fpGif = fopen(GIFName , "wb"))) {
        printf("Error opening gif file for output. Aborting.\n");
        return -1;
    }

    /* hardwire n_images to 1 for now. */
    n_images = number_of_images;

    Background = 0;
    for (idx = 0 ; idx < n_images ; idx++) {
        /* try to read the image and the palette */

        /*
         * Lots of funky stuff to support multiple images has been taken off.
         * Just in case you're extending code, here's what you need to do in
         * short: figure out if there is a global palette or not, if there is
         * one store that one only.  If they are all local or a combination of
         * local and global palettes, you will have to write the global
         * palette out and then independantly write the smaller local palettes
         */
        if (ReadHDF(&Image, GlobalPalette, dim_sizes, HDFName,
                    image_name_arr[idx], pal_name_arr[idx]) < 0) {
            fprintf(stderr , "Unable to read image %s from HDF file %s\n",image_name_arr[idx],HDFName);
            return -1;
        }

		assert(dim_sizes[0]==(hsize_t)((int)dim_sizes[0]));
		assert(dim_sizes[1]==(hsize_t)((int)dim_sizes[1]));
        RWidth  = (int)dim_sizes[1];
        RHeight = (int)dim_sizes[0];
#ifdef UNUSED
        w = dim_sizes[1];
        h = dim_sizes[0];

        LeftOfs = TopOfs = 0;
#endif /* UNUSED */


        /*
         * If the first image does not have a palette, I make my own global
         * color table Obviously this is not the best thing to do, better
         * steps would be:
         *
         *   1. Check for either a global palette or a global attribute called
         *      palette
         *   2. Check for palettes in any of the other images.
         */
        if (!has_local_palette) {
            for (i = 0 ; i < 256 ; i++) {
                Red[i] = 255 - i;
                Green[i] = 255 - i;
                Blue[i] = 255 - i;
            }
        } else {
            for (i = 0 ; i < 256 ; i++){
                Red[i] = GlobalPalette[i][0];
                Green[i] = GlobalPalette[i][1];
                Blue[i] = GlobalPalette[i][2];
            }
        }

        for (i = 0; i < 256; i++) {
            pc2nc[i] = r1[i] = g1[i] = b1[i] = 0;
        }

        /* compute number of unique colors */
        nc = 0;

        for (i = 0; i < numcols; i++) {
            /* see if color #i is already used */
            for (j = 0; j < i; j++) {
                if (Red[i] == Red[j] && Green[i] == Green[j] && Blue[i] == Blue[j])
                    break;
            }

            if (j==i) {
                /* wasn't found */
                pc2nc[i] = nc;
                r1[nc] = Red[i];
                g1[nc] = Green[i];
                b1[nc] = Blue[i];
                nc++;
            } else {
                pc2nc[i] = pc2nc[j];
            }
        }

        /* figure out 'BitsPerPixel' */
        for (i = 1; i < 8; i++) {
            if ((1<<i) >= nc)
                break;
        }

        BitsPerPixel = i;
        ColorMapSize = 1 << BitsPerPixel;

#ifdef UNUSED
        CountDown = w * h;  /* # of pixels we'll be doing */
#endif /* UNUSED */

        if (BitsPerPixel <= 1)
            InitCodeSize = 2;
        else
            InitCodeSize = BitsPerPixel;

#ifdef UNUSED
        curx = cury = 0;
#endif /* UNUSED */

        if (!fpGif) {
            fprintf(stderr,  "WriteGIF: file not open for writing\n" );
            return (1);
        }

        /*
         * If it is the first image we do all the header stuff that isn't
         * required for the rest of the images.
         */
        if (idx == 0) {
            /* Write out the GIF header and logical screen descriptor */
            if (n_images > 1) {
                fwrite("GIF89a", sizeof( char ), (size_t)6, fpGif);  /* the GIF magic number */
            } else {
                fwrite("GIF87a", sizeof( char ), (size_t)6, fpGif);  /* the GIF magic number */
            }

            putword(RWidth, fpGif);             /* screen descriptor */
            putword(RHeight, fpGif);

            i = 0x00;                   /* No, there is no color map */
            i |= (8-1)<<4;              /* OR in the color resolution (hardwired 8) */
            i |= (BitsPerPixel - 1);    /* OR in the # of bits per pixel */
            fputc(i,fpGif);

            fputc(Background,fpGif);    /* background color */
            fputc(0, fpGif);            /* future expansion byte */

            /*
             * If loop_times is 0 , put in the application extension to make
             * the gif anime loop indefinitely
             */
            if (time_out > 0) {
                fputc(0x21 , fpGif);
                fputc(0xFF , fpGif);
                fputc(11 , fpGif);
                fwrite("NETSCAPE2.0" , (size_t)1 , (size_t)11 , fpGif);
                fputc(3 , fpGif);
                fputc(1 , fpGif);
                fputc(0 , fpGif);
                fputc(0 , fpGif);
                fputc(0 , fpGif);
            }
        }

        if (n_images > 1) {
            /* write a graphic control block */
            fputc(0x21 , fpGif);
            fputc(0xF9 , fpGif);
            fputc(4 , fpGif);
            fputc(4 , fpGif);
            putword(time_out , fpGif);
            fputc(255, fpGif);
            fputc(0 , fpGif);
        }

        /*
         * Put Image Descriptor
         * Hardwiring Left Offset and Top Offset to 0x00
         */
        fputc(0x2c , fpGif);
        putword(0x00 , fpGif);
        putword(0x00  , fpGif);
        putword(RWidth   , fpGif);
        putword(RHeight  , fpGif);

        /* since we always have a local color palette ... */
        fputc((0x80 | (BitsPerPixel - 1)) , fpGif);

        for (i = 0; i < ColorMapSize; i++) {
            /* write out Global colormap */
            fputc(r1[i], fpGif);
            fputc(g1[i], fpGif);
            fputc(b1[i], fpGif);
        }

        fputc(InitCodeSize , fpGif);

        i = hdfWriteGIF(fpGif , Image , 0 , (int)dim_sizes[0] ,
                        (int)dim_sizes[1] , r1, g1 , b1 , pc2nc , 256 , 8 ,
                        BitsPerPixel);
        fputc(0x00, fpGif);
        free(Image);
    }

    if (fputc(';',fpGif) == EOF) {
        /* Write GIF file terminator */
        fprintf(stderr , "Error!");
        return -1;
    }

    fclose(fpGif);

#if 0
    while(number_of_images--) {
        if (image_name_arr[number_of_images])
            free(image_name_arr[number_of_images]);
        if (pal_name_arr[number_of_images])
            free(pal_name_arr[number_of_images]);
    }
#endif  /* 0 */

    return 0;
}
