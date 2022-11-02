/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  Title:       GIF.H
 *  Purpose:     GIF Header file
 */
#ifndef GIF_H_
#define GIF_H_ 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hdf5.h"

#define MAX_PAL 768

/* typedef H5T_NATIVE_UINT8  GIFBYTE; */
typedef unsigned char GIFBYTE;

/* typedef H5T_NATIVE_UINT16  GIFWORD; */
typedef unsigned long GIFWORD;

typedef char GIFCHAR;

#ifndef boolean
typedef unsigned char boolean;
#endif

#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif

/* Set the EndianOrder.
** The GIF Reader file should do this.
** Set EndianOrder = 0 if machine is little endian
**     EndianOrder = 1 if machine is big endian.
*/
extern int EndianOrder;

/*
**  The GIF header format.
**
**  This structure actually contains the header, logical screen
**  descriptor, and the global color table for the GIF image.
*/
typedef struct _GifHeader { /* Offset   Description                         */
    GIFBYTE PackedField;    /*  0Ah     Color Information                   */
    GIFWORD TableSize;
    GIFBYTE ImageCount; /* Keep a count of the number of images         */
    GIFBYTE CommentCount;
    GIFBYTE ApplicationCount;
    GIFBYTE PlainTextCount;
    GIFBYTE HDFPalette[256][3];
    GIFBYTE HeaderDump[6]; /* GIFBYTE array to dump header contents           */
    GIFBYTE LSDDump[7];    /* Logical Screen Descriptor dump               */
} GIFHEAD;

/*
**  The GIF Image Descriptor.
*/
typedef struct _GifImageDescriptor {
    GIFWORD ImageWidth;  /* Width of the image in pixels             */
    GIFWORD ImageHeight; /* Height of the image in pixels            */
    GIFBYTE PackedField; /* Image and Color Table Data Information   */
    GIFWORD TableSize;
    GIFWORD CodeSize; /* Minimum LZW CodeSize for image data      */
    GIFBYTE HDFPalette[256][3];
    GIFBYTE GIDDump[9]; /* GifImageDescriptor dump                  */

    GIFBYTE *Image; /* Decompressed Raster Image                */
    GIFBYTE *GIFImage;
} GIFIMAGEDESC;

/*
**  GIF 89a Graphic Control Extension Block
*/
typedef struct _GifGraphicControlExtension {
    GIFBYTE GCEDump[5]; /* Graphic Control Extension Dump           */
} GIFGRAPHICCONTROL;

/*
**  GIF 89a Plain Text Extension Block
*/
typedef struct _GifPlainTextExtension {
    GIFBYTE  PTEDump[15];   /* Plain Text Extension Dump                */
    GIFBYTE *PlainTextData; /* Plain Text data sub-blocks               */
    GIFWORD  DataSize;
} GIFPLAINTEXT;

/*
**  GIF 89a Application Extension Block
*/
typedef struct _GifApplicationExtension {
    GIFBYTE  AEDump[14];      /* Application Extension Dump               */
    GIFBYTE *ApplicationData; /* Application data sub-blocks              */
    GIFWORD  DataSize;
} GIFAPPLICATION;

/*
**  GIF 89a Comment Extension Block
*/
typedef struct _GifCommentExtension {
    GIFBYTE  CEDump[2];   /* Comment Extension Dump                   */
    GIFBYTE *CommentData; /* Comment data sub-blocks                  */
    GIFWORD  DataSize;
    GIFBYTE  Terminator; /* Block Terminator (always 0)              */
} GIFCOMMENT;

/*
** GIF to HDF Memory Struct
** Purpose : The gif to hdf structure is used to pass all the
**           gif data to the memory, which gets caught by the hdf driver
**           Its the drivers job to put the data in the appropriate places
**           in the HDF file.
**           I have assumed that the ImageDescriptors and GraphicControls follow
**           one another, ie. I have not associated them with each other. The driver
**           must assume a 1-1 correspondence. The same discussion with plain text
**           extension.
*/
typedef struct _GifToMem {
    GIFHEAD            *GifHeader;
    GIFIMAGEDESC      **GifImageDesc;
    GIFGRAPHICCONTROL **GifGraphicControlExtension;
    GIFPLAINTEXT      **GifPlainTextExtension;
    GIFAPPLICATION    **GifApplicationExtension;
    GIFCOMMENT        **GifCommentExtension;
} GIFTOMEM;

/*
**  Function Prototypes
*/

/* GIF2MEM.C */
int Gif2Mem(GIFBYTE *, GIFTOMEM *);

/* GIFREAD.C */
int ReadGifHeader(GIFHEAD *, GIFBYTE **);
int ReadGifImageDesc(GIFIMAGEDESC *, GIFBYTE **);
int ReadGifGraphicControl(GIFGRAPHICCONTROL *, GIFBYTE **);
int ReadGifPlainText(GIFPLAINTEXT *, GIFBYTE **);
int ReadGifApplication(GIFAPPLICATION *, GIFBYTE **);
int ReadGifComment(GIFCOMMENT *, GIFBYTE **);

/* HDFGIFWR.C */
int hdfWriteGIF(FILE *fp, GIFBYTE *pic, int ptype, int w, int h, const GIFBYTE *rmap, const GIFBYTE *gmap,
                const GIFBYTE *bmap, const GIFBYTE *pc2ncmap, int numcols, int colorstyle, int BitsPerPixel);

/* WRITEHDF.C */
int WriteHDF(GIFTOMEM, GIFCHAR *);

/* Function:    ReadHDF
** Return:      0 on completion without error, -1 on error
** Input:       GIFCHAR *h5_file - HDF file name
**              GIFCHAR *dset_name - Name of the HDF Image dataset
**              GIFCHAR *pal_name - Name of the HDF palette
** Output:      GIFBYTE* data - the HDF Image to be converted
**              GIFBYTE  palette[256][3] - the corresponding palette
**              hsize_t* image_size - the size of each dimension of the image
*/
int ReadHDF(GIFBYTE **data, GIFBYTE palette[256][3], hsize_t *image_size, GIFCHAR *h5_file,
            GIFCHAR *dset_name, GIFCHAR *pal_name);

GIFBYTE *Decompress(GIFIMAGEDESC *, GIFHEAD *);
GIFBYTE  GetByte(const GIFBYTE *);
GIFWORD  GetWord(GIFBYTE *);

void cleanup(GIFBYTE *);

#endif /* GIF_H_ */
