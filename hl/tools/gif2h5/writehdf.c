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
#include <string.h>
#include <stdlib.h>
#include <string.h>

#include "gif.h"

/*******************************************************************
 * Function:	write_text_attribute
 * Use:         Just a small wrapper to write text attributes easily
 *******************************************************************/
static int write_text_attribute(hid_t dataset_id , const char *attr_name,
                                const char *attr_value, const size_t attr_len)
{
    /* variables for the attributes */
    hsize_t attr_size;     /* dimensions for the attribute */
    hid_t attr_dataspace_id;    /* dataspaces needed for the various attributes */
    hid_t attr_attr_id;	        /* attribute id */
    hid_t attr_type_id;

    /* check strings */
    if (!attr_name || !attr_value)
        return -1;

    /* figure out size of the data */
    attr_size = (hsize_t)attr_len;

    /* set the type to string */
    attr_type_id = H5Tcopy(H5T_C_S1);
    H5Tset_size(attr_type_id , (size_t)attr_size);

    /* create the dataspace for the attribute */
    attr_dataspace_id = H5Screate(H5S_SCALAR);

    /* create the attribute */
    attr_attr_id = H5Acreate(dataset_id , attr_name , attr_type_id ,
                             attr_dataspace_id , H5P_DEFAULT);

    /* write out the attribute data */
    if (H5Awrite(attr_attr_id , attr_type_id , attr_value) < 0)
        return -1;

    /* close the attribute */
    if (H5Aclose(attr_attr_id) < 0)
        return -1;

    /* close the dataspace */
    if (H5Sclose(attr_dataspace_id) < 0) {
        fprintf(stderr , "Unable to close attribute dataspace. Aborting \n");
        return -1;
    }

    return 0;
}

int
WriteHDF(GIFTOMEM GifMemoryStruct, char *HDFName , char *GIFFileName)
{
    GIFHEAD          gifHead;           /* GIF Header structure            */
    GIFIMAGEDESC    *gifImageDesc;      /* Logical Image Descriptor struct */

    long ImageCount;                    /* number of images */
#ifdef UNUSED
    long CommentCount,                  /* number of comments */
         ApplicationCount,              /* number of application extensions */
         PlainTextCount;                /* number of plain text extensions */
#endif /* UNUSED */

    char ImageName[256];                /* Image name for the GR Image */
    char GroupName[VSNAMELENMAX];       /* so that we can name the subgroups appropriately */

    /* H5 variables */
    hid_t file_id;      /* H5 file id */
    hid_t image_id;     /* H5 id for the whole image */
    hid_t pal_id;       /* H5 id for the palette */
    hobj_ref_t pal_ref; /* Create a reference for the palette */

    /* temp counter */
    int i;

    /* get the GIFMem stuff */
    gifHead = *(GifMemoryStruct.GifHeader);

    /* get some data from gifHead */
    ImageCount = gifHead.ImageCount;
#ifdef UNUSED
    CommentCount = (WORD)gifHead.CommentCount;
    ApplicationCount = (WORD)gifHead.ApplicationCount;
    PlainTextCount = (WORD)gifHead.PlainTextCount;
#endif /* UNUSED */

    /* get the main group name from GIFFileName */
    GroupName[0]= '/';

    if (strncpy(GroupName , GIFFileName , VSNAMELENMAX-2) == NULL) {
        fprintf(stderr , "strncpy failed\n");
        exit(1);
    }

    GroupName[VSNAMELENMAX - 1] = '\0';

    if ((file_id = H5Fcreate(HDFName , H5F_ACC_TRUNC , H5P_DEFAULT , H5P_DEFAULT)) < 0) {
        /* error occured opening the HDF File for write */
        fprintf(stderr , "HDF file could not be opened for writing\n");
        fprintf(stderr , "NOTE: GIF file must be present in the same directory as the binary on UNIX systems.\n");
        exit(1);
    }

    /* create a group within the root group to hold the gif image */
    /* might want to make a different naming style out here */
    image_id = H5Gcreate(file_id, GroupName , 0);

    /* first create the global palette if there is one */
    if (gifHead.PackedField & 0x80) { /* global palette exists */
        hid_t dataspace_id;	/* identifier for dataspace */
        hsize_t dims[2];	/* specify the dimensions of the palette */

        hsize_t dimsr[1] = {1};	/* needed to store reference */
        hid_t ref_dataspace_id;	/* dataspace id for references */
        hid_t ref_dataset_id;	/* dataset id for references */

        /* size of the palette is tablesize (rows) X 3 (columns) */
        dims[0] = gifHead.TableSize;
        dims[1] = 3;

        /* create the dataspace */
        if ((dataspace_id = H5Screate_simple(2 , dims , NULL)) < 0) {
            fprintf(stderr , "Could not create dataspace for palette. Aborting...\n");
            return -1;
        }

        /* create the palette dataset */
        if ((pal_id = H5Dcreate(image_id , "Global Palette" , H5T_NATIVE_UINT8 , dataspace_id , H5P_DEFAULT )) < 0) {
            fprintf(stderr , "Could not create palette dataset. Aborting...\n");
            return -1;
        }

        /* write the palette data out */
        /****** Ask Elena about VOIDP ******/
        if (H5Dwrite(pal_id , H5T_NATIVE_UINT8 , H5S_ALL , H5S_ALL , H5P_DEFAULT , (void *)gifHead.HDFPalette) < 0) {
            fprintf(stderr , "Error writing dataset. Aborting...\n");
            return -1;
        }

        /* set palette attributes */

        /* attribute CLASS = PALETTE */
        if ((write_text_attribute(pal_id , "CLASS" , "PALETTE",strlen("PALETTE"))) < 0) {
            fprintf(stderr , "Unable to write palette attributes. Aborting\n");
            return -1;
        }

        /* attribute PAL_COLORMODEL = RGB */
        if ((write_text_attribute(pal_id , "PAL_COLORMODEL" , "RGB",strlen("RGB"))) < 0) {
            fprintf(stderr , "Unable to write palette attributes. Aborting\n");
            return -1;
        }

        /* attribute PAL_TYPE = STANDARD8 */
        if ((write_text_attribute(pal_id , "PAL_TYPE" , "STANDARD8",strlen("STANDARD8"))) < 0) {
            fprintf(stderr , "Unable to write palette attributes. Aborting\n");
            return -1;
        }

        /* attribute PAL_VERSION = 1.0 */
        if ((write_text_attribute(pal_id , "PAL_VERSION" , "1.0",strlen("1.0"))) < 0) {
            fprintf(stderr , "Unable to write palette attributes. Aborting\n");
            return -1;
        }

        /* create dataspace for the dataset to store references */
        ref_dataspace_id = H5Screate_simple(1 , dimsr , NULL);

        /* create a dataset to store the references */
        ref_dataset_id = H5Dcreate(image_id , "Palette Reference" , H5T_STD_REF_OBJ , ref_dataspace_id , H5P_DEFAULT);

        /* create a reference to the palette */
        if (H5Rcreate(&pal_ref , image_id , "Global Palette" , H5R_OBJECT , -1) < 0) {
            fprintf(stderr , "Unable to create palette reference\n");
            return -1;
        }

        /* write the reference out */
        if (H5Dwrite(ref_dataset_id , H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL , H5P_DEFAULT, &pal_ref) < 0) {
            fprintf(stderr , "Unable to write Palette Reference");
            return -1;
        }

        /* close dataset */
        if (H5Dclose(ref_dataset_id) < 0) {
            fprintf(stderr , "Unable to close palette dataset.\n");
            return -1;
        }

        /* close dataspace */
        if (H5Sclose(ref_dataspace_id) < 0) {
            fprintf(stderr , "Unable to close palette dataspace.\n");
            return -1;
        }

        /* close everything */
        if (H5Dclose(pal_id) < 0) {
            fprintf(stderr , "Unable to close palette dataset. Aborting.\n");
            return -1;
        }

        if (H5Sclose(dataspace_id) < 0) {
            fprintf(stderr , "Unable to close palette dataspace. Aborting.\n");
            return -1;
        }
    }

    for(i = 0; i < ImageCount; i++) {
        /* variables for the images */
        hsize_t dims[2];        /* dimensions for the dataset */
        hid_t dataspace_id;     /* identifier for the dataspace */
        hid_t sub_image_id;     /* wierd name to distinguish from the group_id */

        /* variables for the attributes */
        hsize_t attr_dims[2];   /* dimensions for the attribute */
        hid_t attr_dataspace_id;/* dataspaces needed for the various attributes */
        hid_t attr_attr_id;     /* attribute id */
        BYTE minmax[2];         /* lower and upper minmax values */

        /* initialise minmax */
        minmax[0] = 0 ;
        minmax[1] = 255;

        /* get the gifImageDesc */
        gifImageDesc = GifMemoryStruct.GifImageDesc[i];

        /* set the dimensions */
        dims[0] = gifImageDesc->ImageHeight;
        dims[1] = gifImageDesc->ImageWidth;

        /* create the empty dataspace */
        if ((dataspace_id = H5Screate_simple(2 , dims , NULL)) < 0) {
            fprintf(stderr , "Could not create image dataspace. Aborting\n");
            return -1;
        }

        /* create the image name */
        sprintf(ImageName , "Image%d" , i);

        /* create the image data set */
        if ((sub_image_id = H5Dcreate(image_id , ImageName , H5T_NATIVE_UINT8 , dataspace_id , H5P_DEFAULT)) < 0) {
            fprintf(stderr , "Could not create dataset for image. Aborting... \n");
            return -1;
        }

        /* write out the image */
        /****** Ask Elena about VOIDP ******/
        if (H5Dwrite(sub_image_id , H5T_NATIVE_UINT8 , H5S_ALL , H5S_ALL , H5P_DEFAULT , (void *)(gifImageDesc->Image)) < 0) {
            fprintf(stderr , "Error writing image. Aborting... \n");
            return -1;
        }

        /* set the attributes */
        /* This info is available at http://hdf.ncsa.uiuc.edu/HDF5/doc/ImageSpec.html */
        /* The following attributes must be set for each image:
        ** ---------------------------------------
        ** Attribute Name			Value
        **		CLASS				IMAGE
        **		IMAGE_VERSION		1.0
        **		IMAGE_SUBCLASS		IMAGE_BITMAP
        **		PALETTE				ref. to palette datasets
        **		IMAGE_MINMAXRANGE	[0,255]
        ** ---------------------------------------
        */

        /****************************************
        ** Attribute: CLASS
        ** Value	: IMAGE
        *****************************************/

        if (write_text_attribute(sub_image_id , "CLASS" , "IMAGE",strlen("IMAGE")) < 0) {
            fprintf(stderr , "Unable to write CLASS = IMAGE attribute\n");
            return -1;
        }

        /****************************************
        ** Attribute: IMAGE_VERSION
        ** Value	: 1.0
        *****************************************/

        if (write_text_attribute(sub_image_id , "IMAGE_VERSION" , "1.0",strlen("1.0")) < 0) {
            fprintf(stderr , "Unable to write IMAGE_VERSION attribute\n");
            return -1;
        }

        /****************************************
        ** Attribute: IMAGE_SUBCLASS
        ** Value	: IMAGE_BITMAP
        *****************************************/

        if (write_text_attribute(sub_image_id , "IMAGE_SUBCLASS" , "IMAGE_BITMAP", strlen("IMAGE_BITMAP")) < 0) {
            fprintf(stderr , "Unable to write IMAGE_SUBCLASS attribute\n");
            return -1;
        }

        /****************************************
        ** Attribute: IMAGE_COLORMODEL
        ** Value	: RGB
        *****************************************/

        if (write_text_attribute(sub_image_id , "IMAGE_COLORMODEL" , "RGB", strlen("RGB")) < 0) {
            fprintf(stderr , "Unable to write IMAGE_SUBCLASS attribute\n");
            return -1;
        }

        /****************************************
        ** Attribute: PALETTE
        ** Value	: Reference to Palette
        *****************************************/

        /**** MAKE SURE PALETTE EXISTS!!! ****/
        if (gifHead.PackedField & 0x80) {
            /* global palette exists */
            attr_dims[0] = 1;

            /* create the dataspace for the attribute */
            attr_dataspace_id = H5Screate_simple(1 , attr_dims , NULL);

            /* create the attribute */
            attr_attr_id = H5Acreate(sub_image_id , "PALETTE" , H5T_STD_REF_OBJ , attr_dataspace_id , H5P_DEFAULT);

            if (H5Awrite(attr_attr_id , H5T_STD_REF_OBJ , &pal_ref) < 0) {
                fprintf(stderr , "Unable to write attribute. Aborting \n");
                return -1;
            }

            /* close the attribute */
            if (H5Aclose(attr_attr_id) < 0) {
                fprintf(stderr , "Unable to close CLASS IMAGE attribute. Aborting.\n");
                return -1;
            }

            /* close the dataspace */
            if (H5Sclose(attr_dataspace_id) < 0) {
                fprintf(stderr , "Unable to close attribute dataspace. Aborting \n");
                return -1;
            }
        }

        /****************************************
        ** Attribute: IMAGE_MINMAXRANGE
        ** Value	: minmax
        *****************************************/

        attr_dims[0] = 2;

        /* create the dataspace for the attribute */
        attr_dataspace_id = H5Screate_simple(1 , attr_dims , NULL);

        /* create the attribute */
        attr_attr_id = H5Acreate(sub_image_id , "IMAGE_MINMAXRANGE" , H5T_NATIVE_UINT8 , attr_dataspace_id , H5P_DEFAULT);

        if (H5Awrite(attr_attr_id , H5T_NATIVE_UINT8 , minmax) < 0) {
            fprintf(stderr , "Unable to write attribute. Aborting \n");
            return -1;
        }

        /* close the attribute */
        if (H5Aclose(attr_attr_id) < 0) {
            fprintf(stderr , "Unable to close CLASS IMAGE attribute. Aborting.\n");
            return -1;
        }

        /* close the dataspace */
        if (H5Sclose(attr_dataspace_id) < 0) {
            fprintf(stderr , "Unable to close attribute dataspace. Aborting \n");
            return -1;
        }

        /* close everything */
        if (H5Dclose(sub_image_id) < 0) {
            fprintf(stderr , "Unable to close image dataset. Aborting \n");
            return -1;
        }

        if (H5Sclose(dataspace_id) < 0) {
            fprintf(stderr , "Unable to close image dataspace. Aborting \n");
            return -1;
        }
    }

    /* close the main image group */
    if (H5Gclose(image_id) < 0) {
        fprintf(stderr , "Could not close the image group. Aborting...\n");
        return -1;
    }

    /* close the H5 file */
    if (H5Fclose(file_id) < 0) {
        fprintf(stderr , "Could not close HDF5 file. Aborting...\n");
        return -1;
    }

    return 0;
}
