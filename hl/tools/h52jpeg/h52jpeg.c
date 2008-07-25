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



#include <stdio.h>
#include <stdlib.h>

/* 
 * Include file for users of JPEG library.
 * system headers that define at least the typedefs FILE and size_t are needed before
 */

#include "jpeglib.h"
#include "jerror.h"

/* Tools library and HDF5 Image headers */

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5trav.h"
#include "H5IMpublic.h"


const char *progname = "h52jpeg";
int d_status = EXIT_SUCCESS;

/* command-line options: The user can specify short or long-named parameters */
static const char *s_opts = "hVvi:cp:";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "version", no_arg, 'V' },
    { "verbose", no_arg, 'v' },
    { "image", require_arg, 'i' },
    { "convert", no_arg, 'c' },
    { "palette", require_arg, 'p' },
    { NULL, 0, '\0' }
};


/* a structure that contains h52jpeg options */
typedef struct 
{
 const char  *file_name;
 const char  *template_name;
 const char  *image_name;
 int         convert_true;
 int         idx_palette;
 int         verbose;
} h52jpeg_opt_t;


/* prototypes */
static void usage(const char *prog);
static int h52jpeg(h52jpeg_opt_t opt);
static void make_jpeg_name( const char* template_name, const char* image_name, char* jpeg_name);
static int do_object(hid_t fid, h52jpeg_opt_t opt, const char* image_name);
static int do_image(hid_t fid, h52jpeg_opt_t opt, const char* image_name, char* jpeg_name);
static void write_JPEG_file(char *filename, JSAMPLE *image_buffer, int image_height, int image_width, int planes);               
static void convert_to_true( hsize_t width, hsize_t height, unsigned char* ibuf, unsigned char* pbuf, unsigned char* tbuf);

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h52jpeg main program
 *
 * main reads command line options and calls h52jpeg 
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Return: 1, error, 0 success
 *
 * Date: May 30, 2008
 *
 *-------------------------------------------------------------------------
 */
int main(int argc, const char *argv[])
{
    h52jpeg_opt_t opt; /* command line options for h52jpeg */
    int           op;  /* option got from command line */

    /* initialze options to 0 */
    memset(&opt,0,sizeof(h52jpeg_opt_t));
   
    /* parse command line options */
    while ((op = get_option(argc, argv, s_opts, l_opts)) != EOF) 
    {
        switch ((char)op) 
        {

        case 'h':
            usage(progname);
            exit(EXIT_SUCCESS);

        case 'V':
            print_version(progname);
            exit(EXIT_SUCCESS);

        case 'v':
            opt.verbose = 1;
            break;  
            
        case 'i':
            opt.image_name = opt_arg;
            break;
       
        case 'c':
            opt.convert_true = 1;
            
            break;
        case 'p':
            opt.idx_palette = atoi(opt_arg);
            break;
            
        } /* switch */
        
        
    } /* while */
    
    /* check for file names to be processed */
    if ( argv[ opt_ind ] != NULL && argv[ opt_ind + 1 ] != NULL ) 
    {
        opt.file_name = argv[ opt_ind ];
        opt.template_name = argv[ opt_ind + 1 ];    
    }
    
    else
    {
        usage(progname);
        exit(EXIT_FAILURE);
    }

    if ( h52jpeg(opt) < 0 )
        return 1;
    
    
    return 0;
}



/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print usage
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static void usage(const char *prog)
{
    printf("usage: %s [OPTIONS] file template\n", prog);
    printf("  file                     HDF5 file name\n");
    printf("  template                 Name template for jpeg images\n");
    printf("  OPTIONS\n");
    printf("   -h, --help              Print a usage message and exit\n");
    printf("   -v, --verbose           Verbose mode, print object information\n");
    printf("   -V, --version           Print HDF5 version number and exit\n");
    printf("   -i, --image             Image name (full path in HDF5 file)\n");
    printf("   -c, --convert           Convert image from graycolor to truecolor\n");
    printf("   -p P, --palette=P       Use HDF5 palette index P in conversion -c\n");
    
    printf("\n");
    
    printf("  P - is an integer, the palette index in the HDF5 image. Default is 0\n");
    printf("  template - is a string used to name the jpeg file name; this name is made\n");
    printf("             by concatenating this template with the HDF5 image name\n");

    printf("\n");

    printf("Examples of use:\n");
    printf("\n");
    printf("1) h52jpeg -v file.h5 myjpeg\n");
    printf("\n");
    printf("   Exports all images found on file.h5 to jpeg images, giving output information\n");

    printf("\n");
    printf("2) h52jpeg -v -i image file.h5 myjpeg\n");
    printf("\n");
    printf("   Exports the HDF5 image named <image> to a jpeg file\n");

    printf("\n");
    printf("2) h52jpeg -v -c p 1 -i image file.h5 myjpeg\n");
    printf("\n");
    printf("   Exports the HDF5 image named <image> to a true color jpeg image, using\n");
    printf("    the palette of index 1 in that image to do the conversion\n");
    
    
}

/*-------------------------------------------------------------------------
 * Function: h52jpeg
 *
 * Parameters: OPT, options at command line
 *
 * Purpose: traverse the HDF5 file, save HDF5 images to jpeg files
 *
 * Return: 0, success, -1 error
 *
 *-------------------------------------------------------------------------
 */
static int h52jpeg(h52jpeg_opt_t opt)
{
    hid_t         fid;
    trav_table_t  *travt = NULL;
    size_t        i;
   

    /* open the HDF5 file */
    if (( fid = h5tools_fopen(opt.file_name, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, (size_t)0)) < 0) 
    {
        error_msg(progname, "cannot open file <%s>\n", opt.file_name );
        return -1;
    }

    /*-------------------------------------------------------------------------
    * object name was specified at command line
    *-------------------------------------------------------------------------
    */
    
    if ( opt.image_name )
    {
        /* read object, save jpeg image */
        do_object(fid, opt, opt.image_name);
        
    }
    
    /*-------------------------------------------------------------------------
    * object name was not specified; traverse the file
    *-------------------------------------------------------------------------
    */
    
    else
        
    {
        
        /* initialize traversal table */
        trav_table_init(&travt);
        
        /* get the list of objects in the file */
        if ( h5trav_gettable(fid, travt) < 0 )
            goto out;
        
        /* search for images/datasets in file */
        for ( i = 0; i < travt->nobjs; i++) 
        {
            
            switch ( travt->objs[i].type ) 
            {
            default:
                printf( "unknown object. Exiting ...\n" );
                goto out;
                
            case H5TRAV_TYPE_GROUP:
            case H5TRAV_TYPE_NAMED_DATATYPE:
            case H5TRAV_TYPE_LINK:
            case H5TRAV_TYPE_UDLINK:
                
                break;
                
            case H5TRAV_TYPE_DATASET:

                /* read object, save jpeg image */
                do_object(fid, opt, travt->objs[i].name);

                break; /* H5TRAV_TYPE_DATASET */                               
                
        } /* switch */                
        
    } /* i */    
    
    
    /* HDfree table */
    trav_table_free(travt);
    
    } /* opt.image_name */
    
    
    /* close */
    if ( H5Fclose(fid) < 0 )
        return -1;
    
    return 0;
    
out:
    H5E_BEGIN_TRY 
    {
       H5Fclose(fid);
        
    } H5E_END_TRY;

    trav_table_free(travt);

    
    return -1;
}


/*-------------------------------------------------------------------------
 * Function: do_object
 *
 * Parameters: FID: HDF5 file id
 *             OPT: command line options
 *             OBJECT_NAME: the name of the object (type H5TRAV_TYPE_DATASET)
 *
 * Purpose: make the jpeg file name, read HDF5 image, save the jpeg image
 *
 *
 * Return: 0, success, -1 error
 *
 *-------------------------------------------------------------------------
 */
static
int do_object(hid_t fid, h52jpeg_opt_t opt, const char* object_name)
{
    int  done=0; /* return value from do_image */
    char jpeg_name[1024];
        
    /* build the jpeg file name */
    make_jpeg_name( opt.template_name, object_name, jpeg_name);
        
    if ( opt.verbose)
    {
        printf("%s ...", object_name );
    }
    
    /*-------------------------------------------------------------------------
    * HDF5 Image
    *-------------------------------------------------------------------------
    */
    
    if ( H5IMis_image( fid, object_name ) )
    {
        /* read image, save jpeg image */
        done = do_image(fid, opt, object_name, jpeg_name);
    }
           
    
    if ( opt.verbose)
    {                
        if ( done )
        {
            printf("saved to %s\n", jpeg_name );
        }
        else
        {
            printf("\n");
        }
        
    }
    
    return 0;

    
}


/*-------------------------------------------------------------------------
 * Function: do_image
 *
 * Parameters: FID: HDF5 file id
 *             OPT: command line options
 *             OBJECT_NAME: the name of the HDF5 image
 *             JPEG_NAME: the name of the jpeg image
 *
 * Purpose: read HDF5 image, save jpeg image
 *
 * Return: 0, success, -1 error
 *
 *-------------------------------------------------------------------------
 */
static
int do_image(hid_t fid, h52jpeg_opt_t opt, const char* image_name, char* jpeg_name)
{
    hsize_t        width;
    hsize_t        height;
    hsize_t        planes;
    char           interlace[20];
    hssize_t       npals;
    const char*    name;
    int            done;
    unsigned char* ibuf=NULL;
    
    name = image_name;
    
    done = 0;  
    
    if ( H5IMget_image_info( fid, name, &width, &height, &planes, interlace, &npals ) < 0 )
        goto out;
    
    if (NULL == (ibuf = HDmalloc( (int)width * (int)height * (int)planes ))) 
        goto out;
    
    if ( H5IMread_image( fid, name, ibuf ) < 0 )
    {
        goto out;
    }
    
    /*-------------------------------------------------------------------------
    * no conversion to true color requested or true color image, just save what we found
    * this will result either in
    *
    * 24bit HDF5 ---> true color jpeg
    * 8bit HDF5 ---> grey color jpeg
    *
    *-------------------------------------------------------------------------
    */
    if ( planes == 3 || !opt.convert_true )
    {
        /* write the jpeg file */
        write_JPEG_file (jpeg_name, 
            ibuf,	               
            (int) height,	       
            (int) width,           
            (int) planes); 
        
        done = 1;
    }
    /*-------------------------------------------------------------------------
    * conversion to truecolor
    * this will result  in
    *
    * 8bit HDF5 ---> true color jpeg
    *
    *-------------------------------------------------------------------------
    */
    else if (opt.convert_true && planes == 1  )
    {
        hsize_t       pdims[2];  /* palette dimensions */
        unsigned char *pbuf=NULL;/* palette array */
        unsigned char *tbuf=NULL;/* true color array */
        int           ipal;      /* palette to use */
        
        if ( H5IMget_npalettes( fid, name, &npals ) < 0 )
        {
            goto out;
        }
        
        /*-------------------------------------------------------------------------
        * there are palettes
        *-------------------------------------------------------------------------
        */
        if ( npals > 0  )
        {
            /* use either the default (0) palette or a requested one */
            ipal = ( opt.idx_palette > 0 ) ? opt.idx_palette : 0;

            /* the requested palette may not exist . use the default */
            if ( opt.idx_palette >= npals )
            {
                ipal = 0;
                
                if ( opt.verbose )
                {                
                    printf("palette index <%d> does not exist. Using default...", 
                        opt.idx_palette );
                }
            }
                
            
            if ( H5IMget_palette_info( fid, name, ipal, pdims ) < 0 )
                goto out;
            
            if (NULL == (pbuf = HDmalloc( (size_t)pdims[0] * (size_t)pdims[1] ))) 
                goto out;
            
            if (NULL == (tbuf = HDmalloc( (int)width * (int)height * 3 ))) 
                goto out;
            
            if ( H5IMget_palette( fid, name, ipal, pbuf ) < 0 )
                goto out;
            
            /* convert indexed image to true color image */
            convert_to_true(width, height, ibuf, pbuf, tbuf);
            
            /* write the jpeg file */
            write_JPEG_file (jpeg_name, 
                tbuf,	               
                (int) height,	       
                (int) width,           
                3); 
            
            done = 1;
            
            HDfree( pbuf );
            HDfree( tbuf );
            pbuf = NULL;
            tbuf = NULL;  
        }
        /*-------------------------------------------------------------------------
        * there are no palettes
        *-------------------------------------------------------------------------
        */
        else
        {    
            done = 0;
            if ( opt.verbose )
            {                
                printf("image <%s> has no palette...", name );
            }
        } /* no palettes */
        
    } /* conversion to truecolor  */
    
    HDfree( ibuf );
    ibuf = NULL;
 
    return done;
    
out:
    
    return -1;
    
}

/*-------------------------------------------------------------------------
 * Function: make_jpeg_name
 *
 * Parameters: template name (IN), image name (IN), jpeg name (IN/OUT)
 *
 * Purpose: build a name for the jpeg image file upon a template name
 *  and the HDF5 image name. Replace the special characters 
 *  '%', '@', '$', '/', ':', '&', ' ', and '*' with '_'
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static
void make_jpeg_name( const char* template_name, const char* image_name, char* jpeg_name)
{
    int j;
    int len;

    HDstrcpy( jpeg_name, template_name );
    HDstrcat( jpeg_name, image_name );
    HDstrcat( jpeg_name, ".jpeg" );
    len = HDstrlen( jpeg_name);
    
    /* HDF5 path names might contain '/', replace with '_' */
    for (j = 0; j < len; j++)
    {
        if ( (jpeg_name[j] == '/') ||
             (jpeg_name[j] == '%') ||
             (jpeg_name[j] == '@') ||
             (jpeg_name[j] == '$') ||
             (jpeg_name[j] == '/') ||
             (jpeg_name[j] == ':') ||
             (jpeg_name[j] == '&') ||
             (jpeg_name[j] == ' ') ||
             (jpeg_name[j] == '*') )
        {  
            jpeg_name[j] = '_'; 
        }
    }
    
}

/*-------------------------------------------------------------------------
 * Function: convert_to_true
 *
 * Parameters: WIDTH (IN):    width of image
 *             HEIGHT (IN):   height of image
 *             IBUF (IN):     indexed image buffer
 *             PBUF (IN):     palette buffer
 *             TBUF (IN/OUT): true color image buffer
 *
 * Purpose: convert a greycolor buffer to a true color buffer using a palette buffer
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static
void convert_to_true( hsize_t width, hsize_t height, unsigned char* ibuf, unsigned char* pbuf, unsigned char* tbuf)
{
    hsize_t  i, j;
   
    for ( i = 0, j = 0; i < width * height; i++, j += 3) 
    {
        unsigned char idx;
        unsigned char r;
        unsigned char g;
        unsigned char b;

        /* get the index from the grey image */
        idx = ibuf[i];

        /* get the RGB color */
        r = pbuf[3*idx];
        g = pbuf[3*idx+1];
        b = pbuf[3*idx+2];

        /* define the color buffer */
        tbuf[j] = r;
        tbuf[j+1] = g;
        tbuf[j+2] = b;
        
    }
    
    
}


/*-------------------------------------------------------------------------
 * Function: write_JPEG_file (adapted from example.c of jpeg distribution)
 *
 * Purpose: save a jpeg file named FILENAME, using the image buffer IMAGE_BUFFER
 *
 * The standard input image format is a rectangular array of pixels, with
 * each pixel having the same number of "component" values (color channels).
 * Each pixel row is an array of JSAMPLEs (which typically are unsigned chars).
 * If you are working with color data, then the color values for each pixel
 * must be adjacent in the row; for example, R,G,B,R,G,B,R,G,B,... for 24-bit
 * RGB color.
 *
 * Return: void, exits on error
 *
 *-------------------------------------------------------------------------
 */

static 
void write_JPEG_file(char *filename,        /* JPEG file name */
                     JSAMPLE *image_buffer, /* Points to large array of R,G,B-order data */
                     int image_height,      /* Number of rows in image */
                     int image_width,       /* Number of columns in image */
                     int planes)            /* # of color components per pixel */           
{
    /* This struct contains the JPEG compression parameters and pointers to
    * working space (which is allocated as needed by the JPEG library).
    * It is possible to have several such structures, representing multiple
    * compression/decompression processes, in existence at once.  We refer
    * to any one struct (and its associated working data) as a "JPEG object".
    */
    struct jpeg_compress_struct cinfo;
    /* This struct represents a JPEG error handler.  It is declared separately
    * because applications often want to supply a specialized error handler
    * (see the second half of this file for an example).  But here we just
    * take the easy way out and use the standard error handler, which will
    * print a message on stderr and call exit() if compression fails.
    * Note that this struct must live as long as the main JPEG parameter
    * struct, to avoid dangling-pointer problems.
    */
    struct jpeg_error_mgr jerr;
    /* More stuff */
    FILE * outfile;		/* target file */
    JSAMPROW row_pointer[1];	/* pointer to JSAMPLE row[s] */
    int row_stride;		/* physical row width in image buffer */
    
    /* Step 1: allocate and initialize JPEG compression object */
    
    /* We have to set up the error handler first, in case the initialization
    * step fails.  (Unlikely, but it could happen if you are out of memory.)
    * This routine fills in the contents of struct jerr, and returns jerr's
    * address which we place into the link field in cinfo.
    */
    cinfo.err = jpeg_std_error(&jerr);
    /* Now we can initialize the JPEG compression object. */
    jpeg_create_compress(&cinfo);
    
    /* Step 2: specify data destination (eg, a file) */
    /* Note: steps 2 and 3 can be done in either order. */
    
    /* Here we use the library-supplied code to send compressed data to a
    * stdio stream.  You can also write your own code to do something else.
    * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
    * requires it in order to write binary files.
    */
    if ((outfile = fopen(filename, "wb")) == NULL) {
        fprintf(stderr, "can't open %s\n", filename);
        exit(1);
    }
    jpeg_stdio_dest(&cinfo, outfile);
    
    /* Step 3: set parameters for compression */
    
    /* First we supply a description of the input image.
    * Four fields of the cinfo struct must be filled in:
    */
    cinfo.image_width = image_width; 	/* image width and height, in pixels */
    cinfo.image_height = image_height;
    cinfo.input_components = planes;		/* # of color components per pixel */
    
    /* colorspace of input image */
    if (planes == 3)
        cinfo.in_color_space = JCS_RGB; 
    else if (planes == 1)
        cinfo.in_color_space = JCS_GRAYSCALE; 
    
    /* Now use the library's routine to set default compression parameters.
    * (You must set at least cinfo.in_color_space before calling this,
    * since the defaults depend on the source color space.)
    */
    jpeg_set_defaults(&cinfo);
    /* Now you can set any non-default parameters you wish to.
    * Here we just illustrate the use of quality (quantization table) scaling:
    */
    jpeg_set_quality(&cinfo, 100, TRUE /* limit to baseline-JPEG values */);
    
    /* Step 4: Start compressor */
    
    /* TRUE ensures that we will write a complete interchange-JPEG file.
    * Pass TRUE unless you are very sure of what you're doing.
    */
    jpeg_start_compress(&cinfo, TRUE);
    
    /* Step 5: while (scan lines remain to be written) */
    /*           jpeg_write_scanlines(...); */
    
    /* Here we use the library's state variable cinfo.next_scanline as the
    * loop counter, so that we don't have to keep track ourselves.
    * To keep things simple, we pass one scanline per call; you can pass
    * more if you wish, though.
    */
    row_stride = image_width * planes;	/* JSAMPLEs per row in image_buffer */
    
    while (cinfo.next_scanline < cinfo.image_height) {
    /* jpeg_write_scanlines expects an array of pointers to scanlines.
    * Here the array is only one element long, but you could pass
    * more than one scanline at a time if that's more convenient.
        */
        row_pointer[0] = & image_buffer[cinfo.next_scanline * row_stride];
        (void) jpeg_write_scanlines(&cinfo, row_pointer, 1);
    }
    
    /* Step 6: Finish compression */
    
    jpeg_finish_compress(&cinfo);
    /* After finish_compress, we can close the output file. */
    fclose(outfile);
    
    /* Step 7: release JPEG compression object */
    
    /* This is an important step since it will release a good deal of memory. */
    jpeg_destroy_compress(&cinfo);
    
    /* And we're done! */
}

