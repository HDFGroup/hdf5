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

/* From jpeg documentation 
 *
 * Include file for users of JPEG library.
 * You will need to have included system headers that define at least
 * the typedefs FILE and size_t before you can include jpeglib.h.
 * (stdio.h is sufficient on ANSI-conforming systems.)
 * You may also wish to include "jerror.h".
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include "jpeglib.h"
#include "jerror.h"

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5trav.h"
#include "H5IMpublic.h"


const char *progname = "h52jpeg";
int d_status = EXIT_SUCCESS;

/* command-line options: The user can specify short or long-named parameters */
static const char *s_opts = "hVvi:t:";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "version", no_arg, 'V' },
    { "verbose", no_arg, 'v' },
    { "image", require_arg, 'i' },
    { "type", require_arg, 't' },
    { NULL, 0, '\0' }
};


/* a structure that contains h52jpeg options */
typedef struct 
{
 const char  *file_name;
 const char  *template_name;
 const char  *image_name;
 int         image_type;
 int         verbose;
} h52jpeg_opt_t;


/* prototypes */
static void usage(const char *prog);
static int h52jpeg(h52jpeg_opt_t opt);
static void make_jpeg_name( const char* template_name, const char* image_name, char* jpeg_name);
static int do_image(hid_t fid, h52jpeg_opt_t opt, const char* image_name);
static void write_JPEG_file(char *filename, JSAMPLE *image_buffer, int image_height, int image_width, int planes);               

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h52jpeg main program
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: May 30, 2008
 *
 *-------------------------------------------------------------------------
 */
int main(int argc, const char *argv[])
{
    h52jpeg_opt_t opt;
    const char    *image_type = NULL;
    int           op;

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
        case 't':
            image_type = opt_arg;
            
            
            if ( HDstrcmp( image_type, "grey" ) == 0 )
            {
                opt.image_type = 0;
            }
            else if ( HDstrcmp( image_type, "true" ) == 0 )
            {
                opt.image_type = 1;
            }
            else 
            {
                printf("<%s> is an invalid image type\n", image_type); 
                exit(EXIT_FAILURE);
            }
            
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
    printf("   -t T, --type=T          Type of image (graycolor or truecolor)\n");
    
    printf("\n");
    
    printf("  T - is a string, either <grey> or <true>\n");
    
}

/*-------------------------------------------------------------------------
 * Function: h52jpeg
 *
 * Parameters: OPT, options at command line
 *
 * Purpose: traverse the HDF5 file, save HDF5 images to jpeg files, translate
 *  2D datasets of classes H5T_INTEGER and H5T_FLOAT to image data and save them
 *  to jpeg files
 *
 * Return: 0, all is fine, -1 not all is fine
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
    * image/dataset name was specified at command line
    *-------------------------------------------------------------------------
    */
    
    if ( opt.image_name )
    {

        /* read HDF5 image/dataset, save jpeg image */
        do_image(fid, opt, opt.image_name);
        
    }
    
    /*-------------------------------------------------------------------------
    * image name was not specified; traverse the file
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
                goto out;
                
            case H5TRAV_TYPE_GROUP:
            case H5TRAV_TYPE_NAMED_DATATYPE:
            case H5TRAV_TYPE_LINK:
            case H5TRAV_TYPE_UDLINK:
                
                break;
                
            case H5TRAV_TYPE_DATASET:

                /* read HDF5 image/dataset, save jpeg image */
                do_image(fid, opt, travt->objs[i].name);

                break; /* H5TRAV_TYPE_DATASET */                               
                
        } /* switch */                
        
    } /* i */    
    
    
    /* free table */
    trav_table_free(travt);
    
    } /* image_name */
    
    
    /* close */
    if ( H5Fclose(fid) < 0 )
        return -1;
    
    return 0;
    
out:
    H5E_BEGIN_TRY 
    {
       H5Fclose(fid);
        
    } H5E_END_TRY;

    
    return -1;
}



/*-------------------------------------------------------------------------
 * Function: do_image
 *
 * Parameters: HDF5 file id, command line options, an image name
 *
 * Purpose: read HDF5 image/dataset, save jpeg image
 *
 * Return: int
 *
 *-------------------------------------------------------------------------
 */
static
int do_image(hid_t fid, h52jpeg_opt_t opt, const char* image_name)
{
    hsize_t        width;
    hsize_t        height;
    hsize_t        planes;
    char           interlace[20];
    hssize_t       npals;
    unsigned char* buf=NULL;
    H5T_class_t    tclass;
    hid_t          sid;
    hid_t          did;
    hid_t          tid;
    int            rank;
    hsize_t        dims[H5S_MAX_RANK];
    hsize_t        maxdim[H5S_MAX_RANK];
    size_t         size;
    hsize_t        nelmts;
    const char*    name;
    size_t         i;
    int            j;
    int            done;
    char           jpeg_name[1024];

    name = image_name;
    
    /* build the jpeg file name */
    make_jpeg_name( opt.template_name, image_name, jpeg_name);
    
    done = 0;
    
    if ( opt.verbose)
    {
        printf("%s ...", name );
    }
    
    /*-------------------------------------------------------------------------
    * HDF5 Image
    *-------------------------------------------------------------------------
    */
    
    if ( H5IMis_image( fid, name ) )
    {
        
        if ( H5IMget_image_info( fid, name, &width, &height, &planes, interlace, &npals ) < 0 )
            goto out;
        
        if (NULL == (buf = HDmalloc( (size_t)width * (size_t)height * (size_t)planes ))) 
            goto out;
        
        if ( H5IMread_image( fid, name, buf ) < 0 )
            goto out;
        
        /* write the jpeg file */
        write_JPEG_file (jpeg_name, 
            buf,	               
            (int) height,	       
            (int) width,           
            (int) planes);         
        
        
        free( buf );
        buf = NULL;
        
        done = 1;
        
        
        
    }

    /*-------------------------------------------------------------------------
    * HDF5 Image palette, ignore
    *-------------------------------------------------------------------------
    */
    
    else if ( H5IMis_palette( fid, name ) )
    {
        
    }
    
    /*-------------------------------------------------------------------------
    * regular dataset
    *-------------------------------------------------------------------------
    */
    
    else
    {

        unsigned char* image_buffer = NULL;
        
        if (( did = H5Dopen2( fid, name, H5P_DEFAULT )) < 0) 
            goto out;
        if (( sid = H5Dget_space( did )) < 0 )
            goto out;
        if (( rank = H5Sget_simple_extent_ndims(sid)) < 0 )
            goto out;
        if (( tid = H5Dget_type( did )) < 0 )
            goto out;
        if (( tclass = H5Tget_class(tid)) < 0)
            goto out;
        
        if ( ( H5T_FLOAT == tclass || H5T_INTEGER == tclass) &&
            ( rank == 2 ) )
        {
            
            if ( H5Sget_simple_extent_dims( sid, dims, maxdim ) < 0 )
                goto out;
            
            size =  H5Tget_size( tid );
            
            nelmts = 1;
            for ( j = 0; j < rank; j++)
            {
                nelmts *= dims[j];
            }
            
            if ( NULL == (buf = HDmalloc( (size_t)nelmts * size ))) 
                goto out;                  
            if ( H5Dread(did,tid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf) < 0 )
                goto out;                     

            height = dims[0];
            width  = dims[1];

            if ( opt.image_type == 0 )
                planes = 1;
            else if ( opt.image_type == 1 )
                planes = 3; 

            
            if ( NULL == (image_buffer = HDmalloc( (size_t)nelmts * sizeof (unsigned char) ))) 
            {
                goto out; 
            }

            /*-------------------------------------------------------------------------
            * convert the data to unsigned char
            *
            *-------------------------------------------------------------------------
            */
            
            {
                double min = DBL_MAX;
                double max = -DBL_MAX;
                double ratio;
                
                /* search for the minimum and maximum */
                for ( i = 0; i < nelmts; i++)
                {
                    if ( buf[i] < min) min = buf[i];
                    if ( buf[i] > max) max = buf[i];
                }
                /* converts the data based on the ratio to a 0-255 range */
                ratio = (min == max) ? 1.0 : (double)(255.0/(max-min));
                for ( i = 0; i < nelmts; i++)
                {
                    image_buffer[i] = (unsigned char)ceil( (( buf[i] - min ) / ratio) );         
                }
                
            }

            /* write the jpeg file */
            write_JPEG_file (jpeg_name, 
                image_buffer,	               
                (int) height,	       
                (int) width,           
                (int) planes);     
            
            
            free( image_buffer );
            free( buf );
            buf = NULL;
            image_buffer = NULL;
            done = 1;
            
        }
        
        
        
        H5Sclose(sid);
        H5Tclose(tid);
        H5Dclose(did);
        
        
        
        
    } /* else */
    
    
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
    
out:
    H5E_BEGIN_TRY 
    {
        
        H5Sclose(sid);
        H5Tclose(tid);
        H5Dclose(did);
        
    } H5E_END_TRY;
    
    if ( buf != NULL )
        free( buf );
    
    
    return -1;
    
}

/*-------------------------------------------------------------------------
 * Function: make_jpeg_name
 *
 * Parameters: template name (IN), image name (IN), jpeg name (IN/OUT)
 *
 * Purpose: build a name for the jpeg image file upon a template name
 *  and the HDF5 image name
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
        if (jpeg_name[j] == '/')
        {  
            jpeg_name[j] = '_'; 
        }
    }
    
}



/*
 * Sample routine for JPEG compression.  
 *
 * IMAGE DATA FORMATS:
 *
 * The standard input image format is a rectangular array of pixels, with
 * each pixel having the same number of "component" values (color channels).
 * Each pixel row is an array of JSAMPLEs (which typically are unsigned chars).
 * If you are working with color data, then the color values for each pixel
 * must be adjacent in the row; for example, R,G,B,R,G,B,R,G,B,... for 24-bit
 * RGB color.
 *
 * For this example, we'll assume that this data structure matches the way
 * our application has stored the image in memory, so we can just pass a
 * pointer to our image buffer. 
 */

static 
void write_JPEG_file(char *filename, 
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

