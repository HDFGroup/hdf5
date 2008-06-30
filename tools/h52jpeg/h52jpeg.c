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

#include <stdlib.h>
#include <memory.h>



#if 0
#include "jpeglib.h"
#include "jerror.h"
#endif

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
static int h52jpeg(h52jpeg_opt_t options);


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
int main(int argc, char **argv)
{
    h52jpeg_opt_t options;
    const char    *image_type = NULL;
    int           opt;

    /* initialze options to 0 */
    memset(&options,0,sizeof(h52jpeg_opt_t));
    
    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) 
    {
        switch ((char)opt) 
        {
        case 'h':
            usage(progname);
            exit(EXIT_SUCCESS);
        case 'V':
            print_version(progname);
            exit(EXIT_SUCCESS);
        case 'v':
            options.verbose = 1;
            break;        
        case 'i':
            options.image_name = opt_arg;
            break;
        case 't':
            image_type = opt_arg;
            break;
            
        } /* switch */
        
        
    } /* while */
    
    /* check for file names to be processed */
    if ( argv[ opt_ind ] != NULL && argv[ opt_ind + 1 ] != NULL ) 
    {
        options.file_name = argv[ opt_ind ];
        options.template_name = argv[ opt_ind + 1 ];    
    }
    
    else
    {
        usage(progname);
        exit(EXIT_FAILURE);
    }

    if ( h52jpeg(options) < 0 )
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
    printf("   -V, --version           Print version number and exit\n");
    printf("   -i, --image             Image name (full path in HDF5 file)\n");
    printf("   -t T, --type=T          Type of image (8bit or 24bit)\n");
    
    printf("\n");
    
    printf("  T - is a string, either <8bit> or <24bit>\n");
    
}

/*-------------------------------------------------------------------------
 * Function: h52jpeg
 *
 * Parameters: options at command line
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
    hid_t          fid;
    trav_table_t   *travt = NULL;
    hsize_t        width;
    hsize_t        height;
    hsize_t        planes;
    char           interlace[20];
    hssize_t       npals;
    void           *buf=NULL;
    H5T_class_t    tclass;
    hid_t          sid;
    hid_t          did;
    hid_t          tid;
    int            rank;
    hsize_t        dims[H5S_MAX_RANK];
    hsize_t        maxdim[H5S_MAX_RANK];
    size_t         size;
    hsize_t        nelmts;
    char*          name;
    size_t         i;
    int            j;
    int            done;
    char           jpeg_name[1024];

    /* open the HDF5 file */
    if (( fid = h5tools_fopen(opt.file_name, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, (size_t)0)) < 0) 
    {
        error_msg(progname, "cannot open file <%s>\n", opt.file_name );
        return -1;
    }
    
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

            name = travt->objs[i].name;
            strcpy( jpeg_name, opt.template_name );
            strcat( jpeg_name, name );

            done = 0;

            if ( opt.verbose)
                printf("%s ...", name );
            
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
                
                free( buf );
                buf = NULL;
                
                
                
            }

            /*-------------------------------------------------------------------------
            * regular dataset
            *-------------------------------------------------------------------------
            */

            else
            {

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
                    
                    
                    free( buf );
                    buf = NULL;
                    
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
            
            
            
            break; /* H5TRAV_TYPE_DATASET */
            
            
            
        } /* switch */
        
        
        
    } /* i */

    
    
    /* free table */
    trav_table_free(travt);
    
    
    /* close */
    if ( H5Fclose(fid) < 0 )
        return -1;
    
    return 0;
    
out:
    H5E_BEGIN_TRY 
    {

        H5Sclose(sid);
        H5Tclose(tid);
        H5Dclose(did);
        H5Fclose(fid);
        
    } H5E_END_TRY;

    if ( buf != NULL )
        free( buf );
    
    
    return -1;
}


