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
#include "h5tools_utils.h"


const char *progname = "h52jpeg";
static void usage(const char *prog);

/*
 * command-line options: The user can specify short or long-named parameters
 */
static const char *s_opts = "hVvi:t:";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "version", no_arg, 'V' },
    { "verbose", no_arg, 'v' },
    { "image", require_arg, 'i' },
    { "type", require_arg, 't' },
    { NULL, 0, '\0' }
};




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
    int   opt;
    char  *image_name = NULL;
    char  *image_type = NULL;
    char  *file_name = NULL;
    char  *template_name = NULL;
    int   verbose = 0;
    
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
            verbose = 1;
            break;        
        case 'i':
            image_name = argv[ opt_ind ];
            break;
        case 't':
            image_type = argv[ opt_ind ];
            break;
            
        } /* switch */
        
        
    } /* while */
    
    /* check for file names to be processed */
    if ( argv[ opt_ind ] != NULL && argv[ opt_ind + 1 ] != NULL ) 
    {
        file_name = argv[ opt_ind ];
        template_name = argv[ opt_ind + 1 ];    
    }
    
    else
    {
        usage(progname);
        exit(EXIT_FAILURE);
    }
    
    
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


