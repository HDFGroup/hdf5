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
#include <string.h>
#include "h5tools_utils.h"
#include "h5repack.h"

#if 0
#define OLD
#endif

#if defined (OLD)
static void usage(void);
#else
static void usage(const char *prog);
#endif


/* module-scoped variables */
const char *progname = "h5repack";
int d_status = EXIT_SUCCESS;

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char *s_opts = "hVvf:l:m:e:nLc:i:s:";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "version", no_arg, 'V' },
    { "verbose", no_arg, 'v' },
    { "filter", require_arg, 'f' },
    { "layout", require_arg, 'l' },
    { "threshold", require_arg, 'm' },
    { "file", require_arg, 'e' },
    { "native", no_arg, 'n' },
    { "latest", no_arg, 'L' },
    { "compact", require_arg, 'c' },
    { "indexed", require_arg, 'i' },
    { "ssize", require_arg, 's' },
    { NULL, 0, '\0' }
};

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *
 *-------------------------------------------------------------------------
 */

#if 0
#define OLD
#endif

#if defined (OLD)


void parse_command_line(int argc, 
                        const char* argv[], 
                        char** fname1, 
                        char** fname2,
                        pack_opt_t* options)
{
    
    int i;
    
    if (argc<2)
    {
        usage();
        exit(1);
    }
    
    for ( i = 1; i < argc; i++)
    {
        if (strcmp(argv[i], "-h") == 0) {
            usage();
            exit(0);
        }
        else if (strcmp(argv[i], "-V") == 0) {
            print_version(progname);
            exit(0);
        }
        else if (strcmp(argv[i], "-i") == 0) {
            *fname1 = argv[++i];
        }
        else if (strcmp(argv[i], "-o") == 0) {
            *fname2 = argv[++i];
        }
        else if (strcmp(argv[i], "-v") == 0) {
            options->verbose = 1;
        }
        else if (strcmp(argv[i], "-f") == 0) {
            
            /* add the -f filter option */
            if (h5repack_addfilter(argv[i+1], options)<0)
            {
                error_msg(progname, "in parsing filter\n");
                exit(1);
            }
            
            /* jump to next */
            ++i;
        }
        else if (strcmp(argv[i], "-l") == 0) {
            
            /* parse the -l layout option */
            if (h5repack_addlayout(argv[i+1], options)<0)
            {
                error_msg(progname, "in parsing layout\n");
                exit(1);
            }
            
            /* jump to next */
            ++i;
        }
        
        else if (strcmp(argv[i], "-m") == 0) {
            options->threshold = parse_number(argv[i+1]);
            if ((int)options->threshold==-1) {
                error_msg(progname, "invalid treshold size <%s>\n",argv[i+1]);
                exit(1);
            }
            ++i;
        }
        
        else if (strcmp(argv[i], "-e") == 0) {
            read_info(argv[++i], options);
        }
        else if (strcmp(argv[i], "-n") == 0) {
            options->use_native = 1;
        }
        
        else if ( (strcmp(argv[i], "-L") == 0) || (strcmp(argv[i], "--latest") == 0)) {
            options->latest = 1; 
        }
        
        else if ( strncmp(argv[i], "-compact=", 9) == 0 ) {
            options->grp_compact = atoi(argv[i]+9);
            if (options->grp_compact>0)
                options->latest = 1; /* must use latest format */
        }
        
        else if ( strncmp(argv[i], "-indexed=", 9) == 0 ) {
            options->grp_indexed = atoi(argv[i]+9);
            if (options->grp_indexed>0)
                options->latest = 1; /* must use latest format */
        }
        
        else if ( strncmp(argv[i], "-ssize=", 7) == 0 ) {
            int idx = 0;
            int ssize = 0;
            char *msgPtr = strchr(argv[i]+7, ':');
            options->latest = 1; /* must use latest format */
            if (msgPtr == NULL) {
                ssize = atoi(argv[i]+7);
                for (idx=0; idx<5; idx++)
                    options->msg_size[idx] = ssize; 
            }
            else {
                char msgType[10];
                strcpy(msgType, msgPtr+1);
                msgPtr[0] = '\0';
                ssize = atoi(argv[i]+7);
                if (strcmp(msgType, "dspace") == 0) {
                    options->msg_size[0] = ssize;
                }
                else if (strcmp(msgType, "dtype") == 0) {
                    options->msg_size[1] = ssize;
                }
                else if (strcmp(msgType, "fill") == 0) {
                    options->msg_size[2] = ssize;
                }
                else if (strcmp(msgType, "pline") == 0) {
                    options->msg_size[3] = ssize;
                }
                else if (strcmp(msgType, "attr") == 0) {
                    options->msg_size[4] = ssize;
                }
            }
        } /*  else if ( strncmp(argv[i], "-ssize=", 7) == 0 ) */
        
        else if (argv[i][0] == '-') {
            error_msg(progname, " - is not a valid argument\n");
            usage();
            exit(1);
        }
        
 }
 
 
}

#else

void parse_command_line(int argc, 
                        const char* argv[], 
                        pack_opt_t* options)
{
    
    int opt;
   
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
            options->verbose = 1;
            break;
        case 'f':
            
            /* parse the -f filter option */
            if (h5repack_addfilter( opt_arg, options)<0)
            {
                error_msg(progname, "in parsing filter\n");
                exit(EXIT_FAILURE);
            }
            break;
        case 'l':
            
            /* parse the -l layout option */
            if (h5repack_addlayout( opt_arg, options)<0)
            {
                error_msg(progname, "in parsing layout\n");
                exit(EXIT_FAILURE);
            }
            break;
            

        case 'm':

            options->threshold = parse_number( opt_arg );
            if ((int)options->threshold==-1) 
            {
                error_msg(progname, "invalid treshold size <%s>\n", opt_arg );
                exit(EXIT_FAILURE);
            }
            break;
        
        case 'e':
            read_info( opt_arg, options);
            break;

        case 'n':
            options->use_native = 1;
            break;
        
        case 'L':
            options->latest = 1; 
            break;
        
        case 'c':

            options->grp_compact = atoi( opt_arg );
            if (options->grp_compact>0)
                options->latest = 1; /* must use latest format */
            break;
        
        
        case 'i':

            options->grp_indexed = atoi( opt_arg );
            if (options->grp_indexed>0)
                options->latest = 1; /* must use latest format */
            break;

        case 's':

            {
                
                int idx = 0;
                int ssize = 0;
                char *msgPtr = strchr( opt_arg, ':');
                options->latest = 1; /* must use latest format */
                if (msgPtr == NULL) 
                {
                    ssize = atoi( opt_arg );
                    for (idx=0; idx<5; idx++)
                        options->msg_size[idx] = ssize; 
                }
                else 
                {
                    char msgType[10];
                    strcpy(msgType, msgPtr+1);
                    msgPtr[0] = '\0';
                    ssize = atoi( opt_arg );
                    if (strcmp(msgType, "dspace") == 0) {
                        options->msg_size[0] = ssize;
                    }
                    else if (strcmp(msgType, "dtype") == 0) {
                        options->msg_size[1] = ssize;
                    }
                    else if (strcmp(msgType, "fill") == 0) {
                        options->msg_size[2] = ssize;
                    }
                    else if (strcmp(msgType, "pline") == 0) {
                        options->msg_size[3] = ssize;
                    }
                    else if (strcmp(msgType, "attr") == 0) {
                        options->msg_size[4] = ssize;
                    }
                }            
            }
            
            break;

        } /* switch */
        
        
    } /* while */
    
    /* check for file names to be processed */
    if (argc <= opt_ind || argv[ opt_ind + 1 ] == NULL) 
    {
        error_msg(progname, "missing file names\n");
        usage(progname);
        exit(EXIT_FAILURE);
    }
    
    

 
 
}


#endif

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h5repack main program
 *
 * Return: 1, error, 0, no error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications: 
 *  July 2004: Introduced the extra EC or NN option for SZIP
 *  October 2006: Added a new switch -n, that allows to write the dataset 
 *                using a native type. The default to write is the file type.
 *
 * Modification:
 *   Peter Cao, June 13, 2007
 *    Add "-L, --latest" option to pack a file with the latest file format
 *   PVN, November 19, 2007
 *    adopted the syntax h5repack [OPTIONS]  file1 file2
 *-------------------------------------------------------------------------
 */
int main(int argc, char **argv)
{
    char          *infile  = NULL;
    char          *outfile = NULL;
    pack_opt_t    options;            /*the global options */
    int           ret;
    
    /* initialize options  */
    h5repack_init (&options,0);

#if defined (OLD)
    
    parse_command_line(argc, argv, &infile, &outfile, &options);
    
   
#else

    parse_command_line(argc, argv, &options);
    
    if ( argv[ opt_ind ] != NULL && argv[ opt_ind + 1 ] != NULL ) 
    {
        infile = argv[ opt_ind ];
        outfile = argv[ opt_ind + 1 ];
    }
    
    else
    {
        error_msg(progname, "file names missing\n");
        usage(progname);
        exit(EXIT_FAILURE);
    }

#endif
    
    /* pack it */
    ret=h5repack(infile,outfile,&options);
    
    /* free tables */
    h5repack_end(&options);
    
    if (ret==-1)
        return 1;
    else
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

#if defined OLD
static void usage(void)
{
 printf("usage: h5repack [-h] [-v] [-V] [-n] [-L] [-c] [-i] [-s] [-m size] [-e file] [-f FILTER] [-l LAYOUT] file1 file2\n");
 printf("\n");
 printf("file1          Input HDF5 File\n");
 printf("file2          Output HDF5 File\n");
 printf("[-h]           Print this message\n");
 printf("[-v]           Verbose mode, print object information\n");
 printf("[-V]           Print HDF5 version number and exit\n");
 printf("[-n]           Use a native HDF5 type when repacking. Default is the file type\n");
 printf("[-L, --latest]      Use latest version of file format to create groups, datasets and datatypes\n");
 printf("[-c <size>, --compact=<size>]      Set the maximum number of links to store as header messages in a group\n");
 printf("[-i <size>, --indexed=<size>]      Set the minimum number of links to store in the indexed format\n");
 printf("[-s, --ssize=<size>[:<dspace|dtype|fill|pline|attr>]]      Set the shared object header message minimum size\n");
 printf("[-m size]      Do not apply the filter to objects smaller than size\n");
 printf("[-e file]      Name of file with the -f and -l options\n");
 printf("[-f FILTER]    Filter type\n");
 printf("[-l LAYOUT]    Layout type\n");
 printf("\n");
 printf("FILTER is a string with the format:\n");
 printf("\n");
 printf("    <list of objects>:<name of filter>=<filter parameters>\n");
 printf("\n");
 printf("    <list of objects> is a comma separated list of object names, meaning apply\n");
 printf("      compression only to those objects. If no names are specified, the filter\n");
 printf("      is applied to all objects\n");
 printf("    <name of filter> can be:\n");
 printf("      GZIP, to apply the HDF5 GZIP filter (GZIP compression)\n");
 printf("      SZIP, to apply the HDF5 SZIP filter (SZIP compression)\n");
 printf("      SHUF, to apply the HDF5 shuffle filter\n");
 printf("      FLET, to apply the HDF5 checksum filter\n");
 printf("      NBIT, to apply the HDF5 NBIT filter (NBIT compression)\n");
 printf("      SOFF, to apply the HDF5 Scale/Offset filter\n");
 printf("      NONE, to remove all filters\n");
 printf("    <filter parameters> is optional filter parameter information\n");
 printf("      GZIP=<deflation level> from 1-9\n");
 printf("      SZIP=<pixels per block,coding> pixels per block is a even number in\n");
 printf("            2-32 and coding method is either EC or NN\n");
 printf("      SHUF (no parameter)\n");
 printf("      FLET (no parameter)\n");
 printf("      NBIT (no parameter)\n");
 printf("      SOFF=<scale_factor,scale_type> scale_factor is an integer and scale_type\n");
 printf("            is either IN or DS\n");
 printf("      NONE (no parameter)\n");
 printf("\n");
 printf("LAYOUT is a string with the format:\n");
 printf("\n");
 printf("    <list of objects>:<layout type>=<layout parameters>\n");
 printf("\n");
 printf("    <list of objects> is a comma separated list of object names, meaning that\n");
 printf("      layout information is supplied for those objects. If no names are\n");
 printf("      specified, the layout type is applied to all objects\n");
 printf("    <layout type> can be:\n");
 printf("      CHUNK, to apply chunking layout\n");
 printf("      COMPA, to apply compact layout\n");
 printf("      CONTI, to apply continuous layout\n");
 printf("    <layout parameters> is optional layout information\n");
 printf("      CHUNK=DIM[xDIM...xDIM], the chunk size of each dimension\n");
 printf("      COMPA (no parameter)\n");
 printf("      CONTI (no parameter)\n");
 printf("\n");
 printf("Examples of use:\n");
 printf("\n");
 printf("1) h5repack -v -i file1 -o file2 -f GZIP=1\n");
 printf("\n");
 printf("   GZIP compression with level 1 to all objects\n");
 printf("\n");
 printf("2) h5repack -v -i file1 -o file2 -f A:SZIP=8,NN\n");
 printf("\n");
 printf("   SZIP compression with 8 pixels per block and NN coding method to object A\n");
 printf("\n");
 printf("3) h5repack -v -i file1 -o file2 -l A,B:CHUNK=20x10 -f C,D,F:NONE\n");
 printf("\n");
 printf("   Chunked layout, with a layout size of 20x10, to objects A and B\n");
 printf("   and remove filters to objects C, D, F\n");
 printf("\n");
 printf("4) h5repack -L -compact=10 -ssize=20:dtype -i file1 -o file2 \n");
 printf("\n");
 printf("   Using latest file format with maximum compact group size fo 10 and minimum shared datatype size of 20\n");
 printf("\n");
}

#else

static void usage(const char *prog)
{
 printf("usage: %s [OPTIONS] file1 file2\n", prog);
 printf("  file1                    Input HDF5 File\n");
 printf("  file2                    Output HDF5 File\n");
 printf("  OPTIONS\n");
 printf("   -h, --help              Print a usage message and exit\n");
 printf("   -v, --verbose           Verbose mode, print object information\n");
 printf("   -V, --version           Print version number and exit\n");
 printf("   -n, --native            Use a native HDF5 type when repacking\n");
 printf("   -L, --latest            Use latest version of file format\n");
 printf("   -c L1, --compact=L1     Maximum number of links in header messages\n");
 printf("   -i L2, --indexed=L2     Minimum number of links in the indexed format\n");
 printf("   -s S[:F], --ssize=S[:F] Shared object header message minimum size\n");
 printf("   -m T, --threshold=T     Do not apply the filter to datasets smaller than T\n");
 printf("   -e M, --file=M          Name of file M with the -f and -l options\n");
 printf("   -f FILT, --filter=FILT  Filter type\n");
 printf("   -l LAYT, --layout=LAYT  Layout type\n");
 
 printf("\n");

 printf("  T - is an integer greater than 1, size of dataset in bytes \n");
 printf("  M - is a filename.\n");
 printf("  F - is the shared object header message type, any of <dspace|dtype|fill|\n");
 printf("        pline|attr>. If F is not specified, S applies to all messages\n");

 printf("\n");

 printf("  FILT - is a string with the format:\n");
 printf("\n");
 printf("    <list of objects>:<name of filter>=<filter parameters>\n");
 printf("\n");
 printf("    <list of objects> is a comma separated list of object names, meaning apply\n");
 printf("      compression only to those objects. If no names are specified, the filter\n");
 printf("      is applied to all objects\n");
 printf("    <name of filter> can be:\n");
 printf("      GZIP, to apply the HDF5 GZIP filter (GZIP compression)\n");
 printf("      SZIP, to apply the HDF5 SZIP filter (SZIP compression)\n");
 printf("      SHUF, to apply the HDF5 shuffle filter\n");
 printf("      FLET, to apply the HDF5 checksum filter\n");
 printf("      NBIT, to apply the HDF5 NBIT filter (NBIT compression)\n");
 printf("      SOFF, to apply the HDF5 Scale/Offset filter\n");
 printf("      NONE, to remove all filters\n");
 printf("    <filter parameters> is optional filter parameter information\n");
 printf("      GZIP=<deflation level> from 1-9\n");
 printf("      SZIP=<pixels per block,coding> pixels per block is a even number in\n");
 printf("            2-32 and coding method is either EC or NN\n");
 printf("      SHUF (no parameter)\n");
 printf("      FLET (no parameter)\n");
 printf("      NBIT (no parameter)\n");
 printf("      SOFF=<scale_factor,scale_type> scale_factor is an integer and scale_type\n");
 printf("            is either IN or DS\n");
 printf("      NONE (no parameter)\n");
 printf("\n");
 printf("  LAYT - is a string with the format:\n");
 printf("\n");
 printf("    <list of objects>:<layout type>=<layout parameters>\n");
 printf("\n");
 printf("    <list of objects> is a comma separated list of object names, meaning that\n");
 printf("      layout information is supplied for those objects. If no names are\n");
 printf("      specified, the layout type is applied to all objects\n");
 printf("    <layout type> can be:\n");
 printf("      CHUNK, to apply chunking layout\n");
 printf("      COMPA, to apply compact layout\n");
 printf("      CONTI, to apply continuous layout\n");
 printf("    <layout parameters> is optional layout information\n");
 printf("      CHUNK=DIM[xDIM...xDIM], the chunk size of each dimension\n");
 printf("      COMPA (no parameter)\n");
 printf("      CONTI (no parameter)\n");
 printf("\n");
 printf("Examples of use:\n");
 printf("\n");
 printf("1) h5repack -v -f GZIP=1 file1 file2\n");
 printf("\n");
 printf("   GZIP compression with level 1 to all objects\n");
 printf("\n");
 printf("2) h5repack -v -f A:SZIP=8,NN file1 file2\n");
 printf("\n");
 printf("   SZIP compression with 8 pixels per block and NN coding method to object A\n");
 printf("\n");
 printf("3) h5repack -v -l A,B:CHUNK=20x10 -f C,D,F:NONE file1 file2\n");
 printf("\n");
 printf("   Chunked layout, with a layout size of 20x10, to objects A and B\n");
 printf("   and remove filters to objects C, D, F\n");
 printf("\n");
 printf("4) h5repack -L -c 10 -s 20:dtype file1 file2 \n");
 printf("\n");
 printf("   Using latest file format with maximum compact group size of 10 and\n");
 printf("   and minimum shared datatype size of 20\n");
 printf("\n");
}

#endif

