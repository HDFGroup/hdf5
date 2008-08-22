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
#include <ctype.h>

#include "h5tools_utils.h"
#include "h5repack.h"


static void usage(const char *prog);
static int  parse_number(char *str);
static void parse_command_line(int argc, const char* argv[], pack_opt_t* options);
static void read_info(const char *filename,pack_opt_t *options);


/* module-scoped variables */
const char *progname = "h5repack";
int d_status = EXIT_SUCCESS;
static int has_i_o = 0;


/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char *s_opts = "hVvf:l:m:e:nLc:d:s:u:b:";
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
    { "indexed", require_arg, 'd' },
    { "ssize", require_arg, 's' },
    { "ublock", require_arg, 'u' },
    { "block", require_arg, 'b' },
    { NULL, 0, '\0' }
};




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
 *   PVN, November 28, 2007
 *    added support for multiple global filters
 *   PVN, May 16, 2008
 *    added  backward compatibility for -i infile -o outfile 
 *   PVN, August 20, 2008
 *    add a user block to file if requested (new switches -u -b) 
 *-------------------------------------------------------------------------
 */
int main(int argc, char **argv)
{
    char          *infile  = NULL;
    char          *outfile = NULL;
    pack_opt_t    options;            /*the global options */
    int           ret=-1;
    int           i;
    
    /* initialize options  */
    h5repack_init (&options,0);

    /* detect -i or -o for file names */
    for ( i = 1; i < argc; i++)
    {
        
        if (strcmp(argv[i], "-i") == 0) 
        {
            has_i_o = 1;
        }
        if (strcmp(argv[i], "-o") == 0)
        {
            has_i_o = 1;
        }
    }

    if (has_i_o)
    {
        
        for ( i = 1; i < argc; i++)
        {
            if (strcmp(argv[i], "-h") == 0) 
            {
                usage(progname);
                exit(0);
            }
            if (strcmp(argv[i], "-i") == 0) 
            {
                infile = argv[++i];
            }
            else if (strcmp(argv[i], "-o") == 0) 
            {
                outfile = argv[++i];
            }
            else if (strcmp(argv[i], "-v") == 0) 
            {
                options.verbose = 1;
            }
            else if (strcmp(argv[i], "-f") == 0) 
            {
                
                /* add the -f filter option */
                if (h5repack_addfilter(argv[i+1],&options)<0)
                {
                    error_msg(progname, "in parsing filter\n");
                    exit(1);
                }
                
                /* jump to next */
                ++i;
            }
            else if (strcmp(argv[i], "-l") == 0) 
            {
                
                /* parse the -l layout option */
                if (h5repack_addlayout(argv[i+1],&options)<0)
                {
                    error_msg(progname, "in parsing layout\n");
                    exit(1);
                }
                
                /* jump to next */
                ++i;
            }
            
            else if (strcmp(argv[i], "-m") == 0) 
            {
                options.threshold = parse_number(argv[i+1]);
                if ((int)options.threshold==-1) 
                {
                    error_msg(progname, "invalid treshold size <%s>\n",argv[i+1]);
                    exit(1);
                }
                ++i;
            }
            
            else if (strcmp(argv[i], "-e") == 0) 
            {
                read_info(argv[++i],&options);
            }
            else if (strcmp(argv[i], "-n") == 0) 
            {
                options.use_native = 1;
            }

            else if (strcmp(argv[i], "-L") == 0) 
            {
                options.latest = 1;
            }
            else if (strcmp(argv[i], "-c") == 0) 
            {
                options.grp_compact = atoi( argv[++i] );
                if (options.grp_compact>0)
                    options.latest = 1; /* must use latest format */ 
            }
            else if (strcmp(argv[i], "-d") == 0) 
            {
                options.grp_indexed = atoi( argv[++i] );
                if (options.grp_indexed>0)
                    options.latest = 1; /* must use latest format */ 
            }
            else if (strcmp(argv[i], "-s") == 0) 
            {
                
                char *s = argv[++i];
                int idx = 0;
                int ssize = 0;
                char *msgPtr = strchr( s, ':');
                options.latest = 1; /* must use latest format */
                if (msgPtr == NULL) 
                {
                    ssize = atoi( s );
                    for (idx=0; idx<5; idx++)
                        options.msg_size[idx] = ssize; 
                }
                else 
                {
                    char msgType[10];
                    strcpy(msgType, msgPtr+1);
                    msgPtr[0] = '\0';
                    ssize = atoi( s );
                    if (strncmp(msgType, "dspace",6) == 0) {
                        options.msg_size[0] = ssize;
                    }
                    else if (strncmp(msgType, "dtype",5) == 0) {
                        options.msg_size[1] = ssize;
                    }
                    else if (strncmp(msgType, "fill",4) == 0) {
                        options.msg_size[2] = ssize;
                    }
                    else if (strncmp(msgType, "pline",5) == 0) {
                        options.msg_size[3] = ssize;
                    }
                    else if (strncmp(msgType, "attr",4) == 0) {
                        options.msg_size[4] = ssize;
                    }
                }            
                
            }

  
            
            else if (argv[i][0] == '-') {
                error_msg(progname, " - is not a valid argument\n");
                usage(progname);
                exit(1);
            }
        }
        
        if (infile == NULL || outfile == NULL)
        {
            error_msg(progname, "file names missing\n");
            usage(progname);
            exit(1);
        }
        
    }
    
    else
        
    {
        
        parse_command_line(argc, argv, &options);
        
        
        
        if ( argv[ opt_ind ] != NULL && argv[ opt_ind + 1 ] != NULL ) 
        {
            infile = argv[ opt_ind ];
            outfile = argv[ opt_ind + 1 ];
            
            if ( strcmp( infile, outfile ) == 0 )
            {
                error_msg(progname, "file names cannot be the same\n");
                usage(progname);
                exit(EXIT_FAILURE);
                
            }
        }
        
        else
        {
            error_msg(progname, "file names missing\n");
            usage(progname);
            exit(EXIT_FAILURE);
        }
        
    }


   
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
 printf("   -d L2, --indexed=L2     Minimum number of links in the indexed format\n");
 printf("   -s S[:F], --ssize=S[:F] Shared object header message minimum size\n");
 printf("   -m T, --threshold=T     Do not apply the filter to datasets smaller than T\n");
 printf("   -e M, --file=M          Name of file M with the -f and -l options\n");
 printf("   -u U, --ublock=U        Name of file U with user block data to be added\n");
 printf("   -b D, --block=D         Size of user block to be added\n");
 printf("   -f FILT, --filter=FILT  Filter type\n");
 printf("   -l LAYT, --layout=LAYT  Layout type\n");
 
 printf("\n");

 printf("  T - is an integer greater than 1, size of dataset in bytes \n");
 printf("  M - is a filename.\n");
 printf("  U - is a filename.\n");
 printf("  S - is an integer\n");
 printf("  D - is the user block size (any power of 2 equal to 512 or greater)\n");
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
 printf("5) h5repack -f SHUF -f GZIP=1 file1 file2 \n");
 printf("\n");
 printf("   Add both filters SHUF and GZIP in this order to all datasets\n");
 printf("\n");
}


/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *
 *-------------------------------------------------------------------------
 */

static void parse_command_line(int argc, const char* argv[], pack_opt_t* options)
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
        
        
        case 'd':

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
                    if (strncmp(msgType, "dspace",6) == 0) {
                        options->msg_size[0] = ssize;
                    }
                    else if (strncmp(msgType, "dtype", 5) == 0) {
                        options->msg_size[1] = ssize;
                    }
                    else if (strncmp(msgType, "fill", 4) == 0) {
                        options->msg_size[2] = ssize;
                    }
                    else if (strncmp(msgType, "pline", 5) == 0) {
                        options->msg_size[3] = ssize;
                    }
                    else if (strncmp(msgType, "attr", 4) == 0) {
                        options->msg_size[4] = ssize;
                    }
                }            
            }
            
            break;

            
        case 'u':
            
            options->ublock_filename = opt_arg;
            break;
            
        case 'b':
            
            options->ublock_size = atoi( opt_arg );
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

/*-------------------------------------------------------------------------
 * Function: parse_number
 *
 * Purpose: read a number from command line argument
 *
 * Return: number, -1 for FAIL
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September, 23, 2003
 *
 *-------------------------------------------------------------------------
 */


int parse_number(char *str)
{
    unsigned    i;
    int         n;
    char        c;
    size_t      len=strlen(str);
    
    for ( i=0; i<len; i++)
    {
        c = str[i];
        if (!isdigit(c)){
            return -1;
        }
    }
    str[i]='\0';
    n=atoi(str);
    return n;
}


/*-------------------------------------------------------------------------
 * Function: read_info
 *
 * Purpose: read comp and chunk options from a file
 *
 * Return: void, exit on error
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September, 22, 2003
 *
 *-------------------------------------------------------------------------
 */

static
void read_info(const char *filename,
               pack_opt_t *options)
{
    
    char stype[10];
    char comp_info[1024];
    FILE *fp;
    char c;
    int  i, rc=1;
    char  *srcdir = getenv("srcdir"); /* the source directory */
    char  data_file[512]="";          /* buffer to hold name of existing file */
    
    /* compose the name of the file to open, using the srcdir, if appropriate */
    if (srcdir){
        strcpy(data_file,srcdir);
        strcat(data_file,"/");
    }
    strcat(data_file,filename);
    
    
    if ((fp = fopen(data_file, "r")) == (FILE *)NULL) {
        error_msg(progname, "cannot open options file %s\n", filename);
        exit(1);
    }
    
    /* cycle until end of file reached */
    while( 1 )
    {
        rc=fscanf(fp, "%s", stype);
        if (rc==-1)
            break;
        
       /*-------------------------------------------------------------------------
        * filter
        *-------------------------------------------------------------------------
        */
        if (strcmp(stype,"-f") == 0) {
            
            /* find begining of info */
            i=0; c='0';
            while( c!=' ' )
            {
                fscanf(fp, "%c", &c);
                if (feof(fp)) break;
            }
            c='0';
            /* go until end */
            while( c!=' ' )
            {
                fscanf(fp, "%c", &c);
                comp_info[i]=c;
                i++;
                if (feof(fp)) break;
                if (c==10 /*eol*/) break;
            }
            comp_info[i-1]='\0'; /*cut the last " */
            
            if (h5repack_addfilter(comp_info,options)==-1){
                error_msg(progname, "could not add compression option\n");
                exit(1);
            }
        }
        /*-------------------------------------------------------------------------
        * layout
        *-------------------------------------------------------------------------
        */
        else if (strcmp(stype,"-l") == 0) {
            
            /* find begining of info */
            i=0; c='0';
            while( c!=' ' )
            {
                fscanf(fp, "%c", &c);
                if (feof(fp)) break;
            }
            c='0';
            /* go until end */
            while( c!=' ' )
            {
                fscanf(fp, "%c", &c);
                comp_info[i]=c;
                i++;
                if (feof(fp)) break;
                if (c==10 /*eol*/) break;
            }
            comp_info[i-1]='\0'; /*cut the last " */
            
            if (h5repack_addlayout(comp_info,options)==-1){
                error_msg(progname, "could not add chunck option\n");
                exit(1);
            }
        }
        /*-------------------------------------------------------------------------
        * not valid
        *-------------------------------------------------------------------------
        */
        else {
            error_msg(progname, "bad file format for %s", filename);
            exit(1);
        }
    }
    
    fclose(fp);
    return;
}
