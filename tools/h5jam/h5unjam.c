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
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>

#ifdef H5_HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "hdf5.h"
#include "H5private.h"
#include "h5tools_utils.h"

#define TRUE 1
#define FALSE 0

hsize_t write_pad( int , hsize_t );
hsize_t compute_pad( hsize_t );
hsize_t copy_to_file( int , int , ssize_t, ssize_t );

const char  *progname = "h5unjam";
int          d_status = EXIT_SUCCESS;
int do_delete = FALSE;
char *output_file = NULL;
char *input_file = NULL;
char *ub_file = NULL;

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
static const char *s_opts = "hu:i:o:d:V";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "hel", no_arg, 'h' },
  {"i", require_arg, 'i'},	/* input file */
  {"u", require_arg, 'u'},	/* user block file */
  {"o", require_arg, 'o'},	/* output file */
  {"delete", no_arg, 'd'},	/* delete ub */
  {"delet", no_arg, 'd'},
  {"dele", no_arg, 'd'},
  {"del", no_arg, 'd'},
  {"de", no_arg, 'd'},
    { NULL, 0, '\0' }
};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message
 *
 * Return:      void
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    fflush(stdout);
    fprintf(stdout, "usage: %s -i h5_file -o user_block_file_out -o h5_file_out [-d | --delete]\n", prog);
    fprintf(stdout, "           Extract user block from 'h5_file' into 'user_block_file'\n");
    fprintf(stdout, "           and HDF5 file into 'h5_file_out'\n");

    fprintf(stdout, "       %s -h\n",prog);
    fprintf(stdout, "           Print a usage message and exit\n");
    fprintf(stdout, "       %s -V \n", prog);
    fprintf(stdout, "           Print HDF5 library version and exit\n");
}

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse the command line for the h5dumper.
 *
 * Return:      Success:
 *
 *              Failure:    Exits program with EXIT_FAILURE value.
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
parse_command_line(int argc, const char *argv[])
{
    int                 opt  = FALSE;

    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
	case 'o':
	  output_file = HDstrdup (opt_arg);
	  break;
	case 'i':
	  input_file = HDstrdup (opt_arg);
	  break;
	case 'u':
	  ub_file = HDstrdup (opt_arg);
	  break;
	case 'd':
	  do_delete = TRUE;
	  break;
    case 'h':
        usage(progname);
        exit(EXIT_SUCCESS);
    case 'V':
        print_version (progname);
        exit (EXIT_SUCCESS);
    case '?':
    default:
        usage(progname);
        exit(EXIT_FAILURE);
        }
    }

    /* check for file name to be processed */
/*
    if (argc <= opt_ind+2) {
        error_msg(progname, "missing file name\n");
        usage(progname);
        exit(EXIT_FAILURE);
    }
*/
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 user block unjammer
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, const char *argv[])
{
    int   ifid;
    int   ufid;
    int   h5fid;
    void               *edata;
    H5E_auto_t          func;
    hid_t               ifile;
    off_t fsize;
    hsize_t usize;
    htri_t testval;
    herr_t status;
    hid_t plist;
    int res;
    struct stat sbuf;

    /* Disable error reporting */
    H5Eget_auto(&func, &edata);
    H5Eset_auto(NULL, NULL);


    parse_command_line(argc, argv);

    testval = H5Fis_hdf5(input_file);

    if (testval <= 0) {
        error_msg(progname, "Input HDF5 file is not HDF \"%s\"\n", input_file);
        exit(EXIT_FAILURE);
    }

    ifile = H5Fopen(input_file, H5F_ACC_RDONLY , H5P_DEFAULT);

    if (ifile < 0) {
        error_msg(progname, "Can't open input HDF5 file \"%s\"\n", input_file);
        exit(EXIT_FAILURE);
    }

    plist = H5Fget_create_plist(ifile);
    if (plist < 0) {
        error_msg(progname, "Can't get file creation plist for file \"%s\"\n", input_file);
        exit(EXIT_FAILURE);
    }

    status =  H5Pget_userblock(plist, & usize  );
    if (status < 0) {
        error_msg(progname, "Can't get user block for file \"%s\"\n", input_file);
        exit(EXIT_FAILURE);
    }

    if (usize == 0) {
	/* no user block to remove: message? */
        error_msg(progname, "\"%s\" has no user block: no change to file\n", input_file);
        exit(EXIT_SUCCESS);

    }

    res = stat(input_file, &sbuf);

    if (res < 0) {
        error_msg(progname, "Can't stat file \"%s\"\n", input_file);
        exit(EXIT_FAILURE);
    }

    fsize = sbuf.st_size;

    ifid = HDopen(input_file,O_RDONLY,0);

    if (ifid < 0) {
        error_msg(progname, "unable to open input HDF5 file \"%s\"\n", input_file);
        exit(EXIT_FAILURE);
    }

    if (do_delete && (ub_file != NULL)) {
            error_msg(progname, "??\"%s\"\n", ub_file);
            exit(EXIT_FAILURE);
    }

    if (ub_file == NULL) {
	/* write to sdtout */
	ufid = HDdup(1);
    } else {
        ufid = HDopen(ub_file,O_WRONLY|O_CREAT|O_TRUNC, 0644 );

        if (ufid < 0) {
            error_msg(progname, "unable to open user block file for output\"%s\"\n", ub_file);
            exit(EXIT_FAILURE);
        }
    }

    if (output_file == NULL) {
        h5fid = HDopen(input_file,O_WRONLY, 0);

        if (h5fid < 0) {
            error_msg(progname, "unable to open output HDF5 file \"%s\"\n", input_file);
            exit(EXIT_FAILURE);
        }
    } else {
        h5fid = HDopen(output_file,O_WRONLY|O_CREAT|O_TRUNC, 0644 );

        if (h5fid < 0) {
            error_msg(progname, "unable to open output HDF5 file \"%s\"\n", output_file);
            exit(EXIT_FAILURE);
        }
    }


    /* copy from 0 to 'usize - 1' into ufid */
    if (!do_delete) {
	    copy_to_file( ifid, ufid, 0, (ssize_t) usize);
    }

    /* copy from usize to end of file into h5fid,
     * starting at end of user block if present
     */
    copy_to_file( ifid, h5fid, (ssize_t) usize, (ssize_t)(fsize - (ssize_t)usize) );


    HDclose(ufid);
    HDclose(h5fid);
    HDclose(ifid);

    return d_status;
}

/*
 *  Copy 'how_much' bytes from the input file to the output file,
 *  starting at byte 'where' in the input file.
 *
 *  Returns the size of the output file.
 */
hsize_t
copy_to_file( int infid, int ofid, ssize_t where, ssize_t how_much )
{
    char buf[1024];
    off_t to;
    off_t from;
    ssize_t nchars = -1;

    /* nothing to copy */
    if(how_much <= 0)
        return(where);

    from = where;
    to = 0;

    while( how_much > 0) {
        HDlseek(infid,from,SEEK_SET);
        if (how_much > 512)
            nchars = HDread(infid,buf,(unsigned)512);
        else
            nchars = HDread(infid,buf,(unsigned)how_much);
        HDlseek(ofid,to,SEEK_SET);
        HDwrite(ofid,buf,(unsigned)nchars);
        how_much -= nchars;
        from += nchars;
        to += nchars;
    }

    return (where + how_much);
}

