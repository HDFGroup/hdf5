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

#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5repack.h"

/* Name of tool */
#define PROGRAMNAME "h5repack"

static int parse_command_line(int argc, const char **argv, pack_opt_t* options);
static void leave(int ret) H5_ATTR_NORETURN;


/* module-scoped variables */
static int has_i_o = 0;
const char *infile = NULL;
const char *outfile = NULL;

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char *s_opts = "hVvf:l:m:e:nLc:d:s:u:b:M:t:a:i:o:E";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "version", no_arg, 'V' },
    { "verbose", no_arg, 'v' },
    { "filter", require_arg, 'f' },
    { "layout", require_arg, 'l' },
    { "minimum", require_arg, 'm' },
    { "file", require_arg, 'e' },
    { "native", no_arg, 'n' },
    { "latest", no_arg, 'L' },
    { "compact", require_arg, 'c' },
    { "indexed", require_arg, 'd' },
    { "ssize", require_arg, 's' },
    { "ublock", require_arg, 'u' },
    { "block", require_arg, 'b' },
    { "metadata_block_size", require_arg, 'M' },
    { "threshold", require_arg, 't' },
    { "alignment", require_arg, 'a' },
    { "infile", require_arg, 'i' }, /* -i for backward compability */
    { "outfile", require_arg, 'o' }, /* -o for backward compability */
    { "enable-error-stack", no_arg, 'E' },
    { NULL, 0, '\0' }
};

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print usage
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static void usage(const char *prog) {
    FLUSHSTREAM(rawoutstream);
    PRINTSTREAM(rawoutstream, "usage: %s [OPTIONS] file1 file2\n", prog);
    PRINTVALSTREAM(rawoutstream, "  file1                    Input HDF5 File\n");
    PRINTVALSTREAM(rawoutstream, "  file2                    Output HDF5 File\n");
    PRINTVALSTREAM(rawoutstream, "  OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "   -h, --help              Print a usage message and exit\n");
    PRINTVALSTREAM(rawoutstream, "   -v, --verbose           Verbose mode, print object information\n");
    PRINTVALSTREAM(rawoutstream, "   -V, --version           Print version number and exit\n");
    PRINTVALSTREAM(rawoutstream, "   -n, --native            Use a native HDF5 type when repacking\n");
    PRINTVALSTREAM(rawoutstream, "   -L, --latest            Use latest version of file format\n");
    PRINTVALSTREAM(rawoutstream, "   -c L1, --compact=L1     Maximum number of links in header messages\n");
    PRINTVALSTREAM(rawoutstream, "   -d L2, --indexed=L2     Minimum number of links in the indexed format\n");
    PRINTVALSTREAM(rawoutstream, "   -s S[:F], --ssize=S[:F] Shared object header message minimum size\n");
    PRINTVALSTREAM(rawoutstream, "   -m M, --minimum=M       Do not apply the filter to datasets smaller than M\n");
    PRINTVALSTREAM(rawoutstream, "   -e E, --file=E          Name of file E with the -f and -l options\n");
    PRINTVALSTREAM(rawoutstream, "   -u U, --ublock=U        Name of file U with user block data to be added\n");
    PRINTVALSTREAM(rawoutstream, "   -b B, --block=B         Size of user block to be added\n");
    PRINTVALSTREAM(rawoutstream, "   -M A, --metadata_block_size=A  Metadata block size for H5Pset_meta_block_size\n");
    PRINTVALSTREAM(rawoutstream, "   -t T, --threshold=T     Threshold value for H5Pset_alignment\n");
    PRINTVALSTREAM(rawoutstream, "   -a A, --alignment=A     Alignment value for H5Pset_alignment\n");
    PRINTVALSTREAM(rawoutstream, "   -f FILT, --filter=FILT  Filter type\n");
    PRINTVALSTREAM(rawoutstream, "   -l LAYT, --layout=LAYT  Layout type\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    M - is an integer greater than 1, size of dataset in bytes (default is 0) \n");
    PRINTVALSTREAM(rawoutstream, "    E - is a filename.\n");
    PRINTVALSTREAM(rawoutstream, "    S - is an integer\n");
    PRINTVALSTREAM(rawoutstream, "    U - is a filename.\n");
    PRINTVALSTREAM(rawoutstream, "    T - is an integer\n");
    PRINTVALSTREAM(rawoutstream, "    A - is an integer greater than zero\n");
    PRINTVALSTREAM(rawoutstream, "    B - is the user block size, any value that is 512 or greater and is\n");
    PRINTVALSTREAM(rawoutstream, "        a power of 2 (1024 default)\n");
    PRINTVALSTREAM(rawoutstream, "    F - is the shared object header message type, any of <dspace|dtype|fill|\n");
    PRINTVALSTREAM(rawoutstream, "        pline|attr>. If F is not specified, S applies to all messages\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "     --enable-error-stack Prints messages from the HDF5 error stack as they\n");
    PRINTVALSTREAM(rawoutstream, "                          occur.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    FILT - is a string with the format:\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects>:<name of filter>=<filter parameters>\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects> is a comma separated list of object names, meaning apply\n");
    PRINTVALSTREAM(rawoutstream, "        compression only to those objects. If no names are specified, the filter\n");
    PRINTVALSTREAM(rawoutstream, "        is applied to all objects\n");
    PRINTVALSTREAM(rawoutstream, "      <name of filter> can be:\n");
    PRINTVALSTREAM(rawoutstream, "        GZIP, to apply the HDF5 GZIP filter (GZIP compression)\n");
    PRINTVALSTREAM(rawoutstream, "        SZIP, to apply the HDF5 SZIP filter (SZIP compression)\n");
    PRINTVALSTREAM(rawoutstream, "        SHUF, to apply the HDF5 shuffle filter\n");
    PRINTVALSTREAM(rawoutstream, "        FLET, to apply the HDF5 checksum filter\n");
    PRINTVALSTREAM(rawoutstream, "        NBIT, to apply the HDF5 NBIT filter (NBIT compression)\n");
    PRINTVALSTREAM(rawoutstream, "        SOFF, to apply the HDF5 Scale/Offset filter\n");
    PRINTVALSTREAM(rawoutstream, "        UD,   to apply a user defined filter\n");
    PRINTVALSTREAM(rawoutstream, "        NONE, to remove all filters\n");
    PRINTVALSTREAM(rawoutstream, "      <filter parameters> is optional filter parameter information\n");
    PRINTVALSTREAM(rawoutstream, "        GZIP=<deflation level> from 1-9\n");
    PRINTVALSTREAM(rawoutstream, "        SZIP=<pixels per block,coding> pixels per block is a even number in\n");
    PRINTVALSTREAM(rawoutstream, "            2-32 and coding method is either EC or NN\n");
    PRINTVALSTREAM(rawoutstream, "        SHUF (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        FLET (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        NBIT (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        SOFF=<scale_factor,scale_type> scale_factor is an integer and scale_type\n");
    PRINTVALSTREAM(rawoutstream, "            is either IN or DS\n");
    PRINTVALSTREAM(rawoutstream, "        UD=<filter_number,cd_value_count,value_1[,value_2,...,value_N]>\n");
    PRINTVALSTREAM(rawoutstream, "            required values for filter_number,cd_value_count,value_1\n");
    PRINTVALSTREAM(rawoutstream, "            optional values for value_2 to value_N\n");
    PRINTVALSTREAM(rawoutstream, "        NONE (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    LAYT - is a string with the format:\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects>:<layout type>=<layout parameters>\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects> is a comma separated list of object names, meaning that\n");
    PRINTVALSTREAM(rawoutstream, "        layout information is supplied for those objects. If no names are\n");
    PRINTVALSTREAM(rawoutstream, "        specified, the layout type is applied to all objects\n");
    PRINTVALSTREAM(rawoutstream, "      <layout type> can be:\n");
    PRINTVALSTREAM(rawoutstream, "        CHUNK, to apply chunking layout\n");
    PRINTVALSTREAM(rawoutstream, "        COMPA, to apply compact layout\n");
    PRINTVALSTREAM(rawoutstream, "        CONTI, to apply contiguous layout\n");
    PRINTVALSTREAM(rawoutstream, "      <layout parameters> is optional layout information\n");
    PRINTVALSTREAM(rawoutstream, "        CHUNK=DIM[xDIM...xDIM], the chunk size of each dimension\n");
    PRINTVALSTREAM(rawoutstream, "        COMPA (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        CONTI (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "Examples of use:\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "1) h5repack -v -f GZIP=1 file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   GZIP compression with level 1 to all objects\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "2) h5repack -v -f dset1:SZIP=8,NN file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   SZIP compression with 8 pixels per block and NN coding method to object dset1\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "3) h5repack -v -l dset1,dset2:CHUNK=20x10 -f dset3,dset4,dset5:NONE file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Chunked layout, with a layout size of 20x10, to objects dset1 and dset2\n");
    PRINTVALSTREAM(rawoutstream, "   and remove filters to objects dset3, dset4, dset5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "4) h5repack -L -c 10 -s 20:dtype file1 file2 \n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Using latest file format with maximum compact group size of 10 and\n");
    PRINTVALSTREAM(rawoutstream, "   and minimum shared datatype size of 20\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "5) h5repack -f SHUF -f GZIP=1 file1 file2 \n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Add both filters SHUF and GZIP in this order to all datasets\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "6) h5repack -f UD=307,1,9 file1 file2 \n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Add bzip2 filter to all datasets\n");
    PRINTVALSTREAM(rawoutstream, "\n");
}

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown MPI & HDF5 and call exit()
 *
 * Return:      Does not return
 *
 * Programmer:  Quincey Koziol
 *              Saturday, 31. January 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void leave(int ret) {
	h5tools_close();

	HDexit(ret);
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
int read_info(const char *filename, pack_opt_t *options) {

	char stype[10];
	char comp_info[1024];
	FILE *fp = NULL;
	char c;
	int i, rc = 1;
	int ret_value = EXIT_SUCCESS;

	if ((fp = HDfopen(filename, "r")) == (FILE *) NULL) {
		error_msg("cannot open options file %s\n", filename);
		h5tools_setstatus(EXIT_FAILURE);
		ret_value = EXIT_FAILURE;
		goto done;
	}

	/* cycle until end of file reached */
	while (1) {
		rc = fscanf(fp, "%s", stype);
		if (rc == -1)
			break;

		/*-------------------------------------------------------------------------
		 * filter
		 *-------------------------------------------------------------------------
		 */
		if (HDstrcmp(stype,"-f") == 0) {
			/* find begining of info */
			i = 0;
			c = '0';
			while (c != ' ') {
				if(fscanf(fp, "%c", &c) < 0 && HDferror(fp)) {
                    error_msg("fscanf error\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = EXIT_FAILURE;
                    goto done;
                } /* end if */
				if (HDfeof(fp))
					break;
			}
			c = '0';
			/* go until end */
			while (c != ' ') {
				if(fscanf(fp, "%c", &c) < 0 && HDferror(fp)) {
                    error_msg("fscanf error\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = EXIT_FAILURE;
                    goto done;
                } /* end if */
				comp_info[i] = c;
				i++;
				if (HDfeof(fp))
					break;
				if (c == 10 /*eol*/)
					break;
			}
			comp_info[i - 1] = '\0'; /*cut the last " */

			if (h5repack_addfilter(comp_info, options) == -1) {
				error_msg("could not add compression option\n");
				h5tools_setstatus(EXIT_FAILURE);
				ret_value = EXIT_FAILURE;
				goto done;
			}
		}
		/*-------------------------------------------------------------------------
		 * layout
		 *-------------------------------------------------------------------------
		 */
		else if (HDstrcmp(stype,"-l") == 0) {

			/* find begining of info */
			i = 0;
			c = '0';
			while (c != ' ') {
				if(fscanf(fp, "%c", &c) < 0 && HDferror(fp)) {
                    error_msg("fscanf error\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = EXIT_FAILURE;
                    goto done;
                } /* end if */
				if (HDfeof(fp))
					break;
			}
			c = '0';
			/* go until end */
			while (c != ' ') {
				if(fscanf(fp, "%c", &c) < 0 && HDferror(fp)) {
                    error_msg("fscanf error\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = EXIT_FAILURE;
                    goto done;
                } /* end if */
				comp_info[i] = c;
				i++;
				if (HDfeof(fp))
					break;
				if (c == 10 /*eol*/)
					break;
			}
			comp_info[i - 1] = '\0'; /*cut the last " */

			if (h5repack_addlayout(comp_info, options) == -1) {
				error_msg("could not add chunck option\n");
				h5tools_setstatus(EXIT_FAILURE);
				ret_value = EXIT_FAILURE;
				goto done;
			}
		}
		/*-------------------------------------------------------------------------
		 * not valid
		 *-------------------------------------------------------------------------
		 */
		else {
			error_msg("bad file format for %s", filename);
			h5tools_setstatus(EXIT_FAILURE);
			ret_value = EXIT_FAILURE;
			goto done;
		}
	}

done:
	if (fp)
		HDfclose(fp);

	return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *
 *-------------------------------------------------------------------------
 */

static
int parse_command_line(int argc, const char **argv, pack_opt_t* options) {

	int opt;
	int ret_value = 0;

	/* parse command line options */
	while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
		switch ((char) opt) {

		/* -i for backward compability */
		case 'i':
			infile = opt_arg;
			has_i_o = 1;
			break;

		/* -o for backward compability */
		case 'o':
			outfile = opt_arg;
			has_i_o = 1;
			break;

		case 'h':
			usage(h5tools_getprogname());
			h5tools_setstatus(EXIT_SUCCESS);
			ret_value = -1;
			goto done;

		case 'V':
			print_version(h5tools_getprogname());
			h5tools_setstatus(EXIT_SUCCESS);
			ret_value = -1;
			goto done;

		case 'v':
			options->verbose = 1;
			break;

		case 'f':
			/* parse the -f filter option */
			if (h5repack_addfilter(opt_arg, options) < 0) {
				error_msg("in parsing filter\n");
				h5tools_setstatus(EXIT_FAILURE);
				ret_value = -1;
				goto done;
			}
			break;

		case 'l':
			/* parse the -l layout option */
			if (h5repack_addlayout(opt_arg, options) < 0) {
				error_msg("in parsing layout\n");
				h5tools_setstatus(EXIT_FAILURE);
				ret_value = -1;
				goto done;
			}
			break;

		case 'm':
			options->min_comp = HDstrtoull(opt_arg , NULL, 0);
			if ((int) options->min_comp <= 0) {
				error_msg("invalid minimum compress size <%s>\n", opt_arg);
				h5tools_setstatus(EXIT_FAILURE);
				ret_value = -1;
				goto done;
			}
			break;

		case 'e':
			ret_value = read_info(opt_arg, options);
			if (ret_value < 0)
				goto done;
			break;

		case 'n':
			options->use_native = 1;
			break;

		case 'L':
			options->latest = 1;
			break;

		case 'c':
			options->grp_compact = HDatoi( opt_arg );
			if (options->grp_compact > 0)
				options->latest = 1; /* must use latest format */
			break;

		case 'd':
			options->grp_indexed = HDatoi( opt_arg );
			if (options->grp_indexed > 0)
				options->latest = 1; /* must use latest format */
			break;

		case 's':
			{
				int idx = 0;
				int ssize = 0;
				char *msgPtr = HDstrchr( opt_arg, ':');
				options->latest = 1; /* must use latest format */
				if (msgPtr == NULL) {
					ssize = HDatoi( opt_arg );
					for (idx = 0; idx < 5; idx++)
						options->msg_size[idx] = ssize;
				}
				else {
					char msgType[10];
					HDstrcpy(msgType, msgPtr + 1);
					msgPtr[0] = '\0';
					ssize = HDatoi( opt_arg );
					if (HDstrncmp(msgType, "dspace",6) == 0) {
						options->msg_size[0] = ssize;
					}
					else if (HDstrncmp(msgType, "dtype", 5) == 0) {
						options->msg_size[1] = ssize;
					}
					else if (HDstrncmp(msgType, "fill", 4) == 0) {
						options->msg_size[2] = ssize;
					}
					else if (HDstrncmp(msgType, "pline", 5) == 0) {
						options->msg_size[3] = ssize;
					}
					else if (HDstrncmp(msgType, "attr", 4) == 0) {
						options->msg_size[4] = ssize;
					}
				}
			}
			break;

		case 'u':
			options->ublock_filename = opt_arg;
			break;

		case 'b':
			options->ublock_size = (hsize_t) HDatol( opt_arg );
			break;

		case 'M':
			options->meta_block_size = (hsize_t) HDatol( opt_arg );
			break;

		case 't':
			options->threshold = (hsize_t) HDatol( opt_arg );
			break;

		case 'a':
			options->alignment = HDstrtoull(opt_arg , NULL, 0);
			if (options->alignment < 1) {
				error_msg("invalid alignment size\n", opt_arg);
				h5tools_setstatus(EXIT_FAILURE);
				ret_value = -1;
				goto done;
			}
			break;

        case 'E':
            enable_error_stack = TRUE;
            break;

        default:
            break;
        } /* switch */

    } /* while */

    if (has_i_o == 0) {
		/* check for file names to be processed */
		if (argc <= opt_ind || argv[opt_ind + 1] == NULL) {
			error_msg("missing file names\n");
			usage(h5tools_getprogname());
			h5tools_setstatus(EXIT_FAILURE);
			ret_value = -1;
		}
	}

done:
	return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h5repack main program
 *
 * Return: Success: EXIT_SUCCESS(0)
 *
 * Failure: EXIT_FAILURE(1)
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */
int main(int argc, const char **argv) {
    H5E_auto2_t         func;
    H5E_auto2_t         tools_func;
    void               *edata;
    void               *tools_edata;

	pack_opt_t options; /*the global options */

	h5tools_setprogname(PROGRAMNAME);
	h5tools_setstatus(EXIT_SUCCESS);

    /* Disable error reporting */
    H5Eget_auto2(H5E_DEFAULT, &func, &edata);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Disable tools error reporting */
    H5Eget_auto2(H5tools_ERR_STACK_g, &tools_func, &tools_edata);
    H5Eset_auto2(H5tools_ERR_STACK_g, NULL, NULL);

    /* update hyperslab buffer size from H5TOOLS_BUFSIZE env if exist */
    if (h5tools_getenv_update_hyperslab_bufsize() < 0) {
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* initialize options  */
    h5repack_init(&options, 0);

    if (parse_command_line(argc, argv, &options) < 0)
        goto done;

    /* get file names if they were not yet got */
    if (has_i_o == 0) {

        if (argv[opt_ind] != NULL && argv[opt_ind + 1] != NULL) {
            infile = argv[opt_ind];
            outfile = argv[opt_ind + 1];

            if ( HDstrcmp( infile, outfile ) == 0) {
                error_msg("file names cannot be the same\n");
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            }
        }
        else {
            error_msg("file names missing\n");
            usage(h5tools_getprogname());
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    if (enable_error_stack) {
        H5Eset_auto2(H5E_DEFAULT, func, edata);
        H5Eset_auto2(H5tools_ERR_STACK_g, tools_func, tools_edata);
    }

    /* pack it */
	h5tools_setstatus(h5repack(infile, outfile, &options));

done:
	/* free tables */
	h5repack_end(&options);

	leave(h5tools_getstatus());
}

