/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, March 23, 1998
 */
#include <ctype.h>
#include <h5tools.h>
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <H5private.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

/* Command-line switches */
static int verbose_g = 0;
static int dump_g = 0;
static int width_g = 80;

/* Information about how to display each type of object */
static struct dispatch_t {
    const char	*name;
    hid_t	(*open)(hid_t loc, const char *name);
    herr_t	(*close)(hid_t obj);
    herr_t	(*list1)(hid_t obj);
    herr_t	(*list2)(hid_t obj);
} dispatch_g[H5G_NTYPES];

#define DISPATCH(TYPE,NAME,OPEN,CLOSE,LIST1,LIST2) {			      \
    dispatch_g[TYPE].name = (NAME);					      \
    dispatch_g[TYPE].open = (OPEN);					      \
    dispatch_g[TYPE].close = (CLOSE);					      \
    dispatch_g[TYPE].list1 = (LIST1);					      \
    dispatch_g[TYPE].list2 = (LIST2);					      \
}

static herr_t list (hid_t group, const char *name, void *cd);


/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Prints a usage message on stderr and then returns.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (const char *progname)
{
    fprintf(stderr, "\
usage: %s [OPTIONS] FILE [OBJECTS...]\n\
   OPTIONS\n\
      -h, -?, --help   Print a usage message and exit\n\
      -d, --dump       Print the values of datasets\n\
      -wN, --width=N   Set the number of columns of output\n\
      -v, --verbose    Generate more verbose output\n\
      -V, --version    Print version number and exit\n\
   FILE\n\
      The file name may include a printf(3C) integer format such as\n\
      \"%%05d\" to open a file family.\n\
   OBJECTS\n\
      The names of zero or more objects about which information should be\n\
      displayed.  If a group is mentioned then information about each of its\n\
      members is displayed.  If no object names are specified then\n\
      information about all of the objects in the root group is displayed.\n",
	    progname);
}


/*-------------------------------------------------------------------------
 * Function:	dump_dataset_values
 *
 * Purpose:	Prints all values of a dataset.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_dataset_values(hid_t dset)
{
    hid_t		f_type = H5Dget_type(dset);
    size_t		size = H5Tget_size(f_type);
    h5dump_t		info;

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);
    info.idx_fmt = "        (%s) ";
    info.line_ncols = width_g;
    if (verbose_g) info.cmpd_name = "%s=";

    /*
     * If the dataset is a 1-byte integer type then format it as an ASCI
     * character string instead of integers.
     */
    if (1==size && H5T_INTEGER==H5Tget_class(f_type)) {
	info.elmt_suf1 = "";
	info.elmt_suf2 = "";
	info.idx_fmt = "        (%s) \"";
	info.line_suf = "\"";
    }

    
    /*
     * Print all the values.
     */
    printf("    Data:\n");
    if (h5dump(stdout, &info, dset, -1)<0) {
	printf("        Unable to print data.\n");
    }

    H5Tclose(f_type);
}


/*-------------------------------------------------------------------------
 * Function:	list_attr
 *
 * Purpose:	Prints information about attributes.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, June  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list_attr (hid_t obj, const char *attr_name, void __unused__ *op_data)
{
    hid_t	attr;
    int		i;

    printf("    %-10s %-10s", "Attribute:", attr_name);
    if ((attr = H5Aopen_name (obj, attr_name))) {
	hid_t space = H5Aget_space (attr);
	hsize_t size[64];
	int ndims = H5Sget_simple_extent_dims (space, size, NULL);
	H5Sclose (space);
	printf (" {");
	for (i=0; i<ndims; i++) {
	    HDfprintf (stdout, "%s%Hu", i?", ":"", size[i]);
	}
	putchar ('}');
	H5Aclose (attr);
    }
    
    putchar ('\n');
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	dataset_list1
 *
 * Purpose:	List information about a dataset which should appear on the
 *		same line as the dataset name.  This information will precede
 *		information which is applicable to all objects which will be
 *		printed by the caller.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_list1(hid_t dset)
{
    hsize_t		cur_size[64];	/*current dataset dimensions	*/
    hsize_t		max_size[64];	/*maximum dataset dimensions	*/
    hid_t		space;		/*data space			*/
    int 		ndims;		/*dimensionality		*/
    int			i;

    /*
     * Information that goes on the same row as the name.  The name has
     * already been printed.
     */
    space = H5Dget_space(dset);
    ndims = H5Sget_simple_extent_dims(space, cur_size, max_size);
    printf (" {");
    for (i=0; i<ndims; i++) {
	HDfprintf (stdout, "%s%Hu", i?", ":"", cur_size[i]);
	if (max_size[i]==H5S_UNLIMITED) {
	    HDfprintf (stdout, "/%s", "Inf");
	} else if (max_size[i]!=cur_size[i] || verbose_g>0) {
	    HDfprintf(stdout, "/%Hu", max_size[i]);
	}
    }
    putchar('}');
    H5Sclose (space);

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	dataset_list2
 *
 * Purpose:	List information about a dataset which should appear after
 *		information which is general to all objects.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_list2(hid_t dset)
{
    hid_t		dcpl;		/*dataset creation property list*/
    int			nf;		/*number of filters		*/
    unsigned		filt_flags;	/*filter flags			*/
    H5Z_filter_t	filt_id;	/*filter identification number	*/
    unsigned		cd_values[20];	/*filter client data values	*/
    size_t		cd_nelmts;	/*filter client number of values*/
    size_t		cd_num;		/*filter client data counter	*/
    char		f_name[32];	/*filter name			*/
    char		s[64];		/*temporary string buffer	*/
    int			i;

    if (verbose_g>0) {
	dcpl = H5Dget_create_plist(dset);

	/* Print information about raw data filters */
	if ((nf = H5Pget_nfilters(dcpl))>0) {
	    for (i=0; i<nf; i++) {
		cd_nelmts = NELMTS(cd_values);
		filt_id = H5Pget_filter(dcpl, i, &filt_flags, &cd_nelmts,
					cd_values, sizeof(f_name), f_name);
		f_name[sizeof(f_name)-1] = '\0';
		sprintf(s, "Filter-%d:", i);
		printf("    %-10s %s-%u %s {", s,
		       f_name[0]?f_name:"method",
		       (unsigned)filt_id,
		       filt_flags & H5Z_FLAG_OPTIONAL?"OPT":"");
		for (cd_num=0; cd_num<cd_nelmts; cd_num++) {
		    printf("%s%u", cd_num?", ":"", cd_values[cd_num]);
		}
		printf("}\n");
	    }
	}
	H5Pclose(dcpl);
    }

    if (dump_g) dump_dataset_values(dset);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	link_open
 *
 * Purpose:	This gets called to open a symbolic link.  Since symbolic
 *		links don't correspond to actual objects we simply print the
 *		link information and return failure.
 *
 * Return:	Success:	never succeeds
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
link_open(hid_t location, const char *name)
{
    char	buf[64];
    
    if (H5Gget_linkval (location, name, sizeof(buf), buf)<0) return -1;
    if (NULL==HDmemchr(buf, 0, sizeof(buf))) {
	strcpy(buf+sizeof(buf)-4, "...");
    }
    puts(buf);

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	list
 *
 * Purpose:	Prints the group member name.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list (hid_t group, const char *name, void __unused__ *cd)
{
    hid_t	obj;
    char	buf[512], comment[50];
    H5G_stat_t	sb;
    struct tm	*tm;
    herr_t	status;
    
    /* Print the object name */
    printf("%-25s ", name);
    
    /* Get object information */
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(group, name, FALSE, &sb);
    } H5E_END_TRY;
    if (status<0) {
	puts("**NOT FOUND**");
	return 0;
    } else if (sb.type<0 || sb.type>=H5G_NTYPES) {
	printf("Unknown type=%d", sb.type);
	return 0;
    }
    if (dispatch_g[sb.type].name) fputs(dispatch_g[sb.type].name, stdout);

    /*
     * Open the object.  Not all objects can be opened.  If this is the case
     * then return right away.
     */
    if (NULL==dispatch_g[sb.type].open ||
	(obj=(dispatch_g[sb.type].open)(group, name))<0) return 0;

    /*
     * List the first line of information for the object.
     */
    if (dispatch_g[sb.type].list1) (dispatch_g[sb.type].list1)(obj);
    putchar('\n');
    
    /*
     * Show detailed information about the object, beginning with information
     * which is common to all objects.
     */
    if (verbose_g>0) {
	H5Aiterate(obj, NULL, list_attr, NULL);
	printf("    %-10s %lu:%lu:%lu:%lu\n", "Location:",
	       sb.fileno[1], sb.fileno[0], sb.objno[1], sb.objno[0]);
	printf("    %-10s %u\n", "Links:", sb.nlink);
	if (sb.mtime>0 && NULL!=(tm=localtime(&(sb.mtime)))) {
	    strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
	    printf("    %-10s %s\n", "Modtime:", buf);
	}
	comment[0] = '\0';
	H5Gget_comment(group, name, sizeof(comment), comment);
	strcpy(comment+sizeof(comment)-4, "...");
	if (comment[0]) printf("    %-10s %s\n", "Comment:", comment);
    }
    if (dispatch_g[sb.type].list2) (dispatch_g[sb.type].list2)(obj);
    
    /*
     * Close the object.
     */
    (dispatch_g[sb.type].close)(obj);
    return 0;
}



/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Opens a file and lists the specified group
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    hid_t	file, plist=H5P_DEFAULT, root;
    const char	*fname = NULL;
    const char	*progname;
    const char	*s;
    char	*rest;
    int		argno;

    DISPATCH(H5G_DATASET, "Dataset", H5Dopen, H5Dclose,
	     dataset_list1, dataset_list2);
    DISPATCH(H5G_GROUP, "Group", H5Gopen, H5Gclose,
	     NULL, NULL);
    DISPATCH(H5G_TYPE, "Type", H5Topen, H5Tclose,
	     NULL, NULL);
    DISPATCH(H5G_LINK, "-> ", link_open, NULL,
	     NULL, NULL);

    /* Name of this program without the path */
    if ((progname=strrchr (argv[0], '/'))) progname++;
    else progname = argv[0];
    
    /* Switches come before non-switch arguments */
    for (argno=1; argno<argc && '-'==argv[argno][0]; argno++) {
	if (!strcmp(argv[argno], "--")) {
	    /* Last switch */
	    argno++;
	    break;
	} else if (!strcmp(argv[argno], "--help")) {
	    usage(progname);
	    exit(0);
	} else if (!strcmp(argv[argno], "--dump")) {
	    dump_g++;
	} else if (!strncmp(argv[argno], "--width=", 8)) {
	    width_g = strtol(argv[argno]+8, &rest, 0);
	    if (width_g<=0 || *rest) {
		usage(progname);
		exit(1);
	    }
	} else if (!strcmp(argv[argno], "--verbose")) {
	    verbose_g++;
	} else if (!strcmp(argv[argno], "--version")) {
	    printf("This is %s version %u.%u release %u\n",
		   progname, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
	    exit(0);
	} else if (!strncmp(argv[argno], "-w", 2)) {
	    if (argv[argno][2]) {
		s = argv[argno]+2;
	    } else if (argno+1>=argc) {
		usage(progname);
		exit(1);
	    } else {
		s = argv[++argno];
	    }
	    width_g = strtol(s, &rest, 0);
	    if (width_g<=0 || *rest) {
		usage(progname);
		exit(1);
	    }
	} else if ('-'!=argv[argno][1]) {
	    /* Single-letter switches */
	    for (s=argv[argno]+1; *s; s++) {
		switch (*s) {
		case '?':
		case 'h':	/* --help */
		    usage(progname);
		    exit(0);
		case 'd':	/* --dump */
		    dump_g++;
		    break;
		case 'v':	/* --verbose */
		    verbose_g++;
		    break;
		case 'V':	/* --version */
		    printf("This is %s version %u.%u release %u\n",
			   progname, H5_VERS_MAJOR, H5_VERS_MINOR,
			   H5_VERS_RELEASE);
		    exit(0);
		default:
		    usage(progname);
		    exit(1);
		}
	    }
	} else {
	    usage(progname);
	    exit(1);
	}
    }

    /*
     * The first non-switch argument is a file name.  If the file name
     * contains a `%' then assume that a file family is being opened.
     */
    if (argno<argc) {
	fname = argv[argno++];
    } else {
	usage(progname);
	exit(1);
    }
    if (strchr (fname, '%')) {
	plist = H5Pcreate (H5P_FILE_ACCESS);
	H5Pset_family (plist, 0, H5P_DEFAULT);
    }
    if ((file = H5Fopen (fname, H5F_ACC_RDONLY, plist))<0) exit (1);

    /*
     * The remaining optional arguments are the names of the objects to list.
     * If there are no arguments then list `/'.
     */
    if (argno>=argc) {
	H5Giterate(file, "/", NULL, list, NULL);
    } else {
	for (/*void*/; argno<argc; argno++) {
	    H5E_BEGIN_TRY {
		root = H5Gopen (file, argv[argno]);
	    } H5E_END_TRY;
	    if (root>=0) {
		H5Giterate(file, argv[argno], NULL, list, NULL);
	    } else if ((root=H5Gopen(file, "/"))<0) {
		exit(1);
	    } else {
		list(root, argv[argno], NULL);
	    }
	    if (H5Gclose(root)<0) exit(1);
	}
    }

    if (H5Fclose(file)<0) exit(1);
    return 0;
}
