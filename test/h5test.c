/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, November 19, 1998
 *
 * Purpose:	Provides support functions for most of the hdf5 tests cases.
 *		
 */

#undef NDEBUG			/*override -DNDEBUG			*/
#include <h5test.h>

/*
 * Define these environment variables or constants to influence functions in
 * this test support library.  The environment variable is used in preference
 * to the cpp constant.  If neither is defined then use some default value.
 *
 * HDF5_PREFIX:		A string to add to the beginning of all file names.
 *			This can be used to tell MPIO what driver to use
 *			(e.g., "gfs:", "ufs:", or "nfs:") or to use a
 *			different file system (e.g., "/tmp" or "/usr/tmp").
 *			The prefix will be separated from the base file name
 *			by a slash. See h5_fixname() for details.
 *
 * HDF5_DRIVER:		This string describes what low level file driver to
 *			use for HDF5 file access.  The first word in the
 *			value is the name of the driver and subsequent data
 *			is interpreted according to the driver.  See
 *			h5_fileaccess() for details.
 *
 */



/*-------------------------------------------------------------------------
 * Function:	h5_errors
 *
 * Purpose:	Displays the error stack after printing "*FAILED*".
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_errors(void __unused__ *client_data)
{
    FAILED();
    H5Eprint (stdout);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	h5_cleanup
 *
 * Purpose:	Cleanup temporary test files.  The list of test files is in
 *		`extern const char *FILENAMES[]' -- these are only the base
 * 		names.  The file access property list is also closed.
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              May 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5_cleanup(hid_t fapl)
{
    char	filename[1024];
    char	temp[2048];
    int		i, j;

    if (!getenv("HDF5_NOCLEANUP")) {
	for (i=0; FILENAME[i]; i++) {
	    if (NULL==h5_fixname(FILENAME[i], fapl, filename,
				 sizeof filename)) {
		continue;
	    }

	    switch (H5Pget_driver(fapl)) {
	    case H5F_LOW_CORE:
		break; /*nothing to remove*/
		
	    case H5F_LOW_SPLIT:
		HDsnprintf(temp, sizeof temp, "%s.raw", filename);
		remove(temp);
		HDsnprintf(temp, sizeof temp, "%s.meta", filename);
		remove(temp);
		break;

	    case H5F_LOW_FAMILY:
		for (j=0; /*void*/; j++) {
		    HDsnprintf(temp, sizeof temp, filename, j);
		    if (access(temp, F_OK)<0) break;
		    remove(temp);
		}
		break;

	    default:
		remove(filename);
		break;
	    }
	}
    }
    H5Pclose(fapl);
}


/*-------------------------------------------------------------------------
 * Function:	h5_reset
 *
 * Purpose:	Reset the library by closing it.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November 20, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5_reset(void)
{
    fflush(stdout);
    fflush(stderr);
    H5close();
    H5Eset_auto (h5_errors, NULL);}


/*-------------------------------------------------------------------------
 * Function:	h5_fixname
 *
 * Purpose:	Create a file name from a file base name like `test' and
 *		return it through the FULLNAME (at most SIZE characters
 *		counting the null terminator). The full name is created by
 *		prepending the contents of HDF5_PREFIX (separated from the
 *		base name by a slash) and appending a file extension based on
 *		the driver supplied.
 *
 * Return:	Success:	The FULLNAME pointer.
 *
 *		Failure:	NULL if BASENAME or FULLNAME is the null
 *				pointer or if FULLNAME isn't large enough for
 *				the result.
 *
 * Programmer:	Robb Matzke
 *              Thursday, November 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
h5_fixname(const char *basename, hid_t fapl, char *fullname, size_t size)
{
    const char		*prefix=NULL, *suffix=NULL;
    H5F_driver_t	driver;
    
    if (!basename || !fullname || size<1) return NULL;

    /* First use the environment variable, then try the constant */
    prefix = getenv("HDF5_PREFIX");
#ifdef HDF5_PREFIX
    if (!prefix) prefix = HDF5_PREFIX;
#endif

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
	if (HDsnprintf(fullname, size, "%s/%s", prefix, basename)==(int)size) {
	    return NULL; /*buffer is too small*/
	}
    } else if (strlen(basename)>=size) {
	return NULL; /*buffer is too small*/
    } else {
	strcpy(fullname, basename);
    }

    /* Append a suffix */
    if ((driver=H5Pget_driver(fapl))<0) return NULL;
    switch (driver) {
    case H5F_LOW_SPLIT:
    case H5F_LOW_CORE:
	suffix = NULL;
	break;
    case H5F_LOW_FAMILY:
	suffix = "%05d.h5";
	break;
    default:
	suffix = ".h5";
	break;
    }
    if (suffix) {
	if (strlen(fullname)+strlen(suffix)>=size) return NULL;
	strcat(fullname, suffix);
    }
    
    return fullname;
}


/*-------------------------------------------------------------------------
 * Function:	h5_fileaccess
 *
 * Purpose:	Returns a file access template which is the default template
 *		but with a file driver set according to the constant or
 *		environment variable HDF5_DRIVER
 *
 * Return:	Success:	A file access property list
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, November 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5_fileaccess(void)
{
    const char	*val = NULL;
    const char	*name;
    char	s[1024];
    hid_t	fapl = -1;
    hsize_t	fam_size = 1024*1024;
    
    /* First use the environment variable, then the constant */
    val = getenv("HDF5_DRIVER");
#ifdef HDF5_DRIVER
    if (!val) val = HDF5_DRIVER;
#endif

    if ((fapl=H5Pcreate(H5P_FILE_ACCESS))<0) return -1;
    if (!val || !*val) return fapl; /*use default*/
    
    strncpy(s, val, sizeof s);
    s[sizeof(s)-1] = '\0';
    if (NULL==(name=strtok(s, " \t\n\r"))) return fapl;

    if (!strcmp(name, "sec2")) {
	/* Unix read() and write() system calls */
	if (H5Pset_sec2(fapl)<0) return -1;
    } else if (!strcmp(name, "stdio")) {
	/* C standard I/O library */
	if (H5Pset_stdio(fapl)<0) return -1;
    } else if (!strcmp(name, "core")) {
	/* In-core temporary file with 1MB increment */
	if (H5Pset_core(fapl, 1024*1024)<0) return -1;
    } else if (!strcmp(name, "split")) {
	/* Split meta data and raw data each using default driver */
	if (H5Pset_split(fapl, NULL, H5P_DEFAULT, NULL, H5P_DEFAULT)<0)
	    return -1;
    } else if (!strcmp(name, "family")) {
	/* Family of files, each 1MB and using the default driver */
	if ((val=strtok(NULL, " \t\n\r"))) {
	    fam_size = strtod(val, NULL) * 1024*1024;
	}
	if (H5Pset_family(fapl, fam_size, H5P_DEFAULT)<0) return -1;
    } else {
	/* Unknown driver */
	return -1;
    }

    return fapl;
}


/*-------------------------------------------------------------------------
 * Function:	h5_no_hwconv
 *
 * Purpose:	Turn off hardware data type conversions.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November 20, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5_no_hwconv(void)
{
    H5Tunregister(H5T_conv_char_uchar);
    H5Tunregister(H5T_conv_char_short);
    H5Tunregister(H5T_conv_char_ushort);
    H5Tunregister(H5T_conv_char_int);
    H5Tunregister(H5T_conv_char_uint);
    H5Tunregister(H5T_conv_char_long);
    H5Tunregister(H5T_conv_char_ulong);

    H5Tunregister(H5T_conv_uchar_char);
    H5Tunregister(H5T_conv_uchar_short);
    H5Tunregister(H5T_conv_uchar_ushort);
    H5Tunregister(H5T_conv_uchar_int);
    H5Tunregister(H5T_conv_uchar_uint);
    H5Tunregister(H5T_conv_uchar_long);
    H5Tunregister(H5T_conv_uchar_ulong);

    H5Tunregister(H5T_conv_short_char);
    H5Tunregister(H5T_conv_short_uchar);
    H5Tunregister(H5T_conv_short_ushort);
    H5Tunregister(H5T_conv_short_int);
    H5Tunregister(H5T_conv_short_uint);
    H5Tunregister(H5T_conv_short_long);
    H5Tunregister(H5T_conv_short_ulong);

    H5Tunregister(H5T_conv_ushort_char);
    H5Tunregister(H5T_conv_ushort_uchar);
    H5Tunregister(H5T_conv_ushort_short);
    H5Tunregister(H5T_conv_ushort_int);
    H5Tunregister(H5T_conv_ushort_uint);
    H5Tunregister(H5T_conv_ushort_long);
    H5Tunregister(H5T_conv_ushort_ulong);

    H5Tunregister(H5T_conv_int_char);
    H5Tunregister(H5T_conv_int_uchar);
    H5Tunregister(H5T_conv_int_short);
    H5Tunregister(H5T_conv_int_ushort);
    H5Tunregister(H5T_conv_int_uint);
    H5Tunregister(H5T_conv_int_long);
    H5Tunregister(H5T_conv_int_ulong);

    H5Tunregister(H5T_conv_uint_char);
    H5Tunregister(H5T_conv_uint_uchar);
    H5Tunregister(H5T_conv_uint_short);
    H5Tunregister(H5T_conv_uint_ushort);
    H5Tunregister(H5T_conv_uint_int);
    H5Tunregister(H5T_conv_uint_long);
    H5Tunregister(H5T_conv_uint_ulong);

    H5Tunregister(H5T_conv_long_char);
    H5Tunregister(H5T_conv_long_uchar);
    H5Tunregister(H5T_conv_long_short);
    H5Tunregister(H5T_conv_long_ushort);
    H5Tunregister(H5T_conv_long_int);
    H5Tunregister(H5T_conv_long_uint);
    H5Tunregister(H5T_conv_long_ulong);

    H5Tunregister(H5T_conv_ulong_char);
    H5Tunregister(H5T_conv_ulong_uchar);
    H5Tunregister(H5T_conv_ulong_short);
    H5Tunregister(H5T_conv_ulong_ushort);
    H5Tunregister(H5T_conv_ulong_int);
    H5Tunregister(H5T_conv_ulong_uint);
    H5Tunregister(H5T_conv_ulong_long);

    H5Tunregister(H5T_conv_float_double);
    H5Tunregister(H5T_conv_double_float);
}
