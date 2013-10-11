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

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 23, 1998
 *
 * Purpose: A library for displaying the values of a dataset in a human
 *  readable format.
 */

#include <stdio.h>
#include <stdlib.h>

#include "h5tools.h"
#include "h5tools_dump.h"
#include "h5tools_ref.h"
#include "h5tools_utils.h"
#include "H5private.h"

/* global variables */
hid_t H5tools_ERR_STACK_g = 0;
hid_t H5tools_ERR_CLS_g = -1;
hid_t H5E_tools_g = -1;
hid_t H5E_tools_min_id_g = -1;
int         compound_data;
FILE       *rawattrstream = NULL;      /* should initialize to stdout but gcc moans about it */
FILE       *rawdatastream = NULL;      /* should initialize to stdout but gcc moans about it */
FILE       *rawinstream = NULL;        /* should initialize to stdin but gcc moans about it */
FILE       *rawoutstream = NULL;       /* should initialize to stdout but gcc moans about it */
FILE       *rawerrorstream = NULL;     /* should initialize to stderr but gcc moans about it */
int         bin_output;         /* binary output */
int         bin_form;           /* binary form */
int         region_output;      /* region output */
int         oid_output;         /* oid output */
int         data_output;        /* data output */
int         attr_data_output;   /* attribute data output */
int         packed_bits_num;    /* number of packed bits to display */
int         packed_data_offset; /* offset of packed bits to display */
int         packed_data_length; /* lengtht of packed bits to display */
unsigned long long packed_data_mask;  /* mask in which packed bits to display */

/* module-scoped variables */
static int  h5tools_init_g;     /* if h5tools lib has been initialized */
#ifdef H5_HAVE_PARALLEL
static int  h5tools_mpi_init_g; /* if MPI_Init() has been called */
#endif /* H5_HAVE_PARALLEL */

/* Names of VFDs */
static const char *drivernames[]={
    "sec2",
    "family",
    "split",
    "multi",
#ifdef H5_HAVE_STREAM
    "stream",
#endif /* H5_HAVE_STREAM */
#ifdef H5_HAVE_PARALLEL
    "mpio",
    "mpiposix"
#endif /* H5_HAVE_PARALLEL */
};

/* This enum should match the entries in the above drivers_list since they
 * are indexes into the drivers_list array. */
typedef enum {
    SEC2_IDX = 0
   ,FAMILY_IDX
   ,SPLIT_IDX
   ,MULTI_IDX
#ifdef H5_HAVE_STREAM
   ,STREAM_IDX
#endif /* H5_HAVE_STREAM */
#ifdef H5_HAVE_PARALLEL
   ,MPIO_IDX
   ,MPIPOSIX_IDX
#endif /* H5_HAVE_PARALLEL */
} driver_idx;
#define NUM_DRIVERS     (sizeof(drivernames) / sizeof(drivernames[0]))

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Initialize the H5 Tools library
 * Description:
 *      This should be called before any other h5tools function is called.
 *      Effect of any h5tools function called before this has been called is
 *      undetermined.
 * Return:
 *      None
 * Programmer:
 *      Albert Cheng, 2000-10-31
 * Modifications:
 *-------------------------------------------------------------------------
 */
void
h5tools_init(void)
{
    char lib_str[256];

    if (!h5tools_init_g) {
        /* register the error class */
        HDsnprintf(lib_str, sizeof(lib_str), "%d.%d.%d",H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);

        H5tools_ERR_STACK_g = H5Ecreate_stack();
        H5TOOLS_INIT_ERROR()

        if (!rawattrstream)
            rawattrstream = stdout;
        if (!rawdatastream)
            rawdatastream = stdout;
        if (!rawinstream)
            rawinstream = stdin;
        if (!rawoutstream)
            rawoutstream = stdout;
        if (!rawerrorstream)
            rawerrorstream = stderr;

        h5tools_dump_init();

        h5tools_init_g++;
    }
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Close the H5 Tools library
 * Description:
 *      Close or release resources such as files opened by the library. This
 *      should be called after all other h5tools functions have been called.
 *      Effect of any h5tools function called after this has been called is
 *      undetermined.
 * Return:
 *      None
 * Programmer:
 *      Albert Cheng, 2000-10-31
 * Modifications:
 *-------------------------------------------------------------------------
 */
void
h5tools_close(void)
{
    H5E_auto2_t         tools_func;
    void               *tools_edata;
    if (h5tools_init_g) {
        /* special case where only data is output to stdout */
        if((rawoutstream == NULL) && rawdatastream && (rawdatastream == stdout))
            HDfprintf(rawdatastream, "\n");

        H5Eget_auto2(H5tools_ERR_STACK_g, &tools_func, &tools_edata);
        if(tools_func!=NULL)
            H5Eprint2(H5tools_ERR_STACK_g, rawerrorstream);
        if (rawattrstream && rawattrstream != stdout) {
            if (fclose(rawattrstream))
                perror("closing rawattrstream");
            else
                rawattrstream = NULL;
        }
        if (rawdatastream && rawdatastream != stdout) {
            if (fclose(rawdatastream))
                perror("closing rawdatastream");
            else
                rawdatastream = NULL;
        }
        if (rawinstream && rawinstream != stdin) {
            if (fclose(rawinstream))
                perror("closing rawinstream");
            else
                rawinstream = NULL;
        }
        if (rawoutstream && rawoutstream != stdout) {
            if (fclose(rawoutstream))
                perror("closing rawoutstream");
            else
                rawoutstream = NULL;
        }
        if (rawerrorstream && rawerrorstream != stderr) {
            if (fclose(rawerrorstream))
                perror("closing rawerrorstream");
            else
                rawerrorstream = NULL;
        }

        /* Clean up the reference path table, if it's been used */
        term_ref_path_table();

        H5TOOLS_CLOSE_ERROR()
        H5Eclose_stack(H5tools_ERR_STACK_g);
        /* Shut down the library */
        H5close();

        h5tools_init_g = 0;
    }
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_set_data_output_file
 *
 * Purpose:     Open fname as the output file for dataset raw data.
 *      Set rawdatastream as its file stream.
 *
 * Return:      0 -- succeeded
 *      negative -- failed
 *
 * Programmer:  Albert Cheng, 2000/09/30
 *
 * Modifications:
 *  pvn June, 1, 2006. Add a switch for binary output
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_set_data_output_file(const char *fname, int is_bin)
{
    int     retvalue = FAIL;
    FILE    *f;    /* temporary holding place for the stream pointer
                    * so that rawdatastream is changed only when succeeded */

    if (rawdatastream && rawdatastream != stdout) {
        if (HDfclose(rawdatastream))
            HDperror("closing rawdatastream");
        else
            rawdatastream = NULL;
    }

    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
            if ((f = HDfopen(fname, "wb")) != NULL) {
                rawdatastream = f;
                retvalue = SUCCEED;
            }
        }
        else {
            if ((f = HDfopen(fname, "w")) != NULL) {
                rawdatastream = f;
                retvalue = SUCCEED;
            }
        }
    }
    else {
        rawdatastream = NULL;
        retvalue = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_set_attr_output_file
 *
 * Purpose:     Open fname as the output file for attribute raw data.
 *      Set rawattrstream as its file stream.
 *
 * Return:      0 -- succeeded
 *      negative -- failed
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_set_attr_output_file(const char *fname, int is_bin)
{
    int     retvalue = FAIL;
    FILE    *f;    /* temporary holding place for the stream pointer
                    * so that rawattrstream is changed only when succeeded */

    if (rawattrstream && rawattrstream != stdout) {
        if (HDfclose(rawattrstream))
            HDperror("closing rawattrstream");
        else
            rawattrstream = NULL;
    }

    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
			if ((f = HDfopen(fname, "wb")) != NULL) {
				rawattrstream = f;
				retvalue = SUCCEED;
			}
        }
        else {
			if ((f = HDfopen(fname, "w")) != NULL) {
				rawattrstream = f;
				retvalue = SUCCEED;
			}
        }
    }
    else {
        rawattrstream = NULL;
        retvalue = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_set_input_file
 *
 * Purpose:     Open fname as the input file for raw input.
 *      Set rawinstream as its file stream.
 *
 * Return:      0 -- succeeded
 *      negative -- failed
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_set_input_file(const char *fname, int is_bin)
{
    int     retvalue = FAIL;
    FILE    *f;    /* temporary holding place for the stream pointer
                    * so that rawinstream is changed only when succeeded */

    if (rawinstream && rawinstream != stdin) {
        if (HDfclose(rawinstream))
            HDperror("closing rawinstream");
        else
        	rawinstream = NULL;
    }
    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
			if ((f = HDfopen(fname, "rb")) != NULL) {
				rawinstream = f;
				retvalue = SUCCEED;
			}
        }
        else {
			if ((f = HDfopen(fname, "r")) != NULL) {
				rawinstream = f;
				retvalue = SUCCEED;
			}
        }
    }
    else {
    	rawinstream = NULL;
        retvalue = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_set_output_file
 *
 * Purpose:     Open fname as the output file for raw output.
 *      Set rawoutstream as its file stream.
 *
 * Return:      0 -- succeeded
 *      negative -- failed
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_set_output_file(const char *fname, int is_bin)
{
    int     retvalue = FAIL;
    FILE    *f;    /* temporary holding place for the stream pointer
                    * so that rawoutstream is changed only when succeeded */

    if (rawoutstream && rawoutstream != stdout) {
        if (HDfclose(rawoutstream))
            HDperror("closing rawoutstream");
        else
            rawoutstream = NULL;
    }
    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
			if ((f = HDfopen(fname, "wb")) != NULL) {
					rawoutstream = f;
					retvalue = SUCCEED;
			}
        }
        else {
			if ((f = HDfopen(fname, "w")) != NULL) {
					rawoutstream = f;
					retvalue = SUCCEED;
			}
        }
    }
    else {
        rawoutstream = NULL;
        retvalue = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_set_error_file
 *
 * Purpose:     Open fname as the error output file for dataset raw error.
 *      Set rawerrorstream as its file stream.
 *
 * Return:      0 -- succeeded
 *      negative -- failed
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_set_error_file(const char *fname, int is_bin)
{
    int     retvalue = FAIL;
    FILE    *f;    /* temporary holding place for the stream pointer
                    * so that rawerrorstream is changed only when succeeded */

    if (rawerrorstream && rawerrorstream != stderr) {
        if (HDfclose(rawerrorstream))
            HDperror("closing rawerrorstream");
        else
            rawerrorstream = NULL;
    }

    /* First check if filename is string "NULL" */
    if (fname != NULL) {
    /* binary output */
		if (is_bin) {
			if ((f = HDfopen(fname, "wb")) != NULL) {
				rawerrorstream = f;
				retvalue = SUCCEED;
			}
        }
        else {
			if ((f = HDfopen(fname, "w")) != NULL) {
				rawerrorstream = f;
				retvalue = SUCCEED;
			}
		}
    }
    else {
    	rawerrorstream = NULL;
        retvalue = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Audience:    Private
 * Chapter:     H5Tools Library
 * Purpose: Get a FAPL for a driver
 * Description:
 *      Get a FAPL for a given VFL driver name.
 * Return:
 *      None
 * Programmer:
 *      Quincey Koziol, 2004-02-04
 * Modifications:
 *      Pedro Vicente Nunes, Thursday, July 27, 2006
 *   Added error return conditions for the H5Pset_fapl calls
 *-------------------------------------------------------------------------
 */
static hid_t
h5tools_get_fapl(hid_t fapl, const char *driver, unsigned *drivernum)
{
    hid_t new_fapl; /* Copy of file access property list passed in, or new property list */

    /* Make a copy of the FAPL, for the file open call to use, eventually */
    if (fapl == H5P_DEFAULT) {
        if ((new_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            goto error;
    } /* end if */
    else {
        if ((new_fapl = H5Pcopy(fapl)) < 0)
            goto error;
    } /* end else */

    /* Determine which driver the user wants to open the file with. Try
     * that driver. If it can't open it, then fail. */
    if (!HDstrcmp(driver, drivernames[SEC2_IDX])) {
        /* SEC2 driver */
        if (H5Pset_fapl_sec2(new_fapl) < 0)
            goto error;

        if (drivernum)
            *drivernum = SEC2_IDX;
    }
    else if (!HDstrcmp(driver, drivernames[FAMILY_IDX])) {
        /* FAMILY Driver */

        /* Set member size to be 0 to indicate the current first member size
         * is the member size.
         */
        if (H5Pset_fapl_family(new_fapl, (hsize_t) 0, H5P_DEFAULT) < 0)
            goto error;

        if (drivernum)
            *drivernum = FAMILY_IDX;
    }
    else if (!HDstrcmp(driver, drivernames[SPLIT_IDX])) {
        /* SPLIT Driver */
        if (H5Pset_fapl_split(new_fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT) < 0)
            goto error;

        if (drivernum)
            *drivernum = SPLIT_IDX;
    }
    else if (!HDstrcmp(driver, drivernames[MULTI_IDX])) {
        /* MULTI Driver */
        if (H5Pset_fapl_multi(new_fapl, NULL, NULL, NULL, NULL, TRUE) < 0)
        goto error;

        if(drivernum)
        *drivernum = MULTI_IDX;
#ifdef H5_HAVE_STREAM
            }
            else if(!HDstrcmp(driver, drivernames[STREAM_IDX])) {
                /* STREAM Driver */
                if(H5Pset_fapl_stream(new_fapl, NULL) < 0)
                goto error;

                if(drivernum)
                *drivernum = STREAM_IDX;
#endif /* H5_HAVE_STREAM */
#ifdef H5_HAVE_PARALLEL
            }
            else if(!HDstrcmp(driver, drivernames[MPIO_IDX])) {
                /* MPI-I/O Driver */
                /* check if MPI has been initialized. */
                if(!h5tools_mpi_init_g)
                MPI_Initialized(&h5tools_mpi_init_g);
                if(h5tools_mpi_init_g) {
                    if(H5Pset_fapl_mpio(new_fapl, MPI_COMM_WORLD, MPI_INFO_NULL) < 0)
                goto error;

            if(drivernum)
                *drivernum = MPIO_IDX;
        } /* end if */
    }
    else if (!HDstrcmp(driver, drivernames[MPIPOSIX_IDX])) {
        /* MPI-I/O Driver */
        /* check if MPI has been initialized. */
        if(!h5tools_mpi_init_g)
            MPI_Initialized(&h5tools_mpi_init_g);
        if(h5tools_mpi_init_g) {
            if(H5Pset_fapl_mpiposix(new_fapl, MPI_COMM_WORLD, TRUE) < 0)
                goto error;

            if(drivernum)
                *drivernum = MPIPOSIX_IDX;
        } /* end if */
#endif /* H5_HAVE_PARALLEL */
    }
    else {
        goto error;
    }

    return(new_fapl);

error:
    if(new_fapl != H5P_DEFAULT)
        H5Pclose(new_fapl);
    return -1;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Open a file with various VFL drivers.
 * Description:
 *      Loop through the various types of VFL drivers trying to open FNAME.
 *      If the HDF5 library is version 1.2 or less, then we have only the SEC2
 *      driver to try out. If the HDF5 library is greater than version 1.2,
 *      then we have the FAMILY, SPLIT, and MULTI drivers to play with (and
 *      the STREAM driver if H5_HAVE_STREAM is defined, that is).
 *
 *      If DRIVER is non-NULL, then it will try to open the file with that
 *      driver first. We assume that the user knows what they are doing so, if
 *      we fail, then we won't try other file drivers.
 * Return:
 *      On success, returns a file id for the opened file. If DRIVERNAME is
 *      non-null then the first DRIVERNAME_SIZE-1 characters of the driver
 *      name are copied into the DRIVERNAME array and null terminated.
 *
 *      Otherwise, the function returns FAIL. If DRIVERNAME is non-null then
 *      the first byte is set to the null terminator.
 * Programmer:
 *      Lost in the mists of time.
 * Modifications:
 *      Robb Matzke, 2000-06-23
 *      We only have to initialize driver[] on the first call, thereby
 *      preventing memory leaks from repeated calls to H5Pcreate().
 *
 *      Robb Matzke, 2000-06-23
 *      Added DRIVERNAME_SIZE arg to prevent overflows when writing to
 *      DRIVERNAME.
 *
 *      Robb Matzke, 2000-06-23
 *      Added test to prevent coredump when the file could not be opened by
 *      any driver.
 *
 *      Robb Matzke, 2000-06-23
 *      Changed name from H5ToolsFopen() so it jives better with the names we
 *      already have at the top of this source file.
 *
 *      Thomas Radke, 2000-09-12
 *      Added Stream VFD to the driver[] array.
 *
 *      Bill Wendling, 2001-01-10
 *      Changed macro behavior so that if we have a version other than 1.2.x
 *      (i.e., > 1.2), then we do the drivers check.
 *
 *      Bill Wendling, 2001-07-30
 *      Added DRIVER parameter so that the user can specify "try this driver"
 *      instead of the default behaviour. If it fails to open the file with
 *      that driver, this will fail completely (i.e., we won't try the other
 *      drivers). We're assuming the user knows what they're doing. How UNIX
 *      of us.
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_fopen(const char *fname, unsigned flags, hid_t fapl, const char *driver,
    char *drivername, size_t drivername_size)
{
    unsigned    drivernum;
    hid_t       fid = FAIL;
    hid_t       my_fapl = H5P_DEFAULT;

    if (driver && *driver) {
        /* Get the correct FAPL for the given driver */
        if ((my_fapl = h5tools_get_fapl(fapl, driver, &drivernum)) < 0)
            goto done;

        H5E_BEGIN_TRY {
            fid = H5Fopen(fname, flags, my_fapl);
        } H5E_END_TRY;

        if (fid == FAIL)
            goto done;

    }
    else {
        /* Try to open the file using each of the drivers */
        for (drivernum = 0; drivernum < NUM_DRIVERS; drivernum++) {
            /* Get the correct FAPL for the given driver */
            if((my_fapl = h5tools_get_fapl(fapl, drivernames[drivernum], NULL)) < 0)
                goto done;

            H5E_BEGIN_TRY {
                fid = H5Fopen(fname, flags, my_fapl);
            } H5E_END_TRY;

            if (fid != FAIL)
                break;
            else {
                /* Close the FAPL */
                H5Pclose(my_fapl);
                my_fapl = H5P_DEFAULT;
            } /* end else */
        }
    }

    /* Save the driver name */
    if (drivername && drivername_size) {
        if (fid != FAIL) {
            HDstrncpy(drivername, drivernames[drivernum], drivername_size);
            drivername[drivername_size - 1] = '\0';
        }
        else {
            /*no file opened*/
            drivername[0] = '\0';
        }
    }

done:
    if(my_fapl != H5P_DEFAULT)
        H5Pclose(my_fapl);

    return fid;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Count the number of columns in a string.
 * Description:
 *      Count the number of columns in a string. This is the number of
 *      characters in the string not counting line-control characters.
 * Return:
 *      On success, returns the width of the string. Otherwise this function
 *      returns 0.
 * Programmer:
 *       Robb Matzke, Tuesday, April 27, 1999
 * Modifications:
 *-------------------------------------------------------------------------
 */
static size_t
h5tools_count_ncols(const char *s)
{
    register size_t i;

    for (i = 0; *s; s++)
        if (*s >= ' ')
            i++;

    return i;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_detect_vlen
 *
 * Purpose: Recursive check for any variable length data in given type.
 *
 * Return:
 *    TRUE : type conatains any variable length data
 *    FALSE : type doesn't contain any variable length data
 *    Negative value: error occur
 *
 * Programmer: Jonathan Kim  March 18, 2011
 *-------------------------------------------------------------------------
 */
htri_t
h5tools_detect_vlen(hid_t tid)
{
    htri_t ret;

    /* recursive detect any vlen data values in type (compound, array ...) */
    ret = H5Tdetect_class(tid, H5T_VLEN);
    if((ret == TRUE) || (ret < 0))
        goto done;

    /* recursive detect any vlen string in type (compound, array ...) */
    ret = h5tools_detect_vlen_str(tid);
    if((ret == TRUE) || (ret < 0))
        goto done;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_detect_vlen_str
 *
 * Purpose: Recursive check for variable length string of a datatype.
 *
 * Return:
 *    TRUE : type conatains any variable length string
 *    FALSE : type doesn't contain any variable length string
 *    Negative value: error occur
 *
 *-------------------------------------------------------------------------
 */
htri_t
h5tools_detect_vlen_str(hid_t tid)
{
    H5T_class_t tclass = -1;
    htri_t ret = FALSE;

    ret = H5Tis_variable_str(tid);
    if((ret == TRUE) || (ret < 0))
        goto done;

    tclass = H5Tget_class(tid);
    if(tclass == H5T_ARRAY || tclass == H5T_VLEN) {
        hid_t btid = H5Tget_super(tid);

        if(btid < 0) {
            ret = (htri_t)btid;
            goto done;
        }
        ret = h5tools_detect_vlen_str(btid);
        if((ret == TRUE) || (ret < 0)) {
            H5Tclose(btid);
            goto done;
        }
    }
    else if(tclass == H5T_COMPOUND) {
        unsigned nmembs;
        int snmembs = H5Tget_nmembers(tid);
        unsigned u;

        if(snmembs < 0) {
            ret = FAIL;
            goto done;
        }
        nmembs = (unsigned)snmembs;

        for(u = 0; u < nmembs; u++) {
            hid_t mtid = H5Tget_member_type(tid, u);

            ret = h5tools_detect_vlen_str(mtid);
            if((ret == TRUE) || (ret < 0)) {
                H5Tclose(mtid);
                goto done;
            }
            H5Tclose(mtid);
        }
    }

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Emit a simple prefix to STREAM.
 * Description:
 *      If /ctx->need_prefix/ is set then terminate the current line (if
 *      applicable), calculate the prefix string, and display it at the start
 *      of a line.
 * Return:
 *      None
 * Programmer:
 *      Robb Matzke, Monday, April 26, 1999
 * Modifications:
 *      Robb Matzke, 1999-09-29
 * If a new prefix is printed then the current element number is set back
 * to zero.
 *      pvn, 2004-07-08
 * Added support for printing array indices:
 *  the indentation is printed before the prefix (printed one indentation
 *  level before)
 *-------------------------------------------------------------------------
 */
void
h5tools_simple_prefix(FILE *stream, const h5tool_format_t *info,
                      h5tools_context_t *ctx, hsize_t elmtno, int secnum)
{
    h5tools_str_t prefix;
    h5tools_str_t str; /*temporary for indentation */
    size_t templength = 0;
    int i, indentlevel = 0;

    if (stream == NULL)
        return;

    if (!ctx->need_prefix)
        return;

    HDmemset(&prefix, 0, sizeof(h5tools_str_t));
    HDmemset(&str, 0, sizeof(h5tools_str_t));

    /* Terminate previous line, if any */
    if (ctx->cur_column) {
        PUTSTREAM(OPT(info->line_suf, ""), stream);
        HDputc('\n', stream);
        PUTSTREAM(OPT(info->line_sep, ""), stream);
    }

    /* Calculate new prefix */
    h5tools_str_prefix(&prefix, info, elmtno, ctx->ndims, ctx);

    /* Write new prefix to output */
    if (ctx->indent_level >= 0) {
        indentlevel = ctx->indent_level;
    }
    else {
        /*
         * This is because sometimes we don't print out all the header
         * info for the data (like the tattr-2.ddl example). If that happens
         * the ctx->indent_level is negative so we need to skip the above and
         * just print out the default indent levels.
         */
        indentlevel = ctx->default_indent_level;
    }

    /* when printing array indices, print the indentation before the prefix
       the prefix is printed one indentation level before */
    if (info->pindex) {
        for (i = 0; i < indentlevel - 1; i++) {
            PUTSTREAM(h5tools_str_fmt(&str, (size_t)0, info->line_indent), stream);
        }
    }

    if (elmtno == 0 && secnum == 0 && info->line_1st) {
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_1st), stream);
    }
    else if (secnum && info->line_cont) {
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_cont), stream);
    }
    else {
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_pre), stream);
    }

    templength = h5tools_str_len(&prefix);

    for (i = 0; i < indentlevel; i++) {
        /*we already made the indent for the array indices case */
        if (!info->pindex) {
            PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_indent), stream);
            templength += h5tools_str_len(&prefix);
        }
        else {
            /*we cannot count the prefix for the array indices case */
            templength += h5tools_str_len(&str);
        }
    }

    ctx->cur_column = ctx->prev_prefix_len = templength;
    ctx->cur_elmt = 0;
    ctx->need_prefix = 0;

    /* Free string */
    h5tools_str_close(&prefix);
    h5tools_str_close(&str);
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Emit a simple prefix to STREAM.
 * Description:
 *      If /ctx->need_prefix/ is set then terminate the current line (if
 *      applicable), calculate the prefix string, and display it at the start
 *      of a line. Calls region specific function.
 * Return:
 *      None
 *-------------------------------------------------------------------------
 */
void
h5tools_region_simple_prefix(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hsize_t elmtno, hsize_t *ptdata, int secnum)
{
    h5tools_str_t prefix;
    h5tools_str_t str; /*temporary for indentation */
    size_t templength = 0;
    int i, indentlevel = 0;

    if (stream == NULL)
        return;

    if (!ctx->need_prefix)
        return;

    HDmemset(&prefix, 0, sizeof(h5tools_str_t));
    HDmemset(&str, 0, sizeof(h5tools_str_t));

    /* Terminate previous line, if any */
    if (ctx->cur_column) {
        PUTSTREAM(OPT(info->line_suf, ""), stream);
        HDputc('\n', stream);
        PUTSTREAM(OPT(info->line_sep, ""), stream);
    }

    /* Calculate new prefix */
    h5tools_str_region_prefix(&prefix, info, elmtno, ptdata, ctx->ndims, ctx->p_max_idx, ctx);

    /* Write new prefix to output */
    if (ctx->indent_level >= 0) {
        indentlevel = ctx->indent_level;
    }
    else {
        /*
         * This is because sometimes we don't print out all the header
         * info for the data (like the tattr-2.ddl example). If that happens
         * the ctx->indent_level is negative so we need to skip the above and
         * just print out the default indent levels.
         */
        indentlevel = ctx->default_indent_level;
    }

    /* when printing array indices, print the indentation before the prefix
       the prefix is printed one indentation level before */
    if (info->pindex) {
        for (i = 0; i < indentlevel - 1; i++) {
            PUTSTREAM(h5tools_str_fmt(&str, (size_t)0, info->line_indent), stream);
        }
    }

    if (elmtno == 0 && secnum == 0 && info->line_1st) {
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_1st), stream);
    }
    else if (secnum && info->line_cont) {
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_cont), stream);
    }
    else {
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_pre), stream);
    }

    templength = h5tools_str_len(&prefix);

    for (i = 0; i < indentlevel; i++) {
        /*we already made the indent for the array indices case */
        if (!info->pindex) {
            PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_indent), stream);
            templength += h5tools_str_len(&prefix);
        }
        else {
            /*we cannot count the prefix for the array indices case */
            templength += h5tools_str_len(&str);
        }
    }

    ctx->cur_column = ctx->prev_prefix_len = templength;
    ctx->cur_elmt = 0;
    ctx->need_prefix = 0;

    /* Free string */
    h5tools_str_close(&prefix);
    h5tools_str_close(&str);
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Render an element to output STREAM.
 * Description:
 *      Prints the string buffer to the output STREAM. The string is
 *      printed according to the format described in INFO. The CTX struct
 *      contains context information shared between calls to this function.
 *
 * Return:
 *      False if a dimension end is reached, otherwise true
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      h5tools_str_t *buffer
 *      hsize_t *curr_pos
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      hsize_t curr_pos is the total data element position
 *      size_t ncols
 *      hsize_t local_elmt_counter is the local element loop counter
 *      hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_render_element(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, h5tools_str_t *buffer, hsize_t *curr_pos,
        size_t ncols, hsize_t local_elmt_counter, hsize_t elmt_counter)
{
    hbool_t  dimension_break = TRUE;
    char    *s;
    char    *section; /*a section of output  */
    int      secnum; /*section sequence number */
    int      multiline; /*datum was multiline  */

    if (stream == NULL)
        return dimension_break;

    s = h5tools_str_fmt(buffer, (size_t)0, "%s");

    /*
     * If the element would split on multiple lines if printed at our
     * current location...
     */
    if (info->line_multi_new == 1 &&
        (ctx->cur_column + h5tools_count_ncols(s) +
                HDstrlen(OPT(info->elmt_suf2, " ")) +
                HDstrlen(OPT(info->line_suf, ""))) > ncols) {
        if (ctx->prev_multiline) {
            /*
             * ... and the previous element also occupied more than one
             * line, then start this element at the beginning of a line.
             */
            ctx->need_prefix = TRUE;
        }
        else if ((ctx->prev_prefix_len + h5tools_count_ncols(s) +
                HDstrlen(OPT(info->elmt_suf2, " ")) +
                HDstrlen(OPT(info->line_suf, ""))) <= ncols) {
            /*
             * ...but *could* fit on one line otherwise, then we
             * should end the current line and start this element on its
             * own line.
             */
            ctx->need_prefix = TRUE;
        }
    }

    /*
     * We need to break after each row of a dimension---> we should
     * break at the end of the each last dimension well that is the
     * way the dumper did it before
     */
    if (info->arr_linebreak && ctx->cur_elmt) {
        if (ctx->size_last_dim && (ctx->cur_elmt % ctx->size_last_dim) == 0)
            ctx->need_prefix = TRUE;

        if (elmt_counter == ctx->size_last_dim) {
            ctx->need_prefix = TRUE;
            dimension_break = FALSE;
        }
    }

    /*
     * If the previous element occupied multiple lines and this element
     * is too long to fit on a line then start this element at the
     * beginning of the line.
     */
    if (info->line_multi_new == 1 &&
            ctx->prev_multiline &&
            (ctx->cur_column +
            h5tools_count_ncols(s) +
            HDstrlen(OPT(info->elmt_suf2, " ")) +
            HDstrlen(OPT(info->line_suf, ""))) > ncols)
        ctx->need_prefix = TRUE;

    /*
     * If too many elements have already been printed then we need to
     * start a new line.
     */
    if (info->line_per_line > 0 && ctx->cur_elmt >= info->line_per_line)
        ctx->need_prefix = TRUE;

    /*
     * Each OPTIONAL_LINE_BREAK embedded in the rendered string can cause
     * the data to split across multiple lines.  We display the sections
     * one-at a time.
     */
    multiline = 0;
    for (secnum = 0, multiline = 0;
             (section = HDstrtok(secnum ? NULL : s, OPTIONAL_LINE_BREAK));
             secnum++) {
        /*
         * If the current section plus possible suffix and end-of-line
         * information would cause the output to wrap then we need to
         * start a new line.
         */

        /*
         * check for displaying prefix for each section
         */
        if ( (ctx->cur_column + HDstrlen(section) +
                HDstrlen(OPT(info->elmt_suf2, " ")) +
                HDstrlen(OPT(info->line_suf, ""))) > ncols)
            ctx->need_prefix = 1;

        /*
         * Print the prefix or separate the beginning of this element
         * from the previous element.
         */
        if (ctx->need_prefix) {
            if (secnum)
                multiline++;

            /* pass to the prefix in h5tools_simple_prefix the total
             * position instead of the current stripmine position i;
             * this is necessary to print the array indices
             */
            *curr_pos = ctx->sm_pos + local_elmt_counter;

            h5tools_simple_prefix(stream, info, ctx, *curr_pos, secnum);
        }
        else if ((local_elmt_counter || ctx->continuation) && secnum == 0) {
            PUTSTREAM(OPT(info->elmt_suf2, " "), stream);
            ctx->cur_column += HDstrlen(OPT(info->elmt_suf2, " "));
        }

        /* Print the section */
        PUTSTREAM(section, stream);
        ctx->cur_column += HDstrlen(section);
    }

    ctx->prev_multiline = multiline;
    return dimension_break;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Render a region element to output STREAM.
 * Description:
 *      Prints the string buffer to the output STREAM. The string is
 *      printed according to the format described in INFO. The CTX struct
 *      contains context information shared between calls to this function.
 *
 * Return:
 *      False if a dimension end is reached, otherwise true
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      h5tools_str_t *buffer
 *      hsize_t *curr_pos
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      hsize_t curr_pos is the total data element position
 *      size_t ncols
 *      hsize_t *ptdata
 *      hsize_t local_elmt_counter is the local element loop counter
 *      hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_render_region_element(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, h5tools_str_t *buffer, hsize_t *curr_pos,
        size_t ncols, hsize_t *ptdata, hsize_t local_elmt_counter, hsize_t elmt_counter)
{
    hbool_t  dimension_break = TRUE;
    char    *s;
    char    *section; /*a section of output  */
    int      secnum; /*section sequence number */
    int      multiline; /*datum was multiline  */

    s = h5tools_str_fmt(buffer, (size_t)0, "%s");

    /*
     * If the element would split on multiple lines if printed at our
     * current location...
     */
    if (info->line_multi_new == 1 &&
            (ctx->cur_column + h5tools_count_ncols(s) +
                    HDstrlen(OPT(info->elmt_suf2, " ")) +
                    HDstrlen(OPT(info->line_suf, ""))) > ncols) {
        if (ctx->prev_multiline) {
            /*
             * ... and the previous element also occupied more than one
             * line, then start this element at the beginning of a line.
             */
            ctx->need_prefix = TRUE;
        }
        else if ((ctx->prev_prefix_len + h5tools_count_ncols(s) +
                HDstrlen(OPT(info->elmt_suf2, " ")) +
                HDstrlen(OPT(info->line_suf, ""))) <= ncols) {
            /*
             * ...but *could* fit on one line otherwise, then we
             * should end the current line and start this element on its
             * own line.
             */
            ctx->need_prefix = TRUE;
        }
    }

    /*
     * We need to break after each row of a dimension---> we should
     * break at the end of the each last dimension well that is the
     * way the dumper did it before
     */
    if (info->arr_linebreak && ctx->cur_elmt) {
        if (ctx->size_last_dim && (ctx->cur_elmt % ctx->size_last_dim) == 0)
            ctx->need_prefix = TRUE;

        if (elmt_counter == ctx->size_last_dim) {
            ctx->need_prefix = TRUE;
            dimension_break = FALSE;
        }
    }

    /*
     * If the previous element occupied multiple lines and this element
     * is too long to fit on a line then start this element at the
     * beginning of the line.
     */
    if (info->line_multi_new == 1 &&
            ctx->prev_multiline &&
            (ctx->cur_column +
            h5tools_count_ncols(s) +
            HDstrlen(OPT(info->elmt_suf2, " ")) +
            HDstrlen(OPT(info->line_suf, ""))) > ncols)
        ctx->need_prefix = TRUE;

    /*
     * If too many elements have already been printed then we need to
     * start a new line.
     */
    if (info->line_per_line > 0 && ctx->cur_elmt >= info->line_per_line)
        ctx->need_prefix = TRUE;

    /*
     * Each OPTIONAL_LINE_BREAK embedded in the rendered string can cause
     * the data to split across multiple lines.  We display the sections
     * one-at a time.
     */
    multiline = 0;
    for (secnum = 0, multiline = 0; (section = HDstrtok(secnum ? NULL : s,
            OPTIONAL_LINE_BREAK)); secnum++) {
        /*
         * If the current section plus possible suffix and end-of-line
         * information would cause the output to wrap then we need to
         * start a new line.
         */

        /*
         * Added the info->skip_first because the dumper does not want
         * this check to happen for the first line
         */
        if ((!info->skip_first || local_elmt_counter) &&
                (ctx->cur_column +
                        HDstrlen(section) +
                        HDstrlen(OPT(info->elmt_suf2, " ")) +
                        HDstrlen(OPT(info->line_suf, ""))) > ncols)
            ctx->need_prefix = 1;

        /*
         * Print the prefix or separate the beginning of this element
         * from the previous element.
         */
        if (ctx->need_prefix) {
            if (secnum)
                multiline++;

            /* pass to the prefix in h5tools_simple_prefix the total
             * position instead of the current stripmine position i;
             * this is necessary to print the array indices
             */
            *curr_pos = ctx->sm_pos + local_elmt_counter;

            h5tools_region_simple_prefix(stream, info, ctx, local_elmt_counter, ptdata, secnum);
        }
        else if ((local_elmt_counter || ctx->continuation) && secnum == 0) {
            PUTSTREAM(OPT(info->elmt_suf2, " "), stream);
            ctx->cur_column += HDstrlen(OPT(info->elmt_suf2, " "));
        }

        /* Print the section */
        PUTSTREAM(section, stream);
        ctx->cur_column += HDstrlen(section);
    }

    ctx->prev_multiline = multiline;
    return dimension_break;
}

/*-------------------------------------------------------------------------
 * Function:    init_acc_pos
 *
 * Purpose:     initialize accumulator and matrix position
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
init_acc_pos(h5tools_context_t *ctx, hsize_t *dims)
{
    int i;
    unsigned j;

    HDassert(ctx->ndims);

    ctx->acc[ctx->ndims - 1] = 1;
    for (i = (ctx->ndims - 2); i >= 0; i--) {
        ctx->acc[i] = ctx->acc[i + 1] * dims[i + 1];
    }
    for (j = 0; j < ctx->ndims; j++)
        ctx->pos[j] = 0;
}

/*-------------------------------------------------------------------------
 * Function: render_bin_output
 *
 * Purpose: Write one element of memory buffer to a binary file stream
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
int
render_bin_output(FILE *stream, hid_t container, hid_t tid, void *_mem,  hsize_t block_nelmts)
{
    HERR_INIT(int, SUCCEED)
    unsigned char     *mem  = (unsigned char*)_mem;
    size_t             size;   /* datum size */
    hsize_t            block_index;
    H5T_class_t        type_class;

    if((size = H5Tget_size(tid)) == 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((type_class = H5Tget_class(tid)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_class failed");

    switch (type_class) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_ENUM:
            block_index = block_nelmts * size;
            while(block_index > 0) {
                size_t bytes_in        = 0;    /* # of bytes to write  */
                size_t bytes_wrote     = 0;    /* # of bytes written   */

                if(block_index > sizeof(size_t))
                    bytes_in = sizeof(size_t);
                else
                    bytes_in = (size_t)block_index;

                bytes_wrote = HDfwrite(mem, 1, bytes_in, stream);

                if(bytes_wrote != bytes_in || (0 == bytes_wrote && HDferror(stream)))
                    H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");

                block_index -= (hsize_t)bytes_wrote;
                mem = mem + bytes_wrote;
            }
            break;
        case H5T_STRING:
            {
                unsigned int    i;
                H5T_str_t       pad;
                char           *s;
                unsigned char   tempuchar;

                pad = H5Tget_strpad(tid);

                for (block_index = 0; block_index < block_nelmts; block_index++) {
                    mem = ((unsigned char*)_mem) + block_index * size;

                    if (H5Tis_variable_str(tid)) {
                        s = *(char**) mem;
                        if (s != NULL)
                            size = HDstrlen(s);
                        else
                            H5E_THROW(FAIL, H5E_tools_min_id_g, "NULL string");
                    }
                    else {
                        s = (char *) mem;
                    }
                    for (i = 0; i < size && (s[i] || pad != H5T_STR_NULLTERM); i++) {
                        HDmemcpy(&tempuchar, &s[i], sizeof(unsigned char));
                        if (1 != HDfwrite(&tempuchar, sizeof(unsigned char), 1, stream))
                            H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
                    } /* i */
                } /* for (block_index = 0; block_index < block_nelmts; block_index++) */
            }
            break;
        case H5T_COMPOUND:
            {
                int snmembs;
                unsigned nmembs;

                if((snmembs = H5Tget_nmembers(tid)) < 0)
                    H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_nmembers of compound failed");
                nmembs = (unsigned)snmembs;

                for (block_index = 0; block_index < block_nelmts; block_index++) {
                    unsigned j;

                    mem = ((unsigned char*)_mem) + block_index * size;
                    for (j = 0; j < nmembs; j++) {
                        hid_t    memb;
                        size_t   offset;

                        offset = H5Tget_member_offset(tid, j);
                        memb   = H5Tget_member_type(tid, j);

                        if (render_bin_output(stream, container, memb, mem + offset, 1) < 0) {
                            H5Tclose(memb);
                            H5E_THROW(FAIL, H5E_tools_min_id_g, "render_bin_output of compound member failed");
                        }

                        H5Tclose(memb);
                    }
                }
            }
            break;
        case H5T_ARRAY:
            {
                int     k, ndims;
                hsize_t dims[H5S_MAX_RANK], temp_nelmts, nelmts;
                hid_t   memb;

                /* get the array's base datatype for each element */
                memb = H5Tget_super(tid);
                ndims = H5Tget_array_ndims(tid);
                H5Tget_array_dims2(tid, dims);
                HDassert(ndims >= 1 && ndims <= H5S_MAX_RANK);

                /* calculate the number of array elements */
                for (k = 0, nelmts = 1; k < ndims; k++) {
                    temp_nelmts = nelmts;
                    temp_nelmts *= dims[k];
                    nelmts = (size_t) temp_nelmts;
                }

                for (block_index = 0; block_index < block_nelmts; block_index++) {
                    mem = ((unsigned char*)_mem) + block_index * size;
                    /* dump the array element */
                    if (render_bin_output(stream, container, memb, mem, nelmts) < 0) {
                        H5Tclose(memb);
                        H5E_THROW(FAIL, H5E_tools_min_id_g, "render_bin_output failed");
                    }
                }
                H5Tclose(memb);
            }
            break;
        case H5T_VLEN:
            {
                hsize_t      nelmts;
                hid_t        memb;

                /* get the VL sequences's base datatype for each element */
                memb = H5Tget_super(tid);

                for (block_index = 0; block_index < block_nelmts; block_index++) {
                    mem = ((unsigned char*)_mem) + block_index * size;
                    /* Get the number of sequence elements */
                    nelmts = ((hvl_t *) mem)->len;

                    /* dump the array element */
                    if (render_bin_output(stream, container, memb, ((char *) (((hvl_t *) mem)->p)), nelmts) < 0) {
                        H5Tclose(memb);
                        H5E_THROW(FAIL, H5E_tools_min_id_g, "render_bin_output failed");
                    }
                }
                H5Tclose(memb);
            }
            break;
        case H5T_REFERENCE:
            {
                if (size == H5R_DSET_REG_REF_BUF_SIZE) {
                    /* if (H5Tequal(tid, H5T_STD_REF_DSETREG)) */
                    if (region_output) {
                        /* region data */
                        hid_t   region_id, region_space;
                        H5S_sel_type region_type;

                        for (block_index = 0; block_index < block_nelmts; block_index++) {
                            mem = ((unsigned char*)_mem) + block_index * size;
                            region_id = H5Rdereference2(container, H5P_DEFAULT, H5R_DATASET_REGION, mem);
                            if (region_id >= 0) {
                                region_space = H5Rget_region(container, H5R_DATASET_REGION, mem);
                                if (region_space >= 0) {
                                    region_type = H5Sget_select_type(region_space);
                                    if(region_type == H5S_SEL_POINTS)
                                        render_bin_output_region_points(region_space, region_id, stream, container);
                                    else
                                        render_bin_output_region_blocks(region_space, region_id, stream, container);
                                    H5Sclose(region_space);
                                } /* end if (region_space >= 0) */
                                H5Dclose(region_id);
                            } /* end if (region_id >= 0) */
                        }
                    } /* end if (region_output... */
                }
                else if (size == H5R_OBJ_REF_BUF_SIZE) {
                    /* if (H5Tequal(tid, H5T_STD_REF_OBJ)) */
                    ;
                }
            }
            break;
        default:
            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char*)_mem) + block_index * size;
                if (size != HDfwrite(mem, sizeof(char), size, stream))
                    H5E_THROW(FAIL, H5E_tools_min_id_g, "fwrite failed");
            }
            break;
    }

CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type blocks.
 *
 * Return:
 *      The function returns FAIL if there was an error, otherwise SUCEED
 *
 *-------------------------------------------------------------------------
 */
int
render_bin_output_region_data_blocks(hid_t region_id, FILE *stream,
    hid_t container, int ndims, hid_t type_id, hssize_t nblocks, hsize_t *ptdata)
{
    hsize_t     *dims1 = NULL;
    hsize_t     *start = NULL;
    hsize_t     *count = NULL;
    hsize_t      numelem;
    hsize_t      total_size[H5S_MAX_RANK];
    int          jndx;
    size_t       type_size;
    hid_t        mem_space = -1;
    void        *region_buf = NULL;
    int          blkndx;
    hid_t        sid1 = -1;
    int          ret_value = SUCCEED;

    /* Get the dataspace of the dataset */
    if((sid1 = H5Dget_space(region_id)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");

    /* Allocate space for the dimension array */
    if((dims1 = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for dims");

    /* find the dimensions of each data space from the block coordinates */
    numelem = 1;
    for (jndx = 0; jndx < ndims; jndx++) {
        dims1[jndx] = ptdata[jndx + ndims] - ptdata[jndx] + 1;
        numelem = dims1[jndx] * numelem;
    }

    /* Create dataspace for reading buffer */
    if((mem_space = H5Screate_simple(ndims, dims1, NULL)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

    if((type_size = H5Tget_size(type_id)) == 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((region_buf = HDmalloc(type_size * (size_t)numelem)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate region buffer");

    /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
    /*          1   2        n      1   2        n                                       */
    if((start = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for start");

    if((count = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for count");

    for (blkndx = 0; blkndx < nblocks; blkndx++) {
        for (jndx = 0; jndx < ndims; jndx++) {
            start[jndx] = ptdata[jndx + blkndx * ndims * 2];
            count[jndx] = dims1[jndx];
        }

        if(H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");

        if(H5Dread(region_id, type_id, mem_space, sid1, H5P_DEFAULT, region_buf) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dread failed");

        if(H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

        if(render_bin_output(stream, container, type_id, (char*)region_buf, numelem) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "render_bin_output of data region failed");
        /* Render the region data element end */
    } /* end for (blkndx = 0; blkndx < nblocks; blkndx++) */

 done:
    HDfree(start);
    HDfree(count);
    HDfree(region_buf);
    HDfree(dims1);

    if(H5Sclose(mem_space) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
    if(H5Sclose(sid1) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using blocks.
 *
 * Return:
 *      The function returns False if ERROR, otherwise True
 *
 *-------------------------------------------------------------------------
 */
hbool_t
render_bin_output_region_blocks(hid_t region_space, hid_t region_id,
        FILE *stream, hid_t container)
{
    HERR_INIT(hbool_t, TRUE)
    hssize_t     snblocks;
    hsize_t      nblocks;
    hsize_t      alloc_size;
    hsize_t     *ptdata;
    int          ndims;
    hid_t        dtype;
    hid_t        type_id;

    if((snblocks = H5Sget_select_hyper_nblocks(region_space)) <= 0)
        H5E_THROW(FALSE, H5E_tools_min_id_g, "H5Sget_select_hyper_nblocks failed");
    nblocks = (hsize_t)snblocks;

    /* Print block information */
    if((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5E_THROW(FALSE, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    alloc_size = nblocks * ndims * 2 * sizeof(ptdata[0]);
    HDassert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
    if((ptdata = (hsize_t*) HDmalloc((size_t) alloc_size)) == NULL)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "Could not allocate buffer for ptdata");

    H5_CHECK_OVERFLOW(nblocks, hssize_t, hsize_t);
    if(H5Sget_select_hyper_blocklist(region_space, (hsize_t) 0, (hsize_t) nblocks, ptdata) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Rget_select_hyper_blocklist failed");

    if((dtype = H5Dget_type(region_id)) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Dget_type failed");
    if((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Tget_native_type failed");

    render_bin_output_region_data_blocks(region_id, stream, container, ndims,
            type_id, nblocks, ptdata);

 done:
    HDfree(ptdata);

    if(H5Tclose(type_id) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    if(H5Tclose(dtype) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    H5_LEAVE(TRUE)

 CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type points.
 *
 * Return:
 *      The function returns FAIL on error, otherwise SUCCEED
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      size_t ncols
 *      int ndims is the number of dimensions of the region element
 *      hssize_t npoints is the number of points in the region
 *-------------------------------------------------------------------------
 */
int
render_bin_output_region_data_points(hid_t region_space, hid_t region_id,
        FILE *stream, hid_t container,
        int ndims, hid_t type_id, hssize_t npoints)
{
    hsize_t *dims1 = NULL;
    int      type_size;
    hid_t    mem_space = -1;
    void    *region_buf = NULL;
    int      ret_value = SUCCEED;

    if((type_size = H5Tget_size(type_id)) == 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((region_buf = HDmalloc(type_size * (size_t)npoints)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for region");

    /* Allocate space for the dimension array */
    if((dims1 = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for dims");

    dims1[0] = npoints;
    if((mem_space = H5Screate_simple(1, dims1, NULL)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

    if(H5Dread(region_id, type_id, mem_space, region_space, H5P_DEFAULT, region_buf) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dread failed");
    if(H5Sget_simple_extent_dims(region_space, dims1, NULL) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

    if(render_bin_output(stream, container, type_id, (char*)region_buf, npoints) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "render_bin_output of data points failed");

 done:
    HDfree(region_buf);
    HDfree(dims1);

    if(H5Sclose(mem_space) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using points.
 *
 * Return:
 *      The function returns False if the last dimension has been reached, otherwise True
 *
 *-------------------------------------------------------------------------
 */
hbool_t
render_bin_output_region_points(hid_t region_space, hid_t region_id,
        FILE *stream, hid_t container)
{
    HERR_INIT(hbool_t, TRUE)
    hssize_t npoints;
    int      ndims;
    hid_t    dtype;
    hid_t    type_id;

    if((npoints = H5Sget_select_elem_npoints(region_space)) <= 0)
        H5E_THROW(FALSE, H5E_tools_min_id_g, "H5Sget_select_elem_npoints failed");

    /* Allocate space for the dimension array */
    if((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5E_THROW(FALSE, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    if((dtype = H5Dget_type(region_id)) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Dget_type failed");

    if((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(FALSE, H5E_tools_min_id_g, "H5Tget_native_type failed");

    render_bin_output_region_data_points(region_space, region_id,
            stream, container, ndims, type_id, npoints);

 done:
    if(H5Tclose(type_id) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    if(H5Tclose(dtype) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    H5_LEAVE(ret_value)
CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_is_zero
 *
 * Purpose: Determines if memory is initialized to all zero bytes.
 *
 * Return:  TRUE if all bytes are zero; FALSE otherwise
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_is_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char *) _mem;

    while (size-- > 0)
        if (mem[size])
            return FALSE;

    return TRUE;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_is_obj_same
 *
 * Purpose: Check if two given object IDs or link names point to the same object.
 *
 * Parameters:
 *             hid_t loc_id1: location of the first object
 *             char *name1:   link name of the first object.
 *                             Use "." or NULL if loc_id1 is the object to be compared.
 *             hid_t loc_id2: location of the second object
 *             char *name1:   link name of the first object.
 *                             Use "." or NULL if loc_id2 is the object to be compared.
 *
 * Return:  TRUE if it is the same object; FALSE otherwise.
 *
 * Programmer: Peter Cao
 *             4/27/2011
  *
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_is_obj_same(hid_t loc_id1, const char *name1,
                        hid_t loc_id2, const char *name2)
{
    H5O_info_t oinfo1,  oinfo2;
    hbool_t ret_val = 0;

    if ( name1 && HDstrcmp(name1, "."))
      H5Oget_info_by_name(loc_id1, name1, &oinfo1, H5P_DEFAULT);
    else
      H5Oget_info(loc_id1, &oinfo1);

    if ( name2 && HDstrcmp(name2, "."))
      H5Oget_info_by_name(loc_id2, name2, &oinfo2, H5P_DEFAULT);
    else
      H5Oget_info(loc_id2, &oinfo2);

    if (oinfo1.fileno == oinfo2.fileno && oinfo1.addr==oinfo2.addr)
      ret_val = 1;

    return ret_val;
}

