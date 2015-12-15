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
 * Programmer:  
 *
 * Purpose:	
 */

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME 	"h5clear"

/* Make this private property (defined in H5Fprivate.h) available to h5clear. */
#define H5F_ACS_CLEAR_STATUS_FLAGS_NAME            "clear_status_flags"

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Close the tools library and exit
 *
 * Return:   	Does not return
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    h5tools_close();
    HDexit(ret);

} /* leave() */

/*-------------------------------------------------------------------------
 * Function: 	usage
 *
 * Purpose: 	Prints a usage message
 *
 * Return: 	void
 *
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    HDfprintf(stdout, "usage: h5clear filename\n");

} /* usage() */



/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:
 *		Failure:
 *
 * Programmer:	
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    char *fname;		/* File name */
    hbool_t clear = TRUE;  	/* To clear the status_flags in the file's superblock */
    hid_t fapl = -1;   		/* File access property list */
    hid_t fid = -1;		/* File ID */

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);
    
    /* Disable the HDF5 library's error reporting */
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* initialize h5tools lib */
    h5tools_init();

    /* Check for the # of arguments */
    if(argc != 2) {
        usage();
        leave(EXIT_FAILURE);
    }

    /* Duplicate the file name */
    fname = HDstrdup(argv[opt_ind]);

    /* Get a copy of the file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        error_msg("H5Pcreate\n");
        exit(EXIT_FAILURE);
    }

    /* Set to clear the status_flags in the file's superblock */
    /* This is a private property used by h5clear only */
    if(H5Pset(fapl, H5F_ACS_CLEAR_STATUS_FLAGS_NAME, &clear) < 0) {
	error_msg("H5Pset\n");
	exit(EXIT_FAILURE);
    }

    if((fid = h5tools_fopen(fname, H5F_ACC_RDWR, fapl, NULL, NULL, (size_t)0)) < 0) {
	error_msg("h5tools_fopen\n");
	exit(EXIT_FAILURE);
    }

    /* Close the file */
    if(H5Fclose(fid) < 0) {
	error_msg("H5Fclose\n");
	exit(EXIT_FAILURE);
    }

    /* CLose the property list */
    if(H5Pclose(fapl) < 0) {
	error_msg("H5Pclose\n");
	exit(EXIT_FAILURE);
    }

    return EXIT_SUCCESS;
} /* main() */
