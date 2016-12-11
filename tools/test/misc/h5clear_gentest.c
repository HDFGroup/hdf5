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
#include "hdf5.h"
#include "H5private.h"

/* The HDF5 test files */
const char *FILENAME[] = {
    "h5clear_sec2_v3.h5",		/* 0 -- sec2 file with superblock version 3 */
    "h5clear_log_v3.h5",		/* 1 -- log file with superblock veresion 3 */
    "h5clear_sec2_v0.h5",		/* 2 -- sec2 file with superblock version 0 */
    "h5clear_sec2_v2.h5"		/* 3 -- sec2 file with superblock version 2 */
};

#define KB 		1024U

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	To create HDF5 files with non-zero status_flags in the superblock
 *		via flushing and exiting without closing the library.
 *
 *		Due to file locking, status_flags in the superblock will be 
 *		nonzero after H5Fcreate.  The library will clear status_flags
 *		on file closing.  This program, after "H5Fcreate" the files,
 *		exits without going through library closing. Thus, status_flags
 *		for these files are not cleared.
 *		The library will check consistency of status_flags when opening
 *		a file with superblock >= v3 and will return error accordingly.
 *		The library will not check status_flags when opening a file 
 *		with < v3 superblock.
 *
 *		These files are used by "h5clear" to see if the tool clears
 *		status_flags properly so users can open the files afterwards.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Vailin Choi; July 2013
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fid;			/* File ID */
    hid_t fcpl;			/* File creation property list */
    hid_t fapl, new_fapl;	/* File access property lists */
    char fname[512];		/* File name */
    unsigned new_format;		/* To use latest library format or not */

    /* Create a copy of the file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
	goto error;

    /* Copy the file access property list */
    if((new_fapl = H5Pcopy(fapl)) < 0)
	goto error;
    /* Set to latest library format */
    if(H5Pset_libver_bounds(new_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	goto error;

    /* Files created within this for loop will have v3 superblock and nonzero status_flags */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
	hid_t fapl2, my_fapl;	/* File access property lists */

	/* Set to use the appropriate file access property list */
	if(new_format)
	    fapl2 = new_fapl;
	else
	    fapl2 = fapl;
	/*
	 * Create a sec2 file
	 */
	if((my_fapl = H5Pcopy(fapl2)) < 0)
	    goto error;
	/* Create the file */
	sprintf(fname, "%s%s", new_format? "latest_":"", FILENAME[0]);
	if((fid = H5Fcreate(fname, H5F_ACC_TRUNC | (new_format ? 0 : H5F_ACC_SWMR_WRITE), H5P_DEFAULT, my_fapl)) < 0) 
	    goto error;

	/* Flush the file */
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	    goto error;
    
	/* Close the property list */
	if(H5Pclose(my_fapl) < 0)
	    goto error;

	/*
	 * Create a log file
	 */
	/* Create a copy of file access property list */
	if((my_fapl = H5Pcopy(fapl2)) < 0)
	    goto error;

	/* Setup the fapl for the log driver */
	if(H5Pset_fapl_log(my_fapl, "append.log", (unsigned long long)H5FD_LOG_ALL, (size_t)(4 * KB)) < 0)
	    goto error;

	/* Create the file */
	sprintf(fname, "%s%s", new_format? "latest_":"", FILENAME[1]);
	if((fid = H5Fcreate(fname, H5F_ACC_TRUNC | (new_format ? 0 : H5F_ACC_SWMR_WRITE), H5P_DEFAULT, my_fapl)) < 0) 
	    goto error;

	/* Flush the file */
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	    goto error;

	/* Close the property list */
	if(H5Pclose(my_fapl) < 0)
	    goto error;

    } /* end for */

    /* 
     * Create a sec2 file with v0 superblock but nonzero status_flags
     */
    if((fid = H5Fcreate(FILENAME[2], H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) 
	goto error;

    /* Flush the file */
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	goto error;


    /* 
     * Create a sec2 file with v2 superblock but nonzero status_flags
     */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
	goto error;
    if(H5Pset_shared_mesg_nindexes(fcpl, 1) < 0)
        goto error;
    if(H5Pset_shared_mesg_index(fcpl, 0, H5O_SHMESG_DTYPE_FLAG, 50) < 0)
        goto error;

    if((fid = H5Fcreate(FILENAME[3], H5F_ACC_TRUNC, fcpl, fapl)) < 0) 
	goto error;

    /* Flush the file */
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	goto error;

    
    /* Close the property lists */
    if(H5Pclose(fapl) < 0)
	goto error;
    if(H5Pclose(new_fapl) < 0)
	goto error;
    if(H5Pclose(fcpl) < 0)
	goto error;

    fflush(stdout);
    fflush(stderr);

    /* Not going through library closing by calling _exit(0) with success */
    HD_exit(0);

error:

    /* Exit with failure */
    HD_exit(1);
}
