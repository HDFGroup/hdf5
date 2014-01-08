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
    "h5clear_sec2.h5",		/* 0 -- sec2 file */
    "h5clear_core.h5",		/* 1 -- core file */
    "h5clear_fam_%05d.h5",	/* 2 -- family files */
    "h5clear_split",		/* 3 -- split files */
    "h5clear_invalid.h5"	/* 4 -- sec2 file with invalid superblock version */
};

#define FAMILY_SIZE 1024U
#define CORE_INCREMENT 1024U

#define SUPER_VERS_OFF          8
#define SUPER_VERS_SIZE         1
#define SUPER_VERS_LATEST       2

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	To create HDF5 files with non-zero status_flags in the superblock
 *		via flushing and exiting without closing the library.
 *
 *		Due to file locking, status_flag in the superblock will be 
 *		nonzero after H5Fcreate.  The library will clear status_flags
 *		on file closing.  This program, after "H5Fcreate" the files,
 *		exits without going through library closing. Thus, status_flags
 *		for these files are not cleared and users cannot open them.
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
    hid_t fapl, new_fapl;	/* File access property lists */
    char fname[512];		/* File name */
    hbool_t new_format;		/* To use latest library format or not */
    int fd;			/* File descriptor */
    uint8_t super_vers;		/* Superblock version */
    ssize_t bytes_written;	/* The # of bytes written to the file */

    /* Create a copy of the file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
	goto error;

    /* Copy the file access property list */
    if((new_fapl = H5Pcopy(fapl)) < 0)
	goto error;
    /* Set to latest library format */
    if(H5Pset_libver_bounds(new_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	goto error;

    /* Create file with/without latest library format */
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
	if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) 
	    goto error;

	/* Flush the file */
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	    goto error;
    
	/* Close the property list */
	if(H5Pclose(my_fapl) < 0)
	    goto error;

	/*
	 * Create a core file
	 */
	/* Create a copy of file access property list */
	if((my_fapl = H5Pcopy(fapl2)) < 0)
	    goto error;

	/* Setup the fapl for the family file driver */
	if(H5Pset_fapl_core(my_fapl, (size_t)CORE_INCREMENT, TRUE) < 0)
	    goto error;

	/* Create the file */
	sprintf(fname, "%s%s", new_format? "latest_":"", FILENAME[1]);
	if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) 
	    goto error;

	/* Flush the file */
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	    goto error;

	/* Close the property list */
	if(H5Pclose(my_fapl) < 0)
	    goto error;

	/*
	 * Create a family file
	 */
	/* Create a copy of file access property list */
	if((my_fapl = H5Pcopy(fapl2)) < 0)
	    goto error;

	/* Setup the fapl for the family file driver */
	if(H5Pset_fapl_family(my_fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
	    goto error;

	/* Create the file */
	sprintf(fname, "%s%s", new_format? "latest_":"", FILENAME[2]);
	if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) 
	    goto error;

	/* Flush the file */
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	    goto error;

	/* Close the property list */
	if(H5Pclose(my_fapl) < 0)
	    goto error;

	/*
	 * Create a split file
	 */
	 /* Create a copy of file access property list */
	my_fapl = H5Pcopy(fapl2);

	/* Setup the fapl for the split file driver */
	H5Pset_fapl_split(my_fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT);

	/* Create the file */
	sprintf(fname, "%s%s", new_format? "latest_":"", FILENAME[3]);
	if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
	    goto error;

	/* Flush the file */
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	    goto error;

	/* Close the property list */
	if(H5Pclose(my_fapl) < 0)
	    goto error;

	/* 
	 * Create a sec2 file but change its superblock version # 
	 */
	/* Create a copy of file access property list */
	if((my_fapl = H5Pcopy(fapl2)) < 0)
	    goto error;
	/* Create the file */
	sprintf(fname, "%s%s", new_format? "latest_":"", FILENAME[4]);
	if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) 
	    goto error;

	/* Flush the file */
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
	    goto error;
    
	/* Close the property list */
	if(H5Pclose(my_fapl) < 0)
	    goto error;

	/* Open the test file via system call "open" */
	if((fd = HDopen(fname, O_RDWR, 0666)) < 0) {
	    HDfprintf(stdout, "cannot open the file\n");
	    goto error;
	}

	/* Position the file to superblock version via system call "lseek" */
	if(HDlseek(fd, (off_t)SUPER_VERS_OFF, SEEK_SET) < 0) {
	    HDfprintf(stdout, "cannot lseek the file superblock version\n");
	    goto error;
	}

	/* Change to an incorrect superblock version */
	super_vers = SUPER_VERS_LATEST + 1;
	/* Write to the file via system call "write" */
	if((bytes_written = HDwrite(fd, &super_vers, (size_t)SUPER_VERS_SIZE)) < 0) {
	    HDfprintf(stdout, "cannot write to the file with incorrect superblock version\n");
	    goto error;
	}

	/* Close the file via system call "close" */
	if(HDclose(fd) < 0) {
	    HDfprintf(stdout, "cannot close the file\n");
	    goto error;
	}

    } /* end for */

    /* Close the property lists */
    if(H5Pclose(fapl) < 0)
	goto error;
    if(H5Pclose(new_fapl) < 0)
	goto error;

    fflush(stdout);
    fflush(stderr);

    /* Not going through library closing by calling _exit(0) with success */
    HD_exit(0);

error:

    /* Exit with failure */
    HD_exit(1);
}
