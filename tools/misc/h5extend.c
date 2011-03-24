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
 * Programmer:  Mike McGreevy
 *              March 2, 2011
 *
 * Purpose:     Reads the 'EOA' message stored in the superblock
 *              extension of a file, and extends the file via a truncate
 *              call to this size. The 'EOA' message is then either updated
 *              with the new value, or removed entirely. (based on user input).
 *
 */

#include "hdf5.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "H5Iprivate.h"
#define H5F_PACKAGE 
#include "H5Fpkg.h"

/* Name of tool */
#define PROGRAMNAME "h5extend"

/* Command Line Switches */
static hbool_t  quiet_g = FALSE;
static hbool_t  remove_g = FALSE;
static hbool_t  extend_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Prints a usage message.
 *
 * Return:      void
 *
 * Programmer:  Mike McGreevy
 *              March 2, 2011
 *
 *-------------------------------------------------------------------------
 */
static void
usage (FILE * outputfile)
{
    HDfprintf(outputfile, "%s [OPTIONS] SRC\n",h5tools_getprogname());
    HDfprintf(outputfile, "  OPTIONS:\n");
    HDfprintf(outputfile, "    -h, --help       Display this help message.\n");
    HDfprintf(outputfile, "    -r, --remove     Remove 'EOA' message from superblock extension.\n");
    HDfprintf(outputfile, "    -e, --extend     Extend the file to match its 'EOA' value.\n");
    HDfprintf(outputfile, "    -q, --quiet      Work without displaying standard output.\n");
} /* usage */


/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown MPI and/or HDF5 and call exit()
 *
 * Return:      Does not return
 *
 * Programmer: Quincey Koziol, 2/13/2007
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    h5tools_close();
    exit(ret);
} /* end leave() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Main 'h5extend' tool.
 *
 * Return:      Success: EXIT_SUCCESS
 *              Failure: EXIT_FAILURE
 *
 * Programmer:  Mike McGreevy
 *              March 2, 2011
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    hbool_t eoa_msg_found = 0; /* Bool indicating if EOA message found */
    const char *filename; /* Source file name */
    int argno=1; /* Program argument number */
    hid_t fid = -1;
    H5F_t * f = NULL;
    haddr_t eoa,eof,filesize;
    FILE * outputfile = stdout;
    FILE * errorfile = stderr;

    /* Disable the HDF5 library's error reporting */
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Set program name */
    h5tools_setprogname(PROGRAMNAME);

    /* Parse Switches */
    while (argno<argc && '-'==argv[argno][0]) {
        if ((!strcmp (argv[argno], "-e"))||(!strcmp (argv[argno], "--extend"))) {
            extend_g = 1;
            argno++;
        } /* end if */ 
        else if ((!strcmp (argv[argno], "-r"))||(!strcmp (argv[argno], "--remove"))) {
            remove_g = 1;
            argno++;
        } /* end else if */ 
        else if ((!strcmp (argv[argno], "-q"))||(!strcmp (argv[argno], "--quiet"))) {
            quiet_g = TRUE;
            argno++;
        } /* end else if */ 
        else if ((!strcmp (argv[argno], "-h"))||(!strcmp (argv[argno], "--help"))) {
            usage(outputfile);
            leave(EXIT_SUCCESS);
        } /* end else if */ 
        else {
            error_msg("Unrecognized option (%s) provided.\n", argv[argno]);
            usage(errorfile);
            leave(EXIT_FAILURE);
        } /* end else */
    } /* end while */

    /* Print usage if file name is missing */
    if (argno>=argc) {
        error_msg("No file provided.\n");
        usage(errorfile);
        leave(EXIT_FAILURE);
    } /* end if */

    /* Get the name of the source file */
    filename = argv[argno++];

    /* Open File */
    if ((fid = h5tools_fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT,NULL,NULL,0)) < 0) {
        error_msg("Unable to open source file %s.\n", filename);
        leave(EXIT_FAILURE);
    } /* end if */

    /* Get internal file pointer */
    if ((f = (H5F_t *)H5I_object(fid)) == NULL) {
        error_msg("Unable to get internal file pointer.\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* Check to see if the EOA value is stored via an 'EOA' message */
    if (f->shared->sblock->ext_addr != HADDR_UNDEF) {
        H5O_loc_t ext_loc;      /* "Object location" for superblock extension */

        /* Open the superblock extension */
        if (H5F_super_ext_open(f, f->shared->sblock->ext_addr, &ext_loc) < 0) {
            error_msg("Unable to open superblock extension.\n");
            leave(EXIT_FAILURE);
        } /* end if */

        /* Check to see if there is an 'EOA' message */
        if (H5O_msg_exists(&ext_loc, H5O_EOA_ID, H5AC_dxpl_id)) {
            eoa_msg_found = 1;

            /* Retrieve 'EOA' value from message in superblock extension */
            if (H5O_msg_read(&ext_loc, H5O_EOA_ID, &eoa, H5AC_dxpl_id) == NULL) {
                error_msg("Unable to read 'EOA' message in superblock extension.\n");
                leave(EXIT_FAILURE);
            } /* end if */
            if (!quiet_g) HDfprintf(outputfile, "  Superblock extension contains 'EOA' message with value = %a\n", eoa);

            /* If requested, remove the 'EOA' message from the superblock extension */
            if (remove_g) {
                if (H5O_msg_remove(&ext_loc, H5O_EOA_ID, H5O_ALL, TRUE, H5AC_dxpl_id) < 0) {
                    error_msg("Unable to remove 'EOA' message.\n");
                    leave(EXIT_FAILURE);
                } /* end if */
                if (!quiet_g) HDfprintf(outputfile, "  Removed 'EOA' message from superblock extension.\n");
            } /* end if */
        } /* end if */

        /* Close superblock extension */
        if (H5F_super_ext_close(f, &ext_loc, H5AC_dxpl_id, FALSE) < 0) {
            error_msg("Unable to close superblock extension.\n");
            leave(EXIT_FAILURE);
        } /* end if */
    } /* end if */

    /* Retrieve 'EOA' value from file driver layer if not found in sblock extension */
    if (!eoa_msg_found) {
        if ((eoa = H5FDget_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF) {
            error_msg("Unable to determine 'EOA' message from file driver.\n");
            leave(EXIT_FAILURE);
        } /* end if */
        if (!quiet_g) HDfprintf(outputfile, "  No 'EOA' message contained in this file. File driver's reported EOA value = %a\n", eoa);
    } /* end if */

    /* Get 'EOF' value */
    if ((eof = H5FDget_eof(f->shared->lf)) == HADDR_UNDEF) {
        error_msg("Unable to retrieve 'EOF' value from file driver.\n");
        leave(EXIT_FAILURE);
    } /* end if */
    if (!quiet_g) HDfprintf(outputfile, "  File EOF value = %a\n", eof);

    /* Get file size */
    if (H5Fget_filesize(fid, &filesize) < 0) {
        error_msg("Unable to retrieve file size.\n");
        leave(EXIT_FAILURE);
    } /* end if */
    if (!quiet_g) HDfprintf(outputfile, "  File size = %a\n", filesize);

    /* Suggest extending the file if EOA/EOF do not match (and request to remove/extend not already made) */
    if ((!extend_g && !remove_g)&&(eoa != eof))
        if (!quiet_g) HDfprintf(outputfile, "  EOA and EOF do not match! Suggest extending the file for backward compatibility.\n");

    /* Extend the file, if requested */
    if (extend_g) {
        if (eoa != eof) {
            /* truncate file to match 'EOA' value */
            if (!quiet_g) HDfprintf(outputfile, "  Truncating file to match EOA of %a\n", eoa);
            if (H5FDtruncate(f->shared->lf, H5AC_dxpl_id, 0) < 0) {
                error_msg("Unable to truncate file.\n");
                leave(EXIT_FAILURE);
            } /* end if */

            /* Mark superblock dirty, so on file close it is re-written with new filesize value */
            if (H5F_super_dirty(f) < 0) {
                error_msg("Unable to mark file superblock dirty.\n");
                leave(EXIT_FAILURE);
            } /* end if */
        } /* end if */
        else {
            if (!quiet_g) HDfprintf(outputfile,"  EOA and EOF are already the same. Taking no action to extend the file.\n");
        } /* end else */
    } /* end if */

    /* Close File */
    if (H5Fclose(fid) < 0) {
        error_msg("Failed while closing file.\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* Shut down h5tools lib */
    h5tools_close();

    return EXIT_SUCCESS;
} /* main */

