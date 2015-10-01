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
#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */
#include "H5Fpkg.h"

/* Name of tool */
#define PROGRAMNAME "h5extend"


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
usage(FILE * outputfile)
{
    HDfprintf(outputfile, "%s [OPTIONS] SRC\n", h5tools_getprogname());
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
main(int argc, const char *argv[])
{
    hbool_t should_truncate = FALSE;    /* Flag indicating if EOA and EOF are not equal */
    hbool_t quiet = FALSE;              /* Flag indicating 'quiet' output */
    hbool_t rm_msg = FALSE;             /* Flag indicating EOA message should be removed */
    hbool_t extend = FALSE;             /* Flag indicating file should be extended */
    const char *filename;               /* Source file name */
    int argno = 1;                      /* Program argument number */
    hid_t fid = -1;                     /* HDF5 file ID for file to operate on */
    H5F_t * f = NULL;                   /* Internal file pointer for HDF5 file */
    FILE * outputfile = stdout;         /* File handle for normal output */
    FILE * errorfile = stderr;          /* File handle for error output */

    /* Disable the HDF5 library's error reporting */
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Set program name */
    h5tools_setprogname(PROGRAMNAME);

    /* Parse Switches */
    while(argno < argc && '-' == argv[argno][0]) {
        if((!HDstrcmp(argv[argno], "-e")) || (!HDstrcmp(argv[argno], "--extend"))) {
            extend = TRUE;
            argno++;
        } /* end if */ 
        else if((!HDstrcmp(argv[argno], "-r")) || (!HDstrcmp(argv[argno], "--remove"))) {
            rm_msg = TRUE;
            argno++;
        } /* end else if */ 
        else if((!HDstrcmp(argv[argno], "-q")) || (!HDstrcmp(argv[argno], "--quiet"))) {
            quiet = TRUE;
            argno++;
        } /* end else if */ 
        else if((!HDstrcmp(argv[argno], "-h")) || (!HDstrcmp(argv[argno], "--help"))) {
            usage(outputfile);
            leave(EXIT_SUCCESS);
        } /* end else if */ 
        else {
            error_msg("Unrecognized option '%s' provided.\n", argv[argno]);
            usage(errorfile);
            leave(EXIT_FAILURE);
        } /* end else */
    } /* end while */

    /* Print usage if file name is missing */
    if(argno >= argc) {
        error_msg("No file provided.\n");
        usage(errorfile);
        leave(EXIT_FAILURE);
    } /* end if */

    /* Get the name of the source file */
    filename = argv[argno++];

    /* Open File */
    if((fid = h5tools_fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT, NULL, NULL, 0)) < 0) {
        error_msg("Unable to open source file '%s'.\n", filename);
        leave(EXIT_FAILURE);
    } /* end if */

    /* Get internal file pointer */
    if((f = (H5F_t *)H5I_object(fid)) == NULL) {
        error_msg("Unable to get internal file pointer.\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* If a SB extension exists check for EOA message */
    if(f->shared->sblock->ext_addr != HADDR_UNDEF) {
        H5O_loc_t ext_loc;      /* "Object location" for superblock extension */
        H5O_eoa_t eoa_msg;      /* 'EOA' Message' */
        haddr_t filesize;       /* 'Actual' size of the file (max of EOA & EOF) */
        haddr_t eof;            /* EOF value for the file */
        H5FD_mem_t mt;

        /* Open the superblock extension */
        if(H5F_super_ext_open(f, f->shared->sblock->ext_addr, &ext_loc) < 0) {
            error_msg("Unable to open superblock extension.\n");
            leave(EXIT_FAILURE);
        } /* end if */

        /* Check to see if there is an 'EOA' message */
        if(H5O_msg_exists(&ext_loc, H5O_EOA_ID, H5AC_dxpl_id)) {
            /* Retrieve 'EOA' message */ 
            if(H5O_msg_read(&ext_loc, H5O_EOA_ID, &eoa_msg, H5AC_dxpl_id) == NULL) {
                error_msg("Unable to read 'EOA' message in superblock extension.\n");
                leave(EXIT_FAILURE);
            } /* end if */

            if(!quiet) 
                HDfprintf(outputfile, "  Superblock extension contains 'EOA' message\n");

            if(f->shared->feature_flags & H5FD_FEAT_MULTIPLE_MEM_TYPE_BACKENDS) {
                for(mt = H5FD_MEM_SUPER; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1)) {
                    if((eof = H5FDget_eof(f->shared->lf, mt)) == HADDR_UNDEF) {
                        error_msg("Unable to determine 'EOF' message from file driver.\n");
                        leave(EXIT_FAILURE);
                    } /* end if */

                    if(!quiet)
                        HDfprintf(outputfile, "  %d: EOA = %a  EOF = %a\n", mt, eoa_msg.memb_eoa[mt-1], eof);

                    if(eof != eoa_msg.memb_eoa[mt-1])
                        should_truncate = TRUE;
                } /* end for */
            } /* end if */
            else {
                if((eof = H5FDget_eof(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF) {
                    error_msg("Unable to determine 'EOF' message from file driver.\n");
                    leave(EXIT_FAILURE);
                } /* end if */

                if(!quiet)
                    HDfprintf(outputfile, "  EOA = %a  EOF = %a\n", eoa_msg.memb_eoa[0], eof);

                /* Get file size */
                if(H5Fget_filesize(fid, &filesize) < 0) {
                    error_msg("Unable to retrieve file size.\n");
                    leave(EXIT_FAILURE);
                } /* end if */
                if(!quiet)
                    HDfprintf(outputfile, "  File size = %a\n", filesize);

                if(eoa_msg.memb_eoa[0] != eof)
                    should_truncate = TRUE;
            } /* end else */

            /* If requested, remove the 'EOA' message from the superblock extension */
            if(rm_msg) {
                if(H5O_msg_remove(&ext_loc, H5O_EOA_ID, H5O_ALL, TRUE, H5AC_dxpl_id) < 0) {
                    error_msg("Unable to remove 'EOA' message.\n");
                    leave(EXIT_FAILURE);
                } /* end if */
                if(!quiet) 
                    HDfprintf(outputfile, "  Removed 'EOA' message from superblock extension.\n");
            } /* end if */
        } /* end if */
        else {
            if(!quiet)
                HDfprintf(outputfile, "  No 'EOA' message contained in this file.\n");
        } /* end else */

        /* Close superblock extension */
        if(H5F_super_ext_close(f, &ext_loc, H5AC_dxpl_id, FALSE) < 0) {
            error_msg("Unable to close superblock extension.\n");
            leave(EXIT_FAILURE);
        } /* end if */
    } /* end if */

    /* Extend the file, if requested */
    if(extend) {
        /* truncate file to match 'EOA' value */
        if(should_truncate) {
            if(!quiet) {
                HDfprintf(outputfile, "  Truncating file to match EOA\n");
            } /* end if */

            if(H5FDtruncate(f->shared->lf, H5AC_dxpl_id, 0) < 0) {
                error_msg("Unable to truncate file.\n");
                leave(EXIT_FAILURE);
            } /* end if */

            /* Mark superblock dirty, so on file close it is re-written with new filesize value */
            if(H5F_super_dirty(f) < 0) {
                error_msg("Unable to mark file superblock dirty.\n");
                leave(EXIT_FAILURE);
            } /* end if */
        } /* end if */
        else {
            if(!quiet)
                HDfprintf(outputfile,"  EOA and EOF are already the same. Taking no action to extend the file.\n");
        } /* end else */
    } /* end if */
    else {
        /* Suggest extending the file if EOA/EOF do not match (and request to remove/extend not already made) */
        if(!rm_msg && should_truncate)
            if(!quiet)
                HDfprintf(outputfile, "  EOA and EOF do not match! Suggest extending the file for backward compatibility.\n");
    } /* end else */

    /* Close File */
    if(H5Fclose(fid) < 0) {
        error_msg("Failed while closing file.\n");
        leave(EXIT_FAILURE);
    } /* end if */

    /* Shut down h5tools lib */
    h5tools_close();

    return EXIT_SUCCESS;
} /* main */

