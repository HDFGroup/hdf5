/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME "h5copy"

/* command-line options: short and long-named parameters */
static const char *s_opts = "d:f:hi:o:ps:vVE";
static struct long_options l_opts[] = {
    { "destination", require_arg, 'd' },
    { "flag", require_arg, 'f' },
    { "help", no_arg, 'h' },
    { "input", require_arg, 'i' },
    { "output", require_arg, 'o' },
    { "parents", no_arg, 'p' },
    { "source", require_arg, 's' },
    { "verbose", no_arg, 'v' },
    { "version", no_arg, 'V' },
    { "enable-error-stack", no_arg, 'E' },
    { NULL, 0, '\0' }
};
char         *fname_src = NULL;
char         *fname_dst = NULL;
char         *oname_src = NULL;
char         *oname_dst = NULL;
char         *str_flag = NULL;

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
static void
leave(int ret)
{
    if (fname_src)
        HDfree(fname_src);
    if (fname_dst)
        HDfree(fname_dst);
    if (oname_dst)
        HDfree(oname_dst);
    if (oname_src)
        HDfree(oname_src);
    if (str_flag)
        HDfree(str_flag);

    h5tools_close();
    HDexit(ret);
}


/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: Prints a usage message on stderr and then returns.
 *
 * Return: void
 *
 * Programmer: Pedro Vicente Nunes, 7/8/2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (void)
{
    FLUSHSTREAM(rawoutstream);
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "usage: h5copy [OPTIONS] [OBJECTS...]\n");
    PRINTVALSTREAM(rawoutstream, "   OBJECTS\n");
    PRINTVALSTREAM(rawoutstream, "      -i, --input        input file name\n");
    PRINTVALSTREAM(rawoutstream, "      -o, --output       output file name\n");
    PRINTVALSTREAM(rawoutstream, "      -s, --source       source object name\n");
    PRINTVALSTREAM(rawoutstream, "      -d, --destination  destination object name\n");
    PRINTVALSTREAM(rawoutstream, "   OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "      -h, --help         Print a usage message and exit\n");
    PRINTVALSTREAM(rawoutstream, "      -p, --parents      No error if existing, make parent groups as needed\n");
    PRINTVALSTREAM(rawoutstream, "      -v, --verbose      Print information about OBJECTS and OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "      -V, --version      Print version number and exit\n");
    PRINTVALSTREAM(rawoutstream, "      --enable-error-stack\n");
    PRINTVALSTREAM(rawoutstream, "                  Prints messages from the HDF5 error stack as they occur.\n");
    PRINTVALSTREAM(rawoutstream, "      -f, --flag         Flag type\n\n");
    PRINTVALSTREAM(rawoutstream, "      Flag type is one of the following strings:\n\n");
    PRINTVALSTREAM(rawoutstream, "      shallow     Copy only immediate members for groups\n\n");
    PRINTVALSTREAM(rawoutstream, "      soft        Expand soft links into new objects\n\n");
    PRINTVALSTREAM(rawoutstream, "      ext         Expand external links into new objects\n\n");
    PRINTVALSTREAM(rawoutstream, "      ref         Copy references and any referenced objects, i.e., objects\n");
    PRINTVALSTREAM(rawoutstream, "                  that the references point to.\n");
    PRINTVALSTREAM(rawoutstream, "                    Referenced objects are copied in addition to the objects\n");
    PRINTVALSTREAM(rawoutstream, "                  specified on the command line and reference datasets are\n");
    PRINTVALSTREAM(rawoutstream, "                  populated with correct reference values. Copies of referenced\n");
    PRINTVALSTREAM(rawoutstream, "                  datasets outside the copy range specified on the command line\n");
    PRINTVALSTREAM(rawoutstream, "                  will normally have a different name from the original.\n");
    PRINTVALSTREAM(rawoutstream, "                    (Default:Without this option, reference value(s) in any\n");
    PRINTVALSTREAM(rawoutstream, "                  reference datasets are set to NULL and referenced objects are\n");
    PRINTVALSTREAM(rawoutstream, "                  not copied unless they are otherwise within the copy range\n");
    PRINTVALSTREAM(rawoutstream, "                  specified on the command line.)\n\n");
    PRINTVALSTREAM(rawoutstream, "      noattr      Copy object without copying attributes\n\n");
    PRINTVALSTREAM(rawoutstream, "      allflags    Switches all flags from the default to the non-default setting\n\n");
    PRINTVALSTREAM(rawoutstream, "      These flag types correspond to the following API symbols\n\n");
    PRINTVALSTREAM(rawoutstream, "      H5O_COPY_SHALLOW_HIERARCHY_FLAG\n");
    PRINTVALSTREAM(rawoutstream, "      H5O_COPY_EXPAND_SOFT_LINK_FLAG\n");
    PRINTVALSTREAM(rawoutstream, "      H5O_COPY_EXPAND_EXT_LINK_FLAG\n");
    PRINTVALSTREAM(rawoutstream, "      H5O_COPY_EXPAND_REFERENCE_FLAG\n");
    PRINTVALSTREAM(rawoutstream, "      H5O_COPY_WITHOUT_ATTR_FLAG\n");
    PRINTVALSTREAM(rawoutstream, "      H5O_COPY_ALL\n");
}



/*-------------------------------------------------------------------------
 * Function: parse_flag
 *
 * Purpose: read the flag -f STRING
 *
 * STRING is one of the following (API symbol and description)
 *
 * shallow  H5O_COPY_SHALLOW_HIERARCHY_FLAG:  Copy only immediate members for groups
 * soft     H5O_COPY_EXPAND_SOFT_LINK_FLAG:  Expand soft links into new objects
 * ext      H5O_COPY_EXPAND_EXT_LINK_FLAG: Expand external links into new objects
 * ref      H5O_COPY_EXPAND_OBJ_REFERENCE_FLAG: Copy objects that are pointed by references
 * noattr   H5O_COPY_WITHOUT_ATTR_FLAG Copy object without copying attributes
 * allflags Switches all flags from the default to the non-default setting
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *
 * Programmer: Pedro Vicente Nunes, 7/8/2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


static int parse_flag(const char* s_flag, unsigned *flag)
{
    unsigned fla=0;

    if (HDstrcmp(s_flag, "shallow") == 0) {
        fla = H5O_COPY_SHALLOW_HIERARCHY_FLAG;
    }
    else  if (HDstrcmp(s_flag, "soft") == 0) {
        fla = H5O_COPY_EXPAND_SOFT_LINK_FLAG;
    }
    else  if (HDstrcmp(s_flag, "ext") == 0) {
        fla = H5O_COPY_EXPAND_EXT_LINK_FLAG;
    }
    else  if (HDstrcmp(s_flag, "ref") == 0) {
        fla = H5O_COPY_EXPAND_REFERENCE_FLAG;
    }
    else  if (HDstrcmp(s_flag, "noattr") == 0) {
        fla = H5O_COPY_WITHOUT_ATTR_FLAG;
    }
    else  if (HDstrcmp(s_flag, "allflags") == 0) {
        fla = H5O_COPY_ALL;
    }
    else  if (HDstrcmp(s_flag, "nullmsg") == 0) {
        fla = H5O_COPY_PRESERVE_NULL_FLAG;
    }
    else {
        error_msg("Error in input flag\n");
        return -1;
    }

    *flag = (*flag) | fla;

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: main program
 *
 * Programmer: Pedro Vicente Nunes
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main (int argc, const char *argv[])
{
    int            ret_value = 0; /*no need to LEAVE() on ERROR: HERR_INIT(int, SUCCEED) */
    H5E_auto2_t         func;
    H5E_auto2_t         tools_func;
    void               *edata;
    void               *tools_edata;
    hid_t        fid_src = -1;
    hid_t        fid_dst = -1;
    unsigned     flag = 0;
    unsigned     verbose = 0;
    unsigned     parents = 0;
    hid_t        ocpl_id = (-1);          /* Object copy property list */
    hid_t        lcpl_id = (-1);          /* Link creation property list */
    int          opt;
    int          li_ret;
    h5tool_link_info_t linkinfo;

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

    /* init linkinfo struct */
    HDmemset(&linkinfo, 0, sizeof(h5tool_link_info_t));

    /* Check for no command line parameters */
    if(argc == 1) {
        usage();
        leave(EXIT_FAILURE);
    } /* end if */

    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
        case 'd':
            oname_dst = HDstrdup(opt_arg);
            break;

        case 'f':
            /* validate flag */
            if (parse_flag(opt_arg, &flag) < 0) {
                usage();
                leave(EXIT_FAILURE);
            }
            str_flag = HDstrdup(opt_arg);
            break;

        case 'h':
            usage();
            leave(EXIT_SUCCESS);
            break;

        case 'i':
            fname_src = HDstrdup(opt_arg);
            break;

        case 'o':
            fname_dst = HDstrdup(opt_arg);
            break;

        case 'p':
            parents = 1;
            break;

        case 's':
            oname_src = HDstrdup(opt_arg);
            break;

        case 'V':
            print_version(h5tools_getprogname());
            leave(EXIT_SUCCESS);
            break;

        case 'v':
            verbose = 1;
            break;

        case 'E':
            enable_error_stack = 1;
            break;

        default:
            usage();
            leave(EXIT_FAILURE);
        }
    } /* end of while */

/*-------------------------------------------------------------------------
 * check for missing file/object names
 *-------------------------------------------------------------------------*/

    if (fname_src==NULL) {
        error_msg("Input file name missing\n");
        usage();
        leave(EXIT_FAILURE);
    }

    if (fname_dst==NULL) {
        error_msg("Output file name missing\n");
        usage();
        leave(EXIT_FAILURE);
    }

    if (oname_src==NULL) {
        error_msg("Source object name missing\n");
        usage();
        leave(EXIT_FAILURE);
    }

    if (oname_dst==NULL) {
        error_msg("Destination object name missing\n");
        usage();
        leave(EXIT_FAILURE);
    }

    if (enable_error_stack > 0) {
        H5Eset_auto2(H5E_DEFAULT, func, edata);
        H5Eset_auto2(H5tools_ERR_STACK_g, tools_func, tools_edata);
    }

   /*-------------------------------------------------------------------------
    * open output file
    *-------------------------------------------------------------------------*/

    /* Attempt to open an existing HDF5 file first. Need to open the dst file
       before the src file just in case that the dst and src are the same file
     */
    fid_dst = h5tools_fopen(fname_dst, H5F_ACC_RDWR, H5P_DEFAULT, NULL, NULL, 0);

   /*-------------------------------------------------------------------------
    * open input file
    *-------------------------------------------------------------------------*/

    fid_src = h5tools_fopen(fname_src, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, 0);

   /*-------------------------------------------------------------------------
    * test for error in opening input file
    *-------------------------------------------------------------------------*/
    if (fid_src==-1) {
        error_msg("Could not open input file <%s>...Exiting\n", fname_src);
        leave(EXIT_FAILURE);
    }


   /*-------------------------------------------------------------------------
    * create an output file when failed to open it
    *-------------------------------------------------------------------------*/

    /* If we couldn't open an existing file, try creating file */
    /* (use "EXCL" instead of "TRUNC", so we don't blow away existing non-HDF5 file) */
    if(fid_dst < 0)
        fid_dst = H5Fcreate(fname_dst, H5F_ACC_EXCL, H5P_DEFAULT, H5P_DEFAULT);

   /*-------------------------------------------------------------------------
    * test for error in opening output file
    *-------------------------------------------------------------------------*/
    if (fid_dst==-1) {
        error_msg("Could not open output file <%s>...Exiting\n", fname_dst);
        leave(EXIT_FAILURE);
    }

   /*-------------------------------------------------------------------------
    * print some info
    *-------------------------------------------------------------------------*/

    if (verbose) {
        HDprintf("Copying file <%s> and object <%s> to file <%s> and object <%s>\n",
        fname_src, oname_src, fname_dst, oname_dst);
        if (flag) {
            HDprintf("Using %s flag\n", str_flag);
        }
    }


   /*-------------------------------------------------------------------------
    * create property lists for copy
    *-------------------------------------------------------------------------*/

    /* create property to pass copy options */
    if ( (ocpl_id = H5Pcreate(H5P_OBJECT_COPY)) < 0)
        HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Pcreate failed");

    /* set options for object copy */
    if (flag) {
        if ( H5Pset_copy_object(ocpl_id, flag) < 0)
            HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Pset_copy_object failed");
    }

    /* Create link creation property list */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) {
        error_msg("Could not create link creation property list\n");
        HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Pcreate failed");
    } /* end if */

    /* Check for creating intermediate groups */
    if(parents) {
        /* Set the intermediate group creation property */
        if(H5Pset_create_intermediate_group(lcpl_id, 1) < 0) {
            error_msg("Could not set property for creating parent groups\n");
            HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Pset_create_intermediate_group failed");
        } /* end if */

        /* Display some output if requested */
        if(verbose)
            HDprintf("%s: Creating parent groups\n", h5tools_getprogname());
    } /* end if */
    else {
        /* error, if parent groups doesn't already exist in destination file */
        size_t        i, len;

        len = HDstrlen(oname_dst);

        /* check if all the parents groups exist. skip root group */
        for (i = 1; i < len; i++) {
            if ('/'==oname_dst[i]) {
                char         *str_ptr;

                str_ptr = (char *)HDcalloc(i + 1, sizeof(char));
                HDstrncpy(str_ptr, oname_dst, i);
                str_ptr[i]='\0';
                if (H5Lexists(fid_dst, str_ptr, H5P_DEFAULT) <= 0) {
                    error_msg("group <%s> doesn't exist. Use -p to create parent groups.\n", str_ptr);
                    HDfree(str_ptr);
                    HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Lexists failed");
                }
                HDfree(str_ptr);
            }
        }
    }

   /*-------------------------------------------------------------------------
    * do the copy
    *-------------------------------------------------------------------------*/

    if(verbose)
        linkinfo.opt.msg_mode = 1;

    li_ret = H5tools_get_symlink_info(fid_src, oname_src, &linkinfo, 1);
    if (li_ret == 0) {
        /* dangling link */
        if(H5Lcopy(fid_src, oname_src,
                   fid_dst, oname_dst,
                   H5P_DEFAULT, H5P_DEFAULT) < 0)
            HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Lcopy failed");
    }
    else {
        /* valid link */
        if (H5Ocopy(fid_src,          /* Source file or group identifier */
                  oname_src,        /* Name of the source object to be copied */
                  fid_dst,          /* Destination file or group identifier  */
                  oname_dst,        /* Name of the destination object  */
                  ocpl_id,          /* Object copy property list */
                  lcpl_id)<0)       /* Link creation property list */
            HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Ocopy failed");
    }

    /* free link info path */
    if (linkinfo.trg_path)
        HDfree(linkinfo.trg_path);

    /* close propertis */
    if(H5Pclose(ocpl_id)<0)
        HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Pclose failed");
    if(H5Pclose(lcpl_id)<0)
        HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Pclose failed");

    /* close files */
    if (H5Fclose(fid_src)<0)
        HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Fclose failed");
    if (H5Fclose(fid_dst)<0)
        HGOTO_ERROR(EXIT_FAILURE, H5E_tools_min_id_g, "H5Fclose failed");

    leave(EXIT_SUCCESS);

done:
    HDprintf("Error in copy...Exiting\n");

    /* free link info path */
    if (linkinfo.trg_path)
        HDfree(linkinfo.trg_path);

    H5E_BEGIN_TRY {
        H5Pclose(ocpl_id);
        H5Pclose(lcpl_id);
        H5Fclose(fid_src);
        H5Fclose(fid_dst);
    } H5E_END_TRY;

    leave(ret_value);
}

