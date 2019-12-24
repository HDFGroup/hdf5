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

/*
 * Portions of this work are derived from _Obfuscated C and Other Mysteries_,
 * by Don Libes, copyright (c) 1993 by John Wiley & Sons, Inc.
 */

#include "h5tools.h"
#include "h5tools_utils.h"
#include "H5private.h"
#include "h5trav.h"

#ifdef H5_HAVE_ROS3_VFD
#include "H5FDros3.h"
#endif

/* global variables */
unsigned h5tools_nCols = 80;
/* ``get_option'' variables */
int         opt_err = 1;    /*get_option prints errors if this is on */
int         opt_ind = 1;    /*token pointer                          */
const char *opt_arg;        /*flag argument (or value)               */
static int  h5tools_d_status = 0;
static const char  *h5tools_progname = "h5tools";

/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed. This constant sets the limit on the
 * size of that temporary buffer in bytes. For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */
/* Maximum size used in a call to malloc for a dataset */
hsize_t H5TOOLS_MALLOCSIZE = (256 * 1024 * 1024);  /* 256 MB */
/* size of hyperslab buffer when a dataset is bigger than H5TOOLS_MALLOCSIZE */
hsize_t H5TOOLS_BUFSIZE = ( 32 * 1024 * 1024);  /* 32 MB */


/* ``parallel_print'' variables */
unsigned char  g_Parallel = 0;  /*0 for serial, 1 for parallel */
char     outBuff[OUTBUFF_SIZE];
unsigned outBuffOffset;
FILE*    overflow_file = NULL;

/* local functions */
static void init_table(table_t **tbl);
#ifdef H5DUMP_DEBUG
static void dump_table(char* tablename, table_t *table);
#endif  /* H5DUMP_DEBUG */
static void add_obj(table_t *table, haddr_t objno, const char *objname, hbool_t recorded);

/*-------------------------------------------------------------------------
 * Function: parallel_print
 *
 * Purpose:  wrapper for printf for use in parallel mode.
 *-------------------------------------------------------------------------
 */
void
parallel_print(const char* format, ...)
{
    int  bytes_written;
    va_list ap;

    HDva_start(ap, format);

    if(!g_Parallel)
        HDvprintf(format, ap);
    else {
        if(overflow_file == NULL) /*no overflow has occurred yet */ {
            bytes_written = HDvsnprintf(outBuff + outBuffOffset, OUTBUFF_SIZE - outBuffOffset, format, ap);
            HDva_end(ap);
            HDva_start(ap, format);

            if((bytes_written < 0) || ((unsigned)bytes_written >= (OUTBUFF_SIZE - outBuffOffset))) {
                /* Terminate the outbuff at the end of the previous output */
                outBuff[outBuffOffset] = '\0';

                overflow_file = HDtmpfile();
                if(overflow_file == NULL)
                    HDfprintf(rawerrorstream, "warning: could not create overflow file.  Output may be truncated.\n");
                else
                    bytes_written = HDvfprintf(overflow_file, format, ap);
            }
            else
                outBuffOffset += (unsigned)bytes_written;
        }
        else
            bytes_written = HDvfprintf(overflow_file, format, ap);

    }
    HDva_end(ap);
}


/*-------------------------------------------------------------------------
 * Function: error_msg
 *
 * Purpose:  Print a nicely formatted error message to stderr flushing the
 *              stdout stream first.
 *
 * Return:   Nothing
 *-------------------------------------------------------------------------
 */
void
error_msg(const char *fmt, ...)
{
    va_list ap;

    HDva_start(ap, fmt);
    FLUSHSTREAM(rawattrstream);
    FLUSHSTREAM(rawdatastream);
    FLUSHSTREAM(rawoutstream);
    HDfprintf(rawerrorstream, "%s error: ", h5tools_getprogname());
    HDvfprintf(rawerrorstream, fmt, ap);

    HDva_end(ap);
}


/*-------------------------------------------------------------------------
 * Function: warn_msg
 *
 * Purpose:  Print a nicely formatted warning message to stderr flushing
 *              the stdout stream first.
 *
 * Return:   Nothing
 *-------------------------------------------------------------------------
 */
void
warn_msg(const char *fmt, ...)
{
    va_list ap;

    HDva_start(ap, fmt);
    FLUSHSTREAM(rawattrstream);
    FLUSHSTREAM(rawdatastream);
    FLUSHSTREAM(rawoutstream);
    HDfprintf(rawerrorstream, "%s warning: ", h5tools_getprogname());
    HDvfprintf(rawerrorstream, fmt, ap);
    HDva_end(ap);
}

/*-------------------------------------------------------------------------
 * Function: help_ref_msg
 *
 * Purpose:  Print a message to refer help page
 *
 * Return:   Nothing
 *-------------------------------------------------------------------------
 */
void
help_ref_msg(FILE *output)
{
    HDfprintf(output, "Try '-h' or '--help' for more information or ");
    HDfprintf(output, "see the <%s> entry in the 'HDF5 Reference Manual'.\n",h5tools_getprogname());
}


/*-------------------------------------------------------------------------
 * Function: get_option
 *
 * Purpose:  Determine the command-line options a user specified. We can
 *           accept both short and long type command-lines.
 *
 * Return:  Success:    The short valued "name" of the command line
 *              parameter or EOF if there are no more
 *              parameters to process.
 *
 *          Failure:    A question mark.
 *-------------------------------------------------------------------------
 */
int
get_option(int argc, const char **argv, const char *opts, const struct long_options *l_opts)
{
    static int sp = 1;    /* character index in current token */
    int opt_opt = '?';    /* option character passed back to user */

    if (sp == 1) {
        /* check for more flag-like tokens */
        if (opt_ind >= argc || argv[opt_ind][0] != '-' || argv[opt_ind][1] == '\0') {
            return EOF;
        }
        else if (HDstrcmp(argv[opt_ind], "--") == 0) {
            opt_ind++;
            return EOF;
        }
    }

    if (sp == 1 && argv[opt_ind][0] == '-' && argv[opt_ind][1] == '-') {
        /* long command line option */
        const char *arg = &argv[opt_ind][2];
        int i;

        for (i = 0; l_opts && l_opts[i].name; i++) {
            size_t len = HDstrlen(l_opts[i].name);

            if (HDstrncmp(arg, l_opts[i].name, len) == 0) {
                /* we've found a matching long command line flag */
                opt_opt = l_opts[i].shortval;

                if (l_opts[i].has_arg != no_arg) {
                    if (arg[len] == '=') {
                        opt_arg = &arg[len + 1];
                    }
                    else if (l_opts[i].has_arg != optional_arg) {
                        if (opt_ind < (argc - 1))
                            if (argv[opt_ind + 1][0] != '-')
                                opt_arg = argv[++opt_ind];
                    }
                    else if (l_opts[i].has_arg == require_arg) {
                        if (opt_err)
                            HDfprintf(rawerrorstream,
                                    "%s: option required for \"--%s\" flag\n",
                                    argv[0], arg);

                        opt_opt = '?';
                    }
                    else
                        opt_arg = NULL;
                }
                else {
                    if (arg[len] == '=') {
                        if (opt_err)
                            HDfprintf(rawerrorstream,
                                    "%s: no option required for \"%s\" flag\n",
                                    argv[0], arg);

                        opt_opt = '?';
                    }
                    opt_arg = NULL;
                }
                break;
            }
        }

        if (l_opts[i].name == NULL) {
            /* exhausted all of the l_opts we have and still didn't match */
            if (opt_err)
                HDfprintf(rawerrorstream, "%s: unknown option \"%s\"\n", argv[0], arg);

            opt_opt = '?';
        }

        opt_ind++;
        sp = 1;
    }
    else {
        register char *cp;    /* pointer into current token */

        /* short command line option */
        opt_opt = argv[opt_ind][sp];

        if (opt_opt == ':' || (cp = HDstrchr(opts, opt_opt)) == 0) {
            if (opt_err)
                HDfprintf(rawerrorstream, "%s: unknown option \"%c\"\n",
                        argv[0], opt_opt);

            /* if no chars left in this token, move to next token */
            if (argv[opt_ind][++sp] == '\0') {
                opt_ind++;
                sp = 1;
            }
            return '?';
        }

        if (*++cp == ':') {
            /* if a value is expected, get it */
            if (argv[opt_ind][sp + 1] != '\0') {
                /* flag value is rest of current token */
                opt_arg = &argv[opt_ind++][sp + 1];
            }
            else if (++opt_ind >= argc) {
                if (opt_err)
                    HDfprintf(rawerrorstream,
                            "%s: value expected for option \"%c\"\n",
                            argv[0], opt_opt);

                opt_opt = '?';
            }
            else {
                /* flag value is next token */
                opt_arg = argv[opt_ind++];
            }

            sp = 1;
        }
        /* wildcard argument */
        else if (*cp == '*') {
            /* check the next argument */
            opt_ind++;
            /* we do have an extra argument, check if not last */
            if ( (opt_ind+1) < argc ) {
                if ( argv[opt_ind][0] != '-' ) {
                    opt_arg = argv[opt_ind++];
                }
                else {
                    opt_arg = NULL;
                }
            }
            else {
                opt_arg = NULL;
            }
        }
        else {
            /* set up to look at next char in token, next time */
            if (argv[opt_ind][++sp] == '\0') {
                /* no more in current token, so setup next token */
                opt_ind++;
                sp = 1;
            }
            opt_arg = NULL;
        }
    }

    /* return the current flag character found */
    return opt_opt;
}


/*****************************************************************************
 *
 * Function: parse_tuple()
 *
 * Purpose:
 *
 *     Create array of pointers to strings, identified as elements in a tuple
 *     of arbitrary length separated by provided character.
 *     ("tuple" because "nple" looks strange)
 *
 *     * Receives pointer to start of tuple sequence string, '('.
 *     * Attempts to separate elements by token-character `sep`.
 *         * If the separator character is preceded by a backslash '\',
 *           the backslash is deleted and the separator is included in the
 *           element string as any other character.
 *     * To end an element with a backslash, escape the backslash, e.g.
 *       "(myelem\\,otherelem) -> {"myelem\", "otherelem"}
 *     * In all other cases, a backslash appearing not as part of "\\" or
 *       "\<sep>" digraph will be included berbatim.
 *     * Last two characters in the string MUST be ")\0".
 *
 *     * Generates a copy of the input string `start`, (src..")\0"), replacing
 *       separators and close-paren with null charaters.
 *         * This string is allocated at runtime and should be freed when done.
 *     * Generates array of char pointers, and directs start of each element
 *       (each pointer) into this copy.
 *         * Each tuple element points to the start of its string (substring)
 *           and ends with a null terminator.
 *         * This array is allocated at runtime and should be freed when done.
 *     * Reallocates and expands elements array during parsing.
 *         * Initially allocated for 2 (plus one null entry), and grows by
 *           powers of 2.
 *     * The final 'slot' in the element array (elements[nelements], e.g.)
 *       always points to NULL.
 *     * The number of elements found and stored are passed out through pointer
 *       to unsigned, `nelems`.
 *
 * Return:
 *
 *     FAIL    If malformed--does not look like a tuple "(...)"
 *             or major error was encountered while parsing.
 *     or
 *     SUCCEED String looks properly formed "(...)" and no major errors.
 *
 *             Stores number of elements through pointer `nelems`.
 *             Stores list of pointers to char (first char in each element
 *                 string) through pointer `ptrs_out`.
 *                 NOTE: `ptrs_out[nelems] == NULL` should be true.
 *                 NOTE: list is malloc'd by function, and should be freed
 *                       when done.
 *             Stores "source string" for element pointers through `cpy_out`.
 *                 NOTE: Each element substring is null-terminated.
 *                 NOTE: There may be extra characters after the last element
 *                           (past its null terminator), but is guaranteed to
 *                           be null-terminated.
 *                 NOTE: `cpy_out` string is malloc'd by function,
 *                       and should be freed when done.
 *
 * Programmer: Jacob Smith
 *             2017-11-10
 *
 * Changes: None.
 *
 *****************************************************************************
 */
herr_t
parse_tuple(const char   *start,
           int           sep,
           char        **cpy_out,
           unsigned     *nelems,
           char       ***ptrs_out)
{
    char      *elem_ptr    = NULL;
    char      *dest_ptr    = NULL;
    unsigned   elems_count = 0;
    char     **elems       = NULL; /* more like *elems[], but complier... */
    char     **elems_re    = NULL; /* temporary pointer, for realloc */
    char      *cpy         = NULL;
    herr_t     ret_value   = SUCCEED;
    unsigned   init_slots  = 2;



    /*****************
     * SANITY-CHECKS *
     *****************/

    /* must start with "("
     */
    if (start[0] != '(') {
        ret_value = FAIL;
        goto done;
    }

    /* must end with ")"
     */
    while (start[elems_count] != '\0') {
        elems_count++;
    }
    if (start[elems_count - 1] != ')') {
        ret_value = FAIL;
        goto done;
    }

    elems_count = 0;



    /***********
     * PREPARE *
     ***********/

    /* create list
     */
    elems = (char **)HDmalloc(sizeof(char *) * (init_slots + 1));
    if (elems == NULL) { ret_value = FAIL; goto done; } /* CANTALLOC */

    /* create destination string
     */
    start++; /* advance past opening paren '(' */
    cpy = (char *)HDmalloc(sizeof(char) * (HDstrlen(start))); /* no +1; less '(' */
    if (cpy == NULL) { ret_value = FAIL; goto done; } /* CANTALLOC */

    /* set pointers
     */
    dest_ptr = cpy; /* start writing copy here */
    elem_ptr = cpy; /* first element starts here */
    elems[elems_count++] = elem_ptr; /* set first element pointer into list */



    /*********
     * PARSE *
     *********/

    while (*start != '\0') {
        /* For each character in the source string...
         */
        if (*start == '\\') {
            /* Possibly an escape digraph.
             */
            if ((*(start + 1) == '\\') ||
                (*(start + 1) == sep) )
            {
                /* Valid escape digraph of "\\" or "\<sep>".
                 */
                start++; /* advance past escape char '\' */
                *(dest_ptr++) = *(start++); /* Copy subsequent char  */
                                            /* and advance pointers. */
            } else {
               /* Not an accepted escape digraph.
                * Copy backslash character.
                */
                *(dest_ptr++) = *(start++);
            }
        } else if (*start == sep) {
            /* Non-escaped separator.
             * Terminate elements substring in copy, record element, advance.
             * Expand elements list if appropriate.
             */
            *(dest_ptr++) = 0; /* Null-terminate elem substring in copy */
                               /* and advance pointer.                  */
            start++; /* Advance src pointer past separator. */
            elem_ptr = dest_ptr; /* Element pointer points to start of first */
                                 /* character after null sep in copy.        */
            elems[elems_count++] = elem_ptr; /* Set elem pointer in list */
                                             /* and increment count.     */

            /* Expand elements list, if necessary.
             */
            if (elems_count == init_slots) {
                init_slots *= 2;
                elems_re = (char **)realloc(elems, sizeof(char *) * \
                                                   (init_slots + 1));
                if (elems_re == NULL) {
                    /* CANTREALLOC */
                    ret_value = FAIL;
                    goto done;
                }
                elems = elems_re;
            }
        } else if (*start == ')' && *(start + 1) == '\0') {
            /* Found terminal, non-escaped close-paren. Last element.
             * Write null terminator to copy.
             * Advance source pointer to gently break from loop.
             * Requred to prevent ")" from always being added to last element.
             */
            start++;
        } else {
            /* Copy character into destination. Advance pointers.
             */
            *(dest_ptr++) = *(start++);
        }
    }
    *dest_ptr = '\0'; /* Null-terminate destination string. */
    elems[elems_count] = NULL; /* Null-terminate elements list. */



    /********************
     * PASS BACK VALUES *
     ********************/

    *ptrs_out = elems;
    *nelems   = elems_count;
    *cpy_out  = cpy;

done:
    if (ret_value == FAIL) {
        /* CLEANUP */
        if (cpy)   free(cpy);
        if (elems) free(elems);
    }

    return ret_value;

} /* parse_tuple */





/*-------------------------------------------------------------------------
 * Function: indentation
 *
 * Purpose:  Print spaces for indentation
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
void
indentation(unsigned x)
{
    if (x < h5tools_nCols) {
        while (x-- > 0)
            PRINTVALSTREAM(rawoutstream, " ");
    }
    else {
        HDfprintf(rawerrorstream, "error: the indentation exceeds the number of cols.\n");
        HDexit(1);
    }
}


/*-------------------------------------------------------------------------
 * Function: print_version
 *
 * Purpose:  Print the program name and the version information which is
 *           defined the same as the HDF5 library version.
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
void
print_version(const char *progname)
{
    PRINTSTREAM(rawoutstream, "%s: Version %u.%u.%u%s%s\n",
           progname, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE,
           ((const char *)H5_VERS_SUBRELEASE)[0] ? "-" : "", H5_VERS_SUBRELEASE);
}


/*-------------------------------------------------------------------------
 * Function:    init_table
 *
 * Purpose:     allocate and initialize tables for shared groups, datasets,
 *              and committed types
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
init_table(table_t **tbl)
{
    table_t *table = (table_t *)HDmalloc(sizeof(table_t));

    table->size = 20;
    table->nobjs = 0;
    table->objs = (obj_t *)HDmalloc(table->size * sizeof(obj_t));

    *tbl = table;
}


/*-------------------------------------------------------------------------
 * Function:    free_table
 *
 * Purpose:     free tables for shared groups, datasets,
 *              and committed types
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
free_table(table_t *table)
{
    unsigned u;         /* Local index value */

    /* Free the names for the objects in the table */
    for(u = 0; u < table->nobjs; u++)
        if(table->objs[u].objname)
            HDfree(table->objs[u].objname);

    HDfree(table->objs);
}

#ifdef H5DUMP_DEBUG

/*-------------------------------------------------------------------------
 * Function:    dump_table
 *
 * Purpose:     display the contents of tables for debugging purposes
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
dump_table(char* tablename, table_t *table)
{
    unsigned u;

    PRINTSTREAM(rawoutstream,"%s: # of entries = %d\n", tablename,table->nobjs);
    for (u = 0; u < table->nobjs; u++)
        PRINTSTREAM(rawoutstream,"%a %s %d %d\n", table->objs[u].objno,
           table->objs[u].objname,
           table->objs[u].displayed, table->objs[u].recorded);
}


/*-------------------------------------------------------------------------
 * Function:    dump_tables
 *
 * Purpose:     display the contents of tables for debugging purposes
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
dump_tables(find_objs_t *info)
{
    dump_table("group_table", info->group_table);
    dump_table("dset_table", info->dset_table);
    dump_table("type_table", info->type_table);
}
#endif  /* H5DUMP_DEBUG */


/*-------------------------------------------------------------------------
 * Function:    search_obj
 *
 * Purpose:     search the object specified by objno in the table
 *
 * Return:      Success:    an integer, the location of the object
 *
 *              Failure:    FAIL   if object is not found
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE obj_t *
search_obj(table_t *table, haddr_t objno)
{
    unsigned u;

    for(u = 0; u < table->nobjs; u++)
        if(table->objs[u].objno == objno)
            return &(table->objs[u]);

    return NULL;
}


/*-------------------------------------------------------------------------
 * Function:    find_objs_cb
 *
 * Purpose:     Callback to find objects, committed types and store them in tables
 *
 * Return:      Success:    SUCCEED
 *
 *              Failure:    FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
find_objs_cb(const char *name, const H5O_info_t *oinfo, const char *already_seen, void *op_data)
{
    find_objs_t *info = (find_objs_t*)op_data;
    herr_t ret_value = 0;

    switch(oinfo->type) {
        case H5O_TYPE_GROUP:
            if(NULL == already_seen)
                add_obj(info->group_table, oinfo->addr, name, TRUE);
            break;

        case H5O_TYPE_DATASET:
            if(NULL == already_seen) {
                hid_t dset = -1;

                /* Add the dataset to the list of objects */
                add_obj(info->dset_table, oinfo->addr, name, TRUE);

                /* Check for a dataset that uses a named datatype */
                if((dset = H5Dopen2(info->fid, name, H5P_DEFAULT)) >= 0) {
                    hid_t type = H5Dget_type(dset);

                    if(H5Tcommitted(type) > 0) {
                        H5O_info_t type_oinfo;

                        H5Oget_info2(type, &type_oinfo, H5O_INFO_BASIC);
                        if(search_obj(info->type_table, type_oinfo.addr) == NULL)
                            add_obj(info->type_table, type_oinfo.addr, name, FALSE);
                    } /* end if */

                    H5Tclose(type);
                    H5Dclose(dset);
                } /* end if */
                else
                    ret_value = FAIL;
            } /* end if */
            break;

        case H5O_TYPE_NAMED_DATATYPE:
            if(NULL == already_seen) {
                obj_t *found_obj;

                if((found_obj = search_obj(info->type_table, oinfo->addr)) == NULL)
                    add_obj(info->type_table, oinfo->addr, name, TRUE);
                else {
                    /* Use latest version of name */
                    HDfree(found_obj->objname);
                    found_obj->objname = HDstrdup(name);

                    /* Mark named datatype as having valid name */
                    found_obj->recorded = TRUE;
                } /* end else */
            } /* end if */
            break;

        case H5O_TYPE_UNKNOWN:
        case H5O_TYPE_NTYPES:
        default:
            break;
    } /* end switch */

    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:    init_objs
 *
 * Purpose:     Initialize tables for groups, datasets & named datatypes
 *
 * Return:      Success:    SUCCEED
 *
 *              Failure:    FAIL
 *-------------------------------------------------------------------------
 */
herr_t
init_objs(hid_t fid, find_objs_t *info, table_t **group_table,
    table_t **dset_table, table_t **type_table)
{
    herr_t ret_value = SUCCEED;

    /* Initialize the tables */
    init_table(group_table);
    init_table(dset_table);
    init_table(type_table);

    /* Init the find_objs_t */
    info->fid = fid;
    info->group_table = *group_table;
    info->type_table = *type_table;
    info->dset_table = *dset_table;

    /* Find all shared objects */
    if((ret_value = h5trav_visit(fid, "/", TRUE, TRUE, find_objs_cb, NULL, info, H5O_INFO_BASIC)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "finding shared objects failed")

done:
    /* Release resources */
    if(ret_value < 0) {
        free_table(*group_table);
        info->group_table = NULL;
        free_table(*type_table);
        info->type_table = NULL;
        free_table(*dset_table);
        info->dset_table = NULL;
    }
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:    add_obj
 *
 * Purpose:     add a shared object to the table
 *              realloc the table if necessary
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
add_obj(table_t *table, haddr_t objno, const char *objname, hbool_t record)
{
    size_t u;

    /* See if we need to make table larger */
    if(table->nobjs == table->size) {
        table->size *= 2;
        table->objs = (struct obj_t *)HDrealloc(table->objs, table->size * sizeof(table->objs[0]));
    } /* end if */

    /* Increment number of objects in table */
    u = table->nobjs++;

    /* Set information about object */
    table->objs[u].objno = objno;
    table->objs[u].objname = HDstrdup(objname);
    table->objs[u].recorded = record;
    table->objs[u].displayed = 0;
}


#ifndef H5_HAVE_TMPFILE
/*-------------------------------------------------------------------------
 * Function:    tmpfile
 *
 * Purpose:     provide tmpfile() function when it is not supported by the
 *              system.  Always return NULL for now.
 *
 * Return:      a stream description when succeeds.
 *              NULL if fails.
 *-------------------------------------------------------------------------
 */
FILE *
tmpfile(void)
{
    return NULL;
}

#endif

/*-------------------------------------------------------------------------
 * Function: H5tools_get_symlink_info
 *
 * Purpose: Get symbolic link (soft, external) info and its target object type
            (dataset, group, named datatype) and path, if exist
 *
 * Patameters:
 *  - [IN]  fileid : link file id
 *  - [IN]  linkpath : link path
 *  - [OUT] link_info: returning target object info (h5tool_link_info_t)
 *
 * Return:
 *   2 : given pathname is object
 *   1 : Succeed to get link info.
 *   0 : Detected as a dangling link
 *  -1 : H5 API failed.
 *
 * NOTE:
 *  link_info->trg_path must be freed out of this function
 *-------------------------------------------------------------------------*/
int
H5tools_get_symlink_info(hid_t file_id, const char * linkpath, h5tool_link_info_t *link_info, hbool_t get_obj_type)
{
    htri_t l_ret;
    H5O_info_t trg_oinfo;
    hid_t fapl = H5P_DEFAULT;
    hid_t lapl = H5P_DEFAULT;
    int   ret_value = -1; /* init to fail */

    /* init */
    link_info->trg_type = H5O_TYPE_UNKNOWN;

    /* if path is root, return group type */
    if(!HDstrcmp(linkpath,"/")) {
        link_info->trg_type = H5O_TYPE_GROUP;
        HGOTO_DONE(2);
    }

    /* check if link itself exist */
    if(H5Lexists(file_id, linkpath, H5P_DEFAULT) <= 0) {
        if(link_info->opt.msg_mode == 1)
            parallel_print("Warning: link <%s> doesn't exist \n",linkpath);
        HGOTO_DONE(FAIL);
    } /* end if */

    /* get info from link */
    if(H5Lget_info(file_id, linkpath, &(link_info->linfo), H5P_DEFAULT) < 0) {
        if(link_info->opt.msg_mode == 1)
            parallel_print("Warning: unable to get link info from <%s>\n",linkpath);
        HGOTO_DONE(FAIL);
    } /* end if */

    /* given path is hard link (object) */
    if(link_info->linfo.type == H5L_TYPE_HARD)
        HGOTO_DONE(2);

    /* trg_path must be freed out of this function when finished using */
    if((link_info->trg_path = (char*)HDcalloc(link_info->linfo.u.val_size, sizeof(char))) == NULL) {
        if(link_info->opt.msg_mode == 1)
            parallel_print("Warning: unable to allocate buffer for <%s>\n",linkpath);
        HGOTO_DONE(FAIL);
    } /* end if */

    /* get link value */
    if(H5Lget_val(file_id, linkpath, (void *)link_info->trg_path, link_info->linfo.u.val_size, H5P_DEFAULT) < 0) {
        if(link_info->opt.msg_mode == 1)
            parallel_print("Warning: unable to get link value from <%s>\n",linkpath);
        HGOTO_DONE(FAIL);
    } /* end if */

    /*-----------------------------------------------------
     * if link type is external link use different lapl to
     * follow object in other file
     */
    if(link_info->linfo.type == H5L_TYPE_EXTERNAL) {
        if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            HGOTO_DONE(FAIL);
        if(H5Pset_fapl_sec2(fapl) < 0)
            HGOTO_DONE(FAIL);
        if((lapl = H5Pcreate(H5P_LINK_ACCESS)) < 0)
            HGOTO_DONE(FAIL);
        if(H5Pset_elink_fapl(lapl, fapl) < 0)
            HGOTO_DONE(FAIL);
    } /* end if */

    /* Check for retrieving object info */
    if(get_obj_type) {
        /*--------------------------------------------------------------
         * if link's target object exist, get type
         */
         /* check if target object exist */
        l_ret = H5Oexists_by_name(file_id, linkpath, lapl);

        /* detect dangling link */
        if(l_ret == FALSE) {
            HGOTO_DONE(0);
        }
        else if(l_ret < 0) {       /* function failed */
            HGOTO_DONE(FAIL);
        }

        /* get target object info */
        if(H5Oget_info_by_name2(file_id, linkpath, &trg_oinfo, H5O_INFO_BASIC, lapl) < 0) {
            if(link_info->opt.msg_mode == 1)
                parallel_print("Warning: unable to get object information for <%s>\n", linkpath);
            HGOTO_DONE(FAIL);
        } /* end if */

        /* check unknown type */
        if(trg_oinfo.type < H5O_TYPE_GROUP || trg_oinfo.type >=H5O_TYPE_NTYPES) {
            if(link_info->opt.msg_mode == 1)
                parallel_print("Warning: target object of <%s> is unknown type\n", linkpath);
            HGOTO_DONE(FAIL);
        }  /* end if */

        /* set target obj type to return */
        link_info->trg_type = trg_oinfo.type;
        link_info->objno = trg_oinfo.addr;
        link_info->fileno = trg_oinfo.fileno;
    } /* end if */
    else
        link_info->trg_type = H5O_TYPE_UNKNOWN;

    /* succeed */
    ret_value = 1;

done:
    if(fapl != H5P_DEFAULT)
        H5Pclose(fapl);
    if(lapl != H5P_DEFAULT)
        H5Pclose(lapl);

    return ret_value;
} /* end H5tools_get_symlink_info() */

/*-------------------------------------------------------------------------
 * Audience:    Public
 *
 * Purpose:     Initialize the name and operation status of the H5 Tools library
 *
 * Description:
 *      These are utility functions to set/get the program name and operation status.
 *-------------------------------------------------------------------------
 */
void
h5tools_setprogname(const char *Progname)
{
    h5tools_progname = Progname;
}

void
h5tools_setstatus(int D_status)
{
    h5tools_d_status = D_status;
}

H5_ATTR_PURE const char *
h5tools_getprogname(void)
{
   return h5tools_progname;
}

H5_ATTR_PURE int
h5tools_getstatus(void)
{
   return h5tools_d_status;
}

/*-----------------------------------------------------------
 * PURPOSE :
 * if environment variable H5TOOLS_BUFSIZE is set,
 * update H5TOOLS_BUFSIZE and H5TOOLS_MALLOCSIZE from the env
 * This can be called from each tools main() as part of initial act.
 * Note: this is more of debugging purpose for now.
 */
int
h5tools_getenv_update_hyperslab_bufsize(void)
{
    const char *env_str = NULL;
    long hyperslab_bufsize_mb;
    int ret_value = 1;

    /* check if environment variable is set for the hyperslab buffer size */
    if (NULL != (env_str = HDgetenv ("H5TOOLS_BUFSIZE"))) {
        errno = 0;
        hyperslab_bufsize_mb = HDstrtol(env_str, (char**)NULL, 10);
        if (errno != 0 || hyperslab_bufsize_mb <= 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "hyperslab buffer size failed");

        /* convert MB to byte */
        H5TOOLS_BUFSIZE = (hsize_t)hyperslab_bufsize_mb * 1024 * 1024;

        H5TOOLS_MALLOCSIZE = MAX(H5TOOLS_BUFSIZE, H5TOOLS_MALLOCSIZE);
    }

done:
    return ret_value;
}


/*----------------------------------------------------------------------------
 *
 * Function: h5tools_populate_ros3_fapl()
 *
 * Purpose:
 *
 *     Set the values of a ROS3 fapl configuration object.
 *
 *     If the values pointer is NULL, sets fapl target `fa` to a default
 *     (valid, current-version, non-authenticating) fapl config.
 *
 *     If `values` pointer is _not_ NULL, expects `values` to contain at least
 *     three non-null pointers to null-terminated strings, corresponding to:
 *     {   aws_region,
 *         secret_id,
 *         secret_key,
 *     }
 *     If all three strings are empty (""), the default fapl will be default.
 *     Both aws_region and secret_id values must be both empty or both
 *         populated. If
 *     Only secret_key is allowed to be empty (the empty string, "").
 *     All values are checked against overflow as defined in the ros3 vfd
 *     header file; if a value overruns the permitted space, FAIL is returned
 *     and the function aborts without resetting the fapl to values initially
 *     present.
 *
 * Return:
 *
 *     0 (failure) if...
 *         * Read-Only S3 VFD is not enabled.
 *         * NULL fapl pointer: (NULL, {...} )
 *         * Warning: In all cases below, fapl will be set as "default"
 *                    before error occurs.
 *         * NULL value strings: (&fa, {NULL?, NULL? NULL?, ...})
 *         * Incomplete fapl info:
 *             * empty region, non-empty id, key either way
 *                 * (&fa, {"", "...", "?"})
 *             * empty id, non-empty region, key either way
 *                 * (&fa, {"...", "", "?"})
 *             * "non-empty key and either id or region empty
 *                 * (&fa, {"",    "",    "...")
 *                 * (&fa, {"",    "...", "...")
 *                 * (&fa, {"...", "",    "...")
 *             * Any string would overflow allowed space in fapl definition.
 *     or
 *     1 (success)
 *         * Sets components in fapl_t pointer, copying strings as appropriate.
 *         * "Default" fapl (valid version, authenticate->False, empty strings)
 *             * `values` pointer is NULL
 *                 * (&fa, NULL)
 *             * first three strings in `values` are empty ("")
 *                 * (&fa, {"", "", "", ...}
 *         * Authenticating fapl
 *             * region, id, and optional key provided
 *                 * (&fa, {"...", "...", ""})
 *                 * (&fa, {"...", "...", "..."})
 *
 * Programmer: Jacob Smith
 *             2017-11-13
 *
 *----------------------------------------------------------------------------
 */
#ifdef H5_HAVE_ROS3_VFD
int
h5tools_populate_ros3_fapl(H5FD_ros3_fapl_t  *fa,
                           const char       **values)
{
    int show_progress = 0; /* set to 1 for debugging */
    int ret_value     = 1; /* 1 for success, 0 for failure           */
                           /* e.g.? if (!populate()) { then failed } */

    if (show_progress) {
        HDprintf("called h5tools_populate_ros3_fapl\n");
    }

    if (fa == NULL) {
        if (show_progress) {
            HDprintf("  ERROR: null pointer to fapl_t\n");
        }
        ret_value = 0;
        goto done;
    }

    if (show_progress) {
        HDprintf("  preset fapl with default values\n");
    }
    fa->version       = H5FD_CURR_ROS3_FAPL_T_VERSION;
    fa->authenticate  = FALSE;
    *(fa->aws_region) = '\0';
    *(fa->secret_id)  = '\0';
    *(fa->secret_key) = '\0';

    /* sanity-check supplied values
     */
    if (values != NULL) {
        if (values[0] == NULL) {
            if (show_progress) {
                HDprintf("  ERROR: aws_region value cannot be NULL\n");
            }
            ret_value = 0;
            goto done;
        }
        if (values[1] == NULL) {
            if (show_progress) {
                HDprintf("  ERROR: secret_id value cannot be NULL\n");
            }
            ret_value = 0;
            goto done;
        }
        if (values[2] == NULL) {
            if (show_progress) {
                HDprintf("  ERROR: secret_key value cannot be NULL\n");
            }
            ret_value = 0;
            goto done;
        }

        /* if region and ID are supplied (key optional), write to fapl...
         * fail if value would overflow
         */
        if (*values[0] != '\0' &&
            *values[1] != '\0')
        {
            if (HDstrlen(values[0]) > H5FD_ROS3_MAX_REGION_LEN) {
                if (show_progress) {
                    HDprintf("  ERROR: aws_region value too long\n");
                }
                ret_value = 0;
                goto done;
            }
            HDmemcpy(fa->aws_region,                     values[0],
                     (HDstrlen(values[0]) + 1));
            if (show_progress) {
                HDprintf("  aws_region set\n");
            }


            if (HDstrlen(values[1]) > H5FD_ROS3_MAX_SECRET_ID_LEN) {
                if (show_progress) {
                    HDprintf("  ERROR: secret_id value too long\n");
                }
                ret_value = 0;
                goto done;
            }
            HDmemcpy(fa->secret_id,
                     values[1],
                     (HDstrlen(values[1]) + 1));
            if (show_progress) {
                HDprintf("  secret_id set\n");
            }

            if (HDstrlen(values[2]) > H5FD_ROS3_MAX_SECRET_KEY_LEN) {
                if (show_progress) {
                    HDprintf("  ERROR: secret_key value too long\n");
                }
                ret_value = 0;
                goto done;
            }
            HDmemcpy(fa->secret_key,
                     values[2],
                     (HDstrlen(values[2]) + 1));
            if (show_progress) {
                HDprintf("  secret_key set\n");
            }

            fa->authenticate = TRUE;
            if (show_progress) {
                HDprintf("  set to authenticate\n");
            }

        } else if (*values[0] != '\0' ||
                   *values[1] != '\0' ||
                   *values[2] != '\0')
        {
            if (show_progress) {
                HDprintf(
                    "  ERROR: invalid assortment of empty/non-empty values\n"
                );
            }
            ret_value = 0;
            goto done;
        }
    } /* values != NULL */

done:
    return ret_value;

} /* h5tools_populate_ros3_fapl */
#endif /* H5_HAVE_ROS3_VFD */


/*-----------------------------------------------------------------------------
 *
 * Function: h5tools_set_configured_fapl
 *
 * Purpose: prepare fapl_id with the given property list, according to
 *          VFD prototype.
 *
 * Return: 0 on failure, 1 on success
 *
 * Programmer: Jacob Smith
 *             2018-05-21
 *
 * Changes: None.
 *
 *-----------------------------------------------------------------------------
 */
int
h5tools_set_configured_fapl(hid_t      fapl_id,
                           const char  vfd_name[],
                           void       *fapl_t_ptr)
{
    int ret_value = 1;

    if (fapl_id < 0) {
        return 0;
    }

    if (!strcmp("", vfd_name)) {
        goto done;

#ifdef H5_HAVE_ROS3_VFD
    } else if (!strcmp("ros3", vfd_name)) {
        if ((fapl_id == H5P_DEFAULT) ||
            (fapl_t_ptr == NULL) ||
            (FAIL == H5Pset_fapl_ros3(
                fapl_id,
                (H5FD_ros3_fapl_t *)fapl_t_ptr)))
        {
            ret_value = 0;
            goto done;
        }
#endif /* H5_HAVE_ROS3_VFD */

#ifdef H5_HAVE_LIBHDFS
    } else if (!strcmp("hdfs", vfd_name)) {
        if ((fapl_id == H5P_DEFAULT) ||
            (fapl_t_ptr == NULL) ||
            (FAIL == H5Pset_fapl_hdfs(
                fapl_id,
                (H5FD_hdfs_fapl_t *)fapl_t_ptr)))
        {
            ret_value = 0;
            goto done;
        }
#endif /* H5_HAVE_LIBHDFS */

    } else {
        ret_value = 0; /* unrecognized fapl type "name" */
    }

done:
    return ret_value;

} /* h5tools_set_configured_fapl() */

