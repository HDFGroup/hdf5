/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
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

/* Global variables */
unsigned           h5tools_nCols    = 80;
static int         h5tools_d_status = 0;
static const char *h5tools_progname = "h5tools";

/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed. This constant sets the limit on the
 * size of that temporary buffer in bytes. For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */
/* Maximum size used in a call to malloc for a dataset */
hsize_t H5TOOLS_MALLOCSIZE = (256 * 1024 * 1024); /* 256 MB */
/* size of hyperslab buffer when a dataset is bigger than H5TOOLS_MALLOCSIZE */
hsize_t H5TOOLS_BUFSIZE = (32 * 1024 * 1024); /* 32 MB */

/* ``parallel_print'' variables */
unsigned char g_Parallel = 0; /*0 for serial, 1 for parallel */
char          outBuff[OUTBUFF_SIZE];
unsigned      outBuffOffset;
FILE         *overflow_file = NULL;

/* local functions */
static void init_table(hid_t fid, table_t **tbl);
#ifdef H5DUMP_DEBUG
static void dump_table(hid_t fid, char *tablename, table_t *table);
#endif /* H5DUMP_DEBUG */
static void add_obj(table_t *table, const H5O_token_t *obj_token, const char *objname, bool recorded);

/*-------------------------------------------------------------------------
 * Function: parallel_print
 *
 * Purpose:  wrapper for printf for use in parallel mode.
 *-------------------------------------------------------------------------
 */
void
parallel_print(const char *format, ...)
{
    int     bytes_written;
    va_list ap;

    va_start(ap, format);

    if (!g_Parallel)
        vprintf(format, ap);
    else {
        if (overflow_file == NULL) /*no overflow has occurred yet */ {
            bytes_written = vsnprintf(outBuff + outBuffOffset, OUTBUFF_SIZE - outBuffOffset, format, ap);
            va_end(ap);
            va_start(ap, format);

            if ((bytes_written < 0) || ((unsigned)bytes_written >= (OUTBUFF_SIZE - outBuffOffset))) {
                /* Terminate the outbuff at the end of the previous output */
                outBuff[outBuffOffset] = '\0';

                overflow_file = HDtmpfile();
                if (overflow_file == NULL)
                    fprintf(rawerrorstream,
                            "warning: could not create overflow file.  Output may be truncated.\n");
                else
                    bytes_written = vfprintf(overflow_file, format, ap);
            }
            else
                outBuffOffset += (unsigned)bytes_written;
        }
        else
            bytes_written = vfprintf(overflow_file, format, ap);
    }
    va_end(ap);
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

    va_start(ap, fmt);
    FLUSHSTREAM(rawattrstream);
    FLUSHSTREAM(rawdatastream);
    FLUSHSTREAM(rawoutstream);
    fprintf(rawerrorstream, "%s error: ", h5tools_getprogname());
    vfprintf(rawerrorstream, fmt, ap);

    va_end(ap);
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

    va_start(ap, fmt);
    FLUSHSTREAM(rawattrstream);
    FLUSHSTREAM(rawdatastream);
    FLUSHSTREAM(rawoutstream);
    fprintf(rawerrorstream, "%s warning: ", h5tools_getprogname());
    vfprintf(rawerrorstream, fmt, ap);
    va_end(ap);
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
    fprintf(output, "Try '-h' or '--help' for more information or ");
    fprintf(output, "see the <%s> entry in the 'HDF5 Reference Manual'.\n", h5tools_getprogname());
}

/*-------------------------------------------------------------------------
 * Function:    parse_hsize_list
 *
 * Purpose:     Parse a list of comma or space separated integers and return
 *              them in a list. The string being passed into this function
 *              should be at the start of the list you want to parse. You are
 *              responsible for freeing the array returned from here.
 *
 *              Lists in the so-called "terse" syntax are separated by
 *              semicolons (;). The lists themselves can be separated by
 *              either commas (,) or white spaces.
 *
 * Return:      <none>
 *-------------------------------------------------------------------------
 */
void
parse_hsize_list(const char *h_list, subset_d *d)
{
    hsize_t     *p_list;
    const char  *ptr;
    unsigned int size_count = 0;
    unsigned int i          = 0;
    unsigned int last_digit = 0;

    if (!h_list || !*h_list || *h_list == ';')
        return;

    H5TOOLS_START_DEBUG(" - h_list:%s", h_list);
    /* count how many integers do we have */
    for (ptr = h_list; ptr && *ptr && *ptr != ';' && *ptr != ']'; ptr++)
        if (isdigit(*ptr)) {
            if (!last_digit)
                /* the last read character wasn't a digit */
                size_count++;

            last_digit = 1;
        }
        else
            last_digit = 0;

    if (size_count == 0) {
        /* there aren't any integers to read */
        H5TOOLS_ENDDEBUG("No integers to read");
        return;
    }
    H5TOOLS_DEBUG("Number integers to read=%ld", size_count);

    /* allocate an array for the integers in the list */
    if ((p_list = (hsize_t *)calloc(size_count, sizeof(hsize_t))) == NULL)
        H5TOOLS_INFO("Unable to allocate space for subset data");

    for (ptr = h_list; i < size_count && ptr && *ptr && *ptr != ';' && *ptr != ']'; ptr++)
        if (isdigit(*ptr)) {
            /* we should have an integer now */
            p_list[i++] = (hsize_t)strtoull(ptr, NULL, 0);

            while (isdigit(*ptr))
                /* scroll to end of integer */
                ptr++;
        }
    d->data = p_list;
    d->len  = size_count;
    H5TOOLS_ENDDEBUG(" ");
}

/*-------------------------------------------------------------------------
 * Function:    parse_subset_params
 *
 * Purpose:     Parse the so-called "terse" syntax for specifying subsetting parameters.
 *
 * Return:      Success:    struct subset_t object
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
struct subset_t *
parse_subset_params(const char *dset)
{
    struct subset_t *s = NULL;
    char            *brace;
    const char      *q_dset;

    H5TOOLS_START_DEBUG(" - dset:%s", dset);
    /* if dset name is quoted wait till after second quote to look for subset brackets */
    if (*dset == '"')
        q_dset = strchr(dset, '"');
    else
        q_dset = dset;
    if ((brace = strrchr(q_dset, '[')) != NULL) {
        *brace++ = '\0';

        s = (struct subset_t *)calloc(1, sizeof(struct subset_t));
        parse_hsize_list(brace, &s->start);

        while (*brace && *brace != ';')
            brace++;

        if (*brace)
            brace++;

        parse_hsize_list(brace, &s->stride);

        while (*brace && *brace != ';')
            brace++;

        if (*brace)
            brace++;

        parse_hsize_list(brace, &s->count);

        while (*brace && *brace != ';')
            brace++;

        if (*brace)
            brace++;

        parse_hsize_list(brace, &s->block);
    }
    H5TOOLS_ENDDEBUG(" ");

    return s;
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
 *       separators and close-paren with null characters.
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
 *****************************************************************************
 */
herr_t
parse_tuple(const char *start, int sep, char **cpy_out, unsigned *nelems, char ***ptrs_out)
{
    char    *elem_ptr    = NULL;
    char    *dest_ptr    = NULL;
    unsigned elems_count = 0;
    char   **elems       = NULL; /* more like *elems[], but compiler... */
    char   **elems_re    = NULL; /* temporary pointer, for realloc */
    char    *cpy         = NULL;
    herr_t   ret_value   = SUCCEED;
    unsigned init_slots  = 2;

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
    elems = (char **)malloc(sizeof(char *) * (init_slots + 1));
    if (elems == NULL) {
        ret_value = FAIL;
        goto done;
    } /* CANTALLOC */

    /* create destination string
     */
    start++;                                              /* advance past opening paren '(' */
    cpy = (char *)malloc(sizeof(char) * (strlen(start))); /* no +1; less '(' */
    if (cpy == NULL) {
        ret_value = FAIL;
        goto done;
    } /* CANTALLOC */

    /* set pointers
     */
    dest_ptr             = cpy;      /* start writing copy here */
    elem_ptr             = cpy;      /* first element starts here */
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
            if ((*(start + 1) == '\\') || (*(start + 1) == sep)) {
                /* Valid escape digraph of "\\" or "\<sep>".
                 */
                start++;                    /* advance past escape char '\' */
                *(dest_ptr++) = *(start++); /* Copy subsequent char  */
                                            /* and advance pointers. */
            }
            else {
                /* Not an accepted escape digraph.
                 * Copy backslash character.
                 */
                *(dest_ptr++) = *(start++);
            }
        }
        else if (*start == sep) {
            /* Non-escaped separator.
             * Terminate elements substring in copy, record element, advance.
             * Expand elements list if appropriate.
             */
            *(dest_ptr++) = 0;               /* Null-terminate elem substring in copy */
                                             /* and advance pointer.                  */
            start++;                         /* Advance src pointer past separator. */
            elem_ptr = dest_ptr;             /* Element pointer points to start of first */
                                             /* character after null sep in copy.        */
            elems[elems_count++] = elem_ptr; /* Set elem pointer in list */
                                             /* and increment count.     */

            /* Expand elements list, if necessary.
             */
            if (elems_count == init_slots) {
                init_slots *= 2;
                elems_re = (char **)realloc(elems, sizeof(char *) * (init_slots + 1));
                if (elems_re == NULL) {
                    /* CANTREALLOC */
                    ret_value = FAIL;
                    goto done;
                }
                elems = elems_re;
            }
        }
        else if (*start == ')' && *(start + 1) == '\0') {
            /* Found terminal, non-escaped close-paren. Last element.
             * Write null terminator to copy.
             * Advance source pointer to gently break from loop.
             * Required to prevent ")" from always being added to last element.
             */
            start++;
        }
        else {
            /* Copy character into destination. Advance pointers.
             */
            *(dest_ptr++) = *(start++);
        }
    }
    *dest_ptr          = '\0'; /* Null-terminate destination string. */
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
        if (cpy)
            free(cpy);
        if (elems)
            free(elems);
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
        fprintf(rawerrorstream, "error: the indentation exceeds the number of cols.\n");
        exit(1);
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
    PRINTSTREAM(rawoutstream, "%s: Version %u.%u.%u%s%s\n", progname, H5_VERS_MAJOR, H5_VERS_MINOR,
                H5_VERS_RELEASE, ((const char *)H5_VERS_SUBRELEASE)[0] ? "-" : "", H5_VERS_SUBRELEASE);
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
init_table(hid_t fid, table_t **tbl)
{
    table_t *table = (table_t *)malloc(sizeof(table_t));

    table->fid   = fid;
    table->size  = 20;
    table->nobjs = 0;
    table->objs  = (obj_t *)malloc(table->size * sizeof(obj_t));

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
    unsigned u; /* Local index value */

    /* Free the names for the objects in the table */
    for (u = 0; u < table->nobjs; u++)
        if (table->objs[u].objname)
            free(table->objs[u].objname);

    free(table->objs);
    free(table);
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
dump_table(hid_t fid, char *tablename, table_t *table)
{
    unsigned u;
    char    *obj_tok_str = NULL;

    PRINTSTREAM(rawoutstream, "%s: # of entries = %d\n", tablename, table->nobjs);
    for (u = 0; u < table->nobjs; u++) {
        H5Otoken_to_str(fid, &table->objs[u].obj_token, &obj_tok_str);

        PRINTSTREAM(rawoutstream, "%s %s %d %d\n", obj_tok_str, table->objs[u].objname,
                    table->objs[u].displayed, table->objs[u].recorded);

        H5free_memory(obj_tok_str);
    }
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
    dump_table(info->fid, "group_table", info->group_table);
    dump_table(info->fid, "dset_table", info->dset_table);
    dump_table(info->fid, "type_table", info->type_table);
}
#endif /* H5DUMP_DEBUG */

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
search_obj(table_t *table, const H5O_token_t *obj_token)
{
    unsigned u;
    int      token_cmp;

    for (u = 0; u < table->nobjs; u++) {
        if (H5Otoken_cmp(table->fid, &table->objs[u].obj_token, obj_token, &token_cmp) < 0)
            return NULL;
        if (!token_cmp)
            return &(table->objs[u]);
    }

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
find_objs_cb(const char *name, const H5O_info2_t *oinfo, const char *already_seen, void *op_data)
{
    find_objs_t *info      = (find_objs_t *)op_data;
    herr_t       ret_value = 0;

    switch (oinfo->type) {
        case H5O_TYPE_GROUP:
            if (NULL == already_seen)
                add_obj(info->group_table, &oinfo->token, name, true);
            break;

        case H5O_TYPE_DATASET:
            if (NULL == already_seen) {
                hid_t dset = H5I_INVALID_HID;

                /* Add the dataset to the list of objects */
                add_obj(info->dset_table, &oinfo->token, name, true);

                /* Check for a dataset that uses a named datatype */
                if ((dset = H5Dopen2(info->fid, name, H5P_DEFAULT)) >= 0) {
                    hid_t type = H5Dget_type(dset);

                    if (H5Tcommitted(type) > 0) {
                        H5O_info2_t type_oinfo;

                        H5Oget_info3(type, &type_oinfo, H5O_INFO_BASIC);
                        if (search_obj(info->type_table, &type_oinfo.token) == NULL)
                            add_obj(info->type_table, &type_oinfo.token, name, false);
                    } /* end if */

                    H5Tclose(type);
                    H5Dclose(dset);
                } /* end if */
                else
                    ret_value = FAIL;
            } /* end if */
            break;

        case H5O_TYPE_NAMED_DATATYPE:
            if (NULL == already_seen) {
                obj_t *found_obj;

                if ((found_obj = search_obj(info->type_table, &oinfo->token)) == NULL)
                    add_obj(info->type_table, &oinfo->token, name, true);
                else {
                    /* Use latest version of name */
                    free(found_obj->objname);
                    found_obj->objname = strdup(name);

                    /* Mark named datatype as having valid name */
                    found_obj->recorded = true;
                } /* end else */
            }     /* end if */
            break;

        case H5O_TYPE_MAP:
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
init_objs(hid_t fid, find_objs_t *info, table_t **group_table, table_t **dset_table, table_t **type_table)
{
    herr_t ret_value = SUCCEED;

    /* Initialize the tables */
    init_table(fid, group_table);
    init_table(fid, dset_table);
    init_table(fid, type_table);

    /* Init the find_objs_t */
    info->fid         = fid;
    info->group_table = *group_table;
    info->type_table  = *type_table;
    info->dset_table  = *dset_table;

    /* Find all shared objects */
    if ((ret_value = h5trav_visit(fid, "/", true, true, find_objs_cb, NULL, info, H5O_INFO_BASIC)) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "finding shared objects failed");

done:
    /* Release resources */
    if (ret_value < 0) {
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
add_obj(table_t *table, const H5O_token_t *obj_token, const char *objname, bool record)
{
    size_t u;

    /* See if we need to make table larger */
    if (table->nobjs == table->size) {
        table->size *= 2;
        table->objs = (struct obj_t *)realloc(table->objs, table->size * sizeof(table->objs[0]));
    } /* end if */

    /* Increment number of objects in table */
    u = table->nobjs++;

    /* Set information about object */
    memcpy(&table->objs[u].obj_token, obj_token, sizeof(H5O_token_t));
    table->objs[u].objname   = strdup(objname);
    table->objs[u].recorded  = record;
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
 * Parameters:
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
H5tools_get_symlink_info(hid_t file_id, const char *linkpath, h5tool_link_info_t *link_info,
                         bool get_obj_type)
{
    htri_t      l_ret;
    H5O_info2_t trg_oinfo;
    hid_t       fapl      = H5P_DEFAULT;
    hid_t       lapl      = H5P_DEFAULT;
    int         ret_value = -1; /* init to fail */

    /* init */
    link_info->trg_type = H5O_TYPE_UNKNOWN;

    /* if path is root, return group type */
    if (!strcmp(linkpath, "/")) {
        link_info->trg_type = H5O_TYPE_GROUP;
        H5TOOLS_GOTO_DONE(2);
    }

    /* check if link itself exist */
    if (H5Lexists(file_id, linkpath, H5P_DEFAULT) <= 0) {
        if (link_info->opt.msg_mode == 1)
            parallel_print("Warning: link <%s> doesn't exist \n", linkpath);
        H5TOOLS_GOTO_DONE(FAIL);
    } /* end if */

    /* get info from link */
    if (H5Lget_info2(file_id, linkpath, &(link_info->linfo), H5P_DEFAULT) < 0) {
        if (link_info->opt.msg_mode == 1)
            parallel_print("Warning: unable to get link info from <%s>\n", linkpath);
        H5TOOLS_GOTO_DONE(FAIL);
    } /* end if */

    /* given path is hard link (object) */
    if (link_info->linfo.type == H5L_TYPE_HARD)
        H5TOOLS_GOTO_DONE(2);

    /* trg_path must be freed out of this function when finished using */
    if ((link_info->trg_path = (char *)calloc(link_info->linfo.u.val_size, sizeof(char))) == NULL) {
        if (link_info->opt.msg_mode == 1)
            parallel_print("Warning: unable to allocate buffer for <%s>\n", linkpath);
        H5TOOLS_GOTO_DONE(FAIL);
    } /* end if */

    /* get link value */
    if (H5Lget_val(file_id, linkpath, (void *)link_info->trg_path, link_info->linfo.u.val_size, H5P_DEFAULT) <
        0) {
        if (link_info->opt.msg_mode == 1)
            parallel_print("Warning: unable to get link value from <%s>\n", linkpath);
        H5TOOLS_GOTO_DONE(FAIL);
    } /* end if */

    /*-----------------------------------------------------
     * if link type is external link use different lapl to
     * follow object in other file
     */
    if (link_info->linfo.type == H5L_TYPE_EXTERNAL) {
        if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            H5TOOLS_GOTO_DONE(FAIL);
        if (H5Pset_fapl_sec2(fapl) < 0)
            H5TOOLS_GOTO_DONE(FAIL);
        if ((lapl = H5Pcreate(H5P_LINK_ACCESS)) < 0)
            H5TOOLS_GOTO_DONE(FAIL);
        if (H5Pset_elink_fapl(lapl, fapl) < 0)
            H5TOOLS_GOTO_DONE(FAIL);
    } /* end if */

    /* Check for retrieving object info */
    if (get_obj_type) {
        /*--------------------------------------------------------------
         * if link's target object exist, get type
         */
        /* check if target object exist */
        l_ret = H5Oexists_by_name(file_id, linkpath, lapl);

        /* detect dangling link */
        if (l_ret == false) {
            H5TOOLS_GOTO_DONE(0);
        }
        else if (l_ret < 0) { /* function failed */
            H5TOOLS_GOTO_DONE(FAIL);
        }

        /* get target object info */
        if (H5Oget_info_by_name3(file_id, linkpath, &trg_oinfo, H5O_INFO_BASIC, lapl) < 0) {
            if (link_info->opt.msg_mode == 1)
                parallel_print("Warning: unable to get object information for <%s>\n", linkpath);
            H5TOOLS_GOTO_DONE(FAIL);
        } /* end if */

        /* check unknown type */
        if (trg_oinfo.type < H5O_TYPE_GROUP || trg_oinfo.type >= H5O_TYPE_NTYPES) {
            if (link_info->opt.msg_mode == 1)
                parallel_print("Warning: target object of <%s> is unknown type\n", linkpath);
            H5TOOLS_GOTO_DONE(FAIL);
        } /* end if */

        /* set target obj type to return */
        memcpy(&link_info->obj_token, &trg_oinfo.token, sizeof(H5O_token_t));
        link_info->trg_type = trg_oinfo.type;
        link_info->fileno   = trg_oinfo.fileno;
    } /* end if */
    else
        link_info->trg_type = H5O_TYPE_UNKNOWN;

    /* succeed */
    ret_value = 1;

done:
    if (fapl != H5P_DEFAULT)
        H5Pclose(fapl);
    if (lapl != H5P_DEFAULT)
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
    long        hyperslab_bufsize_mb;
    int         ret_value = 1;

    /* check if environment variable is set for the hyperslab buffer size */
    if (NULL != (env_str = getenv("H5TOOLS_BUFSIZE"))) {
        errno                = 0;
        hyperslab_bufsize_mb = strtol(env_str, (char **)NULL, 10);
        if (errno != 0 || hyperslab_bufsize_mb <= 0)
            H5TOOLS_GOTO_ERROR(FAIL, "hyperslab buffer size failed");

        /* convert MB to byte */
        H5TOOLS_BUFSIZE = (hsize_t)hyperslab_bufsize_mb * 1024 * 1024;

        H5TOOLS_MALLOCSIZE = MAX(H5TOOLS_BUFSIZE, H5TOOLS_MALLOCSIZE);
    }

done:
    return ret_value;
}

#ifdef H5_HAVE_ROS3_VFD
/*----------------------------------------------------------------------------
 *
 * Function: h5tools_parse_ros3_fapl_tuple
 *
 * Purpose:  A convenience function that parses a string containing a tuple
 *           of S3 VFD credential information and then passes the result to
 *           `h5tools_populate_ros3_fapl()` in order to setup a valid
 *           configuration for the S3 VFD.
 *
 * Return:   SUCCEED/FAIL
 *
 *----------------------------------------------------------------------------
 */
herr_t
h5tools_parse_ros3_fapl_tuple(const char *tuple_str, int delim, H5FD_ros3_fapl_ext_t *fapl_config_out)
{
    const char *ccred[4];
    unsigned    nelems     = 0;
    char       *s3cred_src = NULL;
    char      **s3cred     = NULL;
    herr_t      ret_value  = SUCCEED;

    /* Attempt to parse S3 credentials tuple */
    if (parse_tuple(tuple_str, delim, &s3cred_src, &nelems, &s3cred) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "failed to parse S3 VFD info tuple");

    /* Sanity-check tuple count */
    if (nelems != 3 && nelems != 4)
        H5TOOLS_GOTO_ERROR(FAIL, "invalid S3 VFD credentials");

    ccred[0] = (const char *)s3cred[0];
    ccred[1] = (const char *)s3cred[1];
    ccred[2] = (const char *)s3cred[2];
    if (nelems == 3) {
        ccred[3] = "";
    }
    else {
        ccred[3] = (const char *)s3cred[3];
    }

    if (0 == h5tools_populate_ros3_fapl(fapl_config_out, ccred))
        H5TOOLS_GOTO_ERROR(FAIL, "failed to populate S3 VFD FAPL config");

done:
    if (s3cred)
        free(s3cred);
    if (s3cred_src)
        free(s3cred_src);

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
 *         * NULL value strings: (&fa, {NULL?, NULL? NULL?, NULL?, ...})
 *         * Incomplete fapl info:
 *             * empty region, non-empty id, key either way, token either way
 *                 * (&fa, token, {"", "...", "?", "?"})
 *             * empty id, non-empty region, key either way, token either way
 *                 * (&fa, token,  {"...", "", "?", "?"})
 *             * "non-empty key, token either way and either id or region empty
 *                 * (&fa, token, {"",    "",    "...", "?")
 *                 * (&fa, token, {"",    "...", "...", "?")
 *                 * (&fa, token, {"...", "",    "...", "?")
 *             * Any string would overflow allowed space in fapl definition.
 *     or
 *     1 (success)
 *         * Sets components in fapl_t pointer, copying strings as appropriate.
 *         * "Default" fapl (valid version, authenticate->False, empty strings)
 *             * `values` pointer is NULL
 *                 * (&fa, token, NULL)
 *             * first four strings in `values` are empty ("")
 *                 * (&fa, token,  {"", "", "", "", ...})
 *         * Authenticating fapl
 *             * region, id, optional key and option session token provided
 *                 * (&fa, token, {"...", "...", "", ""})
 *                 * (&fa, token, {"...", "...", "...", ""})
 *                 * (&fa, token, {"...", "...", "...", "..."})
 *
 *----------------------------------------------------------------------------
 */
int
h5tools_populate_ros3_fapl(H5FD_ros3_fapl_ext_t *fa, const char **values)
{
    int show_progress = 0; /* set to 1 for debugging */
    int ret_value     = 1; /* 1 for success, 0 for failure           */
                           /* e.g.? if (!populate()) { then failed } */

    if (show_progress) {
        printf("called h5tools_populate_ros3_fapl\n");
    }

    if (fa == NULL) {
        if (show_progress) {
            printf("  ERROR: null pointer to fapl_t\n");
        }
        ret_value = 0;
        goto done;
    }

    if (show_progress) {
        printf("  preset fapl with default values\n");
    }
    fa->fa.version       = H5FD_CURR_ROS3_FAPL_T_VERSION;
    fa->fa.authenticate  = false;
    *(fa->fa.aws_region) = '\0';
    *(fa->fa.secret_id)  = '\0';
    *(fa->fa.secret_key) = '\0';
    *(fa->token)         = '\0';

    /* sanity-check supplied values
     */
    if (values != NULL) {
        if (values[0] == NULL) {
            if (show_progress) {
                printf("  ERROR: aws_region value cannot be NULL\n");
            }
            ret_value = 0;
            goto done;
        }
        if (values[1] == NULL) {
            if (show_progress) {
                printf("  ERROR: secret_id value cannot be NULL\n");
            }
            ret_value = 0;
            goto done;
        }
        if (values[2] == NULL) {
            if (show_progress) {
                printf("  ERROR: secret_key value cannot be NULL\n");
            }
            ret_value = 0;
            goto done;
        }
        if (values[3] == NULL) {
            if (show_progress) {
                printf("  ERROR: token value cannot be NULL\n");
            }
            ret_value = 0;
            goto done;
        }

        /* if region and ID are supplied (key optional), write to fapl...
         * fail if value would overflow
         */
        if (*values[0] != '\0' && *values[1] != '\0') {
            if (strlen(values[0]) > H5FD_ROS3_MAX_REGION_LEN) {
                if (show_progress) {
                    printf("  ERROR: aws_region value too long\n");
                }
                ret_value = 0;
                goto done;
            }
            memcpy(fa->fa.aws_region, values[0], (strlen(values[0]) + 1));
            if (show_progress) {
                printf("  aws_region set\n");
            }

            if (strlen(values[1]) > H5FD_ROS3_MAX_SECRET_ID_LEN) {
                if (show_progress) {
                    printf("  ERROR: secret_id value too long\n");
                }
                ret_value = 0;
                goto done;
            }
            memcpy(fa->fa.secret_id, values[1], (strlen(values[1]) + 1));
            if (show_progress) {
                printf("  secret_id set\n");
            }

            if (strlen(values[2]) > H5FD_ROS3_MAX_SECRET_KEY_LEN) {
                if (show_progress) {
                    printf("  ERROR: secret_key value too long\n");
                }
                ret_value = 0;
                goto done;
            }
            memcpy(fa->fa.secret_key, values[2], (strlen(values[2]) + 1));
            if (show_progress) {
                printf("  secret_key set\n");
            }

            if (strlen(values[3]) > H5FD_ROS3_MAX_SECRET_TOK_LEN) {
                if (show_progress) {
                    printf("  ERROR: token value too long\n");
                }
                ret_value = 0;
                goto done;
            }
            memcpy(fa->token, values[3], (strlen(values[3]) + 1));
            if (show_progress) {
                printf("  token set\n");
            }

            fa->fa.authenticate = true;
            if (show_progress) {
                printf("  set to authenticate\n");
            }
        }
        else if (*values[0] != '\0' || *values[1] != '\0' || *values[2] != '\0' || *values[3] != '\0') {
            if (show_progress) {
                printf("  ERROR: invalid assortment of empty/non-empty values\n");
            }
            ret_value = 0;
            goto done;
        }
    } /* values != NULL */

done:
    return ret_value;
} /* h5tools_populate_ros3_fapl */
#endif /* H5_HAVE_ROS3_VFD */

#ifdef H5_HAVE_LIBHDFS
/*----------------------------------------------------------------------------
 *
 * Function: h5tools_parse_hdfs_fapl_tuple
 *
 * Purpose:  A convenience function that parses a string containing a tuple
 *           of HDFS VFD configuration information.
 *
 * Return:   SUCCEED/FAIL
 *
 *----------------------------------------------------------------------------
 */
herr_t
h5tools_parse_hdfs_fapl_tuple(const char *tuple_str, int delim, H5FD_hdfs_fapl_t *fapl_config_out)
{
    unsigned long k         = 0;
    unsigned      nelems    = 0;
    char         *props_src = NULL;
    char        **props     = NULL;
    herr_t        ret_value = SUCCEED;

    /* Attempt to parse HDFS configuration tuple */
    if (parse_tuple(tuple_str, delim, &props_src, &nelems, &props) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "failed to parse HDFS VFD configuration tuple");

    /* Sanity-check tuple count */
    if (nelems != 5)
        H5TOOLS_GOTO_ERROR(FAIL, "invalid HDFS VFD configuration");

    /* Populate fapl configuration structure with given properties.
     * WARNING: No error-checking is done on length of input strings...
     *          Silent overflow is possible, albeit unlikely.
     */
    if (strncmp(props[0], "", 1)) {
        strncpy(fapl_config_out->namenode_name, (const char *)props[0], strlen(props[0]));
    }
    if (strncmp(props[1], "", 1)) {
        k = strtoul((const char *)props[1], NULL, 0);
        if (errno == ERANGE)
            H5TOOLS_GOTO_ERROR(FAIL, "supposed port number wasn't");
        fapl_config_out->namenode_port = (int32_t)k;
    }
    if (strncmp(props[2], "", 1)) {
        strncpy(fapl_config_out->kerberos_ticket_cache, (const char *)props[2], strlen(props[2]));
    }
    if (strncmp(props[3], "", 1)) {
        strncpy(fapl_config_out->user_name, (const char *)props[3], strlen(props[3]));
    }
    if (strncmp(props[4], "", 1)) {
        k = strtoul((const char *)props[4], NULL, 0);
        if (errno == ERANGE)
            H5TOOLS_GOTO_ERROR(FAIL, "supposed buffersize number wasn't");
        fapl_config_out->stream_buffer_size = (int32_t)k;
    }

done:
    if (props)
        free(props);
    if (props_src)
        free(props_src);

    return ret_value;
}
#endif /* H5_HAVE_LIBHDFS */
