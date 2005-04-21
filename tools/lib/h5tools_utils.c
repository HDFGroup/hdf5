/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Bill Wendling <wendling@ncsa.uiuc.edu>
 *              Tuesday, 6. March 2001
 */

/*
 * Portions of this work are derived from _Obfuscated C and Other Mysteries_,
 * by Don Libes, copyright (c) 1993 by John Wiley & Sons, Inc.
 */

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "h5tools_utils.h"
#include "H5private.h"

/* global variables */
int   nCols = 80;

/* ``get_option'' variables */
int         opt_err = 1;    /*get_option prints errors if this is on */
int         opt_ind = 1;    /*token pointer                          */
const char *opt_arg;        /*flag argument (or value)               */

/* local functions */
static void add_obj(table_t *table, unsigned long *objno, char *objname);


/*-------------------------------------------------------------------------
 * Function:	error_msg
 *
 * Purpose:	Print a nicely formatted error message to stderr flushing the
 *              stdout stream first.
 *
 * Return:	Nothing
 *
 * Programmer:	Bill Wendling
 *              Tuesday, 20. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
error_msg(const char *progname, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    HDfflush(stdout);
    HDfprintf(stderr, "%s error: ", progname);
    HDvfprintf(stderr, fmt, ap);
    
    va_end(ap);
}


/*-------------------------------------------------------------------------
 * Function:	warn_msg
 *
 * Purpose:	Print a nicely formatted warning message to stderr flushing
 *              the stdout stream first.
 *
 * Return:	Nothing
 *
 * Programmer:	Bill Wendling
 *              Tuesday, 20. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
warn_msg(const char *progname, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    HDfflush(stdout);
#ifdef WIN32
    HDfprintf(stdout, "%s warning: ", progname);
    HDvfprintf(stdout, fmt, ap);
#else /* WIN32 */
    HDfprintf(stderr, "%s warning: ", progname);
    HDvfprintf(stderr, fmt, ap);
#endif /* WIN32 */
    va_end(ap);
}


/*-------------------------------------------------------------------------
 * Function:	get_option
 *
 * Purpose:	Determine the command-line options a user specified. We can
 *		accept both short and long type command-lines.
 *
 * Return:	Success:	The short valued "name" of the command line
 * 				parameter or EOF if there are no more
 * 				parameters to process.
 *
 *		Failure:	A question mark.
 *
 * Programmer:	Bill Wendling
 *              Friday, 5. January 2001
 *
 * Modifications:
 *
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
        } else if (HDstrcmp(argv[opt_ind], "--") == 0) {
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
                    } else if (opt_ind < (argc - 1) && argv[opt_ind + 1][0] != '-') {
                        opt_arg = argv[++opt_ind];
                    } else if (l_opts[i].has_arg == require_arg) {
                        if (opt_err)
                            HDfprintf(stderr,
                                    "%s: option required for \"--%s\" flag\n",
                                    argv[0], arg);

                        opt_opt = '?';
                    }
                } else {
                    if (arg[len] == '=') {
                        if (opt_err)
                            HDfprintf(stderr,
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
                HDfprintf(stderr, "%s: unknown option \"%s\"\n", argv[0], arg);

            opt_opt = '?';
        }

        opt_ind++;
        sp = 1;
    } else {
        register char *cp;    /* pointer into current token */

        /* short command line option */
        opt_opt = argv[opt_ind][sp];

        if (opt_opt == ':' || (cp = strchr(opts, opt_opt)) == 0) {
            if (opt_err)
                HDfprintf(stderr, "%s: unknown option \"%c\"\n",
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
            } else if (++opt_ind >= argc) {
                if (opt_err)
                    HDfprintf(stderr,
                            "%s: value expected for option \"%c\"\n",
                            argv[0], opt_opt);

                opt_opt = '?';
            } else {
                /* flag value is next token */
                opt_arg = argv[opt_ind++];
            }

            sp = 1;
        } else {
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


/*-------------------------------------------------------------------------
 * Function:    indentation
 *
 * Purpose:     Print spaces for indentation
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
indentation(int x)
{
    if (x < nCols) {
        while (x-- > 0)
            printf(" ");
    } else {
        HDfprintf(stderr, "error: the indentation exceeds the number of cols.\n");
        exit(1);
    }
}


/*-------------------------------------------------------------------------
 * Function:    print_version
 *
 * Purpose:     Print the program name and the version information which is
 *		defined the same as the HDF5 library version.
 *
 * Return:      void
 *
 * Programmer:  unknown
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
print_version(const char *progname)
{
    printf("%s: Version %u.%u.%u%s%s\n",
           progname, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE,
           H5_VERS_SUBRELEASE[0] ? "-" : "", H5_VERS_SUBRELEASE);
}


/*-------------------------------------------------------------------------
 * Function:    init_table
 *
 * Purpose:     allocate and initialize tables for shared groups, datasets, 
 *              and committed types
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
init_table(table_t **tbl)
{
    int i;
    table_t *table = HDmalloc(sizeof(table_t));

    table->size = 20;
    table->nobjs = 0;
    table->objs = HDmalloc(table->size * sizeof(obj_t));

    for (i = 0; i < table->size; i++) {
        table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
        table->objs[i].displayed = 0;
        table->objs[i].recorded = 0;
        table->objs[i].objflag = 0;
        table->objs[i].objname = NULL;
    }

    *tbl = table;
}


/*-------------------------------------------------------------------------
 * Function:    init_prefix
 *
 * Purpose:     allocate and initialize prefix
 *
 * Return:      void
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
init_prefix(char **prefix, size_t prefix_len)
{
    assert(prefix_len > 0);
    *prefix = HDcalloc(prefix_len, 1);
}


/*-------------------------------------------------------------------------
 * Function:    free_table
 *
 * Purpose:     free tables for shared groups, datasets, 
 *              and committed types
 *
 * Return:      void
 *
 * Programmer:  Paul Harten
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
free_table(table_t **table)
{
    HDfree((*table)->objs);
}


/*-------------------------------------------------------------------------
 * Function:    search_obj
 *
 * Purpose:     search the object specified by objno in the table
 *
 * Return:      Success:    an integer, the location of the object
 *
 *              Failure:    FAIL   if object is not found
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int 
search_obj(table_t *table, unsigned long *objno)
{
    int i;

    for (i = 0; i < table->nobjs; i++)
        if (table->objs[i].objno[0] == *objno && table->objs[i].objno[1] == *(objno + 1))
	    return i;
  
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:    find_objs 
 *
 * Purpose:     Find objects, committed types and store them in tables
 *
 * Return:      Success:    SUCCEED
 *
 *              Failure:    FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
find_objs(hid_t group, const char *name, void *op_data)
{
    hid_t obj, type;
    H5G_stat_t statbuf;
    char *tmp;
    find_objs_t *info = (find_objs_t*)op_data;
    int i;

    if (info->threshold > 1)
        /*will get an infinite loop if greater than 1*/
        return FAIL;

    H5Gget_objinfo(group, name, TRUE, &statbuf);

    tmp = HDmalloc(HDstrlen(info->prefix) + HDstrlen(name) + 2);
    HDstrcpy(tmp, info->prefix); 

    switch (statbuf.type) {
        case H5G_GROUP:
            if ((obj = H5Gopen(group, name)) >= 0) {
                while (info->prefix_len < (HDstrlen(info->prefix) + HDstrlen(name) + 2)) {
                    info->prefix_len *= 2;
                    info->prefix = HDrealloc(info->prefix,
                                           info->prefix_len * sizeof(char));
                }

                HDstrcat(HDstrcat(info->prefix,"/"), name);

                if (statbuf.nlink > info->threshold) {
                    if (search_obj(info->group_table, statbuf.objno) == FAIL) {
                        add_obj(info->group_table, statbuf.objno, info->prefix); 
                        H5Giterate(obj, ".", NULL, find_objs, (void *)info);
                    }
                } else {
                    H5Giterate (obj, ".", NULL, find_objs, (void *)info);
                }

                HDstrcpy(info->prefix, tmp);
                H5Gclose (obj);
            } else {
                info->status = 1;
            }

            break;

        case H5G_DATASET:
            HDstrcat(tmp,"/");
            HDstrcat(tmp,name); /* absolute name of the data set */

            if (statbuf.nlink > info->threshold  &&
                            search_obj(info->dset_table, statbuf.objno) == FAIL)
                add_obj(info->dset_table, statbuf.objno, tmp);

            if ((obj = H5Dopen (group, name)) >= 0) {              
                type = H5Dget_type(obj);

                if (H5Tcommitted(type) > 0) {
                    H5Gget_objinfo(type, ".", TRUE, &statbuf);

                    if (search_obj(info->type_table, statbuf.objno) == FAIL) {
                        add_obj(info->type_table, statbuf.objno, tmp);
                        info->type_table->objs[info->type_table->nobjs - 1].objflag = 0;
                    }
                }

                H5Tclose(type);
                H5Dclose (obj);
            } else {
                info->status = 1;
            }
                
            break;

        case H5G_TYPE:
            HDstrcat(tmp,"/");
            HDstrcat(tmp,name); /* absolute name of the type */
            i = search_obj(info->type_table, statbuf.objno);

            if (i == FAIL) {
                add_obj(info->type_table, statbuf.objno, tmp) ;

                /* named data type */
                info->type_table->objs[info->type_table->nobjs-1].recorded = 1;

                /* named data type */
                info->type_table->objs[info->type_table->nobjs-1].objflag = 1;
            } else {
                free(info->type_table->objs[i].objname);
                info->type_table->objs[i].objname = HDstrdup(tmp);
                info->type_table->objs[i].recorded = 1; 

                /* named data type */  
                info->type_table->objs[info->type_table->nobjs-1].objflag = 1;
            }

            break;

        default:
            break;
    }

    HDfree(tmp);
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:    dump_table
 *
 * Purpose:     display the contents of tables for debugging purposes
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void 
dump_table(char* tablename, table_t *table)
{
    int i;

    printf("%s: # of entries = %d\n", tablename,table->nobjs);

    for (i = 0; i < table->nobjs; i++)
        printf("\t%lu %lu %s %d\n",
               table->objs[i].objno[0],
               table->objs[i].objno[1],
               table->objs[i].objname,
               table->objs[i].objflag);
}


/*-------------------------------------------------------------------------
 * Function:    get_table_idx
 *
 * Purpose:     Determine if objects are in a link loop
 *
 * Return:      Success:    table index of object detected to be in loop
 *
 *              Failure:    FAIL
 *
 * Programmer:  Paul Harten
 *
 *-------------------------------------------------------------------------
 */
int
get_table_idx(table_t *table, unsigned long *objno)
{
    return search_obj(table, objno);
}


/*-------------------------------------------------------------------------
 * Function:    get_tableflag
 *
 * Purpose:     Return the i'th element's flag setting
 *
 * Return:      Boolean setting of the i'th element of the object table flag
 *
 * Programmer:  Paul Harten
 *
 *-------------------------------------------------------------------------
 */
int
get_tableflag(table_t *table, int idx)
{
    return table->objs[idx].objflag;
}


/*-------------------------------------------------------------------------
 * Function:    set_tableflag
 *
 * Purpose:     Set the i'th element of the object table's flag to TRUE
 *
 * Return:      Success:    SUCCEED
 *
 *              Failure:    N/A
 *
 * Programmer:  Paul Harten
 *
 *-------------------------------------------------------------------------
 */
int
set_tableflag(table_t *table, int idx)
{
    table->objs[idx].objflag = TRUE;
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:    get_objectname
 *
 * Purpose:     Get name of i'th object in table
 *
 * Return:      Success:    strdup() of object name character string
 *
 *              Failure:    NULL and sets errno to ENOMEM
 *
 * Programmer:  Paul Harten
 *
 *-------------------------------------------------------------------------
 */
char *
get_objectname(table_t *table, int idx)
{
    return HDstrdup(table->objs[idx].objname);
}


/*-------------------------------------------------------------------------
 * Function:    add_obj
 *
 * Purpose:     add a shared object to the table
 *              realloc the table if necessary
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
add_obj(table_t *table, unsigned long *objno, char *objname)
{
    int i;

    if (table->nobjs == table->size) {
        table->size *= 2;
        table->objs = HDrealloc(table->objs, table->size * sizeof(obj_t));

        for (i = table->nobjs; i < table->size; i++) {
            table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
            table->objs[i].displayed = 0;
            table->objs[i].recorded = 0;
            table->objs[i].objflag = 0;
            table->objs[i].objname = NULL;
        }
    }

    i = table->nobjs++;
    table->objs[i].objno[0] = objno[0];
    table->objs[i].objno[1] = objno[1];
    free(table->objs[i].objname);
    table->objs[i].objname = HDstrdup(objname);
}
