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
static void init_table(table_t **tbl);
#ifdef H5DUMP_DEBUG
static void dump_table(char* tablename, table_t *table);
#endif  /* H5DUMP_DEBUG */
static void add_obj(table_t *table, unsigned long *objno, char *objname, hbool_t recorded);
static char * build_obj_path_name(const char *prefix, const char *name);
static herr_t find_objs_cb(hid_t group, const char *name, void *op_data);


/*-------------------------------------------------------------------------
 * Function:    error_msg
 *
 * Purpose: Print a nicely formatted error message to stderr flushing the
 *              stdout stream first.
 *
 * Return:  Nothing
 *
 * Programmer:  Bill Wendling
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
 * Function:    warn_msg
 *
 * Purpose: Print a nicely formatted warning message to stderr flushing
 *              the stdout stream first.
 *
 * Return:  Nothing
 *
 * Programmer:  Bill Wendling
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
 * Function:    get_option
 *
 * Purpose: Determine the command-line options a user specified. We can
 *      accept both short and long type command-lines.
 *
 * Return:  Success:    The short valued "name" of the command line
 *              parameter or EOF if there are no more
 *              parameters to process.
 *
 *      Failure:    A question mark.
 *
 * Programmer:  Bill Wendling
 *              Friday, 5. January 2001
 *
 * Modifications: Pedro Vicente
 *                October, 27 2008
 * Wilcard "*" argument type
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
        } 
        
        /* wildcard argument */
        else if (*cp == '*')
        {
            /* check the next argument */
            opt_ind++;
            /* we do have an extra argument, check if not last */
            if ( argv[opt_ind][0] != '-' && (opt_ind+1) < argc )
            {
                opt_arg = argv[opt_ind++];
            }
            else
            {
                opt_arg = NULL;
            }
        }
        
        else 
        {
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
 *      defined the same as the HDF5 library version.
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
static void
init_table(table_t **tbl)
{
    table_t *table = HDmalloc(sizeof(table_t));

    table->size = 20;
    table->nobjs = 0;
    table->objs = HDmalloc(table->size * sizeof(obj_t));

    *tbl = table;
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
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_table(char* tablename, table_t *table)
{
    unsigned u;

    printf("%s: # of entries = %d\n", tablename,table->nobjs);
    for (u = 0; u < table->nobjs; u++)
    HDfprintf(stdout,"%a %s %d %d\n", table->objs[u].objno,
           table->objs[u].objname,
           table->objs[u].displayed, table->objs[u].recorded);
}


/*-------------------------------------------------------------------------
 * Function:    dump_tables
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
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
obj_t *
search_obj(table_t *table, unsigned long *_objno)
{
    haddr_t objno = ((haddr_t)_objno[1] << (8*sizeof(long))) | (haddr_t)_objno[0];
    unsigned u;

    for (u = 0; u < table->nobjs; u++)
        if (table->objs[u].objno == objno)
        return &(table->objs[u]);

    return NULL;
}


/*-------------------------------------------------------------------------
 * Function:    build_obj_path_name
 *
 * Purpose:     Allocate space & build path name from prefix & name
 *
 * Return:      Success:    SUCCEED
 *
 *              Failure:    FAIL
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
build_obj_path_name(const char *prefix, const char *name)
{
    char *path;

    path = HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    HDstrcpy(path, prefix);
    HDstrcat(path,"/");
    HDstrcat(path,name); /* absolute name of the data set */

    return(path);
} /* end build_obj_path_name() */


/*-------------------------------------------------------------------------
 * Function:    find_objs_cb
 *
 * Purpose:     Callback to find objects, committed types and store them in tables
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
static herr_t
find_objs_cb(hid_t group, const char *name, void *op_data)
{
    H5G_stat_t statbuf;
    find_objs_t *info = (find_objs_t*)op_data;
    herr_t ret_value = 0;

    if(H5Gget_objinfo(group, name, FALSE, &statbuf) < 0)
        ;     /* keep going */
    else {
        switch (statbuf.type) {
            char *tmp;

            case H5G_GROUP:
                if (search_obj(info->group_table, statbuf.objno) == NULL) {
                    char *old_prefix;

                    tmp = build_obj_path_name(info->prefix, name);
                    add_obj(info->group_table, statbuf.objno, tmp, TRUE);

                    old_prefix = info->prefix;
                    info->prefix = tmp;

                    if(H5Giterate(group, name, NULL, find_objs_cb, (void *)info) < 0)
                        ret_value = FAIL;

                    info->prefix = old_prefix;
                } /* end if */
                break;

            case H5G_DATASET:
                if (search_obj(info->dset_table, statbuf.objno) == NULL) {
                    hid_t dset;

                    tmp = build_obj_path_name(info->prefix, name);
                    add_obj(info->dset_table, statbuf.objno, tmp, TRUE);

                    if ((dset = H5Dopen (group, name)) >= 0) {
                        hid_t type;

                        type = H5Dget_type(dset);

                        if (H5Tcommitted(type) > 0) {
                            H5Gget_objinfo(type, ".", TRUE, &statbuf);

                            if (search_obj(info->type_table, statbuf.objno) == NULL) {
                                char *type_name = HDstrdup(tmp);

                                add_obj(info->type_table, statbuf.objno, type_name, FALSE);
                            } /* end if */
                        }

                        H5Tclose(type);
                        H5Dclose(dset);
                    } else {
                        ret_value = FAIL;
                    }
                } /* end if */
                break;

            case H5G_TYPE:
            {
                obj_t *found_obj;

                tmp = build_obj_path_name(info->prefix, name);
                if ((found_obj = search_obj(info->type_table, statbuf.objno)) == NULL)
                    add_obj(info->type_table, statbuf.objno, tmp, TRUE);
                else {
                    /* Use latest version of name */
                    HDfree(found_obj->objname);
                    found_obj->objname = HDstrdup(tmp);

                    /* Mark named datatype as having valid name */
                    found_obj->recorded = TRUE;
                } /* end else */
                break;
            }

            default:
                /* Ignore links, etc. */
                break;
        }
    } /* end else */

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
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
init_objs(hid_t fid, find_objs_t *info, table_t **group_table,
    table_t **dset_table, table_t **type_table)
{
    /* Initialize the tables */
    init_table(group_table);
    init_table(dset_table);
    init_table(type_table);

    /* Init the find_objs_t */
    info->prefix = (char *)"";
    info->group_table = *group_table;
    info->type_table = *type_table;
    info->dset_table = *dset_table;

    {
     /* add the root group as an object, it may have hard links to it */
     
     H5G_stat_t statbuf;
     unsigned long   objno[2];   /*object number         */
     char*      tmp;
     
     if(H5Gget_objinfo(fid, "/", FALSE, &statbuf) < 0)
         return FAIL;
     else 
     {
         objno[0] = (unsigned long)(statbuf.objno[0]);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
         objno[1] = (unsigned long)(statbuf.objno[1] >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
         objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */

         /* call with an empty string, it appends group separator */
         tmp = build_obj_path_name(info->prefix, "");
         add_obj(info->group_table, objno, tmp, TRUE);
     }
    }

    /* Find all shared objects */
    return(H5Giterate(fid, "/", NULL, find_objs_cb, (void *)info));
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
add_obj(table_t *table, unsigned long *_objno, char *objname, hbool_t record)
{
    haddr_t objno = ((haddr_t)_objno[1] << (8*sizeof(long))) | (haddr_t)_objno[0];
    unsigned u;

    /* See if we need to make table larger */
    if (table->nobjs == table->size) {
        table->size *= 2;
        table->objs = HDrealloc(table->objs, table->size * sizeof(obj_t));
    }

    /* Increment number of objects in table */
    u = table->nobjs++;

    /* Set information about object */
    table->objs[u].objno = objno;
    table->objs[u].objname = objname;
    table->objs[u].recorded = record;
    table->objs[u].displayed = 0;
}
