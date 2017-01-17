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

#include <stdlib.h>

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5diff.h"
#include "ph5diff.h"


/*-------------------------------------------------------------------------
 * Function: print_objname
 *
 * Purpose: check if object name is to be printed, only when:
 *  1) verbose mode
 *  2) when diff was found (normal mode)
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE int
print_objname (diff_opt_t * options, hsize_t nfound)
{
    return ((options->m_verbose || nfound) && !options->m_quiet) ? 1 : 0;
}

/*-------------------------------------------------------------------------
 * Function: do_print_objname
 *
 * Purpose: print object name
 *
 *-------------------------------------------------------------------------
 */
void do_print_objname (const char *OBJ, const char *path1, const char *path2, diff_opt_t * opts)
{
    /* if verbose level is higher than 0, put space line before
     * displaying any object or symbolic links. This improves
     * readability of the output.
     */
    if (opts->m_verbose_level >= 1)
        parallel_print("\n");
    parallel_print("%-7s: <%s> and <%s>\n", OBJ, path1, path2);
}

/*-------------------------------------------------------------------------
 * Function: do_print_attrname
 *
 * Purpose: print attribute name
 *
 *-------------------------------------------------------------------------
 */
void
do_print_attrname (const char *attr, const char *path1, const char *path2)
{
    parallel_print("%-7s: <%s> and <%s>\n", attr, path1, path2);
}

/*-------------------------------------------------------------------------
 * Function: print_warn
 *
 * Purpose: check print warning condition.
 * Return:
 *    1 if verbose mode
 *    0 if not verbos mode
 * Programmer: Jonathan Kim
 * Date: Feb 4, 2010
 *-------------------------------------------------------------------------
 */
static int print_warn(diff_opt_t *options)
{
    return ((options->m_verbose))?1:0;
}


#ifdef H5_HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function: phdiff_dismiss_workers
 *
 * Purpose: tell all workers to end.
 *
 * Return: none
 *
 * Programmer: Albert Cheng
 *
 * Date: Feb 6, 2005
 *
 *-------------------------------------------------------------------------
 */
void phdiff_dismiss_workers(void)
{
    int i;
    for(i=1; i<g_nTasks; i++)
        MPI_Send(NULL, 0, MPI_BYTE, i, MPI_TAG_END, MPI_COMM_WORLD);
}


/*-------------------------------------------------------------------------
 * Function: print_incoming_data
 *
 * Purpose: special function that prints any output that has been sent to the manager
 *      and is currently sitting in the incoming message queue
 *
 * Return: none
 *
 * Programmer: Leon Arber
 *
 * Date: March 7, 2005
 *
 *-------------------------------------------------------------------------
 */

static void print_incoming_data(void)
{
    char data[PRINT_DATA_MAX_SIZE+1];
    int  incomingMessage;
    MPI_Status Status;

    do
    {
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &incomingMessage, &Status);
        if(incomingMessage)
        {
            HDmemset(data, 0, PRINT_DATA_MAX_SIZE+1);
            MPI_Recv(data, PRINT_DATA_MAX_SIZE, MPI_CHAR, Status.MPI_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &Status);

            printf("%s", data);
        }
    } while(incomingMessage);
}
#endif

/*-------------------------------------------------------------------------
 * Function: is_valid_options
 *
 * Purpose: check if options are valid
 *
 * Return:
 *  1 : Valid
 *  0 : Not valid
 *
 * Programmer: Jonathan Kim
 *
 * Date: Feb 17, 2010
 *
 *------------------------------------------------------------------------*/
static int is_valid_options(diff_opt_t *options)
{
    int ret=1; /* init to valid */

    /*-----------------------------------------------
     * no -q(quiet) with -v (verbose) or -r (report) */
    if(options->m_quiet && (options->m_verbose || options->m_report))
    {
        parallel_print("Error: -q (quiet mode) cannot be added to verbose or report modes\n");
        options->err_stat=1;
        ret = 0;
        goto out;
    }

    /* -------------------------------------------------------
     * only allow --no-dangling-links along with --follow-symlinks */
    if(options->no_dangle_links && !options->follow_links)
    {
        parallel_print("Error: --no-dangling-links must be used along with --follow-symlinks option.\n");
        options->err_stat=1;
        ret = 0;
        goto out;
    }

out:

    return ret;
}

/*-------------------------------------------------------------------------
 * Function: is_exclude_path
 *
 * Purpose: check if 'paths' are part of exclude path list
 *
 * Return:
 *   1 - excluded path
 *   0 - not excluded path
 *
 * Programmer: Jonathan Kim
 * Date: Aug 23, 2010
 *------------------------------------------------------------------------*/
static int is_exclude_path (char * path, h5trav_type_t type, diff_opt_t *options)
{
    struct exclude_path_list * exclude_path_ptr;
    int ret_cmp;
    int ret = 0;

    /* check if exclude path option is given */
    if (!options->exclude_path)
        goto out;

    /* assign to local exclude list pointer */
    exclude_path_ptr = options->exclude;

    /* search objects in exclude list */
    while (NULL != exclude_path_ptr)
    {
        /* if exclude path is is group, exclude its members as well */
        if (exclude_path_ptr->obj_type == H5TRAV_TYPE_GROUP)
        {
            ret_cmp = HDstrncmp(exclude_path_ptr->obj_path, path,
                                HDstrlen(exclude_path_ptr->obj_path));
            if (ret_cmp == 0)  /* found matching members */
            {
                size_t len_grp;

                /* check if given path belong to an excluding group, if so
                 * exclude it as well.
                 * This verifies if “/grp1/dset1” is only under “/grp1”, but
                 * not under “/grp1xxx/” group.
                 */
                len_grp = HDstrlen(exclude_path_ptr->obj_path);
                if (path[len_grp] == '/')
                {
                    /* belong to excluded group! */
                    ret = 1;
                    break;  /* while */
                }
            }
        }
        /* exclude target is not group, just exclude the object */
        else
        {
            ret_cmp = HDstrcmp(exclude_path_ptr->obj_path, path);
            if (ret_cmp == 0)  /* found matching object */
            {
                /* excluded non-group object */
                ret = 1;
                /* remember the type of this maching object.
                 * if it's group, it can be used for excluding its member
                 * objects in this while() loop */
                exclude_path_ptr->obj_type = type;
                break; /* while */
            }
        }
        exclude_path_ptr = exclude_path_ptr->next;
    }

out:
    return  ret;
}


/*-------------------------------------------------------------------------
 * Function: free_exclude_path_list
 *
 * Purpose: free exclud object list from diff options
 *
 * Programmer: Jonathan Kim
 * Date: Aug 23, 2010
 *------------------------------------------------------------------------*/
static void free_exclude_path_list(diff_opt_t *options)
{
    struct exclude_path_list * curr = options->exclude;
    struct exclude_path_list * next;

    while (NULL != curr)
    {
        next = curr->next;
        HDfree(curr);
        curr = next;
    }
}

/*-------------------------------------------------------------------------
 * Function: build_match_list
 *
 * Purpose: get list of matching path_name from info1 and info2
 *
 * Note:
 *  Find common objects; the algorithm used for this search is the
 *  cosequential match algorithm and is described in
 *  Folk, Michael; Zoellick, Bill. (1992). File Structures. Addison-Wesley.
 *  Moved out from diff_match() to make code more flexible.
 *
 * Parameter:
 *  table_out [OUT] : return the list
 *
 * Programmer: Jonathan Kim
 *
 * Date: Aug 18, 2010
 *------------------------------------------------------------------------*/
static void build_match_list (const char *objname1, trav_info_t *info1, const char *objname2, trav_info_t *info2, trav_table_t ** table_out, diff_opt_t *options)
{
    size_t curr1 = 0;
    size_t curr2 = 0;
    unsigned infile[2];
    char * path1_lp;
    char * path2_lp;
    h5trav_type_t type1_l;
    h5trav_type_t type2_l;
    size_t path1_offset = 0;
    size_t path2_offset = 0;
    int cmp;
    trav_table_t *table;
    size_t  idx;

    h5difftrace("build_match_list start\n");
    /* init */
    trav_table_init( &table );

    /*
     * This is necessary for the case that given objects are group and
     * have different names (ex: obj1 is /grp1 and obj2 is /grp5).
     * All the objects belong to given groups are the cadidates.
     * So prepare to compare paths without the group names.
     */

    /* if obj1 is not root */
    if (HDstrcmp (objname1,"/") != 0)
        path1_offset = HDstrlen(objname1);
    /* if obj2 is not root */
    if (HDstrcmp (objname2,"/") != 0)
        path2_offset = HDstrlen(objname2);

    /*--------------------------------------------------
    * build the list
    */
    while(curr1 < info1->nused && curr2 < info2->nused)
    {
        path1_lp = (info1->paths[curr1].path) + path1_offset;
        path2_lp = (info2->paths[curr2].path) + path2_offset;
        type1_l = info1->paths[curr1].type;
        type2_l = info2->paths[curr2].type;

        /* criteria is string compare */
        cmp = HDstrcmp(path1_lp, path2_lp);

        if(cmp == 0)
        {
            if(!is_exclude_path(path1_lp, type1_l, options))
            {
                infile[0] = 1;
                infile[1] = 1;
                trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
                /* if the two point to the same target object,
                 * mark that in table */
                if (info1->paths[curr1].fileno == info2->paths[curr2].fileno &&
                    info1->paths[curr1].objno == info2->paths[curr2].objno )
                {
                    idx = table->nobjs - 1;
                    table->objs[idx].is_same_trgobj = 1;
                }
            }
            curr1++;
            curr2++;
        } /* end if */
        else if(cmp < 0)
        {
            if(!is_exclude_path(path1_lp, type1_l, options))
            {
                infile[0] = 1;
                infile[1] = 0;
                trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
            }
            curr1++;
        } /* end else-if */
        else
        {
            if (!is_exclude_path(path2_lp, type2_l, options))
            {
                infile[0] = 0;
                infile[1] = 1;
                trav_table_addflags(infile, path2_lp, info2->paths[curr2].type, table);
            }
            curr2++;
        } /* end else */
    } /* end while */

    /* list1 did not end */
    infile[0] = 1;
    infile[1] = 0;
    while(curr1 < info1->nused)
    {
        path1_lp = (info1->paths[curr1].path) + path1_offset;
        type1_l = info1->paths[curr1].type;

        if(!is_exclude_path(path1_lp, type1_l, options))
        {
            trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
        }
        curr1++;
    } /* end while */

    /* list2 did not end */
    infile[0] = 0;
    infile[1] = 1;
    while(curr2 < info2->nused)
    {
        path2_lp = (info2->paths[curr2].path) + path2_offset;
        type2_l = info2->paths[curr2].type;

        if (!is_exclude_path(path2_lp, type2_l, options))
        {
            trav_table_addflags(infile, path2_lp, info2->paths[curr2].type, table);
        }
        curr2++;
    } /* end while */

    free_exclude_path_list (options);

    *table_out = table;
    h5difftrace("build_match_list finish\n");
}


/*-------------------------------------------------------------------------
 * Function: trav_grp_objs
 *
 * Purpose:
 *  Call back function from h5trav_visit().
 *
 * Programmer: Jonathan Kim
 *
 * Date: Aug 16, 2010
 *------------------------------------------------------------------------*/
static herr_t trav_grp_objs(const char *path, const H5O_info_t *oinfo,
    const char *already_visited, void *udata)
{
    trav_info_visit_obj(path, oinfo, already_visited, udata);

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: trav_grp_symlinks
 *
 * Purpose:
 *  Call back function from h5trav_visit().
 *  Track and extra checkings while visiting all symbolic-links.
 *
 * Programmer: Jonathan Kim
 *
 * Date: Aug 16, 2010
 *------------------------------------------------------------------------*/
static herr_t trav_grp_symlinks(const char *path, const H5L_info_t *linfo,
                               void *udata)
{
    trav_info_t *tinfo = (trav_info_t *)udata;
    diff_opt_t *opts = (diff_opt_t *)tinfo->opts;
    int ret;
    h5tool_link_info_t lnk_info;
    const char *ext_fname;
    const char *ext_path;

    /* init linkinfo struct */
    HDmemset(&lnk_info, 0, sizeof(h5tool_link_info_t));

    if (!opts->follow_links)
    {
        trav_info_visit_lnk(path, linfo, tinfo);
        goto done;
    }

    switch(linfo->type)
    {
        case H5L_TYPE_SOFT:
            ret = H5tools_get_symlink_info(tinfo->fid, path, &lnk_info, opts->follow_links);
            /* error */
            if (ret < 0)
                goto done;
            /* no dangling link option given and detect dangling link */
            else if (ret == 0)
            {
                tinfo->symlink_visited.dangle_link = TRUE;
                trav_info_visit_lnk(path, linfo, tinfo);
                if (opts->no_dangle_links)
                    opts->err_stat = 1; /* make dangling link is error */
                goto done;
            }

            /* check if already visit the target object */
            if(symlink_is_visited( &(tinfo->symlink_visited), linfo->type, NULL, lnk_info.trg_path))
                goto done;

            /* add this link as visited link */
            if(symlink_visit_add( &(tinfo->symlink_visited), linfo->type, NULL, lnk_info.trg_path) < 0)
                goto done;

            if(h5trav_visit(tinfo->fid, path, TRUE, TRUE,
                         trav_grp_objs,trav_grp_symlinks, tinfo) < 0)
            {
                parallel_print("Error: Could not get file contents\n");
                opts->err_stat = 1;
                goto done;
            }
            break;

        case H5L_TYPE_EXTERNAL:
            ret = H5tools_get_symlink_info(tinfo->fid, path, &lnk_info, opts->follow_links);
            /* error */
            if (ret < 0)
                goto done;
            /* no dangling link option given and detect dangling link */
            else if (ret == 0)
            {
                tinfo->symlink_visited.dangle_link = TRUE;
                trav_info_visit_lnk(path, linfo, tinfo);
                if (opts->no_dangle_links)
                    opts->err_stat = 1; /* make dangling link is error */
                goto done;
            }

            if(H5Lunpack_elink_val(lnk_info.trg_path, linfo->u.val_size, NULL, &ext_fname, &ext_path) < 0)
                goto done;

            /* check if already visit the target object */
            if(symlink_is_visited( &(tinfo->symlink_visited), linfo->type, ext_fname, ext_path))
                goto done;

            /* add this link as visited link */
            if(symlink_visit_add( &(tinfo->symlink_visited), linfo->type, ext_fname, ext_path) < 0)
                goto done;

            if(h5trav_visit(tinfo->fid, path, TRUE, TRUE,
                            trav_grp_objs,trav_grp_symlinks, tinfo) < 0)
            {
                parallel_print("Error: Could not get file contents\n");
                opts->err_stat = 1;
                goto done;
            }
            break;

        case H5L_TYPE_HARD:
        case H5L_TYPE_MAX:
        case H5L_TYPE_ERROR:
        default:
            parallel_print("Error: Invalid link type\n");
            opts->err_stat = 1;
            goto done;
            break;
    } /* end of switch */

done:
    if (lnk_info.trg_path)
        HDfree(lnk_info.trg_path);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function: h5diff
 *
 * Purpose: public function, can be called in an application program.
 *   return differences between 2 HDF5 files
 *
 * Return: Number of differences found.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 22, 2003
 *
 *-------------------------------------------------------------------------
 */
hsize_t h5diff(const char *fname1,
               const char *fname2,
               const char *objname1,
               const char *objname2,
               diff_opt_t *options)
{
    hid_t        file1_id = (-1);
    hid_t        file2_id = (-1);
    char         filenames[2][MAX_FILENAME];
    hsize_t      nfound = 0;
    int l_ret1 = -1;
    int l_ret2 = -1;
    char * obj1fullname = NULL;
    char * obj2fullname = NULL;
    int both_objs_grp = 0;
    /* init to group type */
    h5trav_type_t obj1type = H5TRAV_TYPE_GROUP;
    h5trav_type_t obj2type = H5TRAV_TYPE_GROUP;
    /* for single object */
    H5O_info_t oinfo1, oinfo2; /* object info */
    trav_info_t  *info1_obj = NULL;
    trav_info_t  *info2_obj = NULL;
    /* for group object */
    trav_info_t  *info1_grp = NULL;
    trav_info_t  *info2_grp = NULL;
    /* local pointer */
    trav_info_t  *info1_lp = NULL;
    trav_info_t  *info2_lp = NULL;
    /* link info from specified object */
    H5L_info_t src_linfo1;
    H5L_info_t src_linfo2;
    /* link info from member object */
    h5tool_link_info_t trg_linfo1;
    h5tool_link_info_t trg_linfo2;
    /* list for common objects */
    trav_table_t *match_list = NULL;

    h5difftrace("h5diff start\n");
    /* init filenames */
    HDmemset(filenames, 0, MAX_FILENAME * 2);
    /* init link info struct */
    HDmemset(&trg_linfo1, 0, sizeof(h5tool_link_info_t));
    HDmemset(&trg_linfo2, 0, sizeof(h5tool_link_info_t));

   /*-------------------------------------------------------------------------
    * check invalid combination of options
    *-----------------------------------------------------------------------*/
    if(!is_valid_options(options))
        goto out;

    options->cmn_objs = 1; /* eliminate warning */

    /*-------------------------------------------------------------------------
    * open the files first; if they are not valid, no point in continuing
    *-------------------------------------------------------------------------
    */

    /* disable error reporting */
    H5E_BEGIN_TRY
    {
        /* open file 1 */
        if((file1_id = h5tools_fopen(fname1, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, (size_t)0)) < 0)
        {
            parallel_print("h5diff: <%s>: unable to open file\n", fname1);
            options->err_stat = 1;
            goto out;
        } /* end if */


        /* open file 2 */
        if((file2_id = h5tools_fopen(fname2, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, (size_t)0)) < 0)
        {
            parallel_print("h5diff: <%s>: unable to open file\n", fname2);
            options->err_stat = 1;
            goto out;
        } /* end if */
    /* enable error reporting */
    } H5E_END_TRY;

    /*-------------------------------------------------------------------------
    * Initialize the info structs
    *-------------------------------------------------------------------------
    */
    trav_info_init(fname1, file1_id, &info1_obj);
    trav_info_init(fname2, file2_id, &info2_obj);

    h5difftrace("trav_info_init initialized\n");
    /* if any object is specified */
    if (objname1)
    {
        /* make the given object1 fullpath, start with "/"  */
        if (HDstrncmp(objname1, "/", 1))
        {
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if(HDasprintf(&obj1fullname, "/%s", objname1) < 0)
                goto out;
#else /* H5_HAVE_ASPRINTF */
            /* (malloc 2 more for "/" and end-of-line) */
            obj1fullname = (char*)HDmalloc(HDstrlen(objname1) + 2);
            HDstrcpy(obj1fullname, "/");
            HDstrcat(obj1fullname, objname1);
#endif /* H5_HAVE_ASPRINTF */
        }
        else
            obj1fullname = HDstrdup(objname1);

        /* make the given object2 fullpath, start with "/" */
        if (HDstrncmp(objname2, "/", 1))
        {
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if(HDasprintf(&obj2fullname, "/%s", objname2) < 0)
                goto out;
#else /* H5_HAVE_ASPRINTF */
            /* (malloc 2 more for "/" and end-of-line) */
            obj2fullname = (char*)HDmalloc(HDstrlen(objname2) + 2);
            HDstrcpy(obj2fullname, "/");
            HDstrcat(obj2fullname, objname2);
#endif /* H5_HAVE_ASPRINTF */
        }
        else
            obj2fullname = HDstrdup(objname2);

        /*----------------------------------------------------------
         * check if obj1 is root, group, single object or symlink
         */
        h5difftrace("h5diff check if obj1 is root, group, single object or symlink\n");
        if(!HDstrcmp(obj1fullname, "/"))
        {
            obj1type = H5TRAV_TYPE_GROUP;
        }
        else
        {
            /* check if link itself exist */
            if(H5Lexists(file1_id, obj1fullname, H5P_DEFAULT) <= 0)
            {
                parallel_print ("Object <%s> could not be found in <%s>\n", obj1fullname, fname1);
                options->err_stat = 1;
                goto out;
            }
            /* get info from link */
            if(H5Lget_info(file1_id, obj1fullname, &src_linfo1, H5P_DEFAULT) < 0)
            {
                parallel_print("Unable to get link info from <%s>\n", obj1fullname);
                goto out;
            }

            info1_lp = info1_obj;

            /*
             * check the type of specified path for hard and symbolic links
             */
            if(src_linfo1.type == H5L_TYPE_HARD)
            {
                size_t idx;

                /* optional data pass */
                info1_obj->opts = (diff_opt_t*)options;

                if(H5Oget_info_by_name(file1_id, obj1fullname, &oinfo1, H5P_DEFAULT) < 0)
                {
                    parallel_print("Error: Could not get file contents\n");
                    options->err_stat = 1;
                    goto out;
                }
                obj1type = (h5trav_type_t)oinfo1.type;
                trav_info_add(info1_obj, obj1fullname, obj1type);
                idx = info1_obj->nused - 1;
                info1_obj->paths[idx].objno = oinfo1.addr;
                info1_obj->paths[idx].fileno = oinfo1.fileno;
            }
            else if (src_linfo1.type == H5L_TYPE_SOFT)
            {
                obj1type = H5TRAV_TYPE_LINK;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
            else if (src_linfo1.type == H5L_TYPE_EXTERNAL)
            {
                obj1type = H5TRAV_TYPE_UDLINK;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
        }

        /*----------------------------------------------------------
         * check if obj2 is root, group, single object or symlink
         */
        h5difftrace("h5diff check if obj2 is root, group, single object or symlink\n");
        if(!HDstrcmp(obj2fullname, "/"))
        {
            obj2type = H5TRAV_TYPE_GROUP;
        }
        else
        {
            /* check if link itself exist */
            if(H5Lexists(file2_id, obj2fullname, H5P_DEFAULT) <= 0)
            {
                parallel_print ("Object <%s> could not be found in <%s>\n", obj2fullname, fname2);
                options->err_stat = 1;
                goto out;
            }
            /* get info from link */
            if(H5Lget_info(file2_id, obj2fullname, &src_linfo2, H5P_DEFAULT) < 0)
            {
                parallel_print("Unable to get link info from <%s>\n", obj2fullname);
                goto out;
            }

            info2_lp = info2_obj;

            /*
             * check the type of specified path for hard and symbolic links
             */
            if(src_linfo2.type == H5L_TYPE_HARD)
            {
                size_t idx;

                /* optional data pass */
                info2_obj->opts = (diff_opt_t*)options;

                if(H5Oget_info_by_name(file2_id, obj2fullname, &oinfo2, H5P_DEFAULT) < 0)
                {
                    parallel_print("Error: Could not get file contents\n");
                    options->err_stat = 1;
                    goto out;
                }
                obj2type = (h5trav_type_t)oinfo2.type;
                trav_info_add(info2_obj, obj2fullname, obj2type);
                idx = info2_obj->nused - 1;
                info2_obj->paths[idx].objno = oinfo2.addr;
                info2_obj->paths[idx].fileno = oinfo2.fileno;
            }
            else if (src_linfo2.type == H5L_TYPE_SOFT)
            {
                obj2type = H5TRAV_TYPE_LINK;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
            else if (src_linfo2.type == H5L_TYPE_EXTERNAL)
            {
                obj2type = H5TRAV_TYPE_UDLINK;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
        }
    }
    /* if no object specified */
    else
    {
        h5difftrace("h5diff no object specified\n");
        /* set root group */
        obj1fullname = (char*)HDstrdup("/");
        obj1type = H5TRAV_TYPE_GROUP;
        obj2fullname = (char*)HDstrdup("/");
        obj2type = H5TRAV_TYPE_GROUP;
    }


    /* get any symbolic links info */
    l_ret1 = H5tools_get_symlink_info(file1_id, obj1fullname, &trg_linfo1, options->follow_links);
    l_ret2 = H5tools_get_symlink_info(file2_id, obj2fullname, &trg_linfo2, options->follow_links);

    /*---------------------------------------------
     * check for following symlinks
     */
    if (options->follow_links)
    {
        /* pass how to handle printing warning to linkinfo option */
        if(print_warn(options))
            trg_linfo1.opt.msg_mode = trg_linfo2.opt.msg_mode = 1;

        /*-------------------------------
         * check symbolic link (object1)
         */
        h5difftrace("h5diff check symbolic link (object1)\n");
        /* dangling link */
        if (l_ret1 == 0)
        {
            h5difftrace("h5diff ... dangling link\n");
            if (options->no_dangle_links)
            {
                /* treat dangling link is error */
                if(options->m_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", obj1fullname);
                options->err_stat = 1;
                goto out;
            }
            else
            {
                if(options->m_verbose)
                    parallel_print("obj1 <%s> is a dangling link.\n", obj1fullname);
                if (l_ret1 != 0 ||  l_ret2 != 0)
                {
                    nfound++;
                    print_found(nfound);
                    goto out;
                }
            }
        }
        else if(l_ret1 < 0) /* fail */
        {
            parallel_print ("Object <%s> could not be found in <%s>\n", obj1fullname, fname1);
            options->err_stat = 1;
            goto out;
        }
        else if(l_ret1 != 2) /* symbolic link */
        {
            obj1type = (h5trav_type_t)trg_linfo1.trg_type;
            h5difftrace("h5diff ... ... trg_linfo1.trg_type == H5L_TYPE_HARD\n");
            if (info1_lp != NULL) {
                size_t idx = info1_lp->nused - 1;

                h5difftrace("h5diff ... ... ... info1_obj not null\n");
                info1_lp->paths[idx].type = (h5trav_type_t)trg_linfo1.trg_type;
                info1_lp->paths[idx].objno = trg_linfo1.objno;
                info1_lp->paths[idx].fileno = trg_linfo1.fileno;
            }
            h5difftrace("h5diff check symbolic link (object1) finished\n");
        }

        /*-------------------------------
         * check symbolic link (object2)
         */
        h5difftrace("h5diff check symbolic link (object2)\n");
        /* dangling link */
        if (l_ret2 == 0)
        {
            h5difftrace("h5diff ... dangling link\n");
            if (options->no_dangle_links)
            {
                /* treat dangling link is error */
                if(options->m_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", obj2fullname);
                options->err_stat = 1;
                goto out;
            }
            else
            {
                if(options->m_verbose)
                    parallel_print("obj2 <%s> is a dangling link.\n", obj2fullname);
                if (l_ret1 != 0 ||  l_ret2 != 0)
                {
                    nfound++;
                    print_found(nfound);
                    goto out;
                }
            }
        }
        else if(l_ret2 < 0) /* fail */
        {
            parallel_print ("Object <%s> could not be found in <%s>\n", obj2fullname, fname2);
            options->err_stat = 1;
            goto out;
        }
        else if(l_ret2 != 2)  /* symbolic link */
        {
            obj2type = (h5trav_type_t)trg_linfo2.trg_type;
            if (info2_lp != NULL) {
                size_t idx = info2_lp->nused - 1;

                h5difftrace("h5diff ... ... ... info2_obj not null\n");
                info2_lp->paths[idx].type = (h5trav_type_t)trg_linfo2.trg_type;
                info2_lp->paths[idx].objno = trg_linfo2.objno;
                info2_lp->paths[idx].fileno = trg_linfo2.fileno;
            }
            h5difftrace("h5diff check symbolic link (object1) finished\n");
        }
    } /* end of if follow symlinks */

   /*
    * If verbose options is not used, don't need to traverse through the list
    * of objects in the group to display objects information,
    * So use h5tools_is_obj_same() to improve performance by skipping
    * comparing details of same objects.
    */

    if(!(options->m_verbose || options->m_report))
    {
        h5difftrace("h5diff NOT (options->m_verbose || options->m_report)\n");
        /* if no danglink links */
        if ( l_ret1 > 0 && l_ret2 > 0 )
            if (h5tools_is_obj_same(file1_id, obj1fullname, file2_id, obj2fullname)!=0)
                goto out;
    }

    both_objs_grp = (obj1type == H5TRAV_TYPE_GROUP && obj2type == H5TRAV_TYPE_GROUP);
    if (both_objs_grp)
    {
        h5difftrace("h5diff both_objs_grp TRUE\n");
        /*
         * traverse group1
         */
        trav_info_init(fname1, file1_id, &info1_grp);
        /* optional data pass */
        info1_grp->opts = (diff_opt_t*)options;

        if(h5trav_visit(file1_id, obj1fullname, TRUE, TRUE,
                        trav_grp_objs, trav_grp_symlinks, info1_grp) < 0)
        {
            parallel_print("Error: Could not get file contents\n");
            options->err_stat = 1;
            goto out;
        }
        info1_lp = info1_grp;

        /*
         * traverse group2
         */
        trav_info_init(fname2, file2_id, &info2_grp);
        /* optional data pass */
        info2_grp->opts = (diff_opt_t*)options;

        if(h5trav_visit(file2_id, obj2fullname, TRUE, TRUE,
                        trav_grp_objs, trav_grp_symlinks, info2_grp) < 0)
        {
            parallel_print("Error: Could not get file contents\n");
            options->err_stat = 1;
            goto out;
        } /* end if */
        info2_lp = info2_grp;
    }

#ifdef H5_HAVE_PARALLEL
    if(g_Parallel)
    {
        int i;

        if((HDstrlen(fname1) > MAX_FILENAME) || (HDstrlen(fname2) > MAX_FILENAME))
        {
            HDfprintf(stderr, "The parallel diff only supports path names up to %d characters\n", MAX_FILENAME);
            MPI_Abort(MPI_COMM_WORLD, 0);
        } /* end if */

        HDstrcpy(filenames[0], fname1);
        HDstrcpy(filenames[1], fname2);

        /* Alert the worker tasks that there's going to be work. */
        for(i = 1; i < g_nTasks; i++)
            MPI_Send(filenames, (MAX_FILENAME * 2), MPI_CHAR, i, MPI_TAG_PARALLEL, MPI_COMM_WORLD);
    } /* end if */
#endif

    /* process the objects */
    build_match_list (obj1fullname, info1_lp, obj2fullname, info2_lp,
                     &match_list, options);
    if (both_objs_grp)
    {
        /*------------------------------------------------------
         * print the list
         */
         if(options->m_verbose)
         {
             unsigned u;

             parallel_print("\n");
             /* if given objects is group under root */
             if (HDstrcmp (obj1fullname,"/") || HDstrcmp (obj2fullname,"/"))
                 parallel_print("group1   group2\n");
             else
                 parallel_print("file1     file2\n");
             parallel_print("---------------------------------------\n");
             for(u = 0; u < match_list->nobjs; u++)
             {
                 char c1, c2;
                 c1 = (match_list->objs[u].flags[0]) ? 'x' : ' ';
                 c2 = (match_list->objs[u].flags[1]) ? 'x' : ' ';
                 parallel_print("%5c %6c    %-15s\n", c1, c2, match_list->objs[u].name);
             } /* end for */
             parallel_print ("\n");
         } /* end if */
    }
    nfound = diff_match(file1_id, obj1fullname, info1_lp,
                        file2_id, obj2fullname, info2_lp,
                        match_list, options);

out:
#ifdef H5_HAVE_PARALLEL
    if(g_Parallel)
        /* All done at this point, let tasks know that they won't be needed */
        phdiff_dismiss_workers();
#endif
    /* free buffers in trav_info structures */
    if (info1_obj)
        trav_info_free(info1_obj);
    if (info2_obj)
        trav_info_free(info2_obj);

    if (info1_grp)
        trav_info_free(info1_grp);
    if (info2_grp)
        trav_info_free(info2_grp);

    /* free buffers */
    if (obj1fullname)
        HDfree(obj1fullname);
    if (obj2fullname)
        HDfree(obj2fullname);

    /* free link info buffer */
    if (trg_linfo1.trg_path)
        HDfree(trg_linfo1.trg_path);
    if (trg_linfo2.trg_path)
        HDfree(trg_linfo2.trg_path);

    /* close */
    H5E_BEGIN_TRY
    {
        H5Fclose(file1_id);
        H5Fclose(file2_id);
    } H5E_END_TRY;
    h5difftrace("h5diff finish\n");

    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_match
 *
 * Purpose:
 *  Compare common objects in given groups according to table structure.
 *  The table structure has flags which can be used to find common objects
 *  and will be compared.
 *  Common object means same name (absolute path) objects in both location.
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Modifications: Jan 2005 Leon Arber, larber@uiuc.edu
 *    Added support for parallel diffing
 *
 * Pedro Vicente, pvn@hdfgroup.org, Nov 4, 2008
 *    Compare the graph and make h5diff return 1 for difference if
 * 1) the number of objects in file1 is not the same as in file2
 * 2) the graph does not match, i.e same names (absolute path)
 * 3) objects with the same name are not of the same type
 *-------------------------------------------------------------------------
 */
hsize_t diff_match(hid_t file1_id, const char *grp1, trav_info_t *info1,
                   hid_t file2_id, const char *grp2, trav_info_t *info2,
                   trav_table_t *table, diff_opt_t *options)
{
    hsize_t      nfound = 0;
    unsigned     i;

    const char * grp1_path = "";
    const char * grp2_path = "";
    char * obj1_fullpath = NULL;
    char * obj2_fullpath = NULL;
    diff_args_t argdata;
    size_t idx1 = 0;
    size_t idx2 = 0;


    h5difftrace("diff_match start\n");
    /*
     * if not root, prepare object name to be pre-appended to group path to
     * make full path
     */
    if(HDstrcmp(grp1, "/"))
        grp1_path = grp1;
    if(HDstrcmp(grp2, "/"))
        grp2_path = grp2;

    /*-------------------------------------------------------------------------
    * regarding the return value of h5diff (0, no difference in files, 1 difference )
    * 1) the number of objects in file1 must be the same as in file2
    * 2) the graph must match, i.e same names (absolute path)
    * 3) objects with the same name must be of the same type
    *-------------------------------------------------------------------------
    */

    /* not valid compare used when --exclude-path option is used */
    if (!options->exclude_path)
    {
        /* number of different objects */
        if ( info1->nused != info2->nused )
        {
            options->contents = 0;
        }
    }

    /* objects in one file and not the other */
    for( i = 0; i < table->nobjs; i++)
    {
        if( table->objs[i].flags[0] != table->objs[i].flags[1] )
        {
            options->contents = 0;
            break;
        }
    }


    /*-------------------------------------------------------------------------
    * do the diff for common objects
    *-------------------------------------------------------------------------
    */
#ifdef H5_HAVE_PARALLEL
    {
    char *workerTasks = (char*)HDmalloc((g_nTasks - 1) * sizeof(char));
    int n;
    int busyTasks = 0;
    struct diffs_found nFoundbyWorker;
    struct diff_mpi_args args;
    int havePrintToken = 1;
    MPI_Status Status;

    /*set all tasks as free */
    HDmemset(workerTasks, 1, (g_nTasks - 1));
#endif

    for(i = 0; i < table->nobjs; i++)
    {
        if( table->objs[i].flags[0] && table->objs[i].flags[1])
        {
            /* make full path for obj1 */
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if(HDasprintf(&obj1_fullpath, "%s%s", grp1_path, table->objs[i].name) < 0)
                goto out;
#else /* H5_HAVE_ASPRINTF */
            obj1_fullpath = (char*)HDmalloc(HDstrlen(grp1_path) + HDstrlen(table->objs[i].name) + 1);
            HDstrcpy(obj1_fullpath, grp1_path);
            HDstrcat(obj1_fullpath, table->objs[i].name);
#endif /* H5_HAVE_ASPRINTF */

            /* make full path for obj2 */
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if(HDasprintf(&obj2_fullpath, "%s%s", grp2_path, table->objs[i].name) < 0)
                goto out;
#else /* H5_HAVE_ASPRINTF */
            obj2_fullpath = (char*)HDmalloc(HDstrlen(grp2_path) + HDstrlen(table->objs[i].name) + 1);
            HDstrcpy(obj2_fullpath, grp2_path);
            HDstrcat(obj2_fullpath, table->objs[i].name);
#endif /* H5_HAVE_ASPRINTF */

            /* get index to figure out type of the object in file1 */
            while(info1->paths[idx1].path &&
                    (HDstrcmp(obj1_fullpath, info1->paths[idx1].path) != 0))
                idx1++;
            /* get index to figure out type of the object in file2 */
            while(info2->paths[idx2].path &&
                    (HDstrcmp(obj2_fullpath, info2->paths[idx2].path) != 0))
                idx2++;

            /* Set argdata to pass other args into diff() */
            argdata.type[0] = info1->paths[idx1].type;
            argdata.type[1] = info2->paths[idx2].type;
            argdata.is_same_trgobj = table->objs[i].is_same_trgobj;

            options->cmn_objs = 1;
            if(!g_Parallel)
            {
                nfound += diff(file1_id, obj1_fullpath,
                               file2_id, obj2_fullpath,
                               options, &argdata);
            } /* end if */
#ifdef H5_HAVE_PARALLEL
            else
            {
                int workerFound = 0;

                h5difftrace("Beginning of big else block\n");
                /* We're in parallel mode */
                /* Since the data type of diff value is hsize_t which can
                * be arbitary large such that there is no MPI type that
                * matches it, the value is passed between processes as
                * an array of bytes in order to be portable.  But this
                * may not work in non-homogeneous MPI environments.
                */

                /*Set up args to pass to worker task. */
                if(HDstrlen(obj1_fullpath) > 255 ||
                   HDstrlen(obj2_fullpath) > 255)
                {
                    printf("The parallel diff only supports object names up to 255 characters\n");
                    MPI_Abort(MPI_COMM_WORLD, 0);
                } /* end if */

                /* set args struct to pass */
                HDstrcpy(args.name1, obj1_fullpath);
                HDstrcpy(args.name2, obj2_fullpath);
                args.options = *options;
                args.argdata.type[0] = info1->paths[idx1].type;
                args.argdata.type[1] = info2->paths[idx2].type;
                args.argdata.is_same_trgobj = table->objs[i].is_same_trgobj;

                /* if there are any outstanding print requests, let's handle one. */
                if(busyTasks > 0)
                {
                    int incomingMessage;

                    /* check if any tasks freed up, and didn't need to print. */
                    MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &incomingMessage, &Status);

                    /* first block*/
                    if(incomingMessage)
                    {
                        workerTasks[Status.MPI_SOURCE - 1] = 1;
                        MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                        nfound += nFoundbyWorker.nfound;
                        options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                        busyTasks--;
                    } /* end if */

                    /* check to see if the print token was returned. */
                    if(!havePrintToken)
                    {
                        /* If we don't have the token, someone is probably sending us output */
                        print_incoming_data();

                        /* check incoming queue for token */
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

                        /* incoming token implies free task. */
                        if(incomingMessage) {
                            workerTasks[Status.MPI_SOURCE - 1] = 1;
                            MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                            nfound += nFoundbyWorker.nfound;
                            options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                            busyTasks--;
                            havePrintToken = 1;
                        } /* end if */
                    } /* end if */

                    /* check to see if anyone needs the print token. */
                    if(havePrintToken)
                    {
                        /* check incoming queue for print token requests */
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &incomingMessage, &Status);
                        if(incomingMessage)
                        {
                            MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
                            MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);
                            havePrintToken = 0;
                        } /* end if */
                    } /* end if */
                } /* end if */

                /* check array of tasks to see which ones are free.
                * Manager task never does work, so freeTasks[0] is really
                * worker task 0. */
                for(n = 1; (n < g_nTasks) && !workerFound; n++)
                {
                    if(workerTasks[n-1])
                    {
                        /* send file id's and names to first free worker */
                        MPI_Send(&args, sizeof(args), MPI_BYTE, n, MPI_TAG_ARGS, MPI_COMM_WORLD);

                        /* increment counter for total number of prints. */
                        busyTasks++;

                        /* mark worker as busy */
                        workerTasks[n - 1] = 0;
                        workerFound = 1;
                    } /* end if */
                } /* end for */

                if(!workerFound)
                {
                    /* if they were all busy, we've got to wait for one free up
                     *  before we can move on.  If we don't have the token, some
                     * task is currently printing so we'll wait for that task to
                     * return it.
                     */

                    if(!havePrintToken)
                    {
                        while(!havePrintToken)
                        {
                            int incomingMessage;

                            print_incoming_data();
                            MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);
                            if(incomingMessage)
                            {
                                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                                havePrintToken = 1;
                                nfound += nFoundbyWorker.nfound;
                                options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                                /* send this task the work unit. */
                                MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
                            } /* end if */
                        } /* end while */
                    } /* end if */
                    /* if we do have the token, check for task to free up, or wait for a task to request it */
                    else
                    {
                        /* But first print all the data in our incoming queue */
                        print_incoming_data();
                        MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
                        if(Status.MPI_TAG == MPI_TAG_DONE)
                        {
                            MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                            nfound += nFoundbyWorker.nfound;
                            options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                            MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
                        } /* end if */
                        else if(Status.MPI_TAG == MPI_TAG_TOK_REQUEST)
                        {
                            int incomingMessage;

                            MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
                            MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);

                            do
                            {
                                MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

                                print_incoming_data();
                            } while(!incomingMessage);

                            MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                            nfound += nFoundbyWorker.nfound;
                            options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                            MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
                        } /* end else-if */
                        else
                        {
                            printf("ERROR: Invalid tag (%d) received \n", Status.MPI_TAG);
                            MPI_Abort(MPI_COMM_WORLD, 0);
                            MPI_Finalize();
                        } /* end else */
                    } /* end else */
                } /* end if */
            } /* end else */
#endif /* H5_HAVE_PARALLEL */
            if(obj1_fullpath)
                HDfree(obj1_fullpath);
            if(obj2_fullpath)
                HDfree(obj2_fullpath);
        } /* end if */
    } /* end for */
    h5difftrace("done with for loop\n");

#ifdef H5_HAVE_PARALLEL
    if(g_Parallel)
    {
        /* make sure all tasks are done */
        while(busyTasks > 0)
        {
            MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
            if(Status.MPI_TAG == MPI_TAG_DONE)
            {
                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                nfound += nFoundbyWorker.nfound;
                options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                busyTasks--;
            } /* end if */
            else if(Status.MPI_TAG == MPI_TAG_TOK_REQUEST)
            {
                MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
                if(havePrintToken)
                {
                    int incomingMessage;

                    MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);

                    do {
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

                        print_incoming_data();
                    } while(!incomingMessage);

                    MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                    nfound += nFoundbyWorker.nfound;
                    options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                    busyTasks--;
                } /* end if */
                /* someone else must have it...wait for them to return it, then give it to the task that just asked for it. */
                else
                {
                    int source = Status.MPI_SOURCE;
                    int incomingMessage;

                    do
                    {
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

                        print_incoming_data();
                    } while(!incomingMessage);


                    MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                    nfound += nFoundbyWorker.nfound;
                    options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                    busyTasks--;
                    MPI_Send(NULL, 0, MPI_BYTE, source, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);
                } /* end else */
            } /* end else-if */
            else if(Status.MPI_TAG == MPI_TAG_TOK_RETURN)
            {
                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                nfound += nFoundbyWorker.nfound;
                options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                busyTasks--;
                havePrintToken = 1;
            } /* end else-if */
            else if(Status.MPI_TAG == MPI_TAG_PRINT_DATA)
            {
                char  data[PRINT_DATA_MAX_SIZE + 1];
                HDmemset(data, 0, PRINT_DATA_MAX_SIZE + 1);

                MPI_Recv(data, PRINT_DATA_MAX_SIZE, MPI_CHAR, Status.MPI_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &Status);

                printf("%s", data);
            } /* end else-if */
            else
            {
                printf("ph5diff-manager: ERROR!! Invalid tag (%d) received \n", Status.MPI_TAG);
                MPI_Abort(MPI_COMM_WORLD, 0);
            } /* end else */
        } /* end while */

        for(i = 1; i < g_nTasks; i++)
            MPI_Send(NULL, 0, MPI_BYTE, i, MPI_TAG_END, MPI_COMM_WORLD);

        /* Print any final data waiting in our queue */
        print_incoming_data();
    } /* end if */
    h5difftrace("done with if block\n");

    HDfree(workerTasks);
    }
#endif /* H5_HAVE_PARALLEL */

out:
    /* free table */
    if (table)
        trav_table_free(table);
    h5difftrace("diff_match finish\n");

    return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose: switch between types and choose the diff function
 * TYPE is either
 *  H5G_GROUP         Object is a group
 *  H5G_DATASET       Object is a dataset
 *  H5G_TYPE          Object is a named data type
 *  H5G_LINK          Object is a symbolic link
 *
 * Return: Number of differences found
 *
 * Programmer: Jonathan Kim
 *  - Move follow symlinks code toward top. (March 2812)
 *  - Add following symlinks feature (Feb 11,2010)
 *  - Change to use diff_args_t to pass the rest of args.
 *    Passing through it instead of individual args provides smoother
 *    extensibility through its members along with MPI code update for ph5diff
 *    as it doesn't require interface change.
 *    (May 6,2011)
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 * Date: May 9, 2003
 *-------------------------------------------------------------------------
 */

hsize_t diff(hid_t file1_id,
              const char *path1,
              hid_t file2_id,
              const char *path2,
              diff_opt_t * options,
              diff_args_t *argdata)
{
    hid_t   dset1_id = (-1);
    hid_t   dset2_id = (-1);
    hid_t   type1_id = (-1);
    hid_t   type2_id = (-1);
    hid_t   grp1_id = (-1);
    hid_t   grp2_id = (-1);
    int     ret;
    hbool_t     is_dangle_link1 = FALSE;
    hbool_t     is_dangle_link2 = FALSE;
    hbool_t     is_hard_link = FALSE;
    hsize_t nfound = 0;
    h5trav_type_t object_type;

    /* to get link info */
    h5tool_link_info_t linkinfo1;
    h5tool_link_info_t linkinfo2;

    h5difftrace("diff start\n");

    /*init link info struct */
    HDmemset(&linkinfo1,0,sizeof(h5tool_link_info_t));
    HDmemset(&linkinfo2,0,sizeof(h5tool_link_info_t));

    /* pass how to handle printing warnings to linkinfo option */
    if(print_warn(options))
        linkinfo1.opt.msg_mode = linkinfo2.opt.msg_mode = 1;

    /* for symbolic links, take care follow symlink and no dangling link
     * options */
    if (argdata->type[0] == H5TRAV_TYPE_LINK ||
        argdata->type[0] == H5TRAV_TYPE_UDLINK ||
        argdata->type[1] == H5TRAV_TYPE_LINK ||
        argdata->type[1] == H5TRAV_TYPE_UDLINK )
    {
        /*
         * check dangling links for path1 and path2
         */

        /* target object1 - get type and name */
        ret = H5tools_get_symlink_info(file1_id, path1, &linkinfo1, options->follow_links);
        /* dangling link */
        if (ret == 0)
        {
            if (options->no_dangle_links)
            {
                /* dangling link is error */
                if(options->m_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", path1);
                goto out;
            }
            else
                is_dangle_link1 = TRUE;
        }
        else if (ret < 0)
            goto out;

        /* target object2 - get type and name */
        ret = H5tools_get_symlink_info(file2_id, path2, &linkinfo2, options->follow_links );
        /* dangling link */
        if (ret == 0)
        {
            if (options->no_dangle_links)
            {
                /* dangling link is error */
                if(options->m_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", path2);
                goto out;
            }
            else
                is_dangle_link2 = TRUE;
        }
        else if (ret < 0)
            goto out;

        /* found dangling link */
        if (is_dangle_link1 || is_dangle_link2)
            goto out2;

        /* follow symbolic link option */
        if (options->follow_links)
        {
            if (linkinfo1.linfo.type == H5L_TYPE_SOFT ||
                    linkinfo1.linfo.type == H5L_TYPE_EXTERNAL)
                argdata->type[0] = (h5trav_type_t)linkinfo1.trg_type;

            if (linkinfo2.linfo.type == H5L_TYPE_SOFT ||
                    linkinfo2.linfo.type == H5L_TYPE_EXTERNAL)
                argdata->type[1] = (h5trav_type_t)linkinfo2.trg_type;
        }
    }
    /* if objects are not the same type */
    if (argdata->type[0] != argdata->type[1])
    {
        if (options->m_verbose||options->m_list_not_cmp)
        {
            parallel_print("Not comparable: <%s> is of type %s and <%s> is of type %s\n",
            path1, get_type(argdata->type[0]),
            path2, get_type(argdata->type[1]));
        }
        options->not_cmp=1;
        /* TODO: will need to update non-comparable is different
         * options->contents = 0;
         */
        goto out2;
    }
    else /* now both object types are same */
        object_type = argdata->type[0];

    /*
     * If both points to the same target object, skip comparing details inside
     * of the objects to improve performance.
     * Always check for the hard links, otherwise if follow symlink option is
     * specified.
     *
     * Perform this to match the outputs as bypassing.
     */
     if (argdata->is_same_trgobj)
     {
        h5difftrace("argdata->is_same_trgobj\n");
        is_hard_link = (object_type == H5TRAV_TYPE_DATASET ||
                        object_type == H5TRAV_TYPE_NAMED_DATATYPE ||
                        object_type == H5TRAV_TYPE_GROUP);
        if (options->follow_links || is_hard_link)
        {
            /* print information is only verbose option is used */
            if(options->m_verbose || options->m_report)
            {
                switch(object_type)
                {
                    case H5TRAV_TYPE_DATASET:
                        do_print_objname("dataset", path1, path2, options);
                        break;
                    case H5TRAV_TYPE_NAMED_DATATYPE:
                        do_print_objname("datatype", path1, path2, options);
                        break;
                    case H5TRAV_TYPE_GROUP:
                        do_print_objname("group", path1, path2, options);
                        break;
                    case H5TRAV_TYPE_LINK:
                        do_print_objname("link", path1, path2, options);
                        break;
                    case H5TRAV_TYPE_UDLINK:
                        if(linkinfo1.linfo.type == H5L_TYPE_EXTERNAL && linkinfo2.linfo.type == H5L_TYPE_EXTERNAL)
                            do_print_objname("external link", path1, path2, options);
                        else
                            do_print_objname ("user defined link", path1, path2, options);
                        break;
                    case H5TRAV_TYPE_UNKNOWN:
                    default:
                        parallel_print("Comparison not supported: <%s> and <%s> are of type %s\n",
                            path1, path2, get_type(object_type) );
                        options->not_cmp = 1;
                        break;
                } /* switch(type)*/

                print_found(nfound);
            } /* if(options->m_verbose || options->m_report) */

            /* exact same, so comparison is done */
            goto out2;
        }
    }

    switch(object_type)
    {
       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_DATASET
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_DATASET:
            if((dset1_id = H5Dopen2(file1_id, path1, H5P_DEFAULT)) < 0)
                goto out;
            if((dset2_id = H5Dopen2(file2_id, path2, H5P_DEFAULT)) < 0)
                goto out;
      /* verbose (-v) and report (-r) mode */
            if(options->m_verbose || options->m_report)
            {
                do_print_objname("dataset", path1, path2, options);
                nfound = diff_dataset(file1_id, file2_id, path1, path2, options);
                print_found(nfound);
            }
            /* quiet mode (-q), just count differences */
            else if(options->m_quiet)
            {
                nfound = diff_dataset(file1_id, file2_id, path1, path2, options);
            }
      /* the rest (-c, none, ...) */
            else
            {
                nfound = diff_dataset(file1_id, file2_id, path1, path2, options);
                /* print info if difference found  */
                if (nfound)
                {
                    do_print_objname("dataset", path1, path2, options);
                    print_found(nfound);
                }
            }


            /*---------------------------------------------------------
             * compare attributes
             * if condition refers to cases when the dataset is a
             * referenced object
             *---------------------------------------------------------
             */
            if(path1)
                nfound += diff_attr(dset1_id, dset2_id, path1, path2, options);


            if(H5Dclose(dset1_id) < 0)
                goto out;
            if(H5Dclose(dset2_id) < 0)
                goto out;
            break;

       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_NAMED_DATATYPE
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_NAMED_DATATYPE:
            if((type1_id = H5Topen2(file1_id, path1, H5P_DEFAULT)) < 0)
                goto out;
            if((type2_id = H5Topen2(file2_id, path2, H5P_DEFAULT)) < 0)
                goto out;

            if((ret = H5Tequal(type1_id, type2_id)) < 0)
                goto out;

            /* if H5Tequal is > 0 then the datatypes refer to the same datatype */
            nfound = (ret > 0) ? 0 : 1;

            if(print_objname(options,nfound))
                do_print_objname("datatype", path1, path2, options);

            /* always print the number of differences found in verbose mode */
            if(options->m_verbose)
                print_found(nfound);

            /*-----------------------------------------------------------------
             * compare attributes
             * the if condition refers to cases when the dataset is a
             * referenced object
             *-----------------------------------------------------------------
             */
            if(path1)
                nfound += diff_attr(type1_id, type2_id, path1, path2, options);

            if(H5Tclose(type1_id) < 0)
                goto out;
            if(H5Tclose(type2_id) < 0)
                goto out;
            break;

       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_GROUP
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_GROUP:
            if(print_objname(options, nfound))
                do_print_objname("group", path1, path2, options);

            /* always print the number of differences found in verbose mode */
            if(options->m_verbose)
                print_found(nfound);

            if((grp1_id = H5Gopen2(file1_id, path1, H5P_DEFAULT)) < 0)
                goto out;
            if((grp2_id = H5Gopen2(file2_id, path2, H5P_DEFAULT)) < 0)
                goto out;

            /*-----------------------------------------------------------------
             * compare attributes
             * the if condition refers to cases when the dataset is a
             * referenced object
             *-----------------------------------------------------------------
             */
            if(path1)
                nfound += diff_attr(grp1_id, grp2_id, path1, path2, options);

            if(H5Gclose(grp1_id) < 0)
                goto out;
            if(H5Gclose(grp2_id) < 0)
                goto out;
            break;


       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_LINK
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_LINK:
            {
            ret = HDstrcmp(linkinfo1.trg_path, linkinfo2.trg_path);

            /* if the target link name is not same then the links are "different" */
            nfound = (ret != 0) ? 1 : 0;

            if(print_objname(options, nfound))
                do_print_objname("link", path1, path2, options);

            /* always print the number of differences found in verbose mode */
            if(options->m_verbose)
                print_found(nfound);

            }
            break;

       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_UDLINK
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_UDLINK:
            {
            /* Only external links will have a query function registered */
            if(linkinfo1.linfo.type == H5L_TYPE_EXTERNAL && linkinfo2.linfo.type == H5L_TYPE_EXTERNAL)
            {
                /* If the buffers are the same size, compare them */
                if(linkinfo1.linfo.u.val_size == linkinfo2.linfo.u.val_size)
                {
                    ret = HDmemcmp(linkinfo1.trg_path, linkinfo2.trg_path, linkinfo1.linfo.u.val_size);
                }
                else
                    ret = 1;

                /* if "linkinfo1.trg_path" != "linkinfo2.trg_path" then the links
                 * are "different" extlinkinfo#.path is combination string of
                 * file_name and obj_name
                 */
                nfound = (ret != 0) ? 1 : 0;

                if(print_objname(options, nfound))
                    do_print_objname("external link", path1, path2, options);

            } /* end if */
            else
            {
                /* If one or both of these links isn't an external link, we can only
                 * compare information from H5Lget_info since we don't have a query
                 * function registered for them.
                 *
                 * If the link classes or the buffer length are not the
                 * same, the links are "different"
                 */
                if((linkinfo1.linfo.type != linkinfo2.linfo.type) ||
                   (linkinfo1.linfo.u.val_size != linkinfo2.linfo.u.val_size))
                    nfound = 1;
                else
                    nfound = 0;

                if (print_objname (options, nfound))
                    do_print_objname ("user defined link", path1, path2, options);
            } /* end else */

            /* always print the number of differences found in verbose mode */
            if(options->m_verbose)
                print_found(nfound);
            }
            break;

        case H5TRAV_TYPE_UNKNOWN:
        default:
            if(options->m_verbose)
                parallel_print("Comparison not supported: <%s> and <%s> are of type %s\n",
                    path1, path2, get_type(object_type) );
            options->not_cmp = 1;
            break;
     }

    /* free link info buffer */
    if (linkinfo1.trg_path)
        HDfree(linkinfo1.trg_path);
    if (linkinfo2.trg_path)
        HDfree(linkinfo2.trg_path);

    return nfound;

out:
    options->err_stat = 1;

out2:
    /*-----------------------------------
     * handle dangling link(s)
     */
    /* both path1 and path2 are dangling links */
    if(is_dangle_link1 && is_dangle_link2)
    {
        if(print_objname(options, nfound))
        {
            do_print_objname("dangling link", path1, path2, options);
            print_found(nfound);
        }
    }
    /* path1 is dangling link */
    else if (is_dangle_link1)
    {
        if(options->m_verbose)
           parallel_print("obj1 <%s> is a dangling link.\n", path1);
        nfound++;
        if(print_objname(options, nfound))
            print_found(nfound);
    }
    /* path2 is dangling link */
    else if (is_dangle_link2)
    {
        if(options->m_verbose)
            parallel_print("obj2 <%s> is a dangling link.\n", path2);
        nfound++;
        if(print_objname(options, nfound))
            print_found(nfound);
    }

    /* free link info buffer */
    if (linkinfo1.trg_path)
        HDfree(linkinfo1.trg_path);
    if (linkinfo2.trg_path)
        HDfree(linkinfo2.trg_path);

    /* close */
    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Tclose(type1_id);
        H5Tclose(type2_id);
        H5Gclose(grp1_id);
        H5Tclose(grp2_id);
        /* enable error reporting */
    } H5E_END_TRY;
    h5difftrace("diff finish\n");

    return nfound;
}

