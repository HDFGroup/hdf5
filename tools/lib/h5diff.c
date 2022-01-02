/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5diff.h"
#include "ph5diff.h"

#define DIFF_COUNT 128
int diff_count_limit = DIFF_COUNT;
int rank_diff_count  = 0;

diff_instance_t **rank_diffs         = NULL;
diff_instance_t * my_diffs           = NULL;
diff_instance_t * current_diff       = NULL;
hsize_t *         local_diff_offsets = NULL;
hsize_t *         local_diff_lengths = NULL;
hsize_t           local_diff_total   = 0;

static int h5diff_open(const char *fname1, const char *fname2, diff_opt_t *opts, hid_t *_file_id1,
                       hid_t *_file_id2);
static int h5diff_initialize_lists(const char *fname1, const char *fname2, const char *objname1,
                                   const char *objname2, hid_t file1_id, hid_t file2_id, diff_opt_t *opts,
                                   char **_obj1fullname, trav_info_t **_info1_lp, char **_obj2fullname,
                                   trav_info_t **_info2_lp, int *_both_objs_grp, trav_table_t **_match_list);

static herr_t trav_grp_objs(const char *path, const H5O_info2_t *oinfo, const char *already_visited,
                            void *udata);
static herr_t trav_grp_symlinks(const char *path, const H5L_info2_t *linfo, void *udata);
static int    check_dataset_sizes(hid_t file_id, trav_table_t *match_list);
static void   ph5diff_gather_diffs(void);

static hsize_t hyperslab_pdiff(hid_t file1_id, const char *path1, hid_t file2_id, const char *path2,
                               diff_opt_t *opts, diff_args_t *argdata);

static hsize_t pdiff_match(hid_t file1_id, const char *grp1, trav_info_t *info1, hid_t file2_id,
                           const char *grp2, trav_info_t *info2, trav_table_t *table, diff_opt_t *opts);

/*-------------------------------------------------------------------------
 * Function: print_objname
 *
 * Purpose:  check if object name is to be printed, only when:
 *             1) verbose mode
 *             2) when diff was found (normal mode)
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE int
print_objname(diff_opt_t *opts, hsize_t nfound)
{
    return ((opts->mode_verbose || nfound) && !opts->mode_quiet) ? 1 : 0;
}

/*-------------------------------------------------------------------------
 * Function: do_print_objname
 *
 * Purpose:  print object name
 *-------------------------------------------------------------------------
 */
void
do_print_objname(const char *OBJ, const char *path1, const char *path2, diff_opt_t *opts)
{
    /* if verbose level is higher than 0, put space line before
     * displaying any object or symbolic links. This improves
     * readability of the output.
     */
    if (opts->mode_verbose_level >= 1)
        parallel_print("\n");
    parallel_print("%-7s: <%s> and <%s>\n", OBJ, path1, path2);
}

/*-------------------------------------------------------------------------
 * Function: do_print_attrname
 *
 * Purpose:  print attribute name
 *-------------------------------------------------------------------------
 */
void
do_print_attrname(const char *attr, const char *path1, const char *path2)
{
    parallel_print("%-7s: <%s> and <%s>\n", attr, path1, path2);
}

/*-------------------------------------------------------------------------
 * Function: print_warn
 *
 * Purpose:  check print warning condition.
 * Return:
 *           1 if verbose mode
 *           0 if not verbos mode
 *-------------------------------------------------------------------------
 */
static int
print_warn(diff_opt_t *opts)
{
    return ((opts->mode_verbose)) ? 1 : 0;
}

#if defined(H5_HAVE_PARALLEL) && defined(USE_ORIGINAL_CODE)
/*-------------------------------------------------------------------------
 * Function: phdiff_dismiss_workers
 *
 * Purpose:  tell all workers to end.
 *
 * Return:   none
 *-------------------------------------------------------------------------
 */
void
phdiff_dismiss_workers(void)
{
    int i;
    for (i = 1; i < g_nTasks; i++)
        MPI_Send(NULL, 0, MPI_BYTE, i, MPI_TAG_END, MPI_COMM_WORLD);
}

/*-------------------------------------------------------------------------
 * Function: print_incoming_data
 *
 * Purpose:  special function that prints any output that has been sent to the manager
 *           and is currently sitting in the incoming message queue
 *
 * Return:   none
 *-------------------------------------------------------------------------
 */

static void
print_incoming_data(void)
{
    char       data[PRINT_DATA_MAX_SIZE + 1];
    int        incomingMessage;
    MPI_Status Status;

    do {
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &incomingMessage, &Status);
        if (incomingMessage) {
            HDmemset(data, 0, PRINT_DATA_MAX_SIZE + 1);
            MPI_Recv(data, PRINT_DATA_MAX_SIZE, MPI_CHAR, Status.MPI_SOURCE, MPI_TAG_PRINT_DATA,
                     MPI_COMM_WORLD, &Status);

            HDprintf("%s", data);
        }
    } while (incomingMessage);
}
#endif /* USE_ORIGINAL_CODE */

/*-------------------------------------------------------------------------
 * Function: is_valid_options
 *
 * Purpose:  check if options are valid
 *
 * Return:
 *           1 : Valid
 *           0 : Not valid
 *------------------------------------------------------------------------*/
static int
is_valid_options(diff_opt_t *opts)
{
    int ret_value = 1; /* init to valid */

    /*-----------------------------------------------
     * no -q(quiet) with -v (verbose) or -r (report) */
    if (opts->mode_quiet && (opts->mode_verbose || opts->mode_report)) {
#ifdef H5_HAVE_PARALLEL
        /* Allow MPI RANK(0) to print. */
        if (g_Parallel && (g_nID == 0)) {
            g_Parallel = 0;
        }
#endif
        parallel_print("Error: -q (quiet mode) cannot be added to verbose or report modes\n");
        opts->err_stat = H5DIFF_ERR;
        H5TOOLS_GOTO_DONE(0);
    }

    /* -------------------------------------------------------
     * only allow --no-dangling-links along with --follow-symlinks */
    if (opts->no_dangle_links && !opts->follow_links) {
#ifdef H5_HAVE_PARALLEL
        /* Allow MPI RANK(0) to print. */
        if (g_Parallel && (g_nID == 0)) {
            g_Parallel = 0;
        }
#endif
        parallel_print("Error: --no-dangling-links must be used along with --follow-symlinks option.\n");
        opts->err_stat = H5DIFF_ERR;
        H5TOOLS_GOTO_DONE(0);
    }

done:
    return ret_value;
} /* is_valid_options() */

/*-------------------------------------------------------------------------
 * Function: is_exclude_path
 *
 * Purpose:  check if 'paths' are part of exclude path list
 *
 * Return:
 *           1 - excluded path
 *           0 - not excluded path
 *------------------------------------------------------------------------*/
static int
is_exclude_path(char *path, h5trav_type_t type, diff_opt_t *opts)
{
    struct exclude_path_list *exclude_path_ptr;
    int                       ret_cmp;
    int                       ret_value = 0;

    /* check if exclude path option is given */
    if (!opts->exclude_path)
        H5TOOLS_GOTO_DONE(0);

    /* assign to local exclude list pointer */
    exclude_path_ptr = opts->exclude;

    /* search objects in exclude list */
    while (NULL != exclude_path_ptr) {
        /* if exclude path is is group, exclude its members as well */
        if (exclude_path_ptr->obj_type == H5TRAV_TYPE_GROUP) {
            ret_cmp = HDstrncmp(exclude_path_ptr->obj_path, path, HDstrlen(exclude_path_ptr->obj_path));
            if (ret_cmp == 0) { /* found matching members */
                size_t len_grp;

                /* check if given path belong to an excluding group, if so
                 * exclude it as well.
                 * This verifies if “/grp1/dset1” is only under “/grp1”, but
                 * not under “/grp1xxx/” group.
                 */
                len_grp = HDstrlen(exclude_path_ptr->obj_path);
                if (path[len_grp] == '/') {
                    /* belong to excluded group! */
                    ret_value = 1;
                    break; /* while */
                }
            }
        }
        /* exclude target is not group, just exclude the object */
        else {
            ret_cmp = HDstrcmp(exclude_path_ptr->obj_path, path);
            if (ret_cmp == 0) { /* found matching object */
                /* excluded non-group object */
                ret_value = 1;
                /* remember the type of this matching object.
                 * if it's group, it can be used for excluding its member
                 * objects in this while() loop */
                exclude_path_ptr->obj_type = type;
                break; /* while */
            }
        }
        exclude_path_ptr = exclude_path_ptr->next;
    }

done:
    return ret_value;
} /* is_exclude_path */

/*-------------------------------------------------------------------------
 * Function: is_exclude_attr
 *
 * Purpose:  check if 'paths' are part of exclude path list
 *
 * Return:
 *           1 - excluded path
 *           0 - not excluded path
 *------------------------------------------------------------------------*/
static int
is_exclude_attr(const char *path, h5trav_type_t type, diff_opt_t *opts)
{
    struct exclude_path_list *exclude_ptr;
    int                       ret_cmp;
    int                       ret_value = 0;

    /* check if exclude attr option is given */
    if (!opts->exclude_attr_path)
        H5TOOLS_GOTO_DONE(0);

    /* assign to local exclude list pointer */
    exclude_ptr = opts->exclude_attr;

    /* search objects in exclude list */
    while (NULL != exclude_ptr) {
        /* if exclude path is is group, exclude its members as well */
        if (exclude_ptr->obj_type == H5TRAV_TYPE_GROUP) {
            ret_cmp = HDstrncmp(exclude_ptr->obj_path, path, HDstrlen(exclude_ptr->obj_path));
            if (ret_cmp == 0) { /* found matching members */
                size_t len_grp;

                /* check if given path belong to an excluding group, if so
                 * exclude it as well.
                 * This verifies if “/grp1/dset1” is only under “/grp1”, but
                 * not under “/grp1xxx/” group.
                 */
                len_grp = HDstrlen(exclude_ptr->obj_path);
                if (path[len_grp] == '/') {
                    /* belong to excluded group! */
                    ret_value = 1;
                    break; /* while */
                }
            }
        }
        /* exclude target is not group, just exclude the object */
        else {
            ret_cmp = HDstrcmp(exclude_ptr->obj_path, path);
            if (ret_cmp == 0) { /* found matching object */
                /* excluded non-group object */
                ret_value = 1;
                /* remember the type of this matching object.
                 * if it's group, it can be used for excluding its member
                 * objects in this while() loop */
                exclude_ptr->obj_type = type;
                break; /* while */
            }
        }
        exclude_ptr = exclude_ptr->next;
    }

done:
    return ret_value;
} /* is_exclude_attr() */

/*-------------------------------------------------------------------------
 * Function: free_exclude_path_list
 *
 * Purpose:  free exclude object list from diff options
 *------------------------------------------------------------------------*/
static void
free_exclude_path_list(diff_opt_t *opts)
{
    struct exclude_path_list *curr = opts->exclude;
    struct exclude_path_list *next;

    while (NULL != curr) {
        next = curr->next;
        HDfree(curr);
        curr = next;
    }
} /* free_exclude_path_list() */

/*-------------------------------------------------------------------------
 * Function: free_exclude_attr_list
 *
 * Purpose:  free exclude object attribute list from diff options
 *------------------------------------------------------------------------*/
static void
free_exclude_attr_list(diff_opt_t *opts)
{
    struct exclude_path_list *curr = opts->exclude_attr;
    struct exclude_path_list *next;

    while (NULL != curr) {
        next = curr->next;
        HDfree(curr);
        curr = next;
    }
} /* free_exclude_attr_list() */

/*-------------------------------------------------------------------------
 * Function: build_match_list
 *
 * Purpose:  get list of matching path_name from info1 and info2
 *
 * Note:
 *           Find common objects; the algorithm used for this search is the
 *           cosequential match algorithm and is described in
 *           Folk, Michael; Zoellick, Bill. (1992). File Structures. Addison-Wesley.
 *           Moved out from diff_match() to make code more flexible.
 *
 * Parameter:
 *           table_out [OUT] : return the list
 *------------------------------------------------------------------------*/
static void
build_match_list(const char *objname1, trav_info_t *info1, const char *objname2, trav_info_t *info2,
                 trav_table_t **table_out, diff_opt_t *opts)
{
    size_t        curr1 = 0;
    size_t        curr2 = 0;
    unsigned      infile[2];
    char *        path1_lp = NULL;
    char *        path2_lp = NULL;
    h5trav_type_t type1_l;
    h5trav_type_t type2_l;
    size_t        path1_offset = 0;
    size_t        path2_offset = 0;
    int           cmp;
    trav_table_t *table = NULL;
    size_t        idx;

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);
    /* init */
    trav_table_init(info1->fid, &table);
    if (table == NULL) {
        H5TOOLS_INFO("Cannot create traverse table");
        H5TOOLS_GOTO_DONE_NO_RET();
    }

    /*
     * This is necessary for the case that given objects are group and
     * have different names (ex: obj1 is /grp1 and obj2 is /grp5).
     * All the objects belong to given groups are the candidates.
     * So prepare to compare paths without the group names.
     */
    H5TOOLS_DEBUG("objname1 = %s objname2 = %s ", objname1, objname2);

    /* if obj1 is not root */
    if (HDstrcmp(objname1, "/") != 0)
        path1_offset = HDstrlen(objname1);
    /* if obj2 is not root */
    if (HDstrcmp(objname2, "/") != 0)
        path2_offset = HDstrlen(objname2);

    /*--------------------------------------------------
     * build the list
     */
    while (curr1 < info1->nused && curr2 < info2->nused) {
        path1_lp = (info1->paths[curr1].path) + path1_offset;
        path2_lp = (info2->paths[curr2].path) + path2_offset;
        type1_l  = info1->paths[curr1].type;
        type2_l  = info2->paths[curr2].type;

        /* criteria is string compare */
        cmp = HDstrcmp(path1_lp, path2_lp);
        if (cmp == 0) {
            if (!is_exclude_path(path1_lp, type1_l, opts)) {
                infile[0] = 1;
                infile[1] = 1;
                trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
                /* if the two point to the same target object,
                 * mark that in table */
                if (info1->paths[curr1].fileno == info2->paths[curr2].fileno) {
                    int token_cmp;

                    if (H5Otoken_cmp(info1->fid, &info1->paths[curr1].obj_token,
                                     &info2->paths[curr2].obj_token, &token_cmp) < 0) {
                        H5TOOLS_INFO("Failed to compare object tokens");
                        opts->err_stat = H5DIFF_ERR;
                        H5TOOLS_GOTO_DONE_NO_RET();
                    }

                    if (!token_cmp) {
                        idx                             = table->nobjs - 1;
                        table->objs[idx].is_same_trgobj = 1;
                    }
                }
            }
            curr1++;
            curr2++;
        } /* end if */
        else if (cmp < 0) {
            if (!is_exclude_path(path1_lp, type1_l, opts)) {
                infile[0] = 1;
                infile[1] = 0;
                trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
            }
            curr1++;
        } /* end else-if */
        else {
            if (!is_exclude_path(path2_lp, type2_l, opts)) {
                infile[0] = 0;
                infile[1] = 1;
                trav_table_addflags(infile, path2_lp, info2->paths[curr2].type, table);
            }
            curr2++;
        } /* end else */
    }     /* end while */

    /* list1 did not end */
    infile[0] = 1;
    infile[1] = 0;
    while (curr1 < info1->nused) {
        path1_lp = (info1->paths[curr1].path) + path1_offset;
        type1_l  = info1->paths[curr1].type;

        if (!is_exclude_path(path1_lp, type1_l, opts)) {
            trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
        }
        curr1++;
    } /* end while */

    /* list2 did not end */
    infile[0] = 0;
    infile[1] = 1;
    while (curr2 < info2->nused) {
        path2_lp = (info2->paths[curr2].path) + path2_offset;
        type2_l  = info2->paths[curr2].type;

        if (!is_exclude_path(path2_lp, type2_l, opts)) {
            trav_table_addflags(infile, path2_lp, info2->paths[curr2].type, table);
        }
        curr2++;
    } /* end while */

    free_exclude_path_list(opts);

done:
    *table_out = table;

    H5TOOLS_ENDDEBUG(" ");
} /* build_match_list() */

static int
h5diff_initialize_lists(const char *fname1, const char *fname2, const char *objname1, const char *objname2,
                        hid_t file1_id, hid_t file2_id, diff_opt_t *opts, char **_obj1fullname,
                        trav_info_t **_info1_lp, char **_obj2fullname, trav_info_t **_info2_lp,
                        int *_both_objs_grp, trav_table_t **_match_list)
{
    trav_info_t *info1_obj    = NULL;
    trav_info_t *info2_obj    = NULL;
    char *       obj1fullname = NULL;
    char *       obj2fullname = NULL;

    h5trav_type_t obj1type = H5TRAV_TYPE_GROUP;
    h5trav_type_t obj2type = H5TRAV_TYPE_GROUP;

    /* object info */
    H5O_info2_t oinfo1;
    H5O_info2_t oinfo2;

    /* for group object */
    trav_info_t *info1_grp = NULL;
    trav_info_t *info2_grp = NULL;

    /* local pointer */
    trav_info_t *info1_lp = NULL;
    trav_info_t *info2_lp = NULL;

    /* link info from specified object */
    H5L_info2_t src_linfo1;
    H5L_info2_t src_linfo2;

    /* local return checks */
    int l_ret1 = -1;
    int l_ret2 = -1;

    /* link info from member object */
    h5tool_link_info_t trg_linfo1;
    h5tool_link_info_t trg_linfo2;

    /* list for common objects */
    trav_table_t *match_list = NULL;
    diff_err_t    ret_value  = H5DIFF_NO_ERR;

    /* flag the same object optimization */
    int both_objs_grp = 0;

    /* flag errors */
    int errors = 0;
    /* count/flag */
    int nfound = 0;

    /* init link info struct */
    HDmemset(&trg_linfo1, 0, sizeof(h5tool_link_info_t));
    HDmemset(&trg_linfo2, 0, sizeof(h5tool_link_info_t));

    /*-------------------------------------------------------------------------
     * Initialize the info structs
     *-------------------------------------------------------------------------
     */
    trav_info_init(fname1, file1_id, &info1_obj);
    trav_info_init(fname2, file2_id, &info2_obj);

    H5TOOLS_DEBUG("trav_info_init initialized");
    /* if any object is specified */
    if (objname1) {
        /* make the given object1 fullpath, start with "/"  */
        if (HDstrncmp(objname1, "/", 1) != 0) {
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if (HDasprintf(&obj1fullname, "/%s", objname1) < 0) {
                errors++;
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
#else  /* H5_HAVE_ASPRINTF */
            /* (malloc 2 more for "/" and end-of-line) */
            if ((obj1fullname = (char *)HDmalloc(HDstrlen(objname1) + 2)) == NULL) {
                errors++;
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
            HDstrcpy(obj1fullname, "/");
            HDstrcat(obj1fullname, objname1);
#endif /* H5_HAVE_ASPRINTF */
        }
        else
            obj1fullname = HDstrdup(objname1);
        H5TOOLS_DEBUG("obj1fullname = %s", obj1fullname);

        /* make the given object2 fullpath, start with "/" */
        if (HDstrncmp(objname2, "/", 1) != 0) {
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if (HDasprintf(&obj2fullname, "/%s", objname2) < 0) {
                errors++;
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
#else  /* H5_HAVE_ASPRINTF */
            /* (malloc 2 more for "/" and end-of-line) */
            if ((obj2fullname = (char *)HDmalloc(HDstrlen(objname2) + 2)) == NULL) {
                errors++;
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
            HDstrcpy(obj2fullname, "/");
            HDstrcat(obj2fullname, objname2);
#endif /* H5_HAVE_ASPRINTF */
        }
        else
            obj2fullname = HDstrdup(objname2);
        H5TOOLS_DEBUG("obj2fullname = %s", obj2fullname);

        /*----------------------------------------------------------
         * check if obj1 is root, group, single object or symlink
         */
        H5TOOLS_DEBUG("h5diff check if obj1=%s is root, group, single object or symlink", obj1fullname);
        if (!HDstrcmp(obj1fullname, "/")) {
            obj1type = H5TRAV_TYPE_GROUP;
        }
        else {
            /* check if link itself exist */
            if (H5Lexists(file1_id, obj1fullname, H5P_DEFAULT) <= 0) {
                errors++;
#ifdef H5_HAVE_PARALLEL
                unsigned char was_Parallel = g_Parallel;
                if (g_Parallel && (g_nID == 0))
                    g_Parallel = 0;
#endif
                parallel_print("Object <%s> could not be found in <%s>\n", obj1fullname, fname1);
#ifdef H5_HAVE_PARALLEL
                g_Parallel = was_Parallel;
#endif
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Error: Object could not be found");
            }
            /* get info from link */
            if (H5Lget_info2(file1_id, obj1fullname, &src_linfo1, H5P_DEFAULT) < 0) {
                errors++;
#ifdef H5_HAVE_PARALLEL
                unsigned char was_Parallel = g_Parallel;
                if (g_Parallel && (g_nID == 0))
                    g_Parallel = 0;
#endif
                parallel_print("Unable to get link info from <%s>\n", obj1fullname);
#ifdef H5_HAVE_PARALLEL
                g_Parallel = was_Parallel;
#endif
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Lget_info failed");
            }

            info1_lp = info1_obj;

            /*
             * check the type of specified path for hard and symbolic links
             */
            if (src_linfo1.type == H5L_TYPE_HARD) {
                size_t idx;

                /* optional data pass */
                info1_obj->opts = (diff_opt_t *)opts;

                if (H5Oget_info_by_name3(file1_id, obj1fullname, &oinfo1, H5O_INFO_BASIC, H5P_DEFAULT) < 0) {
                    errors++;
                    parallel_print("Error: Could not get file contents\n");
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Error: Could not get file contents");
                }
                obj1type = (h5trav_type_t)oinfo1.type;
                trav_info_add(info1_obj, obj1fullname, obj1type);
                idx = info1_obj->nused - 1;
                HDmemcpy(&info1_obj->paths[idx].obj_token, &oinfo1.token, sizeof(H5O_token_t));
                info1_obj->paths[idx].fileno = oinfo1.fileno;
            }
            else if (src_linfo1.type == H5L_TYPE_SOFT) {
                obj1type = H5TRAV_TYPE_LINK;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
            else if (src_linfo1.type == H5L_TYPE_EXTERNAL) {
                obj1type = H5TRAV_TYPE_UDLINK;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
        }

        /*----------------------------------------------------------
         * check if obj2 is root, group, single object or symlink
         */
        H5TOOLS_DEBUG("h5diff check if obj2=%s is root, group, single object or symlink", obj2fullname);
        if (!HDstrcmp(obj2fullname, "/")) {
            obj2type = H5TRAV_TYPE_GROUP;
        }
        else {
            /* check if link itself exist */
            if (H5Lexists(file2_id, obj2fullname, H5P_DEFAULT) <= 0) {
                errors++;
                parallel_print("Object <%s> could not be found in <%s>\n", obj2fullname, fname2);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Error: Object could not be found");
            }
            /* get info from link */
            if (H5Lget_info2(file2_id, obj2fullname, &src_linfo2, H5P_DEFAULT) < 0) {
                errors++;
                parallel_print("Unable to get link info from <%s>\n", obj2fullname);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Lget_info failed");
            }

            info2_lp = info2_obj;

            /*
             * check the type of specified path for hard and symbolic links
             */
            if (src_linfo2.type == H5L_TYPE_HARD) {
                size_t idx;

                /* optional data pass */
                info2_obj->opts = (diff_opt_t *)opts;

                if (H5Oget_info_by_name3(file2_id, obj2fullname, &oinfo2, H5O_INFO_BASIC, H5P_DEFAULT) < 0) {
                    errors++;
                    parallel_print("Error: Could not get file contents\n");
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Error: Could not get file contents");
                }
                obj2type = (h5trav_type_t)oinfo2.type;
                trav_info_add(info2_obj, obj2fullname, obj2type);
                idx = info2_obj->nused - 1;
                HDmemcpy(&info2_obj->paths[idx].obj_token, &oinfo2.token, sizeof(H5O_token_t));
                info2_obj->paths[idx].fileno = oinfo2.fileno;
            }
            else if (src_linfo2.type == H5L_TYPE_SOFT) {
                obj2type = H5TRAV_TYPE_LINK;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
            else if (src_linfo2.type == H5L_TYPE_EXTERNAL) {
                obj2type = H5TRAV_TYPE_UDLINK;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
        }
    }
    /* if no object specified */
    else {
        H5TOOLS_DEBUG("h5diff no object specified");
        /* set root group */
        obj1fullname = (char *)HDstrdup("/");
        obj1type     = H5TRAV_TYPE_GROUP;
        obj2fullname = (char *)HDstrdup("/");
        obj2type     = H5TRAV_TYPE_GROUP;
    }

    H5TOOLS_DEBUG("get any symbolic links info - errstat:%d", opts->err_stat);

    /* get any symbolic links info */
    l_ret1 = H5tools_get_symlink_info(file1_id, obj1fullname, &trg_linfo1, opts->follow_links);
    l_ret2 = H5tools_get_symlink_info(file2_id, obj2fullname, &trg_linfo2, opts->follow_links);

    /*---------------------------------------------
     * check for following symlinks
     */
    if (opts->follow_links) {
        /* pass how to handle printing warning to linkinfo option */
        if (print_warn(opts))
            trg_linfo1.opt.msg_mode = trg_linfo2.opt.msg_mode = 1;

        /*-------------------------------
         * check symbolic link (object1)
         */
        H5TOOLS_DEBUG("h5diff check symbolic link (object1)");
        /* dangling link */
        if (l_ret1 == 0) {
            H5TOOLS_DEBUG("h5diff ... dangling link");
            if (opts->no_dangle_links) {
                errors++;
                /* treat dangling link as error */
                if (opts->mode_verbose) {
#ifdef H5_HAVE_PARALLEL
                    unsigned char was_Parallel = g_Parallel;
                    if (g_Parallel && (g_nID == 0))
                        g_Parallel = 0;
#endif
                    parallel_print("Warning: <%s> is a dangling link.\n", obj1fullname);
#ifdef H5_HAVE_PARALLEL
                    g_Parallel = was_Parallel;
#endif
                }
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "treat dangling link as error");
            }
            else {
                if (opts->mode_verbose) {
#ifdef H5_HAVE_PARALLEL
                    unsigned char was_Parallel = g_Parallel;
                    if (g_Parallel && (g_nID == 0))
                        g_Parallel = 0;
#endif
                    parallel_print("obj1 <%s> is a dangling link.\n", obj1fullname);
#ifdef H5_HAVE_PARALLEL
                    g_Parallel = was_Parallel;
#endif
                }
                if (l_ret1 != 0 || l_ret2 != 0) {
                    nfound++;
#ifdef H5_HAVE_PARALLEL
                    unsigned char was_Parallel = g_Parallel;
                    if (g_Parallel && (g_nID == 0))
                        g_Parallel = 0;
#endif
                    print_found((hsize_t)nfound);
#ifdef H5_HAVE_PARALLEL
                    g_Parallel = was_Parallel;
                    H5TOOLS_GOTO_DONE(H5PDIFF_NO_ERR);
#else
                    H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
#endif
                }
            }
        }
        else if (l_ret1 < 0) { /* fail */
#ifdef H5_HAVE_PARALLEL
            unsigned char was_Parallel = g_Parallel;
            if (g_Parallel && (g_nID == 0))
                g_Parallel = 0;
#endif
            parallel_print("Object <%s> could not be found in <%s>\n", obj1fullname, fname1);
#ifdef H5_HAVE_PARALLEL
            g_Parallel = was_Parallel;
#endif
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Object could not be found");
        }
        else if (l_ret1 != 2) { /* symbolic link */
            obj1type = (h5trav_type_t)trg_linfo1.trg_type;
            H5TOOLS_DEBUG("h5diff ... ... trg_linfo1.trg_type == H5L_TYPE_HARD");
            if (info1_lp != NULL) {
                size_t idx = info1_lp->nused - 1;

                H5TOOLS_DEBUG("h5diff ... ... ... info1_obj not null");
                HDmemcpy(&info1_lp->paths[idx].obj_token, &trg_linfo1.obj_token, sizeof(H5O_token_t));
                info1_lp->paths[idx].type   = (h5trav_type_t)trg_linfo1.trg_type;
                info1_lp->paths[idx].fileno = trg_linfo1.fileno;
            }
            H5TOOLS_DEBUG("h5diff check symbolic link (object1) finished");
        }

        /*-------------------------------
         * check symbolic link (object2)
         */
        H5TOOLS_DEBUG("h5diff check symbolic link (object2)");
        /* dangling link */
        if (l_ret2 == 0) {
            H5TOOLS_DEBUG("h5diff ... dangling link");
            if (opts->no_dangle_links) {
                errors++;
                /* treat dangling link as error */
                if (opts->mode_verbose) {
#ifdef H5_HAVE_PARALLEL
                    unsigned char was_Parallel = g_Parallel;
                    if (g_Parallel && (g_nID == 0))
                        g_Parallel = 0;
#endif
                    parallel_print("Warning: <%s> is a dangling link.\n", obj2fullname);
#ifdef H5_HAVE_PARALLEL
                    g_Parallel = was_Parallel;
#endif
                }
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "treat dangling link as error");
            }
            else {
                if (opts->mode_verbose) {
#ifdef H5_HAVE_PARALLEL
                    unsigned char was_Parallel = g_Parallel;
                    if (g_Parallel && (g_nID == 0))
                        g_Parallel = 0;
#endif
                    parallel_print("obj2 <%s> is a dangling link.\n", obj2fullname);
#ifdef H5_HAVE_PARALLEL
                    g_Parallel = was_Parallel;
#endif
                }

                if (l_ret1 != 0 || l_ret2 != 0) {
                    nfound++;
#ifdef H5_HAVE_PARALLEL
                    unsigned char was_Parallel = g_Parallel;
                    if (g_Parallel && (g_nID == 0))
                        g_Parallel = 0;
#endif
                    print_found((hsize_t)nfound);
#ifdef H5_HAVE_PARALLEL
                    g_Parallel = was_Parallel;
                    H5TOOLS_GOTO_DONE(H5PDIFF_NO_ERR);
#else
                    H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
#endif
                }
            }
        }
        else if (l_ret2 < 0) { /* fail */
            errors++;
#ifdef H5_HAVE_PARALLEL
            unsigned char was_Parallel = g_Parallel;
            if (g_Parallel && (g_nID == 0))
                g_Parallel = 0;
#endif
            parallel_print("Object <%s> could not be found in <%s>\n", obj2fullname, fname2);
#ifdef H5_HAVE_PARALLEL
            g_Parallel = was_Parallel;
#endif
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Object could not be found");
        }
        else if (l_ret2 != 2) { /* symbolic link */
            obj2type = (h5trav_type_t)trg_linfo2.trg_type;
            if (info2_lp != NULL) {
                size_t idx = info2_lp->nused - 1;

                H5TOOLS_DEBUG("h5diff ... ... ... info2_obj not null");
                HDmemcpy(&info2_lp->paths[idx].obj_token, &trg_linfo2.obj_token, sizeof(H5O_token_t));
                info2_lp->paths[idx].type   = (h5trav_type_t)trg_linfo2.trg_type;
                info2_lp->paths[idx].fileno = trg_linfo2.fileno;
            }
            H5TOOLS_DEBUG("h5diff check symbolic link (object1) finished");
        }
    } /* end of if follow symlinks */

    /*
     * If verbose options is not used, don't need to traverse through the list
     * of objects in the group to display objects information,
     * So use h5tools_is_obj_same() to improve performance by skipping
     * comparing details of same objects.
     */

    if (!(opts->mode_verbose || opts->mode_report)) {
        H5TOOLS_DEBUG("h5diff NOT (opts->mode_verbose || opts->mode_report)");
        /* if no danglink links */
        if (l_ret1 > 0 && l_ret2 > 0)
            if (h5tools_is_obj_same(file1_id, obj1fullname, file2_id, obj2fullname) != 0)
                H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
    }

    both_objs_grp = (obj1type == H5TRAV_TYPE_GROUP && obj2type == H5TRAV_TYPE_GROUP);
    if (both_objs_grp) {
        H5TOOLS_DEBUG("h5diff both_objs_grp TRUE");
        /*
         * traverse group1
         */
        trav_info_init(fname1, file1_id, &info1_grp);
        /* optional data pass */
        info1_grp->opts = (diff_opt_t *)opts;

        if (h5trav_visit(file1_id, obj1fullname, TRUE, TRUE, trav_grp_objs, trav_grp_symlinks, info1_grp,
                         H5O_INFO_BASIC) < 0) {
            errors++;
            parallel_print("Error: Could not get file contents\n");
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Could not get file contents");
        }
        info1_lp = info1_grp;

        /*
         * traverse group2
         */
        trav_info_init(fname2, file2_id, &info2_grp);
        /* optional data pass */
        info2_grp->opts = (diff_opt_t *)opts;

        if (h5trav_visit(file2_id, obj2fullname, TRUE, TRUE, trav_grp_objs, trav_grp_symlinks, info2_grp,
                         H5O_INFO_BASIC) < 0) {
            errors++;
            parallel_print("Error: Could not get file contents\n");
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Could not get file contents");
        } /* end if */
        info2_lp = info2_grp;
    }

    H5TOOLS_DEBUG("groups traversed - errstat:%d", opts->err_stat);
    H5TOOLS_DEBUG("build_match_list next - errstat:%d", opts->err_stat);

    /* process the objects */
    build_match_list(obj1fullname, info1_lp, obj2fullname, info2_lp, &match_list, opts);
#ifdef H5_HAVE_PARALLEL
    if (g_Parallel)
        check_dataset_sizes(file1_id, match_list);
#endif
    H5TOOLS_DEBUG("build_match_list finished - errstat:%d", opts->err_stat);

    *_obj1fullname  = obj1fullname;
    *_obj2fullname  = obj2fullname;
    *_info1_lp      = info1_lp;
    *_info2_lp      = info2_lp;
    *_both_objs_grp = both_objs_grp;
    *_match_list    = match_list;

done:
    opts->err_stat = opts->err_stat | ret_value;

    /* free link info buffer */
    if (trg_linfo1.trg_path) {
        HDfree(trg_linfo1.trg_path);
        trg_linfo1.trg_path = NULL;
    }
    if (trg_linfo2.trg_path) {
        HDfree(trg_linfo2.trg_path);
        trg_linfo2.trg_path = NULL;
    }

    H5TOOLS_ENDDEBUG(" - errstat:%d", opts->err_stat);

#ifdef H5_HAVE_PARALLEL
    if ((ret_value == H5PDIFF_ERR) || (ret_value == H5PDIFF_NO_ERR))
        return -1;
#endif
    if (errors)
        return -1;
    return nfound;
}

static int
h5diff_open(const char *fname1, const char *fname2, diff_opt_t *opts, hid_t *_file_id1, hid_t *_file_id2)
{
    int   ret_value = 0;
    hid_t fapl1_id  = H5P_DEFAULT;
    hid_t fapl2_id  = H5P_DEFAULT;
    hid_t file1_id  = H5I_INVALID_HID;
    hid_t file2_id  = H5I_INVALID_HID;
    /*-------------------------------------------------------------------------
     * open the files first; if they are not valid, no point in continuing
     *-------------------------------------------------------------------------
     */
    /* open file 1 */
    if (opts->custom_vol[0] || opts->custom_vfd[0]) {
        if ((fapl1_id = h5tools_get_fapl(H5P_DEFAULT, opts->custom_vol[0] ? &(opts->vol_info[0]) : NULL,
                                         opts->custom_vfd[0] ? &(opts->vfd_info[0]) : NULL)) < 0) {
            ret_value = -1;
            parallel_print("%s: unable to create fapl for input file\n", __func__);
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "unable to create input fapl\n");
        }
    }

    if ((file1_id = h5tools_fopen(fname1, H5F_ACC_RDONLY, fapl1_id, (fapl1_id != H5P_DEFAULT), NULL,
                                  (size_t)0)) < 0) {
        ret_value = -1;
        parallel_print("%s: <%s>: unable to open file\n", __func__, fname1);
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "<%s>: unable to open file\n", fname1);
    }

    *_file_id1 = file1_id;

    H5TOOLS_DEBUG("file1_id = %s", fname1);

    if (opts->custom_vol[1] || opts->custom_vfd[1]) {
        if ((fapl2_id = h5tools_get_fapl(H5P_DEFAULT, opts->custom_vol[1] ? &(opts->vol_info[1]) : NULL,
                                         opts->custom_vfd[1] ? &(opts->vfd_info[1]) : NULL)) < 0) {
            ret_value = -2;
            parallel_print("%s: unable to create fapl for input file\n", __func__);
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "unable to create input fapl\n");
        }
    }

    if ((file2_id = h5tools_fopen(fname2, H5F_ACC_RDONLY, fapl2_id, (fapl2_id != H5P_DEFAULT), NULL,
                                  (size_t)0)) < 0) {
        ret_value = -2;
        parallel_print("%s: <%s>: unable to open file\n", __func__, fname2);
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "<%s>: unable to open file\n", fname2);
    }
    *_file_id2 = file2_id;

    H5TOOLS_DEBUG("file2_id = %s", fname2);
done:
    if (fapl1_id != H5P_DEFAULT)
        H5Pclose(fapl1_id);
    if (fapl2_id != H5P_DEFAULT)
        H5Pclose(fapl2_id);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: trav_grp_objs
 *
 * Purpose:  Call back function from h5trav_visit().
 *------------------------------------------------------------------------*/
static herr_t
trav_grp_objs(const char *path, const H5O_info2_t *oinfo, const char *already_visited, void *udata)
{
    trav_info_visit_obj(path, oinfo, already_visited, udata);

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: trav_grp_symlinks
 *
 * Purpose:  Call back function from h5trav_visit().
 *           Track and extra checkings while visiting all symbolic-links.
 *------------------------------------------------------------------------*/
static herr_t
trav_grp_symlinks(const char *path, const H5L_info2_t *linfo, void *udata)
{
    trav_info_t *      tinfo = (trav_info_t *)udata;
    diff_opt_t *       opts  = (diff_opt_t *)tinfo->opts;
    h5tool_link_info_t lnk_info;
    const char *       ext_fname;
    const char *       ext_path;
    herr_t             ret_value = SUCCEED;

    H5TOOLS_START_DEBUG(" ");
    /* init linkinfo struct */
    HDmemset(&lnk_info, 0, sizeof(h5tool_link_info_t));

    if (!opts->follow_links) {
        trav_info_visit_lnk(path, linfo, tinfo);
        H5TOOLS_GOTO_DONE(SUCCEED);
    }

    switch (linfo->type) {
        case H5L_TYPE_SOFT:
            if ((ret_value = H5tools_get_symlink_info(tinfo->fid, path, &lnk_info, opts->follow_links)) < 0) {
                H5TOOLS_GOTO_DONE(FAIL);
            }
            else if (ret_value == 0) {
                /* no dangling link option given and detect dangling link */
                tinfo->symlink_visited.dangle_link = TRUE;
                trav_info_visit_lnk(path, linfo, tinfo);
                if (opts->no_dangle_links)
                    opts->err_stat = H5DIFF_ERR; /* make dangling link is error */
                H5TOOLS_GOTO_DONE(SUCCEED);
            }

            /* check if already visit the target object */
            if (symlink_is_visited(&(tinfo->symlink_visited), linfo->type, NULL, lnk_info.trg_path))
                H5TOOLS_GOTO_DONE(SUCCEED);

            /* add this link as visited link */
            if (symlink_visit_add(&(tinfo->symlink_visited), linfo->type, NULL, lnk_info.trg_path) < 0)
                H5TOOLS_GOTO_DONE(SUCCEED);

            if (h5trav_visit(tinfo->fid, path, TRUE, TRUE, trav_grp_objs, trav_grp_symlinks, tinfo,
                             H5O_INFO_BASIC) < 0) {
                parallel_print("Error: Could not get file contents\n");
                opts->err_stat = H5DIFF_ERR;
                H5TOOLS_GOTO_ERROR(FAIL, "Error: Could not get file contents");
            }
            break;

        case H5L_TYPE_EXTERNAL:
            if ((ret_value = H5tools_get_symlink_info(tinfo->fid, path, &lnk_info, opts->follow_links)) < 0) {
                H5TOOLS_GOTO_DONE(FAIL);
            }
            else if (ret_value == 0) {
                /* no dangling link option given and detect dangling link */
                tinfo->symlink_visited.dangle_link = TRUE;
                trav_info_visit_lnk(path, linfo, tinfo);
                if (opts->no_dangle_links)
                    opts->err_stat = H5DIFF_ERR; /* make dangling link is error */
                H5TOOLS_GOTO_DONE(SUCCEED);
            }

            if (H5Lunpack_elink_val(lnk_info.trg_path, linfo->u.val_size, NULL, &ext_fname, &ext_path) < 0)
                H5TOOLS_GOTO_DONE(SUCCEED);

            /* check if already visit the target object */
            if (symlink_is_visited(&(tinfo->symlink_visited), linfo->type, ext_fname, ext_path))
                H5TOOLS_GOTO_DONE(SUCCEED);

            /* add this link as visited link */
            if (symlink_visit_add(&(tinfo->symlink_visited), linfo->type, ext_fname, ext_path) < 0)
                H5TOOLS_GOTO_DONE(SUCCEED);

            if (h5trav_visit(tinfo->fid, path, TRUE, TRUE, trav_grp_objs, trav_grp_symlinks, tinfo,
                             H5O_INFO_BASIC) < 0) {
                parallel_print("Error: Could not get file contents\n");
                opts->err_stat = H5DIFF_ERR;
                H5TOOLS_GOTO_ERROR(FAIL, "Error: Could not get file contents\n");
            }
            break;

        case H5L_TYPE_HARD:
        case H5L_TYPE_MAX:
        case H5L_TYPE_ERROR:
        default:
            parallel_print("Error: Invalid link type\n");
            opts->err_stat = H5DIFF_ERR;
            H5TOOLS_GOTO_ERROR(FAIL, "Error: Invalid link type");
            break;
    } /* end of switch */

done:
    if (lnk_info.trg_path)
        HDfree(lnk_info.trg_path);
    H5TOOLS_ENDDEBUG(" ");
    return ret_value;
}

#ifdef H5_HAVE_PARALLEL
#define DEFAULT_LARGE_DSET_SIZE 1048576

static hsize_t
hyperslab_pdiff(hid_t file1_id, const char *path1, hid_t file2_id, const char *path2, diff_opt_t *opts,
                diff_args_t *argdata)
{
    int   i, j;
    int   status    = -1;
    hid_t dset1_id  = H5I_INVALID_HID;
    hid_t dset2_id  = H5I_INVALID_HID;
    hid_t m_tid1    = H5I_INVALID_HID;
    hid_t m_tid2    = H5I_INVALID_HID;
    hid_t sid1      = H5I_INVALID_HID;
    hid_t sid2      = H5I_INVALID_HID;
    hid_t sm_space1 = H5I_INVALID_HID; /*stripmine data space */
    hid_t sm_space2 = H5I_INVALID_HID; /*stripmine data space */
    hid_t f_tid1    = H5I_INVALID_HID;
    hid_t f_tid2    = H5I_INVALID_HID;

    size_t     m_size1;
    size_t     m_size2;
    H5T_sign_t sign1;
    H5T_sign_t sign2;

    hbool_t is_dangle_link1 = FALSE;
    hbool_t is_dangle_link2 = FALSE;
    hbool_t is_hard_link    = FALSE;

    void *buf1    = NULL;
    void *buf2    = NULL;
    void *sm_buf1 = NULL;
    void *sm_buf2 = NULL;

    hsize_t       nfound      = 0;
    int           can_compare = 1; /* do diff or not */
    h5trav_type_t object_type;
    diff_err_t    ret_value = opts->err_stat;

    unsigned int vl_data1 = 0; /*contains VL datatypes */
    unsigned int vl_data2 = 0; /*contains VL datatypes */

    /* to get link info (COVERING options ...) */
    h5tool_link_info_t linkinfo1;
    h5tool_link_info_t linkinfo2;
    dataset_context_t *hs_context1 = NULL;
    dataset_context_t *hs_context2 = NULL;

    /*init link info struct */
    HDmemset(&linkinfo1, 0, sizeof(h5tool_link_info_t));
    HDmemset(&linkinfo2, 0, sizeof(h5tool_link_info_t));

    /* pass how to handle printing warnings to linkinfo option */
    if (print_warn(opts))
        linkinfo1.opt.msg_mode = linkinfo2.opt.msg_mode = 1;

    /* for symbolic links, take care follow symlink and no dangling link
     * options */
    if (argdata->type[0] == H5TRAV_TYPE_LINK || argdata->type[0] == H5TRAV_TYPE_UDLINK ||
        argdata->type[1] == H5TRAV_TYPE_LINK || argdata->type[1] == H5TRAV_TYPE_UDLINK) {
        /*
         * check dangling links for path1 and path2
         */

        H5TOOLS_DEBUG("diff links");
        /* target object1 - get type and name */
        if ((status = H5tools_get_symlink_info(file1_id, path1, &linkinfo1, opts->follow_links)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5tools_get_symlink_info failed");

        /* dangling link */
        if (status == 0) {
            if (opts->no_dangle_links) {
                /* dangling link is error */
                if (opts->mode_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", path1);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "dangling link is error");
            }
            else
                is_dangle_link1 = TRUE;
        }

        /* target object2 - get type and name */
        if ((status = H5tools_get_symlink_info(file2_id, path2, &linkinfo2, opts->follow_links)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5tools_get_symlink_info failed");
        /* dangling link */
        if (status == 0) {
            if (opts->no_dangle_links) {
                /* dangling link is error */
                if (opts->mode_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", path2);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "dangling link is error");
            }
            else
                is_dangle_link2 = TRUE;
        }

        /* found dangling link */
        if (is_dangle_link1 || is_dangle_link2) {
            H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
        }

        /* follow symbolic link option */
        if (opts->follow_links) {
            if (linkinfo1.linfo.type == H5L_TYPE_SOFT || linkinfo1.linfo.type == H5L_TYPE_EXTERNAL)
                argdata->type[0] = (h5trav_type_t)linkinfo1.trg_type;

            if (linkinfo2.linfo.type == H5L_TYPE_SOFT || linkinfo2.linfo.type == H5L_TYPE_EXTERNAL)
                argdata->type[1] = (h5trav_type_t)linkinfo2.trg_type;
        }
    }

    /* if objects are not the same type */
    if (argdata->type[0] != argdata->type[1]) {
        H5TOOLS_DEBUG("diff objects are not the same");
        if (opts->mode_verbose || opts->mode_list_not_cmp) {
            parallel_print("Not comparable: <%s> is of type %s and <%s> is of type %s\n", path1,
                           get_type(argdata->type[0]), path2, get_type(argdata->type[1]));
        }

        opts->not_cmp = 1;
        /* TODO: will need to update non-comparable is different
         * opts->contents = 0;
         */
        H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
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
    if (argdata->is_same_trgobj) {
        H5TOOLS_DEBUG("argdata->is_same_trgobj");
        is_hard_link = (object_type == H5TRAV_TYPE_DATASET || object_type == H5TRAV_TYPE_NAMED_DATATYPE ||
                        object_type == H5TRAV_TYPE_GROUP);
        if (opts->follow_links || is_hard_link) {
            /* print information is only verbose option is used */
            if (opts->mode_verbose || opts->mode_report) {
                switch (object_type) {
                    case H5TRAV_TYPE_DATASET:
                        do_print_objname("dataset", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_NAMED_DATATYPE:
                        do_print_objname("datatype", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_GROUP:
                        do_print_objname("group", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_LINK:
                        do_print_objname("link", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_UDLINK:
                        if (linkinfo1.linfo.type == H5L_TYPE_EXTERNAL &&
                            linkinfo2.linfo.type == H5L_TYPE_EXTERNAL)
                            do_print_objname("external link", path1, path2, opts);
                        else
                            do_print_objname("user defined link", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_UNKNOWN:
                    default:
                        parallel_print("Comparison not supported: <%s> and <%s> are of type %s\n", path1,
                                       path2, get_type(object_type));
                        opts->not_cmp = 1;
                        break;
                } /* switch(type)*/

                print_found(nfound);
            } /* if(opts->mode_verbose || opts->mode_report) */

            /* exact same, so comparison is done */
            H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
        }
    }

    switch (object_type) {
            /*----------------------------------------------------------------------
             * H5TRAV_TYPE_DATASET
             * This should be the princpal use case for this function.
             * We may may also deal with links.
             * NOTE: This particular function is ONLY called when we have identified
             * the specfic input object as a dataset which contain a moderately
             * large number of elements (DEFAULT_LARGE_DSET_SIZE = 1M).  The concern
             * is two fold. 1) By utilizing parallelism, we can improve performance;
             * and 2) The memory footprint utilized for processing the dataset
             * is minimized.  With regards to the latter, we also implement the
             * strip mining approach used by serial version for large datasets.
             *----------------------------------------------------------------------
             */
        case H5TRAV_TYPE_DATASET:
            if ((dset1_id = H5Dopen2(file1_id, path1, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dopen2 failed");

            if (h5tools_initialize_hyperslab_context(dset1_id, &hs_context1) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "hyperslab_context failed");

            if ((dset2_id = H5Dopen2(file2_id, path2, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dopen2 failed");

            if (h5tools_initialize_hyperslab_context(dset2_id, &hs_context2) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "hyperslab_context failed");

            f_tid1 = hs_context1->t_id;
            f_tid2 = hs_context2->t_id;

            /*-------------------------------------------------------------------------
             * check for comparable TYPE and SPACE
             *-------------------------------------------------------------------------
             */
            if (diff_can_type(hs_context1->t_id, hs_context2->t_id, hs_context1->ds_rank,
                              hs_context2->ds_rank, hs_context1->dims, hs_context2->dims,
                              hs_context1->maxdims, hs_context2->maxdims, opts, 0) != 1)
                can_compare = 0;

            /*-------------------------------------------------------------------------
             * memory type and sizes
             *-------------------------------------------------------------------------
             */
            if (H5Tget_class(hs_context1->t_id) == H5T_REFERENCE) {
                if ((m_tid1 = H5Tcopy(H5T_STD_REF)) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tcopy(H5T_STD_REF) first ftype failed");
            }
            else {
                if ((m_tid1 = H5Tget_native_type(hs_context1->t_id, H5T_DIR_DEFAULT)) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tget_native_type first ftype failed");
            }

            if (H5Tget_class(hs_context2->t_id) == H5T_REFERENCE) {
                if ((m_tid2 = H5Tcopy(H5T_STD_REF)) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tcopy(H5T_STD_REF) second ftype failed");
            }
            else {
                if ((m_tid2 = H5Tget_native_type(hs_context2->t_id, H5T_DIR_DEFAULT)) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tget_native_type second ftype failed");
            }

            m_size1 = H5Tget_size(m_tid1);
            m_size2 = H5Tget_size(m_tid2);
            H5TOOLS_DEBUG("type size: %ld - %ld", m_size1, m_size2);

            /*-------------------------------------------------------------------------
             * check for different signed/unsigned types
             *-------------------------------------------------------------------------
             */
            if (can_compare) {
                H5TOOLS_DEBUG("can_compare for sign");
                sign1 = H5Tget_sign(m_tid1);
                sign2 = H5Tget_sign(m_tid2);
                if (sign1 != sign2) {
                    H5TOOLS_DEBUG("sign1 != sign2");
                    if ((opts->mode_verbose || opts->mode_list_not_cmp) && path1 && path2) {
                        parallel_print("Not comparable: <%s> has sign %s ", path1, get_sign(sign1));
                        parallel_print("and <%s> has sign %s\n", path2, get_sign(sign2));
                    }

                    can_compare   = 0;
                    opts->not_cmp = 1;
                }
                H5TOOLS_DEBUG("can_compare for sign - can_compare=%d opts->not_cmp=%d", can_compare,
                              opts->not_cmp);
            }

            /* Check if type is either VLEN-data or VLEN-string to reclaim any
             * VLEN memory buffer later
             */
            if (TRUE == h5tools_detect_vlen(m_tid1))
                vl_data1 = TRUE;
            if (TRUE == h5tools_detect_vlen(m_tid2))
                vl_data2 = TRUE;
            H5TOOLS_DEBUG("h5tools_detect_vlen %d:%d - errstat:%d", vl_data1, vl_data2, opts->err_stat);

            /*------------------------------------------------------------------------
             * only attempt to compare if possible
             *-------------------------------------------------------------------------
             */

            if (can_compare) { /* it is possible to compare */
                hsize_t     need;
                hsize_t     nelmts1 = hs_context1->hs_nelmts;
                hsize_t     nelmts2 = hs_context2->hs_nelmts;
                int         rank1   = hs_context1->ds_rank;
                int         rank2   = hs_context2->ds_rank;
                hsize_t *   dims1   = &hs_context1->dims[0];
                hsize_t *   dims2   = &hs_context1->dims[0];
                H5T_class_t tclass  = H5Tget_class(hs_context1->t_id);

                if ((sid1 = H5Dget_space(dset1_id)) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_space failed");
                if ((sid2 = H5Dget_space(dset2_id)) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_space failed");

                if (tclass != H5T_ARRAY) {
                    /*-----------------------------------------------------------------
                     * "upgrade" the smaller memory size
                     *------------------------------------------------------------------
                     */
                    H5TOOLS_DEBUG("NOT H5T_ARRAY, upgrade the smaller memory size?");
                    if (FAIL == match_up_memsize(f_tid1, f_tid2, &m_tid1, &m_tid2, &m_size1, &m_size2))
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "match_up_memsize failed");
                    H5TOOLS_DEBUG("m_size: %ld - %ld", m_size1, m_size2);
                    opts->rank = rank1;
                    for (i = 0; i < rank1; i++)
                        opts->dims[i] = dims1[i];
                    opts->m_size = m_size1;
                    opts->m_tid  = m_tid1;
                    opts->nelmts = nelmts1;
                    need         = (size_t)(nelmts1 * m_size1); /* bytes needed */
                }
                else {
                    H5TOOLS_DEBUG("Array dims: %d - %d", dims1[0], dims2[0]);
                    /* Compare the smallest array, but create the largest buffer */
                    if (m_size1 <= m_size2) {
                        opts->rank = rank1;
                        for (i = 0; i < rank1; i++)
                            opts->dims[i] = dims1[i];
                        opts->m_size = m_size1;
                        opts->m_tid  = m_tid1;
                        opts->nelmts = nelmts1;
                        need         = (size_t)(nelmts2 * m_size2); /* bytes needed */
                    }
                    else {
                        opts->rank = rank2;
                        for (i = 0; i < rank2; i++)
                            opts->dims[i] = dims2[i];
                        opts->m_size = m_size2;
                        opts->m_tid  = m_tid2;
                        opts->nelmts = nelmts2;
                        need         = (size_t)(nelmts1 * m_size1); /* bytes needed */
                    }
                }
                opts->hs_nelmts = opts->nelmts;

                /*----------------------------------------------------------------
                 * read/compare
                 *-----------------------------------------------------------------
                 */
                if (need < H5TOOLS_MALLOCSIZE) {
                    buf1 = HDmalloc(need);
                    buf2 = HDmalloc(need);
                } /* end if */

                /* Assume entire data space to be printed */
                init_acc_pos((unsigned)opts->rank, opts->dims, opts->acc, opts->pos, opts->p_min_idx);

                for (i = 0; i < opts->rank; i++) {
                    opts->p_max_idx[i] = opts->dims[i];
                }

                if (buf1 != NULL && buf2 != NULL && opts->sset[0] == NULL && opts->sset[1] == NULL) {
                    H5TOOLS_DEBUG("buf1 != NULL && buf2 != NULL");
                    H5TOOLS_DEBUG("H5Dread did1");
                    if (H5Dread(dset1_id, m_tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1) < 0)
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dread failed");
                    H5TOOLS_DEBUG("H5Dread did2");
                    if (H5Dread(dset2_id, m_tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2) < 0)
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dread failed");

                    /* initialize the current stripmine position; this is necessary to print the array indices
                     */
                    for (j = 0; j < opts->rank; j++)
                        opts->sm_pos[j] = (hsize_t)0;

                    /* array diff */
                    nfound = diff_array(buf1, buf2, opts, dset1_id, dset2_id);
                    H5TOOLS_DEBUG("diff_array ret nfound:%d - errstat:%d", nfound, opts->err_stat);

                    /* reclaim any VL memory, if necessary */
                    H5TOOLS_DEBUG("check vl_data1:%d", vl_data1);
                    if (vl_data1)
                        H5Treclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
                    H5TOOLS_DEBUG("check vl_data2:%d", vl_data2);
                    if (vl_data2)
                        H5Treclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
                    if (buf1 != NULL) {
                        HDfree(buf1);
                        buf1 = NULL;
                    }
                    if (buf2 != NULL) {
                        HDfree(buf2);
                        buf2 = NULL;
                    }
                }                   /* end if */
                else {              /* possibly not enough memory, read/compare by hyperslabs */
                    hsize_t elmtno; /* counter  */
                    int     carry;  /* counter carry value */

                    /* stripmine info */
                    hsize_t  sm_size[H5S_MAX_RANK];  /* stripmine size */
                    hsize_t  sm_block[H5S_MAX_RANK]; /* stripmine block size  */
                    hsize_t  sm_nbytes;              /* bytes per stripmine */
                    hsize_t  sm_nelmts1;             /* elements per stripmine */
                    hsize_t  sm_nelmts2;             /* elements per stripmine */
                    hsize_t  ssm_limit;
                    hsize_t  ssm_remaining;
                    hssize_t ssm_nelmts; /* elements temp */

                    /* hyperslab info */
                    hsize_t hs_offset1[H5S_MAX_RANK]; /* starting offset */
                    hsize_t hs_count1[H5S_MAX_RANK];  /* number of blocks */
                    hsize_t hs_block1[H5S_MAX_RANK];  /* size of blocks */
                    hsize_t hs_stride1[H5S_MAX_RANK]; /* stride */
                    hsize_t hs_size1[H5S_MAX_RANK];   /* size this pass */
                    hsize_t hs_offset2[H5S_MAX_RANK]; /* starting offset */
                    hsize_t hs_count2[H5S_MAX_RANK];  /* number of blocks */
                    hsize_t hs_block2[H5S_MAX_RANK];  /* size of blocks */
                    hsize_t hs_stride2[H5S_MAX_RANK]; /* stride */
                    hsize_t hs_size2[H5S_MAX_RANK];   /* size this pass */
                    hsize_t hs_nelmts1 = 0;           /* elements in request */
                    hsize_t hs_nelmts2 = 0;           /* elements in request */
                    hsize_t hs_extra1;                /* temp for strip mining */
                    hsize_t zero[8];                  /* vector of zeros */
                    hsize_t low[H5S_MAX_RANK];        /* low bound of hyperslab */
                    hsize_t high[H5S_MAX_RANK];       /* higher bound of hyperslab */

                    H5TOOLS_DEBUG("reclaim any VL memory and free unused buffers");
                    if (buf1 != NULL) {
                        /* reclaim any VL memory, if necessary */
                        if (vl_data1)
                            H5Treclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
                        HDfree(buf1);
                        buf1 = NULL;
                    }
                    if (buf2 != NULL) {
                        /* reclaim any VL memory, if necessary */
                        if (vl_data2)
                            H5Treclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
                        HDfree(buf2);
                        buf2 = NULL;
                    }

                    /* the stripmine loop */
                    HDmemset(hs_offset1, 0, sizeof hs_offset1);
                    HDmemset(hs_stride1, 0, sizeof hs_stride1);
                    HDmemset(hs_count1, 0, sizeof hs_count1);
                    HDmemset(hs_block1, 0, sizeof hs_block1);
                    HDmemset(hs_size1, 0, sizeof hs_size1);
                    HDmemset(hs_offset2, 0, sizeof hs_offset2);
                    HDmemset(hs_stride2, 0, sizeof hs_stride2);
                    HDmemset(hs_count2, 0, sizeof hs_count2);
                    HDmemset(hs_block2, 0, sizeof hs_block2);
                    HDmemset(hs_size2, 0, sizeof hs_size2);
                    HDmemset(zero, 0, sizeof zero);

                    for (i = 0; i < hs_context1->ds_rank; i++) {
                        hs_offset1[i] = hs_context1->hs_offset[i];
                        hs_offset2[i] = hs_context2->hs_offset[i];
                    }
                    /* IF subsetting was requested - initialize the subsetting variables */
                    /* CAUTION!! I haven't verified this portion of the code for parallel execution!
                     * It is my belief that the user subsetting specification doesn't account for
                     * parallelism and thus the approach here, should probably be done by a single
                     * rank. In particular, our parallel assumptions might be unjustfied since
                     * the subsetting was NOT accounted for when sizing the dataset vs. the
                     * size limit over which, we feel justified for multiple MPI ranks to share
                     * the work.
                     *
                     * [RAW] December 2021
                     */
                    H5TOOLS_DEBUG("compare by hyperslabs: opts->nelmts=%ld - opts->m_size=%ld", opts->nelmts,
                                  opts->m_size);
                    if (opts->sset[0] != NULL) {
                        H5TOOLS_DEBUG("opts->sset[0] != NULL");

                        /* Check for valid settings - default if not specified */
                        if (!opts->sset[0]->start.data || !opts->sset[0]->stride.data ||
                            !opts->sset[0]->count.data || !opts->sset[0]->block.data) {
                            /* they didn't specify a ``stride'' or ``block''. default to 1 in all
                             * dimensions */
                            if (!opts->sset[0]->start.data) {
                                /* default to (0, 0, ...) for the start coord */
                                opts->sset[0]->start.data =
                                    (hsize_t *)HDcalloc((size_t)rank1, sizeof(hsize_t));
                                opts->sset[0]->start.len = (unsigned)rank1;
                            }

                            if (!opts->sset[0]->stride.data) {
                                opts->sset[0]->stride.data =
                                    (hsize_t *)HDcalloc((size_t)rank1, sizeof(hsize_t));
                                opts->sset[0]->stride.len = (unsigned)rank1;
                                for (i = 0; i < rank1; i++)
                                    opts->sset[0]->stride.data[i] = 1;
                            }

                            if (!opts->sset[0]->count.data) {
                                opts->sset[0]->count.data =
                                    (hsize_t *)HDcalloc((size_t)rank1, sizeof(hsize_t));
                                opts->sset[0]->count.len = (unsigned)rank1;
                                for (i = 0; i < rank1; i++)
                                    opts->sset[0]->count.data[i] = 1;
                            }

                            if (!opts->sset[0]->block.data) {
                                opts->sset[0]->block.data =
                                    (hsize_t *)HDcalloc((size_t)rank1, sizeof(hsize_t));
                                opts->sset[0]->block.len = (unsigned)rank1;
                                for (i = 0; i < rank1; i++)
                                    opts->sset[0]->block.data[i] = 1;
                            }

                            /*-------------------------------------------------------------------------
                             * check for block overlap
                             *-------------------------------------------------------------------------
                             */
                            for (i = 0; i < rank1; i++) {
                                if (opts->sset[0]->count.data[i] > 1) {
                                    if (opts->sset[0]->stride.data[i] < opts->sset[0]->block.data[i]) {
                                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR,
                                                           "wrong subset selection[0]; blocks overlap");
                                    } /* end if */
                                }     /* end if */
                            }         /* end for */
                        }

                        /* Reset the total number of elements to the subset from the command */
                        opts->nelmts = 1;
                        for (i = 0; i < rank1; i++) {
                            hs_offset1[i] = opts->sset[0]->start.data[i];
                            hs_stride1[i] = opts->sset[0]->stride.data[i];
                            hs_count1[i]  = opts->sset[0]->count.data[i];
                            hs_block1[i]  = opts->sset[0]->block.data[i];
                            opts->nelmts *= hs_count1[i] * hs_block1[i];
                            hs_size1[i] = 0;
                            H5TOOLS_DEBUG("[%d]hs_offset1:%ld, hs_stride1:%ld, hs_count1:%ld, hs_block1:%ld",
                                          i, hs_offset1[i], hs_stride1[i], hs_count1[i], hs_block1[i]);
                        }
                    }
                    if (opts->sset[1] != NULL) {
                        H5TOOLS_DEBUG("opts->sset[1] != NULL");

                        /* Check for valid settings - default if not specified */
                        if (!opts->sset[1]->start.data || !opts->sset[1]->stride.data ||
                            !opts->sset[1]->count.data || !opts->sset[1]->block.data) {
                            /* they didn't specify a ``stride'' or ``block''. default to 1 in all
                             * dimensions */
                            if (!opts->sset[1]->start.data) {
                                /* default to (0, 0, ...) for the start coord */
                                opts->sset[1]->start.data =
                                    (hsize_t *)HDcalloc((size_t)rank2, sizeof(hsize_t));
                                opts->sset[1]->start.len = (unsigned)rank2;
                            }

                            if (!opts->sset[1]->stride.data) {
                                opts->sset[1]->stride.data =
                                    (hsize_t *)HDcalloc((size_t)rank2, sizeof(hsize_t));
                                opts->sset[1]->stride.len = (unsigned)rank2;
                                for (i = 0; i < rank2; i++)
                                    opts->sset[1]->stride.data[i] = 1;
                            }

                            if (!opts->sset[1]->count.data) {
                                opts->sset[1]->count.data =
                                    (hsize_t *)HDcalloc((size_t)rank2, sizeof(hsize_t));
                                opts->sset[1]->count.len = (unsigned)rank2;
                                for (i = 0; i < rank2; i++)
                                    opts->sset[1]->count.data[i] = 1;
                            }

                            if (!opts->sset[1]->block.data) {
                                opts->sset[1]->block.data =
                                    (hsize_t *)HDcalloc((size_t)rank2, sizeof(hsize_t));
                                opts->sset[1]->block.len = (unsigned)rank2;
                                for (i = 0; i < rank2; i++)
                                    opts->sset[1]->block.data[i] = 1;
                            }

                            /*-------------------------------------------------------------------------
                             * check for block overlap
                             *-------------------------------------------------------------------------
                             */
                            for (i = 0; i < rank2; i++) {
                                if (opts->sset[1]->count.data[i] > 1) {
                                    if (opts->sset[1]->stride.data[i] < opts->sset[1]->block.data[i]) {
                                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR,
                                                           "wrong subset selection[1]; blocks overlap");
                                    } /* end if */
                                }     /* end if */
                            }         /* end for */
                        }

                        for (i = 0; i < rank2; i++) {
                            hs_offset2[i] = opts->sset[1]->start.data[i];
                            hs_stride2[i] = opts->sset[1]->stride.data[i];
                            hs_count2[i]  = opts->sset[1]->count.data[i];
                            hs_block2[i]  = opts->sset[1]->block.data[i];
                            hs_size2[i]   = 0;
                            H5TOOLS_DEBUG("[%d]hs_offset2:%ld, hs_stride2:%ld, hs_count2:%ld, hs_block2:%ld",
                                          i, hs_offset2[i], hs_stride2[i], hs_count2[i], hs_block2[i]);
                        }
                    } /* End subsetting */

                    /*
                     * determine the strip mine size and allocate a buffer. The strip mine is
                     * a hyperslab whose size is manageable.
                     */
                    if (opts->mode_verbose || opts->mode_report) {
                        if (hs_context1->mpi_rank == 0)
                            do_print_objname("dataset", path1, path2, opts);
                    }
                    sm_nbytes = opts->m_size;
                    for (i = 0; i < hs_context1->ds_rank; i++) {
                        hs_block1[i] = hs_context1->hs_block[i];
                        hs_block2[i] = hs_context2->hs_block[i];
                    }

                    if (opts->rank > 0) {
                        for (i = hs_context1->ds_rank; i > 0; --i) {
                            hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
                            if (size == 0) /* datum size > H5TOOLS_BUFSIZE */
                                size = 1;
                            H5TOOLS_DEBUG("opts->dims[%d]: %ld - size: %ld", i - 1, opts->dims[i - 1], size);
                            if (opts->sset[1] != NULL) {
                                sm_size[i - 1]  = MIN(hs_block1[i - 1] * hs_count1[i - 1], size);
                                sm_block[i - 1] = MIN(hs_block1[i - 1], sm_size[i - 1]);
                            }
                            else {
                                sm_size[i - 1]  = MIN(hs_context1->hs_block[i - 1], size);
                                sm_block[i - 1] = sm_size[i - 1];
                            }
                            H5TOOLS_DEBUG("sm_size[%d]: %ld - sm_block:%ld", i - 1, sm_size[i - 1],
                                          sm_block[i - 1]);
                            sm_nbytes *= sm_size[i - 1];
                            H5TOOLS_DEBUG("sm_nbytes: %ld", sm_nbytes);
                        }
                    }

                    H5TOOLS_DEBUG("opts->nelmts: %ld", opts->nelmts);
                    hs_nelmts1 = sm_nbytes / hs_context1->dt_size;
                    ssm_limit  = hs_nelmts1 * 2;
                    for (elmtno = 0; elmtno < opts->nelmts; elmtno += hs_nelmts1) {
                        H5TOOLS_DEBUG("elmtno: %ld - hs_nelmts1: %ld", elmtno, hs_nelmts1);
                        ssm_remaining = opts->nelmts - elmtno;
                        if (ssm_remaining < ssm_limit) {
                            hsize_t elmts_per_row = 1;
                            hsize_t extra_rows;
                            hs_nelmts1 = sm_nbytes / hs_context1->dt_size;
                            hs_extra1  = ssm_remaining - hs_nelmts1;
                            for (i = 1; i < opts->rank; i++)
                                elmts_per_row *= sm_block[i];
                            extra_rows = hs_extra1 / elmts_per_row;
                            sm_nbytes += (extra_rows * elmts_per_row);
                            sm_block[0] += extra_rows;
                            hs_block1[0] += extra_rows;
                            hs_block2[0] += extra_rows;
                        }
                        if (NULL == (sm_buf1 = (unsigned char *)HDmalloc((size_t)sm_nbytes)))
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Could not allocate buffer for strip-mine");
                        if (NULL == (sm_buf2 = (unsigned char *)HDmalloc((size_t)sm_nbytes)))
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Could not allocate buffer for strip-mine");

                        /* calculate the hyperslab size */
                        /* initialize subset */
                        if (opts->rank > 0) {
                            if (opts->sset[0] != NULL) {
                                H5TOOLS_DEBUG("sset1 has data");
                                /* calculate the potential number of elements */
                                for (i = 0; i < rank1; i++) {
                                    H5TOOLS_DEBUG("[%d]opts->dims: %ld - hs_offset1: %ld - sm_block: %ld", i,
                                                  opts->dims[i], hs_offset1[i], sm_block[i]);
                                    hs_size1[i] = MIN(opts->dims[i] - hs_offset1[i], sm_block[i]);
                                    H5TOOLS_DEBUG("hs_size1[%d]: %ld", i, hs_size1[i]);
                                }
                                if (H5Sselect_hyperslab(sid1, H5S_SELECT_SET, hs_offset1, hs_stride1,
                                                        hs_count1, hs_size1) < 0)
                                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab sid1 failed");
                            }
                            else {
                                for (i = 0, hs_nelmts1 = 1; i < rank1; i++) {
                                    /* Use the hyperslab context values rather than the serial hyperslab
                                     * implementation (opts->xxx values)
                                     */
                                    H5TOOLS_DEBUG("[%d]opts->dims: %ld - hs_offset1: %ld - sm_block: %ld", i,
                                                  opts->dims[i], hs_offset1[i], sm_block[i]);
                                    hs_size1[i] = MIN(hs_block1[i] - hs_offset1[i], sm_block[i]);
                                    H5TOOLS_DEBUG("hs_size1[%d]: %ld", i, hs_size1[i]);
                                    hs_nelmts1 *= hs_size1[i];
                                    H5TOOLS_DEBUG("hs_nelmts1:%ld *= hs_size1[%d]: %ld", hs_nelmts1, i,
                                                  hs_size1[i]);
                                }
                                if (H5Sselect_hyperslab(sid1, H5S_SELECT_SET, hs_offset1, NULL, hs_size1,
                                                        NULL) < 0)
                                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab sid1 failed");
                            }

                            if ((ssm_nelmts = H5Sget_select_npoints(sid1)) < 0)
                                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_select_npoints failed");
                            sm_nelmts1 = (hsize_t)ssm_nelmts;
                            H5TOOLS_DEBUG("sm_nelmts1: %ld", sm_nelmts1);
                            hs_nelmts1 = sm_nelmts1;

                            if ((sm_space1 = H5Screate_simple(1, &sm_nelmts1, NULL)) < 0)
                                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Screate_simple failed");

                            if (H5Sselect_hyperslab(sm_space1, H5S_SELECT_SET, zero, NULL, &sm_nelmts1,
                                                    NULL) < 0)
                                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab failed");

                            if (opts->sset[1] != NULL) {
                                H5TOOLS_DEBUG("sset2 has data");
                                for (i = 0; i < rank2; i++) {
                                    H5TOOLS_DEBUG("[%d]opts->dims: %ld - hs_offset2: %ld - sm_block: %ld", i,
                                                  opts->dims[i], hs_offset2[i], sm_block[i]);
                                    hs_size2[i] = MIN(opts->dims[i] - hs_offset2[i], sm_block[i]);
                                    H5TOOLS_DEBUG("hs_size2[%d]: %ld", i, hs_size2[i]);
                                }
                                if (H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset2, hs_stride2,
                                                        hs_count2, hs_size2) < 0)
                                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab sid2 failed");
                            }
                            else {
                                for (i = 0, hs_nelmts2 = 1; i < rank2; i++) {
                                    /* Use the hyperslab context values rather than the serial hyperslab
                                     * implementation (opts->xxx values)
                                     */
                                    H5TOOLS_DEBUG("[%d]opts->dims: %ld - hs_offset2: %ld - sm_block: %ld", i,
                                                  opts->dims[i], hs_offset2[i], sm_block[i]);
                                    hs_size2[i] = MIN(hs_block2[i] - hs_offset2[i], sm_block[i]);
                                    H5TOOLS_DEBUG("hs_size2[%d]: %ld", i, hs_size2[i]);
                                    hs_nelmts2 *= hs_size2[i];
                                    H5TOOLS_DEBUG("hs_nelmts2:%ld *= hs_size2[%d]: %ld", hs_nelmts2, i,
                                                  hs_size2[i]);
                                }
                                if (H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset2, NULL, hs_size2,
                                                        NULL) < 0)
                                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab sid2 failed");
                            }

                            if ((ssm_nelmts = H5Sget_select_npoints(sid2)) < 0)
                                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_select_npoints failed");
                            sm_nelmts2 = (hsize_t)ssm_nelmts;
                            H5TOOLS_DEBUG("sm_nelmts2: %ld", sm_nelmts2);
                            hs_nelmts2 = sm_nelmts2;

                            if ((sm_space2 = H5Screate_simple(1, &sm_nelmts2, NULL)) < 0)
                                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Screate_simple failed");

                            if (H5Sselect_hyperslab(sm_space2, H5S_SELECT_SET, zero, NULL, &sm_nelmts2,
                                                    NULL) < 0)
                                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab failed");
                        }
                        else
                            hs_nelmts1 = 1;
                        opts->hs_nelmts = hs_nelmts1;
                        H5TOOLS_DEBUG("hs_nelmts: %ld", opts->hs_nelmts);

                        /* read the data */
                        if (H5Dread(dset1_id, m_tid1, sm_space1, sid1, H5P_DEFAULT, sm_buf1) < 0)
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dread failed");
                        if (H5Dread(dset2_id, m_tid2, sm_space2, sid2, H5P_DEFAULT, sm_buf2) < 0)
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dread failed");

                        /* print array indices. get the lower bound of the hyperslab and calculate
                           the element position at the start of hyperslab */
                        if (H5Sget_select_bounds(sid1, low, high) < 0)
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_select_bounds failed");
                        /* initialize the current stripmine position; this is necessary to print the array
                         * indices */
                        for (j = 0; j < opts->rank; j++)
                            opts->sm_pos[j] = low[j];

                        /* Assume entire data space to be printed */
                        init_acc_pos((unsigned)opts->rank, opts->dims, opts->acc, opts->pos, opts->p_min_idx);

                        /* get array differences. in the case of hyperslab read, increment the number of
                        differences found in each hyperslab and pass the position at the beginning for
                        printing */
                        nfound += diff_array(sm_buf1, sm_buf2, opts, dset1_id, dset2_id);

                        if (sm_buf1 != NULL) {
                            /* reclaim any VL memory, if necessary */
                            if (vl_data1)
                                H5Treclaim(m_tid1, sm_space1, H5P_DEFAULT, sm_buf1);
                            HDfree(sm_buf1);
                            sm_buf1 = NULL;
                        }
                        if (sm_buf2 != NULL) {
                            /* reclaim any VL memory, if necessary */
                            if (vl_data2)
                                H5Treclaim(m_tid2, sm_space2, H5P_DEFAULT, sm_buf2);
                            HDfree(sm_buf2);
                            sm_buf2 = NULL;
                        }

                        H5Sclose(sm_space1);
                        H5Sclose(sm_space2);

                        /* calculate the next hyperslab offset */
                        for (i = opts->rank, carry = 1; i > 0 && carry; --i) {
                            if (opts->sset[0] != NULL) {
                                H5TOOLS_DEBUG("[%d]hs_size1:%ld - hs_block1:%ld - hs_stride1:%ld", i - 1,
                                              hs_size1[i - 1], hs_block1[i - 1], hs_stride1[i - 1]);
                                if (hs_size1[i - 1] >= hs_block1[i - 1]) {
                                    hs_offset1[i - 1] += hs_size1[i - 1];
                                }
                                else {
                                    hs_offset1[i - 1] += hs_stride1[i - 1];
                                }
                            }
                            else
                                hs_offset1[i - 1] += hs_size1[i - 1];
                            H5TOOLS_DEBUG("[%d]hs_offset1:%ld - opts->dims:%ld", i - 1, hs_offset1[i - 1],
                                          opts->dims[i - 1]);
                            if (hs_offset1[i - 1] >= opts->dims[i - 1])
                                hs_offset1[i - 1] = 0;
                            else
                                carry = 0;
                            H5TOOLS_DEBUG("[%d]hs_offset1:%ld", i - 1, hs_offset1[i - 1]);
                            if (opts->sset[1] != NULL) {
                                H5TOOLS_DEBUG("[%d]hs_size2:%ld - hs_block2:%ld - hs_stride2:%ld", i - 1,
                                              hs_size2[i - 1], hs_block2[i - 1], hs_stride2[i - 1]);
                                if (hs_size2[i - 1] >= hs_block2[i - 1]) {
                                    hs_offset2[i - 1] += hs_size2[i - 1];
                                }
                                else {
                                    hs_offset2[i - 1] += hs_stride2[i - 1];
                                }
                            }
                            else
                                hs_offset2[i - 1] += hs_size2[i - 1];
                            H5TOOLS_DEBUG("[%d]hs_offset2:%ld - opts->dims:%ld", i - 1, hs_offset2[i - 1],
                                          opts->dims[i - 1]);
                            if (hs_offset2[i - 1] >= opts->dims[i - 1])
                                hs_offset2[i - 1] = 0;
                            H5TOOLS_DEBUG("[%d]hs_offset2:%ld", i - 1, hs_offset2[i - 1]);
                        }
                    } /* elmtno for loop */
                }     /* hyperslab read */
                H5TOOLS_DEBUG("can compare complete");
            } /*can_compare*/

            H5E_BEGIN_TRY
            {
                H5Sclose(sid1);
                H5Sclose(sid2);
                H5Sclose(sm_space1);
                H5Sclose(sm_space2);
                H5Pclose(hs_context1->dcpl);
                H5Pclose(hs_context2->dcpl);
                H5Tclose(f_tid1);
                H5Tclose(f_tid2);
                H5Tclose(m_tid1);
                H5Tclose(m_tid2);
                /* enable error reporting */
            }
            H5E_END_TRY;

            HDfree(hs_context1);
            HDfree(hs_context2);

            break;

            /*----------------------------------------------------------------------
             * H5TRAV_TYPE_LINK
             *----------------------------------------------------------------------
             */
        case H5TRAV_TYPE_LINK: {
            H5TOOLS_DEBUG("H5TRAV_TYPE_LINK 1:%s  2:%s ", path1, path2);
            status = HDstrcmp(linkinfo1.trg_path, linkinfo2.trg_path);

            /* if the target link name is not same then the links are "different" */
            nfound = (status != 0) ? 1 : 0;

            if (print_objname(opts, nfound))
                do_print_objname("link", path1, path2, opts);

            /* always print the number of differences found in verbose mode */
            if (opts->mode_verbose)
                print_found(nfound);
        } break;

            /*----------------------------------------------------------------------
             * H5TRAV_TYPE_UDLINK
             *----------------------------------------------------------------------
             */
        case H5TRAV_TYPE_UDLINK: {
            H5TOOLS_DEBUG("H5TRAV_TYPE_UDLINK 1:%s  2:%s ", path1, path2);
            /* Only external links will have a query function registered */
            if (linkinfo1.linfo.type == H5L_TYPE_EXTERNAL && linkinfo2.linfo.type == H5L_TYPE_EXTERNAL) {
                /* If the buffers are the same size, compare them */
                if (linkinfo1.linfo.u.val_size == linkinfo2.linfo.u.val_size) {
                    status = HDmemcmp(linkinfo1.trg_path, linkinfo2.trg_path, linkinfo1.linfo.u.val_size);
                }
                else
                    status = 1;

                /* if "linkinfo1.trg_path" != "linkinfo2.trg_path" then the links
                 * are "different" extlinkinfo#.path is combination string of
                 * file_name and obj_name
                 */
                nfound = (status != 0) ? 1 : 0;

                if (print_objname(opts, nfound))
                    do_print_objname("external link", path1, path2, opts);

            } /* end if */
            else {
                /* If one or both of these links isn't an external link, we can only
                 * compare information from H5Lget_info since we don't have a query
                 * function registered for them.
                 *
                 * If the link classes or the buffer length are not the
                 * same, the links are "different"
                 */
                if ((linkinfo1.linfo.type != linkinfo2.linfo.type) ||
                    (linkinfo1.linfo.u.val_size != linkinfo2.linfo.u.val_size))
                    nfound = 1;
                else
                    nfound = 0;

                if (print_objname(opts, nfound))
                    do_print_objname("user defined link", path1, path2, opts);
            } /* end else */

            /* always print the number of differences found in verbose mode */
            if (opts->mode_verbose)
                print_found(nfound);
        } break;

        case H5TRAV_TYPE_UNKNOWN:
        default:
            if (opts->mode_verbose)
                parallel_print("Comparison not supported: <%s> and <%s> are of type %s\n", path1, path2,
                               get_type(object_type));
            opts->not_cmp = 1;
            break;
    }

done:
    opts->err_stat = opts->err_stat | ret_value;

    return nfound;
} /* end hyperslab_pdiff() */

/*-------------------------------------------------------------------------
 * Function: check_dataset_sizes
 *
 * Purpose:  Compare dataset sizes to a known size limit. If the size
 *           exceeds the limit then the dataset is marked for utilizing
 *           hyperslabs to subdivide the diff operation between parallel
 *           ranks.
 *
 *           TODO:  Maybe we should also be checking link objects which
 *           in turn could point to a dataset?
 * Return:   none
 *-------------------------------------------------------------------------
 */
static int
check_dataset_sizes(hid_t fidin, trav_table_t *travt)
{
    size_t        i;
    hsize_t       dims[H5S_MAX_RANK]; /* dimensions of dataset */
    static size_t large_dset_size = 0;
    int           j, ret_value = 0;

    if (large_dset_size == 0) {
        char *envValue;
        if ((envValue = HDgetenv("H5_PARALLEL_DSET_SIZE")) != NULL) {
            size_t value_check = (size_t)HDatol(envValue);
            if (value_check > 0) {
                large_dset_size = value_check;
            }
        }
        else
            large_dset_size = DEFAULT_LARGE_DSET_SIZE;
    }

    for (i = 0; i < travt->nobjs; i++) {
        if (travt->objs[i].type == H5TRAV_TYPE_DATASET) {
            size_t nelmts;
            int    rank;
            hid_t  dset_in    = H5I_INVALID_HID;
            hid_t  f_space_id = H5I_INVALID_HID;
            if ((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dopen2 failed");
            if ((f_space_id = H5Dget_space(dset_in)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dget_space failed");
            if ((rank = H5Sget_simple_extent_ndims(f_space_id)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_ndims failed");

            H5Sget_simple_extent_dims(f_space_id, dims, NULL);

            nelmts = 1;
            for (j = 0; j < rank; j++)
                nelmts *= dims[j];

            if (nelmts >= large_dset_size)
                travt->objs[i].use_hyperslab = true;
            if (H5Sclose(f_space_id) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Sclose failed");
            if (H5Dclose(dset_in) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dclose failed");
        }
    }
done:
    return ret_value;
}

static int
compare_objIDs(const void *h1, const void *h2)
{
    const print_objs_t *obj1 = (const print_objs_t *)h1;
    const print_objs_t *obj2 = (const print_objs_t *)h2;
    return (obj1->obj_idx > obj2->obj_idx);
}

/*-------------------------------------------------------------------------
 * Function: phdiff_gather_diffs
 *
 * Purpose:  print all saved buffers
 *
 * Return:   none
 *-------------------------------------------------------------------------
 */
static void
ph5diff_gather_diffs(void)
{
    int mpi_rank, mpi_size;
    int total_bytes = 0, total_diff_count = 0;
    int k, my_size                        = (int)local_diff_total + rank_diff_count;
    int send_offset     = 0;
    int local_counts[2] = {(int)my_size, rank_diff_count};

    int * recvcounts = NULL;
    int * recvtuples = NULL;
    int * rankIDs    = NULL;
    int * displs     = NULL;
    int * indices    = NULL;
    char *sendbuf    = NULL;
    char *recvbuf    = NULL;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    if (mpi_rank == 0) {
        displs     = (int *)malloc((size_t)((size_t)mpi_size * sizeof(int)));
        recvcounts = (int *)malloc((size_t)((size_t)mpi_size * sizeof(int)));
        recvtuples = (int *)malloc((size_t)((size_t)mpi_size * 2 * sizeof(int)));
    }

    MPI_Gather(local_counts, 2, MPI_INT, recvtuples, 2, MPI_INT, 0, MPI_COMM_WORLD);

    /* Maybe create a contiguous buffer to send to the root */
    if (rank_diff_count > 1) {
        rankIDs = (int *)malloc((size_t)((size_t)rank_diff_count * sizeof(int)));
        sendbuf = (char *)malloc((size_t)((size_t)local_diff_total + (size_t)rank_diff_count));
        for (k = 0; k < rank_diff_count; k++) {
            int len = (int)rank_diffs[k]->outbuffoffset;
            strncpy(&sendbuf[send_offset], rank_diffs[k]->outbuff, (size_t)len);
            send_offset += len;
            sendbuf[send_offset++] = 0; /* Null terminiate the text */
            rankIDs[k]             = rank_diffs[k]->obj_idx;
        }
    }
    else if (rank_diff_count > 0) {
        rankIDs    = (int *)malloc((size_t)rank_diff_count * sizeof(int));
        rankIDs[0] = rank_diffs[0]->obj_idx;
        /* If ONLY a single outbuff, then use it rather than malloc and copy */
        sendbuf = rank_diffs[0]->outbuff;
    }

    if (mpi_rank == 0) {
        int *next = (int *)recvtuples;

        displs[0]        = 0;
        total_bytes      = next[0]; /* tuple[0] */
        total_diff_count = next[1]; /* tuple[1] */
        recvcounts[0]    = total_bytes;
        next += 2;

        for (k = 1; k < mpi_size; k++) {
            recvcounts[k] = next[0];     /* tuple[0] */
            total_diff_count += next[1]; /* tuple[1] */
            total_bytes += recvcounts[k];
            displs[k] = displs[k - 1] + recvcounts[k - 1];

            next += 2;
        }
        recvbuf = (char *)malloc((size_t)(total_bytes));
    }

    MPI_Gatherv(sendbuf, my_size, MPI_CHAR, recvbuf, recvcounts, displs, MPI_CHAR, 0, MPI_COMM_WORLD);

    /* The 'recvcounts' and 'displs' variables can be reallocated
     * for the next MPI_Gatherv operation...
     */
    if (mpi_rank == 0) {
        int *next     = (int *)recvtuples;
        displs        = (int *)realloc(displs, (size_t)((size_t)total_diff_count * sizeof(int)));
        recvcounts    = (int *)realloc(recvcounts, (size_t)((size_t)total_diff_count * sizeof(int)));
        indices       = (int *)malloc((size_t)total_diff_count * sizeof(int));
        displs[0]     = 0;
        recvcounts[0] = rank_diff_count;

        next += 2;
        for (k = 1; k < mpi_size; k++) {
            recvcounts[k] = next[1];
            displs[k]     = displs[k - 1] + recvcounts[k - 1];
            next += 2;
        }
    }

    MPI_Gatherv(rankIDs, rank_diff_count, MPI_INT, indices, recvcounts, displs, MPI_INT, 0, MPI_COMM_WORLD);

    if (mpi_rank == 0) {
        char *        next         = recvbuf;
        print_objs_t *ordered_objs = (print_objs_t *)HDcalloc((size_t)total_diff_count, sizeof(print_objs_t));
        for (k = 0; k < total_diff_count; k++) {
            size_t len;
            len                       = strlen(next);
            ordered_objs[k].obj_idx   = indices[k];
            ordered_objs[k].obj_diffs = next;
            next += (len + 1);
        }
        qsort(ordered_objs, (size_t)total_diff_count, sizeof(print_objs_t), compare_objIDs);
        for (k = 0; k < total_diff_count; k++) {
            printf("%s", ordered_objs[k].obj_diffs);
        }
        HDfree(ordered_objs);
    }

    fflush(stdout);
    MPI_Barrier(MPI_COMM_WORLD);

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (recvtuples)
        HDfree(recvtuples);
    if (indices)
        HDfree(indices);
    if (rankIDs)
        HDfree(rankIDs);
    if ((rank_diff_count > 1) && sendbuf)
        HDfree(sendbuf);
    if (recvbuf)
        HDfree(recvbuf);
}

/*
 *  NEWER Version of the ph5diff implementation
 */
hsize_t
ph5diff(const char *fname1, const char *fname2, const char *objname1, const char *objname2, diff_opt_t *opts)
{
    hid_t   file1_id = H5I_INVALID_HID;
    hid_t   file2_id = H5I_INVALID_HID;
    hid_t   fapl1_id = H5P_DEFAULT;
    hid_t   fapl2_id = H5P_DEFAULT;
    char    filenames[2][MAX_FILENAME];
    hsize_t nfound = 0;

    char *obj1fullname  = NULL;
    char *obj2fullname  = NULL;
    int   both_objs_grp = 0;

    /* for group object */
    trav_info_t *info1_grp = NULL;
    trav_info_t *info2_grp = NULL;

    /* local pointer */
    trav_info_t *      info1_lp = NULL;
    trav_info_t *      info2_lp = NULL;
    h5tool_link_info_t trg_linfo1;
    h5tool_link_info_t trg_linfo2;

    /* list for common objects */
    trav_table_t *match_list = NULL;
    diff_err_t    ret_value  = H5DIFF_NO_ERR;

    H5TOOLS_START_DEBUG(" ");
    /* init filenames */
    HDmemset(filenames, 0, MAX_FILENAME * 2);
    /* init link info struct */
    HDmemset(&trg_linfo1, 0, sizeof(h5tool_link_info_t));
    HDmemset(&trg_linfo2, 0, sizeof(h5tool_link_info_t));

    /*-------------------------------------------------------------------------
     * check invalid combination of options
     *-----------------------------------------------------------------------*/
    if (!is_valid_options(opts))
        H5TOOLS_GOTO_DONE(0);

    opts->cmn_objs = 1;             /* eliminate warning */
    opts->err_stat = H5DIFF_NO_ERR; /* initialize error status */

    if (h5diff_open(fname1, fname2, opts, &file1_id, &file2_id) < 0) {
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "file open failure\n");
    }
    if (h5diff_initialize_lists(fname1, fname2, objname1, objname2, file1_id, file2_id, opts, &obj1fullname,
                                &info1_lp, &obj2fullname, &info2_lp, &both_objs_grp, &match_list) < 0) {

        goto done;
    }

    if (both_objs_grp) {
        /*------------------------------------------------------
         * print the list
         */
        if (opts->mode_verbose) {
            unsigned u;

#if defined(H5_HAVE_PARALLEL)
            if (g_nID == 0) {
                if (g_Parallel) {
                    char *           outbuff  = NULL;
                    diff_instance_t *new_diff = (diff_instance_t *)malloc(sizeof(diff_instance_t));
                    HDassert((new_diff != NULL));
                    outbuff = (char *)malloc(OUTBUFF_SIZE);
                    HDassert((outbuff != NULL));
                    new_diff->obj_idx       = 0;
                    new_diff->outbuff_size  = OUTBUFF_SIZE;
                    new_diff->outbuffoffset = 0;
                    new_diff->outbuff       = outbuff;
                    if (rank_diff_count == 0) {
                        rank_diffs = (diff_instance_t **)calloc(DIFF_COUNT, sizeof(void *));
                        rank_diffs[rank_diff_count++] = new_diff;
                    }
                    current_diff = new_diff;
                }
#endif

                if (opts->mode_verbose_level > 2) {
                    parallel_print("file1: %s\n", fname1);
                    parallel_print("file2: %s\n", fname2);
                }

                parallel_print("\n");
                /* if given objects is group under root */
                if (HDstrcmp(obj1fullname, "/") != 0 || HDstrcmp(obj2fullname, "/") != 0)
                    parallel_print("group1   group2\n");
                else
                    parallel_print("file1     file2\n");
                parallel_print("---------------------------------------\n");
                for (u = 0; u < match_list->nobjs; u++) {
                    int c1, c2;
                    c1 = (match_list->objs[u].flags[0]) ? 'x' : ' ';
                    c2 = (match_list->objs[u].flags[1]) ? 'x' : ' ';
                    parallel_print("%5c %6c    %-15s\n", c1, c2, match_list->objs[u].name);
                } /* end for */
                parallel_print("\n");
#if defined(H5_HAVE_PARALLEL)
            }
#endif
        } /* end if */
    }

    H5TOOLS_DEBUG("diff_match next - errstat:%d", opts->err_stat);
    nfound = diff_match(file1_id, obj1fullname, info1_lp, file2_id, obj2fullname, info2_lp, match_list, opts);
    H5TOOLS_DEBUG("diff_match nfound: %d - errstat:%d", nfound, opts->err_stat);

    ph5diff_gather_diffs();

done:
    opts->err_stat = opts->err_stat | ret_value;

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
        if (fapl1_id != H5P_DEFAULT)
            H5Pclose(fapl1_id);
        if (fapl2_id != H5P_DEFAULT)
            H5Pclose(fapl2_id);
    }
    H5E_END_TRY;

    H5TOOLS_ENDDEBUG(" - errstat:%d", opts->err_stat);

    return nfound;
}
#endif

/*-------------------------------------------------------------------------
 * Function: h5diff
 *
 * Purpose:  public function, can be called in an application program.
 *           return differences between 2 HDF5 files
 *
 * Return:   Number of differences found.
 *-------------------------------------------------------------------------
 */
hsize_t
h5diff(const char *fname1, const char *fname2, const char *objname1, const char *objname2, diff_opt_t *opts)
{
    hid_t   file1_id = H5I_INVALID_HID;
    hid_t   file2_id = H5I_INVALID_HID;
    hid_t   fapl1_id = H5P_DEFAULT;
    hid_t   fapl2_id = H5P_DEFAULT;
    char    filenames[2][MAX_FILENAME];
    hsize_t nfound        = 0;
    int     l_ret1        = -1;
    int     l_ret2        = -1;
    char *  obj1fullname  = NULL;
    char *  obj2fullname  = NULL;
    int     both_objs_grp = 0;
    /* init to group type */
    h5trav_type_t obj1type = H5TRAV_TYPE_GROUP;
    h5trav_type_t obj2type = H5TRAV_TYPE_GROUP;
    /* for single object */
    H5O_info2_t  oinfo1, oinfo2; /* object info */
    trav_info_t *info1_obj = NULL;
    trav_info_t *info2_obj = NULL;
    /* for group object */
    trav_info_t *info1_grp = NULL;
    trav_info_t *info2_grp = NULL;
    /* local pointer */
    trav_info_t *info1_lp = NULL;
    trav_info_t *info2_lp = NULL;
    /* link info from specified object */
    H5L_info2_t src_linfo1;
    H5L_info2_t src_linfo2;
    /* link info from member object */
    h5tool_link_info_t trg_linfo1;
    h5tool_link_info_t trg_linfo2;
    /* list for common objects */
    trav_table_t *match_list = NULL;
    diff_err_t    ret_value  = H5DIFF_NO_ERR;

    H5TOOLS_START_DEBUG(" ");
    /* init filenames */
    HDmemset(filenames, 0, MAX_FILENAME * 2);
    /* init link info struct */
    HDmemset(&trg_linfo1, 0, sizeof(h5tool_link_info_t));
    HDmemset(&trg_linfo2, 0, sizeof(h5tool_link_info_t));

    /*-------------------------------------------------------------------------
     * check invalid combination of options
     *-----------------------------------------------------------------------*/
    if (!is_valid_options(opts))
        H5TOOLS_GOTO_DONE(0);

    opts->cmn_objs = 1;             /* eliminate warning */
    opts->err_stat = H5DIFF_NO_ERR; /* initialize error status */

    /*-------------------------------------------------------------------------
     * open the files first; if they are not valid, no point in continuing
     *-------------------------------------------------------------------------
     */
    /* open file 1 */
    if (opts->custom_vol[0] || opts->custom_vfd[0]) {
        if ((fapl1_id = h5tools_get_fapl(H5P_DEFAULT, opts->custom_vol[0] ? &(opts->vol_info[0]) : NULL,
                                         opts->custom_vfd[0] ? &(opts->vfd_info[0]) : NULL)) < 0) {
            parallel_print("h5diff: unable to create fapl for input file\n");
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "unable to create input fapl\n");
        }
    }

    if ((file1_id = h5tools_fopen(fname1, H5F_ACC_RDONLY, fapl1_id, (fapl1_id != H5P_DEFAULT), NULL,
                                  (size_t)0)) < 0) {
        parallel_print("h5diff: <%s>: unable to open file\n", fname1);
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "<%s>: unable to open file\n", fname1);
    }
    H5TOOLS_DEBUG("file1_id = %s", fname1);

    /* open file 2 */

    if (opts->custom_vol[1] || opts->custom_vfd[1]) {
        if ((fapl2_id = h5tools_get_fapl(H5P_DEFAULT, opts->custom_vol[1] ? &(opts->vol_info[1]) : NULL,
                                         opts->custom_vfd[1] ? &(opts->vfd_info[1]) : NULL)) < 0) {
            parallel_print("h5diff: unable to create fapl for output file\n");
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "unable to create output fapl\n");
        }
    }

    if ((file2_id = h5tools_fopen(fname2, H5F_ACC_RDONLY, fapl2_id, (fapl2_id != H5P_DEFAULT), NULL,
                                  (size_t)0)) < 0) {
        parallel_print("h5diff: <%s>: unable to open file\n", fname2);
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "<%s>: unable to open file\n", fname2);
    }
    H5TOOLS_DEBUG("file2_id = %s", fname2);

    /*-------------------------------------------------------------------------
     * Initialize the info structs
     *-------------------------------------------------------------------------
     */
    trav_info_init(fname1, file1_id, &info1_obj);
    trav_info_init(fname2, file2_id, &info2_obj);

    H5TOOLS_DEBUG("trav_info_init initialized");
    /* if any object is specified */
    if (objname1) {
        /* make the given object1 fullpath, start with "/"  */
        if (HDstrncmp(objname1, "/", 1) != 0) {
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if (HDasprintf(&obj1fullname, "/%s", objname1) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "name buffer allocation failed");
#else  /* H5_HAVE_ASPRINTF */
            /* (malloc 2 more for "/" and end-of-line) */
            if ((obj1fullname = (char *)HDmalloc(HDstrlen(objname1) + 2)) == NULL)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "name buffer allocation failed");

            HDstrcpy(obj1fullname, "/");
            HDstrcat(obj1fullname, objname1);
#endif /* H5_HAVE_ASPRINTF */
        }
        else
            obj1fullname = HDstrdup(objname1);
        H5TOOLS_DEBUG("obj1fullname = %s", obj1fullname);

        /* make the given object2 fullpath, start with "/" */
        if (HDstrncmp(objname2, "/", 1) != 0) {
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if (HDasprintf(&obj2fullname, "/%s", objname2) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "name buffer allocation failed");
#else  /* H5_HAVE_ASPRINTF */
            /* (malloc 2 more for "/" and end-of-line) */
            if ((obj2fullname = (char *)HDmalloc(HDstrlen(objname2) + 2)) == NULL)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            HDstrcpy(obj2fullname, "/");
            HDstrcat(obj2fullname, objname2);
#endif /* H5_HAVE_ASPRINTF */
        }
        else
            obj2fullname = HDstrdup(objname2);
        H5TOOLS_DEBUG("obj2fullname = %s", obj2fullname);

        /*----------------------------------------------------------
         * check if obj1 is root, group, single object or symlink
         */
        H5TOOLS_DEBUG("h5diff check if obj1=%s is root, group, single object or symlink", obj1fullname);
        if (!HDstrcmp(obj1fullname, "/")) {
            obj1type = H5TRAV_TYPE_GROUP;
        }
        else {
            /* check if link itself exist */
            if (H5Lexists(file1_id, obj1fullname, H5P_DEFAULT) <= 0) {
                parallel_print("Object <%s> could not be found in <%s>\n", obj1fullname, fname1);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Error: Object could not be found");
            }
            /* get info from link */
            if (H5Lget_info2(file1_id, obj1fullname, &src_linfo1, H5P_DEFAULT) < 0) {
                parallel_print("Unable to get link info from <%s>\n", obj1fullname);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Lget_info failed");
            }

            info1_lp = info1_obj;

            /*
             * check the type of specified path for hard and symbolic links
             */
            if (src_linfo1.type == H5L_TYPE_HARD) {
                size_t idx;

                /* optional data pass */
                info1_obj->opts = (diff_opt_t *)opts;

                if (H5Oget_info_by_name3(file1_id, obj1fullname, &oinfo1, H5O_INFO_BASIC, H5P_DEFAULT) < 0) {
                    parallel_print("Error: Could not get file contents\n");
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Error: Could not get file contents");
                }
                obj1type = (h5trav_type_t)oinfo1.type;
                trav_info_add(info1_obj, obj1fullname, obj1type);
                idx = info1_obj->nused - 1;
                HDmemcpy(&info1_obj->paths[idx].obj_token, &oinfo1.token, sizeof(H5O_token_t));
                info1_obj->paths[idx].fileno = oinfo1.fileno;
            }
            else if (src_linfo1.type == H5L_TYPE_SOFT) {
                obj1type = H5TRAV_TYPE_LINK;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
            else if (src_linfo1.type == H5L_TYPE_EXTERNAL) {
                obj1type = H5TRAV_TYPE_UDLINK;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
        }

        /*----------------------------------------------------------
         * check if obj2 is root, group, single object or symlink
         */
        H5TOOLS_DEBUG("h5diff check if obj2=%s is root, group, single object or symlink", obj2fullname);
        if (!HDstrcmp(obj2fullname, "/")) {
            obj2type = H5TRAV_TYPE_GROUP;
        }
        else {
            /* check if link itself exist */
            if (H5Lexists(file2_id, obj2fullname, H5P_DEFAULT) <= 0) {
                parallel_print("Object <%s> could not be found in <%s>\n", obj2fullname, fname2);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Error: Object could not be found");
            }
            /* get info from link */
            if (H5Lget_info2(file2_id, obj2fullname, &src_linfo2, H5P_DEFAULT) < 0) {
                parallel_print("Unable to get link info from <%s>\n", obj2fullname);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Lget_info failed");
            }

            info2_lp = info2_obj;

            /*
             * check the type of specified path for hard and symbolic links
             */
            if (src_linfo2.type == H5L_TYPE_HARD) {
                size_t idx;

                /* optional data pass */
                info2_obj->opts = (diff_opt_t *)opts;

                if (H5Oget_info_by_name3(file2_id, obj2fullname, &oinfo2, H5O_INFO_BASIC, H5P_DEFAULT) < 0) {
                    parallel_print("Error: Could not get file contents\n");
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Error: Could not get file contents");
                }
                obj2type = (h5trav_type_t)oinfo2.type;
                trav_info_add(info2_obj, obj2fullname, obj2type);
                idx = info2_obj->nused - 1;
                HDmemcpy(&info2_obj->paths[idx].obj_token, &oinfo2.token, sizeof(H5O_token_t));
                info2_obj->paths[idx].fileno = oinfo2.fileno;
            }
            else if (src_linfo2.type == H5L_TYPE_SOFT) {
                obj2type = H5TRAV_TYPE_LINK;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
            else if (src_linfo2.type == H5L_TYPE_EXTERNAL) {
                obj2type = H5TRAV_TYPE_UDLINK;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
        }
    }
    /* if no object specified */
    else {
        H5TOOLS_DEBUG("h5diff no object specified");
        /* set root group */
        obj1fullname = (char *)HDstrdup("/");
        obj1type     = H5TRAV_TYPE_GROUP;
        obj2fullname = (char *)HDstrdup("/");
        obj2type     = H5TRAV_TYPE_GROUP;
    }

    H5TOOLS_DEBUG("get any symbolic links info - errstat:%d", opts->err_stat);
    /* get any symbolic links info */
    l_ret1 = H5tools_get_symlink_info(file1_id, obj1fullname, &trg_linfo1, opts->follow_links);
    l_ret2 = H5tools_get_symlink_info(file2_id, obj2fullname, &trg_linfo2, opts->follow_links);

    /*---------------------------------------------
     * check for following symlinks
     */
    if (opts->follow_links) {
        /* pass how to handle printing warning to linkinfo option */
        if (print_warn(opts))
            trg_linfo1.opt.msg_mode = trg_linfo2.opt.msg_mode = 1;

        /*-------------------------------
         * check symbolic link (object1)
         */
        H5TOOLS_DEBUG("h5diff check symbolic link (object1)");
        /* dangling link */
        if (l_ret1 == 0) {
            H5TOOLS_DEBUG("h5diff ... dangling link");
            if (opts->no_dangle_links) {
                /* treat dangling link as error */
                if (opts->mode_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", obj1fullname);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "treat dangling link as error");
            }
            else {
                if (opts->mode_verbose)
                    parallel_print("obj1 <%s> is a dangling link.\n", obj1fullname);
                if (l_ret1 != 0 || l_ret2 != 0) {
                    nfound++;
                    print_found(nfound);
                    H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
                }
            }
        }
        else if (l_ret1 < 0) { /* fail */
            parallel_print("Object <%s> could not be found in <%s>\n", obj1fullname, fname1);
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Object could not be found");
        }
        else if (l_ret1 != 2) { /* symbolic link */
            obj1type = (h5trav_type_t)trg_linfo1.trg_type;
            H5TOOLS_DEBUG("h5diff ... ... trg_linfo1.trg_type == H5L_TYPE_HARD");
            if (info1_lp != NULL) {
                size_t idx = info1_lp->nused - 1;

                H5TOOLS_DEBUG("h5diff ... ... ... info1_obj not null");
                HDmemcpy(&info1_lp->paths[idx].obj_token, &trg_linfo1.obj_token, sizeof(H5O_token_t));
                info1_lp->paths[idx].type   = (h5trav_type_t)trg_linfo1.trg_type;
                info1_lp->paths[idx].fileno = trg_linfo1.fileno;
            }
            H5TOOLS_DEBUG("h5diff check symbolic link (object1) finished");
        }

        /*-------------------------------
         * check symbolic link (object2)
         */
        H5TOOLS_DEBUG("h5diff check symbolic link (object2)");
        /* dangling link */
        if (l_ret2 == 0) {
            H5TOOLS_DEBUG("h5diff ... dangling link");
            if (opts->no_dangle_links) {
                /* treat dangling link as error */
                if (opts->mode_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", obj2fullname);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "treat dangling link as error");
            }
            else {
                if (opts->mode_verbose)
                    parallel_print("obj2 <%s> is a dangling link.\n", obj2fullname);
                if (l_ret1 != 0 || l_ret2 != 0) {
                    nfound++;
                    print_found(nfound);
                    H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
                }
            }
        }
        else if (l_ret2 < 0) { /* fail */
            parallel_print("Object <%s> could not be found in <%s>\n", obj2fullname, fname2);
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Object could not be found");
        }
        else if (l_ret2 != 2) { /* symbolic link */
            obj2type = (h5trav_type_t)trg_linfo2.trg_type;
            if (info2_lp != NULL) {
                size_t idx = info2_lp->nused - 1;

                H5TOOLS_DEBUG("h5diff ... ... ... info2_obj not null");
                HDmemcpy(&info2_lp->paths[idx].obj_token, &trg_linfo2.obj_token, sizeof(H5O_token_t));
                info2_lp->paths[idx].type   = (h5trav_type_t)trg_linfo2.trg_type;
                info2_lp->paths[idx].fileno = trg_linfo2.fileno;
            }
            H5TOOLS_DEBUG("h5diff check symbolic link (object1) finished");
        }
    } /* end of if follow symlinks */

    /*
     * If verbose options is not used, don't need to traverse through the list
     * of objects in the group to display objects information,
     * So use h5tools_is_obj_same() to improve performance by skipping
     * comparing details of same objects.
     */

    if (!(opts->mode_verbose || opts->mode_report)) {
        H5TOOLS_DEBUG("h5diff NOT (opts->mode_verbose || opts->mode_report)");
        /* if no danglink links */
        if (l_ret1 > 0 && l_ret2 > 0)
            if (h5tools_is_obj_same(file1_id, obj1fullname, file2_id, obj2fullname) != 0)
                H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
    }

    both_objs_grp = (obj1type == H5TRAV_TYPE_GROUP && obj2type == H5TRAV_TYPE_GROUP);
    if (both_objs_grp) {
        H5TOOLS_DEBUG("h5diff both_objs_grp TRUE");
        /*
         * traverse group1
         */
        trav_info_init(fname1, file1_id, &info1_grp);
        /* optional data pass */
        info1_grp->opts = (diff_opt_t *)opts;

        if (h5trav_visit(file1_id, obj1fullname, TRUE, TRUE, trav_grp_objs, trav_grp_symlinks, info1_grp,
                         H5O_INFO_BASIC) < 0) {
            parallel_print("Error: Could not get file contents\n");
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Could not get file contents");
        }
        info1_lp = info1_grp;

        /*
         * traverse group2
         */
        trav_info_init(fname2, file2_id, &info2_grp);
        /* optional data pass */
        info2_grp->opts = (diff_opt_t *)opts;

        if (h5trav_visit(file2_id, obj2fullname, TRUE, TRUE, trav_grp_objs, trav_grp_symlinks, info2_grp,
                         H5O_INFO_BASIC) < 0) {
            parallel_print("Error: Could not get file contents\n");
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Could not get file contents");
        } /* end if */
        info2_lp = info2_grp;
    }
    H5TOOLS_DEBUG("groups traversed - errstat:%d", opts->err_stat);

#if defined(H5_HAVE_PARALLEL) && defined(USE_ORIGINAL_CODE)
    if (g_Parallel) {
        int i;

        if ((HDstrlen(fname1) > MAX_FILENAME) || (HDstrlen(fname2) > MAX_FILENAME)) {
            HDfprintf(stderr, "The parallel diff only supports path names up to %d characters\n",
                      MAX_FILENAME);
            MPI_Abort(MPI_COMM_WORLD, 0);
        } /* end if */

        HDstrcpy(filenames[0], fname1);
        HDstrcpy(filenames[1], fname2);

        /* Alert the worker tasks that there's going to be work. */
        for (i = 1; i < g_nTasks; i++)
            MPI_Send(filenames, (MAX_FILENAME * 2), MPI_CHAR, i, MPI_TAG_PARALLEL, MPI_COMM_WORLD);
    } /* end if */
#endif

    H5TOOLS_DEBUG("build_match_list next - errstat:%d", opts->err_stat);
    /* process the objects */
    build_match_list(obj1fullname, info1_lp, obj2fullname, info2_lp, &match_list, opts);
    H5TOOLS_DEBUG("build_match_list finished - errstat:%d", opts->err_stat);
    if (both_objs_grp) {

        /*------------------------------------------------------
         * print the list
         */
        if (opts->mode_verbose) {
            unsigned u;
            if (opts->mode_verbose_level > 2) {
                parallel_print("file1: %s\n", fname1);
                parallel_print("file2: %s\n", fname2);
            }

            parallel_print("\n");
            /* if given objects is group under root */
            if (HDstrcmp(obj1fullname, "/") != 0 || HDstrcmp(obj2fullname, "/") != 0)
                parallel_print("group1   group2\n");
            else
                parallel_print("file1     file2\n");
            parallel_print("---------------------------------------\n");
            for (u = 0; u < match_list->nobjs; u++) {
                int c1, c2;
                c1 = (match_list->objs[u].flags[0]) ? 'x' : ' ';
                c2 = (match_list->objs[u].flags[1]) ? 'x' : ' ';
                parallel_print("%5c %6c    %-15s\n", c1, c2, match_list->objs[u].name);
            } /* end for */
            parallel_print("\n");
        } /* end if */
    }
    H5TOOLS_DEBUG("diff_match next - errstat:%d", opts->err_stat);
    nfound = diff_match(file1_id, obj1fullname, info1_lp, file2_id, obj2fullname, info2_lp, match_list, opts);
    H5TOOLS_DEBUG("diff_match nfound: %d - errstat:%d", nfound, opts->err_stat);

done:
    opts->err_stat = opts->err_stat | ret_value;

#if defined(H5_HAVE_PARALLEL) && defined(USE_ORIGINAL_CODE)
    if (g_Parallel)
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
        if (fapl1_id != H5P_DEFAULT)
            H5Pclose(fapl1_id);
        if (fapl2_id != H5P_DEFAULT)
            H5Pclose(fapl2_id);
    }
    H5E_END_TRY;

    H5TOOLS_ENDDEBUG(" - errstat:%d", opts->err_stat);

    return nfound;
}

#ifdef H5_HAVE_PARALLEL

hsize_t
pdiff_match(hid_t file1_id, const char *grp1, trav_info_t *info1, hid_t file2_id, const char *grp2,
            trav_info_t *info2, trav_table_t *table, diff_opt_t *opts)
{
    hsize_t     nfound = 0;
    unsigned    i;
    const char *grp1_path     = "";
    const char *grp2_path     = "";
    char *      obj1_fullpath = NULL;
    char *      obj2_fullpath = NULL;
    diff_args_t argdata;
    size_t      idx1      = 0;
    size_t      idx2      = 0;
    diff_err_t  ret_value = opts->err_stat;

    int mpi_rank = 0, mpi_size = 1;
    int diff_count = 0;
    int mod_rank   = -1;

    if (g_Parallel) {
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
        MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    }

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);
    /*
     * if not root, prepare object name to be pre-appended to group path to
     * make full path
     */
    if (HDstrcmp(grp1, "/") != 0)
        grp1_path = grp1;
    if (HDstrcmp(grp2, "/") != 0)
        grp2_path = grp2;

    /*-------------------------------------------------------------------------
     * regarding the return value of h5diff (0, no difference in files, 1 difference )
     * 1) the number of objects in file1 must be the same as in file2
     * 2) the graph must match, i.e same names (absolute path)
     * 3) objects with the same name must be of the same type
     *-------------------------------------------------------------------------
     */

    H5TOOLS_DEBUG("exclude_path opts->contents:%d", opts->contents);
    /* not valid compare used when --exclude-path option is used */
    if (!opts->exclude_path) {
        /* number of different objects */
        if (info1->nused != info2->nused) {
            opts->contents = 0;
        }
        H5TOOLS_DEBUG("opts->exclude_path opts->contents:%d", opts->contents);
    }

    /* objects in one file and not the other */
    for (i = 0; i < table->nobjs; i++) {
        if (table->objs[i].flags[0] != table->objs[i].flags[1]) {
            opts->contents = 0;
            break;
        }
        H5TOOLS_DEBUG("table->nobjs[%d] opts->contents:%d", i, opts->contents);
    }

    /*-------------------------------------------------------------------------
     * do the diff for common objects
     *-------------------------------------------------------------------------
     */

    for (i = 0; i < table->nobjs; i++) {
        H5TOOLS_DEBUG("diff for common objects[%d] - errstat:%d", i, opts->err_stat);
        if (table->objs[i].flags[0] && table->objs[i].flags[1]) {
            /* make full path for obj1 */
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if (HDasprintf(&obj1_fullpath, "%s%s", grp1_path, table->objs[i].name) < 0) {
                H5TOOLS_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
#else  /* H5_HAVE_ASPRINTF */
            if ((obj1_fullpath = (char *)HDmalloc(HDstrlen(grp1_path) + HDstrlen(table->objs[i].name) + 1)) ==
                NULL) {
                H5TOOLS_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
            else {
                HDstrcpy(obj1_fullpath, grp1_path);
                HDstrcat(obj1_fullpath, table->objs[i].name);
            }
#endif /* H5_HAVE_ASPRINTF */
            H5TOOLS_DEBUG("diff_match path1 - %s", obj1_fullpath);

            /* make full path for obj2 */
#ifdef H5_HAVE_ASPRINTF
            /* Use the asprintf() routine, since it does what we're trying to do below */
            if (HDasprintf(&obj2_fullpath, "%s%s", grp2_path, table->objs[i].name) < 0) {
                H5TOOLS_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
#else  /* H5_HAVE_ASPRINTF */
            if ((obj2_fullpath = (char *)HDmalloc(HDstrlen(grp2_path) + HDstrlen(table->objs[i].name) + 1)) ==
                NULL) {
                H5TOOLS_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
            else {
                HDstrcpy(obj2_fullpath, grp2_path);
                HDstrcat(obj2_fullpath, table->objs[i].name);
            }
#endif /* H5_HAVE_ASPRINTF */
            H5TOOLS_DEBUG("diff_match path2 - %s", obj2_fullpath);

            /* get index to figure out type of the object in file1 */
            while (info1->paths[idx1].path && (HDstrcmp(obj1_fullpath, info1->paths[idx1].path) != 0))
                idx1++;
            /* get index to figure out type of the object in file2 */
            while (info2->paths[idx2].path && (HDstrcmp(obj2_fullpath, info2->paths[idx2].path) != 0))
                idx2++;

            /* Set argdata to pass other args into diff() */
            argdata.type[0]        = info1->paths[idx1].type;
            argdata.type[1]        = info2->paths[idx2].type;
            argdata.is_same_trgobj = table->objs[i].is_same_trgobj;

            opts->cmn_objs = 1;

            mod_rank = diff_count % mpi_size;
            if (mod_rank == mpi_rank) {
                char *           outbuff  = NULL;
                diff_instance_t *new_diff = (diff_instance_t *)malloc(sizeof(diff_instance_t));
                HDassert((new_diff != NULL));
                outbuff = (char *)malloc(OUTBUFF_SIZE);
                HDassert((outbuff != NULL));
                new_diff->obj_idx       = diff_count;
                new_diff->outbuff_size  = OUTBUFF_SIZE;
                new_diff->outbuffoffset = 0;
                new_diff->outbuff       = outbuff;

                if (rank_diff_count == 0) {
                    rank_diffs = (diff_instance_t **)calloc(DIFF_COUNT, sizeof(void *));
                }
                else if (rank_diff_count == diff_count_limit) {
                    diff_count_limit += DIFF_COUNT;
                    rank_diffs = (diff_instance_t **)realloc(rank_diffs, (size_t)diff_count_limit);
                }
                rank_diffs[rank_diff_count++] = new_diff;
                my_diffs                      = new_diff;
                /* Add a previous outbuff (length) into the accumulated total for this rank */
                if (current_diff)
                    local_diff_total += current_diff->outbuffoffset;
                /* Assign a new diff to be the current (working) outbuff */
                current_diff = new_diff;
                nfound += diff(file1_id, obj1_fullpath, file2_id, obj2_fullpath, opts, &argdata);
            }
            diff_count++;
            if (obj1_fullpath) {
                HDfree(obj1_fullpath);
                obj1_fullpath = NULL;
            }
            if (obj2_fullpath) {
                HDfree(obj2_fullpath);
                obj2_fullpath = NULL;
            }
        }

    } /* end for (common objects) */
    H5TOOLS_DEBUG("done with for loop - errstat:%d", opts->err_stat);

    /* Update the final 'local_diff_total' */
    if (current_diff != NULL) {
        local_diff_total += current_diff->outbuffoffset;
    }

    opts->err_stat = opts->err_stat | ret_value;

    free_exclude_attr_list(opts);

    /* free table */
    if (table)
        trav_table_free(table);

    H5TOOLS_ENDDEBUG(" diffs=%d - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

#endif /* H5_HAVE_PARALLEL */

/*-------------------------------------------------------------------------
 * Function: diff_match
 *
 * Purpose:  Compare common objects in given groups according to table structure.
 *           The table structure has flags which can be used to find common objects
 *           and will be compared.
 *           Common object means same name (absolute path) objects in both location.
 *
 * Return:   Number of differences found
 *
 * Modifications: Compare the graph and make h5diff return 1 for difference if
 *           1) the number of objects in file1 is not the same as in file2
 *           2) the graph does not match, i.e same names (absolute path)
 *           3) objects with the same name are not of the same type
 *-------------------------------------------------------------------------
 */
hsize_t
diff_match(hid_t file1_id, const char *grp1, trav_info_t *info1, hid_t file2_id, const char *grp2,
           trav_info_t *info2, trav_table_t *table, diff_opt_t *opts)
{
#if defined(H5_HAVE_PARALLEL) && !defined(USE_ORIGINAL_CODE)
    if (g_Parallel)
        return pdiff_match(file1_id, grp1, info1, file2_id, grp2, info2, table, opts);
#endif

    hsize_t     nfound = 0;
    unsigned    i;
    const char *grp1_path     = "";
    const char *grp2_path     = "";
    char *      obj1_fullpath = NULL;
    char *      obj2_fullpath = NULL;
    diff_args_t argdata;
    size_t      idx1      = 0;
    size_t      idx2      = 0;
    diff_err_t  ret_value = opts->err_stat;

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);
    /*
     * if not root, prepare object name to be pre-appended to group path to
     * make full path
     */
    if (HDstrcmp(grp1, "/") != 0)
        grp1_path = grp1;
    if (HDstrcmp(grp2, "/") != 0)
        grp2_path = grp2;

    /*-------------------------------------------------------------------------
     * regarding the return value of h5diff (0, no difference in files, 1 difference )
     * 1) the number of objects in file1 must be the same as in file2
     * 2) the graph must match, i.e same names (absolute path)
     * 3) objects with the same name must be of the same type
     *-------------------------------------------------------------------------
     */

    H5TOOLS_DEBUG("exclude_path opts->contents:%d", opts->contents);
    /* not valid compare used when --exclude-path option is used */
    if (!opts->exclude_path) {
        /* number of different objects */
        if (info1->nused != info2->nused) {
            opts->contents = 0;
        }
        H5TOOLS_DEBUG("opts->exclude_path opts->contents:%d", opts->contents);
    }

    /* objects in one file and not the other */
    for (i = 0; i < table->nobjs; i++) {
        if (table->objs[i].flags[0] != table->objs[i].flags[1]) {
            opts->contents = 0;
            break;
        }
        H5TOOLS_DEBUG("table->nobjs[%d] opts->contents:%d", i, opts->contents);
    }

    /*-------------------------------------------------------------------------
     * do the diff for common objects
     *-------------------------------------------------------------------------
     */
#if defined(H5_HAVE_PARALLEL) && defined(USE_ORIGINAL_CODE)
    {
        char *               workerTasks = (char *)HDmalloc((size_t)(g_nTasks - 1) * sizeof(char));
        int                  n;
        int                  busyTasks = 0;
        struct diffs_found   nFoundbyWorker;
        struct diff_mpi_args args;
        int                  havePrintToken = 1;
        MPI_Status           Status;

        /*set all tasks as free */
        HDmemset(workerTasks, 1, (size_t)(g_nTasks - 1) * sizeof(char));
#endif

        for (i = 0; i < table->nobjs; i++) {
            H5TOOLS_DEBUG("diff for common objects[%d] - errstat:%d", i, opts->err_stat);
            if (table->objs[i].flags[0] && table->objs[i].flags[1]) {
                /* make full path for obj1 */
#ifdef H5_HAVE_ASPRINTF
                /* Use the asprintf() routine, since it does what we're trying to do below */
                if (HDasprintf(&obj1_fullpath, "%s%s", grp1_path, table->objs[i].name) < 0) {
                    H5TOOLS_ERROR(H5DIFF_ERR, "name buffer allocation failed");
                }
#else  /* H5_HAVE_ASPRINTF */
            if ((obj1_fullpath = (char *)HDmalloc(HDstrlen(grp1_path) + HDstrlen(table->objs[i].name) + 1)) ==
                NULL) {
                H5TOOLS_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
            else {
                HDstrcpy(obj1_fullpath, grp1_path);
                HDstrcat(obj1_fullpath, table->objs[i].name);
            }
#endif /* H5_HAVE_ASPRINTF */
                H5TOOLS_DEBUG("diff_match path1 - %s", obj1_fullpath);

                /* make full path for obj2 */
#ifdef H5_HAVE_ASPRINTF
                /* Use the asprintf() routine, since it does what we're trying to do below */
                if (HDasprintf(&obj2_fullpath, "%s%s", grp2_path, table->objs[i].name) < 0) {
                    H5TOOLS_ERROR(H5DIFF_ERR, "name buffer allocation failed");
                }
#else  /* H5_HAVE_ASPRINTF */
            if ((obj2_fullpath = (char *)HDmalloc(HDstrlen(grp2_path) + HDstrlen(table->objs[i].name) + 1)) ==
                NULL) {
                H5TOOLS_ERROR(H5DIFF_ERR, "name buffer allocation failed");
            }
            else {
                HDstrcpy(obj2_fullpath, grp2_path);
                HDstrcat(obj2_fullpath, table->objs[i].name);
            }
#endif /* H5_HAVE_ASPRINTF */
                H5TOOLS_DEBUG("diff_match path2 - %s", obj2_fullpath);

                /* get index to figure out type of the object in file1 */
                while (info1->paths[idx1].path && (HDstrcmp(obj1_fullpath, info1->paths[idx1].path) != 0))
                    idx1++;
                /* get index to figure out type of the object in file2 */
                while (info2->paths[idx2].path && (HDstrcmp(obj2_fullpath, info2->paths[idx2].path) != 0))
                    idx2++;

                /* Set argdata to pass other args into diff() */
                argdata.type[0]        = info1->paths[idx1].type;
                argdata.type[1]        = info2->paths[idx2].type;
                argdata.is_same_trgobj = table->objs[i].is_same_trgobj;

                opts->cmn_objs = 1;
                if (!g_Parallel) {
                    H5TOOLS_DEBUG("diff paths - errstat:%d", opts->err_stat);
                    nfound += diff(file1_id, obj1_fullpath, file2_id, obj2_fullpath, opts, &argdata);
                } /* end if */
#if defined(H5_HAVE_PARALLEL) && defined(USE_ORIGINAL_CODE)
                else {
                    int workerFound = 0;

                    H5TOOLS_DEBUG("Beginning of big else block");
                    /* We're in parallel mode */
                    /* Since the data type of diff value is hsize_t which can
                     * be arbitary large such that there is no MPI type that
                     * matches it, the value is passed between processes as
                     * an array of bytes in order to be portable.  But this
                     * may not work in non-homogeneous MPI environments.
                     */

                    /*Set up args to pass to worker task. */
                    if (HDstrlen(obj1_fullpath) > 255 || HDstrlen(obj2_fullpath) > 255) {
                        HDprintf("The parallel diff only supports object names up to 255 characters\n");
                        MPI_Abort(MPI_COMM_WORLD, 0);
                    } /* end if */

                    /* set args struct to pass */
                    HDstrcpy(args.name1, obj1_fullpath);
                    HDstrcpy(args.name2, obj2_fullpath);
                    args.opts                   = *opts;
                    args.argdata.type[0]        = info1->paths[idx1].type;
                    args.argdata.type[1]        = info2->paths[idx2].type;
                    args.argdata.is_same_trgobj = table->objs[i].is_same_trgobj;

                    /* if there are any outstanding print requests, let's handle one. */
                    if (busyTasks > 0) {
                        int incomingMessage;

                        /* check if any tasks freed up, and didn't need to print. */
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &incomingMessage, &Status);

                        /* first block*/
                        if (incomingMessage) {
                            workerTasks[Status.MPI_SOURCE - 1] = 1;
                            MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE,
                                     MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                            nfound += nFoundbyWorker.nfound;
                            opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                            busyTasks--;
                        } /* end if */

                        /* check to see if the print token was returned. */
                        if (!havePrintToken) {
                            /* If we don't have the token, someone is probably sending us output */
                            print_incoming_data();

                            /* check incoming queue for token */
                            MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage,
                                       &Status);

                            /* incoming token implies free task. */
                            if (incomingMessage) {
                                workerTasks[Status.MPI_SOURCE - 1] = 1;
                                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE,
                                         MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                                nfound += nFoundbyWorker.nfound;
                                opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                                busyTasks--;
                                havePrintToken = 1;
                            } /* end if */
                        }     /* end if */

                        /* check to see if anyone needs the print token. */
                        if (havePrintToken) {
                            /* check incoming queue for print token requests */
                            MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &incomingMessage,
                                       &Status);
                            if (incomingMessage) {
                                MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST,
                                         MPI_COMM_WORLD, &Status);
                                MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK,
                                         MPI_COMM_WORLD);
                                havePrintToken = 0;
                            } /* end if */
                        }     /* end if */
                    }         /* end if */

                    /* check array of tasks to see which ones are free.
                     * Manager task never does work, so freeTasks[0] is really
                     * worker task 0. */
                    for (n = 1; (n < g_nTasks) && !workerFound; n++) {
                        if (workerTasks[n - 1]) {
                            /* send file id's and names to first free worker */
                            MPI_Send(&args, sizeof(args), MPI_BYTE, n, MPI_TAG_ARGS, MPI_COMM_WORLD);

                            /* increment counter for total number of prints. */
                            busyTasks++;

                            /* mark worker as busy */
                            workerTasks[n - 1] = 0;
                            workerFound        = 1;
                        } /* end if */
                    }     /* end for */

                    if (!workerFound) {
                        /* if they were all busy, we've got to wait for one free up
                         *  before we can move on.  If we don't have the token, some
                         * task is currently printing so we'll wait for that task to
                         * return it.
                         */

                        if (!havePrintToken) {
                            while (!havePrintToken) {
                                int incomingMessage;

                                print_incoming_data();
                                MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD,
                                           &incomingMessage, &Status);
                                if (incomingMessage) {
                                    MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE,
                                             MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                                    havePrintToken = 1;
                                    nfound += nFoundbyWorker.nfound;
                                    opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                                    /* send this task the work unit. */
                                    MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS,
                                             MPI_COMM_WORLD);
                                } /* end if */
                            }     /* end while */
                        }         /* end if */
                        /* if we do have the token, check for task to free up, or wait for a task to request
                         * it */
                        else {
                            /* But first print all the data in our incoming queue */
                            print_incoming_data();
                            MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
                            if (Status.MPI_TAG == MPI_TAG_DONE) {
                                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE,
                                         MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                                nfound += nFoundbyWorker.nfound;
                                opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                                MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS,
                                         MPI_COMM_WORLD);
                            } /* end if */
                            else if (Status.MPI_TAG == MPI_TAG_TOK_REQUEST) {
                                int incomingMessage;

                                MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST,
                                         MPI_COMM_WORLD, &Status);
                                MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK,
                                         MPI_COMM_WORLD);

                                do {
                                    MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD,
                                               &incomingMessage, &Status);

                                    print_incoming_data();
                                } while (!incomingMessage);

                                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE,
                                         MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                                nfound += nFoundbyWorker.nfound;
                                opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                                MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS,
                                         MPI_COMM_WORLD);
                            } /* end else-if */
                            else {
                                HDprintf("ERROR: Invalid tag (%d) received \n", Status.MPI_TAG);
                                MPI_Abort(MPI_COMM_WORLD, 0);
                                MPI_Finalize();
                            } /* end else */
                        }     /* end else */
                    }         /* end if */
                }             /* end else */
#endif                        /* USE_ORIGINAL_CODE (PARALLEL) */

                if (obj1_fullpath)
                    HDfree(obj1_fullpath);
                if (obj2_fullpath)
                    HDfree(obj2_fullpath);
            } /* end if */
        }     /* end for */
        H5TOOLS_DEBUG("done with for loop - errstat:%d", opts->err_stat);

#if defined(H5_HAVE_PARALLEL) && defined(USE_ORIGINAL_CODE)
        if (g_Parallel) {
            /* make sure all tasks are done */
            while (busyTasks > 0) {
                MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
                if (Status.MPI_TAG == MPI_TAG_DONE) {
                    MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE,
                             MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                    nfound += nFoundbyWorker.nfound;
                    opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                    busyTasks--;
                } /* end if */
                else if (Status.MPI_TAG == MPI_TAG_TOK_REQUEST) {
                    MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD,
                             &Status);
                    if (havePrintToken) {
                        int incomingMessage;

                        MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);

                        do {
                            MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage,
                                       &Status);

                            print_incoming_data();
                        } while (!incomingMessage);

                        MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE,
                                 MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                        nfound += nFoundbyWorker.nfound;
                        opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                        busyTasks--;
                    } /* end if */
                    /* someone else must have it...wait for them to return it, then give it to the task that
                     * just asked for it. */
                    else {
                        int source = Status.MPI_SOURCE;
                        int incomingMessage;

                        do {
                            MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage,
                                       &Status);

                            print_incoming_data();
                        } while (!incomingMessage);

                        MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, MPI_ANY_SOURCE,
                                 MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                        nfound += nFoundbyWorker.nfound;
                        opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                        busyTasks--;
                        MPI_Send(NULL, 0, MPI_BYTE, source, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);
                    } /* end else */
                }     /* end else-if */
                else if (Status.MPI_TAG == MPI_TAG_TOK_RETURN) {
                    MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE,
                             MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                    nfound += nFoundbyWorker.nfound;
                    opts->not_cmp = opts->not_cmp | nFoundbyWorker.not_cmp;
                    busyTasks--;
                    havePrintToken = 1;
                } /* end else-if */
                else if (Status.MPI_TAG == MPI_TAG_PRINT_DATA) {
                    char data[PRINT_DATA_MAX_SIZE + 1];
                    HDmemset(data, 0, PRINT_DATA_MAX_SIZE + 1);

                    MPI_Recv(data, PRINT_DATA_MAX_SIZE, MPI_CHAR, Status.MPI_SOURCE, MPI_TAG_PRINT_DATA,
                             MPI_COMM_WORLD, &Status);

                    HDprintf("%s", data);
                } /* end else-if */
                else {
                    HDprintf("ph5diff-manager: ERROR!! Invalid tag (%d) received \n", Status.MPI_TAG);
                    MPI_Abort(MPI_COMM_WORLD, 0);
                } /* end else */
            }     /* end while */

            for (i = 1; (int)i < g_nTasks; i++)
                MPI_Send(NULL, 0, MPI_BYTE, (int)i, MPI_TAG_END, MPI_COMM_WORLD);

            /* Print any final data waiting in our queue */
            print_incoming_data();
        } /* end if */
        H5TOOLS_DEBUG("done with if block");

        HDfree(workerTasks);
    }
#endif /* USE_ORIGINAL_CODE (PARALLEL) */

    opts->err_stat = opts->err_stat | ret_value;

    free_exclude_attr_list(opts);

    /* free table */
    if (table)
        trav_table_free(table);

    H5TOOLS_ENDDEBUG(" diffs=%d - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose:  switch between types and choose the diff function
 *           TYPE is either
 *               H5G_GROUP         Object is a group
 *               H5G_DATASET       Object is a dataset
 *               H5G_TYPE          Object is a named data type
 *               H5G_LINK          Object is a symbolic link
 *
 * Return:   Number of differences found
 *-------------------------------------------------------------------------
 */
hsize_t
diff(hid_t file1_id, const char *path1, hid_t file2_id, const char *path2, diff_opt_t *opts,
     diff_args_t *argdata)
{
    int           status          = -1;
    hid_t         dset1_id        = H5I_INVALID_HID;
    hid_t         dset2_id        = H5I_INVALID_HID;
    hid_t         type1_id        = H5I_INVALID_HID;
    hid_t         type2_id        = H5I_INVALID_HID;
    hid_t         grp1_id         = H5I_INVALID_HID;
    hid_t         grp2_id         = H5I_INVALID_HID;
    hbool_t       is_dangle_link1 = FALSE;
    hbool_t       is_dangle_link2 = FALSE;
    hbool_t       is_hard_link    = FALSE;
    hsize_t       nfound          = 0;
    h5trav_type_t object_type;
    diff_err_t    ret_value = opts->err_stat;

    /* to get link info */
    h5tool_link_info_t linkinfo1;
    h5tool_link_info_t linkinfo2;

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);

    /*init link info struct */
    HDmemset(&linkinfo1, 0, sizeof(h5tool_link_info_t));
    HDmemset(&linkinfo2, 0, sizeof(h5tool_link_info_t));

    /* pass how to handle printing warnings to linkinfo option */
    if (print_warn(opts))
        linkinfo1.opt.msg_mode = linkinfo2.opt.msg_mode = 1;

    /* for symbolic links, take care follow symlink and no dangling link
     * options */
    if (argdata->type[0] == H5TRAV_TYPE_LINK || argdata->type[0] == H5TRAV_TYPE_UDLINK ||
        argdata->type[1] == H5TRAV_TYPE_LINK || argdata->type[1] == H5TRAV_TYPE_UDLINK) {
        /*
         * check dangling links for path1 and path2
         */

        H5TOOLS_DEBUG("diff links");
        /* target object1 - get type and name */
        if ((status = H5tools_get_symlink_info(file1_id, path1, &linkinfo1, opts->follow_links)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5tools_get_symlink_info failed");

        /* dangling link */
        if (status == 0) {
            if (opts->no_dangle_links) {
                /* dangling link is error */
                if (opts->mode_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", path1);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "dangling link is error");
            }
            else
                is_dangle_link1 = TRUE;
        }

        /* target object2 - get type and name */
        if ((status = H5tools_get_symlink_info(file2_id, path2, &linkinfo2, opts->follow_links)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5tools_get_symlink_info failed");
        /* dangling link */
        if (status == 0) {
            if (opts->no_dangle_links) {
                /* dangling link is error */
                if (opts->mode_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", path2);
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "dangling link is error");
            }
            else
                is_dangle_link2 = TRUE;
        }

        /* found dangling link */
        if (is_dangle_link1 || is_dangle_link2) {
            H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
        }

        /* follow symbolic link option */
        if (opts->follow_links) {
            if (linkinfo1.linfo.type == H5L_TYPE_SOFT || linkinfo1.linfo.type == H5L_TYPE_EXTERNAL)
                argdata->type[0] = (h5trav_type_t)linkinfo1.trg_type;

            if (linkinfo2.linfo.type == H5L_TYPE_SOFT || linkinfo2.linfo.type == H5L_TYPE_EXTERNAL)
                argdata->type[1] = (h5trav_type_t)linkinfo2.trg_type;
        }
    }
    /* if objects are not the same type */
    if (argdata->type[0] != argdata->type[1]) {
        H5TOOLS_DEBUG("diff objects are not the same");
        if (opts->mode_verbose || opts->mode_list_not_cmp) {
            parallel_print("Not comparable: <%s> is of type %s and <%s> is of type %s\n", path1,
                           get_type(argdata->type[0]), path2, get_type(argdata->type[1]));
        }

        opts->not_cmp = 1;
        /* TODO: will need to update non-comparable is different
         * opts->contents = 0;
         */
        H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
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
    if (argdata->is_same_trgobj) {
        H5TOOLS_DEBUG("argdata->is_same_trgobj");
        is_hard_link = (object_type == H5TRAV_TYPE_DATASET || object_type == H5TRAV_TYPE_NAMED_DATATYPE ||
                        object_type == H5TRAV_TYPE_GROUP);
        if (opts->follow_links || is_hard_link) {
            /* print information is only verbose option is used */
            if (opts->mode_verbose || opts->mode_report) {
                switch (object_type) {
                    case H5TRAV_TYPE_DATASET:
                        do_print_objname("dataset", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_NAMED_DATATYPE:
                        do_print_objname("datatype", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_GROUP:
                        do_print_objname("group", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_LINK:
                        do_print_objname("link", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_UDLINK:
                        if (linkinfo1.linfo.type == H5L_TYPE_EXTERNAL &&
                            linkinfo2.linfo.type == H5L_TYPE_EXTERNAL)
                            do_print_objname("external link", path1, path2, opts);
                        else
                            do_print_objname("user defined link", path1, path2, opts);
                        break;
                    case H5TRAV_TYPE_UNKNOWN:
                    default:
                        parallel_print("Comparison not supported: <%s> and <%s> are of type %s\n", path1,
                                       path2, get_type(object_type));
                        opts->not_cmp = 1;
                        break;
                } /* switch(type)*/

                print_found(nfound);
            } /* if(opts->mode_verbose || opts->mode_report) */

            /* exact same, so comparison is done */
            H5TOOLS_GOTO_DONE(H5DIFF_NO_ERR);
        }
    }

    switch (object_type) {
            /*----------------------------------------------------------------------
             * H5TRAV_TYPE_DATASET
             *----------------------------------------------------------------------
             */
        case H5TRAV_TYPE_DATASET:
            H5TOOLS_DEBUG("diff object type H5TRAV_TYPE_DATASET - errstat:%d", opts->err_stat);
            if ((dset1_id = H5Dopen2(file1_id, path1, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dopen2 failed");
            if ((dset2_id = H5Dopen2(file2_id, path2, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dopen2 failed");
            H5TOOLS_DEBUG("paths: %s - %s", path1, path2);
            /* verbose (-v) and report (-r) mode */
            if (opts->mode_verbose || opts->mode_report) {
                do_print_objname("dataset", path1, path2, opts);
                H5TOOLS_DEBUG("call diff_dataset 1:%s  2:%s ", path1, path2);
                nfound = diff_dataset(file1_id, file2_id, path1, path2, opts);
                print_found(nfound);
            }
            /* quiet mode (-q), just count differences */
            else if (opts->mode_quiet) {
                nfound = diff_dataset(file1_id, file2_id, path1, path2, opts);
            }
            /* the rest (-c, none, ...) */
            else {
                nfound = diff_dataset(file1_id, file2_id, path1, path2, opts);
                /* print info if difference found  */
                if (nfound) {
                    do_print_objname("dataset", path1, path2, opts);
                    print_found(nfound);
                }
            }
            H5TOOLS_DEBUG("diff after dataset:%d - errstat:%d", nfound, opts->err_stat);

            /*---------------------------------------------------------
             * compare attributes
             * if condition refers to cases when the dataset is a
             * referenced object
             *---------------------------------------------------------
             */
            if (path1 && !is_exclude_attr(path1, object_type, opts)) {
                H5TOOLS_DEBUG("call diff_attr 1:%s  2:%s ", path1, path2);
                nfound += diff_attr(dset1_id, dset2_id, path1, path2, opts);
            }

            if (H5Dclose(dset1_id) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dclose failed");
            if (H5Dclose(dset2_id) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dclose failed");
            break;

            /*----------------------------------------------------------------------
             * H5TRAV_TYPE_NAMED_DATATYPE
             *----------------------------------------------------------------------
             */
        case H5TRAV_TYPE_NAMED_DATATYPE:
            H5TOOLS_DEBUG("H5TRAV_TYPE_NAMED_DATATYPE 1:%s  2:%s ", path1, path2);
            if ((type1_id = H5Topen2(file1_id, path1, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Topen2 failed");
            if ((type2_id = H5Topen2(file2_id, path2, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Topen2 failed");

            if ((status = H5Tequal(type1_id, type2_id)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tequal failed");

            /* if H5Tequal is > 0 then the datatypes refer to the same datatype */
            nfound = (status > 0) ? 0 : 1;

            if (print_objname(opts, nfound))
                do_print_objname("datatype", path1, path2, opts);

            /* always print the number of differences found in verbose mode */
            if (opts->mode_verbose)
                print_found(nfound);

            /*-----------------------------------------------------------------
             * compare attributes
             * the if condition refers to cases when the dataset is a
             * referenced object
             *-----------------------------------------------------------------
             */
            if (path1 && !is_exclude_attr(path1, object_type, opts)) {
                H5TOOLS_DEBUG("call diff_attr 1:%s  2:%s ", path1, path2);
                nfound += diff_attr(type1_id, type2_id, path1, path2, opts);
            }

            if (H5Tclose(type1_id) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tclose failed");
            if (H5Tclose(type2_id) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tclose failed");
            break;

            /*----------------------------------------------------------------------
             * H5TRAV_TYPE_GROUP
             *----------------------------------------------------------------------
             */
        case H5TRAV_TYPE_GROUP:
            H5TOOLS_DEBUG("H5TRAV_TYPE_GROUP 1:%s  2:%s ", path1, path2);
            if (print_objname(opts, nfound))
                do_print_objname("group", path1, path2, opts);

            /* always print the number of differences found in verbose mode */
            if (opts->mode_verbose)
                print_found(nfound);

            if ((grp1_id = H5Gopen2(file1_id, path1, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Gclose failed");
            if ((grp2_id = H5Gopen2(file2_id, path2, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Gclose failed");

            /*-----------------------------------------------------------------
             * compare attributes
             * the if condition refers to cases when the dataset is a
             * referenced object
             *-----------------------------------------------------------------
             */
            if (path1 && !is_exclude_attr(path1, object_type, opts)) {
                H5TOOLS_DEBUG("call diff_attr 1:%s  2:%s ", path1, path2);
                nfound += diff_attr(grp1_id, grp2_id, path1, path2, opts);
            }

            if (H5Gclose(grp1_id) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Gclose failed");
            if (H5Gclose(grp2_id) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Gclose failed");
            break;

            /*----------------------------------------------------------------------
             * H5TRAV_TYPE_LINK
             *----------------------------------------------------------------------
             */
        case H5TRAV_TYPE_LINK: {
            H5TOOLS_DEBUG("H5TRAV_TYPE_LINK 1:%s  2:%s ", path1, path2);
            status = HDstrcmp(linkinfo1.trg_path, linkinfo2.trg_path);

            /* if the target link name is not same then the links are "different" */
            nfound = (status != 0) ? 1 : 0;

            if (print_objname(opts, nfound))
                do_print_objname("link", path1, path2, opts);

            /* always print the number of differences found in verbose mode */
            if (opts->mode_verbose)
                print_found(nfound);
        } break;

            /*----------------------------------------------------------------------
             * H5TRAV_TYPE_UDLINK
             *----------------------------------------------------------------------
             */
        case H5TRAV_TYPE_UDLINK: {
            H5TOOLS_DEBUG("H5TRAV_TYPE_UDLINK 1:%s  2:%s ", path1, path2);
            /* Only external links will have a query function registered */
            if (linkinfo1.linfo.type == H5L_TYPE_EXTERNAL && linkinfo2.linfo.type == H5L_TYPE_EXTERNAL) {
                /* If the buffers are the same size, compare them */
                if (linkinfo1.linfo.u.val_size == linkinfo2.linfo.u.val_size) {
                    status = HDmemcmp(linkinfo1.trg_path, linkinfo2.trg_path, linkinfo1.linfo.u.val_size);
                }
                else
                    status = 1;

                /* if "linkinfo1.trg_path" != "linkinfo2.trg_path" then the links
                 * are "different" extlinkinfo#.path is combination string of
                 * file_name and obj_name
                 */
                nfound = (status != 0) ? 1 : 0;

                if (print_objname(opts, nfound))
                    do_print_objname("external link", path1, path2, opts);

            } /* end if */
            else {
                /* If one or both of these links isn't an external link, we can only
                 * compare information from H5Lget_info since we don't have a query
                 * function registered for them.
                 *
                 * If the link classes or the buffer length are not the
                 * same, the links are "different"
                 */
                if ((linkinfo1.linfo.type != linkinfo2.linfo.type) ||
                    (linkinfo1.linfo.u.val_size != linkinfo2.linfo.u.val_size))
                    nfound = 1;
                else
                    nfound = 0;

                if (print_objname(opts, nfound))
                    do_print_objname("user defined link", path1, path2, opts);
            } /* end else */

            /* always print the number of differences found in verbose mode */
            if (opts->mode_verbose)
                print_found(nfound);
        } break;

        case H5TRAV_TYPE_UNKNOWN:
        default:
            if (opts->mode_verbose)
                parallel_print("Comparison not supported: <%s> and <%s> are of type %s\n", path1, path2,
                               get_type(object_type));
            opts->not_cmp = 1;
            break;
    }

done:
    opts->err_stat = opts->err_stat | ret_value;

    /*-----------------------------------
     * handle dangling link(s)
     */
    /* both path1 and path2 are dangling links */
    if (is_dangle_link1 && is_dangle_link2) {
        if (print_objname(opts, nfound)) {
            do_print_objname("dangling link", path1, path2, opts);
            print_found(nfound);
        }
    }
    /* path1 is dangling link */
    else if (is_dangle_link1) {
        if (opts->mode_verbose)
            parallel_print("obj1 <%s> is a dangling link.\n", path1);
        nfound++;
        if (print_objname(opts, nfound))
            print_found(nfound);
    }
    /* path2 is dangling link */
    else if (is_dangle_link2) {
        if (opts->mode_verbose)
            parallel_print("obj2 <%s> is a dangling link.\n", path2);
        nfound++;
        if (print_objname(opts, nfound))
            print_found(nfound);
    }

    /* free link info buffer */
    if (linkinfo1.trg_path)
        HDfree(linkinfo1.trg_path);
    if (linkinfo2.trg_path)
        HDfree(linkinfo2.trg_path);

    /* close */
    /* disable error reporting */
    H5E_BEGIN_TRY
    {
        H5Dclose(dset1_id);
        H5Dclose(dset2_id);
        H5Tclose(type1_id);
        H5Tclose(type2_id);
        H5Gclose(grp1_id);
        H5Gclose(grp2_id);
        /* enable error reporting */
    }
    H5E_END_TRY;

    H5TOOLS_ENDDEBUG(": %d - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

#ifdef H5_HAVE_PARALLEL

int
h5diff_get_global(int *flag)
{
    int global_flag;
    MPI_Reduce(flag, &global_flag, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    return global_flag;
}

#endif
