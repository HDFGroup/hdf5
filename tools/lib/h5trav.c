/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5trav.h"
#include "h5tools.h"
#include "H5private.h"

/* Replace uthash's default key comparison function with a wrapper around H5Otoken_cmp */
#undef HASH_KEYCMP
#define HASH_KEYCMP(a, b, len) trav_token_visited_cmp(loc_id, (const H5O_token_t *)a, (const H5O_token_t *)b)

/*-------------------------------------------------------------------------
 * local typedefs
 *-------------------------------------------------------------------------
 */
/* Structure for tracking visited objects in a hash table for
 * quicker lookups to determine when an object has already been
 * visited
 */
typedef struct trav_seen_hash_t {
    trav_seen_t obj;

    UT_hash_handle hh; /* Hash table handle */
} trav_seen_hash_t;

/* Structure for tracking the index into the table of objects
 * where a visited object was placed to facilitate quicker
 * lookups when adding path aliases
 */
typedef struct trav_table_hash_t {
    H5O_token_t token;
    size_t      index;

    UT_hash_handle hh; /* Hash table handle */
} trav_table_hash_t;

typedef struct {
    h5trav_obj_func_t visit_obj; /* Callback for visiting objects */
    h5trav_lnk_func_t visit_lnk; /* Callback for visiting links */
    void             *udata;     /* User data to pass to callbacks */
} trav_visitor_t;

typedef struct {
    trav_seen_hash_t     *objects_seen;  /* Hash table of objects seen already */
    const trav_visitor_t *visitor;       /* Information for visiting each link/object */
    bool                  is_absolute;   /* Whether the traversal has absolute paths */
    const char           *base_grp_name; /* Name of the group that serves as the base
                                          * for iteration */
    unsigned fields;                     /* Fields needed in H5O_info2_t struct */
} trav_ud_traverse_t;

typedef struct {
    hid_t fid; /* File ID being traversed */
} trav_print_udata_t;

typedef struct trav_path_op_data_t {
    const char *path;
} trav_path_op_data_t;

/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */
static herr_t trav_table_add(trav_table_t *table, const char *objname, const H5O_info2_t *oinfo);

static herr_t trav_table_addlink(trav_table_t *table, const H5O_token_t *obj_token, const char *path,
                                 const char *orig_path);

static int trav_token_visited_cmp(hid_t loc_id, const H5O_token_t *token1, const H5O_token_t *token2);

/*-------------------------------------------------------------------------
 * local variables
 *-------------------------------------------------------------------------
 */
static H5_index_t      trav_index_by    = H5_INDEX_NAME;
static H5_iter_order_t trav_index_order = H5_ITER_INC;

static int trav_verbosity = 0;

/*-------------------------------------------------------------------------
 * Function: h5trav_set_index
 *
 * Purpose:  Set indexing properties for the objects & links in the file
 *
 * Return:   none
 *-------------------------------------------------------------------------
 */
void
h5trav_set_index(H5_index_t print_index_by, H5_iter_order_t print_index_order)
{
    trav_index_by    = print_index_by;
    trav_index_order = print_index_order;
}

/*-------------------------------------------------------------------------
 * Function: h5trav_set_verbose
 *
 * Purpose:  Set verbosity of file contents 1=>attributes
 *
 * Return:   none
 *-------------------------------------------------------------------------
 */
void
h5trav_set_verbose(int print_verbose)
{
    trav_verbosity = print_verbose;
}

/*-------------------------------------------------------------------------
 * "h5trav info" public functions. used in h5diff
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: trav_token_add
 *
 * Purpose:  Add an object token to visited data structure
 *
 * Return:   Non-negative on success/negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
trav_token_add(trav_seen_hash_t **objects_seen_ptr, H5O_token_t *token, const char *path,
               trav_seen_t **visited_obj_ret)
{
    trav_seen_hash_t *entry = NULL;

    if (NULL == (entry = malloc(sizeof(*entry))))
        return FAIL;
    if (NULL == (entry->obj.path = strdup(path))) {
        free(entry);
        return FAIL;
    }
    memcpy(&entry->obj.token, token, sizeof(H5O_token_t));

    /* HASH_ADD modifies what's pointed to by objects_seen_ptr when it
     * initializes the hash table after being called for the first time
     */
    HASH_ADD(hh, (*objects_seen_ptr), obj.token, sizeof(H5O_token_t), entry);

    if (visited_obj_ret)
        *visited_obj_ret = &entry->obj;

    return SUCCEED;
} /* end trav_token_add() */

/*-------------------------------------------------------------------------
 * Function: trav_token_visited
 *
 * Purpose:  Check if an object token has already been seen
 *
 * Return:   true/false
 *-------------------------------------------------------------------------
 */
static bool
trav_token_visited(hid_t loc_id, trav_seen_hash_t *objects_seen, H5O_token_t *token,
                   trav_seen_t **visited_obj_ret)
{
    trav_seen_hash_t *entry = NULL;

    HASH_FIND(hh, objects_seen, token, sizeof(H5O_token_t), entry);

    if (visited_obj_ret)
        *visited_obj_ret = &entry->obj;

    return (entry != NULL);
} /* end trav_token_visited() */

/*-------------------------------------------------------------------------
 * Function: trav_token_visited_cmp
 *
 * Purpose:  Wrapper around H5Otoken_cmp for comparing objects in
 *           trav_token_visited()
 *
 * Return:   -1/0/1 (similar to memcmp())
 *-------------------------------------------------------------------------
 */
static int
trav_token_visited_cmp(hid_t loc_id, const H5O_token_t *token1, const H5O_token_t *token2)
{
    int cmp_result = -1;

    if (H5Otoken_cmp(loc_id, token1, token2, &cmp_result) < 0)
        return -1;
    return cmp_result;
}

/*-------------------------------------------------------------------------
 * Function: traverse_cb
 *
 * Purpose:  Iterator callback for traversing objects in file
 *-------------------------------------------------------------------------
 */
static herr_t
traverse_cb(hid_t loc_id, const char *path, const H5L_info2_t *linfo, void *_udata)
{
    trav_ud_traverse_t *udata    = (trav_ud_traverse_t *)_udata; /* User data */
    char               *new_name = NULL;
    const char         *full_name;

    /* Create the full path name for the link */
    if (udata->is_absolute) {
        size_t base_len     = strlen(udata->base_grp_name);
        size_t add_slash    = base_len ? ((udata->base_grp_name)[base_len - 1] != '/') : 1;
        size_t new_name_len = base_len + add_slash + strlen(path) + 1 +
                              3; /* Extra "+3" to quiet GCC warning - 2019/07/05, QAK */

        if (NULL == (new_name = (char *)malloc(new_name_len)))
            return (H5_ITER_ERROR);
        if (add_slash)
            snprintf(new_name, new_name_len, "%s/%s", udata->base_grp_name, path);
        else
            snprintf(new_name, new_name_len, "%s%s", udata->base_grp_name, path);
        full_name = new_name;
    } /* end if */
    else
        full_name = path;

    /* Perform the correct action for different types of links */
    if (linfo->type == H5L_TYPE_HARD) {
        trav_seen_t *visited_obj = NULL;
        H5O_info2_t  oinfo;
        bool         already_visited = false; /* Whether the link/object was already visited */

        /* Get information about the object */
        if (H5Oget_info_by_name3(loc_id, path, &oinfo, udata->fields, H5P_DEFAULT) < 0) {
            if (new_name)
                free(new_name);
            return (H5_ITER_ERROR);
        } /* end if */

        /* If the object has multiple links, add it to the list of addresses
         *  already visited, if it isn't there already
         */
        if (oinfo.rc > 1) {
            already_visited = trav_token_visited(loc_id, udata->objects_seen, &oinfo.token, &visited_obj);
            if (!already_visited) {
                if (trav_token_add(&udata->objects_seen, &oinfo.token, full_name, &visited_obj) < 0)
                    return H5_ITER_ERROR;
            }
        }

        /* Make 'visit object' callback */
        if (udata->visitor->visit_obj)
            if ((*udata->visitor->visit_obj)(full_name, &oinfo, already_visited, visited_obj,
                                             udata->visitor->udata) < 0) {
                if (new_name)
                    free(new_name);
                return (H5_ITER_ERROR);
            } /* end if */
    }         /* end if */
    else {
        /* Make 'visit link' callback */
        if (udata->visitor->visit_lnk)
            if ((*udata->visitor->visit_lnk)(full_name, linfo, udata->visitor->udata) < 0) {
                if (new_name)
                    free(new_name);
                return (H5_ITER_ERROR);
            } /* end if */
    }         /* end else */

    if (new_name)
        free(new_name);

    return (H5_ITER_CONT);
} /* end traverse_cb() */

/*-------------------------------------------------------------------------
 * Function: traverse
 *
 * Purpose:  Iterate over all the objects/links in a file.  Conforms to the
 *           "visitor" pattern.
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
static int
traverse(hid_t file_id, const char *grp_name, bool visit_start, bool recurse, const trav_visitor_t *visitor,
         unsigned fields)
{
    trav_ud_traverse_t udata = {0}; /* User data for iteration callback */
    H5O_info2_t        oinfo;       /* Object info for starting group */
    int                ret_value = 0;

    /* Get info for starting object */
    if (H5Oget_info_by_name3(file_id, grp_name, &oinfo, fields, H5P_DEFAULT) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Oget_info_by_name failed");

    /* Visit the starting object */
    if (visit_start && visitor->visit_obj)
        (*visitor->visit_obj)(grp_name, &oinfo, false, NULL, visitor->udata);

    /* Go visiting, if the object is a group */
    if (oinfo.type == H5O_TYPE_GROUP) {
        /* Set up user data structure */
        udata.objects_seen  = NULL;
        udata.visitor       = visitor;
        udata.is_absolute   = (*grp_name == '/');
        udata.base_grp_name = grp_name;
        udata.fields        = fields;

        /* Check for multiple links to top group */
        if (oinfo.rc > 1)
            trav_token_add(&udata.objects_seen, &oinfo.token, grp_name, NULL);

        /* Check for iteration of links vs. visiting all links recursively */
        if (recurse) {
            /* Visit all links in group, recursively */
            if (H5Lvisit_by_name2(file_id, grp_name, trav_index_by, trav_index_order, traverse_cb, &udata,
                                  H5P_DEFAULT) < 0)
                H5TOOLS_ERROR((-1), "H5Lvisit_by_name failed");
        } /* end if */
        else {
            /* Iterate over links in group */
            if (H5Literate_by_name2(file_id, grp_name, trav_index_by, trav_index_order, NULL, traverse_cb,
                                    &udata, H5P_DEFAULT) < 0)
                H5TOOLS_ERROR((-1), "H5Literate_by_name failed");
        } /* end else */
    }     /* end if */

done:
    if (udata.objects_seen) {
        trav_seen_hash_t *p, *tmp;

        HASH_ITER(hh, udata.objects_seen, p, tmp)
        {
            HASH_DEL(udata.objects_seen, p);
            free(p->obj.path);
            free(p);
        }
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: trav_info_add
 *
 * Purpose:  Add a link path & type to info struct
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
void
trav_info_add(trav_info_t *info, const char *path, h5trav_type_t obj_type)
{
    size_t idx; /* Index of address to use  */

    if (info) {
        /* Allocate space if necessary */
        if (info->nused == info->nalloc) {
            info->nalloc = MAX(1, info->nalloc * 2);
            info->paths  = (trav_path_t *)realloc(info->paths, info->nalloc * sizeof(trav_path_t));
        } /* end if */

        /* Append it */
        idx                     = info->nused++;
        info->paths[idx].path   = strdup(path);
        info->paths[idx].type   = obj_type;
        info->paths[idx].fileno = 0;

        /* Set token to 'undefined' values */
        info->paths[idx].obj_token = H5O_TOKEN_UNDEF;
    }
} /* end trav_info_add() */

/*-------------------------------------------------------------------------
 * Function: trav_fileinfo_add
 *
 * Purpose: Add a file addr & fileno to info struct
 *
 * Return: void
 *-------------------------------------------------------------------------
 */
void
trav_fileinfo_add(trav_info_t *info, hid_t loc_id)
{
    H5O_info2_t oinfo;
    size_t      idx = info->nused - 1;

    if (info->paths[idx].path && strcmp(info->paths[idx].path, ".") != 0)
        H5Oget_info_by_name3(loc_id, info->paths[idx].path, &oinfo, H5O_INFO_BASIC, H5P_DEFAULT);
    else
        H5Oget_info3(loc_id, &oinfo, H5O_INFO_BASIC);

    memcpy(&info->paths[idx].obj_token, &oinfo.token, sizeof(H5O_token_t));
    info->paths[idx].fileno = oinfo.fileno;
} /* end trav_fileinfo_add() */

/*-------------------------------------------------------------------------
 * Function: trav_info_visit_obj
 *
 * Purpose:  Callback for visiting object, with 'info' structure
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
int
trav_info_visit_obj(const char *path, const H5O_info2_t *oinfo, bool H5_ATTR_UNUSED already_visited,
                    const trav_seen_t H5_ATTR_UNUSED *visited_obj_info, void *udata)
{
    size_t       idx;
    trav_info_t *info_p;

    /* Add the object to the 'info' struct */
    /* (object types map directly to "traversal" types) */
    trav_info_add((trav_info_t *)udata, path, (h5trav_type_t)oinfo->type);

    /* set object addr and fileno. These are for checking same object */
    info_p = (trav_info_t *)udata;
    idx    = info_p->nused - 1;
    memcpy(&info_p->paths[idx].obj_token, &oinfo->token, sizeof(H5O_token_t));
    info_p->paths[idx].fileno = oinfo->fileno;

    return (0);
} /* end trav_info_visit_obj() */

/*-------------------------------------------------------------------------
 * Function: trav_info_visit_lnk
 *
 * Purpose:  Callback for visiting link, with 'info' structure
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
int
trav_info_visit_lnk(const char *path, const H5L_info2_t *linfo, void *udata)
{
    /* Add the link to the 'info' struct */
    trav_info_add((trav_info_t *)udata, path,
                  ((linfo->type == H5L_TYPE_SOFT) ? H5TRAV_TYPE_LINK : H5TRAV_TYPE_UDLINK));

    return (0);
} /* end trav_info_visit_lnk() */

/*-------------------------------------------------------------------------
 * Function: h5trav_getinfo
 *
 * Purpose:  get an array of "trav_info_t" , containing the name and type of
 *           objects in the file
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
int
h5trav_getinfo(hid_t file_id, trav_info_t *info)
{
    trav_visitor_t info_visitor; /* Visitor structure for trav_info_t's */
    int            ret_value = 0;

    /* Init visitor structure */
    info_visitor.visit_obj = trav_info_visit_obj;
    info_visitor.visit_lnk = trav_info_visit_lnk;
    info_visitor.udata     = info;

    /* Traverse all objects in the file, visiting each object & link */
    if (traverse(file_id, "/", true, true, &info_visitor, H5O_INFO_BASIC) < 0)
        H5TOOLS_GOTO_ERROR((-1), "traverse failed");

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5trav_getindex
 *
 * Purpose:  get index of OBJ in list
 *
 * Return:   index on success,
 *           -1 if not found
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE ssize_t
h5trav_getindex(const trav_info_t *info, const char *obj)
{
    size_t u; /* Local index variable */

    /* Loop over all paths in 'info' struct, looking for object */
    for (u = 0; u < info->nused; u++) {
        /* Check for object name having full path (with leading '/') */
        if (strcmp(obj, info->paths[u].path) == 0)
            return ((ssize_t)u);

        /* Check for object name without leading '/' */
        if (strcmp(obj, (info->paths[u].path + 1)) == 0)
            return ((ssize_t)u);
    } /* end for */

    return ((ssize_t)-1);
} /* end h5trav_getindex() */

/*-------------------------------------------------------------------------
 * Function: trav_info_init
 *
 * Purpose:  Initialize the info
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
void
trav_info_init(const char *filename, hid_t fileid, trav_info_t **_info)
{
    trav_info_t *info = (trav_info_t *)malloc(sizeof(trav_info_t));

    /* Init info structure */
    info->nused = info->nalloc = 0;
    info->paths                = NULL;
    info->fname                = filename;
    info->fid                  = fileid;

    /* Initialize list of visited symbolic links */
    info->symlink_visited.nused       = 0;
    info->symlink_visited.nalloc      = 0;
    info->symlink_visited.objs        = NULL;
    info->symlink_visited.dangle_link = false;
    *_info                            = info;
} /* end trav_info_init() */

/*-------------------------------------------------------------------------
 * Function: trav_info_free
 *
 * Purpose:  free info memory
 *-------------------------------------------------------------------------
 */
void
trav_info_free(trav_info_t *info)
{
    size_t u; /* Local index variable */

    if (info) {
        /* Free visited symbolic links path and file (if alloc) */
        for (u = 0; u < info->symlink_visited.nused; u++) {
            if (info->symlink_visited.objs[u].file)
                free(info->symlink_visited.objs[u].file);
            free(info->symlink_visited.objs[u].path);
        }
        free(info->symlink_visited.objs);

        /* Free path names */
        for (u = 0; u < info->nused; u++)
            free(info->paths[u].path);
        free(info->paths);
        free(info);
    } /* end if */
} /* end trav_info_free() */

/*-------------------------------------------------------------------------
 * "h5trav table" public functions. used in h5repack
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: trav_table_visit_obj
 *
 * Purpose: Callback for visiting object, with 'table' structure
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
static int
trav_table_visit_obj(const char *path, const H5O_info2_t *oinfo, bool already_visited,
                     const trav_seen_t *visited_obj_info, void *udata)
{
    trav_table_t *table = (trav_table_t *)udata;

    /* Check if we've already seen this object */
    if (!already_visited) {
        /* add object to table */
        if (trav_table_add(table, path, oinfo) < 0)
            return -1;
    }
    else {
        assert(visited_obj_info);

        /* Add alias for object to table */
        if (trav_table_addlink(table, &oinfo->token, path, visited_obj_info->path) < 0)
            return -1;
    }

    return 0;
} /* end trav_table_visit_obj() */

/*-------------------------------------------------------------------------
 * Function: trav_table_visit_lnk
 *
 * Purpose:  Callback for visiting link, with 'table' structure
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
static int
trav_table_visit_lnk(const char *path, const H5L_info2_t H5_ATTR_UNUSED *linfo, void *udata)
{
    /* Add the link to the 'table' struct */
    trav_table_add((trav_table_t *)udata, path, NULL);

    return 0;
} /* end trav_table_visit_lnk() */

/*-------------------------------------------------------------------------
 * Function: h5trav_gettable
 *
 * Purpose:  get the trav_table_t struct
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
int
h5trav_gettable(hid_t fid, trav_table_t *table)
{
    trav_visitor_t table_visitor; /* Visitor structure for trav_table_t's */
    int            ret_value = 0;

    /* Init visitor structure */
    table_visitor.visit_obj = trav_table_visit_obj;
    table_visitor.visit_lnk = trav_table_visit_lnk;
    table_visitor.udata     = table;

    /* Traverse all objects in the file, visiting each object & link */
    if (traverse(fid, "/", true, true, &table_visitor, H5O_INFO_BASIC) < 0)
        H5TOOLS_GOTO_ERROR((-1), "traverse failed");

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5trav_getindext
 *
 * Purpose:  get index of NAME in list
 *
 * Return:   index on success,
 *           -1 if not found
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE int
h5trav_getindext(const char *name, const trav_table_t *table)
{
    unsigned int i;

    if (table) {
        for (i = 0; i < table->nobjs; i++) {
            /* Check for object name having full path (with leading '/') */
            if (strcmp(name, table->objs[i].name) == 0)
                return ((int)i);

            /* Check for object name without leading '/' */
            if (strcmp(name, table->objs[i].name + 1) == 0)
                return ((int)i);

            /* search also in the list of links */
            if (table->objs[i].nlinks) {
                unsigned int j;

                for (j = 0; j < table->objs[i].nlinks; j++) {
                    /* Check for object name having full path (with leading '/') */
                    if (strcmp(name, table->objs[i].links[j].new_name) == 0)
                        return ((int)i);

                    /* Check for object name without leading '/' */
                    if (strcmp(name, table->objs[i].links[j].new_name + 1) == 0)
                        return ((int)i);
                } /* end for */
            }     /* end if */
        }         /* end for */
    }
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: trav_table_add
 *
 * Purpose:  Add OBJNO, NAME and TYPE of object to table
 *
 * Return:   Non-negative on success/negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
trav_table_add(trav_table_t *table, const char *path, const H5O_info2_t *oinfo)
{
    trav_table_hash_t *entry = NULL;
    size_t             new_obj_idx;

    if (!table)
        return FAIL;

    if (table->nobjs == table->size) {
        void *tmp_realloc;

        table->size = MAX(1, table->size * 2);
        tmp_realloc = realloc(table->objs, table->size * sizeof(trav_obj_t));
        if (!tmp_realloc)
            return FAIL;

        table->objs = tmp_realloc;
    } /* end if */

    new_obj_idx = table->nobjs++;
    if (oinfo)
        memcpy(&table->objs[new_obj_idx].obj_token, &oinfo->token, sizeof(H5O_token_t));
    else
        /* Set token to 'undefined' values */
        table->objs[new_obj_idx].obj_token = H5O_TOKEN_UNDEF;
    table->objs[new_obj_idx].flags[0] = table->objs[new_obj_idx].flags[1] = 0;
    table->objs[new_obj_idx].is_same_trgobj                               = 0;
    table->objs[new_obj_idx].name                                         = (char *)strdup(path);
    table->objs[new_obj_idx].type      = oinfo ? (h5trav_type_t)oinfo->type : H5TRAV_TYPE_LINK;
    table->objs[new_obj_idx].nlinks    = 0;
    table->objs[new_obj_idx].sizelinks = 0;
    table->objs[new_obj_idx].links     = NULL;

    /* Add object to the hash table tracking its objects table index */
    if (oinfo) {
        if (NULL == (entry = malloc(sizeof(*entry))))
            return FAIL;
        memcpy(&entry->token, &oinfo->token, sizeof(H5O_token_t));
        entry->index = new_obj_idx;

        /* HASH_ADD modifies what's pointed to by table->priv_data when it
         * initializes the hash table after being called for the first time
         */
        HASH_ADD(hh, (*(trav_table_hash_t **)&table->priv_data), token, sizeof(H5O_token_t), entry);
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function: trav_table_addlink
 *
 * Purpose:  Add a hardlink name to the object
 *
 * Return:   Non-negative on success/negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
trav_table_addlink(trav_table_t *table, const H5O_token_t *obj_token, const char *path, const char *orig_path)
{
    trav_table_hash_t *entry = NULL;
    size_t             i, n;
    int                token_cmp;
    hid_t              loc_id;

    if (!table)
        return FAIL;

    /* Variable must be called "loc_id" for use in HASH_FIND's key comparison
     * function (redirected to calling trav_token_visited_cmp(loc_id, ...))
     */
    loc_id = table->fid;

    /* Look for object in hash table tracking index values. If not found, fall
     * back to linear scan
     */
    HASH_FIND(hh, ((trav_table_hash_t *)table->priv_data), obj_token, sizeof(H5O_token_t), entry);
    if (entry) {
        i = entry->index;

        /* Make sure objects are the same */
        if (orig_path && (0 != strcmp(table->objs[i].name, orig_path)))
            return FAIL;
    }
    else {
        for (i = 0; i < table->nobjs; i++) {
            if (H5Otoken_cmp(loc_id, &table->objs[i].obj_token, obj_token, &token_cmp) < 0)
                return FAIL;
            if (0 == token_cmp)
                break;
        }

        /* Didn't find the object? */
        if (i == table->nobjs)
            return FAIL;
    }

    /* already inserted? */
    if (strcmp(table->objs[i].name, path) == 0)
        return SUCCEED;

    /* allocate space if necessary */
    if (table->objs[i].nlinks == table->objs[i].sizelinks) {
        void *tmp_realloc;

        table->objs[i].sizelinks = MAX(1, table->objs[i].sizelinks * 2);
        tmp_realloc = realloc(table->objs[i].links, table->objs[i].sizelinks * sizeof(trav_link_t));
        if (!tmp_realloc)
            return FAIL;

        table->objs[i].links = tmp_realloc;
    } /* end if */

    /* insert it */
    n = table->objs[i].nlinks++;
    if (NULL == (table->objs[i].links[n].new_name = strdup(path)))
        return FAIL;

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function: trav_table_addflags
 *
 * Purpose:  Add FLAGS, NAME and TYPE of object to table
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
void
trav_table_addflags(const unsigned *flags, char *name, h5trav_type_t type, trav_table_t *table)
{
    size_t new_obj;

    if (table) {
        if (table->nobjs == table->size) {
            table->size = MAX(1, table->size * 2);
            table->objs = (trav_obj_t *)realloc(table->objs, table->size * sizeof(trav_obj_t));
        } /* end if */

        new_obj = table->nobjs++;

        /* Set token to 'undefined' values */
        table->objs[new_obj].obj_token = H5O_TOKEN_UNDEF;

        table->objs[new_obj].flags[0]       = flags[0];
        table->objs[new_obj].flags[1]       = flags[1];
        table->objs[new_obj].is_same_trgobj = 0;
        table->objs[new_obj].name           = (char *)strdup(name);
        table->objs[new_obj].type           = type;
        table->objs[new_obj].nlinks         = 0;
        table->objs[new_obj].sizelinks      = 0;
        table->objs[new_obj].links          = NULL;
    }
}

/*-------------------------------------------------------------------------
 * Function: trav_table_init
 *
 * Purpose:  Initialize the table
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
void
trav_table_init(hid_t fid, trav_table_t **tbl)
{
    trav_table_t *table = (trav_table_t *)malloc(sizeof(trav_table_t));
    if (table) {
        table->fid       = fid;
        table->size      = 0;
        table->nobjs     = 0;
        table->objs      = NULL;
        table->priv_data = NULL;
    }
    *tbl = table;
}

/*-------------------------------------------------------------------------
 * Function: trav_table_free
 *
 * Purpose:  free table memory
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
void
trav_table_free(trav_table_t *table)
{
    if (table) {
        if (table->objs) {
            unsigned int i;

            for (i = 0; i < table->nobjs; i++) {
                free(table->objs[i].name);
                if (table->objs[i].nlinks) {
                    unsigned int j;

                    for (j = 0; j < table->objs[i].nlinks; j++)
                        free(table->objs[i].links[j].new_name);

                    free(table->objs[i].links);
                } /* end if */
            }     /* end for */
            free(table->objs);
        } /* end if */
        if (table->priv_data) {
            trav_table_hash_t *hasht = (trav_table_hash_t *)table->priv_data;
            trav_table_hash_t *p, *tmp;

            HASH_ITER(hh, hasht, p, tmp)
            {
                HASH_DEL(hasht, p);
                free(p);
            }
        }
        free(table);
    }
}

static herr_t
trav_attr(hid_t
#ifndef H5TRAV_PRINT_SPACE
              H5_ATTR_UNUSED
#endif /* H5TRAV_PRINT_SPACE */
                      obj,
          const char *attr_name, const H5A_info_t H5_ATTR_UNUSED *ainfo, void *_op_data)
{
    trav_path_op_data_t *op_data = (trav_path_op_data_t *)_op_data;
    const char          *buf     = op_data->path;

    if ((strlen(buf) == 1) && (*buf == '/'))
        printf(" %-10s %s%s", "attribute", buf, attr_name);
    else
        printf(" %-10s %s/%s", "attribute", buf, attr_name);

#ifdef H5TRAV_PRINT_SPACE
    if (trav_verbosity < 2) {
#endif
        printf("\n");
#ifdef H5TRAV_PRINT_SPACE
    }
    else {
        hid_t       attr  = H5I_INVALID_HID;
        hid_t       space = H5I_INVALID_HID;
        hsize_t     size[H5S_MAX_RANK];
        int         ndims;
        int         i;
        H5S_class_t space_type;

        if ((attr = H5Aopen(obj, attr_name, H5P_DEFAULT))) {
            space = H5Aget_space(attr);

            /* Data space */
            ndims      = H5Sget_simple_extent_dims(space, size, NULL);
            space_type = H5Sget_simple_extent_type(space);
            switch (space_type) {
                case H5S_SCALAR:
                    /* scalar dataspace */
                    printf(" scalar\n");
                    break;

                case H5S_SIMPLE:
                    /* simple dataspace */
                    printf(" {");
                    for (i = 0; i < ndims; i++) {
                        printf("%s%" PRIuHSIZE, i ? ", " : "", size[i]);
                    }
                    printf("}\n");
                    break;

                case H5S_NULL:
                    /* null dataspace */
                    printf(" null\n");
                    break;

                default:
                    /* Unknown dataspace type */
                    printf(" unknown\n");
                    break;
            } /* end switch */

            H5Sclose(space);
            H5Aclose(attr);
        }
    }
#endif

    return (0);
}

/*-------------------------------------------------------------------------
 * Function: trav_print_visit_obj
 *
 * Purpose:  Callback for visiting object, when printing info
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
static int
trav_print_visit_obj(const char *path, const H5O_info2_t *oinfo, bool already_visited,
                     const trav_seen_t *visited_obj_info, void *udata)
{
    trav_print_udata_t *print_udata = (trav_print_udata_t *)udata;
    /* Print the name of the object */
    /* (no new-line, so that objects that we've encountered before can print
     *  the name of the original object)
     */
    switch (oinfo->type) {
        case H5O_TYPE_GROUP:
            printf(" %-10s %s", "group", path);
            break;

        case H5O_TYPE_DATASET:
            printf(" %-10s %s", "dataset", path);
            break;

        case H5O_TYPE_NAMED_DATATYPE:
            printf(" %-10s %s", "datatype", path);
            break;

        case H5O_TYPE_MAP:
        case H5O_TYPE_UNKNOWN:
        case H5O_TYPE_NTYPES:
        default:
            printf(" %-10s %s", "unknown object type", path);
            break;
    } /* end switch */

    /* Check if we've already seen this object */
    if (!already_visited) {
        trav_path_op_data_t op_data;

        op_data.path = path;
        /* Finish printing line about object */
        printf("\n");
        if (trav_verbosity > 0)
            H5Aiterate_by_name(print_udata->fid, path, trav_index_by, trav_index_order, NULL, trav_attr,
                               &op_data, H5P_DEFAULT);
    }
    else {
        assert(visited_obj_info);

        /* Print the link's original name */
        printf(" -> %s\n", visited_obj_info->path);
    }

    return (0);
} /* end trav_print_visit_obj() */

/*-------------------------------------------------------------------------
 * Function: trav_print_visit_lnk
 *
 * Purpose:  Callback for visiting link, when printing info
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
static int
trav_print_visit_lnk(const char *path, const H5L_info2_t *linfo, void *udata)
{
    trav_print_udata_t *print_udata = (trav_print_udata_t *)udata;

    /* Print appropriate information for the type of link */
    switch (linfo->type) {
        case H5L_TYPE_SOFT:
            if (linfo->u.val_size > 0) {
                char *targbuf = (char *)malloc(linfo->u.val_size + 1);
                if (targbuf) {
                    if (H5Lget_val(print_udata->fid, path, targbuf, linfo->u.val_size + 1, H5P_DEFAULT) < 0)
                        targbuf[0] = 0;
                    printf(" %-10s %s -> %s\n", "link", path, targbuf);
                    free(targbuf);
                }
            }
            else
                printf(" %-10s %s ->\n", "link", path);
            break;

        case H5L_TYPE_EXTERNAL:
            if (linfo->u.val_size > 0) {
                char       *targbuf  = NULL;
                const char *filename = NULL;
                const char *objname  = NULL;

                targbuf = (char *)malloc(linfo->u.val_size + 1);
                if (targbuf) {
                    if (H5Lget_val(print_udata->fid, path, targbuf, linfo->u.val_size + 1, H5P_DEFAULT) < 0)
                        targbuf[0] = 0;
                    if (H5Lunpack_elink_val(targbuf, linfo->u.val_size, NULL, &filename, &objname) >= 0)
                        printf(" %-10s %s -> %s %s\n", "ext link", path, filename, objname);
                    free(targbuf);
                }
            } /* end if */
            else
                printf(" %-10s %s ->\n", "ext link", path);
            break;

        case H5L_TYPE_HARD:
            /* Should be handled elsewhere */
            return (-1);

        case H5L_TYPE_ERROR:
        case H5L_TYPE_MAX:
        default:
            printf(" %-10s %s -> ???\n", "unknown type of UD link", path);
            break;
    } /* end switch() */

    return (0);
} /* end trav_print_visit_lnk() */

/*-------------------------------------------------------------------------
 * Function: h5trav_print
 *
 * Purpose:  Print information about the objects & links in the file
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
int
h5trav_print(hid_t fid)
{
    trav_print_udata_t print_udata;   /* User data for traversal */
    trav_visitor_t     print_visitor; /* Visitor structure for printing objects */
    int                ret_value = 0;

    /* Init user data for printing */
    print_udata.fid = fid;

    /* Init visitor structure */
    print_visitor.visit_obj = trav_print_visit_obj;
    print_visitor.visit_lnk = trav_print_visit_lnk;
    print_visitor.udata     = &print_udata;

    /* Traverse all objects in the file, visiting each object & link */
    if (traverse(fid, "/", true, true, &print_visitor, H5O_INFO_BASIC) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "traverse failed");

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5trav_visit
 *
 * Purpose: Generic traversal routine for visiting objects and links
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
int
h5trav_visit(hid_t fid, const char *grp_name, bool visit_start, bool recurse, h5trav_obj_func_t visit_obj,
             h5trav_lnk_func_t visit_lnk, void *udata, unsigned fields)
{
    trav_visitor_t visitor; /* Visitor structure for objects */
    int            ret_value = 0;

    /* Init visitor structure */
    visitor.visit_obj = visit_obj;
    visitor.visit_lnk = visit_lnk;
    visitor.udata     = udata;

    /* Traverse all objects in the file, visiting each object & link */
    if (traverse(fid, grp_name, visit_start, recurse, &visitor, fields) < 0)
        H5TOOLS_GOTO_ERROR((-1), "traverse failed");

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: symlink_visit_add
 *
 * Purpose: Add an symbolic link to visited data structure
 *
 * Return:   0 on success,
 *          -1 on failure
 *-------------------------------------------------------------------------
 */
herr_t
symlink_visit_add(symlink_trav_t *visited, H5L_type_t type, const char *file, const char *path)
{
    size_t idx; /* Index of address to use */
    herr_t ret_value = SUCCEED;

    /* Allocate space if necessary */
    if (visited->nused == visited->nalloc) {
        void *tmp_ptr;

        visited->nalloc = MAX(1, visited->nalloc * 2);
        if (NULL == (tmp_ptr = realloc(visited->objs, visited->nalloc * sizeof(symlink_trav_path_t))))
            H5TOOLS_GOTO_ERROR(FAIL, "visited data structure realloc failed");
        visited->objs = (symlink_trav_path_t *)tmp_ptr;
    } /* end if */

    /* Append it */
    idx = visited->nused++;

    visited->objs[idx].type = type;
    visited->objs[idx].file = NULL;
    visited->objs[idx].path = NULL;

    if (type == H5L_TYPE_EXTERNAL) {
        if (NULL == (visited->objs[idx].file = strdup(file))) {
            visited->nused--;
            H5TOOLS_GOTO_ERROR(FAIL, "visited data structure name allocation failed");
        } /* end if */
    }     /* end if */

    if (NULL == (visited->objs[idx].path = strdup(path))) {
        visited->nused--;
        if (visited->objs[idx].file)
            free(visited->objs[idx].file);
        H5TOOLS_GOTO_ERROR(FAIL, "visited data structure path allocation failed");
    } /* end if */

done:
    return ret_value;
} /* end symlink_visit_add() */

/*-------------------------------------------------------------------------
 * Function: symlink_is_visited
 *
 * Purpose:  Check if an symbolic link has already been visited
 *
 * Return:   true/false
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE bool
symlink_is_visited(symlink_trav_t *visited, H5L_type_t type, const char *file, const char *path)
{
    size_t u; /* Local index variable */

    /* Look for symlink */
    for (u = 0; u < visited->nused; u++) {
        /* Check for symlink values already in array */
        /* check type and path pair to distinguish between symbolic links */
        if ((visited->objs[u].type == type) && !strcmp(visited->objs[u].path, path)) {
            /* if external link, file need to be matched as well */
            if (visited->objs[u].type == H5L_TYPE_EXTERNAL)
                if (!strcmp(visited->objs[u].file, file))
                    return (true);

            return (true);
        } /* end if */
    }     /* end for */

    /* Didn't find symlink */
    return (false);
} /* end symlink_is_visited() */
