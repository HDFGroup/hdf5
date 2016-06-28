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


#include "h5trav.h"
#include "H5private.h"

/*-------------------------------------------------------------------------
 * local typedefs
 *-------------------------------------------------------------------------
 */
typedef struct trav_addr_path_t {
    haddr_t addr;
    char *path;
} trav_addr_path_t;

typedef struct trav_addr_t {
    size_t      nalloc;
    size_t      nused;
    trav_addr_path_t *objs;
} trav_addr_t;

typedef struct {
    h5trav_obj_func_t visit_obj;        /* Callback for visiting objects */
    h5trav_lnk_func_t visit_lnk;        /* Callback for visiting links */
    void *udata;                /* User data to pass to callbacks */
} trav_visitor_t;

typedef struct {
    trav_addr_t *seen;              /* List of addresses seen already */
    const trav_visitor_t *visitor;  /* Information for visiting each link/object */
    hbool_t is_absolute;            /* Whether the traversal has absolute paths */
    const char *base_grp_name;      /* Name of the group that serves as the base
                                     * for iteration */
} trav_ud_traverse_t;

typedef struct {
    hid_t fid;                      /* File ID being traversed */
} trav_print_udata_t;

typedef struct trav_path_op_data_t {
    const char *path;
} trav_path_op_data_t;

/* format for hsize_t */
#ifdef H5TRAV_PRINT_SPACE
#define HSIZE_T_FORMAT   "%" H5_PRINTF_LL_WIDTH "u"
#endif /* H5TRAV_PRINT_SPACE */

/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */
static void trav_table_add(trav_table_t *table,
                        const char *objname,
                        const H5O_info_t *oinfo);

static void trav_table_addlink(trav_table_t *table,
                        haddr_t objno,
                        const char *path);

/*-------------------------------------------------------------------------
 * local variables
 *-------------------------------------------------------------------------
 */
static H5_index_t trav_index_by = H5_INDEX_NAME;
static H5_iter_order_t trav_index_order = H5_ITER_INC;

static int trav_verbosity = 0;

/*-------------------------------------------------------------------------
 * Function: h5trav_set_index
 *
 * Purpose: Set indexing properties for the objects & links in the file
 *
 * Return: none
 *
 *-------------------------------------------------------------------------
 */
void
h5trav_set_index(H5_index_t print_index_by, H5_iter_order_t print_index_order)
{
    trav_index_by = print_index_by;
    trav_index_order = print_index_order;
}

/*-------------------------------------------------------------------------
 * Function: h5trav_set_verbose
 *
 * Purpose: Set verbosity of file contents 1=>attributes
 *
 * Return: none
 *
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
 * Function: trav_addr_add
 *
 * Purpose: Add a hardlink address to visited data structure
 *
 * Return: void
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
static void
trav_addr_add(trav_addr_t *visited, haddr_t addr, const char *path)
{
    size_t idx;         /* Index of address to use */

    /* Allocate space if necessary */
    if(visited->nused == visited->nalloc) {
        visited->nalloc = MAX(1, visited->nalloc * 2);;
        visited->objs = (trav_addr_path_t *)HDrealloc(visited->objs, visited->nalloc * sizeof(trav_addr_path_t));
    } /* end if */

    /* Append it */
    idx = visited->nused++;
    visited->objs[idx].addr = addr;
    visited->objs[idx].path = HDstrdup(path);
} /* end trav_addr_add() */


/*-------------------------------------------------------------------------
 * Function: trav_addr_visited
 *
 * Purpose: Check if an address has already been visited
 *
 * Return: TRUE/FALSE
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE static const char *
trav_addr_visited(trav_addr_t *visited, haddr_t addr)
{
    size_t u;           /* Local index variable */

    /* Look for address */
    for(u = 0; u < visited->nused; u++)
        /* Check for address already in array */
        if(visited->objs[u].addr == addr)
            return(visited->objs[u].path);

    /* Didn't find address */
    return(NULL);
} /* end trav_addr_visited() */


/*-------------------------------------------------------------------------
 * Function: traverse_cb
 *
 * Purpose: Iterator callback for traversing objects in file
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
traverse_cb(hid_t loc_id, const char *path, const H5L_info_t *linfo,
    void *_udata)
{
    trav_ud_traverse_t *udata = (trav_ud_traverse_t *)_udata;     /* User data */
    char *new_name = NULL;
    const char *full_name;
    const char *already_visited = NULL; /* Whether the link/object was already visited */

    /* Create the full path name for the link */
    if(udata->is_absolute) {
        size_t base_len = HDstrlen(udata->base_grp_name);
        size_t add_slash = base_len ? ((udata->base_grp_name)[base_len - 1] != '/') : 1;
        size_t new_name_len = base_len + add_slash + HDstrlen(path) + 1;

        if(NULL == (new_name = (char*)HDmalloc(new_name_len)))
            return(H5_ITER_ERROR);
        if(add_slash)
            HDsnprintf(new_name, new_name_len, "%s/%s", udata->base_grp_name, path);
        else
            HDsnprintf(new_name, new_name_len, "%s%s", udata->base_grp_name, path);
        full_name = new_name;
    } /* end if */
    else
        full_name = path;

    /* Perform the correct action for different types of links */
    if(linfo->type == H5L_TYPE_HARD) {
        H5O_info_t oinfo;

        /* Get information about the object */
        if(H5Oget_info_by_name(loc_id, path, &oinfo, H5P_DEFAULT) < 0) {
            if(new_name)
                HDfree(new_name);
            return(H5_ITER_ERROR);
        } /* end if */

        /* If the object has multiple links, add it to the list of addresses
         *  already visited, if it isn't there already
         */
        if(oinfo.rc > 1)
            if(NULL == (already_visited = trav_addr_visited(udata->seen, oinfo.addr)))
                trav_addr_add(udata->seen, oinfo.addr, full_name);

        /* Make 'visit object' callback */
        if(udata->visitor->visit_obj)
            if((*udata->visitor->visit_obj)(full_name, &oinfo, already_visited, udata->visitor->udata) < 0) {
                if(new_name)
                    HDfree(new_name);
                return(H5_ITER_ERROR);
            } /* end if */
    } /* end if */
    else {
        /* Make 'visit link' callback */
        if(udata->visitor->visit_lnk)
            if((*udata->visitor->visit_lnk)(full_name, linfo, udata->visitor->udata) < 0) {
                if(new_name)
                    HDfree(new_name);
                return(H5_ITER_ERROR);
            } /* end if */
    } /* end else */

    if(new_name)
        HDfree(new_name);

    return(H5_ITER_CONT);
} /* end traverse_cb() */


/*-------------------------------------------------------------------------
 * Function: traverse
 *
 * Purpose: Iterate over all the objects/links in a file.  Conforms to the
 *      "visitor" pattern.
 *
 * Return: 0 on success, -1 on failure
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
traverse(hid_t file_id, const char *grp_name, hbool_t visit_start,
    hbool_t recurse, const trav_visitor_t *visitor)
{
    H5O_info_t  oinfo;          /* Object info for starting group */

    /* Get info for starting object */
    if(H5Oget_info_by_name(file_id, grp_name, &oinfo, H5P_DEFAULT) < 0)
        return -1;

    /* Visit the starting object */
    if(visit_start && visitor->visit_obj)
        (*visitor->visit_obj)(grp_name, &oinfo, NULL, visitor->udata);

    /* Go visiting, if the object is a group */
    if(oinfo.type == H5O_TYPE_GROUP) {
        trav_addr_t seen;           /* List of addresses seen */
        trav_ud_traverse_t udata;   /* User data for iteration callback */

        /* Init addresses seen */
        seen.nused = seen.nalloc = 0;
        seen.objs = NULL;

        /* Check for multiple links to top group */
        if(oinfo.rc > 1)
            trav_addr_add(&seen, oinfo.addr, grp_name);

        /* Set up user data structure */
        udata.seen = &seen;
        udata.visitor = visitor;
        udata.is_absolute = (*grp_name == '/');
        udata.base_grp_name = grp_name;

        /* Check for iteration of links vs. visiting all links recursively */
        if(recurse) {
            /* Visit all links in group, recursively */
            if(H5Lvisit_by_name(file_id, grp_name, trav_index_by, trav_index_order, traverse_cb, &udata, H5P_DEFAULT) < 0)
                return -1;
        } /* end if */
        else {
            /* Iterate over links in group */
            if(H5Literate_by_name(file_id, grp_name, trav_index_by, trav_index_order, NULL, traverse_cb, &udata, H5P_DEFAULT) < 0)
                return -1;
        } /* end else */

        /* Free visited addresses table */
        if(seen.objs) {
            size_t u;       /* Local index variable */

            /* Free paths to objects */
            for(u = 0; u < seen.nused; u++)
                HDfree(seen.objs[u].path);
            HDfree(seen.objs);
        } /* end if */
    } /* end if */

    return 0;
}


/*-------------------------------------------------------------------------
 * Function: trav_info_add
 *
 * Purpose: Add a link path & type to info struct
 *
 * Return: void
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
void
trav_info_add(trav_info_t *info, const char *path, h5trav_type_t obj_type)
{
    size_t idx;         /* Index of address to use  */

    /* Allocate space if necessary */
    if(info->nused == info->nalloc) {
        info->nalloc = MAX(1, info->nalloc * 2);;
        info->paths = (trav_path_t *)HDrealloc(info->paths, info->nalloc * sizeof(trav_path_t));
    } /* end if */

    /* Append it */
    idx = info->nused++;
    info->paths[idx].path = HDstrdup(path);
    info->paths[idx].type = obj_type;
    info->paths[idx].fileno = 0;
    info->paths[idx].objno = HADDR_UNDEF;
} /* end trav_info_add() */


/*-------------------------------------------------------------------------
 * Function: trav_fileinfo_add
 *
 * Purpose: Add a file addr & fileno to info struct
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
void
trav_fileinfo_add(trav_info_t *info, hid_t loc_id)
{
    H5O_info_t oinfo;
    size_t idx = info->nused - 1;

    if ( info->paths[idx].path && HDstrcmp(info->paths[idx].path, "."))
      H5Oget_info_by_name(loc_id, info->paths[idx].path, &oinfo, H5P_DEFAULT);
    else
      H5Oget_info(loc_id, &oinfo);

    info->paths[idx].objno = oinfo.addr;
    info->paths[idx].fileno = oinfo.fileno;
} /* end trav_fileinfo_add() */


/*-------------------------------------------------------------------------
 * Function: trav_info_visit_obj
 *
 * Purpose: Callback for visiting object, with 'info' structure
 *
 * Return: 0 on success, -1 on failure
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
int
trav_info_visit_obj(const char *path, const H5O_info_t *oinfo,
    const char H5_ATTR_UNUSED *already_visited, void *udata)
{
    size_t idx;
    trav_info_t *info_p;
    /* Add the object to the 'info' struct */
    /* (object types map directly to "traversal" types) */
    trav_info_add((trav_info_t *)udata, path, (h5trav_type_t)oinfo->type);

    /* set object addr and fileno. These are for checking same object */
    info_p = (trav_info_t *) udata;
    idx = info_p->nused - 1;
    info_p->paths[idx].objno = oinfo->addr;
    info_p->paths[idx].fileno = oinfo->fileno;

    return(0);
} /* end trav_info_visit_obj() */


/*-------------------------------------------------------------------------
 * Function: trav_info_visit_lnk
 *
 * Purpose: Callback for visiting link, with 'info' structure
 *
 * Return: 0 on success, -1 on failure
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
int
trav_info_visit_lnk(const char *path, const H5L_info_t *linfo, void *udata)
{
    /* Add the link to the 'info' struct */
    trav_info_add((trav_info_t *)udata, path, ((linfo->type == H5L_TYPE_SOFT) ? H5TRAV_TYPE_LINK : H5TRAV_TYPE_UDLINK));

    return(0);
} /* end trav_info_visit_lnk() */


/*-------------------------------------------------------------------------
 * Function: h5trav_getinfo
 *
 * Purpose: get an array of "trav_info_t" , containing the name and type of
 *  objects in the file
 *
 * Return: number of object names in file
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 6, 2002
 *
 *-------------------------------------------------------------------------
 */
int
h5trav_getinfo(hid_t file_id, trav_info_t *info)
{
    trav_visitor_t info_visitor;        /* Visitor structure for trav_info_t's */

    /* Init visitor structure */
    info_visitor.visit_obj = trav_info_visit_obj;
    info_visitor.visit_lnk = trav_info_visit_lnk;
    info_visitor.udata = info;

    /* Traverse all objects in the file, visiting each object & link */
    if(traverse(file_id, "/", TRUE, TRUE, &info_visitor) < 0)
        return -1;

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: h5trav_getindex
 *
 * Purpose: get index of OBJ in list
 *
 * Return: index, -1 if not found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */

H5_ATTR_PURE ssize_t
h5trav_getindex(const trav_info_t *info, const char *obj)
{
    size_t u;           /* Local index variable */

    /* Loop over all paths in 'info' struct, looking for object */
    for(u = 0; u < info->nused; u++) {
        /* Check for object name having full path (with leading '/') */
        if(HDstrcmp(obj, info->paths[u].path) == 0)
            return((ssize_t)u);

        /* Check for object name without leading '/' */
        if(HDstrcmp(obj, (info->paths[u].path + 1)) == 0)
            return((ssize_t)u);
    } /* end for */

    return((ssize_t)-1);
} /* end h5trav_getindex() */


/*-------------------------------------------------------------------------
 * Function: trav_info_init
 *
 * Purpose: Initialize the info
 *
 * Return: void
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 6, 2007
 *
 *-------------------------------------------------------------------------
 */

void
trav_info_init(const char *filename, hid_t fileid, trav_info_t **_info)
{
    trav_info_t *info = (trav_info_t *)HDmalloc(sizeof(trav_info_t));

    /* Init info structure */
    info->nused = info->nalloc = 0;
    info->paths = NULL;
    info->fname = filename;
    info->fid = fileid;

    /* Initialize list of visited symbolic links */
    info->symlink_visited.nused = 0;
    info->symlink_visited.nalloc = 0;
    info->symlink_visited.objs = NULL;
    info->symlink_visited.dangle_link = FALSE;
    *_info = info;
} /* end trav_info_init() */


/*-------------------------------------------------------------------------
 * Function: trav_info_free
 *
 * Purpose: free info memory
 *
 *-------------------------------------------------------------------------
 */

void
trav_info_free(trav_info_t *info)
{
    size_t u;           /* Local index variable */

    if(info) {
        /* Free visited symbolic links path and file (if alloc) */
        for(u=0; u < info->symlink_visited.nused; u++) 
        {
            if (info->symlink_visited.objs[u].file)
                HDfree(info->symlink_visited.objs[u].file);

            HDfree(info->symlink_visited.objs[u].path);
        }
        HDfree(info->symlink_visited.objs);

        /* Free path names */
        for(u = 0; u < info->nused; u++)
            HDfree(info->paths[u].path);
        HDfree(info->paths);
        HDfree(info);
    } /* end if */
} /* end trav_info_free() */


/*-------------------------------------------------------------------------
 * "h5trav table" public functions. used in h5repack
 *-------------------------------------------------------------------------
 */


/*-------------------------------------------------------------------------
 * Function: trav_table_visit_obj
 *
 * Purpose: Callback for visiting object, with 'table' sructure
 *
 * Return: 0 on success, -1 on failure
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
trav_table_visit_obj(const char *path, const H5O_info_t *oinfo,
    const char *already_visited, void *udata)
{
    trav_table_t *table = (trav_table_t *)udata;

    /* Check if we've already seen this object */
    if(NULL == already_visited)
        /* add object to table */
        trav_table_add(table, path, oinfo);
    else
        /* Add alias for object to table */
        trav_table_addlink(table, oinfo->addr, path);

    return(0);
} /* end trav_table_visit_obj() */


/*-------------------------------------------------------------------------
 * Function: trav_table_visit_lnk
 *
 * Purpose: Callback for visiting link, with 'table' sructure
 *
 * Return: 0 on success, -1 on failure
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 1, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
trav_table_visit_lnk(const char *path, const H5L_info_t H5_ATTR_UNUSED *linfo, void *udata)
{
    /* Add the link to the 'table' struct */
    trav_table_add((trav_table_t *)udata, path, NULL);

    return(0);
} /* end trav_table_visit_lnk() */


/*-------------------------------------------------------------------------
 * Function: h5trav_gettable
 *
 * Purpose: get the trav_table_t struct
 *
 * Return: 0, -1 on error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 17, 2003
 *
 *-------------------------------------------------------------------------
 */

int
h5trav_gettable(hid_t fid, trav_table_t *table)
{
    trav_visitor_t table_visitor;       /* Visitor structure for trav_table_t's */

    /* Init visitor structure */
    table_visitor.visit_obj = trav_table_visit_obj;
    table_visitor.visit_lnk = trav_table_visit_lnk;
    table_visitor.udata = table;

    /* Traverse all objects in the file, visiting each object & link */
    if(traverse(fid, "/", TRUE, TRUE, &table_visitor) < 0)
        return -1;
    return 0;
}

/*-------------------------------------------------------------------------
 * Function: h5trav_getindext
 *
 * Purpose: get index of NAME in list
 *
 * Return: index, -1 if not found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 18, 2003
 *
 *-------------------------------------------------------------------------
 */

H5_ATTR_PURE int
h5trav_getindext(const char *name, const trav_table_t *table)
{
    unsigned int i;

    for(i = 0; i < table->nobjs; i++) {
        /* Check for object name having full path (with leading '/') */
        if(HDstrcmp(name, table->objs[i].name) == 0)
            return((int)i);

        /* Check for object name without leading '/' */
        if(HDstrcmp(name, table->objs[i].name + 1) == 0)
            return((int)i);

        /* search also in the list of links */
        if(table->objs[i].nlinks) {
            unsigned int j;

            for ( j=0; j<table->objs[i].nlinks; j++) {
                /* Check for object name having full path (with leading '/') */
                if(HDstrcmp(name, table->objs[i].links[j].new_name) == 0)
                    return((int)i);

                /* Check for object name without leading '/' */
                if(HDstrcmp(name, table->objs[i].links[j].new_name + 1) == 0)
                    return((int)i);
            } /* end for */
        } /* end if */
    } /* end for */

    return -1;
}

/*-------------------------------------------------------------------------
 * Function: trav_table_add
 *
 * Purpose: Add OBJNO, NAME and TYPE of object to table
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

static void
trav_table_add(trav_table_t *table,
                    const char *path,
                    const H5O_info_t *oinfo)
{
    size_t new_obj;

    if(table->nobjs == table->size) {
        table->size = MAX(1, table->size * 2);
        table->objs = (trav_obj_t *)HDrealloc(table->objs, table->size * sizeof(trav_obj_t));
    } /* end if */

    new_obj = table->nobjs++;
    table->objs[new_obj].objno = oinfo ? oinfo->addr : HADDR_UNDEF;
    table->objs[new_obj].flags[0] = table->objs[new_obj].flags[1] = 0;
    table->objs[new_obj].is_same_trgobj = 0;
    table->objs[new_obj].name = (char *)HDstrdup(path);
    table->objs[new_obj].type = oinfo ? (h5trav_type_t)oinfo->type : H5TRAV_TYPE_LINK;
    table->objs[new_obj].nlinks = 0;
    table->objs[new_obj].sizelinks = 0;
    table->objs[new_obj].links = NULL;
}

/*-------------------------------------------------------------------------
 * Function: trav_table_addlink
 *
 * Purpose: Add a hardlink name to the object
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 17, 2003
 *
 *-------------------------------------------------------------------------
 */

static void
trav_table_addlink(trav_table_t *table, haddr_t objno, const char *path)
{
    size_t i;           /* Local index variable */

    for(i = 0; i < table->nobjs; i++) {
        if(table->objs[i].objno == objno) {
            size_t n;

            /* already inserted? */
            if(HDstrcmp(table->objs[i].name, path) == 0)
                return;

            /* allocate space if necessary */
            if(table->objs[i].nlinks == (unsigned)table->objs[i].sizelinks) {
                table->objs[i].sizelinks = MAX(1, table->objs[i].sizelinks * 2);
                table->objs[i].links = (trav_link_t *)HDrealloc(table->objs[i].links, table->objs[i].sizelinks * sizeof(trav_link_t));
            } /* end if */

            /* insert it */
            n = table->objs[i].nlinks++;
            table->objs[i].links[n].new_name = (char *)HDstrdup(path);

            return;
        } /* end for */
    } /* end for */

    HDassert(0 && "object not in table?!?");
}



/*-------------------------------------------------------------------------
 * Function: trav_table_addflags
 *
 * Purpose: Add FLAGS, NAME and TYPE of object to table
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

void trav_table_addflags(unsigned *flags,
                         char *name,
                         h5trav_type_t type,
                         trav_table_t *table)
{
    size_t new_obj;

    if(table->nobjs == table->size) {
        table->size = MAX(1, table->size * 2);
        table->objs = (trav_obj_t *)HDrealloc(table->objs, table->size * sizeof(trav_obj_t));
    } /* end if */

    new_obj = table->nobjs++;
    table->objs[new_obj].objno = 0;
    table->objs[new_obj].flags[0] = flags[0];
    table->objs[new_obj].flags[1] = flags[1];
    table->objs[new_obj].is_same_trgobj = 0;
    table->objs[new_obj].name = (char *)HDstrdup(name);
    table->objs[new_obj].type = type;
    table->objs[new_obj].nlinks = 0;
    table->objs[new_obj].sizelinks = 0;
    table->objs[new_obj].links = NULL;
}


/*-------------------------------------------------------------------------
 * Function: trav_table_init
 *
 * Purpose: Initialize the table
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

void trav_table_init(trav_table_t **tbl)
{
    trav_table_t* table = (trav_table_t*) HDmalloc(sizeof(trav_table_t));

    table->size = 0;
    table->nobjs = 0;
    table->objs = NULL;

    *tbl = table;
}



/*-------------------------------------------------------------------------
 * Function: trav_table_free
 *
 * Purpose: free table memory
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

void trav_table_free( trav_table_t *table )
{
    if(table->objs) {
        unsigned int i;

        for(i = 0; i < table->nobjs; i++) {
            HDfree(table->objs[i].name );
            if(table->objs[i].nlinks) {
                unsigned int j;

                for(j = 0; j < table->objs[i].nlinks; j++)
                    HDfree(table->objs[i].links[j].new_name);

                HDfree(table->objs[i].links);
            } /* end if */
        } /* end for */
        HDfree(table->objs);
    } /* end if */
    HDfree(table);
}

static herr_t
trav_attr(hid_t
#ifndef H5TRAV_PRINT_SPACE
H5_ATTR_UNUSED
#endif /* H5TRAV_PRINT_SPACE */
obj, const char *attr_name, const H5A_info_t H5_ATTR_UNUSED *ainfo, void *_op_data)
{
    trav_path_op_data_t *op_data = (trav_path_op_data_t *)_op_data;
    const char          *buf = op_data->path;

    if((strlen(buf)==1) && (*buf=='/'))
        printf(" %-10s %s%s", "attribute", buf, attr_name);
    else
        printf(" %-10s %s/%s", "attribute", buf, attr_name);

#ifdef H5TRAV_PRINT_SPACE
    if(trav_verbosity < 2) {
#endif
        printf("\n");
#ifdef H5TRAV_PRINT_SPACE
    }
    else {
        hid_t               attr = -1;
        hid_t               space = -1;
        hsize_t             size[H5S_MAX_RANK];
        int                 ndims;
        int                 i;
        H5S_class_t         space_type;

        if((attr = H5Aopen(obj, attr_name, H5P_DEFAULT))) {
            space = H5Aget_space(attr);

            /* Data space */
            ndims = H5Sget_simple_extent_dims(space, size, NULL);
            space_type = H5Sget_simple_extent_type(space);
            switch(space_type) {
                case H5S_SCALAR:
                    /* scalar dataspace */
                    printf(" scalar\n");
                    break;

                case H5S_SIMPLE:
                    /* simple dataspace */
                    printf(" {");
                    for (i=0; i<ndims; i++) {
                        printf("%s" HSIZE_T_FORMAT, i?", ":"", size[i]);
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

    return(0);
}


/*-------------------------------------------------------------------------
 * Function: trav_print_visit_obj
 *
 * Purpose: Callback for visiting object, when printing info
 *
 * Return: 0 on success, -1 on failure
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 6, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
trav_print_visit_obj(const char *path, const H5O_info_t *oinfo,
    const char *already_visited, void *udata)
{
    trav_print_udata_t *print_udata = (trav_print_udata_t *)udata;
    /* Print the name of the object */
    /* (no new-line, so that objects that we've encountered before can print
     *  the name of the original object)
     */
    switch(oinfo->type) {
        case H5O_TYPE_GROUP:
            printf(" %-10s %s", "group", path);
            break;

        case H5O_TYPE_DATASET:
            printf(" %-10s %s", "dataset", path);
            break;

        case H5O_TYPE_NAMED_DATATYPE:
            printf(" %-10s %s", "datatype", path);
            break;

        case H5O_TYPE_UNKNOWN:
        case H5O_TYPE_NTYPES:
        default:
            printf(" %-10s %s", "unknown object type", path);
            break;
    } /* end switch */

    /* Check if we've already seen this object */
    if(NULL == already_visited) {
        trav_path_op_data_t op_data;

        op_data.path = path;
        /* Finish printing line about object */
        printf("\n");
        if(trav_verbosity > 0)
            H5Aiterate_by_name(print_udata->fid, path, trav_index_by, trav_index_order, 
                               NULL, trav_attr, &op_data, H5P_DEFAULT);
    }
    else
        /* Print the link's original name */
        printf(" -> %s\n", already_visited);

    return(0);
} /* end trav_print_visit_obj() */


/*-------------------------------------------------------------------------
 * Function: trav_print_visit_lnk
 *
 * Purpose: Callback for visiting link, when printing info
 *
 * Return: 0 on success, -1 on failure
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 6, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
trav_print_visit_lnk(const char *path, const H5L_info_t *linfo, void *udata)
{
    trav_print_udata_t *print_udata = (trav_print_udata_t *)udata;

    /* Print appropriate information for the type of link */
    switch(linfo->type) {
        case H5L_TYPE_SOFT:
            if(linfo->u.val_size > 0) {
                char *targbuf = (char*)HDmalloc(linfo->u.val_size + 1);
                HDassert(targbuf);

                if(H5Lget_val(print_udata->fid, path, targbuf, linfo->u.val_size + 1, H5P_DEFAULT) < 0)
                    targbuf[0] = 0;
                printf(" %-10s %s -> %s\n", "link", path, targbuf);
                HDfree(targbuf);
            } /* end if */
            else
                printf(" %-10s %s ->\n", "link", path);
            break;

        case H5L_TYPE_EXTERNAL:
            if(linfo->u.val_size > 0) {
                char *targbuf;
                const char *filename = NULL;
                const char *objname = NULL;

                targbuf = (char*)HDmalloc(linfo->u.val_size + 1);
                HDassert(targbuf);

                if(H5Lget_val(print_udata->fid, path, targbuf, linfo->u.val_size + 1, H5P_DEFAULT) < 0)
                    targbuf[0] = 0;
                if(H5Lunpack_elink_val(targbuf, linfo->u.val_size, NULL, &filename, &objname) >= 0)
                    printf(" %-10s %s -> %s %s\n", "ext link", path, filename, objname);
                HDfree(targbuf);
            } /* end if */
            else
                printf(" %-10s %s ->\n", "ext link", path);
            break;

        case H5L_TYPE_HARD:
            /* Should be handled elsewhere */
            return(-1);

        case H5L_TYPE_ERROR:
        case H5L_TYPE_MAX:
        default:
            printf(" %-10s %s -> ???\n", "unknown type of UD link", path);
            break;
    } /* end switch() */

    return(0);
} /* end trav_print_visit_lnk() */


/*-------------------------------------------------------------------------
 * Function: h5trav_print
 *
 * Purpose: Print information about the objects & links in the file
 *
 * Return: 0, -1 on error
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: September 6, 2007
 *
 *-------------------------------------------------------------------------
 */

int
h5trav_print(hid_t fid)
{
    trav_print_udata_t print_udata;     /* User data for traversal */
    trav_visitor_t print_visitor;       /* Visitor structure for printing objects */

    /* Init user data for printing */
    print_udata.fid = fid;

    /* Init visitor structure */
    print_visitor.visit_obj = trav_print_visit_obj;
    print_visitor.visit_lnk = trav_print_visit_lnk;
    print_visitor.udata = &print_udata;

    /* Traverse all objects in the file, visiting each object & link */
    if(traverse(fid, "/", TRUE, TRUE, &print_visitor) < 0)
        return -1;

    return 0;
}


/*-------------------------------------------------------------------------
 * Function: h5trav_visit
 *
 * Purpose: Generic traversal routine for visiting objects and links
 *
 * Return: 0, -1 on error
 *
 * Programmer: Quincey Koziol, koziol@hdfgroup.org
 *
 * Date: November 6, 2007
 *
 *-------------------------------------------------------------------------
 */

int
h5trav_visit(hid_t fid, const char *grp_name, hbool_t visit_start,
    hbool_t recurse, h5trav_obj_func_t visit_obj, h5trav_lnk_func_t visit_lnk,
    void *udata)
{
    trav_visitor_t visitor;             /* Visitor structure for objects */

    /* Init visitor structure */
    visitor.visit_obj = visit_obj;
    visitor.visit_lnk = visit_lnk;
    visitor.udata = udata;

    /* Traverse all objects in the file, visiting each object & link */
    if(traverse(fid, grp_name, visit_start, recurse, &visitor) < 0)
        return -1;

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: symlink_visit_add
 *
 * Purpose: Add an symbolic link to visited data structure
 *
 * Return: 0 on success, -1 on failure
 *
 * Programmer: Neil Fortner, nfortne2@hdfgroup.org
 *             Adapted from trav_addr_add in h5trav.c by Quincey Koziol
 *
 * Date: September 5, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
symlink_visit_add(symlink_trav_t *visited, H5L_type_t type, const char *file, const char *path)
{
    size_t  idx;         /* Index of address to use */

    /* Allocate space if necessary */
    if(visited->nused == visited->nalloc) {
        void    *tmp_ptr;

        visited->nalloc = MAX(1, visited->nalloc * 2);
        if(NULL == (tmp_ptr = HDrealloc(visited->objs, visited->nalloc * sizeof(symlink_trav_path_t))))
            return -1;
        visited->objs = (symlink_trav_path_t *)tmp_ptr;
    } /* end if */

    /* Append it */
    idx = visited->nused++;

    visited->objs[idx].type = type;
    visited->objs[idx].file = NULL;
    visited->objs[idx].path = NULL;

    if(type == H5L_TYPE_EXTERNAL) {
        if(NULL == (visited->objs[idx].file = HDstrdup(file))) {
            visited->nused--;
            return -1;
        } /* end if */
    } /* end if */

    if(NULL == (visited->objs[idx].path = HDstrdup(path))) {
        visited->nused--;
        if(visited->objs[idx].file)
            HDfree (visited->objs[idx].file);
        return -1;
    } /* end if */

    return 0;
} /* end symlink_visit_add() */


/*-------------------------------------------------------------------------
 * Function: symlink_is_visited
 *
 * Purpose: Check if an symbolic link has already been visited
 *
 * Return: TRUE/FALSE
 *
 * Programmer: Neil Fortner, nfortne2@hdfgroup.org
 *             Adapted from trav_addr_visited in h5trav.c by Quincey Koziol
 *
 * Date: September 5, 2008
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE hbool_t
symlink_is_visited(symlink_trav_t *visited, H5L_type_t type, const char *file, const char *path)
{
    size_t u;  /* Local index variable */

    /* Look for symlink */
    for(u = 0; u < visited->nused; u++) {
        /* Check for symlink values already in array */
        /* check type and path pair to distingush between symbolic links */
        if((visited->objs[u].type == type) && !HDstrcmp(visited->objs[u].path, path)) {
            /* if external link, file need to be matched as well */
            if(visited->objs[u].type == H5L_TYPE_EXTERNAL)
                if(!HDstrcmp(visited->objs[u].file, file))
                    return(TRUE);

            return (TRUE);
        } /* end if */
    } /* end for */

    /* Didn't find symlink */
    return(FALSE);
} /* end symlink_is_visited() */

