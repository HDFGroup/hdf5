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
typedef struct trav_addr_t {
    size_t      nalloc;
    size_t      nused;
    haddr_t     *addrs;
} trav_addr_t;

typedef struct {
    herr_t (*visit_obj)(const char *path_name, const H5O_info_t *oinfo, hbool_t already_visited, void *udata);
    herr_t (*visit_lnk)(const char *path_name, const H5L_info_t *linfo, void *udata);
    void *udata;                /* User data to pass to callbacks */
} trav_visitor_t;

typedef struct {
    trav_addr_t *seen;              /* List of addresses seen already */
    const char *curr_path;          /* Current path to parent group */
    const trav_visitor_t *visitor;  /* Information for visiting each link/object */
} trav_ud_traverse_t;

typedef struct {
    hid_t fid;                      /* File ID being traversed */
    trav_table_t *table;            /* Table for tracking name of objects with >1 hard link */
} trav_print_udata_t;

/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */
static void trav_table_add(trav_table_t *table,
                        const char *objname,
                        const H5O_info_t *oinfo);

static size_t trav_table_search(const trav_table_t *table, haddr_t objno);

static void trav_table_addlink(trav_table_t *table,
                        size_t j /* the object index */,
                        const char *path);

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
trav_addr_add(trav_addr_t *visited, haddr_t addr)
{
    size_t idx;         /* Index of address to use */

    /* Allocate space if necessary */
    if(visited->nused == visited->nalloc) {
        visited->nalloc = MAX(1, visited->nalloc * 2);;
        visited->addrs = (haddr_t *)HDrealloc(visited->addrs, visited->nalloc * sizeof(haddr_t));
    } /* end if */

    /* Append it */
    idx = visited->nused++;
    visited->addrs[idx] = addr;
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
static hbool_t
trav_addr_visited(trav_addr_t *visited, haddr_t addr)
{
    size_t u;           /* Local index variable */

    /* Look for address */
    for(u = 0; u < visited->nused; u++)
        /* Check for address already in array */
        if(visited->addrs[u] == addr)
            return(TRUE);

    /* Didn't find address */
    return(FALSE);
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
traverse_cb(hid_t loc_id, const char *link_name, const H5L_info_t *linfo,
    void *_udata)
{
    trav_ud_traverse_t *udata = (trav_ud_traverse_t *)_udata;     /* User data */
    hbool_t is_group = FALSE;           /* If the object is a group */
    hbool_t already_visited = FALSE;    /* Whether the link/object was already visited */
    char *link_path;            /* Full path name of a link */

    /* Construct the full path name of this link */
    link_path = (char*)HDmalloc(HDstrlen(udata->curr_path) + HDstrlen(link_name) + 2);
    HDassert(link_path);
    HDstrcpy(link_path, udata->curr_path);
    HDstrcat(link_path, "/");
    HDstrcat(link_path, link_name);

    /* Perform the correct action for different types of links */
    if(linfo->type == H5L_TYPE_HARD) {
        H5O_info_t oinfo;

        /* Get information about the object */
        if(H5Oget_info_by_name(loc_id, link_name, &oinfo, H5P_DEFAULT) < 0)
            return(H5_ITER_ERROR);

        /* If the object has multiple links, add it to the list of addresses
         *  already visited, if it isn't there already
         */
        if(oinfo.rc > 1) {
            already_visited = trav_addr_visited(udata->seen, oinfo.addr);
            if(!already_visited)
                trav_addr_add(udata->seen, oinfo.addr);
        } /* end if */

        /* Check if object is a group, for later */
        is_group = (oinfo.type == H5O_TYPE_GROUP) ? TRUE : FALSE;

        /* Make 'visit object' callback */
        if(udata->visitor->visit_obj)
            (*udata->visitor->visit_obj)(link_path, &oinfo, already_visited, udata->visitor->udata);
    } /* end if */
    else {
        /* Make 'visit link' callback */
        if(udata->visitor->visit_lnk)
            (*udata->visitor->visit_lnk)(link_path, linfo, udata->visitor->udata);
    } /* end else */

    /* Check for group that we haven't visited yet & recurse */
    if(is_group && !already_visited) {
        const char *prev_path = udata->curr_path;     /* Previous path to link's parent group */

        /* Set current path to this object */
        udata->curr_path = link_path;

        /* Iterate over all links in group object */
        if(H5Literate_by_name(loc_id, link_name, H5_INDEX_NAME, H5_ITER_INC, NULL, traverse_cb, udata, H5P_DEFAULT) < 0)
            return(H5_ITER_ERROR);

        /* Restore path in udata */
        udata->curr_path = prev_path;
    } /* end if */

    /* Free path name for current link/object */
    HDfree(link_path);

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
traverse(hid_t file_id, const trav_visitor_t *visitor)
{
    H5O_info_t  oinfo;          /* Object info for root group */
    trav_addr_t seen;           /* List of addresses seen */
    trav_ud_traverse_t udata;   /* User data for iteration callback */

    /* Get info for root group */
    if(H5Oget_info(file_id, &oinfo) < 0)
        return -1;

    /* Visit the root group of the file */
    (*visitor->visit_obj)("/", &oinfo, FALSE, visitor->udata);

    /* Init addresses seen */
    seen.nused = seen.nalloc = 0;
    seen.addrs = NULL;

    /* Check for multiple links to root group */
    if(oinfo.rc > 1)
        trav_addr_add(&seen, oinfo.addr);

    /* Set up user data structure */
    udata.seen = &seen;
    udata.curr_path = "";
    udata.visitor = visitor;

    /* Iterate over all links in root group */
    if(H5Literate(file_id, H5_INDEX_NAME, H5_ITER_INC, NULL, traverse_cb, &udata) < 0)
        return -1;

    /* Free visited addresses table */
    if(seen.addrs)
        HDfree(seen.addrs);

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
static void
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
} /* end trav_info_add() */


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
static int
trav_info_visit_obj(const char *path, const H5O_info_t *oinfo,
    hbool_t UNUSED already_visited, void *udata)
{
    /* Add the object to the 'info' struct */
    /* (object types map directly to "traversal" types) */
    trav_info_add((trav_info_t *)udata, path, (h5trav_type_t)oinfo->type);

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
static int
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
    if(traverse(file_id, &info_visitor) < 0)
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

ssize_t
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
trav_info_init(trav_info_t **_info)
{
    trav_info_t *info = (trav_info_t *)HDmalloc(sizeof(trav_info_t));

    /* Init info structure */
    info->nused = info->nalloc = 0;
    info->paths = NULL;

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
    hbool_t already_visited, void *udata)
{
    trav_table_t *table = (trav_table_t *)udata;

    /* Check if we've already seen this object */
    if(!already_visited)
        /* add object to table */
        trav_table_add(table, path, oinfo);
    else {
        size_t found;           /* Index of original object seen */

        /* Look for object in existing table */
        found = trav_table_search(table, oinfo->addr);
        HDassert(found < table->nobjs);

        /* Add alias for object to table */
        trav_table_addlink(table, found, path);
    } /* end else */

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
trav_table_visit_lnk(const char *path, const H5L_info_t UNUSED *linfo, void *udata)
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
    if(traverse(fid, &table_visitor) < 0)
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

int
h5trav_getindext(const char *name, const trav_table_t *table)
{
    unsigned int i;

    for(i = 0; i < table->nobjs; i++) {
        /* Check for object name having full path (with leading '/') */
        if(HDstrcmp(name, table->objs[i].name) == 0)
            return(i);

        /* Check for object name without leading '/' */
        if(HDstrcmp(name, table->objs[i].name + 1) == 0)
            return(i);

        /* search also in the list of links */
        if(table->objs[i].nlinks) {
            unsigned int j;

            for ( j=0; j<table->objs[i].nlinks; j++) {
                /* Check for object name having full path (with leading '/') */
                if(HDstrcmp(name, table->objs[i].links[j].new_name) == 0)
                    return(i);

                /* Check for object name without leading '/' */
                if(HDstrcmp(name, table->objs[i].links[j].new_name + 1) == 0)
                    return(i);
            } /* end for */
        } /* end if */
    } /* end for */

    return -1;
}

/*-------------------------------------------------------------------------
 * Function: trav_table_search
 *
 * Purpose: Search in the table for OBJNO
 *
 * Return: index of object in table
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

static size_t
trav_table_search(const trav_table_t *table, haddr_t objno)
{
    size_t i;

    for(i = 0; i < table->nobjs; i++)
        if(table->objs[i].objno == objno)
            return(i);
    return(i);
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
    size_t new;

    if(table->nobjs == table->size) {
        table->size = MAX(1, table->size * 2);
        table->objs = (trav_obj_t*)HDrealloc(table->objs, table->size * sizeof(trav_obj_t));
    } /* end if */

    new = table->nobjs++;
    table->objs[new].objno = oinfo ? oinfo->addr : HADDR_UNDEF;
    table->objs[new].flags[0] = table->objs[new].flags[1] = 0;
    table->objs[new].name = (char *)HDstrdup(path);
    table->objs[new].type = oinfo ? (h5trav_type_t)oinfo->type : H5TRAV_TYPE_LINK;
    table->objs[new].nlinks = 0;
    table->objs[new].sizelinks = 0;
    table->objs[new].links = NULL;
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
trav_table_addlink(trav_table_t *table,
                        size_t j /* the object index */,
                        const char *path)
{
    size_t new;

    /* already inserted */
    if(HDstrcmp(table->objs[j].name, path) == 0)
        return;

    /* allocate space if necessary */
    if(table->objs[j].nlinks == (unsigned)table->objs[j].sizelinks) {
        table->objs[j].sizelinks = MAX(1, table->objs[j].sizelinks * 2);
        table->objs[j].links = (trav_link_t*)HDrealloc(table->objs[j].links, table->objs[j].sizelinks * sizeof(trav_link_t));
    } /* end if */

    /* insert it */
    new = table->objs[j].nlinks++;
    table->objs[j].links[new].new_name = (char *)HDstrdup(path);
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
    unsigned int new;

    if(table->nobjs == table->size) {
        table->size = MAX(1, table->size * 2);
        table->objs = (trav_obj_t *)HDrealloc(table->objs, table->size * sizeof(trav_obj_t));
    } /* end if */

    new = table->nobjs++;
    table->objs[new].objno = 0;
    table->objs[new].flags[0] = flags[0];
    table->objs[new].flags[1] = flags[1];
    table->objs[new].name = (char *)HDstrdup(name);
    table->objs[new].type = type;
    table->objs[new].nlinks = 0;
    table->objs[new].sizelinks = 0;
    table->objs[new].links = NULL;
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
    hbool_t already_visited, void *udata)
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

        default:
            printf(" %-10s %s", "unknown object type", path);
            break;
    } /* end switch */

    /* Check if we've already seen this object */
    if(!already_visited) {
        /* Finish printing line about object */
        printf("\n");

        /* Check if we will encounter another hard link to this object */
        if(oinfo->rc > 1) {
            /* Add object to table */
            trav_table_add(print_udata->table, path, oinfo);
        } /* end if */
    } /* end if */
    else {
        size_t found;           /* Index of original object seen */

        /* Locate object in table */
        found = trav_table_search(print_udata->table, oinfo->addr);
        HDassert(found < print_udata->table->nobjs);

        /* Print the link's destination */
        printf(" -> %s\n", print_udata->table->objs[found].name);
    } /* end else */

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
                char *targbuf = HDmalloc(linfo->u.val_size + 1);
                HDassert(targbuf);

                H5Lget_val(print_udata->fid, path, targbuf, linfo->u.val_size + 1, H5P_DEFAULT);
                printf(" %-10s %s -> %s\n", "link", path, targbuf);
                free(targbuf);
            } /* end if */
            else
                printf(" %-10s %s ->\n", "link", path);
            break;

        case H5L_TYPE_EXTERNAL:
            if(linfo->u.val_size > 0) {
                char *targbuf;
                const char *filename;
                const char *objname;

                targbuf = HDmalloc(linfo->u.val_size + 1);
                assert(targbuf);

                H5Lget_val(print_udata->fid, path, targbuf, linfo->u.val_size + 1, H5P_DEFAULT);
                H5Lunpack_elink_val(targbuf, linfo->u.val_size, NULL, &filename, &objname);
                printf(" %-10s %s -> %s %s\n", "ext link", path, filename, objname);
                free(targbuf);
            } /* end if */
            else
                printf(" %-10s %s ->\n", "ext link", path);
            break;

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
    trav_table_t *table = NULL;         /* Table for objects w/multiple hard links */
    trav_print_udata_t print_udata;     /* User data for traversal */
    trav_visitor_t print_visitor;       /* Visitor structure for printing objects */

    /* Initialize the table */
    trav_table_init(&table);

    /* Init user data for printing */
    print_udata.fid = fid;
    print_udata.table = table;

    /* Init visitor structure */
    print_visitor.visit_obj = trav_print_visit_obj;
    print_visitor.visit_lnk = trav_print_visit_lnk;
    print_visitor.udata = &print_udata;

    /* Traverse all objects in the file, visiting each object & link */
    if(traverse(fid, &print_visitor) < 0)
        return -1;

    /* Free table */
    trav_table_free(table);

    return 0;
}

