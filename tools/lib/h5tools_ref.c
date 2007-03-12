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

#include <stdio.h>
#include <stdlib.h>
#include "h5tools_ref.h"
#include "H5private.h"
#include "H5SLprivate.h"
#include "h5tools.h"
#include "h5tools_utils.h"


/*
 *  Table to look up a path name for an object
 *  reference.
 *
 *  This table stores mappings of reference -> path
 *  for all objects in the file that may be the target of
 *  an object reference.
 *
 *  The 'path' is an absolute path by which the object
 *  can be accessed.  When an object has > 1 such path,
 *  only one will be used in the table, with no particular
 *  method of selecting which one.
 */

typedef struct {
    haddr_t objno;      /* Object ID (i.e. address) */
    const char *path;   /* Object path */
} ref_path_node_t;

static H5SL_t *ref_path_table = NULL;   /* the "table" (implemented with a skip list) */
static hid_t thefile;

extern char  *progname;
extern int   d_status;

static int ref_path_table_put(const char *, haddr_t objno);
static hbool_t ref_path_table_find(haddr_t objno);

/*-------------------------------------------------------------------------
 * Function:    init_ref_path_table
 *
 * Purpose:     Enter the root group ("/") into the path table
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
init_ref_path_table(hid_t fid)
{
    H5G_stat_t              sb;
    haddr_t objno;
    char *root_path;

    /* Set file ID for later queries (XXX: this should be fixed) */
    thefile = fid;

    /* Create skip list to store reference path information */
    if((ref_path_table = H5SL_create(H5SL_TYPE_HADDR, 0.5, (size_t)16))==NULL)
    return (-1);

    if((root_path = HDstrdup("/")) == NULL)
    return (-1);

    if(H5Gget_objinfo(fid, "/", TRUE, &sb)<0) {
    /* fatal error? */
    HDfree(root_path);
    return (-1);
    }

    /* Insert into table (takes ownership of path) */
    objno = ((haddr_t)sb.objno[1] << (8*sizeof(long))) | (haddr_t)sb.objno[0];
    ref_path_table_put(root_path, objno);

    return(0);
}

/*-------------------------------------------------------------------------
 * Function:    free_ref_path_info
 *
 * Purpose:     Free the key for a reference path table node
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
free_ref_path_info(void *item, void UNUSED *key, void UNUSED *operator_data/*in,out*/)
{
    ref_path_node_t *node = (ref_path_node_t *)item;

    HDfree((void *)node->path);
    HDfree(node);

    return(0);
}

/*-------------------------------------------------------------------------
 * Function:    term_ref_path_table
 *
 * Purpose:     Terminate the reference path table
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
term_ref_path_table(void)
{
    /* Destroy reference path table, freeing all memory */
    if(ref_path_table)
        H5SL_destroy(ref_path_table, free_ref_path_info, NULL);

    return(0);
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_lookup
 *
 * Purpose:     Looks up a table entry given a path name.
 *              Used during construction of the table.
 *
 * Return:      The table entre (pte) or NULL if not in the
 *              table.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
ref_path_table_lookup(const char *thepath)
{
    H5G_stat_t  sb;
    haddr_t objno;
    haddr_t     ret_value;

    /* Get object ID for object at path */
    if(H5Gget_objinfo(thefile, thepath, TRUE, &sb)<0)
    /*  fatal error ? */
    return HADDR_UNDEF;

    /* Return OID or HADDR_UNDEF */
    objno = ((haddr_t)sb.objno[1] << (8*sizeof(long))) | (haddr_t)sb.objno[0];
    ret_value = ref_path_table_find(objno) ? objno : HADDR_UNDEF;

    return(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_find
 *
 * Purpose:     Looks up a table entry given a object number.
 *              Used during construction of the table.
 *
 * Return:      TRUE/FALSE on success, can't fail
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
ref_path_table_find(haddr_t objno)
{
    HDassert(ref_path_table);

    if(H5SL_search(ref_path_table, &objno) == NULL)
        return FALSE;
    else
        return TRUE;
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_put
 *
 * Purpose:     Enter the 'obj' with 'path' in the table (assumes its not
 *              already there)
 *
 *              Create an object reference, pte, and store them
 *              in the table.
 *
 *              NOTE: Takes ownership of the path name string passed in!
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
ref_path_table_put(const char *path, haddr_t objno)
{
    ref_path_node_t *new_node;

    HDassert(ref_path_table);
    HDassert(path);

    if((new_node = HDmalloc(sizeof(ref_path_node_t))) == NULL)
        return(-1);

    new_node->objno = objno;
    new_node->path = path;

    return(H5SL_insert(ref_path_table, new_node, &(new_node->objno)));
}

/*
 *  counter used to disambiguate multiple instances of same object.
 */
int xid = 1;

int get_next_xid() {
    return xid++;
}

/*
 *  This counter is used to create fake object ID's
 *  The idea is to set it to the largest possible offest, which
 *  minimizes the chance of collision with a real object id.
 *
 */
haddr_t fake_xid = HADDR_MAX;
haddr_t
get_fake_xid () {
    return (fake_xid--);
}

/*
 * for an object that does not have an object id (e.g., soft link),
 * create a table entry with a fake object id as the key.
 *
 * Assumes 'path' is for an object that is not in the table.
 *
 */

haddr_t
ref_path_table_gen_fake(const char *path)
{
    const char *dup_path;
    haddr_t fake_objno;

    if((dup_path = HDstrdup(path)) == NULL)
        return HADDR_UNDEF;

    /* Generate fake ID for string */
    fake_objno = get_fake_xid();

    /* Insert "fake" object into table (takes ownership of path) */
    ref_path_table_put(dup_path, fake_objno);

    return(fake_objno);
}

/*-------------------------------------------------------------------------
 * Function:    lookup_ref_path
 *
 * Purpose:     Lookup the path to the object with refernce 'ref'.
 *
 * Return:      Return a path to the object, or NULL if not found.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const char *
lookup_ref_path(haddr_t ref)
{
    uint8_t                *p;          /* Pointer to reference to translate */
    haddr_t                 addr;       /* Resulting address */
    unsigned            i;          /* Local index variable */
    haddr_t         tmp;        /* Temporary portion of address */
    uint8_t         c;          /* Byte from address */
    hbool_t         all_zero = TRUE;    /* If the address is all zeros, make into HADDR_UNDEF */
    ref_path_node_t        *node;       /* Ref path node found for address */

    /* Be safer for h5ls */
    if(!ref_path_table)
        return(NULL);

    /* Compensate for endianness differences */
    p = (uint8_t *)&ref;
    addr = 0;

    for (i=0; i<sizeof(haddr_t); i++) {
    c = *p++;
    if (c != 0xff)
            all_zero = FALSE;

    if (i<sizeof(haddr_t)) {
        tmp = c;
        tmp <<= (i * 8);    /*use tmp to get casting right */
        addr |= tmp;
    } else if (!all_zero) {
        assert(0 == *p);    /*overflow */
    }
    }
    if (all_zero)
        addr = HADDR_UNDEF;

    /* Check if address is in reference path table */
    node = H5SL_search(ref_path_table, &addr);

    return(node ? node->path : NULL);
}

/*-------------------------------------------------------------------------
 * Function:    fill_ref_path_table
 *
 * Purpose:     Called by interator to create references for
 *              all objects and enter them in the table.
 *
 * Return:      Error status.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
fill_ref_path_table(hid_t group, const char *obj_name, void *op_data)
{
    const char *obj_prefix = (const char *)op_data;
    H5G_stat_t              statbuf;
    haddr_t objno;

    H5Gget_objinfo(group, obj_name, FALSE, &statbuf);
    objno = ((haddr_t)statbuf.objno[1] << (8*sizeof(long))) | (haddr_t)statbuf.objno[0];

    /* Check if the object is in the path table */
    if (!ref_path_table_find(objno)) {
        size_t                  tmp_len;
        char                   *thepath;

        /* Compute length for this object's path */
        tmp_len = HDstrlen(obj_prefix) + HDstrlen(obj_name) + 2;

        /* Allocate room for the path for this object */
        if ((thepath = (char *) HDmalloc(tmp_len)) == NULL)
            return FAIL;

        /* Build the name for this object */
        HDstrcpy(thepath, obj_prefix);
        HDstrcat(thepath, "/");
        HDstrcat(thepath, obj_name);

        /* Insert the object into the path table */
        ref_path_table_put(thepath, objno);

        if(statbuf.type == H5G_GROUP) {
            /* Iterate over objects in this group, using this group's
             * name as their prefix
             */
            if(H5Giterate(group, obj_name, NULL, fill_ref_path_table, thepath) < 0) {
                error_msg(progname, "unable to dump group \"%s\"\n", obj_name);
                d_status = EXIT_FAILURE;
            }
        }
    }

    return 0;
}

