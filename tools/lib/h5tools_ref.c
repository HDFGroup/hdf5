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

#include "h5tools_ref.h"
#include "H5private.h"
#include "H5SLprivate.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5trav.h"
#include "H5VLnative_private.h"

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
    H5O_token_t obj_token; /* Object token */
    char       *path;      /* Object path */
} ref_path_node_t;

static H5SL_t *ref_path_table = NULL; /* the "table" (implemented with a skip list) */
static hid_t   thefile        = (-1);

static int ref_path_table_put(const char *, const H5O_token_t *token);

/*-------------------------------------------------------------------------
 * Function:    free_ref_path_info
 *
 * Purpose:     Free the key for a reference path table node
 *
 * Return:      Non-negative on success, negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
free_ref_path_info(void *item, void H5_ATTR_UNUSED *key, void H5_ATTR_UNUSED *operator_data /*in,out*/)
{
    ref_path_node_t *node = (ref_path_node_t *)item;

    free(node->path);
    free(node);

    return (0);
}

/*-------------------------------------------------------------------------
 * Function:    init_ref_path_cb
 *
 * Purpose:     Called by iterator to create references for
 *              all objects and enter them in the table.
 *
 * Return:      Error status.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
init_ref_path_cb(const char *obj_name, const H5O_info2_t *oinfo, const char *already_seen,
                 void H5_ATTR_UNUSED *_udata)
{
    /* Check if the object is already in the path table */
    if (NULL == already_seen) {
        /* Insert the object into the path table */
        ref_path_table_put(obj_name, &oinfo->token);
    } /* end if */

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_cmp
 *
 * Purpose:     Skip list key comparison function which compares two
 *              H5O_token_t objects.
 *
 * Return:      Negative (if token2 is greater than token1)
 *              0 (if tokens are equal)
 *              or
 *              Positive (if token1 is greater than token2)
 *
 *-------------------------------------------------------------------------
 */
static int
ref_path_table_cmp(const void *key1, const void *key2)
{
    const H5O_token_t *token1    = (const H5O_token_t *)key1;
    const H5O_token_t *token2    = (const H5O_token_t *)key2;
    int                cmp_value = 0;

    if (thefile > 0)
        H5Otoken_cmp(thefile, token1, token2, &cmp_value);
    else
        cmp_value = memcmp(token1, token2, sizeof(H5O_token_t));

    return cmp_value;
}

/*-------------------------------------------------------------------------
 * Function:    init_ref_path_table
 *
 * Purpose:     Initialize the reference path table
 *
 * Return:      Non-negative on success, negative on failure
 *
 *-------------------------------------------------------------------------
 */
static int
init_ref_path_table(void)
{
    /* Sanity check */
    if (thefile > 0) {
        /* Create skip list to store reference path information */
        if ((ref_path_table = H5SL_create(H5SL_TYPE_GENERIC, ref_path_table_cmp)) == NULL)
            return (-1);

        /* Iterate over objects in this file */
        if (h5trav_visit(thefile, "/", true, true, init_ref_path_cb, NULL, NULL, H5O_INFO_BASIC) < 0) {
            error_msg("unable to construct reference path table\n");
            h5tools_setstatus(EXIT_FAILURE);
        } /* end if */

        return (0);
    }
    else
        return (-1);
}

/*-------------------------------------------------------------------------
 * Function:    term_ref_path_table
 *
 * Purpose:     Terminate the reference path table
 *
 * Return:      Non-negative on success, negative on failure
 *
 *-------------------------------------------------------------------------
 */
int
term_ref_path_table(void)
{
    /* Destroy reference path table, freeing all memory */
    if (ref_path_table)
        H5SL_destroy(ref_path_table, free_ref_path_info, NULL);

    return (0);
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_lookup
 *
 * Purpose:     Looks up a table entry given a path name.
 *              Used during construction of the table.
 *
 * Return:      Negative on failure, Non-negative on success. The object
 *              token for the table entry is returned through the token
 *              parameter if the table entry is found by the given path
 *              name.
 *
 *-------------------------------------------------------------------------
 */
int
ref_path_table_lookup(const char *thepath, H5O_token_t *token)
{
    H5O_info2_t oi;

    if ((thepath == NULL) || (strlen(thepath) == 0))
        return -1;
    /* Allow lookups on the root group, even though it doesn't have any link info */
    if (strcmp(thepath, "/") != 0) {
        H5L_info2_t li;

        /* Check for external link first, so we don't return the OID of an object in another file */
        if (H5Lget_info2(thefile, thepath, &li, H5P_DEFAULT) < 0)
            return -1;

        /* UD links can't be followed, so they always "dangle" like soft links.  */
        if (li.type >= H5L_TYPE_UD_MIN)
            return -1;
    } /* end if */

    /* Get the object info now */
    /* (returns failure for dangling soft links) */
    if (H5Oget_info_by_name3(thefile, thepath, &oi, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
        return -1;

    /* Return object token through parameter */
    memcpy(token, &oi.token, sizeof(H5O_token_t));

    return 0;
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
 *-------------------------------------------------------------------------
 */
static int
ref_path_table_put(const char *path, const H5O_token_t *token)
{
    ref_path_node_t *new_node;

    if (ref_path_table && path) {
        if ((new_node = (ref_path_node_t *)malloc(sizeof(ref_path_node_t))) == NULL)
            return (-1);

        memcpy(&new_node->obj_token, token, sizeof(H5O_token_t));
        new_node->path = strdup(path);

        return (H5SL_insert(ref_path_table, new_node, &(new_node->obj_token)));
    }
    else
        return (-1);
}

/*
 *  counter used to disambiguate multiple instances of same object.
 */
static int xid = 1;

int
get_next_xid(void)
{
    return xid++;
}

/*
 *  This counter is used to create fake object ID's
 *  The idea is to set it to the largest possible offset, which
 *  minimizes the chance of collision with a real object id.
 *
 */
static haddr_t fake_xid = HADDR_MAX;

void
get_fake_token(H5O_token_t *token)
{
    if (thefile > 0) {
        /* TODO: potential for this to be called with non-native connector objects */
        if (H5VLnative_addr_to_token(thefile, fake_xid, token) < 0)
            *token = H5O_TOKEN_UNDEF;
        fake_xid--;
    }
    else
        *token = H5O_TOKEN_UNDEF;
}

/*
 * for an object that does not have an object token (e.g., soft link),
 * create a table entry with a fake object token as the key.
 *
 * Assumes 'path' is for an object that is not in the table.
 *
 */

void
ref_path_table_gen_fake(const char *path, H5O_token_t *token)
{
    /* Generate fake object token for string */
    get_fake_token(token);

    /* Create ref path table, if it hasn't already been created */
    if (ref_path_table == NULL)
        init_ref_path_table();

    /* Insert "fake" object into table */
    ref_path_table_put(path, token);
}

/*-------------------------------------------------------------------------
 * Function:    lookup_ref_path
 *
 * Purpose:     Lookup the path to the object with the reference 'refbuf'.
 *
 * Return:      Return a path to the object, or NULL if not found.
 *
 *-------------------------------------------------------------------------
 */
const char *
lookup_ref_path(H5R_ref_t refbuf)
{
    H5O_info2_t      oinfo;
    H5R_type_t       ref_type;
    hid_t            ref_object;
    ref_path_node_t *node;

    /* Be safer for h5ls */
    if (thefile < 0)
        return (NULL);

    /* Retrieve reference type */
    if (H5R_BADTYPE == (ref_type = H5Rget_type(&refbuf)))
        return (NULL);

    /* Open the referenced object */
    switch (ref_type) {
        case H5R_OBJECT1:
        case H5R_OBJECT2:
            if ((ref_object = H5Ropen_object(&refbuf, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                return (NULL);
            break;

        /* Invalid referenced object type */
        case H5R_DATASET_REGION1:
        case H5R_DATASET_REGION2:
        case H5R_ATTR:
        case H5R_MAXTYPE:
        case H5R_BADTYPE:
        default:
            return (NULL);
    }

    /* Retrieve info about the referenced object */
    if (H5Oget_info3(ref_object, &oinfo, H5O_INFO_ALL) < 0)
        return (NULL);

    /* Create ref path table, if it hasn't already been created */
    if (ref_path_table == NULL)
        init_ref_path_table();

    node = (ref_path_node_t *)H5SL_search(ref_path_table, &oinfo.token);

    return (node ? node->path : NULL);
}

/*-------------------------------------------------------------------------
 * Function:    fill_ref_path_table
 *
 * Purpose:     Called by iterator to create references for
 *              all objects and enter them in the table.
 *
 * Return:      Error status.
 *
 *-------------------------------------------------------------------------
 */
herr_t
fill_ref_path_table(hid_t fid)
{
    /* Set file ID for later queries (XXX: this should be fixed) */
    thefile = fid;

    /* Defer creating the ref path table until it's needed */

    return 0;
}
