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

#include <stdio.h>
#include <stdlib.h>
#include "h5tools_ref.h"
#include "H5private.h"
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


extern hid_t thefile;
extern char  *prefix;
extern char  *progname;
extern int   d_status;


struct ref_path_table_entry_t *ref_path_table = NULL;	/* the table */
int                     npte = 0;	/* number of entries in the table */

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
struct ref_path_table_entry_t *
ref_path_table_lookup(const char *thepath)
{
    int                     i;
    hobj_ref_t             *ref;
    herr_t                  status;
    struct ref_path_table_entry_t *pte = ref_path_table;

    if (ref_path_table == NULL)
	return NULL;

    ref = (hobj_ref_t *) malloc(sizeof(hobj_ref_t));

    if (ref == NULL) {
	/*  fatal error ? */
	return NULL;
    }

    status = H5Rcreate(ref, thefile, thepath, H5R_OBJECT, -1);

    if (status < 0) {
	/*  fatal error ? */
	return NULL;
    }

    for (i = 0; i < npte; i++) {
	if (memcmp(ref, pte->obj_ref, sizeof(hobj_ref_t)) == 0) {
	    return pte;
	}

	pte = pte->next;
    }

    return NULL;
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_put
 *
 * Purpose:     Enter the 'obj' with 'path' in the table if
 *              not already there.
 *              Create an object reference, pte, and store them
 *              in the table.
 *
 * Return:      The object reference for the object.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hobj_ref_t *
ref_path_table_put(hid_t obj, const char *path)
{
    hobj_ref_t             *ref;
    H5G_stat_t             *sb;
    herr_t                  status;
    struct ref_path_table_entry_t *pte;

    /* look up 'obj'.  If already in table, return */
    pte = ref_path_table_lookup(path);
    if (pte != NULL)
	return pte->obj_ref;

    /* if not found, then make new entry */

    pte = (struct ref_path_table_entry_t *)
	malloc(sizeof(struct ref_path_table_entry_t));
    if (pte == NULL) {
	/* fatal error? */
	return NULL;
    }

    pte->obj = obj;
    ref = (hobj_ref_t *) malloc(sizeof(hobj_ref_t));
    if (ref == NULL) {
	/* fatal error? */
	free(pte);
	return NULL;
    }

    status = H5Rcreate(ref, thefile, path, H5R_OBJECT, -1);
    if (status < 0) {
	/* fatal error? */
	free(ref);
	free(pte);
	return NULL;
    }

    pte->obj_ref = ref;

    pte->apath = HDstrdup(path);

    sb = (H5G_stat_t *) malloc(sizeof(H5G_stat_t));
    if (sb == NULL) {
	/* fatal error? */
	free(pte);
	return NULL;
    }
    H5Gget_objinfo(thefile, path, TRUE, sb);

    memcpy((char *)&(pte->statbuf),(char *)sb,sizeof(H5G_stat_t));

    pte->next = ref_path_table;
    ref_path_table = pte;

    npte++;

    return ref;
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
 */

struct ref_path_table_entry_t *
ref_path_table_gen_fake(const char *path)
{
	union {
		hobj_ref_t             rr;
		char cc[16];
		unsigned long ll[2];
	} uu;
	hobj_ref_t             *ref;
	H5G_stat_t             *sb;
	struct ref_path_table_entry_t *pte;

    /* look up 'obj'.  If already in table, return */
    pte = ref_path_table_lookup(path);
    if (pte != NULL) {
	return pte;
	}

    /* if not found, then make new entry */

    pte = (struct ref_path_table_entry_t *)
	malloc(sizeof(struct ref_path_table_entry_t));
    if (pte == NULL) {
	/* fatal error? */
	return NULL;
    }

    pte->obj = (hid_t)-1;

    sb = (H5G_stat_t *) malloc(sizeof(H5G_stat_t));
    if (sb == NULL) {
	/* fatal error? */
	free(pte);
	return NULL;
    }
    sb->objno[0] = (unsigned long)get_fake_xid();
    sb->objno[1] = (unsigned long)get_fake_xid();

    memcpy((char *)&(pte->statbuf),(char *)sb,sizeof(H5G_stat_t));

    ref = (hobj_ref_t *) malloc(sizeof(hobj_ref_t));
    if (ref == NULL) {
	free(pte);
	return NULL;
    }

    uu.ll[0] = sb->objno[0];
    uu.ll[1] = sb->objno[1];

    memcpy((char *)ref,(char *)&uu.rr,sizeof(ref));

    pte->obj_ref = ref;

    pte->apath = HDstrdup(path);

    pte->next = ref_path_table;
    ref_path_table = pte;

    npte++;

    return pte;
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
char*
lookup_ref_path(hobj_ref_t * ref)
{
    int                     i;
    struct ref_path_table_entry_t *pte = NULL;

    if (ref_path_table == NULL)
	return NULL;

    pte = ref_path_table;
    if (pte == NULL) {
	/* fatal -- not initialized? */
	return NULL;
    }
    for (i = 0; i < npte; i++) {
	if (memcmp(ref, pte->obj_ref, sizeof(hobj_ref_t)) == 0) {
	    return pte->apath;
	}
	pte = pte->next;
    }
    return NULL;
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
fill_ref_path_table(hid_t group, const char *name, void UNUSED * op_data)
{
    hid_t                   obj;
    char                   *tmp;
    H5G_stat_t              statbuf;
    struct ref_path_table_entry_t *pte;
    char                   *thepath;

    H5Gget_objinfo(group, name, FALSE, &statbuf);
    tmp = (char *) malloc(strlen(prefix) + strlen(name) + 2);

    if (tmp == NULL)
	return FAIL;

    thepath = (char *) malloc(strlen(prefix) + strlen(name) + 2);

    if (thepath == NULL) {
	free(tmp);
	return FAIL;
    }

    strcpy(tmp, prefix);

    strcpy(thepath, prefix);
    strcat(thepath, "/");
    strcat(thepath, name);

    switch (statbuf.type) {
    case H5G_DATASET:
	if ((obj = H5Dopen(group, name)) >= 0) {
	    pte = ref_path_table_lookup(thepath);
	    if (pte == NULL) {
		ref_path_table_put(obj, thepath);
	    }
	    H5Dclose(obj);
	} else {
     error_msg(progname, "unable to get dataset \"%s\"\n", name);
	    d_status = EXIT_FAILURE;
	}
	break;
    case H5G_GROUP:
	if ((obj = H5Gopen(group, name)) >= 0) {
	    strcat(strcat(prefix, "/"), name);
	    pte = ref_path_table_lookup(thepath);
	    if (pte == NULL) {
		ref_path_table_put(obj, thepath);
		H5Giterate(obj, ".", NULL, fill_ref_path_table, NULL);
		strcpy(prefix, tmp);
	    }
	    H5Gclose(obj);
	} else {
            error_msg(progname, "unable to dump group \"%s\"\n", name);
	    d_status = EXIT_FAILURE;
	}
	break;
    case H5G_TYPE:
	if ((obj = H5Topen(group, name)) >= 0) {
	    pte = ref_path_table_lookup(thepath);
	    if (pte == NULL) {
		ref_path_table_put(obj, thepath);
	    }
	    H5Tclose(obj);
	} else {
            error_msg(progname, "unable to get dataset \"%s\"\n", name);
	    d_status = EXIT_FAILURE;
	}
	break;
    default:
        break;
    }

    free(tmp);
    free(thepath);
    return 0;
}

