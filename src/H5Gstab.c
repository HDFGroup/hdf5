/*
 * Copyright (C) 1997 National Center for Supercomputing Applications
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Friday, September 19, 1997
 *
 */
#define H5G_PACKAGE

#include <H5private.h>
#include <H5ACprivate.h>
#include <H5Eprivate.h>
#include <H5Gpkg.h>
#include <H5Hprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK      H5G_stab_mask
static hbool_t          interface_initialize_g = FALSE;
#define INTERFACE_INIT  NULL

/*-------------------------------------------------------------------------
 * Function:    H5G_stab_create
 *
 * Purpose:     Creates a new empty symbol table (object header, name heap,
 *              and B-tree).  The caller can specify an initial size for the
 *              name heap.  The object header of the group is opened for
 *              write access.
 *
 *              In order for the B-tree to operate correctly, the first
 *              item in the heap is the empty string, and must appear at
 *              heap offset zero.
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_stab_create(H5F_t *f, size_t init, H5G_entry_t *self /*out */ )
{
    size_t                  name;       /*offset of "" name     */
    H5O_stab_t              stab;       /*symbol table message  */

    FUNC_ENTER(H5G_stab_create, FAIL);

    /*
     * Check arguments.
     */
    assert(f);
    assert(self);
    init = MAX(init, H5H_SIZEOF_FREE(f) + 2);

    /* Create symbol table private heap */
    if (H5H_create(f, H5H_LOCAL, init, &(stab.heap_addr) /*out */ ) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create heap");
    }
    if ((name = H5H_insert(f, &(stab.heap_addr), 1, "") < 0)) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't initialize heap");
    }
    /*
     * B-tree's won't work if the first name isn't at the beginning
     * of the heap.
     */
    assert(0 == name);

    /* Create the B-tree */
    if (H5B_create(f, H5B_SNODE, NULL, &(stab.btree_addr) /*out */ ) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create B-tree");
    }
    /*
     * Create symbol table object header.  It has a zero link count
     * since nothing refers to it yet.  The link count will be
     * incremented if the object is added to the group directed graph.
     */
    if (H5O_create(f, 4 + 2 * H5F_SIZEOF_ADDR(f), self /*out */ ) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create header");
    }
    /*
     * Insert the symbol table message into the object header and the symbol
     * table entry.
     */
    if (H5O_modify(self, H5O_STAB, H5O_NEW_MESG, H5O_FLAG_CONSTANT, &stab) < 0) {
        H5O_close(self);
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message");
    }
    self->cache.stab.btree_addr = stab.btree_addr;
    self->cache.stab.heap_addr = stab.heap_addr;
    self->type = H5G_CACHED_STAB;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_stab_find
 *
 * Purpose:     Finds a symbol named NAME in the symbol table whose
 *              description is stored in GRP_ENT in file F and returns its
 *              symbol table entry through OBJ_ENT (which is optional).
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_stab_find(H5G_entry_t *grp_ent, const char *name,
              H5G_entry_t *obj_ent /*out */ )
{
    H5G_bt_ud1_t            udata;      /*data to pass through B-tree   */
    H5O_stab_t              stab;       /*symbol table message          */

    FUNC_ENTER(H5G_stab_find, FAIL);

    /* Check arguments */
    assert(grp_ent);
    assert(grp_ent->file);
    assert(name && *name);

    /* set up the udata */
    if (NULL == H5O_read(grp_ent, H5O_STAB, 0, &stab)) {
        HRETURN_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't read message");
    }
    udata.operation = H5G_OPER_FIND;
    udata.name = name;
    udata.heap_addr = stab.heap_addr;

    /* search the B-tree */
    if (H5B_find(grp_ent->file, H5B_SNODE, &(stab.btree_addr), &udata) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "not found");
    }
    if (obj_ent)
        *obj_ent = udata.ent;
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_stab_insert
 *
 * Purpose:     Insert a new symbol into the table described by GRP_ENT in
 *              file F.  The name of the new symbol is NAME and its symbol
 *              table entry is OBJ_ENT.
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_stab_insert(H5G_entry_t *grp_ent, const char *name, H5G_entry_t *obj_ent)
{
    H5O_stab_t              stab;       /*symbol table message          */
    H5G_bt_ud1_t            udata;      /*data to pass through B-tree   */

    FUNC_ENTER(H5G_stab_insert, FAIL);

    /* check arguments */
    assert(grp_ent && grp_ent->file);
    assert(name && *name);
    assert(obj_ent && obj_ent->file);
    assert(grp_ent->file->shared == obj_ent->file->shared);

    /* initialize data to pass through B-tree */
    if (NULL == H5O_read(grp_ent, H5O_STAB, 0, &stab)) {
        HRETURN_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "not a symbol table");
    }
    udata.operation = H5G_OPER_INSERT;
    udata.name = name;
    udata.heap_addr = stab.heap_addr;
    udata.ent = *obj_ent;

    /* insert */
    if (H5B_insert(grp_ent->file, H5B_SNODE, &(stab.btree_addr), &udata) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "can't insert entry");
    }
    /* update the name offset in the entry */
    obj_ent->name_off = udata.ent.name_off;
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_stab_list
 *
 * Purpose:     Returns a list of all the symbols in a symbol table.
 *              The caller allocates an array of pointers which this
 *              function will fill in with malloc'd names.  The caller
 *              also allocates an array of symbol table entries which will
 *              be filled in with data from the symbol table.  Each of these
 *              arrays should have at least MAXENTRIES elements.
 *
 * Errors:
 *              SYM       BADMESG       Not a symbol table. 
 *              SYM       CANTLIST      B-tree list failure. 
 *
 * Return:      Success:        The total number of symbols in the
 *                              symbol table.  This may exceed MAXENTRIES,
 *                              but at most MAXENTRIES values are copied
 *                              into the NAMES and ENTRIES arrays.
 *
 *              Failure:        FAIL, the pointers in NAMES are undefined but
 *                              no memory is allocated.  The values in
 *                              ENTRIES are undefined.
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5G_stab_list(H5G_entry_t *grp_ent, intn maxentries, char *names[] /*out */ ,
              H5G_entry_t entries[] /*out */ )
{
    H5G_bt_ud2_t            udata;
    H5O_stab_t              stab;
    intn                    i;

    FUNC_ENTER(H5G_stab_list, FAIL);

    /* check args */
    assert(grp_ent && grp_ent->file);
    assert(maxentries >= 0);

    /* initialize data to pass through B-tree */
    if (NULL == H5O_read(grp_ent, H5O_STAB, 0, &stab)) {
        HRETURN_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "not a symbol table");
    }
    udata.entry = entries;
    udata.name = names;
    udata.heap_addr = stab.heap_addr;
    udata.maxentries = maxentries;
    udata.nsyms = 0;
    if (names)
        HDmemset(names, 0, maxentries);

    /* list */
    if (H5B_list(grp_ent->file, H5B_SNODE, &(stab.btree_addr), &udata) < 0) {
        if (names) {
            for (i = 0; i < maxentries; i++)
                H5MM_xfree(names[i]);
        }
        HRETURN_ERROR(H5E_SYM, H5E_CANTLIST, FAIL, "b-tree list failure");
    }
    FUNC_LEAVE(udata.nsyms);
}
