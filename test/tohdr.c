/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             tohdr.c
 *                      Aug  6 1997
 *                      Robb Matzke <robb@maya.nuance.com>
 *
 * Purpose:             
 *
 * Modifications:       
 *
 *-------------------------------------------------------------------------
 */
#include <testhdf5.h>

#include <H5private.h>
#include <H5ACprivate.h>
#include <H5Fprivate.h>
#include <H5Gprivate.h>
#include <H5Oprivate.h>

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include <H5Gpkg.h>

/*-------------------------------------------------------------------------
 * Function:    test_ohdr
 *
 * Purpose:     Test object headers.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              robb@maya.nuance.com
 *              Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
test_ohdr(void)
{
    hid_t                   fid;
    H5F_t                  *f;
    H5G_entry_t             oh_ent;
    H5O_stab_t              stab, ro;
    herr_t                  status;
    void                   *ptr;
    int                     i;

    MESSAGE(5, ("Testing Object Headers\n"));

    /* create the file */
    fid = H5Fcreate("tohdr.h5", H5ACC_OVERWRITE, 0, 0);
    CHECK(fid, FAIL, "H5Fcreate");
    f = H5Aatom_object(fid);
    CHECK(f, NULL, "H5Aatom_object");

    /* the new object header */
    MESSAGE(8, ("Creating new object header...\n"));
    status = H5O_create(f, 64, &oh_ent /*out */ );
    CHECK_I(status, "H5O_new");

    /*
     * Test creation of a new message.
     */
    MESSAGE(8, ("Creating new message...\n"));
    stab.btree_addr.offset = 11111111;
    stab.heap_addr.offset = 22222222;
    status = H5O_modify(&oh_ent, H5O_STAB, H5O_NEW_MESG, 0, &stab);
    VERIFY(status, 0, "H5O_modify");

    H5AC_flush(f, NULL, 0, TRUE);
    ptr = H5O_read(&oh_ent, H5O_STAB, 0, &ro);
    CHECK_PTR(ptr, "H5O_read");
    VERIFY(ptr, &ro, "H5O_read");
    VERIFY(ro.btree_addr.offset, stab.btree_addr.offset, "H5O_read");
    VERIFY(ro.heap_addr.offset, stab.heap_addr.offset, "H5O_read");

    /*
     * Test modification of an existing message.
     */
    MESSAGE(8, ("Modifying message...\n"));
    stab.btree_addr.offset = 33333333;
    stab.heap_addr.offset = 44444444;
    status = H5O_modify(&oh_ent, H5O_STAB, 0, 0, &stab);
    VERIFY(status, 0, "H5O_modify");

    H5AC_flush(f, NULL, 0, TRUE);
    ptr = H5O_read(&oh_ent, H5O_STAB, 0, &ro);
    CHECK_PTR(ptr, "H5O_read");
    VERIFY(ptr, &ro, "H5O_read");
    VERIFY(ro.btree_addr.offset, stab.btree_addr.offset, "H5O_read");
    VERIFY(ro.heap_addr.offset, stab.heap_addr.offset, "H5O_read");

    /*
     * Test creation of a second message of the same type with a symbol
     * table.
     */
    MESSAGE(8, ("Creating a duplicate message...\n"));
    stab.btree_addr.offset = 55555555;
    stab.heap_addr.offset = 66666666;
    status = H5O_modify(&oh_ent, H5O_STAB, H5O_NEW_MESG, 0, &stab);
    VERIFY(status, 1, "H5O_modify");

    H5AC_flush(f, NULL, 0, TRUE);
    ptr = H5O_read(&oh_ent, H5O_STAB, 1, &ro);
    CHECK_PTR(ptr, "H5O_read");
    VERIFY(ptr, &ro, "H5O_read");
    VERIFY(ro.btree_addr.offset, stab.btree_addr.offset, "H5O_read");
    VERIFY(ro.heap_addr.offset, stab.heap_addr.offset, "H5O_read");

    /*
     * Test modification of the second message with a symbol table.
     */
    MESSAGE(8, ("Modifying the duplicate message...\n"));
    stab.btree_addr.offset = 77777777;
    stab.heap_addr.offset = 88888888;
    status = H5O_modify(&oh_ent, H5O_STAB, 1, 0, &stab);
    VERIFY(status, 1, "H5O_modify");

    H5AC_flush(f, NULL, 0, TRUE);
    ptr = H5O_read(&oh_ent, H5O_STAB, 1, &ro);
    CHECK_PTR(ptr, "H5O_read");
    VERIFY(ptr, &ro, "H5O_read");
    VERIFY(ro.btree_addr.offset, stab.btree_addr.offset, "H5O_read");
    VERIFY(ro.heap_addr.offset, stab.heap_addr.offset, "H5O_read");

    /*
     * Test creation of a bunch of messages one after another to see
     * what happens when the object header overflows in core.
     */
    MESSAGE(8, ("Overflowing header in core...\n"));
    for (i = 0; i < 40; i++) {
        stab.btree_addr.offset = (i + 1) * 1000 + 1;
        stab.heap_addr.offset = (i + 1) * 1000 + 2;
        status = H5O_modify(&oh_ent, H5O_STAB, H5O_NEW_MESG, 0, &stab);
        VERIFY(status, 2 + i, "H5O_modify");
    }
    H5AC_flush(f, NULL, 0, TRUE);

    /*
     * Test creation of a bunch of messages one after another to see
     * what happens when the object header overflows on disk.
     */
    MESSAGE(8, ("Overflowing header on disk...\n"));
    for (i = 0; i < 10; i++) {
        stab.btree_addr.offset = (i + 1) * 1000 + 10;
        stab.heap_addr.offset = (i + 1) * 1000 + 20;
        status = H5O_modify(&oh_ent, H5O_STAB, H5O_NEW_MESG, 0, &stab);
        VERIFY(status, 42 + i, "H5O_modify");
        H5AC_flush(f, NULL, 0, TRUE);
    }

    /*
     * Delete all symbol table messages.
     */
    status = H5O_remove(&oh_ent, H5O_STAB, H5O_ALL);
    CHECK_I(status, "H5O_remove");

    /* release resources */
    H5O_close(&oh_ent);
    H5Fclose(fid);
}
