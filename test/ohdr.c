/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, November 24, 1998
 */
#include <h5test.h>
#include <H5Iprivate.h>
#include <H5Oprivate.h>

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include <H5Gpkg.h>

const char *FILENAME[] = {
    "ohdr",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl=-1, file=-1;
    H5F_t	*f=NULL;
    char	filename[1024];
    H5G_entry_t	oh_ent;
    H5O_stab_t	stab, ro;
    int		i;

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if (NULL==(f=H5I_object(file))) {
	H5Eprint(stdout);
	goto error;
    }

    /*
     * Test object header creation
     */
    TESTING("object header creation");
    if (H5O_create(f, 64, &oh_ent/*out*/)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    PASSED();

    /* create a new message */
    TESTING("message creation");
    stab.btree_addr = 11111111;
    stab.heap_addr = 22222222;
    if (H5O_modify(&oh_ent, H5O_STAB, H5O_NEW_MESG, 0, &stab)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5AC_flush(f, NULL, HADDR_UNDEF, TRUE)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (NULL==H5O_read(&oh_ent, H5O_STAB, 0, &ro)) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5F_addr_ne(ro.btree_addr, stab.btree_addr) ||
	H5F_addr_ne(ro.heap_addr, stab.heap_addr)) {
	FAILED();
	HDfprintf(stdout, "    got: {%a, %a}\n",
		  ro.btree_addr, ro.heap_addr);
	HDfprintf(stdout, "    ans: {%a, %a}\n",
		  stab.btree_addr, stab.heap_addr);
	goto error;
    }
    PASSED();

    /*
     * Test modification of an existing message.
     */
    TESTING("message modification");
    stab.btree_addr = 33333333;
    stab.heap_addr = 44444444;
    if (H5O_modify(&oh_ent, H5O_STAB, 0, 0, &stab)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5AC_flush(f, NULL, HADDR_UNDEF, TRUE)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (NULL==H5O_read(&oh_ent, H5O_STAB, 0, &ro)) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5F_addr_ne(ro.btree_addr, stab.btree_addr) ||
	H5F_addr_ne(ro.heap_addr, stab.heap_addr)) {
	FAILED();
	HDfprintf(stdout, "    got: {%a, %a}\n",
		  ro.btree_addr, ro.heap_addr);
	HDfprintf(stdout, "    ans: {%a, %a}\n",
		  stab.btree_addr, stab.heap_addr);
	goto error;
    }
    PASSED();


    /*
     * Test creation of a second message of the same type.
     */
    TESTING("duplicate message creation");
    stab.btree_addr = 55555555;
    stab.heap_addr = 66666666;
    if (H5O_modify(&oh_ent, H5O_STAB, H5O_NEW_MESG, 0, &stab)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5AC_flush(f, NULL, HADDR_UNDEF, TRUE)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (NULL==H5O_read(&oh_ent, H5O_STAB, 1, &ro)) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5F_addr_ne(ro.btree_addr, stab.btree_addr) ||
	H5F_addr_ne(ro.heap_addr, stab.heap_addr)) {
	FAILED();
	HDfprintf(stdout, "    got: {%a, %a}\n",
		  ro.btree_addr, ro.heap_addr);
	HDfprintf(stdout, "    ans: {%a, %a}\n",
		  stab.btree_addr, stab.heap_addr);
	goto error;
    }
    PASSED();
	
    /*
     * Test modification of the second message with a symbol table.
     */
    TESTING("duplicate message modification");
    stab.btree_addr = 77777777;
    stab.heap_addr = 88888888;
    if (H5O_modify(&oh_ent, H5O_STAB, 1, 0, &stab)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5AC_flush(f, NULL, HADDR_UNDEF, TRUE)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (NULL==H5O_read(&oh_ent, H5O_STAB, 1, &ro)) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5F_addr_ne(ro.btree_addr, stab.btree_addr) ||
	H5F_addr_ne(ro.heap_addr, stab.heap_addr)) {
	FAILED();
	HDfprintf(stdout, "    got: {%a, %a}\n",
		  ro.btree_addr, ro.heap_addr);
	HDfprintf(stdout, "    ans: {%a, %a}\n",
		  stab.btree_addr, stab.heap_addr);
	goto error;
    }
    PASSED();

    /*
     * Test creation of a bunch of messages one after another to see
     * what happens when the object header overflows in core.
     */
    TESTING("object header overflow in memory");
    for (i=0; i<40; i++) {
        stab.btree_addr = (i+1)*1000+1;
        stab.heap_addr = (i+1)*1000+2;
        if (H5O_modify(&oh_ent, H5O_STAB, H5O_NEW_MESG, 0, &stab)<0) {
	    FAILED();
	    H5Eprint(stdout);
	    goto error;
	}
    }
    if (H5AC_flush(f, NULL, HADDR_UNDEF, TRUE)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    PASSED();

    /*
     * Test creation of a bunch of messages one after another to see
     * what happens when the object header overflows on disk.
     */
    TESTING("object header overflow on disk");
    for (i=0; i<10; i++) {
        stab.btree_addr = (i + 1) * 1000 + 10;
        stab.heap_addr = (i + 1) * 1000 + 20;
        if (H5O_modify(&oh_ent, H5O_STAB, H5O_NEW_MESG, 0, &stab)<0) {
	    FAILED();
	    H5Eprint(stdout);
	    goto error;
	}
        if (H5AC_flush(f, NULL, HADDR_UNDEF, TRUE)<0) {
	    FAILED();
	    H5Eprint(stdout);
	    goto error;
	}
    }
    PASSED();

    /*
     * Delete all symbol table messages.
     */
    TESTING("message deletion");
    if (H5O_remove(&oh_ent, H5O_STAB, H5O_ALL)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5O_read(&oh_ent, H5O_STAB, 0, &ro)) {
	FAILED();
	puts("    H5O_read() should have failed but didn't");
	H5Eclear();
	goto error;
    }
    PASSED();
    

    /* release resources */
    TESTING("object header closing");
    if (H5O_close(&oh_ent)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5Fclose(file)<0) goto error;
    PASSED();

    puts("All object header tests passed.");
    h5_cleanup(fapl);
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}
