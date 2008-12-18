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

/*-------------------------------------------------------------------------
 *
 * Created:		H5Bdbg.c
 *			Dec 11 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Debugging routines for B-link tree package.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5B_PACKAGE		/*suppress error about including H5Bpkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Bpkg.h"		/* B-link trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/


/*-------------------------------------------------------------------------
 * Function:	H5B_debug
 *
 * Purpose:	Prints debugging info about a B-tree.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  4 1997
 *
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *-------------------------------------------------------------------------
 */
herr_t
H5B_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth,
	  const H5B_class_t *type, void *udata)
{
    H5B_t	*bt = NULL;
    H5B_shared_t        *shared;        /* Pointer to shared B-tree info */
    unsigned	u;                      /* Local index variable */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5B_debug, FAIL)

    /*
     * Check arguments.
     */
    assert(f);
    assert(H5F_addr_defined(addr));
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);
    assert(type);

    /*
     * Load the tree node.
     */
    if (NULL == (bt = (H5B_t *)H5AC_protect(f, dxpl_id, H5AC_BT, addr, type, udata, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree node")
    shared=(H5B_shared_t *)H5RC_GET_OBJ(bt->rc_shared);
    HDassert(shared);

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Tree type ID:",
	      ((shared->type->id)==H5B_SNODE_ID ? "H5B_SNODE_ID" :
            ((shared->type->id)==H5B_CHUNK_ID ? "H5B_CHUNK_ID" : "Unknown!")));
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of node:",
	      shared->sizeof_rnode);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of raw (disk) key:",
	      shared->sizeof_rkey);
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty flag:",
	      bt->cache_info.is_dirty ? "True" : "False");
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Level:",
	      bt->level);

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Address of left sibling:",
	      bt->left);

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Address of right sibling:",
	      bt->right);

    HDfprintf(stream, "%*s%-*s %u (%u)\n", indent, "", fwidth,
	      "Number of children (max):",
	      bt->nchildren, shared->two_k);

    /*
     * Print the child addresses
     */
    for (u = 0; u < bt->nchildren; u++) {
	HDfprintf(stream, "%*sChild %d...\n", indent, "", u);
	HDfprintf(stream, "%*s%-*s %a\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Address:", bt->child[u]);

        /* If there is a key debugging routine, use it to display the left & right keys */
	if (type->debug_key) {
            /* Decode the 'left' key & print it */
            HDfprintf(stream, "%*s%-*s\n", indent + 3, "", MAX(0, fwidth - 3),
                      "Left Key:");
            assert(H5B_NKEY(bt,shared,u));
	    (void)(type->debug_key)(stream, f, dxpl_id, indent+6, MAX (0, fwidth-6),
			      H5B_NKEY(bt,shared,u), udata);

            /* Decode the 'right' key & print it */
            HDfprintf(stream, "%*s%-*s\n", indent + 3, "", MAX(0, fwidth - 3),
                      "Right Key:");
            assert(H5B_NKEY(bt,shared,u+1));
	    (void)(type->debug_key)(stream, f, dxpl_id, indent+6, MAX (0, fwidth-6),
			      H5B_NKEY(bt,shared,u+1), udata);
	}
    }

done:
    if (bt && H5AC_unprotect(f, dxpl_id, H5AC_BT, addr, bt, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5B_assert
 *
 * Purpose:	Verifies that the tree is structured correctly.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	aborts if something is wrong.
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November  4, 1997
 *
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 *              John Mainzer, 6/8/05
 *              Modified the function to use the new dirtied parameter of
 *              of H5AC_unprotect() instead of modifying the is_dirty
 *              field of the cache info.
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5B_DEBUG
herr_t
H5B_assert(H5F_t *f, hid_t dxpl_id, haddr_t addr, const H5B_class_t *type, void *udata)
{
    H5B_t	*bt = NULL;
    H5B_shared_t        *shared;        /* Pointer to shared B-tree info */
    int	i, ncell, cmp;
    static int	ncalls = 0;
    herr_t	status;
    herr_t      ret_value=SUCCEED;       /* Return value */

    /* A queue of child data */
    struct child_t {
	haddr_t			addr;
	unsigned		level;
	struct child_t	       *next;
    } *head = NULL, *tail = NULL, *prev = NULL, *cur = NULL, *tmp = NULL;

    FUNC_ENTER_NOAPI(H5B_assert, FAIL)

    if (0==ncalls++) {
	if (H5DEBUG(B)) {
	    fprintf(H5DEBUG(B), "H5B: debugging B-trees (expensive)\n");
	}
    }
    /* Initialize the queue */
    bt = (H5B_t *)H5AC_protect(f, dxpl_id, H5AC_BT, addr, type, udata, H5AC_READ);
    assert(bt);
    shared=(H5B_shared_t *)H5RC_GET_OBJ(bt->rc_shared);
    HDassert(shared);
    cur = H5MM_calloc(sizeof(struct child_t));
    assert (cur);
    cur->addr = addr;
    cur->level = bt->level;
    head = tail = cur;

    status = H5AC_unprotect(f, dxpl_id, H5AC_BT, addr, bt, H5AC__NO_FLAGS_SET);
    assert(status >= 0);
    bt=NULL;    /* Make certain future references will be caught */

    /*
     * Do a breadth-first search of the tree.  New nodes are added to the end
     * of the queue as the `cur' pointer is advanced toward the end.  We don't
     * remove any nodes from the queue because we need them in the uniqueness
     * test.
     */
    for (ncell = 0; cur; ncell++) {
	bt = (H5B_t *)H5AC_protect(f, dxpl_id, H5AC_BT, cur->addr, type, udata, H5AC_READ);
	assert(bt);

	/* Check node header */
	assert(bt->level == cur->level);
	if (cur->next && cur->next->level == bt->level) {
	    assert(H5F_addr_eq(bt->right, cur->next->addr));
	} else {
	    assert(!H5F_addr_defined(bt->right));
	}
	if (prev && prev->level == bt->level) {
	    assert(H5F_addr_eq(bt->left, prev->addr));
	} else {
	    assert(!H5F_addr_defined(bt->left));
	}

	if (cur->level > 0) {
	    for (i = 0; i < bt->nchildren; i++) {

		/*
		 * Check that child nodes haven't already been seen.  If they
		 * have then the tree has a cycle.
		 */
		for (tmp = head; tmp; tmp = tmp->next) {
		    assert(H5F_addr_ne(tmp->addr, bt->child[i]));
		}

		/* Add the child node to the end of the queue */
		tmp = H5MM_calloc(sizeof(struct child_t));
		assert (tmp);
		tmp->addr = bt->child[i];
		tmp->level = bt->level - 1;
		tail->next = tmp;
		tail = tmp;

		/* Check that the keys are monotonically increasing */
		cmp = (type->cmp2) (f, dxpl_id, H5B_NKEY(bt,shared,i), udata,
				    H5B_NKEY(bt,shared,i+1));
		assert(cmp < 0);
	    }
	}
	/* Release node */
	status = H5AC_unprotect(f, dxpl_id, H5AC_BT, cur->addr, bt, H5AC__NO_FLAGS_SET);
	assert(status >= 0);
        bt=NULL;    /* Make certain future references will be caught */

	/* Advance current location in queue */
	prev = cur;
	cur = cur->next;
    }

    /* Free all entries from queue */
    while (head) {
	tmp = head->next;
	H5MM_xfree(head);
	head = tmp;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}
#endif /* H5B_DEBUG */

