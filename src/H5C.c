/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <stdarg.h>

/* Private header files */
#include <H5private.h>          /* Generic Functions                      */
#include <H5Aprivate.h>         /* Atoms                          */
#include <H5Bprivate.h>         /* B-tree subclass names          */
#include <H5Cprivate.h>         /* Template information           */
#include <H5Dprivate.h>         /* Datasets                               */
#include <H5Eprivate.h>         /* Error handling                 */
#include <H5MMprivate.h>        /* Memory management                      */

#define PABLO_MASK      H5C_mask

/* Is the interface initialized? */
static hbool_t          interface_initialize_g = FALSE;
#define INTERFACE_INIT H5C_init_interface
static herr_t           H5C_init_interface(void);

/* PRIVATE PROTOTYPES */
static void             H5C_term_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5C_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5C_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5C_init_interface(void)
{
    herr_t                  ret_value = SUCCEED;
    intn                    i;
    herr_t                  status;

    FUNC_ENTER(H5C_init_interface, FAIL);

    assert(H5C_NCLASSES <= H5_TEMPLATE_MAX - H5_TEMPLATE_0);

    /*
     * Initialize the mappings between template classes and atom groups. We
     * keep the two separate because template classes are publicly visible but
     * atom groups aren't.
     */
    for (i = 0; i < H5C_NCLASSES; i++) {
        status = H5A_init_group(H5_TEMPLATE_0 + i, H5A_TEMPID_HASHSIZE, 0, NULL);
        if (status < 0)
            ret_value = FAIL;
    }
    if (ret_value < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
                      "unable to initialize atom group");
    }
    /*
     * Register cleanup function.
     */
    if (H5_add_exit(H5C_term_interface) < 0) {
        HRETURN_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL,
                      "unable to install atexit function");
    }
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5C_term_interface
 PURPOSE
    Terminate various H5C objects
 USAGE
    void H5C_term_interface()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5C_term_interface(void)
{
    intn                    i;

    for (i = 0; i < H5C_NCLASSES; i++) {
        H5A_destroy_group(H5_TEMPLATE_0 + i);
    }
}

/*--------------------------------------------------------------------------
 NAME
    H5Ccreate
 PURPOSE
    Returns a copy of the default template for some class of templates.
 USAGE
    herr_t H5Ccreate (type)
        H5C_class_t type;       IN: Template class whose default is desired.
 RETURNS
    Template ID or FAIL
 
 ERRORS
    ARGS      BADVALUE      Unknown template class. 
    ATOM      CANTINIT      Can't register template. 
    INTERNAL  UNSUPPORTED   Not implemented yet. 

 DESCRIPTION
    Returns a copy of the default template for some class of templates.
--------------------------------------------------------------------------*/
hid_t
H5Ccreate(H5C_class_t type)
{
    hid_t                   ret_value = FAIL;
    void                   *tmpl = NULL;

    FUNC_ENTER(H5Ccreate, FAIL);

    /* Allocate a new template and initialize it with default values */
    switch (type) {
    case H5C_FILE_CREATE:
        tmpl = H5MM_xmalloc(sizeof(H5F_create_t));
        memcpy(tmpl, &H5F_create_dflt, sizeof(H5F_create_t));
        break;

    case H5C_FILE_ACCESS:
        HRETURN_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
                      "not implemented yet");

    case H5C_DATASET_CREATE:
        tmpl = H5MM_xmalloc(sizeof(H5D_create_t));
        memcpy(tmpl, &H5D_create_dflt, sizeof(H5D_create_t));
        break;

    case H5C_DATASET_XFER:
        tmpl = H5MM_xmalloc(sizeof(H5D_xfer_t));
        memcpy(tmpl, &H5D_xfer_dflt, sizeof(H5D_xfer_t));
        break;

    default:
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                      "unknown template class");
    }

    /* Atomize the new template */
    if ((ret_value = H5C_create(type, tmpl)) < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
                      "can't register template");
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5C_create
 *
 * Purpose:     Given a pointer to some template struct, atomize the template
 *              and return its ID. The template memory is not copied, so the
 *              caller should not free it; it will be freed by H5C_release().
 *
 * Return:      Success:        A new template ID.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5C_create(H5C_class_t type, void *tmpl)
{
    hid_t                   ret_value = FAIL;

    FUNC_ENTER(H5C_create, FAIL);

    /* check args */
    assert(type >= 0 && type < H5C_NCLASSES);
    assert(tmpl);

    /* Atomize the new template */
    if ((ret_value = H5A_register(H5_TEMPLATE_0 + type, tmpl)) < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
                      "can't register template");
    }
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5Cclose
 PURPOSE
    Release access to a template object.
 USAGE
    herr_t H5Cclose(oid)
        hid_t oid;       IN: Template object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function releases access to a template object
--------------------------------------------------------------------------*/
herr_t
H5Cclose(hid_t tid)
{
    void                   *tmpl = NULL;

    FUNC_ENTER(H5Cclose, FAIL);

    /* Chuck the object! :-) */
    if (NULL == (tmpl = H5A_remove(tid))) {
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "unable to remove atom");
    }
    H5MM_xfree(tmpl);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cget_class
 *
 * Purpose:     Returns the class identifier for a template.
 *
 * Return:      Success:        A template class
 *
 *              Failure:        H5C_NO_CLASS (-1)
 *
 * Programmer:  Robb Matzke
 *              Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5C_class_t
H5Cget_class(hid_t tid)
{
    group_t                 group;
    H5C_class_t             ret_value = H5C_NO_CLASS;

    FUNC_ENTER(H5Cget_class, H5C_NO_CLASS);

    if ((group = H5A_group(tid)) < 0 ||
#ifndef NDEBUG
        group >= H5_TEMPLATE_MAX ||
#endif
        group < H5_TEMPLATE_0) {
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, H5C_NO_CLASS, "not a template");
    }
    ret_value = group - H5_TEMPLATE_0;
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cget_version
 *
 * Purpose:     Retrieves version information for various parts of a file.
 *
 *              BOOT:           The file boot block.
 *              HEAP:           The global heap.
 *              FREELIST:       The global free list.
 *              STAB:           The root symbol table entry.
 *              SHHDR:          Shared object headers.
 *
 *              Any (or even all) of the output arguments can be null
 *              pointers.
 *
 * Return:      Success:        SUCCEED, version information is returned
 *                              through the arguments.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cget_version(hid_t tid, int *boot /*out */ , int *heap /*out */ ,
         int *freelist /*out */ , int *stab /*out */ , int *shhdr /*out */ )
{
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cget_version, FAIL);

    /* Check arguments */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    /* Get values */
    if (boot)
        *boot = tmpl->bootblock_ver;
    if (heap)
        *heap = tmpl->smallobject_ver;
    if (freelist)
        *freelist = tmpl->freespace_ver;
    if (stab)
        *stab = tmpl->objectdir_ver;
    if (shhdr)
        *shhdr = tmpl->sharedheader_ver;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cset_userblock
 *
 * Purpose:     Sets the userblock size field of a file creation template.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cset_userblock(hid_t tid, size_t size)
{
    intn                    i;
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cset_userblock, FAIL);

    /* Check arguments */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    for (i = 8; i < 8 * sizeof(int); i++) {
        uintn                   p2 = 8 == i ? 0 : 1 << i;
        if (size == p2)
            break;
    }
    if (i >= 8 * sizeof(int)) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                      "userblock size is not valid");
    }
    /* Set value */
    tmpl->userblock_size = size;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cget_userblock
 *
 * Purpose:     Queries the size of a user block in a file creation template.
 *
 * Return:      Success:        SUCCEED, size returned through SIZE argument.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cget_userblock(hid_t tid, size_t *size)
{
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cget_userblock, FAIL);

    /* Check args */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    /* Get value */
    if (size)
        *size = tmpl->userblock_size;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cset_sizes
 *
 * Purpose:     Sets file size-of addresses and sizes.  TEMPLATE
 *              should be a file creation template.  A value of zero causes
 *              the property to not change.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cset_sizes(hid_t tid, size_t sizeof_addr, size_t sizeof_size)
{
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cset_sizeof_addr, FAIL);

    /* Check arguments */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    if (sizeof_addr) {
        if (sizeof_addr != 2 && sizeof_addr != 4 &&
            sizeof_addr != 8 && sizeof_addr != 16) {
            HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                          "file haddr_t size is not valid");
        }
    }
    if (sizeof_size) {
        if (sizeof_size != 2 && sizeof_size != 4 &&
            sizeof_size != 8 && sizeof_size != 16) {
            HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                          "file size_t size is not valid");
        }
    }
    /* Set value */
    if (sizeof_addr)
        tmpl->sizeof_addr = sizeof_addr;
    if (sizeof_size)
        tmpl->sizeof_size = sizeof_size;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cget_sizes
 *
 * Purpose:     Returns the size of address and size quantities stored in a
 *              file according to a file creation template.  Either (or even
 *              both) SIZEOF_ADDR and SIZEOF_SIZE may be null pointers.
 *
 * Return:      Success:        SUCCEED, sizes returned through arguments.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cget_sizes(hid_t tid,
             size_t *sizeof_addr /*out */ , size_t *sizeof_size /*out */ )
{
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cget_sizes, FAIL);

    /* Check args */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    /* Get values */
    if (sizeof_addr)
        *sizeof_addr = tmpl->sizeof_addr;
    if (sizeof_size)
        *sizeof_size = tmpl->sizeof_size;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cset_sym_k
 *
 * Purpose:     IK is one half the rank of a tree that stores a symbol
 *              table for a group.  Internal nodes of the symbol table are on
 *              average 75% full.  That is, the average rank of the tree is
 *              1.5 times the value of IK.
 *
 *              LK is one half of the number of symbols that can be stored in
 *              a symbol table node.  A symbol table node is the leaf of a
 *              symbol table tree which is used to store a group.  When
 *              symbols are inserted randomly into a group, the group's
 *              symbol table nodes are 75% full on average.  That is, they
 *              contain 1.5 times the number of symbols specified by LK.
 *
 *              Either (or even both) of IK and LK can be zero in which case
 *              that value is left unchanged.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cset_sym_k(hid_t tid, int ik, int lk)
{
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cset_sym_k, FAIL);

    /* Check arguments */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    /* Set values */
    if (ik > 0) {
        tmpl->btree_k[H5B_SNODE_ID] = ik;
    }
    if (lk > 0) {
        tmpl->sym_leaf_k = lk;
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cget_sym_k
 *
 * Purpose:     Retrieves the symbol table B-tree 1/2 rank (IK) and the
 *              symbol table leaf node 1/2 size (LK).  See H5Cset_sym_k() for
 *              details. Either (or even both) IK and LK may be null
 *              pointers.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cget_sym_k(hid_t tid, int *ik /*out */ , int *lk /*out */ )
{
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cget_sym_k, FAIL);

    /* Check arguments */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    /* Get values */
    if (ik)
        *ik = tmpl->btree_k[H5B_SNODE_ID];
    if (lk)
        *lk = tmpl->sym_leaf_k;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cset_istore_k
 *
 * Purpose:     IK is one half the rank of a tree that stores chunked raw
 *              data.  On average, such a tree will be 75% full, or have an
 *              average rank of 1.5 times the value of IK.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cset_istore_k(hid_t tid, int ik)
{
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cset_istore_k, FAIL);

    /* Check arguments */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    if (ik <= 0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                      "istore IK value must be positive");
    }
    /* Set value */
    tmpl->btree_k[H5B_ISTORE_ID] = ik;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cget_istore_k
 *
 * Purpose:     Queries the 1/2 rank of an indexed storage B-tree.  See
 *              H5Cset_istore_k() for details.  The argument IK may be the
 *              null pointer.
 *
 * Return:      Success:        SUCCEED, size returned through IK
 *
 *              Failure:        
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cget_istore_k(hid_t tid, int *ik /*out */ )
{
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cget_istore_k, FAIL);

    /* Check arguments */
    if (H5C_FILE_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a file creation template");
    }
    /* Get value */
    if (ik)
        *ik = tmpl->btree_k[H5B_ISTORE_ID];

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cset_layout
 *
 * Purpose:     Sets the layout of raw data in the file.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cset_layout(hid_t tid, H5D_layout_t layout)
{
    H5D_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cset_layout, FAIL);

    /* Check arguments */
    if (H5C_DATASET_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a dataset creation template");
    }
    if (layout < 0 || layout >= H5D_NLAYOUTS) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
                      "raw data layout method is not valid");
    }
    /* Set value */
    tmpl->layout = layout;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cget_layout
 *
 * Purpose:     Retrieves layout type of a dataset creation template.
 *
 * Return:      Success:        The layout type
 *
 *              Failure:        H5D_LAYOUT_ERROR (-1, same as FAIL)
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5D_layout_t
H5Cget_layout(hid_t tid)
{
    H5D_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cget_layout, H5D_LAYOUT_ERROR);

    /* Check arguments */
    if (H5C_DATASET_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5D_LAYOUT_ERROR,
                      "not a dataset creation template");
    }
    FUNC_LEAVE(tmpl->layout);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cset_chunk
 *
 * Purpose:     Sets the number of dimensions and the size of each chunk to
 *              the values specified.  The dimensionality of the chunk should
 *              match the dimensionality of the data space.
 *
 *              As a side effect, the layout method is changed to
 *              H5D_CHUNKED.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cset_chunk(hid_t tid, int ndims, size_t dim[])
{
    int                     i;
    H5D_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cset_chunk, FAIL);

    /* Check arguments */
    if (H5C_DATASET_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a dataset creation template");
    }
    if (ndims <= 0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
                      "chunk dimensionality must be positive");
    }
    if (ndims > NELMTS(tmpl->chunk_size)) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
                      "chunk dimensionality is too large");
    }
    if (!dim) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                      "no chunk dimensions specified");
    }
    for (i = 0; i < ndims; i++) {
        if (dim[i] <= 0) {
            HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
                          "all chunk dimensions must be positive");
        }
    }

    /* Set value */
    tmpl->layout = H5D_CHUNKED;
    tmpl->chunk_ndims = ndims;
    for (i = 0; i < ndims; i++)
        tmpl->chunk_size[i] = dim[i];

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Cget_chunk
 *
 * Purpose:     Retrieves the chunk size of chunked layout.  The chunk
 *              dimensionality is returned and the chunk size in each
 *              dimension is returned through the DIM argument.  At most
 *              MAX_NDIMS elements of DIM will be initialized.
 *
 * Return:      Success:        Positive Chunk dimensionality.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Cget_chunk(hid_t tid, int max_ndims, size_t dim[] /*out */ )
{
    int                     i;
    H5D_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Cget_chunk, FAIL);

    /* Check arguments */
    if (H5C_DATASET_CREATE != H5Cget_class(tid) ||
        NULL == (tmpl = H5A_object(tid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                      "not a dataset creation template");
    }
    if (H5D_CHUNKED != tmpl->layout) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                      "not a chunked storage layout");
    }
    for (i = 0; i < tmpl->chunk_ndims && i < max_ndims && dim; i++) {
        dim[i] = tmpl->chunk_size[i];
    }

    FUNC_LEAVE(tmpl->chunk_ndims);
}

/*--------------------------------------------------------------------------
 NAME
    H5Ccopy
 PURPOSE
    Copy a template
 USAGE
    hid_t H5C_copy(tid)
        hid_t tid;        IN: Template object to copy
 RETURNS
    Returns template ID (atom) on success, FAIL on failure

 ERRORS
    ARGS      BADRANGE      Unknown template class. 
    ATOM      BADATOM       Can't unatomize template. 
    ATOM      CANTREGISTER  Register the atom for the new template. 
    INTERNAL  UNSUPPORTED   Dataset transfer properties are not implemented
                            yet. 
    INTERNAL  UNSUPPORTED   File access properties are not implemented yet. 

 DESCRIPTION
    This function creates a new copy of a template with all the same parameter
    settings.
--------------------------------------------------------------------------*/
hid_t
H5Ccopy(hid_t tid)
{
    const void             *tmpl = NULL;
    void                   *new_tmpl = NULL;
    H5C_class_t             type;
    size_t                  size;
    hid_t                   ret_value = FAIL;
    group_t                 group;

    FUNC_ENTER(H5Ccopy, FAIL);
    H5ECLEAR;

    /* check args */
    if (NULL == (tmpl = H5A_object(tid)) ||
        (type = H5Cget_class(tid)) < 0 ||
        (group = H5A_group(tid)) < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL,
                      "can't unatomize template");
    }
    /* How big is the template */
    switch (type) {
    case H5C_FILE_CREATE:
        size = sizeof(H5F_create_t);
        break;

    case H5C_FILE_ACCESS:
        HRETURN_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
                      "file access properties are not implemented yet");

    case H5C_DATASET_CREATE:
        size = sizeof(H5D_create_t);
        break;

    case H5C_DATASET_XFER:
        size = sizeof(H5D_xfer_t);
        break;

    default:
        HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
                      "unknown template class");
    }

    /* Create the new template */
    new_tmpl = H5MM_xmalloc(size);
    HDmemcpy(new_tmpl, tmpl, size);

    /* Register the atom for the new template */
    if ((ret_value = H5A_register(group, new_tmpl)) < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
                      "unable to atomize template pointer");
    }
    FUNC_LEAVE(ret_value);
}
