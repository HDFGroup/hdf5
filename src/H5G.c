/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             H5G.c
 *                      Jul 18 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Symbol table functions.  The functions that
 *                      begin with `H5G_stab_' don't understand the
 *                      naming system; they operate on a single
 *                      symbol table at a time.
 *
 *                      The functions that begin with `H5G_node_' operate
 *                      on the leaf nodes of a symbol table B-tree.  They
 *                      should be defined in the H5Gnode.c file.
 *
 *                      The remaining functions know how to traverse the
 *                      group directed graph
 *
 * Modifications:
 *
 *      Robb Matzke, 5 Aug 1997
 *      Added calls to H5E.
 *
 *      Robb Matzke, 30 Aug 1997
 *      Added `Errors:' field to function prologues.
 *
 *-------------------------------------------------------------------------
 */
#define H5G_PACKAGE             /*suppress error message about including H5Gpkg.h */

/* Packages needed by this file... */
#include <H5private.h>
#include <H5Aprivate.h>
#include <H5Bprivate.h>
#include <H5Eprivate.h>
#include <H5Gpkg.h>
#include <H5Hprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define H5G_INIT_HEAP           8192
#define H5G_RESERVED_ATOMS      0
#define PABLO_MASK              H5G_mask

/* Interface initialization */
static hbool_t          interface_initialize_g = FALSE;
#define INTERFACE_INIT          H5G_init_interface
static herr_t           H5G_init_interface(void);
static void             H5G_term_interface(void);

static H5G_entry_t     *H5G_getcwg(H5F_t *f);

/*-------------------------------------------------------------------------
 * Function:    H5Gcreate
 *
 * Purpose:     Creates a new group in FILE and gives it the specified
 *              NAME. Unless NAME begins with `/' it is relative to the
 *              current working group.  The group is opened for write access
 *              and it's object ID is returned.
 *
 *              The optional SIZE_HINT specifies how much file space to
 *              reserve to store the names that will appear in this
 *              group. If a non-positive value is supplied for the SIZE_HINT
 *              then a default size is chosen.
 *
 * See also:    H5Gset(), H5Gpush(), H5Gpop()
 *
 * Errors:
 *
 * Return:      Success:        The object ID of a new, empty group open for
 *                              writing.  Call H5Gclose() when finished with
 *                              the group.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gcreate(hid_t file_id, const char *name, size_t size_hint)
{
    H5F_t                  *f = NULL;
    H5G_t                  *grp;
    hid_t                   ret_value = FAIL;

    FUNC_ENTER(H5Gcreate, FAIL);
    H5ECLEAR;

    /* Check arguments */
    if (H5_FILE != H5A_group(file_id) ||
        NULL == (f = H5A_object(file_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    if (!name || !*name) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
    }
    /* Create the group */
    if (NULL == (grp = H5G_create(f, name, size_hint))) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group");
    }
    if ((ret_value = H5A_register(H5_GROUP, grp)) < 0) {
        H5G_close(grp);
        HRETURN_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
                      "unable to register group");
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5Gopen
 *
 * Purpose:     Opens an existing group for modification.  When finished,
 *              call H5Gclose() to close it and release resources.
 *
 * Errors:
 *
 * Return:      Success:        Object ID of the group.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, December 31, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gopen(hid_t file_id, const char *name)
{
    hid_t                   ret_value = FAIL;
    H5F_t                  *f = NULL;
    H5G_t                  *grp = NULL;

    FUNC_ENTER(H5Gopen, FAIL);
    H5ECLEAR;

    /* Check args */
    if (H5_FILE != H5A_group(file_id) ||
        NULL == (f = H5A_object(file_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    if (!name || !*name) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    /* Open the group */
    if (NULL == (grp = H5G_open(f, name))) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group");
    }
    /* Register an atom for the group */
    if ((ret_value = H5A_register(H5_GROUP, grp)) < 0) {
        H5G_close(grp);
        HRETURN_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
                      "unable to register group");
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5Gclose
 *
 * Purpose:     Closes the specified group.  The group ID will no longer be
 *              valid for accessing the group.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, December 31, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gclose(hid_t grp_id)
{
    H5G_t                  *grp = NULL;

    FUNC_ENTER(H5Gclose, FAIL);
    H5ECLEAR;

    /* Check args */
    if (H5_GROUP != H5A_group(grp_id) ||
        NULL == (grp = H5A_object(grp_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group");
    }
    /*
     * Decrement the counter on the group atom.  It will be freed if the count
     * reaches zero.
     */
    if (H5A_dec_ref(grp_id) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close group");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Gset
 *
 * Purpose:     Sets the working group for file handle FILE to the
 *              specified group.
 *
 *              Each file handle maintains its own notion of the current
 *              working group.  That is, if a single file is opened with
 *              multiple calls to H5Fopen(), which returns multiple file
 *              handles, then each handle's current working group can be
 *              set independently of the other file handles for that file.
 *
 * See also:    H5Gpush(), H5Gpop()
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gset(hid_t file_id, const char *name)
{
    H5F_t                  *f = NULL;
    H5G_t                  *grp;

    FUNC_ENTER(H5Gset, FAIL);
    H5ECLEAR;

    /* Check/fix arguments */
    if (H5_FILE != H5A_group(file_id) ||
        NULL == (f = H5A_object(file_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    if (!name || !*name) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    if (NULL == (grp = H5G_open(f, name))) {
        HRETURN_ERROR(H5E_ARGS, H5E_NOTFOUND, FAIL, "no such group");
    }
    /* Set the current working group */
    if (H5G_set(f, grp) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL,
                      "unable to change current working group");
    }
    /* Close the handle */
    if (H5G_close(grp) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close group");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Gpush
 *
 * Purpose:     Similar to H5Gset() except the new working group is pushed
 *              on a stack.
 *
 *              Each file handle maintains its own notion of the current
 *              working group.  That is, if a single file is opened with
 *              multiple calls to H5Fopen(), which returns multiple file
 *              handles, then each handle's current working group can be
 *              set independently of the other file handles for that file.
 *
 * See also:    H5Gset(), H5Gpop()
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gpush(hid_t file_id, const char *name)
{
    H5F_t                  *f = NULL;
    H5G_t                  *grp;

    FUNC_ENTER(H5Gpush, FAIL);
    H5ECLEAR;

    /* Check arguments */
    if (H5_FILE != H5A_group(file_id) ||
        NULL == (f = H5A_object(file_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    if (!name || !*name) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    if (NULL == (grp = H5G_open(f, name))) {
        HRETURN_ERROR(H5E_ARGS, H5E_NOTFOUND, FAIL, "no such group");
    }
    /* Push group onto stack */
    if (H5G_push(f, grp) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL,
                      "can't change current working group");
    }
    /* Close the handle */
    if (H5G_close(grp) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close group");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Gpop
 *
 * Purpose:     Removes the top (latest) entry from the working group stack
 *              and sets the current working group to the previous value.
 *
 *              Each file handle maintains its own notion of the current
 *              working group.  That is, if a single file is opened with
 *              multiple calls to H5Fopen(), which returns multiple file
 *              handles, then each handle's current working group can be
 *              set independently of the other file handles for that file.
 *
 * See also:    H5Gset(), H5Gpush()
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL.  The final entry cannot be popped from
 *                              the group stack (but it can be changed
 *                              with H5Gset()).
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gpop(hid_t file_id)
{
    H5F_t                  *f = NULL;

    FUNC_ENTER(H5Gpop, FAIL);
    H5ECLEAR;

    /* Check arguments */
    if (H5_FILE != H5A_group(file_id) ||
        NULL == (f = H5A_object(file_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    /* pop */
    if (H5G_pop(f) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL,
                      "stack is empty");
    }
    FUNC_LEAVE(SUCCEED);
}

/*
 *-------------------------------------------------------------------------
 *-------------------------------------------------------------------------
 *   N O   A P I   F U N C T I O N S   B E Y O N D   T H I S   P O I N T
 *-------------------------------------------------------------------------
 *------------------------------------------------------------------------- 
 */

/*-------------------------------------------------------------------------
 * Function:    H5G_init_interface
 *
 * Purpose:     Initializes the H5G interface.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Monday, January  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_init_interface(void)
{
    FUNC_ENTER(H5G_init_interface, FAIL);

    /* Initialize the atom group for the group IDs */
    if (H5A_init_group(H5_GROUP, H5A_GROUPID_HASHSIZE, H5G_RESERVED_ATOMS,
		       (herr_t (*)(void *)) H5G_close) < 0 ||
        H5_add_exit(H5G_term_interface) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL,
                      "unable to initialize interface");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_term_interface
 *
 * Purpose:     Terminates the H5G interface
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              Monday, January  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5G_term_interface(void)
{
    H5A_destroy_group(H5_GROUP);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_component
 *
 * Purpose:     Returns the pointer to the first component of the
 *              specified name by skipping leading slashes.  Returns
 *              the size in characters of the component through SIZE_P not
 *              counting leading slashes or the null terminator.
 *
 * Errors:
 *
 * Return:      Success:        Ptr into NAME.
 *
 *              Failure:        Ptr to the null terminator of NAME.
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static const char      *
H5G_component(const char *name, size_t *size_p)
{
    assert(name);

    while ('/' == *name)
        name++;
    if (size_p)
        *size_p = HDstrcspn(name, "/");
    return name;
}

/*-------------------------------------------------------------------------
 * Function:    H5G_namei
 *
 * Purpose:     Translates a name to a symbol table entry.
 *
 *              If the specified name can be fully resolved, then this
 *              function returns the symbol table entry for the named object
 *              through the OBJ_ENT argument. The symbol table entry for the
 *              group containing the named object is returned through the
 *              GRP_ENT argument if it is non-null.  However, if the name
 *              refers to the root object then the GRP_ENT will be
 *              initialized with an undefined object header address.  The
 *              REST argument, if present, will point to the null terminator
 *              of NAME.
 *
 *              If the specified name cannot be fully resolved, then OBJ_ENT
 *              is initialized with the undefined object header address. The
 *              REST argument will point into the NAME argument to the start
 *              of the component that could not be located.  The GRP_ENT will
 *              contain the entry for the symbol table that was being
 *              searched at the time of the failure and will have an
 *              undefined object header address if the search failed at the
 *              root object. For instance, if NAME is `/foo/bar/baz' and the
 *              root directory exists and contains an entry for `foo', and
 *              foo is a group that contains an entry for baz, but baz is not
 *              a group, then the results will be that REST points to `baz',
 *              GRP_ENT has an undefined object header address, and GRP_ENT
 *              is the symbol table entry for `bar' in `/foo'.
 *
 *              If a file contains more than one object, then `/' is the name
 *              of the root object which is a group.  Otherwise, a file can
 *              consist of a single object, not necessarily a group, whose
 *              name is `/foo' where `foo' is the value of the name messsage
 *              in the object header.  A file can also contain no objects in
 *              which case the function returns so REST points to the
 *              beginning of NAME and OBJ_ENT and GRP_ENT contain undefined
 *              header addresses.
 *
 *              Components of a name are separated from one another by one or
 *              more slashes (/).  Slashes at the end of a name are ignored.
 *              If the name begins with a slash then the search begins at the
 *              root object, otherwise it begins at the group CWG, otherwise
 *              it begins at the current working group of file F. The
 *              component `.' is a no-op, but `..' is not understood by this
 *              function (unless it appears as an entry in the symbol table).
 *              
 * Errors:
 *
 * Return:      Success:        SUCCEED if name can be fully resolved.  See
 *                              above for values of REST, GRP_ENT, and
 *                              OBJ_ENT.
 *
 *              Failure:        FAIL if the name could not be fully resolved.
 *                              See above for values of REST, GRP_ENT, and
 *                              OBJ_ENT.
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_namei(H5F_t *f, H5G_entry_t *cwg, const char *name,
          const char **rest /*out */ , H5G_entry_t *grp_ent /*out */ ,
          H5G_entry_t *obj_ent /*out */ )
{
    H5G_entry_t             _grp_ent;   /*entry for current group       */
    H5G_entry_t             _obj_ent;   /*entry found                   */
    size_t                  nchars;     /*component name length         */
    char                    comp[1024];         /*component name buffer         */
    hbool_t                 aside = FALSE;      /*did we look at a name message? */

    /* clear output args before FUNC_ENTER() in case it fails */
    if (rest)
        *rest = name;
    if (!grp_ent)
        grp_ent = &_grp_ent;
    if (!obj_ent)
        obj_ent = &_obj_ent;
    memset(grp_ent, 0, sizeof(H5G_entry_t));
    H5F_addr_undef(&(grp_ent->header));
    memset(obj_ent, 0, sizeof(H5G_entry_t));
    H5F_addr_undef(&(obj_ent->header));

    FUNC_ENTER(H5G_namei, FAIL);

    /* check args */
    assert(f);
    assert(name && *name);

    /* If the file contains no objects then return failure */
    if (!f->shared->root_ent) {
        HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "no root group");
    }
    /* starting point */
    if ('/' == *name) {
        *obj_ent = *(f->shared->root_ent);
    } else if (cwg) {
        *obj_ent = *cwg;
    } else if ((cwg = H5G_getcwg(f))) {
        *obj_ent = *cwg;
    } else {
        HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "no current working group");
    }
    assert(H5F_addr_defined(&(obj_ent->header)));

    /* traverse the name */
    while ((name = H5G_component(name, &nchars)) && *name) {
        if (rest)
            *rest = name;

        /*
         * The special name `.' is a no-op.
         */
        if ('.' == name[0] && !name[1])
            continue;

        /*
         * Advance to the next component of the name.
         */
        *grp_ent = *obj_ent;
        HDmemset(obj_ent, 0, sizeof(H5G_entry_t));
        H5F_addr_undef(&(obj_ent->header));

        /*
         * Copy the component name into a null-terminated buffer so
         * we can pass it down to the other symbol table functions.
         */
        if (nchars + 1 > sizeof(comp)) {
            HRETURN_ERROR(H5E_SYM, H5E_COMPLEN, FAIL, "component is too long");
        }
        HDmemcpy(comp, name, nchars);
        comp[nchars] = '\0';

        if (H5G_stab_find(grp_ent, comp, obj_ent /*out */ ) < 0) {
            /*
             * Component was not found in the current symbol table, possibly
             * because GRP_ENT isn't a symbol table.  If it is the root symbol
             * then see if it has the appropriate name field.  The ASIDE
             * variable prevents us from saying `/foo/foo' where the root object
             * has the name `foo'.
             */
            H5O_name_t              mesg =
            {0};
            if (!aside &&
                H5F_addr_eq(&(grp_ent->header),
                            &(f->shared->root_ent->header)) &&
                H5O_read(grp_ent, H5O_NAME, 0, &mesg) &&
                !HDstrcmp(mesg.s, comp)) {
                H5O_reset(H5O_NAME, &mesg);
                *obj_ent = *grp_ent;
                HDmemset(grp_ent, 0, sizeof(H5G_entry_t));
                H5F_addr_undef(&(grp_ent->header));
                aside = TRUE;
            } else {
                H5O_reset(H5O_NAME, &mesg);
                HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found");
            }
        }
        /* next component */
        name += nchars;
    }
    if (rest)
        *rest = name;           /*final null */

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_mkroot
 *
 * Purpose:     Creates the root group if it doesn't exist; otherwise
 *              nothing happens.  If the root symbol table entry previously
 *              pointed to something other than a group, then that object
 *              is made a member of the root group and is given a name
 *              corresponding to the object's name message (the name message
 *              is removed).  If the root object doesn't have a name message
 *              then the name `Root Object' is used.
 *
 * Errors:
 * 
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL.  This function returns -2 if the
 *                              failure is because a root group already
 *                              exists.
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_mkroot(H5F_t *f, size_t size_hint)
{
    herr_t                  ret_value = FAIL;   /*return value                  */
    H5O_name_t              name =
    {NULL};                     /*object name                   */
    H5O_stab_t              stab;       /*symbol table message          */
    H5G_entry_t             new_root;   /*new root object               */
    const char             *obj_name = NULL;    /*name of old root object       */

    FUNC_ENTER(H5G_mkroot, FAIL);

    /* check args */
    assert(f);

    /*
     * If we already have a root object, then get it's name.
     */
    if (f->shared->root_ent) {
        if (H5O_read(f->shared->root_ent, H5O_STAB, 0, &stab)) {
            HGOTO_ERROR(H5E_SYM, H5E_EXISTS, -2, "root group already exists");
        } else if (NULL == H5O_read(f->shared->root_ent, H5O_NAME, 0, &name)) {
            obj_name = "Root Object";
        } else {
            obj_name = name.s;  /*don't reset message until the end! */
        }
    }
    /*
     * Create the new root group.  Set the link count to 1.
     */
    if (H5G_stab_create(f, size_hint, &new_root /*out */ ) < 0) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "cant create root");
    }
    if (1 != H5O_link(&new_root, 1)) {
        HGOTO_ERROR(H5E_SYM, H5E_LINK, FAIL,
                    "internal error (wrong link count)");
    }
    /*
     * If there was a previous root object then insert it into the new root
     * symbol table with the specified name.  Then make the root object the
     * new symbol table.
     */
    if (f->shared->root_ent) {
        assert(1 == H5O_link(f->shared->root_ent, 0));

        if (H5G_stab_insert(&new_root, obj_name, f->shared->root_ent) < 0) {
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL,
                        "can't reinsert old root object");
        }
        /*
         * Remove all `name' messages from the old root object.  The only time
         * a name message should ever appear is to give the root object a name,
         * but the old root object is no longer the root object.
         */
        H5O_remove(f->shared->root_ent, H5O_NAME, H5O_ALL);
        H5ECLEAR;               /*who really cares? */

        *(f->shared->root_ent) = new_root;

    } else {
        f->shared->root_ent = H5G_ent_calloc(&new_root);
    }

    H5O_close(&new_root);
    ret_value = SUCCEED;

  done:
    H5O_reset(H5O_NAME, &name);
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_create
 *
 * Purpose:     Creates a new empty group with the specified name. The name
 *              is either an absolute name or is relative to the current
 *              working group.
 *
 *              A root group is created implicitly by this function
 *              when necessary.  Calling this function with the name "/"
 *              (or any equivalent name) will result in an H5E_EXISTS
 *              failure.
 *
 * Errors:
 *
 * Return:      Success:        A handle for the group.  The group is opened
 *                              and should eventually be close by calling
 *                              H5G_close().
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_t                  *
H5G_create(H5F_t *f, const char *name, size_t size_hint)
{
    const char             *rest = NULL;        /*the base name                 */
    H5G_entry_t             grp_ent;    /*group containing new group    */
    char                    _comp[1024];        /*name component                */
    size_t                  nchars;     /*number of characters in compon */
    herr_t                  status;     /*function return status        */
    H5G_t                  *grp = NULL;         /*new group                     */

    FUNC_ENTER(H5G_create, NULL);

    /* check args */
    assert(f);
    assert(name && *name);

    /*
     * Try to create the root group.  Ignore the error if this function
     * fails because the root group already exists.
     */
    if ((status = H5G_mkroot(f, H5G_SIZE_HINT)) < 0 && -2 != status) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create root group");
    }
    H5ECLEAR;

    /* lookup name */
    if (0 == H5G_namei(f, NULL, name, &rest, &grp_ent, NULL)) {
        HRETURN_ERROR(H5E_SYM, H5E_EXISTS, NULL, "already exists");
    }
    H5ECLEAR;                   /*it's OK that we didn't find it */
    assert(H5F_addr_defined(&(grp_ent.header)));

    /* should be one null-terminated component left */
    rest = H5G_component(rest, &nchars);
    assert(rest && *rest);
    if (rest[nchars]) {
        if (H5G_component(rest + nchars, NULL)) {
            HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "missing component");
        } else if (nchars + 1 > sizeof _comp) {
            HRETURN_ERROR(H5E_SYM, H5E_COMPLEN, NULL, "component is too long");
        } else {
            /* null terminate */
            HDmemcpy(_comp, rest, nchars);
            _comp[nchars] = '\0';
            rest = _comp;
        }
    }
    /* create an open group */
    grp = H5MM_xcalloc(1, sizeof(H5G_t));
    if (H5G_stab_create(f, size_hint, &(grp->ent) /*out */ ) < 0) {
        grp = H5MM_xfree(grp);
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create grp");
    }
    /* insert child name into parent */
    if (H5G_stab_insert(&grp_ent, rest, &(grp->ent)) < 0) {
        H5O_close(&(grp->ent));
        grp = H5MM_xfree(grp);
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't insert");
    }
    grp->nref = 1;
    FUNC_LEAVE(grp);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_open
 *
 * Purpose:     Opens an existing group.  The group should eventually be
 *              closed by calling H5G_close().
 *
 * Return:      Success:        Ptr to a new group.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, January  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_t                  *
H5G_open(H5F_t *f, const char *name)
{
    H5G_t                  *grp = NULL;
    H5G_t                  *ret_value = NULL;

    FUNC_ENTER(H5G_open, NULL);

    /* Check args */
    assert(f);
    assert(name && *name);

    /* Open the group */
    grp = H5MM_xcalloc(1, sizeof(H5G_t));
    if (H5G_find(f, name, NULL, &(grp->ent)) < 0) {
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "group not found");
    }
    if (H5O_open(f, &(grp->ent)) < 0) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open group");
    }
    grp->nref = 1;
    ret_value = grp;

  done:
    if (!ret_value && grp) {
        H5MM_xfree(grp);
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_reopen
 *
 * Purpose:     Reopens a group by incrementing the open count.
 *
 * Return:      Success:        The GRP argument.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, January  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_t                  *
H5G_reopen(H5G_t *grp)
{
    FUNC_ENTER(H5G_reopen, NULL);

    assert(grp);
    assert(grp->nref > 0);

    grp->nref++;

    FUNC_LEAVE(grp);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_close
 *
 * Purpose:     Closes the specified group.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Monday, January  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_close(H5G_t *grp)
{
    FUNC_ENTER(H5G_close, FAIL);

    /* Check args */
    assert(grp);
    assert(grp->nref > 0);

    if (1 == grp->nref) {
        if (H5O_close(&(grp->ent)) < 0) {
            HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close");
        }
    }
    --grp->nref;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_set
 *
 * Purpose:     Sets the current working group to be the specified group.
 *              This affects only the top item on the group stack for the
 *              specified file as accessed through this file handle.  If the
 *              file is opened multiple times, then the current working group
 *              for this file handle is the only one that is changed.
 *
 * Note:        The group is re-opened and held open until it is removed from
 *              the current working group stack.
 *
 * Errors:
 *              SYM       CWG           Can't open group. 
 *              SYM       CWG           Couldn't close previous c.w.g. 
 *              SYM       CWG           Not a group. 
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_set(H5F_t *f, H5G_t *grp)
{
    FUNC_ENTER(H5G_set, FAIL);

    /* check args */
    assert(f);
    assert(grp);

    /*
     * If there is no stack then create one, otherwise close the current
     * working group.
     */
    if (!f->cwg_stack) {
        f->cwg_stack = H5MM_xcalloc(1, sizeof(H5G_cwgstk_t));
    } else if (H5G_close(f->cwg_stack->grp) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CWG, FAIL, "couldn't close previous c.w.g.");
    }
    f->cwg_stack->grp = H5G_reopen(grp);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_getcwg
 *
 * Purpose:     Returns a ptr to the symbol table entry for the current
 *              working group.  If there is no current working group then a
 *              pointer to the root symbol is returned.  If there is no root
 *              symbol then the null pointer is returned.
 *
 * Return:      Success:        Ptr to the current working group or root
 *                              object. The pointer is valid only until the
 *                              root object changes or the current working
 *                              group changes.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5G_entry_t     *
H5G_getcwg(H5F_t *f)
{
    H5G_entry_t            *ret_value = NULL;

    FUNC_ENTER(H5G_getcwg, NULL);

    /* check args */
    assert(f);

    /* return a pointer directly into the stack */
    if (f->cwg_stack && f->cwg_stack->grp) {
        ret_value = &(f->cwg_stack->grp->ent);
    } else {
        ret_value = f->shared->root_ent;
    }

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_push
 *
 * Purpose:     Pushes a new current working group onto the stack.  The GRP
 *              is reopened and held open until it is removed from the stack.
 *
 * Errors:
 *              SYM       CWG           Can't open group. 
 *              SYM       CWG           Not a group. 
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_push(H5F_t *f, H5G_t *grp)
{
    H5G_cwgstk_t           *stack = NULL;

    FUNC_ENTER(H5G_push, FAIL);

    /* check args */
    assert(f);
    assert(grp);

    /*
     * Push a new entry onto the stack.
     */
    stack = H5MM_xcalloc(1, sizeof(H5G_cwgstk_t));
    stack->grp = H5G_reopen(grp);
    stack->next = f->cwg_stack;
    f->cwg_stack = stack;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_pop
 *
 * Purpose:     Pops the top current working group off the stack.
 *
 * Errors:
 *              SYM       CWG           Can't close current working group. 
 *              SYM       CWG           Stack is empty. 
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL if the stack is empty.
 *
 * Programmer:  Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_pop(H5F_t *f)
{
    H5G_cwgstk_t           *stack = NULL;

    FUNC_ENTER(H5G_pop, FAIL);

    /* check args */
    assert(f);

    if ((stack = f->cwg_stack)) {
        if (H5G_close(stack->grp) < 0) {
            HRETURN_ERROR(H5E_SYM, H5E_CWG, FAIL,
                          "can't close current working group");
        }
        f->cwg_stack = stack->next;
        stack->grp = NULL;
        H5MM_xfree(stack);
    } else {
        HRETURN_ERROR(H5E_SYM, H5E_CWG, FAIL, "stack is empty");
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_insert
 *
 * Purpose:     Inserts a symbol table entry into the group graph.  The file
 *              that is used is contained in the ENT argument.
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_insert(const char *name, H5G_entry_t *ent)
{
    const char             *rest = NULL;        /*part of name not existing yet */
    H5G_entry_t             grp;        /*entry for group to contain obj */
    size_t                  nchars;     /*number of characters in name  */
    char                    _comp[1024];        /*name component                */
    hbool_t                 update_grp;
    herr_t                  status;

    FUNC_ENTER(H5G_insert, FAIL);

    /* Check args. */
    assert(name && *name);
    assert(ent);

    /*
     * Look up the name -- it shouldn't exist yet.
     */
    if (H5G_namei(ent->file, NULL, name, &rest, &grp, NULL) >= 0) {
        HRETURN_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "already exists");
    }
    H5ECLEAR;                   /*it's OK that we didn't find it */
    rest = H5G_component(rest, &nchars);

    if (!rest || !*rest) {
        /*
         * The caller is attempting to insert a root object that either
         * doesn't have a name or we shouldn't interfere with the name
         * it already has as a message.
         */
        if (ent->file->shared->root_ent) {
            HRETURN_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "root exists");
        }
        if (1 != H5O_link(ent, 1)) {
            HRETURN_ERROR(H5E_SYM, H5E_LINK, FAIL, "bad link count");
        }
        ent->name_off = 0;
        ent->file->shared->root_ent = H5G_ent_calloc(ent);
        HRETURN(SUCCEED);
    }
    /*
     * There should be one component left.  Make sure it's null
     * terminated.
     */
    if (rest[nchars]) {
        if (H5G_component(rest + nchars, NULL)) {
            HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found");
        } else if (nchars + 1 > sizeof _comp) {
            HRETURN_ERROR(H5E_SYM, H5E_COMPLEN, FAIL, "component is too long");
        } else {
            /* null terminate */
            HDmemcpy(_comp, rest, nchars);
            _comp[nchars] = '\0';
            rest = _comp;
        }
    }
    if (!ent->file->shared->root_ent) {
        /*
         * This will be the only object in the file. Insert it as the root
         * object and add a name messaage to the object header (or modify
         * the first one we find).
         */
        H5O_name_t              name_mesg;
        name_mesg.s = rest;
        if (H5O_modify(ent, H5O_NAME, 0, 0, &name_mesg) < 0) {
            HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL,
                          "cannot add/change name message");
        }
        if (1 != H5O_link(ent, 1)) {
            HRETURN_ERROR(H5E_SYM, H5E_LINK, FAIL, "bad link count");
        }
        ent->file->shared->root_ent = H5G_ent_calloc(ent);
        HRETURN(SUCCEED);
    }
    /*
     * Make sure the root group exists.  Ignore the failure if it's
     * because the group already exists.
     */
    update_grp = H5F_addr_eq(&(grp.header),
                             &(ent->file->shared->root_ent->header));
    if ((status = H5G_mkroot(ent->file, H5G_SIZE_HINT)) < 0 && -2 != status) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create root group");
    }
    H5ECLEAR;
    if (update_grp)
        grp = *(ent->file->shared->root_ent);

    /*
     * This is the normal case.  The object is just being inserted as a normal
     * entry into a symbol table.
     */
    if (H5O_link(ent, 1) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_LINK, FAIL, "link inc failure");
    }
    if (H5G_stab_insert(&grp, rest, ent) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_find
 *
 * Purpose:     Finds an object with the specified NAME in file F.  If
 *              the name is relative then it is interpretted relative
 *              to the current working group.  On successful return,
 *              GRP_ENT (if non-null) will be initialized with the symbol
 *              table information for the group in which the object
 *              appears (it will have an undefined object header address if
 *              the object is the root object) and OBJ_ENT will be
 *              initialized with the symbol table entry for the object
 *              (OBJ_ENT is optional when the caller is interested only in
 *              the existence of the object).
 *
 *              This function will fail if the root object is requested and
 *              there is none.
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED, see above for values of GRP_ENT and
 *                              OBJ_ENT.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_find(H5F_t *f, const char *name,
         H5G_entry_t *grp_ent /*out */ , H5G_entry_t *obj_ent /*out */ )
{
    FUNC_ENTER(H5G_find, FAIL);

    /* check args */
    assert(f);
    assert(name && *name);

    if (H5G_namei(f, NULL, name, NULL, grp_ent, obj_ent) < 0) {
        HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");
    }
    FUNC_LEAVE(SUCCEED);
}
