/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
 *
 * Notes: It is safe to call HRETURN_ERROR(), HGOTO_ERROR(), HERROR(), and
 *        H5ECLEAR within any of these functions except H5E_push() (see
 *        comments in H5E_push()).  However, some of the H5E API functions
 *        don't call H5ECLEAR the the error stack on which they're operating
 *        is the thread's global error stack.  If the thread's global error
 *        stack isn't defined yet, then HRETURN_ERROR(), HGOTO_ERROR(), and
 *        HERROR() don't push an error message and H5ECLEAR just returns
 *        without doing anything.
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <H5private.h>          /* Generic Functions                      */
#include <H5Aprivate.h>         /* Atoms                          */
#include <H5Eprivate.h>         /* Private error routines         */
#include <H5MMprivate.h>        /* Memory management                      */

#define PABLO_MASK      H5E_mask

/*-------------------- Locally scoped variables -----------------------------*/

static const H5E_major_mesg_t H5E_major_mesg_g[] =
{
    {H5E_NONE_MAJOR, "No error"},
    {H5E_ARGS, "Invalid arguments to routine"},
    {H5E_RESOURCE, "Resource unavailable"},
    {H5E_INTERNAL, "Internal HDF5 error"},
    {H5E_FILE, "File Accessability"},
    {H5E_IO, "Low-level I/O"},
    {H5E_FUNC, "Function Entry/Exit"},
    {H5E_ATOM, "Object Atom"},
    {H5E_CACHE, "Object Cache"},
    {H5E_BTREE, "B-Tree Node"},
    {H5E_SYM, "Symbol Table"},
    {H5E_HEAP, "Heap"},
    {H5E_OHDR, "Object Header"},
    {H5E_DATATYPE, "Datatype"},
    {H5E_DATASPACE, "Dataspace"},
    {H5E_DATASET, "Dataset"},
    {H5E_STORAGE, "Data Storage"},
    {H5E_TEMPLATE, "Template"},
};

static const H5E_minor_mesg_t H5E_minor_mesg_g[] =
{
    {H5E_NONE_MINOR, "No error"},
    {H5E_UNINITIALIZED, "Information is uninitialized"},
    {H5E_UNSUPPORTED, "Feature is unsupported"},
    {H5E_BADTYPE, "Incorrect type found"},
    {H5E_BADRANGE, "Argument out of range"},
    {H5E_BADVALUE, "Bad value for argument"},
    {H5E_NOSPACE, "No space available for allocation"},
    {H5E_FILEEXISTS, "File already exists"},
    {H5E_FILEOPEN, "File already open"},
    {H5E_CANTCREATE, "Can't create file"},
    {H5E_CANTOPENFILE, "Can't open file"},
    {H5E_CANTOPENOBJ, "Can't open object"},
    {H5E_NOTHDF5, "Not an HDF5 format file"},
    {H5E_BADFILE, "Bad file ID accessed"},
    {H5E_TRUNCATED, "File has been truncated"},
    {H5E_SEEKERROR, "Seek failed"},
    {H5E_READERROR, "Read failed"},
    {H5E_WRITEERROR, "Write failed"},
    {H5E_CLOSEERROR, "Close failed"},
    {H5E_OVERFLOW, "Address overflowed"},
    {H5E_CANTINIT, "Can't initialize interface"},
    {H5E_ALREADYINIT, "Object already initialized"},
    {H5E_BADATOM, "Can't find atom information"},
    {H5E_CANTREGISTER, "Can't register new atom"},
    {H5E_CANTFLUSH, "Can't flush object from cache"},
    {H5E_CANTLOAD, "Can't load object into cache"},
    {H5E_PROTECT, "Protected object error"},
    {H5E_NOTCACHED, "Object not currently cached"},
    {H5E_NOTFOUND, "Object not found"},
    {H5E_EXISTS, "Object already exists"},
    {H5E_CANTENCODE, "Can't encode value"},
    {H5E_CANTDECODE, "Can't decode value"},
    {H5E_CANTSPLIT, "Can't split node"},
    {H5E_CANTINSERT, "Can't insert object"},
    {H5E_CANTLIST, "Can't list node"},
    {H5E_LINKCOUNT, "Bad object header link count"},
    {H5E_VERSION, "Wrong version number"},
    {H5E_ALIGNMENT, "Alignment error"},
    {H5E_BADMESG, "Unrecognized message"},
    {H5E_COMPLEN, "Name component is too long"},
    {H5E_CWG, "Problem with current working group"},
    {H5E_LINK, "Link count failure"},
};

/* Interface initialization? */
static intn             interface_initialize_g = FALSE;
#define INTERFACE_INIT H5E_init_interface
static herr_t           H5E_init_interface(void);
static void             H5E_term_interface(void);

const hbool_t		H5E_clearable_g = TRUE;
hid_t                   H5E_thrdid_g = FAIL;    /* Thread-specific "global" error-handler ID */

/*--------------------------------------------------------------------------
NAME
   H5E_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5E_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

Modifications:
    Robb Matzke, 4 Aug 1997
    Changed the pablo mask from H5_mask to H5E_mask

--------------------------------------------------------------------------*/
static herr_t
H5E_init_interface(void)
{
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5E_init_interface, FAIL);

    /* Initialize the atom group for the error stacks */
    if ((ret_value = H5A_init_group(H5_ERR, H5A_ERRSTACK_HASHSIZE, 0,
				    (herr_t (*)(void *)) H5E_close)) != FAIL) {
        ret_value = H5_add_exit(H5E_term_interface);
    }
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5E_term_interface
 PURPOSE
    Terminate various H5E objects
 USAGE
    void H5E_term_interface()
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
H5E_term_interface(void)
{
    H5A_destroy_group(H5_ERR);
}

/*--------------------------------------------------------------------------
NAME
   H5Ecreate -- Create a new error stack
USAGE
    hid_t H5Ecreate (initial_stack_size);
    uintn initial_stack_size;       IN: Starting size of the error stack
   
RETURNS
    The ID of the error stack created on success, FAIL on failure.

DESCRIPTION
    Dynamically creates a new error stack to push error values onto.

--------------------------------------------------------------------------*/
hid_t
H5Ecreate(uintn initial_stack_nelmts)
{
    H5E_t                  *new_stack = NULL;   /* Pointer to the new error stack */
    hid_t                   ret_value = FAIL;

    FUNC_ENTER(H5Ecreate, FAIL);

    /* Check args */
    initial_stack_nelmts = MAX(10, MIN(initial_stack_nelmts, 1000));

    /* Allocate the stack header */
    new_stack = H5MM_xmalloc(sizeof(H5E_t));

    /* Initialize the stack header */
    new_stack->nelmts = initial_stack_nelmts;
    new_stack->top = 0;
    new_stack->stack = H5MM_xcalloc(initial_stack_nelmts, sizeof(H5E_error_t));
    new_stack->push = H5E_push; /* Set the default error handler */

    /* Get an atom for the error stack */
    if ((ret_value = H5A_register(H5_ERR, new_stack)) < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
                      "unable to register error stack");
    }
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
NAME
   H5Eclose -- Destroy an error stack
USAGE
    herr_t H5Eclose (err_stack);
    hid_t err_stack;       IN: Error stack to delete
   
RETURNS
    SUCCEED/FAIL

DESCRIPTION
    Destroys an error stack, releasing memory allocated, etc.

--------------------------------------------------------------------------*/
herr_t
H5Eclose(hid_t estack_id)
{
    FUNC_ENTER(H5Eclose, FAIL);

    /* check args */
    if (H5_ERR != H5A_group(estack_id)) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error stack");
    }
    /*
     * Decrement the reference count.  When it reaches zero the error stack
     * will be freed.
     */
    H5A_dec_ref(estack_id);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Epush
 *
 * Purpose:     Pushes a new error record onto error stack ESTACK_ID.  The
 *              error has major and minor numbers MAJ_NUM and MIN_NUM, the
 *              name of a function where the error was detected, the name of
 *              the file where the error was detected, and the line within
 *              that file.  An error description string is also passed to
 *              this function.
 *
 *              The FUNCTION_NAME is copied (and possibly truncated) into a
 *              fixed length character buffer; the FILE_NAME is pointed to
 *              without copying it (we assume it's statically allocated from
 *              __FILE__); and the DESC argument is strdup'd.
 *
 *              It is safe to call this function before the thread global
 *              error stack is initialized.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, December 12, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epush(hid_t estack_id, H5E_major_t maj_num, H5E_minor_t min_num,
        const char *function_name, const char *file_name, intn line,
        const char *desc)
{

    H5E_t                  *estack = NULL;      /* Ptr to the stack to put value on */

    /*
     * WARNING WARNING WARNING: We cannot call HERROR() from within this
     * function if ESTACK_ID is the thread global error stack or else we may
     * enter infinite recursion.  Furthermore, we also cannot call any other
     * HDF5 macro or function which might call HERROR().  HERROR() is called
     * by HRETURN_ERROR() which could be called by FUNC_ENTER().
     */

    /*
     * Clear the thread global error stack only if it isn't the error stack on
     * which we're pushing the new error.
     */
    if (estack_id != H5E_thrdid_g)
        H5ECLEAR;

    /*
     * check args, but don't call error functions if ESTACK_ID is the thread
     * global error handler.
     */
    if (H5_ERR != H5A_group(estack_id) ||
        NULL == (estack = H5A_object(estack_id))) {
        HRETURN(FAIL);
    }
    if (!function_name || !file_name || !desc) {
        HRETURN(FAIL);
    }
    if (!estack->push) {
        HRETURN(FAIL);
    }
    /* Push the new error.  It must be safe to call the push function. */
    if ((estack->push) (estack, maj_num, min_num, function_name, file_name,
                        line, desc) < 0) {
        HRETURN(FAIL);
    }
    return SUCCEED;             /*don't use FUNC_LEAVE() here */
}

/*--------------------------------------------------------------------------
NAME
   H5Eclear -- Clear an error stack for later error entries
USAGE
    void H5Eclear(int32 err_hand)
    int32 err_hand;         IN: The ID of the error stack to push the error onto.
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Clear an error stack to allow errors to be pushed onto it.

--------------------------------------------------------------------------*/
herr_t
H5Eclear(hid_t estack_id)
{
    H5E_t		*estack = NULL;
    hbool_t		H5E_clearable_g = FALSE; /*override global*/

    FUNC_ENTER(H5Eclear, FAIL);

    /*
     * Normally we would clear the thread error stack since we're entering an
     * API function, but we have to be careful here to not get into an
     * infinite recursion.
     */
    if (estack_id != H5E_thrdid_g)
        H5ECLEAR;

    /* check args */
    if (H5_ERR != H5A_group(estack_id) ||
        NULL == (estack = H5A_object(estack_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error stack");
    }
    if (H5E_clear(estack) < 0) {
        HRETURN_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL,
                      "unable to clear error stack");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5Eprint
 *
 * Purpose:     Prints the current contents of error stack ESTACK_ID to the
 *              stream FILE.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, December 12, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eprint(hid_t estack_id, FILE * file)
{
    H5E_t               *estack = NULL;
    hbool_t		H5E_clearable_g = FALSE; /*override global*/

    FUNC_ENTER(H5Eprint, FAIL);
    /*
     * Don't clear the thread error stack if it's the one we're about to
     * print.
     */
    if (estack_id != H5E_thrdid_g)
        H5ECLEAR;

    /* check args */
    if (H5_ERR != H5A_group(estack_id) ||
        NULL == (estack = H5A_object(estack_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error stack");
    }
    if (!file)
        file = stderr;

    /* print it */
    if (H5E_print(estack, file) < 0) {
        HRETURN_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL,
                      "can't print error stack");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5E_close
 *
 * Purpose:     Frees resources associated with an error stack.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, December 12, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_close(H5E_t *estack)
{
    FUNC_ENTER(H5E_close, FAIL);

    /* check args */
    assert(estack);

    /* clear error stack, then free it */
    H5E_clear(estack);
    H5MM_xfree(estack->stack);
    H5MM_xfree(estack);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5E_clear
 *
 * Purpose:     Clears an error stack but does not release the stack.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, December 12, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_clear(H5E_t *estack)
{
    int                     i;

    FUNC_ENTER(H5E_clear, FAIL);

    /* check args */
    assert(estack);

    /* Clear the error descriptions and reset the stack top */
    for (i = 0; i < estack->top; i++) {
        H5MM_xfree(estack->stack[i].desc);
        estack->stack[i].desc = NULL;
    }
    estack->top = 0;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5E_push
 *
 * Purpose:     Push an error onto an error stack.  The FUNCTION_NAME is
 *              copied (and possibly truncated) into the error record.  The
 *              FILE_NAME pointer is used directly since we assume it came
 *              from the __FILE__ construct and is thus static data.  The
 *              description, DESC, is strdup'd into the error record.
 *
 * Note:        Warning: to prevent infinite recursivion this function must
 *                       not call any other HDF5 function and especially not
 *                       the HDF5 error handling macros.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, December 12, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_push(H5E_t *estack, H5E_major_t maj_num, H5E_minor_t min_num,
         const char *function_name, const char *file_name, intn line,
         const char *desc)
{

    /* FUNC_ENTER (H5E_push, FAIL); -- can't do this here! */

    /* check args */
    assert(estack);
    assert(function_name);
    assert(file_name);

    /* Check if we need to expand the stack */
    if (estack->top >= estack->nelmts) {
        /*
         * Ask for new stack that's twice as large.  Do not use hdf5 functions
         * to allocate the memory!
         */
        estack->nelmts *= 2;
        estack->stack = realloc(estack->stack,
                                estack->nelmts * sizeof(H5E_error_t));
        assert(estack->stack);
    }
    /* Push the error onto the error stack */
    estack->stack[estack->top].maj_num = maj_num;
    estack->stack[estack->top].min_num = min_num;
    HDstrncpy(estack->stack[estack->top].func_name, function_name,
              MAX_FUNC_NAME);
    estack->stack[estack->top].func_name[MAX_FUNC_NAME - 1] = '\0';
    estack->stack[estack->top].file_name = file_name;
    estack->stack[estack->top].line = line;

    /* strdup the description but don't use H5MM_xstrdup() */
    estack->stack[estack->top].desc = malloc(strlen(desc) + 1);
    assert(estack->stack[estack->top].desc);
    strcpy(estack->stack[estack->top].desc, desc);

    /* Increment the top of the error stack */
    estack->top++;

    return SUCCEED;             /*don't use FUNC_LEAVE() here */
}

/*-------------------------------------------------------------------------
 * Function:    H5E_print
 *
 * Purpose:     Prints an error stack ESTACK to the stream FILE.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, December 12, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_print(H5E_t *estack, FILE * file)
{
    intn                    i, j;
    const char             *maj_str = NULL;
    const char             *min_str = NULL;

    FUNC_ENTER(H5E_print, FAIL);

    /* check args */
    assert(estack);
    assert(file);

    if (0 == estack->top)
        HRETURN(SUCCEED);

    fprintf(file, "HDF5-DIAG: error stack:\n");
    for (i = 0; i < estack->top; i++) {

        /* Find major and minor error strings */
        for (j = 0, maj_str = "??"; j < NELMTS(H5E_major_mesg_g); j++) {
            if (H5E_major_mesg_g[j].error_code == estack->stack[i].maj_num) {
                maj_str = H5E_major_mesg_g[j].str;
                break;
            }
        }
        for (j = 0, min_str = "??"; j < NELMTS(H5E_minor_mesg_g); j++) {
            if (H5E_minor_mesg_g[j].error_code == estack->stack[i].min_num) {
                min_str = H5E_minor_mesg_g[j].str;
                break;
            }
        }

        /* Print error message */
        fprintf(file, "   #%03d: %s:%d in %s() error %02d/%02d: %s\n",
                i, estack->stack[i].file_name, estack->stack[i].line,
                estack->stack[i].func_name, estack->stack[i].maj_num,
                estack->stack[i].min_num, estack->stack[i].desc);
        fprintf(file, "         %s (%s)\n", maj_str, min_str);
    }

    FUNC_LEAVE(SUCCEED);
}
