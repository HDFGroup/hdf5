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
 * Created:	H5Edeprec.c
 *		April 11 2007
 *		Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:	Deprecated functions from the H5E interface.  These
 *              functions are here for compatibility purposes and may be
 *              removed in the future.  Applications should switch to the
 *              newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5E_PACKAGE		/*suppress error about including H5Epkg   */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5E_init_deprec_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Iprivate.h"		/* IDs                                  */
#include "H5Epkg.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5MMprivate.h"	/* Memory management			*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*--------------------------------------------------------------------------
NAME
   H5E_init_deprec_interface -- Initialize interface-specific information
USAGE
    herr_t H5E_init_deprec_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5E_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5E_init_deprec_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5E_init_deprec_interface)

    FUNC_LEAVE_NOAPI(H5E_init())
} /* H5E_init_deprec_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_major
 *
 * Purpose:	Retrieves a major error message.
 *
 * Return:      Returns message if succeeds.
 *              otherwise returns NULL.
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 *-------------------------------------------------------------------------
 */
const char *
H5Eget_major(H5E_major_t maj)
{
    H5E_msg_t   *msg;           /* Pointer to error message */
    ssize_t     size = 0;       /* Return value */
    H5E_type_t  type;
    char        *msg_str;
    char        *ret_value = NULL;

    FUNC_ENTER_API_NOCLEAR(H5Eget_major, NULL)

    /* Get the message object */
    if(NULL == (msg = H5I_object_verify(maj, H5I_ERROR_MSG)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a error message ID")

    /* Get the message's text */
    if((size = H5E_get_msg(msg, &type, NULL, (size_t)0)) < 0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "can't get error message text")

    if(type != H5E_MAJOR)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "Error message isn't a major one")

    /* Don't know who is going to free it */
    msg_str = (char *)H5MM_malloc((size_t)(++size) * sizeof(char));

    if(H5E_get_msg(msg, NULL, msg_str, (size_t)size) < 0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "can't get error message text")

    ret_value = msg_str;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eget_major() */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_minor
 *
 * Purpose:	Retrieves a minor error message.
 *
 * Return:      Returns message if succeeds.
 *              otherwise returns NULL.
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 *-------------------------------------------------------------------------
 */
const char *
H5Eget_minor(H5E_minor_t min)
{
    H5E_msg_t   *msg;           /* Pointer to error message */
    ssize_t      size = 0;       /* Return value */
    H5E_type_t  type;
    char        *msg_str;
    char        *ret_value = NULL;

    FUNC_ENTER_API_NOCLEAR(H5Eget_minor, NULL)

    /* Get the message object */
    if(NULL == (msg = H5I_object_verify(min, H5I_ERROR_MSG)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a error message ID")

    /* Get the message's text */
    if((size = H5E_get_msg(msg, &type, NULL, (size_t)0)) < 0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "can't get error message text")

    if(type != H5E_MINOR)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "Error message isn't a minor one")

    /* Don't know who is going to free it */
    msg_str = (char *)H5MM_malloc((size_t)(++size) * sizeof(char));

    if(H5E_get_msg(msg, NULL, msg_str, (size_t)size) < 0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "can't get error message text")

    ret_value = msg_str;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eget_minor() */


/*-------------------------------------------------------------------------
 * Function:	H5Epush
 *
 * Purpose:	This function definition is for backward compatibility only.
 *              It doesn't have error stack and error class as parameters.
 *              The old definition of major and minor is casted as HID_T
 *              in H5Epublic.h
 *
 * Notes: 	Basically a public API wrapper around the H5E_push2
 *              function.  For backward compatibility, it maintains the
 *              same parameter as the old function, in contrary to
 *              H5Epush2.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Tuesday, Sep 16, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epush(const char *file, const char *func, unsigned line,
        H5E_major_t maj, H5E_minor_t min, const char *str)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Epush, FAIL)
    H5TRACE6("e", "*s*sIuii*s", file, func, line, maj, min, str);

    /* Push the error on the default error stack */
    if(H5E_push_stack(NULL, file, func, line, H5E_ERR_CLS_g, maj, min, str) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't push error on stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Epush() */


/*-------------------------------------------------------------------------
 * Function:	H5Eclear
 *
 * Purpose:	This function is for backward compatbility.
 *              Clears the error stack for the specified error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Wednesday, July 16, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclear(void)
{
    herr_t ret_value = SUCCEED; /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eclear, FAIL)
    H5TRACE0("e","");

    /* Clear the default error stack */
    if(H5E_clear_stack(NULL) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't clear error stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eclear() */


/*-------------------------------------------------------------------------
 * Function:	H5Eprint
 *
 * Purpose:	This function is for backward compatbility.
 *              Prints the error stack in some default way.  This is just a
 *		convenience function for H5Ewalk() with a function that
 *		prints error messages.  Users are encouraged to write there
 *		own more specific error handlers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Sep 16, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eprint(FILE *stream)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value = SUCCEED; /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eprint, FAIL)
    /*NO TRACE*/

    if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Print error stack */
    if(H5E_print2(estack, stream, TRUE) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't display error stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eprint() */


/*-------------------------------------------------------------------------
 * Function:	H5Ewalk
 *
 * Purpose:	This function is for backward compatbility.
 *              Walks the error stack for the current thread and calls some
 *		function for each error along the way.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Sep 16, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Ewalk(H5E_direction_t direction, H5E_walk_t func, void *client_data)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value = SUCCEED; /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Ewalk, FAIL)
    /*NO TRACE*/

    if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Walk the error stack */
    if(H5E_walk2(estack, direction, func, NULL, TRUE, client_data) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't walk error stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Ewalk() */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_auto
 *
 * Purpose:	This function is for backward compatbility.
 *              Returns the current settings for the automatic error stack
 *		traversal function and its data for specific error stack.
 *		Either (or both) arguments may be null in which case the
 *		value is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Sep 16, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eget_auto(H5E_auto_t *func, void **client_data)
{
    H5E_t   *estack;            /* Error stack to operate on */
    H5E_auto_op_t f;            /* Error stack function */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Eget_auto, FAIL)
    H5TRACE2("e", "*x**x", func, client_data);

    /* Retrieve default error stack */
    if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Get the automatic error reporting information */
    if(H5E_get_auto2(estack, FALSE, &f, client_data) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get automatic error info")
    if(func)
        *func = f.efunc;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eget_auto() */


/*-------------------------------------------------------------------------
 * Function:	H5Eset_auto
 *
 * Purpose:	This function is for backward compatbility.
 *              Turns on or off automatic printing of errors for certain
 *              error stack.  When turned on (non-null FUNC pointer) any
 *              API function which returns an error indication will first
 *              call FUNC passing it CLIENT_DATA as an argument.
 *
 *		The default values before this function is called are
 *		H5Eprint() with client data being the standard error stream,
 *		stderr.
 *
 *		Automatic stack traversal is always in the H5E_WALK_DOWNWARD
 *		direction.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Sep 16, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eset_auto(H5E_auto_t func, void *client_data)
{
    H5E_t   *estack;            /* Error stack to operate on */
    H5E_auto_op_t f;            /* Error stack function */
    herr_t ret_value = SUCCEED; /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eset_auto, FAIL)
    H5TRACE2("e", "x*x", func, client_data);

    if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Set the automatic error reporting information */
    f.efunc = func;
    if(H5E_set_auto2(estack, FALSE, &f, client_data) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't set automatic error info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eset_auto() */

