/****************************************************************************
* NCSA HDF								   *
* Software Development Group						   *
* National Center for Supercomputing Applications			   *
* University of Illinois at Urbana-Champaign				   *
* 605 E. Springfield, Champaign IL 61820				   *
*									   *
* For conditions of distribution and use, see the accompanying		   *
* hdf/COPYING file.							   *
*									   *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <H5private.h>		/* Generic Functions			  */
#include <H5Iprivate.h>		/* ID Functions		  */
#include <H5Eprivate.h>		/* Error handling		  */
#include <H5Rprivate.h>		/* Data-space functions			  */

/* Interface initialization */
#define PABLO_MASK	H5R_mask
#define INTERFACE_INIT	H5R_init_interface
static intn		interface_initialize_g = FALSE;
static herr_t		H5R_init_interface(void);
static void		H5R_term_interface(void);


/*--------------------------------------------------------------------------
NAME
   H5R_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5R_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5R_init_interface(void)
{
    herr_t		    ret_value = SUCCEED;
    FUNC_ENTER(H5R_init_interface, FAIL);

    /* Initialize the atom group for the file IDs */
    if ((ret_value = H5I_init_group(H5I_REFERENCE, H5I_REFID_HASHSIZE,
            H5R_RESERVED_ATOMS, (herr_t (*)(void *)) NULL)) != FAIL) {
        ret_value = H5_add_exit(&H5R_term_interface);
    }

    FUNC_LEAVE(ret_value);
}   /* end H5R_init_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5R_term_interface
 PURPOSE
    Terminate various H5R objects
 USAGE
    void H5R_term_interface()
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
H5R_term_interface(void)
{
    /* Free ID group */
    H5I_destroy_group(H5I_REFERENCE);
}   /* end H5R_term_interface() */

