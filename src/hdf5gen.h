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

/* $Id$ */

/*
 * This file contains general macros used throughout HDF5 library & interfaces
 */
#include "hdf5pabl.h"

#ifndef HDF5GEN_H
#define HDF5GEN_H

/* return code - since some unix/c routines use 0 and -1 as their return
   code, and some assumption had been made in the code about that, it is
   important to keep these constants the same values.  For explicitly
   boolean functions, use TRUE and FALSE */

#define SUCCEED 0
#define FAIL (-1)

/* boolean values,  reminder: NEVER compare with numeric values */

#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE (!FALSE)
#endif

/* number of members in an array */
#ifndef NELMTS
#   define NELMTS(X)	(sizeof(X)/sizeof(X[0]))
#endif

/*-------------------------------------------------------------------------
 * Purpose:	Register function entry for library initialization and code
 *		profiling.
 *
 * Notes:	Every file must have a file-scope variable called
 *		`initialize_interface'.
 *
 * 		Don't use local variable initializers which contain
 *		calls to other library functions since the initializer
 *		would happen before the FUNC_ENTER() gets called.  Don't
 *		use initializers that require special cleanup code to
 *		execute if FUNC_ENTER() fails since a failing FUNC_ENTER()
 *		returns immediately without branching to the `done' label.
 *
 * Programmer:	Quincey Koziol
 *
 * Modifications:
 *
 * 	Robb Matzke, 4 Aug 1997
 *	The `interface_init_func' can be the null pointer.  Changed
 *	HGOTO_ERROR() to HRETURN_ERROR() since no clean-up needs to occur
 *	when an error is detected at this point since this must be the
 *	first executable statement in a function.  This allows functions
 *	to omit the `done:' label when convenient to do so.
 *
 * 	Robb Matzke, 4 Aug 1997
 *	The pablo mask comes from the constant PABLO_MASK defined on
 *	a per-file basis.  The `pablo_func_id' is generated from the
 *	`func_name' argument by prepending an `ID_' to the name.  The
 *      pablo function identifier should be saved in a local variable
 *	so FUNC_LEAVE() can access it.
 *
 * 	Robb Matzke, 4 Aug 1997
 *	It is safe to call this function even inside various library
 *	initializing functions.  Infinite recursion is no longer a
 *	danger.
 *
 *-------------------------------------------------------------------------
 */
#define FUNC_ENTER(func_name,interface_init_func,err)			      \
   CONSTR (FUNC, #func_name);						      \
   PABLO_SAVE (ID_ ## func_name);					      \
									      \
   PABLO_TRACE_ON (PABLO_MASK, pablo_func_id);				      \
									      \
   if (!library_initialize_g) {						      \
      library_initialize_g = TRUE;					      \
      if (H5_init_library()<0) {					      \
	 HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, err);			      \
      }									      \
   }									      \
									      \
   if (!thread_initialize_g) {						      \
      thread_initialize_g = TRUE;					      \
      if (H5_init_thread()<0) {						      \
	 HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, err);			      \
      }									      \
   }									      \
      									      \
   if (!interface_initialize_g) {					      \
      interface_initialize_g = TRUE;					      \
      if (interface_init_func &&					      \
	  ((herr_t(*)(void))interface_init_func)()<0) {			      \
	 HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, err);			      \
      }									      \
   }



/*-------------------------------------------------------------------------
 * Purpose:	Register function exit for code profiling.  This should be
 *		the last statement executed by a function.
 *
 * Programmer:	Quincey Koziol
 *
 * Modifications:
 *
 * 	Robb Matzke, 4 Aug 1997
 *	The pablo mask comes from the constant PABLO_MASK defined on a
 *	per-file basis.  The pablo_func_id comes from an auto variable
 *	defined by FUNC_ENTER.
 *
 *-------------------------------------------------------------------------
 */
#define FUNC_LEAVE(return_value) HRETURN(return_value)

#endif /* HDF5GEN_H */

