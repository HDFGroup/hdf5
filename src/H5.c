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
static char RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*+
   FILE
       hdf5.c
   HDF library support routines

   EXPORTED ROUTINES
       H5dont_atexit    -- Indicate that an 'atexit' routine is _not_ to be installed
       H5version        -- Check the version of the library

   LIBRARY-SCOPED ROUTINES
       H5_init_library      -- initialize the HDF5 library
       H5_term_library      -- shut-down the HDF5 library
       H5_init_thread       -- initialize thread-specific information

   LOCAL ROUTINES
       H5_init_interface    -- initialize the H5 interface
   + */

#define HDF5_MASTER
#include "hdf5.h"
#undef HDF5_MASTER

/* private headers */
#include "H5ACprivate.h"		/*cache				*/
#include "H5Bprivate.h"			/*B-link trees			*/
#include "H5private.h"			/* Generic info */

#define PABLO_MASK	H5_mask

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize_g = FALSE;

/*------------------_-- Local function prototypes ----------------------------*/
static herr_t H5_init_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5_init_library -- Initialize library-global information
USAGE
    herr_t H5_init_library()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any library-global data or routines.

--------------------------------------------------------------------------*/
herr_t H5_init_library(void)
{
    FUNC_ENTER (H5_init_library, NULL, FAIL);

    /* Install atexit() library cleanup routine */
    if(install_atexit==TRUE)
        if (HDatexit(&H5_term_library) != 0)
          HRETURN_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL);

    FUNC_LEAVE (SUCCEED);
}	/* H5_init_library */

/*--------------------------------------------------------------------------
 NAME
    H5_term_library
 PURPOSE
    Terminate various static buffers and shutdown the library.
 USAGE
    void HPend()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Walk through the shutdown routines for the various interfaces and 
    terminate them all.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Should only ever be called by the "atexit" function, or real power-users.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void
H5_term_library (void)
{
} /* end H5_term_library() */

/*--------------------------------------------------------------------------
NAME
   H5_init_thread -- Initialize thread-specific information
USAGE
    void H5_init_thread()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any thread-specific data or routines.

--------------------------------------------------------------------------*/
herr_t H5_init_thread(void)
{
    FUNC_ENTER (H5_init_thread, NULL, FAIL);

    /* Create/initialize this thread's error stack */
    if((thrderrid=H5Enew_err_stack(16))==FAIL)
       HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, FAIL);

    FUNC_LEAVE (SUCCEED);
}	/* H5_init_thread */

/*--------------------------------------------------------------------------
NAME
   H5_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t H5_init_interface(void)
{
    FUNC_ENTER (H5_init_interface, NULL, FAIL);

    FUNC_LEAVE (SUCCEED);
}	/* H5_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5dont_atexit
 PURPOSE
    Indicates to the library that an 'atexit()' routine is _not_ to be installed
 USAGE
    herr_t H5dont_atexit(void)
 RETURNS
    Returns SUCCEED/FAIL
 DESCRIPTION
        This routine indicates to the library that an 'atexit()' cleanip routine
    should not be installed.  The major (only?) purpose for this is in
    situations where the library is dynamically linked into an application and
    is un-linked from the application before 'exit()' gets callled.  In those
    situations, a routine installed with 'atexit()' would jump to a routine
    which was no longer in memory, causing errors.
        In order to be effective, this routine _must_ be called before any other
    HDF function calls, and must be called each time the library is loaded/
    linked into the application. (the first time and after it's been un-loaded) 
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    If this routine is used, certain memory buffers will not be de-allocated,
    although in theory a user could call HPend on their own...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5dont_atexit(void)
{
    FUNC_ENTER (H5dont_atexit, NULL, FAIL);

    if(install_atexit == TRUE)
        install_atexit=FALSE;

    FUNC_LEAVE (SUCCEED);
} /* end H5dont_atexit() */

/*--------------------------------------------------------------------------
NAME
   H5version -- Checks the version of the library
USAGE
   herr_t H5version(majnum, minnum, relnum, patnum)
   uintn *majnum;   OUT: The major revision number of the HDF5 library
   uintn *minnum;   OUT: The minor revision number of the HDF5 library
   uintn *relnum;   OUT: The release revision number of the HDF5 library
   uintn *patnum;   OUT: The patch revision number of the HDF5 library
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Checks the version numbers of the library.

--------------------------------------------------------------------------*/
herr_t H5version(uintn *majnum, uintn *minnum, uintn *relnum, uintn *patnum)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER(H5version, H5_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if (majnum==NULL || minnum==NULL || relnum==NULL || patnum==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    /* Set the version information */
    *majnum=HDF5_MAJOR_VERSION;
    *minnum=HDF5_MINOR_VERSION;
    *relnum=HDF5_RELEASE_VERSION;
    *patnum=HDF5_PATCH_VERSION;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
}	/* H5version */

