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


/* private headers */
#include <H5private.h>			/*library			*/
#include <H5ACprivate.h>		/*cache				*/
#include <H5Bprivate.h>			/*B-link trees			*/
#include <H5Eprivate.h>			/*error handling		*/
#include <H5MMprivate.h>		/*memory management		*/
#include <H5Tprivate.h>			/*data types			*/

#define PABLO_MASK	H5_mask

/*--------------------- Locally scoped variables -----------------------------*/


hbool_t library_initialize_g = FALSE;
hbool_t	thread_initialize_g = FALSE;
hbool_t install_atexit_g = TRUE;

typedef struct H5_exit {
   void (*func)(void);     /* Interface function to call during exit */
   struct H5_exit *next;   /* Pointer to next node with exit function */
} H5_exit_t;

H5_exit_t *lib_exit_head;   /* Pointer to the head of the list of 'atexit' functions */

/* Interface initialization */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT H5_init_interface
static herr_t H5_init_interface (void);

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
   FUNC_ENTER_INIT (H5_init_library, NULL, FAIL);

   /* Install atexit() library cleanup routine */
   if(install_atexit_g==TRUE)
      if (HDatexit(&H5_term_library) != 0)
	 HRETURN_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL,
		       "unable to register atexit function");

   /*
    * Initialize interfaces that might not be able to initialize themselves
    * soon enough.
    */
   if (H5T_init_interface ()<0) {
      HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, FAIL,
		     "unable to initialize type interface");
   }
   

   FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5_add_exit
 PURPOSE
    Add an exit routine to the list of routines to call during 'atexit'
 USAGE
    herr_t H5_add_exit(func)
        void (*func)(void);     IN: Function pointer of routine to add to chain

 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Pre-pend the new function to the list of function to call during the exit
    process.  These routines are responsible for free'ing static buffers, etc.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Don't make assumptions about the environment during the exit procedure...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5_add_exit (void (*func)(void))
{
    herr_t ret_value = SUCCEED;
    H5_exit_t *new;

    FUNC_ENTER_INIT (H5_add_exit, NULL, FAIL);

    assert(func);

    new = H5MM_xcalloc (1, sizeof(H5_exit_t));

    new->func=func;
    new->next=lib_exit_head;
    lib_exit_head=new;

    FUNC_LEAVE(ret_value);
} /* end H5_add_exit() */

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
    H5_exit_t *temp;

    temp=lib_exit_head;
    while(lib_exit_head!=NULL)
      {
          (*lib_exit_head->func)();
          lib_exit_head=lib_exit_head->next;
          HDfree(temp);
          temp=lib_exit_head;
      } /* end while */
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
    FUNC_ENTER_INIT (H5_init_thread, NULL, FAIL);

    /* Create/initialize this thread's error stack */
    if((thrderrid=H5Enew_err_stack(16))==FAIL)
       HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, FAIL,
		      "unable to create thread error stack");

    /* Add the "thread termination" routine to the exit chain */
    if(H5_add_exit(&H5_term_thread)==FAIL)
       HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, FAIL,
		      "unable to set thread atexit function");

    FUNC_LEAVE (SUCCEED);
}	/* H5_init_thread */

/*--------------------------------------------------------------------------
 NAME
    H5_term_thread
 PURPOSE
    Terminate various thread-specific objects
 USAGE
    void H5_term_thread()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the error stack and any other thread-specific resources allocated
    on a "per thread" basis.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void
H5_term_thread (void)
{
    H5Edelete_err_stack(thrderrid);
} /* end H5_term_thread() */

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
    FUNC_ENTER (H5_init_interface, FAIL);

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
    FUNC_ENTER_INIT (H5dont_atexit, NULL, FAIL);

    if(install_atexit_g == TRUE)
        install_atexit_g=FALSE;

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

    FUNC_ENTER (H5version, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if (majnum==NULL || minnum==NULL || relnum==NULL || patnum==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
		    "null pointer argument");

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
}



/*-------------------------------------------------------------------------
 * Function:	H5init
 *
 * Purpose:	Initialize the library.  This is normally called
 *		automatically, but if you find that an HDF5 library function
 *		is failing inexplicably, then try calling this function
 *		first.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5init (void)
{
   FUNC_ENTER (H5init, FAIL);
   /* all work is done by FUNC_ENTER() */
   FUNC_LEAVE (SUCCEED);
}

   
