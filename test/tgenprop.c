/****************************************************************************
 * NCSA HDF								                                    *
 * Software Development Group						                        *
 * National Center for Supercomputing Applications			                *
 * University of Illinois at Urbana-Champaign				                *
 * 605 E. Springfield, Champaign IL 61820				                    *
 *									                                        *
 * For conditions of distribution and use, see the accompanying		        *
 * hdf/COPYING file.							                            *
 *									                                        *
 ****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "$Revision$";
#endif

/* $Id$ */

/***********************************************************
*
* Test program:	 tgenprop
*
* Test the Generic Property functionality
*
*************************************************************/

#include <testhdf5.h>

#include <hdf5.h>

#define FILENAME   "tgenprop.h5"

/****************************************************************
**
**  test_genprop(): Main generic property testing routine.
** 
****************************************************************/
void 
test_genprop(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Generic Properties\n"));

    /* These tests use the same file... */

}   /* test_genprop() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_genprop
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              June 8, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_genprop(void)
{
    remove(FILENAME);
}

