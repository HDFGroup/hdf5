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
* Test program:	 tselect
*
* Test the dataspace selection functionality
*
*************************************************************/

#include <testhdf5.h>

#include <hdf5.h>

#define FILE   "tselect.h5"

/* 3-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	3
#define SPACE1_DIM1	3
#define SPACE1_DIM2	15
#define SPACE1_DIM3	13

/* 2-D dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	2
#define SPACE2_DIM1	30
#define SPACE2_DIM2	26

/* 2-D dataset with fixed dimensions */
#define SPACE3_NAME  "Space3"
#define SPACE3_RANK	2
#define SPACE3_DIM1	15
#define SPACE3_DIM2	26

/****************************************************************
**
**  test_select_hyper(): Test basic H5S (dataspace) selection code.
**      Tests hyperslabs of various sizes and dimensionalities.
** 
****************************************************************/
static void 
test_select_hyper(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;	    /* Group ID			    */
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr;	    /* Attribute ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    uint8      *wbuf,       /* buffer to write to disk */
               *rbuf;       /* buffer read from disk */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=malloc(sizeof(uint8)*SPACE3_DIM1*SPACE3_DIM2);

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for memory */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_select_hyper() */

/****************************************************************
**
**  test_select(): Main H5S selection testing routine.
** 
****************************************************************/
void 
test_select(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Selections\n"));

    /* These next two tests use the same file information */
    test_select_hyper();        /* Test basic H5S hyperslab selection code */

}   /* test_select() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_select
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_select(void)
{
    remove(FILE);
}

