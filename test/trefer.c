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
* Test program:	 trefer
*
* Test the Reference functionality
*
*************************************************************/

#include <testhdf5.h>

#include <hdf5.h>

#define FILE   "trefer.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

/****************************************************************
**
**  test_reference_obj(): Test basic H5R (reference) object reference code.
**      Tests references to various kinds of objects
** 
****************************************************************/
static void 
test_reference_obj(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    href_t      *wbuf,      /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temp. buffer read from disk */
    H5R_type_t  reftype;    /* Reference type created */
    uint32      *tu32;      /* Temporary pointer to uint32 data */
    intn        i;          /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Object Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(href_t)*SPACE1_DIM1);
    rbuf=malloc(sizeof(href_t)*SPACE1_DIM1);
    tbuf=malloc(sizeof(href_t)*SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a group */
    group=H5Gcreate(fid1,"Group1",-1);
    CHECK(group, FAIL, "H5Gcreate");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_STD_U32LE,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    for(tu32=(uint32 *)wbuf,i=0; i<SPACE1_DIM1; i++)
        *tu32++=i*3;

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_STD_U32LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset */
    dataset=H5Dcreate(fid1,"Dataset2",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset3",H5T_NATIVE_PTR_OBJ,sid1,H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate");

    /* Create references */
    ret = H5Rcreate(&wbuf[0],fid1,"/Dataset1",H5R_OBJECT,-1);
    CHECK(ret, FAIL, "H5Rcreate");
    reftype = H5Rget_type(&wbuf[0]);
    VERIFY(reftype, H5R_OBJECT, "H5Rget_type");

    ret = H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_OBJECT,-1);
    CHECK(ret, FAIL, "H5Rcreate");
    reftype = H5Rget_type(&wbuf[1]);
    VERIFY(reftype, H5R_OBJECT, "H5Rget_type");

    ret = H5Rcreate(&wbuf[2],fid1,"/Group1",H5R_OBJECT,-1);
    CHECK(ret, FAIL, "H5Rcreate");
    reftype = H5Rget_type(&wbuf[2]);
    VERIFY(reftype, H5R_OBJECT, "H5Rget_type");

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_PTR_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset=H5Dopen(fid1,"/Dataset3");
    CHECK(ret, FAIL, "H5Dcreate");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_PTR_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Check references */
    reftype = H5Rget_type(&rbuf[0]);
    VERIFY(reftype, H5R_OBJECT, "H5Rget_type");

    reftype = H5Rget_type(&rbuf[1]);
    VERIFY(reftype, H5R_OBJECT, "H5Rget_type");

    reftype = H5Rget_type(&rbuf[2]);
    VERIFY(reftype, H5R_OBJECT, "H5Rget_type");

    /* Try to open objects */
    dataset = H5Rdereference(&rbuf[0]);
    CHECK(dataset, FAIL, "H5Rdereference");

    /* Check information in referenced dataset */

    /* Read from disk */
    ret=H5Dread(dataset,H5T_STD_U32LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,tbuf);
    CHECK(ret, FAIL, "H5Dread");

    for(tu32=(uint32 *)tbuf,i=0; i<SPACE1_DIM1; i++,tu32++)
        VERIFY(*tu32, (uint32)(i*3), "Data");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");


    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
}   /* test_reference_obj() */

/****************************************************************
**
**  test_reference(): Main H5R reference testing routine.
** 
****************************************************************/
void 
test_reference(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing References\n"));

    /* These next tests use the same file */
    test_reference_obj();      /* Test basic H5R object reference code */

}   /* test_reference() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_reference
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              September 8, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_reference(void)
{
    remove(FILE);
}

