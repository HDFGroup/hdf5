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
* Test program:	 tvltypes
*
* Test the Variable-Length Datatype functionality
*
*************************************************************/

#include <testhdf5.h>

#include <hdf5.h>

#define FILE   "tvltypes.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

/* 2-D dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

/****************************************************************
**
**  test_vltypes_atomic(): Test basic VL datatype code.
**      Tests VL datatypes of atomic datatypes
** 
****************************************************************/
static void 
test_vltypes_atomic(void)
{
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    uintn       i,j;        /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic VL Datatype Functionality\n"));

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(uint32_t));
        wdata[i].len=i+1;
        for(j=0; j<(i+1); j++)
            ((uint32_t *)wdata[i].p)[j]=i*10+j;
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);
    CHECK(tid1, FAIL, "H5Tvlen_create");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read dataset from disk */
    ret=H5Dread(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(wdata[i].len!=rdata[i].len) {
            num_errs++;
            printf("VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
            continue;
        } /* end if */
        for(j=0; j<rdata[i].len; j++) {
            if( ((uint32_t *)wdata[i].p)[j] != ((uint32_t *)rdata[i].p)[j] ) {
                num_errs++;
                printf("VL data values don't match!, wdata[%d].p[%d]=%d, rdata[%d].p[%d]=%d\n",(int)i,(int)j, (int)((uint32_t *)wdata[i].p)[j], (int)i,(int)j, (int)((uint32_t *)rdata[i].p)[j]);
                continue;
            } /* end if */
        } /* end for */
    } /* end for */

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_vltypes_atomic() */

/****************************************************************
**
**  test_vltypes(): Main VL datatype testing routine.
** 
****************************************************************/
void 
test_vltypes(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Variable-Length Datatypes\n"));

    /* These next tests use the same file */
    test_vltypes_atomic();       /* Test basic VL datatype code */

}   /* test_vltypes() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_vltypes
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
cleanup_vltypes(void)
{
    remove(FILE);
}

