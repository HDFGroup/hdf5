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
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hsize_t		start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    uint8      *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 2x15x13 hyperslab for disk dataset */
    start[0]=1; start[1]=0; start[2]=0;
    stride[0]=1; stride[1]=1; stride[2]=1;
    count[0]=2; count[1]=15; count[2]=13;
    block[0]=1; block[1]=1; block[2]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select 15x26 hyperslab for memory dataset */
    start[0]=15; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=15; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for reading buffer */
    sid2 = H5Screate_simple(SPACE3_RANK, dims3, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 15x26 hyperslab for reading memory dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=15; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    for(i=0; i<SPACE3_DIM1; i++) {
        tbuf=wbuf+((i+15)*SPACE2_DIM2);
        tbuf2=rbuf+(i*SPACE3_DIM2);
        for(j=0; j<SPACE3_DIM2; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2) {
                printf("hyperslab values don't match!, i=%d, j=%d\n",i,j);
            } /* end if */
        } /* end for */
    } /* end for */

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
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

