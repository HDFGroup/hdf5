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

/* 2-D dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

/* Element selection information */
#define POINT1_NPOINTS 10

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
    hid_t		dataset,	/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    hobj_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temp. buffer read from disk */
    uint32_t   *tu32;      /* Temporary pointer to uint32 data */
    intn        i;          /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Object Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    rbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    tbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);

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

    for(tu32=(uint32_t *)wbuf,i=0; i<SPACE1_DIM1; i++)
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
    dataset=H5Dcreate(fid1,"Dataset3",H5T_STD_REF_OBJ,sid1,H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate");

    /* Create references */
    ret = H5Rcreate(&wbuf[0],fid1,"/Dataset1",H5R_OBJECT,-1);
    CHECK(ret, FAIL, "H5Rcreate");

    ret = H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_OBJECT,-1);
    CHECK(ret, FAIL, "H5Rcreate");

    ret = H5Rcreate(&wbuf[2],fid1,"/Group1",H5R_OBJECT,-1);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);
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
    ret=H5Dread(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Try to open objects */
    dset2 = H5Rdereference(dataset,H5R_OBJECT,&rbuf[0]);
    CHECK(dset2, FAIL, "H5Rdereference");

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);
    CHECK(sid1, FAIL, "H5Dget_space");

    ret=H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, 4, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret=H5Dread(dset2,H5T_STD_U32LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,tbuf);
    CHECK(ret, FAIL, "H5Dread");

    for(tu32=(uint32_t *)tbuf,i=0; i<SPACE1_DIM1; i++,tu32++)
        VERIFY(*tu32, (uint32_t)(i*3), "Data");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");


    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(tbuf);
}   /* test_reference_obj() */

/****************************************************************
**
**  test_reference_region(): Test basic H5R (reference) object reference code.
**      Tests references to various kinds of objects
** 
****************************************************************/
static void 
test_reference_region(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dset1,	/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t		sid1,       /* Dataspace ID	#1		*/
                sid2;       /* Dataspace ID	#2		*/
    hsize_t		dims1[] = {SPACE1_DIM1},
            	dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hssize_t	start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE2_RANK];     /* Block size of hyperslab */
    hssize_t	coord1[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hdset_reg_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf;       /* buffer read from disk */
    uint8_t    *dwbuf,      /* Buffer for writing numeric data to disk */
               *drbuf;      /* Buffer for reading numeric data from disk */
    uint8_t    *tu8;        /* Temporary pointer to uint8 data */
    intn        i;          /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataset Region Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(hdset_reg_ref_t)*SPACE1_DIM1);
    rbuf=malloc(sizeof(hdset_reg_ref_t)*SPACE1_DIM1);
    dwbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    drbuf=calloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dset2=H5Dcreate(fid1,"Dataset2",H5T_STD_U8LE,sid2,H5P_DEFAULT);
    CHECK(dset2, FAIL, "H5Dcreate");

    for(tu8=dwbuf,i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++)
        *tu8++=i*3;

    /* Write selection to disk */
    ret=H5Dwrite(dset2,H5T_STD_U8LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,dwbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dset1=H5Dcreate(fid1,"Dataset1",H5T_STD_REF_DSETREG,sid1,H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Dcreate");

    /* Create references */

    /* Select 6x6 hyperslab for first reference */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=6; count[1]=6;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Store first dataset region */
    ret = H5Rcreate(&wbuf[0],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Select sequence of ten points for second reference */
    coord1[0][0]=6; coord1[0][1]=9;
    coord1[1][0]=2; coord1[1][1]=2;
    coord1[2][0]=8; coord1[2][1]=4;
    coord1[3][0]=1; coord1[3][1]=6;
    coord1[4][0]=2; coord1[4][1]=8;
    coord1[5][0]=3; coord1[5][1]=2;
    coord1[6][0]=0; coord1[6][1]=4;
    coord1[7][0]=9; coord1[7][1]=0;
    coord1[8][0]=7; coord1[8][1]=1;
    coord1[9][0]=3; coord1[9][1]=3;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Store second dataset region */
    ret = H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write selection to disk */
    ret=H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close Dataset */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close uint8 dataset dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dset1=H5Dopen(fid1,"/Dataset1");
    CHECK(dset1, FAIL, "H5Dopen");

    /* Read selection from disk */
    ret=H5Dread(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Try to open objects */
    dset2 = H5Rdereference(dset1,H5R_DATASET_REGION,&rbuf[0]);
    CHECK(dset2, FAIL, "H5Rdereference");

#ifdef LATER
    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);
    CHECK(sid1, FAIL, "H5Dget_space");

    ret=H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, 4, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret=H5Dread(dset2,H5T_STD_U32LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,drbuf);
    CHECK(ret, FAIL, "H5Dread");

    for(tu32=(uint32 *)drbuf,i=0; i<SPACE1_DIM1; i++,tu32++)
        VERIFY(*tu32, (uint32)(i*3), "Data");

#endif /* LATER */

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataset */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(dwbuf);
    free(drbuf);
}   /* test_reference_region() */

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
    test_reference_obj();       /* Test basic H5R object reference code */
    test_reference_region();    /* Test basic H5R dataset region reference code */

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

