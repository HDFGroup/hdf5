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
* Test the Dataspace selection functionality
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

/* Element selection information */
#define POINT1_NPOINTS 10

/* Location comparison function */
int compare_size_t(const void *s1, const void *s2);

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
    hssize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    uint8      *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    H5S_class_t ext_type;   /* Extent type */

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

    /* Verify extent type */
    ext_type = H5Sget_simple_extent_type(sid1);
    VERIFY(ext_type, H5S_SIMPLE, "H5Sget_simple_extent_type");

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
**  test_select_point(): Test basic H5S (dataspace) selection code.
**      Tests element selections between dataspaces of various sizes
**      and dimensionalities.
** 
****************************************************************/
static void 
test_select_point(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hssize_t	coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hssize_t	coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    uint8      *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Element Selection Functions\n"));

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

    /* Create dataspace for write buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select sequence of ten points for disk dataset */
    coord1[0][0]=0; coord1[0][1]=10; coord1[0][2]= 5;
    coord1[1][0]=1; coord1[1][1]= 2; coord1[1][2]= 7;
    coord1[2][0]=2; coord1[2][1]= 4; coord1[2][2]= 9;
    coord1[3][0]=0; coord1[3][1]= 6; coord1[3][2]=11;
    coord1[4][0]=1; coord1[4][1]= 8; coord1[4][2]=13;
    coord1[5][0]=2; coord1[5][1]=12; coord1[5][2]= 0;
    coord1[6][0]=0; coord1[6][1]=14; coord1[6][2]= 2;
    coord1[7][0]=1; coord1[7][1]= 0; coord1[7][2]= 4;
    coord1[8][0]=2; coord1[8][1]= 1; coord1[8][2]= 6;
    coord1[9][0]=0; coord1[9][1]= 3; coord1[9][2]= 8;
    ret = H5Sselect_elements(sid1,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Select sequence of ten points for write dataset */
    coord2[0][0]=12; coord2[0][1]= 3;
    coord2[1][0]=15; coord2[1][1]=13;
    coord2[2][0]= 7; coord2[2][1]=25;
    coord2[3][0]= 0; coord2[3][1]= 6;
    coord2[4][0]=13; coord2[4][1]= 0;
    coord2[5][0]=24; coord2[5][1]=11;
    coord2[6][0]=12; coord2[6][1]=21;
    coord2[7][0]=29; coord2[7][1]= 4;
    coord2[8][0]= 8; coord2[8][1]= 8;
    coord2[9][0]=19; coord2[9][1]=17;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord2);
    CHECK(ret, FAIL, "H5Sselect_elements");

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

    /* Select sequence of points for read dataset */
    coord3[0][0]= 0; coord3[0][1]= 2;
    coord3[1][0]= 4; coord3[1][1]= 8;
    coord3[2][0]=13; coord3[2][1]=13;
    coord3[3][0]=14; coord3[3][1]=25;
    coord3[4][0]= 7; coord3[4][1]= 9;
    coord3[5][0]= 2; coord3[5][1]= 0;
    coord3[6][0]= 9; coord3[6][1]=19;
    coord3[7][0]= 1; coord3[7][1]=22;
    coord3[8][0]=12; coord3[8][1]=21;
    coord3[9][0]=11; coord3[9][1]= 6;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord3);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    for(i=0; i<POINT1_NPOINTS; i++) {
        tbuf=wbuf+(coord2[i][0]*SPACE2_DIM2)+coord2[i][1];
        tbuf2=rbuf+(coord3[i][0]*SPACE3_DIM2)+coord3[i][1];
        if(*tbuf!=*tbuf2) {
            printf("element values don't match!, i=%d\n",i);
        } /* end if */
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
}   /* test_select_point() */

/****************************************************************
**
**  test_select_all(): Test basic H5S (dataspace) selection code.
**      Tests "all" selections.
** 
****************************************************************/
static void 
test_select_all(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE3_DIM1, SPACE3_DIM2};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    uint8      *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    H5S_class_t ext_type;   /* Extent type */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 'All' Selection Functions\n"));

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
    sid1 = H5Screate_simple(SPACE3_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Verify extent type */
    ext_type = H5Sget_simple_extent_type(sid1);
    VERIFY(ext_type, H5S_SIMPLE, "H5Sget_simple_extent_type");

    /* Select entire 15x26 extent for disk dataset */
    ret = H5Sselect_all(sid1);
    CHECK(ret, FAIL, "H5Sselect_all");

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

    /* Select no extent for disk dataset */
    ret = H5Sselect_none(sid1);
    CHECK(ret, FAIL, "H5Sselect_all");

    /* Read selection from disk (should fail with no selection defined) */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    VERIFY(ret, FAIL, "H5Dread");

    /* Select entire 15x26 extent for disk dataset */
    ret = H5Sselect_all(sid1);
    CHECK(ret, FAIL, "H5Sselect_all");

    /* Read selection from disk (should work now) */
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
**  test_select_combo(): Test basic H5S (dataspace) selection code.
**      Tests combinations of element and hyperslab selections between
**      dataspaces of various sizes and dimensionalities.
** 
****************************************************************/
static void 
test_select_combo(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hssize_t    start[SPACE1_RANK];     /* Starting location of hyperslab */
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
    MESSAGE(5, ("Testing Combination of Hyperslab & Element Selection Functions\n"));

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

    /* Create dataspace for write buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select sequence of ten points for disk dataset */
    coord1[0][0]=0; coord1[0][1]=10; coord1[0][2]= 5;
    coord1[1][0]=1; coord1[1][1]= 2; coord1[1][2]= 7;
    coord1[2][0]=2; coord1[2][1]= 4; coord1[2][2]= 9;
    coord1[3][0]=0; coord1[3][1]= 6; coord1[3][2]=11;
    coord1[4][0]=1; coord1[4][1]= 8; coord1[4][2]=13;
    coord1[5][0]=2; coord1[5][1]=12; coord1[5][2]= 0;
    coord1[6][0]=0; coord1[6][1]=14; coord1[6][2]= 2;
    coord1[7][0]=1; coord1[7][1]= 0; coord1[7][2]= 4;
    coord1[8][0]=2; coord1[8][1]= 1; coord1[8][2]= 6;
    coord1[9][0]=0; coord1[9][1]= 3; coord1[9][2]= 8;
    ret = H5Sselect_elements(sid1,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Select 1x10 hyperslab for writing memory dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=1; count[1]=10;
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

    /* Select 10x1 hyperslab for reading memory dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=1;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    for(i=0; i<POINT1_NPOINTS; i++) {
        tbuf=wbuf+i;
        tbuf2=rbuf+(i*SPACE3_DIM2);
        if(*tbuf!=*tbuf2) {
            printf("element values don't match!, i=%d\n",i);
        } /* end if */
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
}   /* test_select_combo() */

int 
compare_size_t(const void *s1, const void *s2)
{
    if(*(const size_t *)s1<*(const size_t *)s2)
        return(-1);
    else
        if(*(const size_t *)s1>*(const size_t *)s2)
            return(1);
        else
            return(0);
}

/****************************************************************
**
**  test_select_hyper_stride(): Test H5S (dataspace) selection code.
**      Tests strided hyperslabs of various sizes and dimensionalities.
** 
****************************************************************/
static void 
test_select_hyper_stride(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    uint16     *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    size_t      loc1[72]={  /* Gruesomely ugly way to make certain hyperslab locations are checked correctly */
       27, 28, 29, 53, 54, 55, 79, 80, 81,   /* Block #1 */
       32, 33, 34, 58, 59, 60, 84, 85, 86,   /* Block #2 */
      157,158,159,183,184,185,209,210,211,   /* Block #3 */
      162,163,164,188,189,190,214,215,216,   /* Block #4 */
      287,288,289,313,314,315,339,340,341,   /* Block #5 */
      292,293,294,318,319,320,344,345,346,   /* Block #6 */
      417,418,419,443,444,445,469,470,471,   /* Block #7 */
      422,423,424,448,449,450,474,475,476,   /* Block #8 */
            };
    size_t      loc2[72]={
        0,  1,  2, 26, 27, 28,    /* Block #1 */
        4,  5,  6, 30, 31, 32,    /* Block #2 */
        8,  9, 10, 34, 35, 36,    /* Block #3 */
       12, 13, 14, 38, 39, 40,    /* Block #4 */
      104,105,106,130,131,132,    /* Block #5 */
      108,109,110,134,135,136,    /* Block #6 */
      112,113,114,138,139,140,    /* Block #7 */
      116,117,118,142,143,144,    /* Block #8 */
      208,209,210,234,235,236,    /* Block #9 */
      212,213,214,238,239,240,    /* Block #10 */
      216,217,218,242,243,244,    /* Block #11 */
      220,221,222,246,247,248,    /* Block #12 */
            };
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslabs with Strides Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint16)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint16),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint16)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 2x3x3 count with a stride of 2x4x3 & 1x2x2 block hyperslab for disk dataset */
    start[0]=0; start[1]=0; start[2]=0;
    stride[0]=2; stride[1]=4; stride[2]=3;
    count[0]=2; count[1]=3; count[2]=3;
    block[0]=1; block[1]=2; block[2]=2;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select 4x2 count with a stride of 5x5 & 3x3 block hyperslab for memory dataset */
    start[0]=1; start[1]=1;
    stride[0]=5; stride[1]=5;
    count[0]=4; count[1]=2;
    block[0]=3; block[1]=3;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_STD_U16LE,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_STD_U16LE,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for reading buffer */
    sid2 = H5Screate_simple(SPACE3_RANK, dims3, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 3x4 count with a stride of 4x4 & 2x3 block hyperslab for memory dataset */
    start[0]=0; start[1]=0;
    stride[0]=4; stride[1]=4;
    count[0]=3; count[1]=4;
    block[0]=2; block[1]=3;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_STD_U16LE,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Sort the locations into the proper order */
    qsort(loc1,72,sizeof(size_t),compare_size_t);
    qsort(loc2,72,sizeof(size_t),compare_size_t);
    /* Compare data read with data written out */
    for(i=0; i<72; i++) {
        tbuf=wbuf+loc1[i];
        tbuf2=rbuf+loc2[i];
        if(*tbuf!=*tbuf2) {
            printf("hyperslab values don't match!, loc1[%d]=%d, loc2[%d]=%d\n",i,(int)loc1[i],i,(int)loc2[i]);
#ifdef QAK
            printf("wbuf=%p, *tbuf=%p, rbuf=%p, tbuf2=%p\n",wbuf,tbuf,rbuf,tbuf2);
            printf("*tbuf=%d, *tbuf2=%d\n",(int)*tbuf,(int)*tbuf2);
#endif /* QAK */
        } /* end if */
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
**  test_select_hyper_copy(): Test H5S (dataspace) selection code.
**      Tests copying hyperslab selections
** 
****************************************************************/
static void 
test_select_hyper_copy(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		data1,data2;	/* Dataset IDs			*/
    hid_t		sid1,sid2,sid3; /* Dataspace IDs		*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    uint16     *wbuf,       /* buffer to write to disk */
               *rbuf,       /* 1st buffer read from disk */
               *rbuf2,      /* 2nd buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslabs with Strides Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint16)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint16),SPACE3_DIM1*SPACE3_DIM2);
    rbuf2=calloc(sizeof(uint16),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint16)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 2x3x3 count with a stride of 2x4x3 & 1x2x2 block hyperslab for disk dataset */
    start[0]=0; start[1]=0; start[2]=0;
    stride[0]=2; stride[1]=4; stride[2]=3;
    count[0]=2; count[1]=3; count[2]=3;
    block[0]=1; block[1]=2; block[2]=2;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select 4x2 count with a stride of 5x5 & 3x3 block hyperslab for memory dataset */
    start[0]=1; start[1]=1;
    stride[0]=5; stride[1]=5;
    count[0]=4; count[1]=2;
    block[0]=3; block[1]=3;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Make a copy of the dataspace to write */
    sid3 = H5Scopy(sid2);
    CHECK(sid3, FAIL, "H5Scopy");

    /* Create a dataset */
    data1=H5Dcreate(fid1,"Dataset1",H5T_STD_U16LE,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(data1,H5T_STD_U16LE,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create another dataset */
    data2=H5Dcreate(fid1,"Dataset2",H5T_STD_U16LE,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(data2,H5T_STD_U16LE,sid3,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close memory dataspace */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for reading buffer */
    sid2 = H5Screate_simple(SPACE3_RANK, dims3, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 3x4 count with a stride of 4x4 & 2x3 block hyperslab for memory dataset */
    start[0]=0; start[1]=0;
    stride[0]=4; stride[1]=4;
    count[0]=3; count[1]=4;
    block[0]=2; block[1]=3;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Make a copy of the dataspace to read */
    sid3 = H5Scopy(sid2);
    CHECK(sid3, FAIL, "H5Scopy");

    /* Read selection from disk */
    ret=H5Dread(data1,H5T_STD_U16LE,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Read selection from disk */
    ret=H5Dread(data2,H5T_STD_U16LE,sid3,sid1,H5P_DEFAULT,rbuf2);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    if(HDmemcmp(rbuf,rbuf2,sizeof(uint16)*SPACE3_DIM1*SPACE3_DIM2)) {
        printf("hyperslab values don't match!\n");
    } /* end if */

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close 2nd memory dataspace */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close Dataset */
    ret = H5Dclose(data1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataset */
    ret = H5Dclose(data2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(rbuf2);
}   /* test_select_hyper_copy() */

/****************************************************************
**
**  test_select_point_copy(): Test H5S (dataspace) selection code.
**      Tests copying point selections
** 
****************************************************************/
static void 
test_select_point_copy(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		data1,data2;	/* Dataset IDs			*/
    hid_t		sid1,sid2,sid3; /* Dataspace IDs		*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hssize_t	coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hssize_t	coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    uint16     *wbuf,       /* buffer to write to disk */
               *rbuf,       /* 1st buffer read from disk */
               *rbuf2,      /* 2nd buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslabs with Strides Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint16)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint16),SPACE3_DIM1*SPACE3_DIM2);
    rbuf2=calloc(sizeof(uint16),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint16)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select sequence of ten points for disk dataset */
    coord1[0][0]=0; coord1[0][1]=10; coord1[0][2]= 5;
    coord1[1][0]=1; coord1[1][1]= 2; coord1[1][2]= 7;
    coord1[2][0]=2; coord1[2][1]= 4; coord1[2][2]= 9;
    coord1[3][0]=0; coord1[3][1]= 6; coord1[3][2]=11;
    coord1[4][0]=1; coord1[4][1]= 8; coord1[4][2]=13;
    coord1[5][0]=2; coord1[5][1]=12; coord1[5][2]= 0;
    coord1[6][0]=0; coord1[6][1]=14; coord1[6][2]= 2;
    coord1[7][0]=1; coord1[7][1]= 0; coord1[7][2]= 4;
    coord1[8][0]=2; coord1[8][1]= 1; coord1[8][2]= 6;
    coord1[9][0]=0; coord1[9][1]= 3; coord1[9][2]= 8;
    ret = H5Sselect_elements(sid1,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Select sequence of ten points for write dataset */
    coord2[0][0]=12; coord2[0][1]= 3;
    coord2[1][0]=15; coord2[1][1]=13;
    coord2[2][0]= 7; coord2[2][1]=25;
    coord2[3][0]= 0; coord2[3][1]= 6;
    coord2[4][0]=13; coord2[4][1]= 0;
    coord2[5][0]=24; coord2[5][1]=11;
    coord2[6][0]=12; coord2[6][1]=21;
    coord2[7][0]=29; coord2[7][1]= 4;
    coord2[8][0]= 8; coord2[8][1]= 8;
    coord2[9][0]=19; coord2[9][1]=17;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord2);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Make a copy of the dataspace to write */
    sid3 = H5Scopy(sid2);
    CHECK(sid3, FAIL, "H5Scopy");

    /* Create a dataset */
    data1=H5Dcreate(fid1,"Dataset1",H5T_STD_U16LE,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(data1,H5T_STD_U16LE,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create another dataset */
    data2=H5Dcreate(fid1,"Dataset2",H5T_STD_U16LE,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(data2,H5T_STD_U16LE,sid3,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close memory dataspace */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for reading buffer */
    sid2 = H5Screate_simple(SPACE3_RANK, dims3, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select sequence of points for read dataset */
    coord3[0][0]= 0; coord3[0][1]= 2;
    coord3[1][0]= 4; coord3[1][1]= 8;
    coord3[2][0]=13; coord3[2][1]=13;
    coord3[3][0]=14; coord3[3][1]=25;
    coord3[4][0]= 7; coord3[4][1]= 9;
    coord3[5][0]= 2; coord3[5][1]= 0;
    coord3[6][0]= 9; coord3[6][1]=19;
    coord3[7][0]= 1; coord3[7][1]=22;
    coord3[8][0]=12; coord3[8][1]=21;
    coord3[9][0]=11; coord3[9][1]= 6;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord3);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Make a copy of the dataspace to read */
    sid3 = H5Scopy(sid2);
    CHECK(sid3, FAIL, "H5Scopy");

    /* Read selection from disk */
    ret=H5Dread(data1,H5T_STD_U16LE,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Read selection from disk */
    ret=H5Dread(data2,H5T_STD_U16LE,sid3,sid1,H5P_DEFAULT,rbuf2);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    if(HDmemcmp(rbuf,rbuf2,sizeof(uint16)*SPACE3_DIM1*SPACE3_DIM2)) {
        printf("hyperslab values don't match!\n");
    } /* end if */

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close 2nd memory dataspace */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close Dataset */
    ret = H5Dclose(data1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataset */
    ret = H5Dclose(data2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(rbuf2);
}   /* test_select_point_copy() */

/****************************************************************
**
**  test_select_hyper_offset(): Test basic H5S (dataspace) selection code.
**      Tests hyperslabs of various sizes and dimensionalities with selection
**      offsets.
** 
****************************************************************/
static void 
test_select_hyper_offset(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    hssize_t	offset[SPACE1_RANK];    /* Offset of selection */
    uint8      *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    hbool_t	    valid;		/* Generic boolean return value		*/
    H5S_class_t ext_type;   /* Extent type */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with Offsets\n"));

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

    /* Verify extent type */
    ext_type = H5Sget_simple_extent_type(sid1);
    VERIFY(ext_type, H5S_SIMPLE, "H5Sget_simple_extent_type");

    /* Select 2x15x13 hyperslab for disk dataset */
    start[0]=1; start[1]=0; start[2]=0;
    stride[0]=1; stride[1]=1; stride[2]=1;
    count[0]=2; count[1]=15; count[2]=13;
    block[0]=1; block[1]=1; block[2]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Check a valid offset */
    offset[0]=-1; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, TRUE, "H5Svalid_offset");

    /* Check an invalid offset */
    offset[0]=10; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, FALSE, "H5Svalid_offset");

    /* Reset offset */
    offset[0]=0; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, TRUE, "H5Svalid_offset");

    /* Select 15x26 hyperslab for memory dataset */
    start[0]=15; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=15; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Choose a valid offset for the memory dataspace */
    offset[0]=-10; offset[1]=0;
    ret = H5Soffset_simple(sid2,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid2);
    VERIFY(valid, TRUE, "H5Svalid_offset");

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
        tbuf=wbuf+((i+5)*SPACE2_DIM2);
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
}   /* test_select_hyper_offset() */

/****************************************************************
**
**  test_select_point_offset(): Test basic H5S (dataspace) selection code.
**      Tests element selections between dataspaces of various sizes
**      and dimensionalities with selection offsets.
** 
****************************************************************/
static void 
test_select_point_offset(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hssize_t	coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hssize_t	coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    hssize_t	offset[SPACE1_RANK];    /* Offset of selection */
    uint8      *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    hbool_t	    valid;		/* Generic boolean return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Element Selection Functions\n"));

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

    /* Create dataspace for write buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select sequence of ten points for disk dataset */
    coord1[0][0]=0; coord1[0][1]=10; coord1[0][2]= 5;
    coord1[1][0]=1; coord1[1][1]= 2; coord1[1][2]= 7;
    coord1[2][0]=2; coord1[2][1]= 4; coord1[2][2]= 9;
    coord1[3][0]=0; coord1[3][1]= 6; coord1[3][2]=11;
    coord1[4][0]=1; coord1[4][1]= 8; coord1[4][2]=12;
    coord1[5][0]=2; coord1[5][1]=12; coord1[5][2]= 0;
    coord1[6][0]=0; coord1[6][1]=14; coord1[6][2]= 2;
    coord1[7][0]=1; coord1[7][1]= 0; coord1[7][2]= 4;
    coord1[8][0]=2; coord1[8][1]= 1; coord1[8][2]= 6;
    coord1[9][0]=0; coord1[9][1]= 3; coord1[9][2]= 8;
    ret = H5Sselect_elements(sid1,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Check a valid offset */
    offset[0]=0; offset[1]=0; offset[2]=1;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, TRUE, "H5Svalid_offset");

    /* Check an invalid offset */
    offset[0]=10; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, FALSE, "H5Svalid_offset");

    /* Reset offset */
    offset[0]=0; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, TRUE, "H5Svalid_offset");

    /* Select sequence of ten points for write dataset */
    coord2[0][0]=12; coord2[0][1]= 3;
    coord2[1][0]=15; coord2[1][1]=13;
    coord2[2][0]= 7; coord2[2][1]=24;
    coord2[3][0]= 0; coord2[3][1]= 6;
    coord2[4][0]=13; coord2[4][1]= 0;
    coord2[5][0]=24; coord2[5][1]=11;
    coord2[6][0]=12; coord2[6][1]=21;
    coord2[7][0]=23; coord2[7][1]= 4;
    coord2[8][0]= 8; coord2[8][1]= 8;
    coord2[9][0]=19; coord2[9][1]=17;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord2);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Choose a valid offset for the memory dataspace */
    offset[0]=5; offset[1]=1;
    ret = H5Soffset_simple(sid2,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid2);
    VERIFY(valid, TRUE, "H5Svalid_offset");

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

    /* Select sequence of points for read dataset */
    coord3[0][0]= 0; coord3[0][1]= 2;
    coord3[1][0]= 4; coord3[1][1]= 8;
    coord3[2][0]=13; coord3[2][1]=13;
    coord3[3][0]=14; coord3[3][1]=25;
    coord3[4][0]= 7; coord3[4][1]= 9;
    coord3[5][0]= 2; coord3[5][1]= 0;
    coord3[6][0]= 9; coord3[6][1]=19;
    coord3[7][0]= 1; coord3[7][1]=22;
    coord3[8][0]=12; coord3[8][1]=21;
    coord3[9][0]=11; coord3[9][1]= 6;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord3);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    for(i=0; i<POINT1_NPOINTS; i++) {
        tbuf=wbuf+((coord2[i][0]+offset[0])*SPACE2_DIM2)+coord2[i][1]+offset[1];
        tbuf2=rbuf+(coord3[i][0]*SPACE3_DIM2)+coord3[i][1];
        if(*tbuf!=*tbuf2) {
            printf("element values don't match!, i=%d\n",i);
        } /* end if */
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
}   /* test_select_point() */

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

    /* These next tests use the same file */
    test_select_hyper();        /* Test basic H5S hyperslab selection code */
    test_select_point();        /* Test basic H5S element selection code */
    test_select_all();          /* Test basic all & none selection code */
    test_select_combo();        /* Test combined hyperslab & element selection code */
    test_select_hyper_stride(); /* Test strided hyperslab selection code */
    test_select_hyper_copy();   /* Test hyperslab selection copying code */
    test_select_point_copy();   /* Test point selection copying code */
    test_select_hyper_offset(); /* Test selection offset code with hyperslabs */
    test_select_point_offset(); /* Test selection offset code with elements */

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

