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

#define FILENAME   "tselect.h5"

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

/* 3-D dataset with fixed dimensions */
#define SPACE4_NAME  "Space4"
#define SPACE4_RANK	3
#define SPACE4_DIM1	11
#define SPACE4_DIM2	13
#define SPACE4_DIM3	17

/* Element selection information */
#define POINT1_NPOINTS 10

/* Location comparison function */
int compare_size_t(const void *s1, const void *s2);

herr_t test_select_hyper_iter1(void *elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *operator_data);
herr_t test_select_point_iter1(void *elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *operator_data);
herr_t test_select_all_iter1(void *elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *operator_data);
herr_t test_select_none_iter1(void *elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *operator_data);

/****************************************************************
**
**  test_select_hyper_iter1(): Iterator for checking hyperslab iteration
** 
****************************************************************/
herr_t 
test_select_hyper_iter1(void *_elem,hid_t UNUSED type_id, hsize_t UNUSED ndim, hssize_t UNUSED *point, void *_operator_data)
{
    uint8_t *tbuf=(uint8_t *)_elem,     /* temporary buffer pointer */
            **tbuf2=(uint8_t **)_operator_data; /* temporary buffer handle */

    if(*tbuf!=**tbuf2)
        return(-1);
    else {
        (*tbuf2)++;
        return(0);
    }
}   /* end test_select_hyper_iter1() */

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
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    H5S_class_t ext_type;   /* Extent type */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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

    /* Check that the values match with a dataset iterator */
    tbuf=wbuf+(15*SPACE2_DIM2);
    ret = H5Diterate(rbuf,H5T_NATIVE_UCHAR,sid2,test_select_hyper_iter1,&tbuf);
    CHECK(ret, FAIL, "H5Diterate");

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

struct pnt_iter {
    hssize_t	coord[POINT1_NPOINTS*2][SPACE2_RANK]; /* Coordinates for point selection */
    uint8_t *buf;           /* Buffer the points are in */
    intn offset;            /* Which point we are looking at */
};

/****************************************************************
**
**  test_select_point_iter1(): Iterator for checking point iteration
**  (This is really ugly code, not a very good example of correct usage - QAK)
** 
****************************************************************/
herr_t 
test_select_point_iter1(void *_elem,hid_t UNUSED type_id, hsize_t UNUSED ndim, hssize_t UNUSED *point, void *_operator_data)
{
    uint8_t *elem=(uint8_t *)_elem;  /* Pointer to the element to examine */
    uint8_t *tmp;                       /* temporary ptr to element in operator data */
    struct pnt_iter *pnt_info=(struct pnt_iter *)_operator_data;
    
    tmp=pnt_info->buf+(pnt_info->coord[pnt_info->offset][0]*SPACE2_DIM2)+pnt_info->coord[pnt_info->offset][1];
    if(*elem!=*tmp)
        return(-1);
    else {
        pnt_info->offset++;
        return(0);
    }
}   /* end test_select_hyper_iter1() */

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
    hssize_t	temp_coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hssize_t	coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hssize_t	temp_coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hssize_t	coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    hssize_t	temp_coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    struct pnt_iter pi;     /* Custom Pointer iterator struct */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Element Selection Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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

    /* Verify correct elements selected */
    H5Sget_select_elem_pointlist(sid1,0,POINT1_NPOINTS,(hsize_t *)temp_coord1);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord1[i][0],coord1[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord1[i][1],coord1[i][1],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord1[i][2],coord1[i][2],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = H5Sget_select_npoints(sid1);
    VERIFY(ret, 10, "H5Sget_select_npoints");

    /* Append another sequence of ten points to disk dataset */
    coord1[0][0]=0; coord1[0][1]= 2; coord1[0][2]= 0;
    coord1[1][0]=1; coord1[1][1]=10; coord1[1][2]= 8;
    coord1[2][0]=2; coord1[2][1]= 8; coord1[2][2]=10;
    coord1[3][0]=0; coord1[3][1]= 7; coord1[3][2]=12;
    coord1[4][0]=1; coord1[4][1]= 3; coord1[4][2]=11;
    coord1[5][0]=2; coord1[5][1]= 1; coord1[5][2]= 1;
    coord1[6][0]=0; coord1[6][1]=13; coord1[6][2]= 7;
    coord1[7][0]=1; coord1[7][1]=14; coord1[7][2]= 6;
    coord1[8][0]=2; coord1[8][1]= 2; coord1[8][2]= 5;
    coord1[9][0]=0; coord1[9][1]= 6; coord1[9][2]=13;
    ret = H5Sselect_elements(sid1,H5S_SELECT_APPEND,POINT1_NPOINTS,(const hssize_t **)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Verify correct elements selected */
    H5Sget_select_elem_pointlist(sid1,POINT1_NPOINTS,POINT1_NPOINTS,(hsize_t *)temp_coord1);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord1[i][0],coord1[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord1[i][1],coord1[i][1],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord1[i][2],coord1[i][2],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = H5Sget_select_npoints(sid1);
    VERIFY(ret, 20, "H5Sget_select_npoints");

    /* Select sequence of ten points for memory dataset */
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

    /* Verify correct elements selected */
    H5Sget_select_elem_pointlist(sid2,0,POINT1_NPOINTS,(hsize_t *)temp_coord2);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord2[i][0],coord2[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord2[i][1],coord2[i][1],"H5Sget_select_elem_pointlist");
    } /* end for */

    /* Save points for later iteration */
    /* (these are in the second half of the buffer, because we are prepending */
    /*  the next list of points to the beginning of the point selection list) */
    HDmemcpy(((char *)pi.coord)+sizeof(coord2),coord2,sizeof(coord2));

    ret = H5Sget_select_npoints(sid2);
    VERIFY(ret, 10, "H5Sget_select_npoints");

    /* Append another sequence of ten points to memory dataset */
    coord2[0][0]=24; coord2[0][1]= 0;
    coord2[1][0]= 2; coord2[1][1]=25;
    coord2[2][0]=13; coord2[2][1]=17;
    coord2[3][0]= 8; coord2[3][1]= 3;
    coord2[4][0]=29; coord2[4][1]= 4;
    coord2[5][0]=11; coord2[5][1]=14;
    coord2[6][0]= 5; coord2[6][1]=22;
    coord2[7][0]=12; coord2[7][1]= 2;
    coord2[8][0]=21; coord2[8][1]=12;
    coord2[9][0]= 9; coord2[9][1]=18;
    ret = H5Sselect_elements(sid2,H5S_SELECT_PREPEND,POINT1_NPOINTS,(const hssize_t **)coord2);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Verify correct elements selected */
    H5Sget_select_elem_pointlist(sid2,0,POINT1_NPOINTS,(hsize_t *)temp_coord2);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord2[i][0],coord2[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord2[i][1],coord2[i][1],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = H5Sget_select_npoints(sid2);
    VERIFY(ret, 20, "H5Sget_select_npoints");

    /* Save points for later iteration */
    HDmemcpy(pi.coord,coord2,sizeof(coord2));

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
    coord3[3][0]=14; coord3[3][1]=20;
    coord3[4][0]= 7; coord3[4][1]= 9;
    coord3[5][0]= 2; coord3[5][1]= 0;
    coord3[6][0]= 9; coord3[6][1]=19;
    coord3[7][0]= 1; coord3[7][1]=22;
    coord3[8][0]=12; coord3[8][1]=21;
    coord3[9][0]=11; coord3[9][1]= 6;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord3);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Verify correct elements selected */
    H5Sget_select_elem_pointlist(sid2,0,POINT1_NPOINTS,(hsize_t *)temp_coord3);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord3[i][0],coord3[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord3[i][1],coord3[i][1],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = H5Sget_select_npoints(sid2);
    VERIFY(ret, 10, "H5Sget_select_npoints");

    /* Append another sequence of ten points to disk dataset */
    coord3[0][0]=14; coord3[0][1]=25;
    coord3[1][0]= 0; coord3[1][1]= 0;
    coord3[2][0]=11; coord3[2][1]=11;
    coord3[3][0]= 5; coord3[3][1]=14;
    coord3[4][0]= 3; coord3[4][1]= 5;
    coord3[5][0]= 2; coord3[5][1]= 2;
    coord3[6][0]= 7; coord3[6][1]=13;
    coord3[7][0]= 9; coord3[7][1]=16;
    coord3[8][0]=12; coord3[8][1]=22;
    coord3[9][0]=13; coord3[9][1]= 9;
    ret = H5Sselect_elements(sid2,H5S_SELECT_APPEND,POINT1_NPOINTS,(const hssize_t **)coord3);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Verify correct elements selected */
    H5Sget_select_elem_pointlist(sid2,POINT1_NPOINTS,POINT1_NPOINTS,(hsize_t *)temp_coord3);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord3[i][0],coord3[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord3[i][1],coord3[i][1],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = H5Sget_select_npoints(sid2);
    VERIFY(ret, 20, "H5Sget_select_npoints");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Check that the values match with a dataset iterator */
    pi.buf=wbuf;
    pi.offset=0;
    ret = H5Diterate(rbuf,H5T_NATIVE_UCHAR,sid2,test_select_point_iter1,&pi);
    CHECK(ret, FAIL, "H5Diterate");

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
**  test_select_all_iter1(): Iterator for checking all iteration
**  
** 
****************************************************************/
herr_t 
test_select_all_iter1(void *_elem,hid_t UNUSED type_id, hsize_t UNUSED ndim, hssize_t UNUSED *point, void *_operator_data)
{
    uint8_t *tbuf=(uint8_t *)_elem,     /* temporary buffer pointer */
            **tbuf2=(uint8_t **)_operator_data; /* temporary buffer handle */

    if(*tbuf!=**tbuf2)
        return(-1);
    else {
        (*tbuf2)++;
        return(0);
    }
}   /* end test_select_all_iter1() */

/****************************************************************
**
**  test_select_none_iter1(): Iterator for checking none iteration
**      (This is never supposed to be called, so it always returns -1)
** 
****************************************************************/
herr_t 
test_select_none_iter1(void UNUSED *_elem,hid_t UNUSED type_id, hsize_t UNUSED ndim, hssize_t UNUSED *point, void UNUSED *_operator_data)
{
    return(-1);
}   /* end test_select_none_iter1() */

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
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    H5S_class_t ext_type;   /* Extent type */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 'All' Selection Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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

    /* Check that the values match with a dataset iterator */
    tbuf=wbuf+(15*SPACE2_DIM2);
    ret = H5Diterate(rbuf,H5T_NATIVE_UCHAR,sid2,test_select_all_iter1,&tbuf);
    CHECK(ret, FAIL, "H5Diterate");

    /* A quick check to make certain that iterating through a "none" selection works */
    ret = H5Sselect_none(sid2);
    CHECK(ret, FAIL, "H5Sselect_all");
    ret = H5Diterate(rbuf,H5T_NATIVE_UCHAR,sid2,test_select_none_iter1,&tbuf);
    CHECK(ret, FAIL, "H5Diterate");

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
}   /* test_select_all() */

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
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Combination of Hyperslab & Element Selection Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
    uint16_t   *wbuf,       /* buffer to write to disk */
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
    wbuf=malloc(sizeof(uint16_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint16_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
    ret=H5Dwrite(dataset,H5T_NATIVE_USHORT,sid2,sid1,H5P_DEFAULT,wbuf);
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
    ret=H5Dread(dataset,H5T_NATIVE_USHORT,sid2,sid1,H5P_DEFAULT,rbuf);
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
    uint16_t   *wbuf,       /* buffer to write to disk */
               *rbuf,       /* 1st buffer read from disk */
               *rbuf2,      /* 2nd buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslabs with Strides Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint16_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);
    rbuf2=calloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint16_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
    if(HDmemcmp(rbuf,rbuf2,sizeof(uint16_t)*SPACE3_DIM1*SPACE3_DIM2)) {
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
    uint16_t   *wbuf,       /* buffer to write to disk */
               *rbuf,       /* 1st buffer read from disk */
               *rbuf2,      /* 2nd buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslabs with Strides Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint16_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);
    rbuf2=calloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint16_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
    if(HDmemcmp(rbuf,rbuf2,sizeof(uint16_t)*SPACE3_DIM1*SPACE3_DIM2)) {
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
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    htri_t	    valid;		/* Generic boolean return value		*/
    H5S_class_t ext_type;   /* Extent type */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with Offsets\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
    VERIFY(valid, TRUE, "H5Sselect_valid");

    /* Check an invalid offset */
    offset[0]=10; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, FALSE, "H5Sselect_valid");

    /* Reset offset */
    offset[0]=0; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, TRUE, "H5Sselect_valid");

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
    VERIFY(valid, TRUE, "H5Sselect_valid");

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
                printf("%d: hyperslab values don't match!, i=%d, j=%d\n",__LINE__,i,j);
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
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    htri_t	    valid;		/* Generic boolean return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Element Selection Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
    VERIFY(valid, TRUE, "H5Sselect_valid");

    /* Check an invalid offset */
    offset[0]=10; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, FALSE, "H5Sselect_valid");

    /* Reset offset */
    offset[0]=0; offset[1]=0; offset[2]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, TRUE, "H5Sselect_valid");

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
    VERIFY(valid, TRUE, "H5Sselect_valid");

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
}   /* test_select_point_offset() */

/****************************************************************
**
**  test_select_hyper_union(): Test basic H5S (dataspace) selection code.
**      Tests unions of hyperslabs of various sizes and dimensionalities.
** 
****************************************************************/
static void 
test_select_hyper_union(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		xfer;	    /* Dataset Transfer Property List ID */
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    size_t      begin[SPACE2_DIM1]=     /* Offset within irregular block */
        {0,0,0,0,0,0,0,0,0,0,           /* First ten rows start at offset 0 */
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5}; /* Next eighteen rows start at offset 5 */
    size_t      len[SPACE2_DIM1]=       /* Len of each row within irregular block */
        {10,10,10,10,10,10,10,10,       /* First eight rows are 10 long */
         20,20,                         /* Next two rows are 20 long */
        15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15}; /* Next eighteen rows are 15 long */
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/
    hsize_t	    npoints;	/* Number of elements in selection */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with unions of hyperslabs\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

/* Test simple case of one block overlapping another */
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

    /* Select 8x26 hyperslab for memory dataset */
    start[0]=15; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=8; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Union overlapping 8x26 hyperslab for memory dataset (to form a 15x26 selection) */
    start[0]=22; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=8; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 15*26, "H5Sget_select_npoints");

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
                printf("hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",i,j,(int)*tbuf,(int)*tbuf2);
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

/* Test simple case of several block overlapping another */
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

    /* Select 8x15 hyperslab for memory dataset */
    start[0]=15; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=8; count[1]=15;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Union overlapping 8x15 hyperslab for memory dataset (to form a 15x15 selection) */
    start[0]=22; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=8; count[1]=15;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Union overlapping 15x15 hyperslab for memory dataset (to form a 15x26 selection) */
    start[0]=15; start[1]=11;
    stride[0]=1; stride[1]=1;
    count[0]=15; count[1]=15;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 15*26, "H5Sget_select_npoints");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset2",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

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
                printf("hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",i,j,(int)*tbuf,(int)*tbuf2);
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

/* Test disjoint case of two non-overlapping blocks */
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

    /* Select 7x26 hyperslab for memory dataset */
    start[0]=1; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=7; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Union non-overlapping 8x26 hyperslab for memory dataset (to form a 15x26 disjoint selection) */
    start[0]=22; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=8; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 15*26, "H5Sget_select_npoints");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset3",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

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
        /* Jump over gap in middle */
        if(i<7)
            tbuf=wbuf+((i+1)*SPACE2_DIM2);
        else
            tbuf=wbuf+((i+15)*SPACE2_DIM2);
        tbuf2=rbuf+(i*SPACE3_DIM2);
        for(j=0; j<SPACE3_DIM2; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2) {
                printf("hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",i,j,(int)*tbuf,(int)*tbuf2);
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

/* Test disjoint case of two non-overlapping blocks with hyperslab caching turned off */
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

    /* Select 7x26 hyperslab for memory dataset */
    start[0]=1; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=7; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Union non-overlapping 8x26 hyperslab for memory dataset (to form a 15x26 disjoint selection) */
    start[0]=22; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=8; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 15*26, "H5Sget_select_npoints");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset4",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    xfer = H5Pcreate (H5P_DATA_XFER);
    CHECK(xfer, FAIL, "H5Pcreate");

    ret = H5Pset_hyper_cache(xfer,0,1);
    CHECK(ret, FAIL, "H5Pset_hyper_cache");

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer,wbuf);
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
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Close transfer property list */
    ret = H5Pclose(xfer);
    CHECK(ret, FAIL, "H5Pclose");

    /* Compare data read with data written out */
    for(i=0; i<SPACE3_DIM1; i++) {
        /* Jump over gap in middle */
        if(i<7)
            tbuf=wbuf+((i+1)*SPACE2_DIM2);
        else
            tbuf=wbuf+((i+15)*SPACE2_DIM2);
        tbuf2=rbuf+(i*SPACE3_DIM2);
        for(j=0; j<SPACE3_DIM2; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2) {
                printf("hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",i,j,(int)*tbuf,(int)*tbuf2);
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

/* Test case of two blocks which overlap corners and must be split */
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

    /* Select 10x10 hyperslab for memory dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Union overlapping 15x20 hyperslab for memory dataset (forming a irregularly shaped region) */
    start[0]=8; start[1]=5;
    stride[0]=1; stride[1]=1;
    count[0]=20; count[1]=15;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 15*26, "H5Sget_select_npoints");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset5",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

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
    for(i=0,tbuf2=rbuf; i<SPACE2_DIM1; i++) {
        tbuf=wbuf+(i*SPACE2_DIM2)+begin[i];
        for(j=0; j<(intn)len[i]; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2) {
                printf("hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",i,j,(int)*tbuf,(int)*tbuf2);
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
}   /* test_select_hyper_union() */

/****************************************************************
**
**  test_select_hyper_union_3d(): Test basic H5S (dataspace) selection code.
**      Tests unions of hyperslabs in 3-D
** 
****************************************************************/
static void 
test_select_hyper_union_3d(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE4_DIM1, SPACE4_DIM2, SPACE4_DIM3};
    hsize_t		dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hssize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    struct row_list {
        size_t z;
        size_t y;
        size_t x;
        size_t l;
    } rows[]= {             /* Array of x,y,z coordinates & length for each row written from memory */
        {0,0,0,6},          /* 1st face of 3-D object */
        {0,1,0,6},
        {0,2,0,6},
        {0,3,0,6},
        {0,4,0,6},
        {1,0,0,6},          /* 2nd face of 3-D object */
        {1,1,0,6},
        {1,2,0,6},
        {1,3,0,6},
        {1,4,0,6},
        {2,0,0,6},          /* 3rd face of 3-D object */
        {2,1,0,10},
        {2,2,0,10},
        {2,3,0,10},
        {2,4,0,10},
        {2,5,2,8},
        {2,6,2,8},
        {3,0,0,6},          /* 4th face of 3-D object */
        {3,1,0,10},
        {3,2,0,10},
        {3,3,0,10},
        {3,4,0,10},
        {3,5,2,8},
        {3,6,2,8},
        {4,0,0,6},          /* 5th face of 3-D object */
        {4,1,0,10},
        {4,2,0,10},
        {4,3,0,10},
        {4,4,0,10},
        {4,5,2,8},
        {4,6,2,8},
        {5,1,2,8},          /* 6th face of 3-D object */
        {5,2,2,8},
        {5,3,2,8},
        {5,4,2,8},
        {5,5,2,8},
        {5,6,2,8},
        {6,1,2,8},          /* 7th face of 3-D object */
        {6,2,2,8},
        {6,3,2,8},
        {6,4,2,8},
        {6,5,2,8},
        {6,6,2,8},
        {7,1,2,8},          /* 8th face of 3-D object */
        {7,2,2,8},
        {7,3,2,8},
        {7,4,2,8},
        {7,5,2,8},
        {7,6,2,8}};
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    intn        i,j,k;      /* Counters */
    herr_t		ret;		/* Generic return value		*/
    hsize_t	    npoints;	/* Number of elements in selection */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with unions of 3-D hyperslabs\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE4_DIM1*SPACE4_DIM2*SPACE4_DIM3);
    rbuf=calloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE4_DIM1; i++)
        for(j=0; j<SPACE4_DIM2; j++)
            for(k=0; k<SPACE4_DIM3; k++)
                *tbuf++=(uint8_t)((((i*SPACE4_DIM2)+j)*SPACE4_DIM3)+k);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

/* Test case of two blocks which overlap corners and must be split */
    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE4_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 2x15x13 hyperslab for disk dataset */
    start[0]=1; start[1]=0; start[2]=0;
    stride[0]=1; stride[1]=1; stride[2]=1;
    count[0]=2; count[1]=15; count[2]=13;
    block[0]=1; block[1]=1; block[2]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select 5x5x6 hyperslab for memory dataset */
    start[0]=0; start[1]=0; start[2]=0;
    stride[0]=1; stride[1]=1; stride[2]=1;
    count[0]=5; count[1]=5; count[2]=6;
    block[0]=1; block[1]=1; block[2]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Union overlapping 15x20 hyperslab for memory dataset (forming a irregularly shaped region) */
    start[0]=2; start[1]=1; start[2]=2;
    stride[0]=1; stride[1]=1; stride[2]=1;
    count[0]=6; count[1]=6; count[2]=8;
    block[0]=1; block[1]=1; block[2]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 15*26, "H5Sget_select_npoints");

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
    for(i=0,tbuf2=rbuf; i<(intn)(sizeof(rows)/sizeof(struct row_list)); i++) {
        tbuf=wbuf+(rows[i].z*SPACE4_DIM3*SPACE4_DIM2)+(rows[i].y*SPACE4_DIM3)+rows[i].x;
        for(j=0; j<(intn)rows[i].l; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2) {
                printf("hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",i,j,(int)*tbuf,(int)*tbuf2);
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
}   /* test_select_hyper_union_3d() */

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
    test_select_point();        /* Test basic H5S element selection code, also tests appending to existing element selections */
    test_select_all();          /* Test basic all & none selection code */
    test_select_combo();        /* Test combined hyperslab & element selection code */
    test_select_hyper_stride(); /* Test strided hyperslab selection code */
    test_select_hyper_copy();   /* Test hyperslab selection copying code */
    test_select_point_copy();   /* Test point selection copying code */
    test_select_hyper_offset(); /* Test selection offset code with hyperslabs */
    test_select_point_offset(); /* Test selection offset code with elements */
    test_select_hyper_union();  /* Test hyperslab union code */
    test_select_hyper_union_3d();  /* Test hyperslab union code for 3-D dataset */

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
    remove(FILENAME);
}

