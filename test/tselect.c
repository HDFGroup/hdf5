/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* $Id$ */

/***********************************************************
*
* Test program:	 tselect
*
* Test the Dataspace selection functionality
*
*************************************************************/

#include "testhdf5.h"

#include "hdf5.h"

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

/* Number of random hyperslabs to test */
#define NHYPERSLABS 10

/* Number of random hyperslab tests performed */
#define NRAND_HYPER 100

/* 5-D dataset with fixed dimensions */
#define SPACE5_NAME  "Space5"
#define SPACE5_RANK	5
#define SPACE5_DIM1	10
#define SPACE5_DIM2	10
#define SPACE5_DIM3	10
#define SPACE5_DIM4	10
#define SPACE5_DIM5	10

/* 1-D dataset with same size as 5-D dataset */
#define SPACE6_NAME  "Space6"
#define SPACE6_RANK	1
#define SPACE6_DIM1	(SPACE5_DIM1*SPACE5_DIM2*SPACE5_DIM3*SPACE5_DIM4*SPACE5_DIM5)

/* 4-D dataset with fixed dimensions */
#define SPACE7_NAME  "Space7"
#define SPACE7_RANK	4
#define SPACE7_DIM1	11
#define SPACE7_DIM2	13
#define SPACE7_DIM3	17
#define SPACE7_DIM4	19

/* 2-D dataset with easy dimensions */
#define SPACE8_NAME  "Space8"
#define SPACE8_RANK	2
#define SPACE8_DIM1	10
#define SPACE8_DIM2	10

/* Element selection information */
#define POINT1_NPOINTS 10

/* Chunked dataset information */
#define DATASETNAME "ChunkArray" 
#define NX_SUB   87                     /* hyperslab dimensions */ 
#define NY_SUB   61 
#define NZ_SUB  181 
#define NX       87                     /* output buffer dimensions */ 
#define NY       61 
#define NZ      181 
#define RANK_F     3                    /* File dataspace rank */
#define RANK_M     3                    /* Memory dataspace rank */
#define X    87                         /* dataset dimensions */
#define Y    61   
#define Z   181  
#define CHUNK_X   87                    /* chunk dimensions */
#define CHUNK_Y   61    
#define CHUNK_Z  181    

/* Location comparison function */
int compare_size_t(const void *s1, const void *s2);

herr_t test_select_hyper_iter1(void *elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *operator_data);
herr_t test_select_point_iter1(void *elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *operator_data);
herr_t test_select_all_iter1(void *elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *operator_data);
herr_t test_select_none_iter1(void *elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *operator_data);
herr_t test_select_hyper_iter2(void *_elem, hid_t type_id, hsize_t ndim, hssize_t *point, void *_operator_data);
herr_t test_select_hyper_iter3(void *_elem,hid_t type_id, hsize_t ndim, hssize_t *point, void *_operator_data);

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
test_select_hyper(hid_t xfer_plist)
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
    int        i,j;        /* Counters */
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
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,wbuf);
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
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,rbuf);
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
    int offset;            /* Which point we are looking at */
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
test_select_point(hid_t xfer_plist)
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
    int        i,j;        /* Counters */
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
    H5Sget_select_elem_pointlist(sid1,(hsize_t)0,(hsize_t)POINT1_NPOINTS,(hsize_t *)temp_coord1);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord1[i][0],coord1[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord1[i][1],coord1[i][1],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord1[i][2],coord1[i][2],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = (int)H5Sget_select_npoints(sid1);
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
    H5Sget_select_elem_pointlist(sid1,(hsize_t)POINT1_NPOINTS,(hsize_t)POINT1_NPOINTS,(hsize_t *)temp_coord1);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord1[i][0],coord1[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord1[i][1],coord1[i][1],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord1[i][2],coord1[i][2],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = (int)H5Sget_select_npoints(sid1);
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
    H5Sget_select_elem_pointlist(sid2,(hsize_t)0,(hsize_t)POINT1_NPOINTS,(hsize_t *)temp_coord2);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord2[i][0],coord2[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord2[i][1],coord2[i][1],"H5Sget_select_elem_pointlist");
    } /* end for */

    /* Save points for later iteration */
    /* (these are in the second half of the buffer, because we are prepending */
    /*  the next list of points to the beginning of the point selection list) */
    HDmemcpy(((char *)pi.coord)+sizeof(coord2),coord2,sizeof(coord2));

    ret = (int)H5Sget_select_npoints(sid2);
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
    H5Sget_select_elem_pointlist(sid2,(hsize_t)0,(hsize_t)POINT1_NPOINTS,(hsize_t *)temp_coord2);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord2[i][0],coord2[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord2[i][1],coord2[i][1],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = (int)H5Sget_select_npoints(sid2);
    VERIFY(ret, 20, "H5Sget_select_npoints");

    /* Save points for later iteration */
    HDmemcpy(pi.coord,coord2,sizeof(coord2));

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,wbuf);
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
    H5Sget_select_elem_pointlist(sid2,(hsize_t)0,(hsize_t)POINT1_NPOINTS,(hsize_t *)temp_coord3);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord3[i][0],coord3[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord3[i][1],coord3[i][1],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = (int)H5Sget_select_npoints(sid2);
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
    H5Sget_select_elem_pointlist(sid2,(hsize_t)POINT1_NPOINTS,(hsize_t)POINT1_NPOINTS,(hsize_t *)temp_coord3);
    for(i=0; i<POINT1_NPOINTS; i++) {
        VERIFY(temp_coord3[i][0],coord3[i][0],"H5Sget_select_elem_pointlist");
        VERIFY(temp_coord3[i][1],coord3[i][1],"H5Sget_select_elem_pointlist");
    } /* end for */

    ret = (int)H5Sget_select_npoints(sid2);
    VERIFY(ret, 20, "H5Sget_select_npoints");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,rbuf);
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
test_select_all(hid_t xfer_plist)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;	        /* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE4_DIM1, SPACE4_DIM2, SPACE4_DIM3};
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    int        i,j,k;      /* Counters */
    herr_t		ret;		/* Generic return value		*/
    H5S_class_t ext_type;   /* Extent type */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 'All' Selection Functions\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE4_DIM1*SPACE4_DIM2*SPACE4_DIM3);
    rbuf=calloc(sizeof(uint8_t),SPACE4_DIM1*SPACE4_DIM2*SPACE4_DIM3);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE4_DIM1; i++)
        for(j=0; j<SPACE4_DIM2; j++)
            for(k=0; k<SPACE4_DIM2; k++)
                *tbuf++=(uint8_t)(((i*SPACE4_DIM2)+j)*SPACE4_DIM3)+k;

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE4_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Verify extent type */
    ext_type = H5Sget_simple_extent_type(sid1);
    VERIFY(ext_type, H5S_SIMPLE, "H5Sget_simple_extent_type");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_INT,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,H5S_ALL,H5S_ALL,xfer_plist,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,H5S_ALL,H5S_ALL,xfer_plist,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Check that the values match with a dataset iterator */
    tbuf=wbuf;
    ret = H5Diterate(rbuf,H5T_NATIVE_UCHAR,sid1,test_select_all_iter1,&tbuf);
    CHECK(ret, FAIL, "H5Diterate");

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
**  test_select_all_hyper(): Test basic H5S (dataspace) selection code.
**      Tests "all" and hyperslab selections.
** 
****************************************************************/
static void 
test_select_all_hyper(hid_t xfer_plist)
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
    int        i,j;        /* Counters */
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
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,wbuf);
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
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,rbuf);
    VERIFY(ret, FAIL, "H5Dread");

    /* Select entire 15x26 extent for disk dataset */
    ret = H5Sselect_all(sid1);
    CHECK(ret, FAIL, "H5Sselect_all");

    /* Read selection from disk (should work now) */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,rbuf);
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
}   /* test_select_all_hyper() */

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
    int        i,j;        /* Counters */
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
test_select_hyper_stride(hid_t xfer_plist)
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
    int        i,j;        /* Counters */
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
    ret=H5Dwrite(dataset,H5T_NATIVE_USHORT,sid2,sid1,xfer_plist,wbuf);
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
    ret=H5Dread(dataset,H5T_NATIVE_USHORT,sid2,sid1,xfer_plist,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Sort the locations into the proper order */
    qsort(loc1,72,sizeof(size_t),compare_size_t);
    qsort(loc2,72,sizeof(size_t),compare_size_t);
    /* Compare data read with data written out */
    for(i=0; i<72; i++) {
        tbuf=wbuf+loc1[i];
        tbuf2=rbuf+loc2[i];
        if(*tbuf!=*tbuf2) {
            printf("%d: hyperslab values don't match!, loc1[%d]=%d, loc2[%d]=%d\n",__LINE__,i,(int)loc1[i],i,(int)loc2[i]);
#ifndef QAK
            printf("wbuf=%p, tbuf=%p, rbuf=%p, tbuf2=%p\n",wbuf,tbuf,rbuf,tbuf2);
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
}   /* test_select_hyper_stride() */

/****************************************************************
**
**  test_select_hyper_contig(): Test H5S (dataspace) selection code.
**      Tests contiguous hyperslabs of various sizes and dimensionalities.
** 
****************************************************************/
static void 
test_select_hyper_contig(hid_t dset_type, hid_t xfer_plist)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims2[] = {SPACE2_DIM2, SPACE2_DIM1};
    hssize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE1_RANK];     /* Block size of hyperslab */
    uint16_t   *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    int        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Contiguous Hyperslabs Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint16_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint16_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint16_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 12x10 count with a stride of 1x3 & 3x3 block hyperslab for disk dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=3;
    count[0]=12; count[1]=10;
    block[0]=1; block[1]=3;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select 4x5 count with a stride of 3x6 & 3x6 block hyperslab for memory dataset */
    start[0]=0; start[1]=0;
    stride[0]=3; stride[1]=6;
    count[0]=4; count[1]=5;
    block[0]=3; block[1]=6;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",dset_type,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_USHORT,sid2,sid1,xfer_plist,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for reading buffer */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 6x5 count with a stride of 2x6 & 2x6 block hyperslab for disk dataset */
    start[0]=0; start[1]=0;
    stride[0]=2; stride[1]=6;
    count[0]=6; count[1]=5;
    block[0]=2; block[1]=6;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select 3x15 count with a stride of 4x2 & 4x2 block hyperslab for memory dataset */
    start[0]=0; start[1]=0;
    stride[0]=4; stride[1]=2;
    count[0]=3; count[1]=15;
    block[0]=4; block[1]=2;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_USHORT,sid2,sid1,xfer_plist,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    if(HDmemcmp(rbuf,wbuf,sizeof(uint16_t)*30*12)) {
        num_errs++;
        printf("Error: hyperslab values don't match!\n");
#ifdef QAK
        for(i=0, tbuf=wbuf; i<12; i++)
            for(j=0; j<30; j++)
                printf("i=%d, j=%d, *wbuf=%u, *rbuf=%u\n",i,j,(unsigned)*(wbuf+i*30+j),(unsigned)*(rbuf+i*30+j));
#endif /* QAK */
    } /* end if */

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
}   /* test_select_hyper_contig() */

/****************************************************************
**
**  test_select_hyper_contig2(): Test H5S (dataspace) selection code.
**      Tests more contiguous hyperslabs of various sizes and dimensionalities.
** 
****************************************************************/
static void 
test_select_hyper_contig2(hid_t dset_type, hid_t xfer_plist)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims2[] = {SPACE7_DIM4, SPACE7_DIM3, SPACE7_DIM2, SPACE7_DIM1};
    hssize_t	        start[SPACE7_RANK];     /* Starting location of hyperslab */
    hsize_t		count[SPACE7_RANK];     /* Element count of hyperslab */
    uint16_t   *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    int        i,j,k,l;     /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing More Contiguous Hyperslabs Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint16_t)*SPACE7_DIM1*SPACE7_DIM2*SPACE7_DIM3*SPACE7_DIM4);
    rbuf=calloc(sizeof(uint16_t),SPACE7_DIM1*SPACE7_DIM2*SPACE7_DIM3*SPACE7_DIM4);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            for(k=0; k<SPACE7_DIM3; k++)
                for(l=0; l<SPACE7_DIM4; l++)
                    *tbuf++=(uint16_t)((i*SPACE7_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE7_RANK, dims2, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE7_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select contiguous hyperslab for disk dataset */
    start[0]=0; start[1]=0; start[2]=0; start[3]=0;
    count[0]=2; count[1]=SPACE7_DIM3; count[2]=SPACE7_DIM2; count[3]=SPACE7_DIM1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select contiguous hyperslab in memory */
    start[0]=0; start[1]=0; start[2]=0; start[3]=0;
    count[0]=2; count[1]=SPACE7_DIM3; count[2]=SPACE7_DIM2; count[3]=SPACE7_DIM1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",dset_type,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_USHORT,sid2,sid1,xfer_plist,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close memory dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for reading buffer */
    sid2 = H5Screate_simple(SPACE7_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 6x5 count with a stride of 2x6 & 2x6 block hyperslab for disk dataset */
    start[0]=0; start[1]=0; start[2]=0; start[3]=0;
    count[0]=2; count[1]=SPACE7_DIM3; count[2]=SPACE7_DIM2; count[3]=SPACE7_DIM1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select 3x15 count with a stride of 4x2 & 4x2 block hyperslab for memory dataset */
    start[0]=0; start[1]=0; start[2]=0; start[3]=0;
    count[0]=2; count[1]=SPACE7_DIM3; count[2]=SPACE7_DIM2; count[3]=SPACE7_DIM1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_USHORT,sid2,sid1,xfer_plist,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    if(HDmemcmp(rbuf,wbuf,sizeof(uint16_t)*2*SPACE7_DIM3*SPACE7_DIM2*SPACE7_DIM1)) {
        num_errs++;
        printf("Error: hyperslab values don't match!\n");
#ifdef QAK
        for(i=0, tbuf=wbuf; i<12; i++)
            for(j=0; j<30; j++)
                printf("i=%d, j=%d, *wbuf=%u, *rbuf=%u\n",i,j,(unsigned)*(wbuf+i*30+j),(unsigned)*(rbuf+i*30+j));
#endif /* QAK */
    } /* end if */

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
}   /* test_select_hyper_contig2() */

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
    int        i,j;        /* Counters */
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
    int        i,j;        /* Counters */
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
    int        i,j;        /* Counters */
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
**  test_select_hyper_offset2(): Test basic H5S (dataspace) selection code.
**      Tests optimized hyperslab I/O with selection offsets.
** 
****************************************************************/
static void 
test_select_hyper_offset2(void)
{
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid1,sid2;	/* Dataspace ID			*/
    hsize_t	dims1[] = {SPACE8_DIM1, SPACE8_DIM2};
    hsize_t	dims2[] = {SPACE8_DIM1, SPACE8_DIM2};
    hssize_t	start[SPACE8_RANK];     /* Starting location of hyperslab */
    hsize_t	count[SPACE8_RANK];     /* Element count of hyperslab */
    hssize_t	offset[SPACE8_RANK];    /* Offset of selection */
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    int         i,j;        /* Counters */
    herr_t	ret;        /* Generic return value */
    htri_t	valid;      /* Generic boolean return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing More Hyperslab Selection Functions with Offsets\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE8_DIM1*SPACE8_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE8_DIM1*SPACE8_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE8_DIM1; i++)
        for(j=0; j<SPACE8_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE8_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE8_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE8_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 4x10 hyperslab for disk dataset */
    start[0]=1; start[1]=0;
    count[0]=4; count[1]=10;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Set offset */
    offset[0]=1; offset[1]=0;
    ret = H5Soffset_simple(sid1,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid1);
    VERIFY(valid, TRUE, "H5Sselect_valid");

    /* Select 4x10 hyperslab for memory dataset */
    start[0]=1; start[1]=0;
    count[0]=4; count[1]=10;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Choose a valid offset for the memory dataspace */
    offset[0]=2; offset[1]=0;
    ret = H5Soffset_simple(sid2,offset);
    CHECK(ret, FAIL, "H5Soffset_simple");
    valid = H5Sselect_valid(sid2);
    VERIFY(valid, TRUE, "H5Sselect_valid");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    for(i=0; i<4; i++) {
        tbuf=wbuf+((i+3)*SPACE8_DIM2);
        tbuf2=rbuf+((i+3)*SPACE8_DIM2);
        for(j=0; j<SPACE8_DIM2; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2) {
                printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%u, *tbuf2=%u\n",__LINE__,i,j,(unsigned)*tbuf,(unsigned)*tbuf2);
                num_errs++;
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
}   /* test_select_hyper_offset2() */

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
    int        i,j;        /* Counters */
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
    int        i,j;        /* Counters */
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
                printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
                printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
                printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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

    xfer = H5Pcreate (H5P_DATASET_XFER);
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
                printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
        for(j=0; j<(int)len[i]; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2) {
                printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
**  test_select_hyper_union_stagger(): Test basic H5S (dataspace) selection code.
**      Tests unions of staggered hyperslabs
** 
****************************************************************/
static void 
test_select_hyper_union_stagger(void)
{
    hid_t file_id;      /* File ID */
    hid_t dset_id;      /* Dataset ID */
    hid_t dataspace;    /* File dataspace ID */
    hid_t memspace;     /* Memory dataspace ID */
    hsize_t dimsm[2]={7,7}; /* Memory array dimensions */
    hsize_t dimsf[2]={6,5}; /* File array dimensions */
    hsize_t count[2]={3,1}; /* 1st Hyperslab size */
    hsize_t count2[2]={3,1}; /* 2nd Hyperslab size */
    hsize_t count3[2]={2,1}; /* 3rd Hyperslab size */
    hssize_t offset[2]={0,0}; /* 1st Hyperslab offset */
    hssize_t offset2[2]={2,1}; /* 2nd Hyperslab offset */
    hssize_t offset3[2]={4,2}; /* 3rd Hyperslab offset */
    hsize_t count_out[2]={4,2}; /* Hyperslab size in memory */
    hssize_t offset_out[2]={0,3}; /* Hyperslab offset in memory */
    int data[6][5];     /* Data to write */
    int data_out[7][7]; /* Data read in */
    int input_loc[8][2]={{0,0},
                         {1,0},
                         {2,0},
                         {2,1},
                         {3,1},
                         {4,1},
                         {4,2},
                         {5,2}};
    int output_loc[8][2]={{0,3},
                         {0,4},
                         {1,3},
                         {1,4},
                         {2,3},
                         {2,4},
                         {3,3},
                         {3,4}};
    int dsetrank=2;     /* File Dataset rank */
    int memrank=2;      /* Memory Dataset rank */
    int i,j;            /* Local counting variables */
    herr_t error;
    hsize_t stride[2]={1,1};
    hsize_t block[2]={1,1};

    /* Initialize data to write */
    for(i=0; i<6; i++)
        for(j=0; j<5; j++)
           data[i][j] = j*10 + i;

    /* Create file */
    file_id=H5Fcreate(FILENAME,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fcreate");

    /* Create File Dataspace */
    dataspace=H5Screate_simple(dsetrank,dimsf,NULL);
    CHECK(dataspace, FAIL, "H5Screate_simple");

    /* Create File Dataset */
    dset_id=H5Dcreate(file_id,"IntArray",H5T_NATIVE_INT,dataspace,H5P_DEFAULT);
    CHECK(dset_id, FAIL, "H5Dcreate");

    /* Write File Dataset */
    error=H5Dwrite(dset_id,H5T_NATIVE_INT,dataspace,dataspace,H5P_DEFAULT,data);
    CHECK(error, FAIL, "H5Dwrite");

    /* Close things */
    error=H5Sclose(dataspace);
    CHECK(error, FAIL, "H5Sclose");
    error=H5Dclose(dset_id);
    CHECK(error, FAIL, "H5Dclose");
    error=H5Fclose(file_id);
    CHECK(error, FAIL, "H5Fclose");

    /* Initialize intput buffer */
    memset(data_out,0,7*7*sizeof(int));

    /* Open file */
    file_id=H5Fopen(FILENAME,H5F_ACC_RDONLY,H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Open dataset */
    dset_id=H5Dopen(file_id,"IntArray");
    CHECK(dset_id, FAIL, "H5Dopen");

    /* Get the dataspace */
    dataspace=H5Dget_space(dset_id);
    CHECK(dataspace, FAIL, "H5Dget_space");

    /* Select the hyperslabs */
    error=H5Sselect_hyperslab(dataspace,H5S_SELECT_SET,offset,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");
    error=H5Sselect_hyperslab(dataspace,H5S_SELECT_OR,offset2,stride,count2,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");
    error=H5Sselect_hyperslab(dataspace,H5S_SELECT_OR,offset3,stride,count3,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Create Memory Dataspace */
    memspace=H5Screate_simple(memrank,dimsm,NULL);
    CHECK(memspace, FAIL, "H5Screate_simple");

    /* Select hyperslab in memory */
    error=H5Sselect_hyperslab(memspace,H5S_SELECT_SET,offset_out,stride,count_out,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");
    
    /* Read File Dataset */
    error=H5Dread(dset_id,H5T_NATIVE_INT,memspace,dataspace,H5P_DEFAULT,data_out);
    CHECK(error, FAIL, "H5Dread");

    /* Verify input data */
    for(i=0; i<8; i++) {
        if(data[input_loc[i][0]][input_loc[i][1]]!=data_out[output_loc[i][0]][output_loc[i][1]]) {
            printf("input data #%d is wrong!\n",i);
            printf("input_loc=[%d][%d]\n",input_loc[i][0],input_loc[i][1]);
            printf("output_loc=[%d][%d]\n",output_loc[i][0],output_loc[i][1]);
            printf("data=%d\n",data[input_loc[i][0]][input_loc[i][1]]);
            printf("data_out=%d\n",data_out[output_loc[i][0]][output_loc[i][1]]);
            num_errs++;
        } /* end if */
    } /* end for */

    /* Close things */
    error=H5Sclose(dataspace);
    CHECK(error, FAIL, "H5Sclose");
    error=H5Sclose(memspace);
    CHECK(error, FAIL, "H5Sclose");
    error=H5Dclose(dset_id);
    CHECK(error, FAIL, "H5Dclose");
    error=H5Fclose(file_id);
    CHECK(error, FAIL, "H5Fclose");
}

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
    int        i,j,k;      /* Counters */
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
    for(i=0,tbuf2=rbuf; i<(int)(sizeof(rows)/sizeof(struct row_list)); i++) {
        tbuf=wbuf+(rows[i].z*SPACE4_DIM3*SPACE4_DIM2)+(rows[i].y*SPACE4_DIM3)+rows[i].x;
        for(j=0; j<(int)rows[i].l; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2) {
                printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
**  test_select_hyper_iter2(): Iterator for checking hyperslab iteration
** 
****************************************************************/
herr_t 
test_select_hyper_iter2(void *_elem, hid_t UNUSED type_id, hsize_t ndim, hssize_t *point, void *_operator_data)
{
    int *tbuf=(int *)_elem,     /* temporary buffer pointer */
        **tbuf2=(int **)_operator_data; /* temporary buffer handle */
    unsigned u;             /* Local counting variable */

    if(*tbuf!=**tbuf2) {
        num_errs++;
        printf("Error in hyperslab iteration!\n");
        printf("location: { ");
        for(u=0; u<(unsigned)ndim; u++) {
            printf("%2d",(int)point[u]);
            if(u<(unsigned)(ndim-1))
                printf(", ");
        } /* end for */
        printf("}\n");
        printf("*tbuf=%d, **tbuf2==%d\n",*tbuf,**tbuf2);
        return(-1);
    } /* end if */
    else {
        (*tbuf2)++;
        return(0);
    }
}   /* end test_select_hyper_iter1() */

/****************************************************************
**
**  test_select_hyper_union_random_5d(): Test basic H5S (dataspace) selection code.
**      Tests random unions of 5-D hyperslabs
** 
****************************************************************/
static void 
test_select_hyper_union_random_5d(hid_t read_plist)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE5_DIM1, SPACE5_DIM2, SPACE5_DIM3, SPACE5_DIM4, SPACE5_DIM5};
    hsize_t		dims2[] = {SPACE6_DIM1};
    hssize_t	start[SPACE5_RANK];     /* Starting location of hyperslab */
    hsize_t		count[SPACE5_RANK];     /* Element count of hyperslab */
    int    *wbuf,          /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    int        i,j,k,l,m;  /* Counters */
    herr_t		ret;		/* Generic return value		*/
    hssize_t	npoints,	/* Number of elements in file selection */
                npoints2;	/* Number of elements in memory selection */
    unsigned    seed;       /* Random number seed for each test */
    unsigned    test_num;   /* Count of tests being executed */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with random unions of 5-D hyperslabs\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(int)*SPACE5_DIM1*SPACE5_DIM2*SPACE5_DIM3*SPACE5_DIM4*SPACE5_DIM5);
    rbuf=calloc(sizeof(int),SPACE5_DIM1*SPACE5_DIM2*SPACE5_DIM3*SPACE5_DIM4*SPACE5_DIM5);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE5_DIM1; i++)
        for(j=0; j<SPACE5_DIM2; j++)
            for(k=0; k<SPACE5_DIM3; k++)
                for(l=0; l<SPACE5_DIM4; l++)
                    for(m=0; m<SPACE5_DIM5; m++)
                        *tbuf++=(int)(((((((i*SPACE4_DIM2)+j)*SPACE4_DIM3)+k)*SPACE5_DIM4)+l)*SPACE5_DIM5)+m;

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE5_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_INT,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Write entire dataset to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Create dataspace for reading buffer */
    sid2 = H5Screate_simple(SPACE6_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Get initial random # seed */
    seed=(unsigned)time(NULL)+(unsigned)clock();

    /* Crunch through a bunch of random hyperslab reads from the file dataset */
    for(test_num=0; test_num<NRAND_HYPER; test_num++) {
        /* Save random # seed for later use */
        /* (Used in case of errors, to regenerate the hyperslab sequence) */
        seed+=(unsigned)clock();
        srand(seed);

#ifdef QAK
printf("test_num=%d, seed=%u\n",test_num,seed);
#endif /* QAK */
        for(i=0; i<NHYPERSLABS; i++) {
#ifdef QAK
printf("hyperslab=%d\n",i);
#endif /* QAK */
            /* Select random hyperslab location & size for selection */
            for(j=0; j<SPACE5_RANK; j++) {
                start[j]=rand()%dims1[j];
                count[j]=(rand()%(dims1[j]-start[j]))+1;
#ifdef QAK
printf("start[%d]=%d, count[%d]=%d\n",j,(int)start[j],j,(int)count[j]);
#endif /* QAK */
            } /* end for */

            /* Select hyperslab */
            ret = H5Sselect_hyperslab(sid1,(i==0 ? H5S_SELECT_SET : H5S_SELECT_OR),start,NULL,count,NULL);
            CHECK(ret, FAIL, "H5Sselect_hyperslab");
        } /* end for */

        /* Get the number of elements selected */
        npoints=H5Sget_select_npoints(sid1);
        CHECK(npoints, 0, "H5Sget_select_npoints");

        /* Select linear 1-D hyperslab for memory dataset */
        start[0]=0;
        count[0]=npoints;
        ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,NULL,count,NULL);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        npoints2 = H5Sget_select_npoints(sid2);
        VERIFY(npoints, npoints2, "H5Sget_select_npoints");

#ifdef QAK
printf("random I/O, before H5Dread()\n");
{
    hsize_t blocks[15][2][SPACE5_RANK];
    hssize_t nblocks;
    int k;

    nblocks=H5Sget_select_hyper_nblocks(sid1);
    printf("nblocks=%d\n",(int)nblocks);
    H5Sget_select_hyper_blocklist(sid1,0,nblocks,blocks);
    for(j=0; j<nblocks; j++) {
        printf("Block #%d, start = {",j);
        for(k=0; k<SPACE5_RANK; k++) {
            printf("%d",blocks[j][0][k]);
            if(k<(SPACE5_RANK-1))
                printf(", ");
            else
                printf("}, end = {");
        } /* end for */
        for(k=0; k<SPACE5_RANK; k++) {
            printf("%d",blocks[j][1][k]);
            if(k<(SPACE5_RANK-1))
                printf(", ");
            else
                printf("}\n");
        } /* end for */
    } /* end for */
}
#endif /* QAK */
        /* Read selection from disk */
        ret=H5Dread(dataset,H5T_NATIVE_INT,sid2,sid1,read_plist,rbuf);
        CHECK(ret, FAIL, "H5Dread");
#ifdef QAK
printf("random I/O, after H5Dread()\n");
#endif /* QAK */

        /* Compare data read with data written out */
        tbuf=rbuf;
        ret = H5Diterate(wbuf,H5T_NATIVE_INT,sid1,test_select_hyper_iter2,&tbuf);
        if(ret<0) {
            num_errs++;
            printf("Random hyperslabs for seed %u failed!\n",seed);
        }

        /* Set the read buffer back to all zeroes */
        memset(rbuf,0,SPACE6_DIM1);
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
}   /* test_select_hyper_union_random_5d() */

/****************************************************************
**
**  test_select_hyper_chunk(): Test basic H5S (dataspace) selection code.
**      Tests large hyperslab selection in chunked dataset
** 
****************************************************************/
static void 
test_select_hyper_chunk(hid_t fapl_plist, hid_t xfer_plist)
{
    hsize_t     dimsf[3];              /* dataset dimensions */
    hsize_t     chunk_dimsf[3] = {CHUNK_X, CHUNK_Y, CHUNK_Z};    /* chunk sizes */
    short      *data;                   /* data to write */
    short      *tmpdata;                /* data to write */

    /* 
     * Data  and output buffer initialization. 
     */
    hid_t       file, dataset;         /* handles */
    hid_t       dataspace;   
    hid_t       memspace; 
    hid_t       plist;                 
    hsize_t     dimsm[3];              /* memory space dimensions */
    hsize_t     dims_out[3];           /* dataset dimensions */      
    herr_t      status;                             

    short         *data_out; /* output buffer */
    short         *tmpdata_out; /* output buffer */
   
    hsize_t     count[3];              /* size of the hyperslab in the file */
    hssize_t    offset[3];             /* hyperslab offset in the file */
    hsize_t     count_out[3];          /* size of the hyperslab in memory */
    hssize_t    offset_out[3];         /* hyperslab offset in memory */
    int         i, j, k, status_n, rank;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab I/O on Large Chunks\n"));

    /* Allocate the transfer buffers */
    data = (short*)malloc(sizeof(short)*X*Y*Z);
    data_out = (short*)calloc(NX*NY*NZ,sizeof(short));
 
    /* 
     * Data buffer initialization. 
     */
    tmpdata = data;
    for (j = 0; j < X; j++)
	for (i = 0; i < Y; i++)
	    for (k = 0; k < Z; k++)
                *tmpdata++ =  (short)((k+1)%256);

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * the default file creation properties, and the default file
     * access properties.
     */
    file = H5Fcreate (FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_plist);
    CHECK(file, FAIL, "H5Fcreate");

    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset. 
     */
    dimsf[0] = X;
    dimsf[1] = Y;
    dimsf[2] = Z;
    dataspace = H5Screate_simple (RANK_F, dimsf, NULL); 
    CHECK(dataspace, FAIL, "H5Screate_simple");

    /*
     * Create a new dataset within the file using defined dataspace and
     * chunking properties.
     */
    plist = H5Pcreate (H5P_DATASET_CREATE);
    CHECK(plist, FAIL, "H5Pcreate");
    status = H5Pset_chunk (plist, RANK_F, chunk_dimsf);
    CHECK(status, FAIL, "H5Pset_chunk");
    dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_UCHAR, dataspace, plist); 
    CHECK(dataset, FAIL, "H5Dcreate");

    /* 
     * Define hyperslab in the dataset. 
     */
    offset[0] = 0;
    offset[1] = 0;
    offset[2] = 0;
    count[0]  = NX_SUB;
    count[1]  = NY_SUB;
    count[2]  = NZ_SUB;
    status = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, offset, NULL, 
                                  count, NULL);
    CHECK(status, FAIL, "H5Sselect_hyperslab");

    /*
     * Define the memory dataspace.
     */
    dimsm[0] = NX;
    dimsm[1] = NY;
    dimsm[2] = NZ;
    memspace = H5Screate_simple (RANK_M, dimsm, NULL);   
    CHECK(memspace, FAIL, "H5Screate_simple");

    /* 
     * Define memory hyperslab. 
     */
    offset_out[0] = 0;
    offset_out[1] = 0;
    offset_out[2] = 0;
    count_out[0]  = NX_SUB;
    count_out[1]  = NY_SUB;
    count_out[2]  = NZ_SUB;
    status = H5Sselect_hyperslab (memspace, H5S_SELECT_SET, offset_out, NULL, 
                                  count_out, NULL);
    CHECK(status, FAIL, "H5Sselect_hyperslab");

    /*
     * Write the data to the dataset using hyperslabs
     */
    status = H5Dwrite (dataset, H5T_NATIVE_SHORT, memspace, dataspace,
                      xfer_plist, data);
    CHECK(status, FAIL, "H5Dwrite");

    /*
     * Close/release resources.
     */
    status=H5Pclose (plist);
    CHECK(status, FAIL, "H5Pclose");
    status=H5Sclose (dataspace);
    CHECK(status, FAIL, "H5Sclose");
    status=H5Sclose (memspace);
    CHECK(status, FAIL, "H5Sclose");
    status=H5Dclose (dataset);
    CHECK(status, FAIL, "H5Dclose");
    status=H5Fclose (file);
    CHECK(status, FAIL, "H5Fclose");
 

/*************************************************************  

  This reads the hyperslab from the test.h5 file just 
  created, into a 3-dimensional plane of the 3-dimensional 
  array.

 ************************************************************/  
 
    /*
     * Open the file and the dataset.
     */
    file = H5Fopen (FILENAME, H5F_ACC_RDONLY, fapl_plist);
    CHECK(file, FAIL, "H5Fopen");
    dataset = H5Dopen (file, DATASETNAME);
    CHECK(dataset, FAIL, "H5Dopen");

    dataspace = H5Dget_space (dataset);    /* dataspace handle */
    CHECK(dataspace, FAIL, "H5Dget_space");
    rank      = H5Sget_simple_extent_ndims (dataspace);
    VERIFY(rank, 3, "H5Sget_simple_extent_ndims");
    status_n  = H5Sget_simple_extent_dims (dataspace, dims_out, NULL);
    CHECK(status_n, FAIL, "H5Sget_simple_extent_dims");
    VERIFY(dims_out[0], dimsf[0], "Dataset dimensions");
    VERIFY(dims_out[1], dimsf[1], "Dataset dimensions");
    VERIFY(dims_out[2], dimsf[2], "Dataset dimensions");

    /* 
     * Define hyperslab in the dataset. 
     */
    offset[0] = 0;
    offset[1] = 0;
    offset[2] = 0;
    count[0]  = NX_SUB;
    count[1]  = NY_SUB;
    count[2]  = NZ_SUB;
    status = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, offset, NULL, 
                                  count, NULL);
    CHECK(status, FAIL, "H5Sselect_hyperslab");

    /*
     * Define the memory dataspace.
     */
    dimsm[0] = NX;
    dimsm[1] = NY;
    dimsm[2] = NZ;
    memspace = H5Screate_simple (RANK_M, dimsm, NULL);   
    CHECK(memspace, FAIL, "H5Screate_simple");

    /* 
     * Define memory hyperslab. 
     */
    offset_out[0] = 0;
    offset_out[1] = 0;
    offset_out[2] = 0;
    count_out[0]  = NX_SUB;
    count_out[1]  = NY_SUB;
    count_out[2]  = NZ_SUB;
    status = H5Sselect_hyperslab (memspace, H5S_SELECT_SET, offset_out, NULL, 
                                  count_out, NULL);
    CHECK(status, FAIL, "H5Sselect_hyperslab");

    /*
     * Read data from hyperslab in the file into the hyperslab in 
     * memory and display.
     */
    status = H5Dread (dataset, H5T_NATIVE_SHORT, memspace, dataspace,
                      xfer_plist, data_out);
    CHECK(status, FAIL, "H5Dread");

    /* Compare data written with data read in */
    tmpdata = data;
    tmpdata_out = data_out;
    for (j = 0; j < X; j++)
	for (i = 0; i < Y; i++)
	    for (k = 0; k < Z; k++,tmpdata++,tmpdata_out++) {
                if(*tmpdata!=*tmpdata_out) {
                    num_errs++;
                    printf("Error! j=%d, i=%d, k=%d, *tmpdata=%x, *tmpdata_out=%x\n",j,i,k,(unsigned)*tmpdata,(unsigned)*tmpdata_out);
                } /* end if */
            } /* end for */

    /*
     * Close and release resources.
     */
    status=H5Dclose (dataset);
    CHECK(status, FAIL, "H5Dclose");
    status=H5Sclose (dataspace);
    CHECK(status, FAIL, "H5Sclose");
    status=H5Sclose (memspace);
    CHECK(status, FAIL, "H5Sclose");
    status=H5Fclose (file);
    CHECK(status, FAIL, "H5Fclose");
    free (data);
    free (data_out);
}   /* test_select_hyper_chunk() */

/****************************************************************
**
**  test_select_valid(): Test basic H5S (dataspace) selection code.
**      Tests selection validity
** 
****************************************************************/
static void 
test_select_valid(void)
{
    herr_t error;
    htri_t valid;
    hid_t main_space, sub_space;
    hssize_t safe_start[2]={1,1};
    hsize_t safe_count[2]={1,1};
    hssize_t start[2];
    hsize_t dims[2],maxdims[2],size[2],count[2];

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Selection Validity\n"));

    count[0] = count[1] = 1;
    dims[0] = dims[1] = maxdims[0] = maxdims[1] = 10;

    main_space = H5Screate_simple(2,dims,maxdims);
    CHECK(main_space, FAIL, "H5Screate_simple");

    MESSAGE(8, ( "Case 1 : in the dimensions\nTry offset (4,4) and size(6,6), the original space is of size (10,10)\n"));
    start[0] = start[1] = 4;
    size[0] = size[1] = 6;

    sub_space = H5Scopy(main_space);
    CHECK(sub_space, FAIL, "H5Scopy");

    error=H5Sselect_hyperslab(sub_space,H5S_SELECT_SET,start,size,count,size);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    valid=H5Sselect_valid(sub_space);
    VERIFY(valid, TRUE, "H5Sselect_valid");

    error=H5Sselect_hyperslab(sub_space,H5S_SELECT_OR,safe_start,NULL,safe_count,NULL);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    valid=H5Sselect_valid(sub_space);
    VERIFY(valid, TRUE, "H5Sselect_valid");

    error=H5Sclose(sub_space);
    CHECK(error, FAIL, "H5Sclose");

    MESSAGE(8, ( "Case 2 : exceed dimensions by 1\nTry offset (5,5) and size(6,6), the original space is of size (10,10)\n"));
    start[0] = start[1] = 5;
    size[0] = size[1] = 6;

    sub_space = H5Scopy(main_space);
    CHECK(sub_space, FAIL, "H5Scopy");

    error=H5Sselect_hyperslab(sub_space,H5S_SELECT_SET,start,size,count,size);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    valid=H5Sselect_valid(sub_space);
    VERIFY(valid, FALSE, "H5Sselect_valid");

    error=H5Sselect_hyperslab(sub_space,H5S_SELECT_OR,safe_start,NULL,safe_count,NULL);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    valid=H5Sselect_valid(sub_space);
    VERIFY(valid, FALSE, "H5Sselect_valid");

    error=H5Sclose(sub_space);
    CHECK(error, FAIL, "H5Sclose");

    MESSAGE(8, ( "Case 3 : exceed dimensions by 2\nTry offset (6,6) and size(6,6), the original space is of size (10,10)\n"));
    start[0] = start[1] = 6;
    size[0] = size[1] = 6;

    sub_space = H5Scopy(main_space);
    CHECK(sub_space, FAIL, "H5Scopy");

    error=H5Sselect_hyperslab(sub_space,H5S_SELECT_SET,start,size,count,size);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    valid=H5Sselect_valid(sub_space);
    VERIFY(valid, FALSE, "H5Sselect_valid");

    error=H5Sselect_hyperslab(sub_space,H5S_SELECT_OR,safe_start,NULL,safe_count,NULL);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    valid=H5Sselect_valid(sub_space);
    VERIFY(valid, FALSE, "H5Sselect_valid");

    error=H5Sclose(sub_space);
    CHECK(error, FAIL, "H5Sclose");
    error=H5Sclose(main_space);
    CHECK(error, FAIL, "H5Sclose");
}   /* test_select_hyper_chunk() */

/*
 * Typedef for iteration structure used in the iteration tests
 */
typedef struct {
    size_t curr_coord;          /* Current coordinate to examine */
    hssize_t *coords;           /* Pointer to selection's coordinates */
} iter_test_info;

/****************************************************************
**
**  test_select_hyper_iter3(): Iterator for checking hyperslab iteration
** 
****************************************************************/
herr_t 
test_select_hyper_iter3(void UNUSED *_elem,hid_t UNUSED type_id, hsize_t ndim, hssize_t *point, void *_operator_data)
{
    iter_test_info *iter_info=(iter_test_info *)_operator_data; /* Get the pointer to the iterator information */
    hssize_t *coord_ptr;        /* Pointer to the coordinate information for a point*/

    /* Check number of dimensions */
    if(ndim!=SPACE8_RANK)
        return(-1);
    else {
        /* Check Coordinates */
        coord_ptr=iter_info->coords+(2*iter_info->curr_coord);
        iter_info->curr_coord++;
        if(coord_ptr[0]!=point[0])
            return(-1);
        else if(coord_ptr[1]!=point[1])
            return(-1);
        else
            return(0);
    } /* end else */
}   /* end test_select_hyper_iter3() */

/****************************************************************
**
**  test_select_iterate_all(): Test basic H5S (dataspace) selection code.
**      Tests iterating through "all" selections
** 
****************************************************************/
static void 
test_select_iterate_all(void)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE8_DIM1, SPACE8_DIM2};
    iter_test_info iter_info;   /* Iterator information structure */
    hssize_t    points[SPACE8_DIM1*SPACE8_DIM2][SPACE8_RANK];   /* Coordinates of selection */
    unsigned short fake_buf;    /* Fake "buffer" to iterate through */
    int         i,j;            /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Iterating Through 'all' Selections\n"));

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE8_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Space defaults to "all" selection */

    /* Set the coordinates of the selection */
    for(i=0; i<SPACE8_DIM1; i++)
        for(j=0; j<SPACE8_DIM2; j++) {
            points[(i*SPACE8_DIM2)+j][0]=i;
            points[(i*SPACE8_DIM2)+j][1]=j;
        } /* end for */

    /* Initialize the iterator structure */
    iter_info.curr_coord=0;
    iter_info.coords=(hssize_t *)points;

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(&fake_buf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
}   /* test_select_iterate_all() */

/****************************************************************
**
**  test_select_iterate_point(): Test basic H5S (dataspace) selection code.
**      Tests iterating through "point" selections
** 
****************************************************************/
static void 
test_select_iterate_point(hssize_t *offset)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE8_DIM1, SPACE8_DIM2};
    hssize_t    real_offset[SPACE8_RANK];       /* Actual offset to use */
    hssize_t    points[5][SPACE8_RANK] = {{2,4}, {3,8}, {8,4}, {7,5}, {7,7}};
    size_t      num_points=5;   /* Number of points selected */
    iter_test_info iter_info;   /* Iterator information structure */
    unsigned short fake_buf;    /* Fake "buffer" to iterate through */
    int         i;              /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Iterating Through 'point' Selections\n"));

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE8_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Select "point" selection */
    ret = H5Sselect_elements(sid1, H5S_SELECT_SET,num_points,(const hssize_t **)points);
    CHECK(ret, FAIL, "H5Sselect_elements");

    if(offset!=NULL) {
        HDmemcpy(real_offset,offset,SPACE8_RANK*sizeof(hssize_t));

        /* Set offset, if provided */
        ret = H5Soffset_simple(sid1,real_offset);
        CHECK(ret, FAIL, "H5Soffset_simple");
    } /* end if */
    else
        HDmemset(real_offset,0,SPACE8_RANK*sizeof(hssize_t));

    /* Initialize the iterator structure */
    iter_info.curr_coord=0;
    iter_info.coords=(hssize_t *)points;

    /* Add in the offset */
    for(i=0; i<(int)num_points; i++) {
        points[i][0]+=real_offset[0];
        points[i][1]+=real_offset[1];
    } /* end for */

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(&fake_buf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
}   /* test_select_iterate_point() */

/****************************************************************
**
**  test_select_iterate_hyper_simple(): Test basic H5S (dataspace) selection code.
**      Tests iterating through "simple" (i.e. one block) hyperslab selections
** 
****************************************************************/
static void 
test_select_iterate_hyper_simple(hssize_t *offset)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE8_DIM1, SPACE8_DIM2};
    hssize_t    real_offset[SPACE8_RANK];       /* Actual offset to use */
    hssize_t    start[SPACE8_RANK];     /* Hyperslab start */
    hsize_t     count[SPACE8_RANK];     /* Hyperslab block size */
    size_t      num_points;     /* Number of points in selection */
    hssize_t    points[16][SPACE8_RANK];        /* Coordinates selected */
    iter_test_info iter_info;   /* Iterator information structure */
    unsigned short fake_buf;    /* Fake "buffer" to iterate through */
    int         i,j;            /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Iterating Through Simple 'hyperslab' Selections\n"));

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE8_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Select "hyperslab" selection */
    start[0]=3; start[1]=3;
    count[0]=4; count[1]=4;
    ret = H5Sselect_hyperslab(sid1, H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    if(offset!=NULL) {
        HDmemcpy(real_offset,offset,SPACE8_RANK*sizeof(hssize_t));

        /* Set offset, if provided */
        ret = H5Soffset_simple(sid1,real_offset);
        CHECK(ret, FAIL, "H5Soffset_simple");
    } /* end if */
    else
        HDmemset(real_offset,0,SPACE8_RANK*sizeof(hssize_t));

    /* Initialize the iterator structure */
    iter_info.curr_coord=0;
    iter_info.coords=(hssize_t *)points;

    /* Set the coordinates of the selection (with the offset) */
    for(i=0, num_points=0; i<(int)count[0]; i++)
        for(j=0; j<(int)count[1]; j++, num_points++) {
            points[num_points][0]=i+start[0]+real_offset[0];
            points[num_points][1]=j+start[1]+real_offset[1];
        } /* end for */

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(&fake_buf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
}   /* test_select_iterate_hyper_simple() */

/****************************************************************
**
**  test_select_iterate_hyper_regular(): Test basic H5S (dataspace) selection code.
**      Tests iterating through "regular" (i.e. strided block) hyperslab selections
** 
****************************************************************/
static void 
test_select_iterate_hyper_regular(hssize_t *offset)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE8_DIM1, SPACE8_DIM2};
    hssize_t    real_offset[SPACE8_RANK];       /* Actual offset to use */
    hssize_t    start[SPACE8_RANK];     /* Hyperslab start */
    hsize_t     stride[SPACE8_RANK];    /* Hyperslab stride size */
    hsize_t     count[SPACE8_RANK];     /* Hyperslab block count */
    hsize_t     block[SPACE8_RANK];     /* Hyperslab block size */
    hssize_t    points[16][SPACE8_RANK] = {
        {2,2}, {2,3}, {2,6}, {2,7},
        {3,2}, {3,3}, {3,6}, {3,7},
        {6,2}, {6,3}, {6,6}, {6,7},
        {7,2}, {7,3}, {7,6}, {7,7},
        };
    size_t      num_points=16;  /* Number of points selected */
    iter_test_info iter_info;   /* Iterator information structure */
    unsigned short fake_buf;    /* Fake "buffer" to iterate through */
    int         i;              /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Iterating Through Regular 'hyperslab' Selections\n"));

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE8_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Select "hyperslab" selection */
    start[0]=2; start[1]=2;
    stride[0]=4; stride[1]=4;
    count[0]=2; count[1]=2;
    block[0]=2; block[1]=2;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    if(offset!=NULL) {
        HDmemcpy(real_offset,offset,SPACE8_RANK*sizeof(hssize_t));

        /* Set offset, if provided */
        ret = H5Soffset_simple(sid1,real_offset);
        CHECK(ret, FAIL, "H5Soffset_simple");
    } /* end if */
    else
        HDmemset(real_offset,0,SPACE8_RANK*sizeof(hssize_t));

    /* Initialize the iterator structure */
    iter_info.curr_coord=0;
    iter_info.coords=(hssize_t *)points;

    /* Add in the offset */
    for(i=0; i<(int)num_points; i++) {
        points[i][0]+=real_offset[0];
        points[i][1]+=real_offset[1];
    } /* end for */

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(&fake_buf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
}   /* test_select_iterate_hyper_regular() */

/****************************************************************
**
**  test_select_iterate_hyper_irregular(): Test basic H5S (dataspace) selection code.
**      Tests iterating through "irregular" (i.e. combined blocks) hyperslab selections
** 
****************************************************************/
static void 
test_select_iterate_hyper_irregular(hssize_t *offset)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE8_DIM1, SPACE8_DIM2};
    hssize_t    real_offset[SPACE8_RANK];       /* Actual offset to use */
    hssize_t    start[SPACE8_RANK];     /* Hyperslab start */
    hsize_t     count[SPACE8_RANK];     /* Hyperslab block count */
    hssize_t    iter_points[28][SPACE8_RANK] = { /* Coordinates, as iterated through */
        {2,2}, {2,3}, {2,4}, {2,5},
        {3,2}, {3,3}, {3,4}, {3,5},
        {4,2}, {4,3}, {4,4}, {4,5}, {4,6}, {4,7},
        {5,2}, {5,3}, {5,4}, {5,5}, {5,6}, {5,7},
        {6,4}, {6,5}, {6,6}, {6,7},
        {7,4}, {7,5}, {7,6}, {7,7},
        };
    size_t      num_iter_points=28;  /* Number of resulting points */
    iter_test_info iter_info;   /* Iterator information structure */
    unsigned short fake_buf;    /* Fake "buffer" to iterate through */
    int         i;              /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Iterating Through Irregular 'hyperslab' Selections\n"));

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE8_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Select first "hyperslab" selection */
    start[0]=2; start[1]=2;
    count[0]=4; count[1]=4;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Combine with second "hyperslab" selection */
    start[0]=4; start[1]=4;
    count[0]=4; count[1]=4;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_OR,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    if(offset!=NULL) {
        HDmemcpy(real_offset,offset,SPACE8_RANK*sizeof(hssize_t));

        /* Set offset, if provided */
        ret = H5Soffset_simple(sid1,real_offset);
        CHECK(ret, FAIL, "H5Soffset_simple");
    } /* end if */
    else
        HDmemset(real_offset,0,SPACE8_RANK*sizeof(hssize_t));

    /* Initialize the iterator structure */
    iter_info.curr_coord=0;
    iter_info.coords=(hssize_t *)iter_points;

    /* Add in the offset */
    for(i=0; i<(int)num_iter_points; i++) {
        iter_points[i][0]+=real_offset[0];
        iter_points[i][1]+=real_offset[1];
    } /* end for */

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(&fake_buf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

}   /* test_select_iterate_hyper_irregular() */

/****************************************************************
**
**  test_select(): Main H5S selection testing routine.
** 
****************************************************************/
void 
test_select(void)
{
    hid_t plist_id;     /* Property list for reading random hyperslabs */
    hid_t fapl;         /* Property list accessing the file */
    int mdc_nelmts;     /* Metadata number of elements */
    int rdcc_nelmts;    /* Raw data number of elements */
    size_t rdcc_nbytes; /* Raw data number of bytes */
    double rdcc_w0;     /* Raw data write percentage */
    hssize_t offset[SPACE7_RANK]={1,1}; /* Offset for testing selection offsets */
    herr_t	ret;	/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Selections\n"));

    /* Create a dataset transfer property list */
    plist_id=H5Pcreate(H5P_DATASET_XFER);
    CHECK(plist_id, FAIL, "H5Pcreate");

    /* test I/O with a very small buffer for reads */
    ret=H5Pset_buffer(plist_id,(hsize_t)59,NULL,NULL);
    CHECK(ret, FAIL, "H5Pset_buffer");

    /* These next tests use the same file */
    test_select_hyper(H5P_DEFAULT);     /* Test basic H5S hyperslab selection code */
    test_select_hyper(plist_id);        /* Test basic H5S hyperslab selection code */
    test_select_point(H5P_DEFAULT);     /* Test basic H5S element selection code, also tests appending to existing element selections */
    test_select_point(plist_id);        /* Test basic H5S element selection code, also tests appending to existing element selections */
    test_select_all(H5P_DEFAULT);       /* Test basic all & none selection code */
    test_select_all(plist_id);          /* Test basic all & none selection code */
    test_select_all_hyper(H5P_DEFAULT);       /* Test basic all & none selection code */
    test_select_all_hyper(plist_id);          /* Test basic all & none selection code */

    /* These next tests use the same file */
    test_select_combo();        /* Test combined hyperslab & element selection code */
    test_select_hyper_stride(H5P_DEFAULT); /* Test strided hyperslab selection code */
    test_select_hyper_stride(plist_id); /* Test strided hyperslab selection code */
    test_select_hyper_contig(H5T_STD_U16LE,H5P_DEFAULT); /* Test contiguous hyperslab selection code */
    test_select_hyper_contig(H5T_STD_U16LE,plist_id); /* Test contiguous hyperslab selection code */
    test_select_hyper_contig(H5T_STD_U16BE,H5P_DEFAULT); /* Test contiguous hyperslab selection code */
    test_select_hyper_contig(H5T_STD_U16BE,plist_id); /* Test contiguous hyperslab selection code */
    test_select_hyper_contig2(H5T_STD_U16LE,H5P_DEFAULT); /* Test more contiguous hyperslab selection cases */
    test_select_hyper_contig2(H5T_STD_U16LE,plist_id); /* Test more contiguous hyperslab selection cases */
    test_select_hyper_contig2(H5T_STD_U16BE,H5P_DEFAULT); /* Test more contiguous hyperslab selection cases */
    test_select_hyper_contig2(H5T_STD_U16BE,plist_id); /* Test more contiguous hyperslab selection cases */
    test_select_hyper_copy();   /* Test hyperslab selection copying code */
    test_select_point_copy();   /* Test point selection copying code */
    test_select_hyper_offset(); /* Test selection offset code with hyperslabs */
    test_select_hyper_offset2();/* Test more selection offset code with hyperslabs */
    test_select_point_offset(); /* Test selection offset code with elements */
    test_select_hyper_union();  /* Test hyperslab union code */
    test_select_hyper_union_stagger();  /* Test hyperslab union code for staggered slabs */
    test_select_hyper_union_3d();  /* Test hyperslab union code for 3-D dataset */

    /* test the random hyperslab I/O with the default property list for reading */
    test_select_hyper_union_random_5d(H5P_DEFAULT);  /* Test hyperslab union code for random 5-D hyperslabs */

    /* test random hyperslab I/O with a small buffer for reads */
    test_select_hyper_union_random_5d(plist_id);  /* Test hyperslab union code for random 5-D hyperslabs */

    /* Create a dataset transfer property list */
    fapl=H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Get the default file access properties for caching */
    ret=H5Pget_cache(fapl,&mdc_nelmts,&rdcc_nelmts,&rdcc_nbytes,&rdcc_w0);
    CHECK(ret, FAIL, "H5Pget_cache");

    /* Increase the size of the raw data cache */
    rdcc_nbytes=10*1024*1024;

    /* Set the file access properties for caching */
    ret=H5Pset_cache(fapl,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0);
    CHECK(ret, FAIL, "H5Pset_cache");

    /* Test reading in a large hyperslab with a chunked dataset */
    test_select_hyper_chunk(fapl,H5P_DEFAULT);

    /* Test reading in a large hyperslab with a chunked dataset a small amount at a time */
    test_select_hyper_chunk(fapl,plist_id);

    /* Close file access property list */
    ret=H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataset transfer property list */
    ret=H5Pclose(plist_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* More tests for checking validity of selections */
    test_select_valid();

    /* Test iterating through selections */
    test_select_iterate_all();
    test_select_iterate_point(NULL);
    test_select_iterate_point(offset);
    test_select_iterate_hyper_simple(NULL);
    test_select_iterate_hyper_simple(offset);
    test_select_iterate_hyper_regular(NULL);
    test_select_iterate_hyper_regular(offset);
    test_select_iterate_hyper_irregular(NULL);
    test_select_iterate_hyper_irregular(offset);

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

