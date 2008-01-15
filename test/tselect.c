/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/***********************************************************
*
* Test program:	 tselect
*
* Test the Dataspace selection functionality
*
*************************************************************/

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

/* Define this macro to indicate that the testing APIs should be available */
#define H5S_TESTING

#include "testhdf5.h"
#include "hdf5.h"
#include "H5Spkg.h"		/* Dataspaces				*/

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
#define SPACE2A_RANK	1
#define SPACE2A_DIM1	(SPACE2_DIM1*SPACE2_DIM2)

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

/* 2-D dataset with easy dimension sizes */
#define SPACE7_NAME  "Space7"
#define SPACE7_RANK	2
#define SPACE7_DIM1	10
#define SPACE7_DIM2	10
#define SPACE7_FILL     254
#define SPACE7_CHUNK_DIM1 5
#define SPACE7_CHUNK_DIM2 5
#define SPACE7_NPOINTS  8

/* 4-D dataset with fixed dimensions */
#define SPACE8_NAME  "Space8"
#define SPACE8_RANK	4
#define SPACE8_DIM1	11
#define SPACE8_DIM2	13
#define SPACE8_DIM3	17
#define SPACE8_DIM4	19

/* Another 2-D dataset with easy dimension sizes */
#define SPACE9_NAME  "Space9"
#define SPACE9_RANK	2
#define SPACE9_DIM1	12
#define SPACE9_DIM2	12

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

/* Basic chunk size */
#define SPACE10_DIM1    180
#define SPACE10_CHUNK_SIZE 12

/* Information for bounds checking test */
#define SPACE11_RANK	2
#define SPACE11_DIM1    100
#define SPACE11_DIM2    100
#define SPACE11_NPOINTS 4

/* Information for offsets w/chunks test #2 */
#define SPACE12_RANK	        1
#define SPACE12_DIM0            25
#define SPACE12_CHUNK_DIM0      5

/* Location comparison function */
int compare_size_t(const void *s1, const void *s2);

herr_t test_select_hyper_iter1(void *elem,hid_t type_id, unsigned ndim, const hsize_t *point, void *operator_data);
herr_t test_select_point_iter1(void *elem,hid_t type_id, unsigned ndim, const hsize_t *point, void *operator_data);
herr_t test_select_all_iter1(void *elem,hid_t type_id, unsigned ndim, const hsize_t *point, void *operator_data);
herr_t test_select_none_iter1(void *elem,hid_t type_id, unsigned ndim, const hsize_t *point, void *operator_data);
herr_t test_select_hyper_iter2(void *_elem, hid_t type_id, unsigned ndim, const hsize_t *point, void *_operator_data);
herr_t test_select_hyper_iter3(void *elem,hid_t type_id, unsigned ndim, const hsize_t *point, void *operator_data);

/****************************************************************
**
**  test_select_hyper_iter1(): Iterator for checking hyperslab iteration
**
****************************************************************/
herr_t
test_select_hyper_iter1(void *_elem, hid_t UNUSED type_id, unsigned UNUSED ndim, const hsize_t UNUSED *point, void *_operator_data)
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
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid1,sid2;	/* Dataspace ID			*/
    hsize_t	dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t	dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t	dims3[] = {SPACE3_DIM1, SPACE3_DIM2};
    hsize_t	start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t	stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t	count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t	block[SPACE1_RANK];     /* Block size of hyperslab */
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

    /* Test selecting stride==0 to verify failure */
    start[0]=1; start[1]=0; start[2]=0;
    stride[0]=0; stride[1]=0; stride[2]=0;
    count[0]=2; count[1]=15; count[2]=13;
    block[0]=1; block[1]=1; block[2]=1;
    H5E_BEGIN_TRY {
        ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sselect_hyperslab");

    /* Test selecting stride<block to verify failure */
    start[0]=1; start[1]=0; start[2]=0;
    stride[0]=1; stride[1]=1; stride[2]=1;
    count[0]=2; count[1]=15; count[2]=13;
    block[0]=2; block[1]=2; block[2]=2;
    H5E_BEGIN_TRY {
        ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sselect_hyperslab");

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

    /* Exercise check for NULL buffer and valid selection */
    H5E_BEGIN_TRY {
        ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,NULL);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Dwrite");

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

    /* Select 0x26 hyperslab to OR into current selection (should be a NOOP) */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=0; count[1]=26;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Exercise check for NULL buffer and valid selection */
    H5E_BEGIN_TRY {
        ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,NULL);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Dread");

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
    hsize_t	coord[POINT1_NPOINTS*2][SPACE2_RANK]; /* Coordinates for point selection */
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
test_select_point_iter1(void *_elem, hid_t UNUSED type_id, unsigned UNUSED ndim, const hsize_t UNUSED *point, void *_operator_data)
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
}   /* end test_select_point_iter1() */

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
    hsize_t	coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hsize_t	temp_coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hsize_t	coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hsize_t	temp_coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hsize_t	coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    hsize_t	temp_coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
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
    ret = H5Sselect_elements(sid1,H5S_SELECT_SET,POINT1_NPOINTS,coord1);
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
    ret = H5Sselect_elements(sid1,H5S_SELECT_APPEND,POINT1_NPOINTS,coord1);
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
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,coord2);
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
    ret = H5Sselect_elements(sid2,H5S_SELECT_PREPEND,POINT1_NPOINTS,coord2);
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
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,coord3);
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
    ret = H5Sselect_elements(sid2,H5S_SELECT_APPEND,POINT1_NPOINTS,coord3);
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
test_select_all_iter1(void *_elem, hid_t UNUSED type_id, unsigned UNUSED ndim, const hsize_t UNUSED *point, void *_operator_data)
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
test_select_none_iter1(void UNUSED *_elem, hid_t UNUSED type_id, unsigned UNUSED ndim, const hsize_t UNUSED *point, void UNUSED *_operator_data)
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
            for(k=0; k<SPACE4_DIM3; k++)
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
    hsize_t		start[SPACE1_RANK];     /* Starting location of hyperslab */
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
    hsize_t		coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hsize_t		start[SPACE1_RANK];     /* Starting location of hyperslab */
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
    wbuf=HDmalloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

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
    ret = H5Sselect_elements(sid1,H5S_SELECT_SET,POINT1_NPOINTS,coord1);
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
        if(*tbuf!=*tbuf2)
            TestErrPrintf("element values don't match!, i=%d\n",i);
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
    HDfree(wbuf);
    HDfree(rbuf);
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
    hsize_t		start[SPACE1_RANK];     /* Starting location of hyperslab */
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
    wbuf=HDmalloc(sizeof(uint16_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);

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
            printf("wbuf=%p, tbuf=%p, rbuf=%p, tbuf2=%p\n",(void *)wbuf,(void *)tbuf,(void *)rbuf,(void *)tbuf2);
            TestErrPrintf("*tbuf=%u, *tbuf2=%u\n",(unsigned)*tbuf,(unsigned)*tbuf2);
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
    HDfree(wbuf);
    HDfree(rbuf);
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
    hsize_t		start[SPACE1_RANK];     /* Starting location of hyperslab */
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
    wbuf=HDmalloc(sizeof(uint16_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint16_t),SPACE2_DIM1*SPACE2_DIM2);

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
        TestErrPrintf("hyperslab values don't match! Line=%d\n",__LINE__);
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
    HDfree(wbuf);
    HDfree(rbuf);
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
    hsize_t		dims2[] = {SPACE8_DIM4, SPACE8_DIM3, SPACE8_DIM2, SPACE8_DIM1};
    hsize_t	        start[SPACE8_RANK];     /* Starting location of hyperslab */
    hsize_t		count[SPACE8_RANK];     /* Element count of hyperslab */
    uint16_t   *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    int        i,j,k,l;     /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing More Contiguous Hyperslabs Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=HDmalloc(sizeof(uint16_t)*SPACE8_DIM1*SPACE8_DIM2*SPACE8_DIM3*SPACE8_DIM4);
    rbuf=HDcalloc(sizeof(uint16_t),SPACE8_DIM1*SPACE8_DIM2*SPACE8_DIM3*SPACE8_DIM4);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE8_DIM1; i++)
        for(j=0; j<SPACE8_DIM2; j++)
            for(k=0; k<SPACE8_DIM3; k++)
                for(l=0; l<SPACE8_DIM4; l++)
                    *tbuf++=(uint16_t)((i*SPACE8_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE8_RANK, dims2, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE8_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select contiguous hyperslab for disk dataset */
    start[0]=0; start[1]=0; start[2]=0; start[3]=0;
    count[0]=2; count[1]=SPACE8_DIM3; count[2]=SPACE8_DIM2; count[3]=SPACE8_DIM1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select contiguous hyperslab in memory */
    start[0]=0; start[1]=0; start[2]=0; start[3]=0;
    count[0]=2; count[1]=SPACE8_DIM3; count[2]=SPACE8_DIM2; count[3]=SPACE8_DIM1;
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
    sid2 = H5Screate_simple(SPACE8_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select contiguous hyperslab in memory */
    start[0]=0; start[1]=0; start[2]=0; start[3]=0;
    count[0]=2; count[1]=SPACE8_DIM3; count[2]=SPACE8_DIM2; count[3]=SPACE8_DIM1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select contiguous hyperslab in memory */
    start[0]=0; start[1]=0; start[2]=0; start[3]=0;
    count[0]=2; count[1]=SPACE8_DIM3; count[2]=SPACE8_DIM2; count[3]=SPACE8_DIM1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_USHORT,sid2,sid1,xfer_plist,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    if(HDmemcmp(rbuf,wbuf,sizeof(uint16_t)*2*SPACE8_DIM3*SPACE8_DIM2*SPACE8_DIM1)) {
        TestErrPrintf("Error: hyperslab values don't match!\n");
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
    HDfree(wbuf);
    HDfree(rbuf);
}   /* test_select_hyper_contig2() */

/****************************************************************
**
**  test_select_hyper_contig3(): Test H5S (dataspace) selection code.
**      Tests contiguous hyperslabs of various sizes and dimensionalities.
**  This test uses a hyperslab that is contiguous in the lowest dimension,
**  not contiguous in a dimension, then has a selection across the entire next
**  dimension (which should be "flattened" out also).
**
****************************************************************/
static void
test_select_hyper_contig3(hid_t dset_type, hid_t xfer_plist)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims2[] = {SPACE8_DIM4, SPACE8_DIM3, SPACE8_DIM2, SPACE8_DIM1};
    hsize_t	        start[SPACE8_RANK];     /* Starting location of hyperslab */
    hsize_t		count[SPACE8_RANK];     /* Element count of hyperslab */
    uint16_t   *wbuf,           /* Buffer to write to disk */
               *rbuf,           /* Buffer read from disk */
               *tbuf, *tbuf2;   /* Temporary buffer pointers */
    unsigned   i,j,k,l;     /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Yet More Contiguous Hyperslabs Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=HDmalloc(sizeof(uint16_t)*SPACE8_DIM1*SPACE8_DIM2*SPACE8_DIM3*SPACE8_DIM4);
    rbuf=HDcalloc(sizeof(uint16_t),SPACE8_DIM1*SPACE8_DIM2*SPACE8_DIM3*SPACE8_DIM4);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE8_DIM4; i++)
        for(j=0; j<SPACE8_DIM3; j++)
            for(k=0; k<SPACE8_DIM2; k++)
                for(l=0; l<SPACE8_DIM1; l++)
                    *tbuf++=(uint16_t)((k*SPACE8_DIM2)+l);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE8_RANK, dims2, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE8_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select semi-contiguous hyperslab for disk dataset */
    start[0]=0; start[1]=0; start[2]=SPACE8_DIM2/2; start[3]=0;
    count[0]=2; count[1]=SPACE8_DIM3; count[2]=SPACE8_DIM2/2; count[3]=SPACE8_DIM1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select semi-contiguous hyperslab in memory */
    start[0]=0; start[1]=0; start[2]=SPACE8_DIM2/2; start[3]=0;
    count[0]=2; count[1]=SPACE8_DIM3; count[2]=SPACE8_DIM2/2; count[3]=SPACE8_DIM1;
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
    sid2 = H5Screate_simple(SPACE8_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select semi-contiguous hyperslab in memory */
    start[0]=0; start[1]=0; start[2]=SPACE8_DIM2/2; start[3]=0;
    count[0]=2; count[1]=SPACE8_DIM3; count[2]=SPACE8_DIM2/2; count[3]=SPACE8_DIM1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Select semi-contiguous hyperslab in memory */
    start[0]=0; start[1]=0; start[2]=SPACE8_DIM2/2; start[3]=0;
    count[0]=2; count[1]=SPACE8_DIM3; count[2]=SPACE8_DIM2/2; count[3]=SPACE8_DIM1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_USHORT,sid2,sid1,xfer_plist,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    for(i=0, tbuf=wbuf,tbuf2=rbuf; i<SPACE8_DIM4; i++)
        for(j=0; j<SPACE8_DIM3; j++)
            for(k=0; k<SPACE8_DIM2; k++)
                for(l=0; l<SPACE8_DIM1; l++,tbuf++,tbuf2++)
                    if( (i>=start[0] && i<(start[0]+count[0])) &&
                            (j>=start[1] && j<(start[1]+count[1])) &&
                            (k>=start[2] && k<(start[2]+count[2])) &&
                            (l>=start[3] && l<(start[3]+count[3])) ) {
                        if(*tbuf!=*tbuf2) {
                            printf("Error: hyperslab values don't match!\n");
                            TestErrPrintf("Line: %d, i=%u, j=%u, k=%u, l=%u, *tbuf=%u,*tbuf2=%u\n",__LINE__,i,j,k,l,(unsigned)*tbuf,(unsigned)*tbuf2);
                        } /* end if */
                    } /* end if */
                    else {
                        if(*tbuf2!=0) {
                            printf("Error: invalid data in read buffer!\n");
                            TestErrPrintf("Line: %d, i=%u, j=%u, k=%u, l=%u, *tbuf=%u,*tbuf2=%u\n",__LINE__,i,j,k,l,(unsigned)*tbuf,(unsigned)*tbuf2);
                        } /* end if */
                    } /* end else */

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
    HDfree(wbuf);
    HDfree(rbuf);
}   /* test_select_hyper_contig3() */

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
    hsize_t		start[SPACE1_RANK];     /* Starting location of hyperslab */
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
    wbuf=HDmalloc(sizeof(uint16_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);
    rbuf2=HDcalloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);

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
        TestErrPrintf("hyperslab values don't match! Line=%d\n",__LINE__);
#ifdef QAK
        for(i=0; i<SPACE3_DIM1; i++)
            for(j=0; j<SPACE3_DIM2; j++)
                if((unsigned)*(rbuf+i*SPACE3_DIM2+j)!=(unsigned)*(rbuf2+i*SPACE3_DIM2+j))
                    printf("i=%d, j=%d, *rbuf=%u, *rbuf2=%u\n",i,j,(unsigned)*(rbuf+i*SPACE3_DIM2+j),(unsigned)*(rbuf2+i*SPACE3_DIM2+j));
#endif /* QAK */
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
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(rbuf2);
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
    hsize_t	coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hsize_t	coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hsize_t	coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    uint16_t   *wbuf,       /* buffer to write to disk */
               *rbuf,       /* 1st buffer read from disk */
               *rbuf2,      /* 2nd buffer read from disk */
               *tbuf;       /* temporary buffer pointer */
    int        i,j;        /* Counters */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslabs with Strides Functionality\n"));

    /* Allocate write & read buffers */
    wbuf=HDmalloc(sizeof(uint16_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);
    rbuf2=HDcalloc(sizeof(uint16_t),SPACE3_DIM1*SPACE3_DIM2);

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
    ret = H5Sselect_elements(sid1,H5S_SELECT_SET,POINT1_NPOINTS,coord1);
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
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,coord2);
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
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,coord3);
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
    if(HDmemcmp(rbuf,rbuf2,sizeof(uint16_t)*SPACE3_DIM1*SPACE3_DIM2))
        TestErrPrintf("point values don't match!\n");

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
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(rbuf2);
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
    hsize_t		start[SPACE1_RANK];     /* Starting location of hyperslab */
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
    wbuf=HDmalloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

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
            if(*tbuf!=*tbuf2)
                TestErrPrintf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%u, *tbuf2=%u\n",__LINE__,i,j,(unsigned)*tbuf,(unsigned)*tbuf2);
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
    HDfree(wbuf);
    HDfree(rbuf);
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
    hsize_t	dims1[] = {SPACE7_DIM1, SPACE7_DIM2};
    hsize_t	dims2[] = {SPACE7_DIM1, SPACE7_DIM2};
    hsize_t	start[SPACE7_RANK];     /* Starting location of hyperslab */
    hsize_t	count[SPACE7_RANK];     /* Element count of hyperslab */
    hssize_t	offset[SPACE7_RANK];    /* Offset of selection */
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
    wbuf=HDmalloc(sizeof(uint8_t)*SPACE7_DIM1*SPACE7_DIM2);
    rbuf=HDcalloc(sizeof(uint8_t),SPACE7_DIM1*SPACE7_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE7_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE7_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE7_RANK, dims2, NULL);
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
        tbuf=wbuf+((i+3)*SPACE7_DIM2);
        tbuf2=rbuf+((i+3)*SPACE7_DIM2);
        for(j=0; j<SPACE7_DIM2; j++, tbuf++, tbuf2++) {
            if(*tbuf!=*tbuf2)
                TestErrPrintf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%u, *tbuf2=%u\n",__LINE__,i,j,(unsigned)*tbuf,(unsigned)*tbuf2);
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
    HDfree(wbuf);
    HDfree(rbuf);
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
    hsize_t	coord1[POINT1_NPOINTS][SPACE1_RANK]; /* Coordinates for point selection */
    hsize_t	coord2[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hsize_t	coord3[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
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
    wbuf=HDmalloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

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
    ret = H5Sselect_elements(sid1,H5S_SELECT_SET,POINT1_NPOINTS,coord1);
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
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,coord2);
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
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,coord3);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read with data written out */
    for(i=0; i<POINT1_NPOINTS; i++) {
        tbuf=wbuf+((coord2[i][0]+offset[0])*SPACE2_DIM2)+coord2[i][1]+offset[1];
        tbuf2=rbuf+(coord3[i][0]*SPACE3_DIM2)+coord3[i][1];
        if(*tbuf!=*tbuf2)
            TestErrPrintf("element values don't match!, i=%d\n",i);
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
    HDfree(wbuf);
    HDfree(rbuf);
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
    hsize_t		start[SPACE1_RANK];     /* Starting location of hyperslab */
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
    wbuf=HDmalloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint8_t),SPACE3_DIM1*SPACE3_DIM2);

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

    npoints = H5Sget_select_npoints(sid1);
    VERIFY(npoints, 2*15*13, "H5Sget_select_npoints");

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
            if(*tbuf!=*tbuf2)
                TestErrPrintf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
            if(*tbuf!=*tbuf2)
                TestErrPrintf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
            if(*tbuf!=*tbuf2)
                TestErrPrintf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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

#ifdef H5_WANT_H5_V1_4_COMPAT
    ret = H5Pset_hyper_cache(xfer,0,1);
    CHECK(ret, FAIL, "H5Pset_hyper_cache");
#endif /* H5_WANT_H5_V1_4_COMPAT */

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
            if(*tbuf!=*tbuf2)
                TestErrPrintf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
            if(*tbuf!=*tbuf2)
                TestErrPrintf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
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
    HDfree(wbuf);
    HDfree(rbuf);
}   /* test_select_hyper_union() */

/****************************************************************
**
**  test_select_hyper_and_2d(): Test basic H5S (dataspace) selection code.
**      Tests 'and' of hyperslabs in 2-D
**
****************************************************************/
static void
test_select_hyper_and_2d(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims2[] = {SPACE2A_DIM1};
    hsize_t	        start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE2_RANK];     /* Block size of hyperslab */
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    int        i,j;        /* Counters */
    herr_t		ret;    /* Generic return value		*/
    hsize_t	    npoints;	/* Number of elements in selection */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with intersection of 2-D hyperslabs\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE2_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2A_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 10x10 hyperslab for disk dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Intersect overlapping 10x10 hyperslab */
    start[0]=5; start[1]=5;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_AND,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid1);
    VERIFY(npoints, 5*5, "H5Sget_select_npoints");

    /* Select 25 hyperslab for memory dataset */
    start[0]=0;
    stride[0]=1;
    count[0]=25;
    block[0]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 5*5, "H5Sget_select_npoints");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read entire dataset from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Initialize write buffer */
    for(i=0, tbuf=rbuf, tbuf2=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++, tbuf++) {
            if((i>=5 && i<=9) && (j>=5 && j<=9)) {
                if(*tbuf!=*tbuf2)
                    printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
                tbuf2++;
            } /* end if */
            else {
                if(*tbuf!=0)
                    printf("%d: hyperslab element has wrong value!, i=%d, j=%d, *tbuf=%d\n",__LINE__,i,j,(int)*tbuf);
            } /* end else */
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
}   /* test_select_hyper_and_2d() */

/****************************************************************
**
**  test_select_hyper_xor_2d(): Test basic H5S (dataspace) selection code.
**      Tests 'xor' of hyperslabs in 2-D
**
****************************************************************/
static void
test_select_hyper_xor_2d(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims2[] = {SPACE2A_DIM1};
    hsize_t	        start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE2_RANK];     /* Block size of hyperslab */
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    int        i,j;        /* Counters */
    herr_t		ret;    /* Generic return value		*/
    hsize_t	    npoints;	/* Number of elements in selection */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with XOR of 2-D hyperslabs\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE2_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2A_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 10x10 hyperslab for disk dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Intersect overlapping 10x10 hyperslab */
    start[0]=5; start[1]=5;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_XOR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid1);
    VERIFY(npoints, 150, "H5Sget_select_npoints");

    /* Select 25 hyperslab for memory dataset */
    start[0]=0;
    stride[0]=1;
    count[0]=150;
    block[0]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 150, "H5Sget_select_npoints");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read entire dataset from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Initialize write buffer */
    for(i=0, tbuf=rbuf, tbuf2=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++, tbuf++) {
            if(((i>=0 && i<=4) && (j>=0 && j<=9)) ||
                    ((i>=5 && i<=9) && ((j>=0 && j<=4) || (j>=10 && j<=14))) ||
                    ((i>=10 && i<=14) && (j>=5 && j<=14))) {
                if(*tbuf!=*tbuf2)
                    printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
                tbuf2++;
            } /* end if */
            else {
                if(*tbuf!=0)
                    printf("%d: hyperslab element has wrong value!, i=%d, j=%d, *tbuf=%d\n",__LINE__,i,j,(int)*tbuf);
            } /* end else */
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
}   /* test_select_hyper_xor_2d() */

/****************************************************************
**
**  test_select_hyper_notb_2d(): Test basic H5S (dataspace) selection code.
**      Tests 'notb' of hyperslabs in 2-D
**
****************************************************************/
static void
test_select_hyper_notb_2d(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims2[] = {SPACE2A_DIM1};
    hsize_t	        start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE2_RANK];     /* Block size of hyperslab */
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    int        i,j;        /* Counters */
    herr_t		ret;    /* Generic return value		*/
    hsize_t	    npoints;	/* Number of elements in selection */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with NOTB of 2-D hyperslabs\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=calloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE2_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2A_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 10x10 hyperslab for disk dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Intersect overlapping 10x10 hyperslab */
    start[0]=5; start[1]=5;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_NOTB,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid1);
    VERIFY(npoints, 75, "H5Sget_select_npoints");

    /* Select 75 hyperslab for memory dataset */
    start[0]=0;
    stride[0]=1;
    count[0]=75;
    block[0]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 75, "H5Sget_select_npoints");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read entire dataset from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Initialize write buffer */
    for(i=0, tbuf=rbuf, tbuf2=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++, tbuf++) {
            if(((i>=0 && i<=4) && (j>=0 && j<=9)) ||
                    ((i>=5 && i<=9) && (j>=0 && j<=4))) {
                if(*tbuf!=*tbuf2)
                    printf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
                tbuf2++;
            } /* end if */
            else {
                if(*tbuf!=0)
                    printf("%d: hyperslab element has wrong value!, i=%d, j=%d, *tbuf=%d\n",__LINE__,i,j,(int)*tbuf);
            } /* end else */
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
}   /* test_select_hyper_notb_2d() */

/****************************************************************
**
**  test_select_hyper_nota_2d(): Test basic H5S (dataspace) selection code.
**      Tests 'nota' of hyperslabs in 2-D
**
****************************************************************/
static void
test_select_hyper_nota_2d(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hsize_t		dims1[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t		dims2[] = {SPACE2A_DIM1};
    hsize_t	        start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE2_RANK];     /* Block size of hyperslab */
    uint8_t    *wbuf,       /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf,       /* temporary buffer pointer */
               *tbuf2;      /* temporary buffer pointer */
    int        i,j;        /* Counters */
    herr_t		ret;    /* Generic return value		*/
    hsize_t	    npoints;	/* Number of elements in selection */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab Selection Functions with NOTA of 2-D hyperslabs\n"));

    /* Allocate write & read buffers */
    wbuf=HDmalloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    rbuf=HDcalloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE2_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE2A_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Select 10x10 hyperslab for disk dataset */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Intersect overlapping 10x10 hyperslab */
    start[0]=5; start[1]=5;
    stride[0]=1; stride[1]=1;
    count[0]=10; count[1]=10;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_NOTA,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid1);
    VERIFY(npoints, 75, "H5Sget_select_npoints");

    /* Select 75 hyperslab for memory dataset */
    start[0]=0;
    stride[0]=1;
    count[0]=75;
    block[0]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    npoints = H5Sget_select_npoints(sid2);
    VERIFY(npoints, 75, "H5Sget_select_npoints");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read entire dataset from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Initialize write buffer */
    for(i=0, tbuf=rbuf, tbuf2=wbuf; i<SPACE2_DIM1; i++)
        for(j=0; j<SPACE2_DIM2; j++, tbuf++) {
            if(((i>=10 && i<=14) && (j>=5 && j<=14)) ||
                    ((i>=5 && i<=9) && (j>=10 && j<=14))) {
                if(*tbuf!=*tbuf2)
                    TestErrPrintf("%d: hyperslab values don't match!, i=%d, j=%d, *tbuf=%d, *tbuf2=%d\n",__LINE__,i,j,(int)*tbuf,(int)*tbuf2);
                tbuf2++;
            } /* end if */
            else {
                if(*tbuf!=0)
                    TestErrPrintf("%d: hyperslab element has wrong value!, i=%d, j=%d, *tbuf=%d\n",__LINE__,i,j,(int)*tbuf);
            } /* end else */
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
    HDfree(wbuf);
    HDfree(rbuf);
}   /* test_select_hyper_nota_2d() */

/****************************************************************
**
**  test_select_hyper_iter2(): Iterator for checking hyperslab iteration
**
****************************************************************/
herr_t
test_select_hyper_iter2(void *_elem, hid_t UNUSED type_id, unsigned ndim, const hsize_t *point, void *_operator_data)
{
    int *tbuf=(int *)_elem,     /* temporary buffer pointer */
        **tbuf2=(int **)_operator_data; /* temporary buffer handle */
    unsigned u;             /* Local counting variable */

    if(*tbuf!=**tbuf2) {
        TestErrPrintf("Error in hyperslab iteration!\n");
        printf("location: { ");
        for(u=0; u<ndim; u++) {
            printf("%2d",(int)point[u]);
            if(u<(ndim-1))
                printf(", ");
        } /* end for */
        printf("}\n");
        printf("*tbuf=%d, **tbuf2=%d\n",*tbuf,**tbuf2);
        return(-1);
    } /* end if */
    else {
        (*tbuf2)++;
        return(0);
    }
}   /* end test_select_hyper_iter2() */

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
    hsize_t		start[SPACE5_RANK];     /* Starting location of hyperslab */
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
    wbuf=HDmalloc(sizeof(int)*SPACE5_DIM1*SPACE5_DIM2*SPACE5_DIM3*SPACE5_DIM4*SPACE5_DIM5);
    rbuf=HDcalloc(sizeof(int),SPACE5_DIM1*SPACE5_DIM2*SPACE5_DIM3*SPACE5_DIM4*SPACE5_DIM5);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE5_DIM1; i++)
        for(j=0; j<SPACE5_DIM2; j++)
            for(k=0; k<SPACE5_DIM3; k++)
                for(l=0; l<SPACE5_DIM4; l++)
                    for(m=0; m<SPACE5_DIM5; m++)
                        *tbuf++=(int)(((((((i*SPACE5_DIM2)+j)*SPACE5_DIM3)+k)*SPACE5_DIM4)+l)*SPACE5_DIM5)+m;

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
#ifndef QAK
        seed+=(unsigned)clock();
#else /* QAK */
        seed=987909620;
#endif /* QAK */
        srand(seed);

#ifdef QAK
printf("test_num=%d, seed=%u\n",test_num,seed);
#endif /* QAK */
#ifndef QAK
        for(i=0; i<NHYPERSLABS; i++) {
#else /* QAK */
        for(i=0; i<2; i++) {
#endif /* QAK */
#ifdef QAK
printf("hyperslab=%d\n",i);
#endif /* QAK */
            /* Select random hyperslab location & size for selection */
            for(j=0; j<SPACE5_RANK; j++) {
                start[j]=rand()%dims1[j];
                count[j]=(rand()%(dims1[j]-start[j]))+1;
#ifdef QAK
printf("start[%d]=%d, count[%d]=%d (end[%d]=%d)\n",j,(int)start[j],j,(int)count[j],j,(int)(start[j]+count[j]-1));
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
printf("random I/O, before H5Dread(), npoints=%lu\n",(unsigned long)npoints);
{
    hsize_t blocks[128][2][SPACE5_RANK];
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
            TestErrPrintf("Random hyperslabs for seed %u failed!\n",seed);
            break;
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
    HDfree(wbuf);
    HDfree(rbuf);
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
    hsize_t     offset[3];             /* hyperslab offset in the file */
    hsize_t     count_out[3];          /* size of the hyperslab in memory */
    hsize_t     offset_out[3];         /* hyperslab offset in memory */
    int         i, j, k, status_n, rank;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Hyperslab I/O on Large Chunks\n"));

    /* Allocate the transfer buffers */
    data = HDmalloc(sizeof(short)*X*Y*Z);
    data_out = HDcalloc(NX*NY*NZ,sizeof(short));

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
                if(*tmpdata!=*tmpdata_out)
                    TestErrPrintf("Line %d: Error! j=%d, i=%d, k=%d, *tmpdata=%x, *tmpdata_out=%x\n",__LINE__,j,i,k,(unsigned)*tmpdata,(unsigned)*tmpdata_out);
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
    HDfree (data);
    HDfree (data_out);
}   /* test_select_hyper_chunk() */

/****************************************************************
**
**  test_select_point_chunk(): Test basic H5S (dataspace) selection code.
**      Tests combinations of hyperslab and point selections on
**      chunked datasets.
**
****************************************************************/
static void
test_select_point_chunk(void)
{
    hsize_t     dimsf[SPACE7_RANK];     /* dataset dimensions */
    hsize_t     chunk_dimsf[SPACE7_RANK] = {SPACE7_CHUNK_DIM1,SPACE7_CHUNK_DIM2};    /* chunk sizes */
    unsigned    *data;                   /* data to write */
    unsigned    *tmpdata;                /* data to write */

    /*
     * Data  and output buffer initialization.
     */
    hid_t       file, dataset;         /* handles */
    hid_t       dataspace;
    hid_t       pnt1_space;                     /* Dataspace to hold 1st point selection */
    hid_t       pnt2_space;                     /* Dataspace to hold 2nd point selection */
    hid_t       hyp1_space;                     /* Dataspace to hold 1st hyperslab selection */
    hid_t       hyp2_space;                     /* Dataspace to hold 2nd hyperslab selection */
    hid_t       dcpl;
    herr_t      ret;                            /* Generic return value */

    unsigned    *data_out;			/* output buffer */
#ifdef LATER
    unsigned    *tmpdata_out;			/* output buffer */
#endif /* LATER */

    hsize_t     start[SPACE7_RANK];             /* hyperslab offset */
    hsize_t     count[SPACE7_RANK];             /* size of the hyperslab */
    hsize_t     points[SPACE7_NPOINTS][SPACE7_RANK];   /* points for selection */
    unsigned    i, j;                           /* Local index variables */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Point Selections on Chunked Datasets\n"));

    /* Allocate the transfer buffers */
    data = (unsigned*)HDmalloc(sizeof(unsigned)*SPACE7_DIM1*SPACE7_DIM2);
    data_out = (unsigned*)HDcalloc(SPACE7_DIM1*SPACE7_DIM2,sizeof(unsigned));

    /*
     * Data buffer initialization.
     */
    tmpdata = data;
    for (i = 0; i < SPACE7_DIM1; i++)
	for (j = 0; j < SPACE7_DIM1; j++)
            *tmpdata++ = ((i*SPACE7_DIM2)+j)%256;

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * the default file creation properties and file
     * access properties.
     */
    file = H5Fcreate (FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create file dataspace */
    dimsf[0] = SPACE7_DIM1;
    dimsf[1] = SPACE7_DIM2;
    dataspace = H5Screate_simple (SPACE7_RANK, dimsf, NULL);
    CHECK(dataspace, FAIL, "H5Screate_simple");

    /*
     * Create a new dataset within the file using defined dataspace and
     * chunking properties.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");
    ret = H5Pset_chunk (dcpl, SPACE7_RANK, chunk_dimsf);
    CHECK(ret, FAIL, "H5Pset_chunk");
    dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_UCHAR, dataspace, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Create 1st point selection */
    pnt1_space = H5Scopy (dataspace);
    CHECK(pnt1_space, FAIL, "H5Scopy");

    points[0][0]=3;
    points[0][1]=3;
    points[1][0]=3;
    points[1][1]=8;
    points[2][0]=8;
    points[2][1]=3;
    points[3][0]=8;
    points[3][1]=8;
    points[4][0]=1;    /* In same chunk as point #0, but "earlier" in chunk */
    points[4][1]=1;
    points[5][0]=1;    /* In same chunk as point #1, but "earlier" in chunk */
    points[5][1]=6;
    points[6][0]=6;    /* In same chunk as point #2, but "earlier" in chunk */
    points[6][1]=1;
    points[7][0]=6;    /* In same chunk as point #3, but "earlier" in chunk */
    points[7][1]=6;
    ret = H5Sselect_elements(pnt1_space,H5S_SELECT_SET,SPACE7_NPOINTS,points);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Create 1st hyperslab selection */
    hyp1_space = H5Scopy (dataspace);
    CHECK(hyp1_space, FAIL, "H5Scopy");

    start[0]=2; start[1]=2;
    count[0]=4; count[1]=2;
    ret = H5Sselect_hyperslab(hyp1_space,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Write out data using 1st point selection for file & hyperslab for memory */
    ret=H5Dwrite(dataset,H5T_NATIVE_UINT,hyp1_space,pnt1_space,H5P_DEFAULT,data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Create 2nd point selection */
    pnt2_space = H5Scopy (dataspace);
    CHECK(pnt2_space, FAIL, "H5Scopy");

    points[0][0]=4;
    points[0][1]=4;
    points[1][0]=4;
    points[1][1]=9;
    points[2][0]=9;
    points[2][1]=4;
    points[3][0]=9;
    points[3][1]=9;
    points[4][0]=2;    /* In same chunk as point #0, but "earlier" in chunk */
    points[4][1]=2;
    points[5][0]=2;    /* In same chunk as point #1, but "earlier" in chunk */
    points[5][1]=7;
    points[6][0]=7;    /* In same chunk as point #2, but "earlier" in chunk */
    points[6][1]=2;
    points[7][0]=7;    /* In same chunk as point #3, but "earlier" in chunk */
    points[7][1]=7;
    ret = H5Sselect_elements(pnt2_space,H5S_SELECT_SET,SPACE7_NPOINTS,points);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Create 2nd hyperslab selection */
    hyp2_space = H5Scopy (dataspace);
    CHECK(hyp2_space, FAIL, "H5Scopy");

    start[0]=2; start[1]=4;
    count[0]=4; count[1]=2;
    ret = H5Sselect_hyperslab(hyp2_space,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Write out data using 2nd hyperslab selection for file & point for memory */
    ret=H5Dwrite(dataset,H5T_NATIVE_UINT,pnt2_space,hyp2_space,H5P_DEFAULT,data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close everything (except selections) */
    ret=H5Pclose (dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret=H5Sclose (dataspace);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Dclose (dataset);
    CHECK(ret, FAIL, "H5Dclose");
    ret=H5Fclose (file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file & dataset */
    file = H5Fopen (FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");
    dataset = H5Dopen (file, DATASETNAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Read data using 1st point selection for file and hyperslab for memory */
    ret=H5Dread(dataset,H5T_NATIVE_UINT,hyp1_space,pnt1_space,H5P_DEFAULT,data_out);
    CHECK(ret, FAIL, "H5Dread");

/* Verify data (later) */

    /* Read data using 2nd hyperslab selection for file and point for memory */
    ret=H5Dread(dataset,H5T_NATIVE_UINT,pnt2_space,hyp2_space,H5P_DEFAULT,data_out);
    CHECK(ret, FAIL, "H5Dread");

/* Verify data (later) */

    /* Close everything (inclusing selections) */
    ret=H5Sclose (pnt1_space);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Sclose (pnt2_space);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Sclose (hyp1_space);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Sclose (hyp2_space);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Dclose (dataset);
    CHECK(ret, FAIL, "H5Dclose");
    ret=H5Fclose (file);
    CHECK(ret, FAIL, "H5Fclose");

    free (data);
    free (data_out);
}   /* test_select_point_chunk() */

/****************************************************************
**
**  test_select_sclar_chunk(): Test basic H5S (dataspace) selection code.
**      Tests using a scalar dataspace (in memory) to access chunked datasets.
**
****************************************************************/
static void
test_select_scalar_chunk(void)
{
    hid_t file_id;              /* File ID */
    hid_t dcpl;                 /* Dataset creation property list */
    hid_t dsid;                 /* Dataset ID */
    hid_t sid;                  /* Dataspace ID */
    hid_t m_sid;                /* Memory dataspace */
    hsize_t dims[] = {2};       /* Dataset dimensions */
    hsize_t maxdims[] = {H5S_UNLIMITED};        /* Dataset maximum dimensions */
    hsize_t offset[] = {0};     /* Hyperslab start */
    hsize_t count[] = {1};      /* Hyperslab count */
    unsigned data = 2;          /* Data to write */
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspaces and Chunked Datasets\n"));

    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fcreate");

    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    dims[0] = 1024U;
    ret = H5Pset_chunk(dcpl, 1, dims);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Create 1-D dataspace */
    sid = H5Screate_simple(1, dims, maxdims);
    CHECK(sid, FAIL, "H5Screate_simple");

    dsid = H5Dcreate(file_id, "dset", H5T_NATIVE_UINT, sid, dcpl);
    CHECK(dsid, FAIL, "H5Dcreate");

    /* Select scalar area (offset 0, count 1) */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, offset, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create scalar memory dataspace */
    m_sid = H5Screate(H5S_SCALAR);
    CHECK(m_sid, FAIL, "H5Screate");

    /* Write out data using scalar dataspace for memory dataspace */
    ret = H5Dwrite (dsid, H5T_NATIVE_UINT, m_sid, sid, H5P_DEFAULT, &data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close resources */
    ret = H5Sclose(m_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Dclose(dsid);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Fclose (file_id);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_select_scalar_chunk() */

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
    hsize_t safe_start[2]={1,1};
    hsize_t safe_count[2]={1,1};
    hsize_t start[2];
    hsize_t dims[2],maxdims[2],size[2],count[2];

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Selection Validity\n"));

    MESSAGE(8, ( "Case 1 : sub_space is not a valid dataspace\n"));
    dims[0] = dims[1] = H5S_UNLIMITED;

    sub_space = H5Screate_simple(2,dims,NULL);
    VERIFY(sub_space, FAIL, "H5Screate_simple"); 

    valid=H5Sselect_valid(sub_space);
    VERIFY(valid, FAIL, "H5Sselect_valid");

    /* Set arrays and dataspace for the rest of the cases */
    count[0] = count[1] = 1;
    dims[0] = dims[1] = maxdims[0] = maxdims[1] = 10;

    main_space = H5Screate_simple(2,dims,maxdims);
    CHECK(main_space, FAIL, "H5Screate_simple");

    MESSAGE(8, ( "Case 2 : sub_space is a valid but closed dataspace\n"));
    sub_space = H5Scopy(main_space);
    CHECK(sub_space, FAIL, "H5Scopy");

    error=H5Sclose(sub_space);
    CHECK(error, FAIL, "H5Sclose");

    valid=H5Sselect_valid(sub_space);
    VERIFY(valid, FAIL, "H5Sselect_valid");

    MESSAGE(8, ( "Case 3 : in the dimensions\nTry offset (4,4) and size(6,6), the original space is of size (10,10)\n"));
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

    MESSAGE(8, ( "Case 4 : exceed dimensions by 1\nTry offset (5,5) and size(6,6), the original space is of size (10,10)\n"));
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

    MESSAGE(8, ( "Case 5 : exceed dimensions by 2\nTry offset (6,6) and size(6,6), the original space is of size (10,10)\n"));
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
}   /* test_select_valid() */

/****************************************************************
**
**  test_select_combine(): Test basic H5S (dataspace) selection code.
**      Tests combining "all" and "none" selections with hyperslab
**      operations.
**
****************************************************************/
static void
test_select_combine(void)
{
    hid_t base_id;      /* Base dataspace for test */
    hid_t all_id;       /* Dataspace for "all" selection */
    hid_t none_id;      /* Dataspace for "none" selection */
    hid_t space1;       /* Temporary dataspace #1 */
    hsize_t start[SPACE7_RANK];        /* Hyperslab start */
    hsize_t stride[SPACE7_RANK];        /* Hyperslab stride */
    hsize_t count[SPACE7_RANK];         /* Hyperslab count */
    hsize_t block[SPACE7_RANK];         /* Hyperslab block */
    hsize_t dims[SPACE7_RANK]={SPACE7_DIM1,SPACE7_DIM2};        /* Dimensions of dataspace */
    H5S_sel_type sel_type;      /* Selection type */
    hssize_t nblocks;   /* Number of hyperslab blocks */
    hsize_t blocks[128][2][SPACE7_RANK];    /* List of blocks */
    herr_t error;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Selection Combinations\n"));

    /* Create dataspace for dataset on disk */
    base_id = H5Screate_simple(SPACE7_RANK, dims, NULL);
    CHECK(base_id, FAIL, "H5Screate_simple");

    /* Copy base dataspace and set selection to "all" */
    all_id=H5Scopy(base_id);
    CHECK(all_id, FAIL, "H5Scopy");
    error=H5Sselect_all(all_id);
    CHECK(error, FAIL, "H5Sselect_all");
    sel_type=H5Sget_select_type(all_id);
    VERIFY(sel_type, H5S_SEL_ALL, "H5Sget_select_type");

    /* Copy base dataspace and set selection to "none" */
    none_id=H5Scopy(base_id);
    CHECK(none_id, FAIL, "H5Scopy");
    error=H5Sselect_none(none_id);
    CHECK(error, FAIL, "H5Sselect_all");
    sel_type=H5Sget_select_type(none_id);
    VERIFY(sel_type, H5S_SEL_NONE, "H5Sget_select_type");


    /* Copy "all" selection & space */
    space1=H5Scopy(all_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'OR' "all" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_OR,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that it's still "all" selection */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_ALL, "H5Sget_select_type");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");

    /* Copy "all" selection & space */
    space1=H5Scopy(all_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'AND' "all" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_AND,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is the same at the original block */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");

    /* Verify that there is only one block */
    nblocks=H5Sget_select_hyper_nblocks(space1);
    VERIFY(nblocks, 1, "H5Sget_select_hyper_nblocks");

    /* Retrieve the block defined */
    HDmemset(blocks,-1,sizeof(blocks));  /* Reset block list */
    error=H5Sget_select_hyper_blocklist(space1,(hsize_t)0,(hsize_t)nblocks,(hsize_t *)blocks);
    CHECK(error, FAIL, "H5Sget_select_hyper_blocklist");

    /* Verify that the correct block is defined */
    VERIFY(blocks[0][0][0], (hsize_t)start[0], "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][0][1], (hsize_t)start[1], "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][0], (block[0]-1), "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][1], (block[1]-1), "H5Sget_select_hyper_blocklist");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");

    /* Copy "all" selection & space */
    space1=H5Scopy(all_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'XOR' "all" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_XOR,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is an inversion of the original block */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");

    /* Verify that there are two blocks */
    nblocks=H5Sget_select_hyper_nblocks(space1);
    VERIFY(nblocks, 2, "H5Sget_select_hyper_nblocks");

    /* Retrieve the block defined */
    HDmemset(blocks,-1,sizeof(blocks));  /* Reset block list */
    error=H5Sget_select_hyper_blocklist(space1,(hsize_t)0,(hsize_t)nblocks,(hsize_t *)blocks);
    CHECK(error, FAIL, "H5Sget_select_hyper_blocklist");

    /* Verify that the correct block is defined */
    VERIFY(blocks[0][0][0], 0, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][0][1], 5, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][0], 4, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][1], 9, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[1][0][0], 5, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[1][0][1], 0, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[1][1][0], 9, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[1][1][1], 9, "H5Sget_select_hyper_blocklist");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");

    /* Copy "all" selection & space */
    space1=H5Scopy(all_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'NOTB' "all" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_NOTB,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is an inversion of the original block */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");

    /* Verify that there are two blocks */
    nblocks=H5Sget_select_hyper_nblocks(space1);
    VERIFY(nblocks, 2, "H5Sget_select_hyper_nblocks");

    /* Retrieve the block defined */
    HDmemset(blocks,-1,sizeof(blocks));  /* Reset block list */
    error=H5Sget_select_hyper_blocklist(space1,(hsize_t)0,(hsize_t)nblocks,(hsize_t *)blocks);
    CHECK(error, FAIL, "H5Sget_select_hyper_blocklist");

    /* Verify that the correct block is defined */
    VERIFY(blocks[0][0][0], 0, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][0][1], 5, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][0], 4, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][1], 9, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[1][0][0], 5, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[1][0][1], 0, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[1][1][0], 9, "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[1][1][1], 9, "H5Sget_select_hyper_blocklist");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");

    /* Copy "all" selection & space */
    space1=H5Scopy(all_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'NOTA' "all" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_NOTA,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is the "none" selection */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_NONE, "H5Sget_select_type");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");


    /* Copy "none" selection & space */
    space1=H5Scopy(none_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'OR' "none" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_OR,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is the same as the original hyperslab */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");

    /* Verify that there is only one block */
    nblocks=H5Sget_select_hyper_nblocks(space1);
    VERIFY(nblocks, 1, "H5Sget_select_hyper_nblocks");

    /* Retrieve the block defined */
    HDmemset(blocks,-1,sizeof(blocks));  /* Reset block list */
    error=H5Sget_select_hyper_blocklist(space1,(hsize_t)0,(hsize_t)nblocks,(hsize_t *)blocks);
    CHECK(error, FAIL, "H5Sget_select_hyper_blocklist");

    /* Verify that the correct block is defined */
    VERIFY(blocks[0][0][0], (hsize_t)start[0], "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][0][1], (hsize_t)start[1], "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][0], (block[0]-1), "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][1], (block[1]-1), "H5Sget_select_hyper_blocklist");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");

    /* Copy "none" selection & space */
    space1=H5Scopy(none_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'AND' "none" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_AND,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is the "none" selection */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_NONE, "H5Sget_select_type");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");

    /* Copy "none" selection & space */
    space1=H5Scopy(none_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'XOR' "none" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_XOR,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is the same as the original hyperslab */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");

    /* Verify that there is only one block */
    nblocks=H5Sget_select_hyper_nblocks(space1);
    VERIFY(nblocks, 1, "H5Sget_select_hyper_nblocks");

    /* Retrieve the block defined */
    HDmemset(blocks,-1,sizeof(blocks));  /* Reset block list */
    error=H5Sget_select_hyper_blocklist(space1,(hsize_t)0,(hsize_t)nblocks,(hsize_t *)blocks);
    CHECK(error, FAIL, "H5Sget_select_hyper_blocklist");

    /* Verify that the correct block is defined */
    VERIFY(blocks[0][0][0], (hsize_t)start[0], "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][0][1], (hsize_t)start[1], "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][0], (block[0]-1), "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][1], (block[1]-1), "H5Sget_select_hyper_blocklist");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");

    /* Copy "none" selection & space */
    space1=H5Scopy(none_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'NOTB' "none" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_NOTB,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is the "none" selection */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_NONE, "H5Sget_select_type");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");

    /* Copy "none" selection & space */
    space1=H5Scopy(none_id);
    CHECK(space1, FAIL, "H5Scopy");

    /* 'NOTA' "none" selection with another hyperslab */
    start[0]=start[1]=0;
    stride[0]=stride[1]=1;
    count[0]=count[1]=1;
    block[0]=block[1]=5;
    error=H5Sselect_hyperslab(space1,H5S_SELECT_NOTA,start,stride,count,block);
    CHECK(error, FAIL, "H5Sselect_hyperslab");

    /* Verify that the new selection is the same as the original hyperslab */
    sel_type=H5Sget_select_type(space1);
    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");

    /* Verify that there is only one block */
    nblocks=H5Sget_select_hyper_nblocks(space1);
    VERIFY(nblocks, 1, "H5Sget_select_hyper_nblocks");

    /* Retrieve the block defined */
    HDmemset(blocks,-1,sizeof(blocks));  /* Reset block list */
    error=H5Sget_select_hyper_blocklist(space1,(hsize_t)0,(hsize_t)nblocks,(hsize_t *)blocks);
    CHECK(error, FAIL, "H5Sget_select_hyper_blocklist");

    /* Verify that the correct block is defined */
    VERIFY(blocks[0][0][0], (hsize_t)start[0], "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][0][1], (hsize_t)start[1], "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][0], (block[0]-1), "H5Sget_select_hyper_blocklist");
    VERIFY(blocks[0][1][1], (block[1]-1), "H5Sget_select_hyper_blocklist");

    /* Close temporary dataspace */
    error=H5Sclose(space1);
    CHECK(error, FAIL, "H5Sclose");


    /* Close dataspaces */
    error=H5Sclose(base_id);
    CHECK(error, FAIL, "H5Sclose");

    error=H5Sclose(all_id);
    CHECK(error, FAIL, "H5Sclose");

    error=H5Sclose(none_id);
    CHECK(error, FAIL, "H5Sclose");
}   /* test_select_combine() */

/*
 * Typedef for iteration structure used in the fill value tests
 */
typedef struct {
    unsigned short fill_value;  /* The fill value to check */
    size_t curr_coord;          /* Current coordinate to examine */
    hsize_t *coords;            /* Pointer to selection's coordinates */
} fill_iter_info;

/****************************************************************
**
**  test_select_hyper_iter3(): Iterator for checking hyperslab iteration
**
****************************************************************/
herr_t
test_select_hyper_iter3(void *_elem, hid_t UNUSED type_id, unsigned ndim, const hsize_t *point, void *_operator_data)
{
    unsigned short *tbuf=(unsigned short *)_elem;     /* temporary buffer pointer */
    fill_iter_info *iter_info=(fill_iter_info *)_operator_data; /* Get the pointer to the iterator information */
    hsize_t *coord_ptr;        /* Pointer to the coordinate information for a point*/

    /* Check value in current buffer location */
    if(*tbuf!=iter_info->fill_value)
        return(-1);
    else {
        /* Check number of dimensions */
        if(ndim!=SPACE7_RANK)
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
    } /* end else */
}   /* end test_select_hyper_iter3() */

/****************************************************************
**
**  test_select_fill_all(): Test basic H5S (dataspace) selection code.
**      Tests filling "all" selections
**
****************************************************************/
static void
test_select_fill_all(void)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE7_DIM1, SPACE7_DIM2};
    int         fill_value;     /* Fill value */
    fill_iter_info iter_info;   /* Iterator information structure */
    hsize_t     points[SPACE7_DIM1*SPACE7_DIM2][SPACE7_RANK];   /* Coordinates of selection */
    unsigned short *wbuf,       /* buffer to write to disk */
               *tbuf;           /* temporary buffer pointer */
    int         i,j;            /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Filling 'all' Selections\n"));

    /* Allocate memory buffer */
    wbuf=HDmalloc(sizeof(unsigned short)*SPACE7_DIM1*SPACE7_DIM2);

    /* Initialize memory buffer */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            *tbuf++=(unsigned short)(i*SPACE7_DIM2)+j;

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE7_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Space defaults to "all" selection */

    /* Set fill value */
    fill_value=SPACE7_FILL;

    /* Fill selection in memory */
    ret=H5Dfill(&fill_value,H5T_NATIVE_INT,wbuf,H5T_NATIVE_USHORT,sid1);
    CHECK(ret, FAIL, "H5Dfill");

    /* Verify memory buffer the hard way... */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            if(*tbuf!=(unsigned short)fill_value)
                TestErrPrintf("Error! j=%d, i=%d, *tbuf=%x, fill_value=%x\n",j,i,(unsigned)*tbuf,(unsigned)fill_value);

    /* Set the coordinates of the selection */
    for(i=0; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++) {
            points[(i*SPACE7_DIM2)+j][0]=i;
            points[(i*SPACE7_DIM2)+j][1]=j;
        } /* end for */

    /* Initialize the iterator structure */
    iter_info.fill_value=SPACE7_FILL;
    iter_info.curr_coord=0;
    iter_info.coords=(hsize_t *)points;

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(wbuf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Free memory buffers */
    HDfree(wbuf);
}   /* test_select_fill_all() */

/****************************************************************
**
**  test_select_fill_point(): Test basic H5S (dataspace) selection code.
**      Tests filling "point" selections
**
****************************************************************/
static void
test_select_fill_point(hssize_t *offset)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE7_DIM1, SPACE7_DIM2};
    hssize_t    real_offset[SPACE7_RANK];       /* Actual offset to use */
    hsize_t     points[5][SPACE7_RANK] = {{2,4}, {3,8}, {8,4}, {7,5}, {7,7}};
    size_t      num_points=5;   /* Number of points selected */
    int         fill_value;     /* Fill value */
    fill_iter_info iter_info;   /* Iterator information structure */
    unsigned short *wbuf,       /* buffer to write to disk */
               *tbuf;           /* temporary buffer pointer */
    int         i,j,k;          /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Filling 'point' Selections\n"));

    /* Allocate memory buffer */
    wbuf=HDmalloc(sizeof(unsigned short)*SPACE7_DIM1*SPACE7_DIM2);

    /* Initialize memory buffer */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            *tbuf++=(unsigned short)(i*SPACE7_DIM2)+j;

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE7_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Select "point" selection */
    ret = H5Sselect_elements(sid1, H5S_SELECT_SET,num_points,points);
    CHECK(ret, FAIL, "H5Sselect_elements");

    if(offset!=NULL) {
        HDmemcpy(real_offset,offset,SPACE7_RANK*sizeof(hssize_t));

        /* Set offset, if provided */
        ret = H5Soffset_simple(sid1,real_offset);
        CHECK(ret, FAIL, "H5Soffset_simple");
    } /* end if */
    else
        HDmemset(real_offset,0,SPACE7_RANK*sizeof(hssize_t));

    /* Set fill value */
    fill_value=SPACE7_FILL;

    /* Fill selection in memory */
    ret=H5Dfill(&fill_value,H5T_NATIVE_INT,wbuf,H5T_NATIVE_USHORT,sid1);
    CHECK(ret, FAIL, "H5Dfill");

    /* Verify memory buffer the hard way... */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++, tbuf++) {
            for(k=0; k<(int)num_points; k++) {
                if(i==(int)(points[k][0]+real_offset[0]) && j==(int)(points[k][1]+real_offset[1])) {
                    if(*tbuf!=(unsigned short)fill_value)
                        TestErrPrintf("Error! j=%d, i=%d, *tbuf=%u, fill_value=%u\n",j,i,(unsigned)*tbuf,(unsigned)fill_value);
                    break;
                } /* end if */
            } /* end for */
            if(k==(int)num_points && *tbuf!=((unsigned short)(i*SPACE7_DIM2)+j))
                TestErrPrintf("Error! j=%d, i=%d, *tbuf=%u, should be: %u\n",j,i,(unsigned)*tbuf,(unsigned)((i*SPACE7_DIM2)+j));
        } /* end for */

    /* Initialize the iterator structure */
    iter_info.fill_value=SPACE7_FILL;
    iter_info.curr_coord=0;
    iter_info.coords=(hsize_t *)points;

    /* Add in the offset */
    for(i=0; i<(int)num_points; i++) {
        points[i][0]+=real_offset[0];
        points[i][1]+=real_offset[1];
    } /* end for */

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(wbuf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Free memory buffers */
    HDfree(wbuf);
}   /* test_select_fill_point() */

/****************************************************************
**
**  test_select_fill_hyper_simple(): Test basic H5S (dataspace) selection code.
**      Tests filling "simple" (i.e. one block) hyperslab selections
**
****************************************************************/
static void
test_select_fill_hyper_simple(hssize_t *offset)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE7_DIM1, SPACE7_DIM2};
    hssize_t    real_offset[SPACE7_RANK];       /* Actual offset to use */
    hsize_t     start[SPACE7_RANK];     /* Hyperslab start */
    hsize_t     count[SPACE7_RANK];     /* Hyperslab block size */
    size_t      num_points;     /* Number of points in selection */
    hsize_t     points[16][SPACE7_RANK];        /* Coordinates selected */
    int         fill_value;     /* Fill value */
    fill_iter_info iter_info;   /* Iterator information structure */
    unsigned short *wbuf,       /* buffer to write to disk */
               *tbuf;           /* temporary buffer pointer */
    int         i,j;            /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Filling Simple 'hyperslab' Selections\n"));

    /* Allocate memory buffer */
    wbuf=HDmalloc(sizeof(unsigned short)*SPACE7_DIM1*SPACE7_DIM2);

    /* Initialize memory buffer */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            *tbuf++=(unsigned short)(i*SPACE7_DIM2)+j;

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE7_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Select "hyperslab" selection */
    start[0]=3; start[1]=3;
    count[0]=4; count[1]=4;
    ret = H5Sselect_hyperslab(sid1, H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    if(offset!=NULL) {
        HDmemcpy(real_offset,offset,SPACE7_RANK*sizeof(hssize_t));

        /* Set offset, if provided */
        ret = H5Soffset_simple(sid1,real_offset);
        CHECK(ret, FAIL, "H5Soffset_simple");
    } /* end if */
    else
        HDmemset(real_offset,0,SPACE7_RANK*sizeof(hssize_t));

    /* Set fill value */
    fill_value=SPACE7_FILL;

    /* Fill selection in memory */
    ret=H5Dfill(&fill_value,H5T_NATIVE_INT,wbuf,H5T_NATIVE_USHORT,sid1);
    CHECK(ret, FAIL, "H5Dfill");

    /* Verify memory buffer the hard way... */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++, tbuf++) {
            if((i>=(int)(start[0]+real_offset[0]) && i<(int)(start[0]+count[0]+real_offset[0]))
                && (j>=(int)(start[1]+real_offset[1]) && j<(int)(start[1]+count[1]+real_offset[1]))) {
                    if(*tbuf!=(unsigned short)fill_value)
                        TestErrPrintf("Error! j=%d, i=%d, *tbuf=%u, fill_value=%u\n",j,i,(unsigned)*tbuf,(unsigned)fill_value);
                } /* end if */
            else {
                if(*tbuf!=((unsigned short)(i*SPACE7_DIM2)+j))
                    TestErrPrintf("Error! j=%d, i=%d, *tbuf=%u, should be: %u\n",j,i,(unsigned)*tbuf,(unsigned)((i*SPACE7_DIM2)+j));
            } /* end else */
        } /* end for */

    /* Initialize the iterator structure */
    iter_info.fill_value=SPACE7_FILL;
    iter_info.curr_coord=0;
    iter_info.coords=(hsize_t *)points;

    /* Set the coordinates of the selection (with the offset) */
    for(i=0, num_points=0; i<(int)count[0]; i++)
        for(j=0; j<(int)count[1]; j++, num_points++) {
            points[num_points][0]=i+start[0]+real_offset[0];
            points[num_points][1]=j+start[1]+real_offset[1];
        } /* end for */

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(wbuf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Free memory buffers */
    HDfree(wbuf);
}   /* test_select_fill_hyper_simple() */

/****************************************************************
**
**  test_select_fill_hyper_regular(): Test basic H5S (dataspace) selection code.
**      Tests filling "regular" (i.e. strided block) hyperslab selections
**
****************************************************************/
static void
test_select_fill_hyper_regular(hssize_t *offset)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE7_DIM1, SPACE7_DIM2};
    hssize_t    real_offset[SPACE7_RANK];       /* Actual offset to use */
    hsize_t     start[SPACE7_RANK];     /* Hyperslab start */
    hsize_t     stride[SPACE7_RANK];    /* Hyperslab stride size */
    hsize_t     count[SPACE7_RANK];     /* Hyperslab block count */
    hsize_t     block[SPACE7_RANK];     /* Hyperslab block size */
    hsize_t     points[16][SPACE7_RANK] = {
        {2,2}, {2,3}, {2,6}, {2,7},
        {3,2}, {3,3}, {3,6}, {3,7},
        {6,2}, {6,3}, {6,6}, {6,7},
        {7,2}, {7,3}, {7,6}, {7,7},
        };
    size_t      num_points=16;  /* Number of points selected */
    int         fill_value;     /* Fill value */
    fill_iter_info iter_info;   /* Iterator information structure */
    unsigned short *wbuf,       /* buffer to write to disk */
               *tbuf;           /* temporary buffer pointer */
    int         i,j,k;          /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Filling Regular 'hyperslab' Selections\n"));

    /* Allocate memory buffer */
    wbuf=HDmalloc(sizeof(unsigned short)*SPACE7_DIM1*SPACE7_DIM2);

    /* Initialize memory buffer */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            *tbuf++=(unsigned short)(i*SPACE7_DIM2)+j;

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE7_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Select "hyperslab" selection */
    start[0]=2; start[1]=2;
    stride[0]=4; stride[1]=4;
    count[0]=2; count[1]=2;
    block[0]=2; block[1]=2;
    ret = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    if(offset!=NULL) {
        HDmemcpy(real_offset,offset,SPACE7_RANK*sizeof(hssize_t));

        /* Set offset, if provided */
        ret = H5Soffset_simple(sid1,real_offset);
        CHECK(ret, FAIL, "H5Soffset_simple");
    } /* end if */
    else
        HDmemset(real_offset,0,SPACE7_RANK*sizeof(hssize_t));

    /* Set fill value */
    fill_value=SPACE7_FILL;

    /* Fill selection in memory */
    ret=H5Dfill(&fill_value,H5T_NATIVE_INT,wbuf,H5T_NATIVE_USHORT,sid1);
    CHECK(ret, FAIL, "H5Dfill");

    /* Verify memory buffer the hard way... */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++, tbuf++) {
            for(k=0; k<(int)num_points; k++) {
                if(i==(int)(points[k][0]+real_offset[0]) && j==(int)(points[k][1]+real_offset[1])) {
                    if(*tbuf!=(unsigned short)fill_value)
                        TestErrPrintf("Error! j=%d, i=%d, *tbuf=%u, fill_value=%u\n",j,i,(unsigned)*tbuf,(unsigned)fill_value);
                    break;
                } /* end if */
            } /* end for */
            if(k==(int)num_points && *tbuf!=((unsigned short)(i*SPACE7_DIM2)+j))
                TestErrPrintf("Error! j=%d, i=%d, *tbuf=%u, should be: %u\n",j,i,(unsigned)*tbuf,(unsigned)((i*SPACE7_DIM2)+j));
        } /* end for */

    /* Initialize the iterator structure */
    iter_info.fill_value=SPACE7_FILL;
    iter_info.curr_coord=0;
    iter_info.coords=(hsize_t *)points;

    /* Add in the offset */
    for(i=0; i<(int)num_points; i++) {
        points[i][0]+=real_offset[0];
        points[i][1]+=real_offset[1];
    } /* end for */

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(wbuf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Free memory buffers */
    HDfree(wbuf);
}   /* test_select_fill_hyper_regular() */

/****************************************************************
**
**  test_select_fill_hyper_irregular(): Test basic H5S (dataspace) selection code.
**      Tests filling "irregular" (i.e. combined blocks) hyperslab selections
**
****************************************************************/
static void
test_select_fill_hyper_irregular(hssize_t *offset)
{
    hid_t	sid1;           /* Dataspace ID */
    hsize_t	dims1[] = {SPACE7_DIM1, SPACE7_DIM2};
    hssize_t    real_offset[SPACE7_RANK];       /* Actual offset to use */
    hsize_t     start[SPACE7_RANK];     /* Hyperslab start */
    hsize_t     count[SPACE7_RANK];     /* Hyperslab block count */
    hsize_t     points[32][SPACE7_RANK] = { /* Yes, some of the are duplicated.. */
        {2,2}, {2,3}, {2,4}, {2,5},
        {3,2}, {3,3}, {3,4}, {3,5},
        {4,2}, {4,3}, {4,4}, {4,5},
        {5,2}, {5,3}, {5,4}, {5,5},
        {4,4}, {4,5}, {4,6}, {4,7},
        {5,4}, {5,5}, {5,6}, {5,7},
        {6,4}, {6,5}, {6,6}, {6,7},
        {7,4}, {7,5}, {7,6}, {7,7},
        };
    hsize_t     iter_points[28][SPACE7_RANK] = { /* Coordinates, as iterated through */
        {2,2}, {2,3}, {2,4}, {2,5},
        {3,2}, {3,3}, {3,4}, {3,5},
        {4,2}, {4,3}, {4,4}, {4,5}, {4,6}, {4,7},
        {5,2}, {5,3}, {5,4}, {5,5}, {5,6}, {5,7},
        {6,4}, {6,5}, {6,6}, {6,7},
        {7,4}, {7,5}, {7,6}, {7,7},
        };
    size_t      num_points=32;  /* Number of points selected */
    size_t      num_iter_points=28;  /* Number of resulting points */
    int         fill_value;     /* Fill value */
    fill_iter_info iter_info;   /* Iterator information structure */
    unsigned short *wbuf,       /* buffer to write to disk */
               *tbuf;           /* temporary buffer pointer */
    int         i,j,k;          /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Filling Irregular 'hyperslab' Selections\n"));

    /* Allocate memory buffer */
    wbuf=HDmalloc(sizeof(unsigned short)*SPACE7_DIM1*SPACE7_DIM2);

    /* Initialize memory buffer */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            *tbuf++=(unsigned short)(i*SPACE7_DIM2)+j;

    /* Create dataspace for dataset on disk */
    sid1 = H5Screate_simple(SPACE7_RANK, dims1, NULL);
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
        HDmemcpy(real_offset,offset,SPACE7_RANK*sizeof(hssize_t));

        /* Set offset, if provided */
        ret = H5Soffset_simple(sid1,real_offset);
        CHECK(ret, FAIL, "H5Soffset_simple");
    } /* end if */
    else
        HDmemset(real_offset,0,SPACE7_RANK*sizeof(hssize_t));

    /* Set fill value */
    fill_value=SPACE7_FILL;

    /* Fill selection in memory */
    ret=H5Dfill(&fill_value,H5T_NATIVE_INT,wbuf,H5T_NATIVE_USHORT,sid1);
    CHECK(ret, FAIL, "H5Dfill");

    /* Verify memory buffer the hard way... */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++, tbuf++) {
            for(k=0; k<(int)num_points; k++) {
                if(i==(int)(points[k][0]+real_offset[0]) && j==(int)(points[k][1]+real_offset[1])) {
                    if(*tbuf!=(unsigned short)fill_value)
                        TestErrPrintf("Error! j=%d, i=%d, *tbuf=%u, fill_value=%u\n",j,i,(unsigned)*tbuf,(unsigned)fill_value);
                    break;
                } /* end if */
            } /* end for */
            if(k==(int)num_points && *tbuf!=((unsigned short)(i*SPACE7_DIM2)+j))
                TestErrPrintf("Error! j=%d, i=%d, *tbuf=%u, should be: %u\n",j,i,(unsigned)*tbuf,(unsigned)((i*SPACE7_DIM2)+j));
        } /* end for */

    /* Initialize the iterator structure */
    iter_info.fill_value=SPACE7_FILL;
    iter_info.curr_coord=0;
    iter_info.coords=(hsize_t *)iter_points;

    /* Add in the offset */
    for(i=0; i<(int)num_iter_points; i++) {
        iter_points[i][0]+=real_offset[0];
        iter_points[i][1]+=real_offset[1];
    } /* end for */

    /* Iterate through selection, verifying correct data */
    ret = H5Diterate(wbuf,H5T_NATIVE_USHORT,sid1,test_select_hyper_iter3,&iter_info);
    CHECK(ret, FAIL, "H5Diterate");

    /* Close dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Free memory buffers */
    HDfree(wbuf);
}   /* test_select_fill_hyper_irregular() */

/****************************************************************
**
**  test_select_none(): Test basic H5S (dataspace) selection code.
**      Tests I/O on 0-sized point selections
**
****************************************************************/
static void
test_select_none(void)
{
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid1,sid2;	/* Dataspace ID			*/
    hsize_t	dims1[] = {SPACE7_DIM1, SPACE7_DIM2};
    hsize_t	dims2[] = {SPACE7_DIM1, SPACE7_DIM2};
    uint8_t    *wbuf,           /* buffer to write to disk */
               *rbuf,           /* buffer to read from disk */
               *tbuf;           /* temporary buffer pointer */
    int         i,j;            /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing I/O on 0-sized Selections\n"));

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(uint8_t)*SPACE7_DIM1*SPACE7_DIM2);
    rbuf=malloc(sizeof(uint8_t)*SPACE7_DIM1*SPACE7_DIM2);

    /* Initialize write buffer */
    for(i=0, tbuf=wbuf; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++)
            *tbuf++=(uint8_t)((i*SPACE7_DIM2)+j);

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE7_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE7_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Make "none" selection in both disk and memory datasets */
    ret = H5Sselect_none(sid1);
    CHECK(ret, FAIL, "H5Sselect_none");

    ret = H5Sselect_none(sid2);
    CHECK(ret, FAIL, "H5Sselect_none");

    /* Attempt to read "nothing" from disk (before space is allocated) */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Write "nothing" to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Write "nothing" to disk (with a datatype conversion :-) */
    ret=H5Dwrite(dataset,H5T_NATIVE_INT,sid2,sid1,H5P_DEFAULT,wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Write "nothing" to disk (with NULL buffer argument) */
    ret=H5Dwrite(dataset,H5T_NATIVE_INT,sid2,sid1,H5P_DEFAULT,NULL);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read "nothing" from disk (with NULL buffer argument) */
    ret=H5Dread(dataset,H5T_NATIVE_INT,sid2,sid1,H5P_DEFAULT,NULL);
    CHECK(ret, FAIL, "H5Dread");

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
}   /* test_select_none() */

/****************************************************************
**
**  test_scalar_select(): Test basic H5S (dataspace) selection code.
**      Tests selections on scalar dataspaces
**
****************************************************************/
static void
test_scalar_select(void)
{
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid1,sid2;	/* Dataspace ID			*/
    hsize_t	dims2[] = {SPACE7_DIM1, SPACE7_DIM2};
    hsize_t	coord1[SPACE7_RANK]; /* Coordinates for point selection */
    hsize_t     start[SPACE7_RANK]; /* Hyperslab start */
    hsize_t     count[SPACE7_RANK]; /* Hyperslab block count */
    uint8_t    *wbuf_uint8,     /* buffer to write to disk */
                rval_uint8,     /* value read back in */
               *tbuf_uint8;     /* temporary buffer pointer */
    unsigned short *wbuf_ushort,/* another buffer to write to disk */
                rval_ushort,    /* value read back in */
               *tbuf_ushort;    /* temporary buffer pointer */
    int         i,j;            /* Counters */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing I/O on Selections in Scalar Dataspaces\n"));

    /* Allocate write & read buffers */
    wbuf_uint8=HDmalloc(sizeof(uint8_t)*SPACE7_DIM1*SPACE7_DIM2);
    wbuf_ushort=HDmalloc(sizeof(unsigned short)*SPACE7_DIM1*SPACE7_DIM2);

    /* Initialize write buffers */
    for(i=0, tbuf_uint8=wbuf_uint8, tbuf_ushort=wbuf_ushort; i<SPACE7_DIM1; i++)
        for(j=0; j<SPACE7_DIM2; j++) {
            *tbuf_uint8++=(uint8_t)((i*SPACE7_DIM2)+j);
            *tbuf_ushort++=(unsigned short)((j*SPACE7_DIM2)+i);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate(H5S_SCALAR);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create dataspace for writing buffer */
    sid2 = H5Screate_simple(SPACE7_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Select one element in memory with a point selection */
    coord1[0]=0; coord1[1]= 2;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,1,&coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Write single point to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf_uint8);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read scalar element from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid1,sid1,H5P_DEFAULT,&rval_uint8);
    CHECK(ret, FAIL, "H5Dread");

    /* Check value read back in */
    if(rval_uint8!=*(wbuf_uint8+2))
        TestErrPrintf("Error! rval=%u, should be: *(wbuf+2)=%u\n",(unsigned)rval_uint8,(unsigned)*(wbuf_uint8+2));

    /* Write single point to disk (with a datatype conversion) */
    ret=H5Dwrite(dataset,H5T_NATIVE_USHORT,sid2,sid1,H5P_DEFAULT,wbuf_ushort);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read scalar element from disk */
    ret=H5Dread(dataset,H5T_NATIVE_USHORT,sid1,sid1,H5P_DEFAULT,&rval_ushort);
    CHECK(ret, FAIL, "H5Dread");

    /* Check value read back in */
    if(rval_ushort!=*(wbuf_ushort+2))
        TestErrPrintf("Error! rval=%u, should be: *(wbuf+2)=%u\n",(unsigned)rval_ushort,(unsigned)*(wbuf_ushort+2));

    /* Select one element in memory with a hyperslab selection */
    start[0]=4; start[1]=3;
    count[0]=1; count[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,NULL,count,NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Write single hyperslab element to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf_uint8);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read scalar element from disk */
    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid1,sid1,H5P_DEFAULT,&rval_uint8);
    CHECK(ret, FAIL, "H5Dread");

    /* Check value read back in */
    if(rval_uint8!=*(wbuf_uint8+(SPACE7_DIM2*4)+3))
        TestErrPrintf("Error! rval=%u, should be: *(wbuf+(SPACE7_DIM2*4)+3)=%u\n",(unsigned)rval_uint8,(unsigned)*(wbuf_uint8+(SPACE7_DIM2*4)+3));

    /* Write single hyperslab element to disk (with a datatype conversion) */
    ret=H5Dwrite(dataset,H5T_NATIVE_USHORT,sid2,sid1,H5P_DEFAULT,wbuf_ushort);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Read scalar element from disk */
    ret=H5Dread(dataset,H5T_NATIVE_USHORT,sid1,sid1,H5P_DEFAULT,&rval_ushort);
    CHECK(ret, FAIL, "H5Dread");

    /* Check value read back in */
    if(rval_ushort!=*(wbuf_ushort+(SPACE7_DIM2*4)+3))
        TestErrPrintf("Error! rval=%u, should be: *(wbuf+(SPACE7_DIM2*4)+3)=%u\n",(unsigned)rval_ushort,(unsigned)*(wbuf_ushort+(SPACE7_DIM2*4)+3));

    /* Select no elements in memory & file with "none" selections */
    ret = H5Sselect_none(sid1);
    CHECK(ret, FAIL, "H5Sselect_none");

    ret = H5Sselect_none(sid2);
    CHECK(ret, FAIL, "H5Sselect_none");

    /* Write no data to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_UCHAR,sid2,sid1,H5P_DEFAULT,wbuf_uint8);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Write no data to disk (with a datatype conversion) */
    ret=H5Dwrite(dataset,H5T_NATIVE_USHORT,sid2,sid1,H5P_DEFAULT,wbuf_ushort);
    CHECK(ret, FAIL, "H5Dwrite");

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
    HDfree(wbuf_uint8);
    HDfree(wbuf_ushort);
}   /* test_scalar_select() */

/****************************************************************
**
**  test_scalar_select2(): Tests selections on scalar dataspace,
**	verify H5Shyperslab and H5Sselect_elements fails for
**	scalar dataspace.
**
****************************************************************/
static void
test_scalar_select2(void)
{
    hid_t	sid;		/* Dataspace ID			*/
    hsize_t	coord1[1]; /* Coordinates for point selection */
    hsize_t     start[1]; /* Hyperslab start */
    hsize_t     count[1]; /* Hyperslab block count */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(6, ("Testing Selections in Scalar Dataspaces\n"));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Select one element in memory with a point selection */
    coord1[0]=0;
    H5E_BEGIN_TRY {
    	ret = H5Sselect_elements(sid,H5S_SELECT_SET,1,&coord1);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sselect_elements");

    /* Select one element in memory with a hyperslab selection */
    start[0]=0;
    count[0]=0;
    H5E_BEGIN_TRY {
    	ret = H5Sselect_hyperslab(sid,H5S_SELECT_SET,start,NULL,count,NULL);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sselect_hyperslab");

    /* Select no elements in memory & file with "none" selection */
    ret = H5Sselect_none(sid);
    CHECK(ret, FAIL, "H5Sselect_none");

    /* Select all elements in memory & file with "all" selection */
    ret = H5Sselect_all(sid);
    CHECK(ret, FAIL, "H5Sselect_none");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_scalar_select2() */

/****************************************************************
**
**  test_shape_same(): Tests selections on dataspace, verify that
**	"shape same" routine is working correctly.
**
****************************************************************/
static void
test_shape_same(void)
{
    hid_t	all_sid;	/* Dataspace ID	with "all" selection */
    hid_t	none_sid;	/* Dataspace ID	with "none" selection */
    hid_t	single_pt_sid;	/* Dataspace ID	with single point selection */
    hid_t	mult_pt_sid;	/* Dataspace ID	with multiple point selection */
    hid_t	single_hyper_sid;	/* Dataspace ID	with single block hyperslab selection */
    hid_t	single_hyper_all_sid;	/* Dataspace ID	with single block hyperslab
                                         * selection that is the entire dataspace
                                         */
    hid_t	single_hyper_pt_sid;	/* Dataspace ID	with single block hyperslab
                                         * selection that is the same as the single
                                         * point selection
                                         */
    hid_t	regular_hyper_sid;	/* Dataspace ID	with regular hyperslab selection */
    hid_t	irreg_hyper_sid;	/* Dataspace ID	with irregular hyperslab selection */
    hid_t	none_hyper_sid;	/* Dataspace ID	with "no hyperslabs" selection */
    hid_t	tmp_sid;	/* Temporary dataspace ID */
    hsize_t	dims[] = {SPACE9_DIM1, SPACE9_DIM2};
    hsize_t	coord1[1][SPACE2_RANK]; /* Coordinates for single point selection */
    hsize_t	coord2[SPACE9_DIM2][SPACE9_RANK]; /* Coordinates for multiple point selection */
    hsize_t     start[SPACE9_RANK]; /* Hyperslab start */
    hsize_t     stride[SPACE9_RANK]; /* Hyperslab stride */
    hsize_t     count[SPACE9_RANK]; /* Hyperslab block count */
    hsize_t     block[SPACE9_RANK]; /* Hyperslab block size */
    unsigned    u,v;            /* Local index variables */
    htri_t	check;		/* Shape comparison return value */
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(6, ("Testing Same Shape Comparisons\n"));
    assert(SPACE9_DIM2>=POINT1_NPOINTS);

    /* Create dataspace for "all" selection */
    all_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(all_sid, FAIL, "H5Screate_simple");

    /* Select entire extent for dataspace */
    ret = H5Sselect_all(all_sid);
    CHECK(ret, FAIL, "H5Sselect_all");

    /* Create dataspace for "none" selection */
    none_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(none_sid, FAIL, "H5Screate_simple");

    /* Un-Select entire extent for dataspace */
    ret = H5Sselect_none(none_sid);
    CHECK(ret, FAIL, "H5Sselect_none");

    /* Create dataspace for single point selection */
    single_pt_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(single_pt_sid, FAIL, "H5Screate_simple");

    /* Select sequence of ten points for multiple point selection */
    coord1[0][0]=2; coord1[0][1]=2;
    ret = H5Sselect_elements(single_pt_sid,H5S_SELECT_SET,1,coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Create dataspace for multiple point selection */
    mult_pt_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(mult_pt_sid, FAIL, "H5Screate_simple");

    /* Select sequence of ten points for multiple point selection */
    coord2[0][0]=2; coord2[0][1]=2;
    coord2[1][0]=7; coord2[1][1]=2;
    coord2[2][0]=1; coord2[2][1]=4;
    coord2[3][0]=2; coord2[3][1]=6;
    coord2[4][0]=0; coord2[4][1]=8;
    coord2[5][0]=3; coord2[5][1]=2;
    coord2[6][0]=4; coord2[6][1]=4;
    coord2[7][0]=1; coord2[7][1]=0;
    coord2[8][0]=5; coord2[8][1]=1;
    coord2[9][0]=9; coord2[9][1]=3;
    ret = H5Sselect_elements(mult_pt_sid,H5S_SELECT_SET,POINT1_NPOINTS,coord2);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Create dataspace for single hyperslab selection */
    single_hyper_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(single_hyper_sid, FAIL, "H5Screate_simple");

    /* Select 10x10 hyperslab for single hyperslab selection  */
    start[0]=1; start[1]=1;
    stride[0]=1; stride[1]=1;
    count[0]=1; count[1]=1;
    block[0]=(SPACE9_DIM1-2); block[1]=(SPACE9_DIM2-2);
    ret = H5Sselect_hyperslab(single_hyper_sid,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create dataspace for single hyperslab selection with entire extent selected */
    single_hyper_all_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(single_hyper_all_sid, FAIL, "H5Screate_simple");

    /* Select entire extent for hyperslab selection */
    start[0]=0; start[1]=0;
    stride[0]=1; stride[1]=1;
    count[0]=1; count[1]=1;
    block[0]=SPACE9_DIM1; block[1]=SPACE9_DIM2;
    ret = H5Sselect_hyperslab(single_hyper_all_sid,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create dataspace for single hyperslab selection with single point selected */
    single_hyper_pt_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(single_hyper_pt_sid, FAIL, "H5Screate_simple");

    /* Select entire extent for hyperslab selection */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=1; count[1]=1;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(single_hyper_pt_sid,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create dataspace for regular hyperslab selection */
    regular_hyper_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(regular_hyper_sid, FAIL, "H5Screate_simple");

    /* Select regular, strided hyperslab selection */
    start[0]=2; start[1]=2;
    stride[0]=2; stride[1]=2;
    count[0]=5; count[1]=2;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(regular_hyper_sid,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create dataspace for irregular hyperslab selection */
    irreg_hyper_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(irreg_hyper_sid, FAIL, "H5Screate_simple");

    /* Create irregular hyperslab selection by OR'ing two blocks together */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=1; count[1]=1;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(irreg_hyper_sid,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    start[0]=4; start[1]=4;
    stride[0]=1; stride[1]=1;
    count[0]=1; count[1]=1;
    block[0]=3; block[1]=3;
    ret = H5Sselect_hyperslab(irreg_hyper_sid,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create dataspace for "no" hyperslab selection */
    none_hyper_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
    CHECK(none_hyper_sid, FAIL, "H5Screate_simple");

    /* Create "no" hyperslab selection by XOR'ing same blocks together */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=1; count[1]=1;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(none_hyper_sid,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    ret = H5Sselect_hyperslab(none_hyper_sid,H5S_SELECT_XOR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Compare "all" selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(all_sid,all_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(all_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(all_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "none" selection */
        check=H5S_select_shape_same_test(all_sid,none_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against single point selection */
        check=H5S_select_shape_same_test(all_sid,single_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against multiple point selection */
        check=H5S_select_shape_same_test(all_sid,mult_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "plain" single hyperslab selection */
        check=H5S_select_shape_same_test(all_sid,single_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "all" single hyperslab selection */
        check=H5S_select_shape_same_test(all_sid,single_hyper_all_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against "single point" single hyperslab selection */
        check=H5S_select_shape_same_test(all_sid,single_hyper_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against regular, strided hyperslab selection */
        check=H5S_select_shape_same_test(all_sid,regular_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against irregular hyperslab selection */
        check=H5S_select_shape_same_test(all_sid,irreg_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(all_sid,none_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

    /* Compare "none" selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(none_sid,none_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(none_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(none_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "all" selection */
        check=H5S_select_shape_same_test(none_sid,all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against single point selection */
        check=H5S_select_shape_same_test(none_sid,single_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against multiple point selection */
        check=H5S_select_shape_same_test(none_sid,mult_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "plain" single hyperslab selection */
        check=H5S_select_shape_same_test(none_sid,single_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "all" single hyperslab selection */
        check=H5S_select_shape_same_test(none_sid,single_hyper_all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "single point" single hyperslab selection */
        check=H5S_select_shape_same_test(none_sid,single_hyper_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against regular, strided hyperslab selection */
        check=H5S_select_shape_same_test(none_sid,regular_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against irregular hyperslab selection */
        check=H5S_select_shape_same_test(none_sid,irreg_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(none_sid,none_hyper_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

    /* Compare single point selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(single_pt_sid,single_pt_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(single_pt_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(single_pt_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "all" selection */
        check=H5S_select_shape_same_test(single_pt_sid,all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "none" selection */
        check=H5S_select_shape_same_test(single_pt_sid,none_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against multiple point selection */
        check=H5S_select_shape_same_test(single_pt_sid,mult_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "plain" single hyperslab selection */
        check=H5S_select_shape_same_test(single_pt_sid,single_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "all" single hyperslab selection */
        check=H5S_select_shape_same_test(single_pt_sid,single_hyper_all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "single point" single hyperslab selection */
        check=H5S_select_shape_same_test(single_pt_sid,single_hyper_pt_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against regular, strided hyperslab selection */
        check=H5S_select_shape_same_test(single_pt_sid,regular_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against irregular hyperslab selection */
        check=H5S_select_shape_same_test(single_pt_sid,irreg_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(single_pt_sid,none_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

    /* Compare multiple point selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(mult_pt_sid,mult_pt_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(mult_pt_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(mult_pt_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "all" selection */
        check=H5S_select_shape_same_test(mult_pt_sid,all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "none" selection */
        check=H5S_select_shape_same_test(mult_pt_sid,none_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against single point selection */
        check=H5S_select_shape_same_test(mult_pt_sid,single_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "plain" single hyperslab selection */
        check=H5S_select_shape_same_test(mult_pt_sid,single_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "all" single hyperslab selection */
        check=H5S_select_shape_same_test(mult_pt_sid,single_hyper_all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "single point" single hyperslab selection */
        check=H5S_select_shape_same_test(mult_pt_sid,single_hyper_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against regular, strided hyperslab selection */
        check=H5S_select_shape_same_test(mult_pt_sid,regular_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against irregular hyperslab selection */
        check=H5S_select_shape_same_test(mult_pt_sid,irreg_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(mult_pt_sid,none_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

    /* Compare single "normal" hyperslab selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(single_hyper_sid,single_hyper_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(single_hyper_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(single_hyper_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "all" selection */
        check=H5S_select_shape_same_test(single_hyper_sid,all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "none" selection */
        check=H5S_select_shape_same_test(single_hyper_sid,none_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against single point selection */
        check=H5S_select_shape_same_test(single_hyper_sid,single_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against multiple point selection */
        check=H5S_select_shape_same_test(single_hyper_sid,mult_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "all" single hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_sid,single_hyper_all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "single point" single hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_sid,single_hyper_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against regular, strided hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_sid,regular_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against irregular hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_sid,irreg_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_sid,none_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

#ifdef NOT_YET
/* In theory, these two selections are the same shape, but the
 * H5S_select_shape_same() routine is just not this sophisticated yet and it
 * would take too much effort to make this work.  The worst case is that the
 * non-optimized chunk mapping routines will be invoked instead of the more
 * optimized routines, so this only hurts performance, not correctness
 */
        /* Construct point selection which matches "plain" hyperslab selection */
            /* Create dataspace for point selection */
            tmp_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
            CHECK(tmp_sid, FAIL, "H5Screate_simple");

            /* Select sequence of points for point selection */
            for(u=1; u<(SPACE9_DIM1-1); u++) {
                for(v=1; v<(SPACE9_DIM2-1); v++) {
                    coord2[v-1][0]=u; coord2[v-1][1]=v;
                } /* end for */
                ret = H5Sselect_elements(tmp_sid,H5S_SELECT_APPEND,(SPACE9_DIM2-2),coord2);
                CHECK(ret, FAIL, "H5Sselect_elements");
            } /* end for */

            /* Compare against hyperslab selection */
            check=H5S_select_shape_same_test(single_hyper_sid,tmp_sid);
            VERIFY(check, TRUE, "H5S_select_shape_same_test");

            ret = H5Sclose(tmp_sid);
            CHECK(ret, FAIL, "H5Sclose");
#endif /* NOT_YET */

        /* Construct hyperslab selection which matches "plain" hyperslab selection */
            /* Create dataspace for hyperslab selection */
            tmp_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
            CHECK(tmp_sid, FAIL, "H5Screate_simple");

            /* Un-select entire extent */
            ret = H5Sselect_none(tmp_sid);
            CHECK(ret, FAIL, "H5Sselect_none");

            /* Select sequence of rows for hyperslab selection */
            for(u=1; u<(SPACE9_DIM1-1); u++) {
                start[0]=u; start[1]=1;
                stride[0]=1; stride[1]=1;
                count[0]=1; count[1]=1;
                block[0]=1; block[1]=(SPACE9_DIM2-2);
                ret = H5Sselect_hyperslab(tmp_sid,H5S_SELECT_OR,start,stride,count,block);
                CHECK(ret, FAIL, "H5Sselect_hyperslab");
            } /* end for */

            /* Compare against hyperslab selection */
            check=H5S_select_shape_same_test(single_hyper_sid,tmp_sid);
            VERIFY(check, TRUE, "H5S_select_shape_same_test");

            ret = H5Sclose(tmp_sid);
            CHECK(ret, FAIL, "H5Sclose");

    /* Compare single "all" hyperslab selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(single_hyper_all_sid,single_hyper_all_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(single_hyper_all_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(single_hyper_all_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "all" selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,all_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against "none" selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,none_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against single point selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,single_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against multiple point selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,mult_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "plain" single hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,single_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "single point" single hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,single_hyper_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against regular, strided hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,regular_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against irregular hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,irreg_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_all_sid,none_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

#ifdef NOT_YET
/* In theory, these two selections are the same shape, but the
 * H5S_select_shape_same() routine is just not this sophisticated yet and it
 * would take too much effort to make this work.  The worst case is that the
 * non-optimized chunk mapping routines will be invoked instead of the more
 * optimized routines, so this only hurts performance, not correctness
 */
        /* Construct point selection which matches "all" hyperslab selection */
            /* Create dataspace for point selection */
            tmp_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
            CHECK(tmp_sid, FAIL, "H5Screate_simple");

            /* Select sequence of points for point selection */
            for(u=0; u<SPACE9_DIM1; u++) {
                for(v=0; v<SPACE9_DIM2; v++) {
                    coord2[v][0]=u; coord2[v][1]=v;
                } /* end for */
                ret = H5Sselect_elements(tmp_sid,H5S_SELECT_APPEND,SPACE9_DIM2,coord2);
                CHECK(ret, FAIL, "H5Sselect_elements");
            } /* end for */

            /* Compare against hyperslab selection */
            check=H5S_select_shape_same_test(single_hyper_all_sid,tmp_sid);
            VERIFY(check, TRUE, "H5S_select_shape_same_test");

            ret = H5Sclose(tmp_sid);
            CHECK(ret, FAIL, "H5Sclose");
#endif /* NOT_YET */

        /* Construct hyperslab selection which matches "all" hyperslab selection */
            /* Create dataspace for hyperslab selection */
            tmp_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
            CHECK(tmp_sid, FAIL, "H5Screate_simple");

            /* Un-select entire extent */
            ret = H5Sselect_none(tmp_sid);
            CHECK(ret, FAIL, "H5Sselect_none");

            /* Select sequence of rows for hyperslab selection */
            for(u=0; u<SPACE9_DIM2; u++) {
                start[0]=u; start[1]=0;
                stride[0]=1; stride[1]=1;
                count[0]=1; count[1]=1;
                block[0]=1; block[1]=SPACE9_DIM2;
                ret = H5Sselect_hyperslab(tmp_sid,H5S_SELECT_OR,start,stride,count,block);
                CHECK(ret, FAIL, "H5Sselect_hyperslab");
            } /* end for */

            /* Compare against hyperslab selection */
            check=H5S_select_shape_same_test(single_hyper_all_sid,tmp_sid);
            VERIFY(check, TRUE, "H5S_select_shape_same_test");

            ret = H5Sclose(tmp_sid);
            CHECK(ret, FAIL, "H5Sclose");

    /* Compare single "point" hyperslab selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,single_hyper_pt_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(single_hyper_pt_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(single_hyper_pt_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "all" selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "none" selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,none_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against single point selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,single_pt_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against multiple point selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,mult_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "plain" single hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,single_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "all" single hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,single_hyper_all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against regular, strided hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,regular_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against irregular hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,irreg_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(single_hyper_pt_sid,none_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

    /* Compare regular, strided hyperslab selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(regular_hyper_sid,regular_hyper_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(regular_hyper_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(regular_hyper_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "all" selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "none" selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,none_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against single point selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,single_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against multiple point selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,mult_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "plain" single hyperslab selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,single_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "all" single hyperslab selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,single_hyper_all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "single point" single hyperslab selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,single_hyper_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against irregular hyperslab selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,irreg_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(regular_hyper_sid,none_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Construct point selection which matches regular, strided hyperslab selection */
            /* Create dataspace for point selection */
            tmp_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
            CHECK(tmp_sid, FAIL, "H5Screate_simple");

            /* Select sequence of points for point selection */
            for(u=2; u<11; u+=2) {
                for(v=0; v<2; v++) {
                    coord2[v][0]=u; coord2[v][1]=(v*2)+2;
                } /* end for */
                ret = H5Sselect_elements(tmp_sid,H5S_SELECT_APPEND,2,coord2);
                CHECK(ret, FAIL, "H5Sselect_elements");
            } /* end for */

            /* Compare against hyperslab selection */
            check=H5S_select_shape_same_test(regular_hyper_sid,tmp_sid);
            VERIFY(check, TRUE, "H5S_select_shape_same_test");

            ret = H5Sclose(tmp_sid);
            CHECK(ret, FAIL, "H5Sclose");

        /* Construct hyperslab selection which matches regular, strided hyperslab selection */
            /* Create dataspace for hyperslab selection */
            tmp_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
            CHECK(tmp_sid, FAIL, "H5Screate_simple");

            /* Un-select entire extent */
            ret = H5Sselect_none(tmp_sid);
            CHECK(ret, FAIL, "H5Sselect_none");

            /* Select sequence of rows for hyperslab selection */
            for(u=2; u<11; u+=2) {
                start[0]=u; start[1]=3;
                stride[0]=1; stride[1]=2;
                count[0]=1; count[1]=2;
                block[0]=1; block[1]=1;
                ret = H5Sselect_hyperslab(tmp_sid,H5S_SELECT_OR,start,stride,count,block);
                CHECK(ret, FAIL, "H5Sselect_hyperslab");
            } /* end for */

            /* Compare against hyperslab selection */
            check=H5S_select_shape_same_test(regular_hyper_sid,tmp_sid);
            VERIFY(check, TRUE, "H5S_select_shape_same_test");

            ret = H5Sclose(tmp_sid);
            CHECK(ret, FAIL, "H5Sclose");

        /* Construct regular hyperslab selection with an offset which matches regular, strided hyperslab selection */
            /* Create dataspace for hyperslab selection */
            tmp_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
            CHECK(tmp_sid, FAIL, "H5Screate_simple");

            /* Select regular, strided hyperslab selection at an offset */
            start[0]=1; start[1]=1;
            stride[0]=2; stride[1]=2;
            count[0]=5; count[1]=2;
            block[0]=1; block[1]=1;
            ret = H5Sselect_hyperslab(tmp_sid,H5S_SELECT_SET,start,stride,count,block);
            CHECK(ret, FAIL, "H5Sselect_hyperslab");

            /* Compare against hyperslab selection */
            check=H5S_select_shape_same_test(regular_hyper_sid,tmp_sid);
            VERIFY(check, TRUE, "H5S_select_shape_same_test");

            ret = H5Sclose(tmp_sid);
            CHECK(ret, FAIL, "H5Sclose");

    /* Compare irregular hyperslab selection to all the selections created */
        /* Compare against itself */
        check=H5S_select_shape_same_test(irreg_hyper_sid,irreg_hyper_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        /* Compare against copy of itself */
        tmp_sid=H5Scopy(irreg_hyper_sid);
        CHECK(tmp_sid, FAIL, "H5Scopy");

        check=H5S_select_shape_same_test(irreg_hyper_sid,tmp_sid);
        VERIFY(check, TRUE, "H5S_select_shape_same_test");

        ret = H5Sclose(tmp_sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Compare against "all" selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "none" selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,none_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against single point selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,single_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against multiple point selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,mult_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "plain" single hyperslab selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,single_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "all" single hyperslab selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,single_hyper_all_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "single point" single hyperslab selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,single_hyper_pt_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against regular, strided hyperslab selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,regular_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Compare against "no" hyperslab selection */
        check=H5S_select_shape_same_test(irreg_hyper_sid,none_hyper_sid);
        VERIFY(check, FALSE, "H5S_select_shape_same_test");

        /* Construct hyperslab selection which matches irregular hyperslab selection */
            /* Create dataspace for hyperslab selection */
            tmp_sid = H5Screate_simple(SPACE9_RANK, dims, NULL);
            CHECK(tmp_sid, FAIL, "H5Screate_simple");

            start[0]=2; start[1]=2;
            stride[0]=1; stride[1]=1;
            count[0]=1; count[1]=1;
            block[0]=1; block[1]=1;
            ret = H5Sselect_hyperslab(tmp_sid,H5S_SELECT_SET,start,stride,count,block);
            CHECK(ret, FAIL, "H5Sselect_hyperslab");

            /* Select sequence of columns for hyperslab selection */
            for(u=0; u<3; u++) {
                start[0]=4; start[1]=u+4;
                stride[0]=1; stride[1]=1;
                count[0]=1; count[1]=1;
                block[0]=3; block[1]=1;
                ret = H5Sselect_hyperslab(tmp_sid,H5S_SELECT_OR,start,stride,count,block);
                CHECK(ret, FAIL, "H5Sselect_hyperslab");
            } /* end for */

            /* Compare against hyperslab selection */
            check=H5S_select_shape_same_test(irreg_hyper_sid,tmp_sid);
            VERIFY(check, TRUE, "H5S_select_shape_same_test");

            ret = H5Sclose(tmp_sid);
            CHECK(ret, FAIL, "H5Sclose");


    /* Close dataspaces */
    ret = H5Sclose(all_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(none_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(single_pt_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(mult_pt_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(single_hyper_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(single_hyper_all_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(single_hyper_pt_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(regular_hyper_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(irreg_hyper_sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(none_hyper_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_shape_same() */

/****************************************************************
**
**  test_select_hyper_chunk_offset(): Tests selections on dataspace,
**      verify that offsets for hyperslab selections are working in
**      chunked datasets.
**
****************************************************************/
static void
test_select_hyper_chunk_offset(void)
{
    hid_t fid;          /* File ID  */
    hid_t sid;          /* Dataspace ID */
    hid_t msid;         /* Memory dataspace ID */
    hid_t did;          /* Dataset ID */
    const hsize_t mem_dims[1] = { SPACE10_DIM1 };      /* Dataspace dimensions for memory */
    const hsize_t dims[1] = { 0 };      /* Dataspace initial dimensions */
    const hsize_t maxdims[1] = { H5S_UNLIMITED };       /* Dataspace mam dims */
    int *wbuf;                          /* Buffer for writing data */
    int *rbuf;                          /* Buffer for reading data */
    hid_t dcpl;                         /* Dataset creation property list ID */
    hsize_t chunks[1]={SPACE10_CHUNK_SIZE };    /* Chunk size */
    hsize_t start[1] = { 0 };           /* The start of the hyperslab */
    hsize_t count[1] = { SPACE10_CHUNK_SIZE };  /* The size of the hyperslab */
    int i,j;                            /* Local index */
    herr_t ret;                         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(6, ("Testing hyperslab selections using offsets in chunked datasets\n"));

    /* Allocate buffers */
    wbuf= HDmalloc(sizeof(int)*SPACE10_DIM1);
    CHECK(wbuf, NULL, "HDmalloc");
    rbuf= HDmalloc(sizeof(int)*SPACE10_DIM1);
    CHECK(rbuf, NULL, "HDmalloc");

    /* Initialize the write buffer */
    for(i=0; i<SPACE10_DIM1; i++)
        wbuf[i]=i;

    /* Create file */
    fid = H5Fcreate (FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a dataset creation property list */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set to chunked storage layout */
    ret=H5Pset_layout (dcpl, H5D_CHUNKED);
    CHECK(ret, FAIL, "H5Pset_layout");

    /* Set the chunk size */
    ret=H5Pset_chunk (dcpl, 1, chunks);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Create dataspace for memory */
    msid = H5Screate_simple (1, mem_dims, NULL);
    CHECK(msid, FAIL, "H5Screate_simple");

    /* Select the correct chunk in the memory dataspace */
    ret=H5Sselect_hyperslab (msid, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create dataspace for dataset */
    sid = H5Screate_simple (1, dims, maxdims);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create the dataset */
    did = H5Dcreate (fid, "fooData", H5T_NATIVE_INT, sid, dcpl);
    CHECK(did, FAIL, "H5Dcreate");

    /* Close the dataspace */
    ret=H5Sclose (sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the dataset creation property list */
    ret=H5Pclose (dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Loop over writing out each chunk */
    for(i=SPACE10_CHUNK_SIZE; i<=SPACE10_DIM1; i+=SPACE10_CHUNK_SIZE) {
        hssize_t offset[1];                 /* Offset of selection */
        hid_t fsid;                         /* File dataspace ID */
        hsize_t size[1];                    /* The size to extend the dataset to */

        /* Extend the dataset */
        size[0] = i;                 /* The size to extend the dataset to */
        ret=H5Dextend (did, size);
        CHECK(ret, FAIL, "H5Dextend");

        /* Get the (extended) dataspace from the dataset */
        fsid = H5Dget_space (did);
        CHECK(fsid, FAIL, "H5Dget_space");

        /* Select the correct chunk in the dataset */
        ret=H5Sselect_hyperslab (fsid, H5S_SELECT_SET, start, NULL, count, NULL);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        /* Set the selection offset for the file dataspace */
        offset[0] = i - SPACE10_CHUNK_SIZE;
        ret=H5Soffset_simple (fsid, offset);
        CHECK(ret, FAIL, "H5Soffset_simple");

        /* Set the selection offset for the memory dataspace */
        offset[0] = SPACE10_DIM1-i;
        ret=H5Soffset_simple (msid, offset);
        CHECK(ret, FAIL, "H5Soffset_simple");

        /* Write the data to the chunk */
        ret=H5Dwrite (did, H5T_NATIVE_INT, msid, fsid, H5P_DEFAULT, wbuf);
        CHECK(ret, FAIL, "H5Soffset_simple");

        /* Close the file dataspace copy */
        ret=H5Sclose (fsid);
        CHECK(ret, FAIL, "H5Sclose");
    }

    /* Read the data back in */
    ret=H5Dread (did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Verify the information read in */
    for(i=0; i<SPACE10_DIM1; i+=SPACE10_CHUNK_SIZE)
        for(j=0; j<SPACE10_CHUNK_SIZE; j++)
            if(wbuf[i+j]!=rbuf[((SPACE10_DIM1-i)-SPACE10_CHUNK_SIZE)+j])
                TestErrPrintf("Line: %d - Error! i=%d, j=%d, rbuf=%d, wbuf=%d\n",__LINE__,i,j,rbuf[((SPACE10_DIM1-i)-SPACE10_CHUNK_SIZE)+j],wbuf[i+j]);

/* Check with 'OR'ed set of hyperslab selections, which makes certain the
 * hyperslab spanlist code gets tested. -QAK
 */

    /* Re-initialize the write buffer */
    for(i=0; i<SPACE10_DIM1; i++)
        wbuf[i]=i*2;

    /* Change the selected the region in the memory dataspace */
    start[0] = 0;
    count[0] = SPACE10_CHUNK_SIZE/3;
    ret=H5Sselect_hyperslab (msid, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");
    start[0] = (2*SPACE10_CHUNK_SIZE)/3;
    ret=H5Sselect_hyperslab (msid, H5S_SELECT_OR, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Loop over writing out each chunk */
    for(i=SPACE10_CHUNK_SIZE; i<=SPACE10_DIM1; i+=SPACE10_CHUNK_SIZE) {
        hssize_t offset[1];                 /* Offset of selection */
        hid_t fsid;                         /* File dataspace ID */
        hsize_t size[1];                    /* The size to extend the dataset to */

        /* Extend the dataset */
        size[0] = i;                 /* The size to extend the dataset to */
        ret=H5Dextend (did, size);
        CHECK(ret, FAIL, "H5Dextend");

        /* Get the (extended) dataspace from the dataset */
        fsid = H5Dget_space (did);
        CHECK(fsid, FAIL, "H5Dget_space");

        /* Select the correct region in the dataset */
        start[0] = 0;
        ret=H5Sselect_hyperslab (fsid, H5S_SELECT_SET, start, NULL, count, NULL);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");
        start[0] = (2*SPACE10_CHUNK_SIZE)/3;
        ret=H5Sselect_hyperslab (fsid, H5S_SELECT_OR, start, NULL, count, NULL);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        /* Set the selection offset for the file dataspace */
        offset[0] = i - SPACE10_CHUNK_SIZE;
        ret=H5Soffset_simple (fsid, offset);
        CHECK(ret, FAIL, "H5Soffset_simple");

        /* Set the selection offset for the memory dataspace */
        offset[0] = SPACE10_DIM1-i;
        ret=H5Soffset_simple (msid, offset);
        CHECK(ret, FAIL, "H5Soffset_simple");

        /* Write the data to the chunk */
        ret=H5Dwrite (did, H5T_NATIVE_INT, msid, fsid, H5P_DEFAULT, wbuf);
        CHECK(ret, FAIL, "H5Soffset_simple");

        /* Close the file dataspace copy */
        ret=H5Sclose (fsid);
        CHECK(ret, FAIL, "H5Sclose");
    }

    /* Read the data back in */
    ret=H5Dread (did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Verify the information read in */
    for(i=0; i<SPACE10_DIM1; i+=SPACE10_CHUNK_SIZE)
        for(j=0; j<SPACE10_CHUNK_SIZE; j++)
            /* We're not writing out the "middle" of each chunk, so don't check that */
            if(j<(SPACE10_CHUNK_SIZE/3) || j>=((2*SPACE10_CHUNK_SIZE)/3))
                if(wbuf[i+j]!=rbuf[((SPACE10_DIM1-i)-SPACE10_CHUNK_SIZE)+j])
                    TestErrPrintf("Line: %d - Error! i=%d, j=%d, rbuf=%d, wbuf=%d\n",__LINE__,i,j,rbuf[((SPACE10_DIM1-i)-SPACE10_CHUNK_SIZE)+j],wbuf[i+j]);

    /* Close the memory dataspace */
    ret=H5Sclose (msid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the dataset */
    ret=H5Dclose (did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the file */
    ret=H5Fclose (fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free the buffers */
    HDfree(wbuf);
    HDfree(rbuf);
}   /* test_select_hyper_chunk_offset() */

/****************************************************************
**
**  test_select_hyper_chunk_offset2(): Tests selections on dataspace,
**      another test to verify that offsets for hyperslab selections are
**      working in chunked datasets.
**
****************************************************************/
static void
test_select_hyper_chunk_offset2(void)
{
    hid_t       file, dataset;  /* handles */
    hid_t       dataspace;
    hid_t       memspace;
    hid_t       dcpl;           /* Dataset creation property list */
    herr_t      status;
    unsigned    data_out[SPACE12_DIM0]; /* output buffer */
    unsigned    data_in[SPACE12_CHUNK_DIM0]; /* input buffer */
    hsize_t     dims[SPACE12_RANK]={SPACE12_DIM0};              /* Dimension size */
    hsize_t     chunk_dims[SPACE12_RANK]={SPACE12_CHUNK_DIM0};  /* Chunk size */
    hsize_t     start[SPACE12_RANK];    /* Start of hyperslab */
    hsize_t     count[SPACE12_RANK];    /* Size of hyperslab */
    hssize_t    offset[SPACE12_RANK];   /* hyperslab offset in the file */
    unsigned    u, v;           /* Local index variables */

    /* Output message about test being performed */
    MESSAGE(6, ("Testing more hyperslab selections using offsets in chunked datasets\n"));

    /* Initialize data to write out */
    for (u = 0; u < SPACE12_DIM0; u++)
        data_out[u] = u;

    /* Create the file */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create dataspace */
    dataspace = H5Screate_simple(SPACE12_RANK, dims, NULL);
    CHECK(dataspace, FAIL, "H5Screate_simple");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set chunk sizes */
    status = H5Pset_chunk(dcpl, SPACE12_RANK, chunk_dims);
    CHECK(status, FAIL, "H5Pset_chunk");

    /* Create dataset */
    dataset = H5Dcreate(file, DATASETNAME, H5T_NATIVE_UINT, dataspace, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Close DCPL */
    status = H5Pclose(dcpl);
    CHECK(status, FAIL, "H5Pclose");

    /* Write out entire dataset */
    status = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_out);
    CHECK(status, FAIL, "H5Dclose");

    /* Create memory dataspace (same size as a chunk) */
    memspace = H5Screate_simple(SPACE12_RANK, chunk_dims, NULL);
    CHECK(dataspace, FAIL, "H5Screate_simple");

    /*
     * Define hyperslab in the file dataspace.
     */
    start[0] = 0;
    count[0] = SPACE12_CHUNK_DIM0;
    status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(status, FAIL, "H5Sselect_hyperslab");

    /* Loop through retrieving data from file, checking it against data written */
    for(u = 0; u < SPACE12_DIM0; u += SPACE12_CHUNK_DIM0) {
        /* Set the offset of the file selection */
        offset[0] = u;
        status = H5Soffset_simple(dataspace, offset);
        CHECK(status, FAIL, "H5Soffset_simple");

        /* Read in buffer of data */
        status = H5Dread(dataset, H5T_NATIVE_UINT, memspace, dataspace,
                H5P_DEFAULT, data_in);
        CHECK(status, FAIL, "H5Dread");

        /* Check data read in */
        for(v = 0; v < SPACE12_CHUNK_DIM0; v++)
            if(data_out[u + v] != data_in[v])
                TestErrPrintf("Error! data_out[%u]=%u, data_in[%u]=%u\n",(unsigned)(u + v), data_out[u + v], v, data_in[v]);
    } /* end for */

    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");

    status = H5Sclose(dataspace);
    CHECK(status, FAIL, "H5Sclose");

    status = H5Sclose(memspace);
    CHECK(status, FAIL, "H5Sclose");

    status = H5Fclose(file);
    CHECK(status, FAIL, "H5Fclose");
}   /* test_select_hyper_chunk_offset2() */

/****************************************************************
**
**  test_select_bounds(): Tests selection bounds on dataspaces,
**      both with and without offsets.
**
****************************************************************/
static void
test_select_bounds(void)
{
    hid_t sid;          /* Dataspace ID */
    const hsize_t dims[SPACE11_RANK] = { SPACE11_DIM1, SPACE11_DIM2 };      /* Dataspace dimensions */
    hsize_t coord[SPACE11_NPOINTS][SPACE11_RANK]; /* Coordinates for point selection */
    hsize_t start[SPACE11_RANK];       /* The start of the hyperslab */
    hsize_t stride[SPACE11_RANK];       /* The stride between block starts for the hyperslab */
    hsize_t count[SPACE11_RANK];        /* The number of blocks for the hyperslab */
    hsize_t block[SPACE11_RANK];        /* The size of each block for the hyperslab */
    hssize_t offset[SPACE11_RANK];      /* Offset amount for selection */
    hsize_t low_bounds[SPACE11_RANK];  /* The low bounds for the selection */
    hsize_t high_bounds[SPACE11_RANK]; /* The high bounds for the selection */
    herr_t ret;                         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(6, ("Testing selection bounds\n"));

    /* Create dataspace */
    sid = H5Screate_simple (SPACE11_RANK, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Get bounds for 'all' selection */
    ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low_bounds[0],0,"H5Sget_select_bounds");
    VERIFY(low_bounds[1],0,"H5Sget_select_bounds");
    VERIFY(high_bounds[0],SPACE11_DIM1-1,"H5Sget_select_bounds");
    VERIFY(high_bounds[1],SPACE11_DIM2-1,"H5Sget_select_bounds");

    /* Set offset for selection */
    offset[0]=1; offset[1]=1;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Get bounds for 'all' selection with offset (which should be ignored) */
    ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low_bounds[0],0,"H5Sget_select_bounds");
    VERIFY(low_bounds[1],0,"H5Sget_select_bounds");
    VERIFY(high_bounds[0],SPACE11_DIM1-1,"H5Sget_select_bounds");
    VERIFY(high_bounds[1],SPACE11_DIM2-1,"H5Sget_select_bounds");

    /* Reset offset for selection */
    offset[0]=0; offset[1]=0;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Set 'none' selection */
    ret=H5Sselect_none(sid);
    CHECK(ret, FAIL, "H5Sselect_none");

    /* Get bounds for 'none' selection */
    H5E_BEGIN_TRY {
        ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sget_select_bounds");

    /* Set point selection */
    coord[0][0]=  3; coord[0][1]=  3;
    coord[1][0]=  3; coord[1][1]= 96;
    coord[2][0]= 96; coord[2][1]=  3;
    coord[3][0]= 96; coord[3][1]= 96;
    ret = H5Sselect_elements(sid,H5S_SELECT_SET,SPACE11_NPOINTS,coord);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Get bounds for point selection */
    ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low_bounds[0],3,"H5Sget_select_bounds");
    VERIFY(low_bounds[1],3,"H5Sget_select_bounds");
    VERIFY(high_bounds[0],SPACE11_DIM1-4,"H5Sget_select_bounds");
    VERIFY(high_bounds[1],SPACE11_DIM2-4,"H5Sget_select_bounds");

    /* Set bad offset for selection */
    offset[0]=5; offset[1]=-5;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Get bounds for hyperslab selection with negative offset */
    H5E_BEGIN_TRY {
        ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sget_select_bounds");

    /* Set valid offset for selection */
    offset[0]=2; offset[1]=-2;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Get bounds for point selection with offset */
    ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low_bounds[0],5,"H5Sget_select_bounds");
    VERIFY(low_bounds[1],1,"H5Sget_select_bounds");
    VERIFY(high_bounds[0],SPACE11_DIM1-2,"H5Sget_select_bounds");
    VERIFY(high_bounds[1],SPACE11_DIM2-6,"H5Sget_select_bounds");

    /* Reset offset for selection */
    offset[0]=0; offset[1]=0;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Set "regular" hyperslab selection */
    start[0]=  2; start[1]=  2;
    stride[0]=  10; stride[1]= 10;
    count[0]= 4; count[1]=  4;
    block[0]= 5; block[1]= 5;
    ret = H5Sselect_hyperslab(sid,H5S_SELECT_SET,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Get bounds for hyperslab selection */
    ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low_bounds[0],2,"H5Sget_select_bounds");
    VERIFY(low_bounds[1],2,"H5Sget_select_bounds");
    VERIFY(high_bounds[0],36,"H5Sget_select_bounds");
    VERIFY(high_bounds[1],36,"H5Sget_select_bounds");

    /* Set bad offset for selection */
    offset[0]=5; offset[1]=-5;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Get bounds for hyperslab selection with negative offset */
    H5E_BEGIN_TRY {
        ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sget_select_bounds");

    /* Set valid offset for selection */
    offset[0]=5; offset[1]=-2;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Get bounds for hyperslab selection with offset */
    ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low_bounds[0],7,"H5Sget_select_bounds");
    VERIFY(low_bounds[1],0,"H5Sget_select_bounds");
    VERIFY(high_bounds[0],41,"H5Sget_select_bounds");
    VERIFY(high_bounds[1],34,"H5Sget_select_bounds");

    /* Reset offset for selection */
    offset[0]=0; offset[1]=0;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Make "irregular" hyperslab selection */
    start[0]= 20; start[1]= 20;
    stride[0]=  20; stride[1]= 20;
    count[0]= 2; count[1]=  2;
    block[0]= 10; block[1]= 10;
    ret = H5Sselect_hyperslab(sid,H5S_SELECT_OR,start,stride,count,block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Get bounds for hyperslab selection */
    ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low_bounds[0],2,"H5Sget_select_bounds");
    VERIFY(low_bounds[1],2,"H5Sget_select_bounds");
    VERIFY(high_bounds[0],49,"H5Sget_select_bounds");
    VERIFY(high_bounds[1],49,"H5Sget_select_bounds");

    /* Set bad offset for selection */
    offset[0]=5; offset[1]=-5;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Get bounds for hyperslab selection with negative offset */
    H5E_BEGIN_TRY {
        ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Sget_select_bounds");

    /* Set valid offset for selection */
    offset[0]=5; offset[1]=-2;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Get bounds for hyperslab selection with offset */
    ret = H5Sget_select_bounds(sid,low_bounds,high_bounds);
    CHECK(ret, FAIL, "H5Sget_select_bounds");
    VERIFY(low_bounds[0],7,"H5Sget_select_bounds");
    VERIFY(low_bounds[1],0,"H5Sget_select_bounds");
    VERIFY(high_bounds[0],54,"H5Sget_select_bounds");
    VERIFY(high_bounds[1],47,"H5Sget_select_bounds");

    /* Reset offset for selection */
    offset[0]=0; offset[1]=0;
    ret=H5Soffset_simple(sid, offset);
    CHECK(ret, FAIL, "H5Soffset_simple");

    /* Close the dataspace */
    ret=H5Sclose (sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_select_bounds() */

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
#ifdef H5_WANT_H5_V1_4_COMPAT
    int rdcc_nelmts;   /* Raw data number of elements */
#else /* H5_WANT_H5_V1_4_COMPAT */
    size_t rdcc_nelmts;   /* Raw data number of elements */
#endif /* H5_WANT_H5_V1_4_COMPAT */
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
#ifdef H5_WANT_H5_V1_4_COMPAT
    ret=H5Pset_buffer(plist_id,(hsize_t)59,NULL,NULL);
#else /* H5_WANT_H5_V1_4_COMPAT */
    ret=H5Pset_buffer(plist_id,(size_t)59,NULL,NULL);
#endif /* H5_WANT_H5_V1_4_COMPAT */
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
    test_select_hyper_contig3(H5T_STD_U16LE,H5P_DEFAULT); /* Test yet more contiguous hyperslab selection cases */
    test_select_hyper_contig3(H5T_STD_U16LE,plist_id); /* Test yet more contiguous hyperslab selection cases */
    test_select_hyper_contig3(H5T_STD_U16BE,H5P_DEFAULT); /* Test yet more contiguous hyperslab selection cases */
    test_select_hyper_contig3(H5T_STD_U16BE,plist_id); /* Test yet more contiguous hyperslab selection cases */
    test_select_hyper_copy();   /* Test hyperslab selection copying code */
    test_select_point_copy();   /* Test point selection copying code */
    test_select_hyper_offset(); /* Test selection offset code with hyperslabs */
    test_select_hyper_offset2();/* Test more selection offset code with hyperslabs */
    test_select_point_offset(); /* Test selection offset code with elements */
    test_select_hyper_union();  /* Test hyperslab union code */
    test_select_hyper_and_2d(); /* Test hyperslab intersection (AND) code for 2-D dataset */
    test_select_hyper_xor_2d(); /* Test hyperslab XOR code for 2-D dataset */
    test_select_hyper_notb_2d(); /* Test hyperslab NOTB code for 2-D dataset */
    test_select_hyper_nota_2d(); /* Test hyperslab NOTA code for 2-D dataset */

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

    /* Tests for combining "all" and "none" selections with hyperslabs */
    test_select_combine();

    /* Test filling selections */
    /* (Also tests iterating through each selection */
    test_select_fill_all();
    test_select_fill_point(NULL);
    test_select_fill_point(offset);
    test_select_fill_hyper_simple(NULL);
    test_select_fill_hyper_simple(offset);
    test_select_fill_hyper_regular(NULL);
    test_select_fill_hyper_regular(offset);
    test_select_fill_hyper_irregular(NULL);
    test_select_fill_hyper_irregular(offset);

    /* Test 0-sized selections */
    test_select_none();

    /* Test selections on scalar dataspaces */
    test_scalar_select();
    test_scalar_select2();

    /* Test "same shape" routine */
    test_shape_same();

    /* Test point selections in chunked datasets */
    test_select_point_chunk();

    /* Test scalar dataspaces in chunked datasets */
    test_select_scalar_chunk();

    /* Test using selection offset on hyperslab in chunked dataset */
    test_select_hyper_chunk_offset();
    test_select_hyper_chunk_offset2();

    /* Test selection bounds with & without offsets */
    test_select_bounds();

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

