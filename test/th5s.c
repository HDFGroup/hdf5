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
* Test program:	 th5p
*
* Test the dataspace functionality
*
*************************************************************/

#include "testhdf5.h"

#include "H5private.h"
#include "H5Bprivate.h"
#include "H5Sprivate.h"
#include "H5Pprivate.h"

#define TESTFILE   "th5s.h5"
#define FILE   "th5s1.h5"

/* 3-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	3
#define SPACE1_DIM1	3
#define SPACE1_DIM2	15
#define SPACE1_DIM3	13

/* 4-D dataset with one unlimited dimension */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	4
#define SPACE2_DIM1	0
#define SPACE2_DIM2	15
#define SPACE2_DIM3	13
#define SPACE2_DIM4	23
#define SPACE2_MAX1	H5S_UNLIMITED
#define SPACE2_MAX2	15
#define SPACE2_MAX3	13
#define SPACE2_MAX4	23

/* Scalar dataset with simple datatype */
#define SPACE3_NAME  "Scalar1"
#define SPACE3_RANK	0
unsigned space3_data=65;

/* Scalar dataset with compound datatype */
#define SPACE4_NAME  "Scalar2"
#define SPACE4_RANK	0
#define SPACE4_FIELDNAME1	"c1"
#define SPACE4_FIELDNAME2	"u"
#define SPACE4_FIELDNAME3	"f"
#define SPACE4_FIELDNAME4	"c2"
size_t space4_field1_off=0;
size_t space4_field2_off=0;
size_t space4_field3_off=0;
size_t space4_field4_off=0;
struct space4_struct {
    char c1;
    unsigned u;
    float f;
    char c2;
 } space4_data={'v',987123,(float)-3.14,'g'}; /* Test data for 4th dataspace */

/****************************************************************
**
**  test_h5s_basic(): Test basic H5S (dataspace) code.
** 
****************************************************************/
static void 
test_h5s_basic(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		sid1, sid2;	/* Dataspace ID			*/
    hid_t		dset1;		/* Dataset ID			*/
    unsigned		rank;		/* Logical rank of dataspace	*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2, SPACE2_DIM3,
				   SPACE2_DIM4};
    hsize_t		dims3[H5S_MAX_RANK+1];
    hsize_t		max2[] = {SPACE2_MAX1, SPACE2_MAX2, SPACE2_MAX3,
				  SPACE2_MAX4};
    hsize_t		tdims[4];	/* Dimension array to test with */
    hsize_t		tmax[4];
    size_t		n;	 	/* Number of dataspace elements */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace Manipulation\n"));

    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

   
    H5_ASSIGN_OVERFLOW(n,H5Sget_simple_extent_npoints(sid1),hsize_t,size_t);
    CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3,
	   "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE1_RANK, "H5Sget_simple_extent_ndims");

    ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    VERIFY(HDmemcmp(tdims, dims1, SPACE1_RANK * sizeof(unsigned)), 0,
	   "H5Sget_simple_extent_dims");

    sid2 = H5Screate_simple(SPACE2_RANK, dims2, max2);
    CHECK(sid2, FAIL, "H5Screate_simple");

    H5_ASSIGN_OVERFLOW(n,H5Sget_simple_extent_npoints(sid2),hsize_t,size_t);    
    CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, SPACE2_DIM1 * SPACE2_DIM2 * SPACE2_DIM3 * SPACE2_DIM4,
	   "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid2);
    CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE2_RANK, "H5Sget_simple_extent_ndims");

    ret = H5Sget_simple_extent_dims(sid2, tdims, tmax);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    VERIFY(HDmemcmp(tdims, dims2, SPACE2_RANK * sizeof(unsigned)), 0,
	   "H5Sget_simple_extent_dims");
    VERIFY(HDmemcmp(tmax, max2, SPACE2_RANK * sizeof(unsigned)), 0,
	   "H5Sget_simple_extent_dims");

    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /*
     * Check to be sure we can't create a simple data space that has too many
     * dimensions.
     */
    H5E_BEGIN_TRY {
	sid1 = H5Screate_simple(H5S_MAX_RANK+1, dims3, NULL);
    } H5E_END_TRY;
    VERIFY(sid1, FAIL, "H5Screate_simple");

    /*
     * Try reading a file that has been prepared that has a dataset with a
     * higher dimensionality than what the library can handle.
     *
     * If this test fails and the H5S_MAX_RANK variable has changed, follow
     * the instructions in space_overflow.c for regenating the th5s.h5 file.
     */
    {
    char testfile[512]="";
    char *srcdir = getenv("srcdir");
    if (srcdir && ((strlen(srcdir) + strlen(TESTFILE) + 1) < sizeof(testfile))){
	strcpy(testfile, srcdir);
	strcat(testfile, "/");
    }
    strcat(testfile, TESTFILE);
    fid1 = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK_I(fid1, "H5Fopen");
    if (fid1 >= 0){
	dset1 = H5Dopen(fid1, "dset");
	VERIFY(dset1, FAIL, "H5Dopen");
	ret = H5Fclose(fid1);
	CHECK_I(ret, "H5Fclose");
    }
    else
	printf("***cannot open the pre-created H5S_MAX_RANK test file (%s)\n",
	    testfile);
    }

    /* Verify that incorrect dimensions don't work */
    dims1[0]=0;
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    VERIFY(sid1, FAIL, "H5Screate_simple");

    sid1 = H5Screate(H5S_SIMPLE);
    CHECK(sid1, FAIL, "H5Screate");

    ret = H5Sset_extent_simple(sid1,SPACE1_RANK,dims1,NULL);
    VERIFY(ret, FAIL, "H5Sset_extent_simple");

    ret = H5Sclose(sid1);
    CHECK_I(ret, "H5Sclose");
}				/* test_h5s_basic() */

/****************************************************************
**
**  test_h5s_scalar_write(): Test scalar H5S (dataspace) writing code.
** 
****************************************************************/
static void 
test_h5s_scalar_write(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;	    /* Dataspace ID			*/
    unsigned		rank;		/* Logical rank of dataspace	*/
    hsize_t		tdims[4];	/* Dimension array to test with */
    size_t		n;	 	/* Number of dataspace elements */
    H5S_class_t ext_type;   /* Extent type */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation\n"));

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create scalar dataspace */
    sid1 = H5Screate_simple(SPACE3_RANK, NULL, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");
    H5_ASSIGN_OVERFLOW(n,H5Sget_simple_extent_npoints(sid1),hsize_t,size_t);    
    CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

    ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    VERIFY(ret, 0, "H5Sget_simple_extent_dims");

    /* Verify extent type */
    ext_type = H5Sget_simple_extent_type(sid1);
    VERIFY(ext_type, H5S_SCALAR, "H5Sget_simple_extent_type");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",H5T_NATIVE_UINT,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &space3_data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close scalar dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}				/* test_h5s_scalar_write() */

/****************************************************************
**
**  test_h5s_scalar_read(): Test scalar H5S (dataspace) reading code.
** 
****************************************************************/
static void 
test_h5s_scalar_read(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;	    	/* Dataspace ID			*/
    unsigned		rank;		/* Logical rank of dataspace	*/
    hsize_t		tdims[4];	/* Dimension array to test with */
    size_t		n;	 	/* Number of dataspace elements */
    unsigned      	rdata;      	/* Scalar data read in 		*/
    herr_t		ret;		/* Generic return value		*/
    H5S_class_t ext_type;               /* Extent type */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation\n"));

    /* Create file */
    fid1 = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Create a dataset */
    dataset=H5Dopen(fid1,"Dataset1");
    CHECK(dataset, FAIL, "H5Dopen");

    sid1=H5Dget_space(dataset);
    CHECK(sid1, FAIL, "H5Dget_space");

    H5_ASSIGN_OVERFLOW(n,H5Sget_simple_extent_npoints(sid1),hsize_t,size_t);    
    CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

    ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    VERIFY(ret, 0, "H5Sget_simple_extent_dims");

    /* Verify extent type */
    ext_type = H5Sget_simple_extent_type(sid1);
    VERIFY(ext_type, H5S_SCALAR, "H5Sget_simple_extent_type");

    ret = H5Dread(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    VERIFY(rdata, space3_data, "H5Dread");
    
    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close scalar dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}				/* test_h5s_scalar_read() */

/****************************************************************
**
**  test_h5s_compound_scalar_write(): Test scalar H5S (dataspace) writing for
**          compound datatypes.
** 
****************************************************************/
static void 
test_h5s_compound_scalar_write(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t       	tid1;       	/* Attribute datatype ID	*/
    hid_t		sid1;	    	/* Dataspace ID			*/
    unsigned		rank;		/* Logical rank of dataspace	*/
    hsize_t		tdims[4];	/* Dimension array to test with */
    size_t		n;	 	/* Number of dataspace elements */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation\n"));

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create the compound datatype.  */
    tid1 = H5Tcreate (H5T_COMPOUND, sizeof(struct space4_struct));
    CHECK(tid1, FAIL, "H5Tcreate");
    space4_field1_off=HOFFSET(struct space4_struct, c1);
    ret = H5Tinsert(tid1, SPACE4_FIELDNAME1, space4_field1_off,
		    H5T_NATIVE_SCHAR);
    CHECK(ret, FAIL, "H5Tinsert");
    space4_field2_off=HOFFSET(struct space4_struct, u);
    ret = H5Tinsert(tid1, SPACE4_FIELDNAME2, space4_field2_off,
		    H5T_NATIVE_UINT);
    CHECK(ret, FAIL, "H5Tinsert");
    space4_field3_off=HOFFSET(struct space4_struct, f);
    ret = H5Tinsert(tid1, SPACE4_FIELDNAME3, space4_field3_off,
		    H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");
    space4_field4_off=HOFFSET(struct space4_struct, c2);
    ret = H5Tinsert(tid1, SPACE4_FIELDNAME4, space4_field4_off,
		    H5T_NATIVE_SCHAR);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create scalar dataspace */
    sid1 = H5Screate_simple(SPACE3_RANK, NULL, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    H5_ASSIGN_OVERFLOW(n,H5Sget_simple_extent_npoints(sid1),hsize_t,size_t);    
    CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

    ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    VERIFY(ret, 0, "H5Sget_simple_extent_dims");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, &space4_data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close scalar dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}				/* test_h5s_compound_scalar_write() */

/****************************************************************
**
**  test_h5s_compound_scalar_read(): Test scalar H5S (dataspace) reading for
**          compound datatypes.
** 
****************************************************************/
static void 
test_h5s_compound_scalar_read(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;	    	/* Dataspace ID			*/
    hid_t       	type;       	/* Datatype             	*/
    unsigned		rank;		/* Logical rank of dataspace	*/
    hsize_t		tdims[4];	/* Dimension array to test with */
    size_t		n;	 	/* Number of dataspace elements */
    struct space4_struct rdata; 	/* Scalar data read in 		*/
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation\n"));

    /* Create file */
    fid1 = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Create a dataset */
    dataset=H5Dopen(fid1,"Dataset1");
    CHECK(dataset, FAIL, "H5Dopen");

    sid1=H5Dget_space(dataset);
    CHECK(sid1, FAIL, "H5Dget_space");

    H5_ASSIGN_OVERFLOW(n,H5Sget_simple_extent_npoints(sid1),hsize_t,size_t);    
    CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

    ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    VERIFY(ret, 0, "H5Sget_simple_extent_dims");

    type=H5Dget_type(dataset);
    CHECK(type, FAIL, "H5Dget_type");
     
    ret = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if(HDmemcmp(&space4_data,&rdata,sizeof(struct space4_struct))) {
        printf("scalar data different: space4_data.c1=%c, read_data4.c1=%c\n",space4_data.c1,rdata.c1);
        printf("scalar data different: space4_data.u=%u, read_data4.u=%u\n",space4_data.u,rdata.u);
        printf("scalar data different: space4_data.f=%f, read_data4.f=%f\n",space4_data.f,rdata.f);
        printf("scalar data different: space4_data.c1=%c, read_data4.c1=%c\n",space4_data.c1,rdata.c2);
        num_errs++;
     } /* end if */
    
    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close scalar dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}				/* test_h5s_compound_scalar_read() */

/* Data arrays for chunk test */
double  chunk_data_dbl[50000][3];
float  chunk_data_flt[50000][3];

/****************************************************************
**
**  test_h5s_chunk(): Exercise chunked I/O, testing when data conversion
**      is necessary and the entire chunk read in doesn't fit into the
**      conversion buffer
** 
****************************************************************/
static void 
test_h5s_chunk(void)
{
    herr_t status;
    hid_t fileID, dsetID;
    hid_t plist_id;
    hid_t space_id;
    hsize_t dims[2];
    hsize_t csize[2];
    int i,j;

    fileID = H5Fcreate(FILE,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
    CHECK(fileID, FAIL, "H5Fcreate");

    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(plist_id, FAIL, "H5Pcreate");

    csize[0] = 50000;
    csize[1] = 3;
    status = H5Pset_chunk(plist_id, 2, csize);
    CHECK(status, FAIL, "H5Pset_chunk");

    /* Create the data space */
    dims[0] = 50000;
    dims[1] = 3;
    space_id = H5Screate_simple(2, dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple");

    dsetID = H5Dcreate(fileID,"coords",H5T_NATIVE_FLOAT,space_id,plist_id);
    CHECK(dsetID, FAIL, "H5Dcreate");

    /* Initialize float array */
    for(i=0; i<50000; i++)
        for(j=0; j<3; j++)
            chunk_data_flt[i][j]=(float)(i*2.5-j*100.3);

    status= H5Dwrite(dsetID,H5T_NATIVE_FLOAT,H5S_ALL,H5S_ALL,H5P_DEFAULT,chunk_data_flt);
    CHECK(status, FAIL, "H5Dwrite");

    status=H5Pclose(plist_id);
    CHECK(status, FAIL, "H5Pclose");
    status=H5Sclose(space_id);
    CHECK(status, FAIL, "H5Sclose");
    status=H5Dclose(dsetID);
    CHECK(status, FAIL, "H5Dclose");
    status=H5Fclose(fileID);
    CHECK(status, FAIL, "H5Fclose");

    /* Reset/initialize the data arrays to read in */
    HDmemset(chunk_data_dbl,0,sizeof(double)*50000*3);
    HDmemset(chunk_data_flt,0,sizeof(float)*50000*3);

    fileID = H5Fopen(FILE,H5F_ACC_RDONLY,H5P_DEFAULT);
    CHECK(fileID, FAIL, "H5Fopen");
    dsetID = H5Dopen(fileID,"coords");
    CHECK(dsetID, FAIL, "H5Dopen");

    status= H5Dread (dsetID,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,H5P_DEFAULT,chunk_data_dbl);
    CHECK(status, FAIL, "H5Dread");
    status= H5Dread (dsetID,H5T_NATIVE_FLOAT,H5S_ALL,H5S_ALL,H5P_DEFAULT,chunk_data_flt);
    CHECK(status, FAIL, "H5Dread");

    status=H5Dclose(dsetID);
    CHECK(status, FAIL, "H5Dclose");
    status=H5Fclose(fileID);
    CHECK(status, FAIL, "H5Fclose");

    for(i=0; i<50000; i++) {
        for(j=0; j<3; j++) {
            if(chunk_data_dbl[i][j]!=chunk_data_flt[i][j]) {
                num_errs++;
                printf("chunk_data_dbl[%d][%d]=%f, chunk_data_flt[%d][%d]=%f\n",i,j,chunk_data_dbl[i][j],i,j,chunk_data_flt[i][j]);
            } /* end if */
        } /* end for */
    } /* end for */
} /* test_h5s_chunk() */

/****************************************************************
**
**  test_h5s(): Main H5S (dataspace) testing routine.
** 
****************************************************************/
void 
test_h5s(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspaces\n"));

    test_h5s_basic();		/* Test basic H5S code */
    test_h5s_scalar_write();	/* Test scalar H5S writing code */
    test_h5s_scalar_read();		/* Test scalar H5S reading code */
    test_h5s_compound_scalar_write();	/* Test compound datatype scalar H5S writing code */
    test_h5s_compound_scalar_read();	/* Test compound datatype scalar H5S reading code */

    /* This test was added later to exercise a bug in chunked I/O */
    test_h5s_chunk();	        /* Exercise bug fix for chunked I/O */
} /* test_h5s() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_h5s
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
cleanup_h5s(void)
{
    remove(FILE);
}

