/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "$Revision$";
#endif

/* $Id$ */

/***********************************************************
*
* Test program:	 th5p
*
* Test the dataspace functionality
*
*************************************************************/

#include <testhdf5.h>

#include <H5private.h>
#include <H5Bprivate.h>
#include <H5Sprivate.h>
#include <H5Pprivate.h>

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
 } space4_data={'v',987123,-3.14,'g'}; /* Test data for 4th dataspace */

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
    unsigned		rank;		/* Logical rank of dataspace	*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2, SPACE2_DIM3,
				   SPACE2_DIM4};
    hsize_t		tdims[4];	/* Dimension array to test with */
    size_t		n;	 	/* Number of dataspace elements */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace Manipulation\n"));

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    n = H5Sget_npoints(sid1);
    CHECK(n, UFAIL, "H5Sget_npoints");
    VERIFY(n, SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3, "H5Sget_npoints");

    rank = H5Sget_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_lrank");
    VERIFY(rank, SPACE1_RANK, "H5Sget_lrank");

    ret = H5Sget_dims(sid1, tdims, NULL);
    CHECK(ret, FAIL, "H5Sget_ldims");
    VERIFY(HDmemcmp(tdims, dims1, SPACE1_RANK * sizeof(unsigned)), 0,
	   "H5Sget_ldims");

    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    n = H5Sget_npoints(sid2);
    CHECK(n, UFAIL, "H5Snelem");
    VERIFY(n, SPACE2_DIM1 * SPACE2_DIM2 * SPACE2_DIM3 * SPACE2_DIM4,
	   "H5Snelem");

    rank = H5Sget_ndims(sid2);
    CHECK(rank, UFAIL, "H5Sget_lrank");
    VERIFY(rank, SPACE2_RANK, "H5Sget_lrank");

    ret = H5Sget_dims(sid2, tdims, NULL);
    CHECK(ret, FAIL, "H5Sget_ldims");
    VERIFY(HDmemcmp(tdims, dims2, SPACE2_RANK * sizeof(unsigned)), 0,
	   "H5Sget_ldims");

    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close first file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
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
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation\n"));

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create scalar dataspace */
    sid1 = H5Screate_simple(SPACE3_RANK, NULL, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    n = H5Sget_npoints(sid1);
    CHECK(n, UFAIL, "H5Sget_npoints");
    VERIFY(n, 1, "H5Sget_npoints");

    rank = H5Sget_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_lrank");
    VERIFY(rank, SPACE3_RANK, "H5Sget_lrank");

    ret = H5Sget_dims(sid1, tdims, NULL);
    VERIFY(ret, 0, "H5Sget_dims");

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

    n = H5Sget_npoints(sid1);
    CHECK(n, UFAIL, "H5Sget_npoints");
    VERIFY(n, 1, "H5Sget_npoints");

    rank = H5Sget_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_lrank");
    VERIFY(rank, SPACE3_RANK, "H5Sget_lrank");

    ret = H5Sget_dims(sid1, tdims, NULL);
    VERIFY(ret, 0, "H5Sget_dims");

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
		    H5T_NATIVE_CHAR);
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
		    H5T_NATIVE_CHAR);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create scalar dataspace */
    sid1 = H5Screate_simple(SPACE3_RANK, NULL, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    n = H5Sget_npoints(sid1);
    CHECK(n, UFAIL, "H5Sget_npoints");
    VERIFY(n, 1, "H5Sget_npoints");

    rank = H5Sget_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_lrank");
    VERIFY(rank, SPACE3_RANK, "H5Sget_lrank");

    ret = H5Sget_dims(sid1, tdims, NULL);
    VERIFY(ret, 0, "H5Sget_dims");

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

    n = H5Sget_npoints(sid1);
    CHECK(n, UFAIL, "H5Sget_npoints");
    VERIFY(n, 1, "H5Sget_npoints");

    rank = H5Sget_ndims(sid1);
    CHECK(rank, UFAIL, "H5Sget_lrank");
    VERIFY(rank, SPACE3_RANK, "H5Sget_lrank");

    ret = H5Sget_dims(sid1, tdims, NULL);
    VERIFY(ret, 0, "H5Sget_dims");

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

