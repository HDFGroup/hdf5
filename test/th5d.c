/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char RcsId[] = "$Revision$";
#endif

/* $Id$ */

/***********************************************************
*
* Test program:  th5d
*
* Test the basic dataset functionality
*
*************************************************************/

#include <testhdf5.h>

#include <H5private.h>
#include <H5Bprivate.h>
#include <H5Mprivate.h>
#include <H5Dprivate.h>

#define FILE   "th5d1.h5"

/* 3-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK     3
#define SPACE1_DIM1     3
#define SPACE1_DIM2     15
#define SPACE1_DIM3     13

/* 32-bit big-endian integer type */
#define TYPE1_NAME  "Type1"
#define TYPE1_BASE  H5T_INT
#define TYPE1_LEN   4
#define TYPE1_ARCH  H5T_BIGENDIAN

/* 3-d 32-bit integer dataset */
#define DATA1_NAME  "Data1"
int32 data1[SPACE1_DIM1][SPACE1_DIM2][SPACE1_DIM3]={1,2,3,4,5,6};

/****************************************************************
**
**  test_h5d_basic(): Test basic H5D (dataset) code.
** 
****************************************************************/
static void test_h5d_basic(void)
{
    hatom_t fid1;   /* HDF5 File IDs */
    hatom_t did1;   /* Dataset ID */
    hatom_t tid1;   /* Datatype ID */
    hatom_t sid1;   /* Dataspace ID */
    uint32 dims1[]={SPACE1_DIM1,SPACE1_DIM2,SPACE1_DIM3};   /* dataspace dim sizes */
    uintn n;            /* number of dataspace elements */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Datatype Manipulation\n"));

    /* Create file */
    fid1=H5Fcreate(FILE,H5ACC_OVERWRITE,0,0);
    CHECK(fid1,FAIL,"H5Fcreate");

    sid1=H5Mcreate(fid1,H5_DATASPACE,SPACE1_NAME);
    CHECK(sid1,FAIL,"H5Mcreate");

    ret=H5Pset_space(sid1,SPACE1_RANK,dims1);
    CHECK(ret,FAIL,"H5Pset_space");
    
    tid1=H5Mcreate(fid1,H5_DATATYPE,TYPE1_NAME);
    CHECK(tid1,FAIL,"H5Mcreate");
    
    ret=H5Tset_type(tid1,TYPE1_BASE,TYPE1_LEN,TYPE1_ARCH);
    CHECK(ret,FAIL,"H5Tset_type");
    
    did1=H5Mcreate(fid1,H5_DATASET,DATA1_NAME);
    CHECK(sid1,FAIL,"H5Mcreate");

    ret=H5Dset_info(did1,tid1,sid1);
    CHECK(ret,FAIL,"H5Dset_info");

    ret=H5Dwrite(did1,0,data1);
    CHECK(ret,FAIL,"H5Dwrite");
    
    ret=H5Mrelease(did1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(tid1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(sid1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    /* Close first file */
    ret=H5Fclose(fid1);
    CHECK(ret,FAIL,"H5Fclose");
}   /* test_h5d_basic() */


/****************************************************************
**
**  test_h5d(): Main H5D (dataset) testing routine.
** 
****************************************************************/
void test_h5d(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing datasets\n"));

    test_h5d_basic();       /* Test basic H5D code */
}   /* test_h5d() */

