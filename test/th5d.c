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

/* 4-D dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK     4
#define SPACE2_DIM1     3
#define SPACE2_DIM2     15
#define SPACE2_DIM3     13
#define SPACE2_DIM4     7 

/* 64-bit big-endian floating-point type */
#define TYPE2_NAME  "Type2"
#define TYPE2_BASE  H5T_FLOAT
#define TYPE2_LEN   8
#define TYPE2_ARCH  H5T_BIGENDIAN

/* 4-d 64-bit floating-point dataset */
#define DATA2_NAME  "Data2"
float64 data2[SPACE2_DIM1][SPACE2_DIM2][SPACE2_DIM3][SPACE2_DIM4]=
    {0.5,1.0,2.0,4.0,8.0,16.0,32.0};

/****************************************************************
**
**  test_h5d_basic_write(): Test basic H5D (dataset) writing code.
** 
****************************************************************/
static void test_h5d_basic_write(void)
{
    hatom_t fid1;   /* HDF5 File IDs */
    hatom_t did1;   /* Dataset ID */
    hatom_t tid1;   /* Datatype ID */
    hatom_t sid1;   /* Dataspace ID */
    uint32 dims1[]={SPACE1_DIM1,SPACE1_DIM2,SPACE1_DIM3};   /* dataspace dim sizes */
    hatom_t did2;   /* Dataset ID */
    hatom_t tid2;   /* Datatype ID */
    hatom_t sid2;   /* Dataspace ID */
    uint32 dims2[]={SPACE2_DIM1,SPACE2_DIM2,SPACE2_DIM3,SPACE2_DIM4};   /* dataspace dim sizes */
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

    ret=H5Dwrite(did1,H5P_SCALAR,data1);
    CHECK(ret,FAIL,"H5Dwrite");
    
    ret=H5Mrelease(did1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(tid1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    sid2=H5Mcreate(fid1,H5_DATASPACE,SPACE2_NAME);
    CHECK(sid1,FAIL,"H5Mcreate");

    ret=H5Pset_space(sid2,SPACE2_RANK,dims2);
    CHECK(ret,FAIL,"H5Pset_space");
    
    tid2=H5Mcreate(fid1,H5_DATATYPE,TYPE2_NAME);
    CHECK(tid2,FAIL,"H5Mcreate");
    
    ret=H5Tset_type(tid2,TYPE2_BASE,TYPE2_LEN,TYPE2_ARCH);
    CHECK(ret,FAIL,"H5Tset_type");
    
    did2=H5Mcreate(fid1,H5_DATASET,DATA2_NAME);
    CHECK(sid2,FAIL,"H5Mcreate");

    ret=H5Dset_info(did2,tid2,sid2);
    CHECK(ret,FAIL,"H5Dset_info");

    ret=H5Dwrite(did2,H5P_SCALAR,data2);
    CHECK(ret,FAIL,"H5Dwrite");
    
    ret=H5Mrelease(did2);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(tid2);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(sid2);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(sid1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    /* Close first file */
    ret=H5Fclose(fid1);
    CHECK(ret,FAIL,"H5Fclose");
}   /* test_h5d_basic_write() */

/****************************************************************
**
**  test_h5d_basic_read(): Test basic H5D (dataset) reading code.
** 
****************************************************************/
static void test_h5d_basic_read(void)
{
    hatom_t fid1;   /* HDF5 File IDs */
    hatom_t oid1;   /* Dataset OID */
    hatom_t did1;   /* Dataset ID */
    hatom_t tid1;   /* Datatype ID */
    hatom_t sid1;   /* Dataspace ID */
    void *buf;      /* space for the buffer read */
    uint32 buf_size;    /* size of the buffer to read */
    hatom_t type;   /* Datatype's base type */
    uint8 len, arch;    /* Datatype's length and architecture */
    uintn n;            /* number of dataspace elements */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Datatype Manipulation\n"));

    /* Create file */
    fid1=H5Fopen(FILE,0,0);
    CHECK(fid1,FAIL,"H5Fopen");

    oid1=H5Mfind_name(fid1,H5_DATASET,DATA1_NAME);
    CHECK(oid1,FAIL,"H5Mfind_name");

    did1=H5Maccess(oid1);
    CHECK(did1,FAIL,"H5Maccess");

    ret=H5Dget_info(did1,&tid1,&sid1);
    CHECK(ret,FAIL,"H5Pset_space");
    
    ret=H5Tis_atomic(tid1);
    VERIFY(ret,BTRUE,"H5Tis_atomic");
    
    ret=H5Tget_type(tid1,&type,&len,&arch);
    CHECK(ret,FAIL,"H5Tget_type");
    VERIFY(type,TYPE1_BASE,"H5Tget_type");
    VERIFY(len,TYPE1_LEN,"H5Tget_type");
    VERIFY(arch,TYPE1_ARCH,"H5Tget_type");

    n=H5Pnelem(sid1);
    CHECK(n,UFAIL,"H5Pnelem");
    VERIFY(n,SPACE1_DIM1*SPACE1_DIM2*SPACE1_DIM3,"H5Pnelem");
    
    buf_size=H5Tsize(tid1,len,arch,BTRUE)*n;
    buf=HDmalloc(buf_size);
    ret=H5Dread(did1,H5P_SCALAR,buf);
    CHECK(ret,FAIL,"H5Dread");
    VERIFY(HDmemcmp(buf,data1,buf_size),0,"H5Dread");
    
    ret=H5Mrelease(tid1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(sid1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(did1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    /* Close first file */
    ret=H5Fclose(fid1);
    CHECK(ret,FAIL,"H5Fclose");
}   /* test_h5d_basic_read() */


/****************************************************************
**
**  test_h5d(): Main H5D (dataset) testing routine.
** 
****************************************************************/
void test_h5d(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing datasets\n"));

    test_h5d_basic_write();      /* Test basic H5D writing code */
    test_h5d_basic_read();       /* Test basic H5D reading code */
}   /* test_h5d() */

