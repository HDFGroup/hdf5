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
* Test program:  tfile
*
* Test the low-level file I/O features.
*
*************************************************************/

#include <testhdf5.h>

#include <H5private.h>
#include <H5Bprivate.h>
#include <H5Mprivate.h>
#include <H5Tprivate.h>

#define FILE   "th5t1.h5"

#define TYPE1_NAME  "Type1"
#define TYPE1_BASE  H5T_INT
#define TYPE1_LEN   4
#define TYPE1_ARCH  H5T_BIGENDIAN

#define TYPE2_NAME  "Type2"
#define TYPE2_BASE  H5T_FLOAT
#define TYPE2_LEN   8
#define TYPE2_ARCH  H5T_LITTLEENDIAN

/****************************************************************
**
**  test_h5t_basic(): Test basic H5T (datatype) code.
** 
****************************************************************/
static void test_h5t_basic(void)
{
    hatom_t fid1;   /* HDF5 File IDs */
    hatom_t tid1,tid2;   /* Datatype ID */
    hatom_t type;   /* Datatype's base type */
    uint8 len, arch;    /* Datatype's length and architecture */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Datatype Manipulation\n"));

    /* Create file */
    fid1=H5Fcreate(FILE,H5ACC_OVERWRITE,0,0);
    CHECK(fid1,FAIL,"H5Fcreate");

    tid1=H5Mcreate(fid1,H5_DATATYPE,TYPE1_NAME);
    CHECK(tid1,FAIL,"H5Mcreate");
    
    ret=H5Tset_type(tid1,TYPE1_BASE,TYPE1_LEN,TYPE1_ARCH);
    CHECK(ret,FAIL,"H5Tset_type");
    
    ret=H5Tsize(tid1,-1,-1,BTRUE);
    VERIFY(ret,TYPE1_LEN,"H5Tsize");
    
    ret=H5Tis_atomic(tid1);
    VERIFY(ret,BTRUE,"H5Tis_atomic");
    
    ret=H5Tget_type(tid1,&type,&len,&arch);
    CHECK(ret,FAIL,"H5Tget_type");
    VERIFY(type,TYPE1_BASE,"H5Tget_type");
    VERIFY(len,TYPE1_LEN,"H5Tget_type");
    VERIFY(arch,TYPE1_ARCH,"H5Tget_type");
    
    tid2=H5Mcreate(fid1,H5_DATATYPE,TYPE2_NAME);
    CHECK(tid1,FAIL,"H5Mcreate");
    
    ret=H5Tset_type(tid2,TYPE2_BASE,TYPE2_LEN,TYPE2_ARCH);
    CHECK(ret,FAIL,"H5Tset_type");
    
    ret=H5Tsize(tid2,-1,-1,BTRUE);
    VERIFY(ret,TYPE2_LEN,"H5Tsize");
    
    ret=H5Tis_atomic(tid2);
    VERIFY(ret,BTRUE,"H5Tis_atomic");
    
    ret=H5Tget_type(tid2,&type,&len,&arch);
    CHECK(ret,FAIL,"H5Tget_type");
    VERIFY(type,TYPE2_BASE,"H5Tget_type");
    VERIFY(len,TYPE2_LEN,"H5Tget_type");
    VERIFY(arch,TYPE2_ARCH,"H5Tget_type");
    
    ret=H5Mrelease(tid1);
    CHECK(ret,FAIL,"H5Mrelease");
    
    ret=H5Mrelease(tid2);
    CHECK(ret,FAIL,"H5Mrelease");
    
    /* Close first file */
    ret=H5Fclose(fid1);
    CHECK(ret,FAIL,"H5Fclose");
}   /* test_h5t_basic() */


/****************************************************************
**
**  test_h5t(): Main H5T (datatype) testing routine.
** 
****************************************************************/
void test_h5t(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Datatypes\n"));

    test_h5t_basic();       /* Test basic H5T code */
}   /* test_h5t() */

