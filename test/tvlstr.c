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
* Test program:	 tvlstr
*
* Test the Variable-Length String functionality
*
*************************************************************/

#ifdef WIN32
#include <stdio.h>
#endif

#include "testhdf5.h"

#include "hdf5.h"

#define DATAFILE   "tvlstr.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

/* 2-D dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

#define VLSTR_TYPE	"vl_string_type"

/* String for testing attributes */
static const char *string_att = "This is the string for the attribute";

void *test_vlstr_alloc_custom(size_t size, void *info);
void test_vlstr_free_custom(void *mem, void *info);

/****************************************************************
**
**  test_vlstr_alloc_custom(): Test VL datatype custom memory
**      allocation routines.  This routine just uses malloc to
**      allocate the memory and increments the amount of memory
**      allocated.
** 
****************************************************************/
void *test_vlstr_alloc_custom(size_t size, void *info)
{
    void *ret_value=NULL;       /* Pointer to return */
    int *mem_used=(int *)info;  /* Get the pointer to the memory used */
    size_t extra;               /* Extra space needed */

    /*
     *  This weird contortion is required on the DEC Alpha to keep the
     *  alignment correct - QAK
     */
    extra=MAX(sizeof(void *),sizeof(size_t));

    if((ret_value=HDmalloc(extra+size))!=NULL) {
        *(size_t *)ret_value=size;
        *mem_used+=size;
    } /* end if */
    ret_value=((unsigned char *)ret_value)+extra;
    return(ret_value);
}

/****************************************************************
**
**  test_vlstr_free_custom(): Test VL datatype custom memory
**      allocation routines.  This routine just uses free to
**      release the memory and decrements the amount of memory
**      allocated.
** 
****************************************************************/
void test_vlstr_free_custom(void *_mem, void *info)
{
    unsigned char *mem;
    int *mem_used=(int *)info;  /* Get the pointer to the memory used */
    size_t extra;               /* Extra space needed */

    /*
     *  This weird contortion is required on the DEC Alpha to keep the
     *  alignment correct - QAK
     */
    extra=MAX(sizeof(void *),sizeof(size_t));

    if(_mem!=NULL) {
        mem=((unsigned char *)_mem)-extra;
        *mem_used-=*(size_t *)mem;
        HDfree(mem);
    } /* end if */
}

/****************************************************************
**
**  test_vlstrings_basic(): Test basic VL string code.
**      Tests simple VL string I/O
** 
****************************************************************/
static void
test_vlstrings_basic(void)
{
    const char *wdata[SPACE1_DIM1]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure."
        };   /* Information to write */
    char *rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hid_t       xfer_pid;   /* Dataset transfer property list ID */
    hsize_t		dims1[] = {SPACE1_DIM1};
    hsize_t     size;       /* Number of bytes which will be used */
    unsigned       i;          /* counting variable */
    int         str_used;   /* String data in memory */
    int         mem_used=0; /* Memory used during allocation */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic VL String Functionality\n"));

    /* Create file */
    fid1 = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid1 = H5Tcopy (H5T_C_S1);
    CHECK(tid1, FAIL, "H5Tcopy");

    ret = H5Tset_size (tid1,H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Change to the custom memory allocation routines for reading VL string */
    xfer_pid=H5Pcreate(H5P_DATASET_XFER);
    CHECK(xfer_pid, FAIL, "H5Pcreate");

    ret=H5Pset_vlen_mem_manager(xfer_pid,test_vlstr_alloc_custom,&mem_used,test_vlstr_free_custom,&mem_used);
    CHECK(ret, FAIL, "H5Pset_vlen_mem_manager");

    /* Make certain the correct amount of memory will be used */
    ret=H5Dvlen_get_buf_size(dataset,tid1,sid1,&size);
    CHECK(ret, FAIL, "H5Dvlen_get_buf_size");

    /* Count the actual number of bytes used by the strings */
    for(i=0,str_used=0; i<SPACE1_DIM1; i++)
        str_used+=strlen(wdata[i])+1;

    /* Compare against the strings actually written */
    VERIFY(size,(hsize_t)str_used,"H5Dvlen_get_buf_size");

    /* Read dataset from disk */
    ret=H5Dread(dataset,tid1,H5S_ALL,H5S_ALL,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Make certain the correct amount of memory has been used */
    VERIFY(mem_used,str_used,"H5Dread");

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(strlen(wdata[i])!=strlen(rdata[i])) {
            num_errs++;
            printf("VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",(int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            continue;
        } /* end if */
        if( strcmp(wdata[i],rdata[i]) != 0 ) {
            num_errs++;
            printf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",(int)i,wdata[i],(int)i,rdata[i]);
            continue;
        } /* end if */
    } /* end for */

    /* Reclaim the read VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Make certain the VL memory has been freed */
    VERIFY(mem_used,0,"H5Dvlen_reclaim");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close dataset transfer property list */
    ret = H5Pclose(xfer_pid);
    CHECK(ret, FAIL, "H5Pclose");
    
    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_vlstrings_basic() */

/****************************************************************
**
**  test_vlstring_type(): Test VL string type.
**      Tests if VL string is treated as string. 
**
****************************************************************/
static void test_vlstring_type(void)
{
    hid_t               fid;           /* HDF5 File IDs                */
    hid_t		tid_vlstr;
    H5T_cset_t		cset;
    H5T_str_t		pad;
    herr_t              ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing VL String type\n"));

    /* Create file */
    fid = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a datatype to refer to */
    tid_vlstr = H5Tcopy(H5T_C_S1);
    CHECK(tid_vlstr, FAIL, "H5Tcopy");

    /* Change padding and verify it */ 
    ret = H5Tset_strpad(tid_vlstr, H5T_STR_NULLPAD);
    CHECK(ret, FAIL, "H5Tset_strpad");
    pad = H5Tget_strpad(tid_vlstr);
    VERIFY(pad, H5T_STR_NULLPAD, "H5Tget_strpad");

    /* Convert to variable-length string */
    ret = H5Tset_size(tid_vlstr, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    /* Check default character set and padding */
    cset = H5Tget_cset(tid_vlstr);
    VERIFY(cset, H5T_CSET_ASCII, "H5Tget_cset");
    pad = H5Tget_strpad(tid_vlstr);
    VERIFY(pad, H5T_STR_NULLPAD, "H5Tget_strpad");

    /* Commit variable-length string datatype to storage */
    ret = H5Tcommit(fid, VLSTR_TYPE, tid_vlstr);
    CHECK(ret, FAIL, "H5Tcommit");

    ret = H5Tclose(tid_vlstr);
    CHECK(ret, FAIL, "H5Tclose");

    /* Open the variable-length string datatype just created */ 
    tid_vlstr = H5Topen(fid, VLSTR_TYPE);
    CHECK(tid_vlstr, FAIL, "H5Topen");

    /* Verify character set and padding */ 
    cset = H5Tget_cset(tid_vlstr);
    VERIFY(cset, H5T_CSET_ASCII, "H5Tget_cset");
    pad = H5Tget_strpad(tid_vlstr);
    VERIFY(pad, H5T_STR_NULLPAD, "H5Tget_strpad");

    /* Close datatype and file */
    ret = H5Tclose(tid_vlstr);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");    

} /* end test_vlstring_type() */

/****************************************************************
**
**  test_write_vl_string_attribute(): Test basic VL string code.
**      Tests writing VL strings as attributes
** 
****************************************************************/
static void test_write_vl_string_attribute(void)
{
    hid_t file, root, dataspace, att;
    hid_t type;
    herr_t ret;
    char *string_att_check;

    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create a datatype to refer to. */
    type = H5Tcopy (H5T_C_S1);
    CHECK(type, FAIL, "H5Tcopy");

    ret = H5Tset_size (type, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    root = H5Gopen(file, "/");
    CHECK(root, FAIL, "H5Gopen");

    dataspace = H5Screate(H5S_SCALAR);
    CHECK(dataspace, FAIL, "H5Screate");

    att = H5Acreate(root, "test_scalar", type, dataspace, H5P_DEFAULT);
    CHECK(att, FAIL, "H5Acreate");

    ret = H5Awrite(att, type, &string_att);
    CHECK(ret, FAIL, "H5Awrite");

    ret = H5Aread(att, type, &string_att_check);
    CHECK(ret, FAIL, "H5Aread");

    if(HDstrcmp(string_att_check,string_att)!=0) {
        num_errs++;
        printf("VL string attributes don't match!, string_att=%s, string_att_check=%s\n",string_att,string_att_check);
    } /* end if */

    free(string_att_check);

    ret = H5Aclose(att);
    CHECK(ret, FAIL, "HAclose");

    ret = H5Gclose(root);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    return;
}

/****************************************************************
**
**  test_read_vl_string_attribute(): Test basic VL string code.
**      Tests reading VL strings from attributes
** 
****************************************************************/
static void test_read_vl_string_attribute(void)
{
    hid_t file, root, att;
    hid_t type;
    herr_t ret;
    char *string_att_check;

    file = H5Fopen(DATAFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    /* Create a datatype to refer to. */
    type = H5Tcopy (H5T_C_S1);
    CHECK(type, FAIL, "H5Tcopy");

    ret = H5Tset_size (type, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    root = H5Gopen(file, "/");
    CHECK(root, FAIL, "H5Gopen");

    att = H5Aopen_name(root, "test_scalar");
    CHECK(att, FAIL, "H5Aopen_name");

    ret = H5Aread(att, type, &string_att_check);
    CHECK(ret, FAIL, "H5Aread");

    if(HDstrcmp(string_att_check,string_att)!=0) {
        num_errs++;
        printf("VL string attributes don't match!, string_att=%s, string_att_check=%s\n",string_att,string_att_check);
    } /* end if */

    free(string_att_check);

    ret = H5Aclose(att);
    CHECK(ret, FAIL, "HAclose");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Gclose(root);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    return;
}

/****************************************************************
**
**  test_vlstrings(): Main VL string testing routine.
** 
****************************************************************/
void 
test_vlstrings(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Variable-Length Strings\n"));

    /* These next tests use the same file */
    test_vlstrings_basic();       /* Test basic VL string datatype */
    test_vlstring_type();

    /* Test using VL strings in attributes */
    test_write_vl_string_attribute();
    test_read_vl_string_attribute();
}   /* test_vlstrings() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_vlstrings
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              September 10, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_vlstrings(void)
{
    remove(DATAFILE);
}

