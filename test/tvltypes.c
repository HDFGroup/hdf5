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
* Test program:	 tvltypes
*
* Test the Variable-Length Datatype functionality
*
*************************************************************/

#include <testhdf5.h>

#include <hdf5.h>

#define FILE   "tvltypes.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

/* 2-D dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

void *test_vltypes_alloc_custom(size_t size, void *info);
void test_vltypes_free_custom(void *mem, void *info);

/****************************************************************
**
**  test_vltypes_alloc_custom(): Test VL datatype custom memory
**      allocation routines.  This routine just uses malloc to
**      allocate the memory and increments the amount of memory
**      allocated.
** 
****************************************************************/
void *test_vltypes_alloc_custom(size_t size, void *info)
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
**  test_vltypes_free_custom(): Test VL datatype custom memory
**      allocation routines.  This routine just uses free to
**      release the memory and decrements the amount of memory
**      allocated.
** 
****************************************************************/
void test_vltypes_free_custom(void *_mem, void *info)
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
**  test_vltypes_vlen_atomic(): Test basic VL datatype code.
**      Tests VL datatypes of atomic datatypes
** 
****************************************************************/
static void 
test_vltypes_vlen_atomic(void)
{
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hid_t       xfer_pid;   /* Dataset transfer property list ID */
    hsize_t		dims1[] = {SPACE1_DIM1};
    hsize_t     size;       /* Number of bytes which will be used */
    uintn       i,j;        /* counting variables */
    int         mem_used=0; /* Memory used during allocation */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Atomic VL Datatype Functionality\n"));

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(unsigned int));
        wdata[i].len=i+1;
        for(j=0; j<(i+1); j++)
            ((unsigned int *)wdata[i].p)[j]=i*10+j;
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);
    CHECK(tid1, FAIL, "H5Tvlen_create");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Change to the custom memory allocation routines for reading VL data */
    xfer_pid=H5Pcreate(H5P_DATA_XFER);
    CHECK(xfer_pid, FAIL, "H5Pcreate");

    ret=H5Pset_vlen_mem_manager(xfer_pid,test_vltypes_alloc_custom,&mem_used,test_vltypes_free_custom,&mem_used);
    CHECK(ret, FAIL, "H5Pset_vlen_mem_manager");

    /* Make certain the correct amount of memory will be used */
    ret=H5Dvlen_get_buf_size(dataset,tid1,sid1,&size);
    CHECK(ret, FAIL, "H5Dvlen_get_buf_size");

    /* 10 elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    VERIFY(size,10*sizeof(unsigned int),"H5Dvlen_get_buf_size");

    /* Read dataset from disk */
    ret=H5Dread(dataset,tid1,H5S_ALL,H5S_ALL,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Make certain the correct amount of memory has been used */
    /* 10 elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    VERIFY(mem_used,10*sizeof(unsigned int),"H5Dread");

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(wdata[i].len!=rdata[i].len) {
            num_errs++;
            printf("VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
            continue;
        } /* end if */
        for(j=0; j<rdata[i].len; j++) {
            if( ((unsigned int *)wdata[i].p)[j] != ((unsigned int *)rdata[i].p)[j] ) {
                num_errs++;
                printf("VL data values don't match!, wdata[%d].p[%d]=%d, rdata[%d].p[%d]=%d\n",(int)i,(int)j, (int)((unsigned int *)wdata[i].p)[j], (int)i,(int)j, (int)((unsigned int *)rdata[i].p)[j]);
                continue;
            } /* end if */
        } /* end for */
    } /* end for */

    /* Reclaim the read VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Make certain the VL memory has been freed */
    VERIFY(mem_used,0,"H5Dvlen_reclaim");

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

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

} /* end test_vltypes_vlen_atomic() */

/****************************************************************
**
**  test_vltypes_vlen_compound(): Test basic VL datatype code.
**      Tests VL datatypes of compound datatypes
** 
****************************************************************/
static void 
test_vltypes_vlen_compound(void)
{
    typedef struct {             /* Struct that the VL sequences are composed of */
        int i;
        float f;
    } s1;
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1, tid2; /* Datatype IDs         */
    hid_t       xfer_pid;   /* Dataset transfer property list ID */
    hsize_t		dims1[] = {SPACE1_DIM1};
    hsize_t     size;       /* Number of bytes which will be used */
    uintn       i,j;        /* counting variables */
    int         mem_used=0; /* Memory used during allocation */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Compound VL Datatype Functionality\n"));

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(s1));
        wdata[i].len=i+1;
        for(j=0; j<(i+1); j++) {
            ((s1 *)wdata[i].p)[j].i=i*10+j;
            ((s1 *)wdata[i].p)[j].f=(i*20+j)/3.0;
          } /* end for */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create the base compound type */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1));
    CHECK(tid2, FAIL, "H5Tcreate");

    /* Insert fields */
    ret=H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");
    ret=H5Tinsert(tid2, "f", HOFFSET(s1, f), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create a datatype to refer to */
    tid1 = H5Tvlen_create (tid2);
    CHECK(tid1, FAIL, "H5Tvlen_create");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Change to the custom memory allocation routines for reading VL data */
    xfer_pid=H5Pcreate(H5P_DATA_XFER);
    CHECK(xfer_pid, FAIL, "H5Pcreate");

    ret=H5Pset_vlen_mem_manager(xfer_pid,test_vltypes_alloc_custom,&mem_used,test_vltypes_free_custom,&mem_used);
    CHECK(ret, FAIL, "H5Pset_vlen_mem_manager");

    /* Make certain the correct amount of memory will be used */
    ret=H5Dvlen_get_buf_size(dataset,tid1,sid1,&size);
    CHECK(ret, FAIL, "H5Dvlen_get_buf_size");

    /* 10 elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    VERIFY(size,10*sizeof(s1),"H5Dvlen_get_buf_size");

    /* Read dataset from disk */
    ret=H5Dread(dataset,tid1,H5S_ALL,H5S_ALL,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Make certain the correct amount of memory has been used */
    /* 10 elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    VERIFY(mem_used,10*sizeof(s1),"H5Dread");

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(wdata[i].len!=rdata[i].len) {
            num_errs++;
            printf("VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
            continue;
        } /* end if */
        for(j=0; j<rdata[i].len; j++) {
            if( ((s1 *)wdata[i].p)[j].i != ((s1 *)rdata[i].p)[j].i ) {
                num_errs++;
                printf("VL data values don't match!, wdata[%d].p[%d].i=%d, rdata[%d].p[%d].i=%d\n",(int)i,(int)j, (int)((s1 *)wdata[i].p)[j].i, (int)i,(int)j, (int)((s1 *)rdata[i].p)[j].i);
                continue;
            } /* end if */
            if( ((s1 *)wdata[i].p)[j].f != ((s1 *)rdata[i].p)[j].f ) {
                num_errs++;
                printf("VL data values don't match!, wdata[%d].p[%d].f=%f, rdata[%d].p[%d].f=%f\n",(int)i,(int)j, (double)((s1 *)wdata[i].p)[j].f, (int)i,(int)j, (double)((s1 *)rdata[i].p)[j].f);
                continue;
            } /* end if */
        } /* end for */
    } /* end for */

    /* Reclaim the VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Make certain the VL memory has been freed */
    VERIFY(mem_used,0,"H5Dvlen_reclaim");

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close datatype */
    ret = H5Tclose(tid2);
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

} /* end test_vltypes_vlen_compound() */

/****************************************************************
**
**  test_vltypes_compound_vlen_atomic(): Test basic VL datatype code.
**      Tests compound datatypes with VL datatypes of atomic datatypes.
** 
****************************************************************/
static void 
test_vltypes_compound_vlen_atomic(void)
{
    typedef struct {             /* Struct that the VL sequences are composed of */
        int i;
        float f;
        hvl_t v;
    } s1;
    s1 wdata[SPACE1_DIM1];   /* Information to write */
    s1 rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1, tid2; /* Datatype IDs         */
    hid_t       xfer_pid;   /* Dataset transfer property list ID */
    hsize_t		dims1[] = {SPACE1_DIM1};
    hsize_t     size;       /* Number of bytes which will be used */
    uintn       i,j;        /* counting variables */
    int         mem_used=0; /* Memory used during allocation */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Compound Datatypes with VL Atomic Datatype Component Functionality\n"));

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].i=i*10;
        wdata[i].f=(i*20)/3.0;
        wdata[i].v.p=malloc((i+1)*sizeof(unsigned int));
        wdata[i].v.len=i+1;
        for(j=0; j<(i+1); j++)
            ((unsigned int *)wdata[i].v.p)[j]=i*10+j;
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);
    CHECK(tid1, FAIL, "H5Tvlen_create");

    /* Create the base compound type */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1));
    CHECK(tid2, FAIL, "H5Tcreate");

    /* Insert fields */
    ret=H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");
    ret=H5Tinsert(tid2, "f", HOFFSET(s1, f), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");
    ret=H5Tinsert(tid2, "v", HOFFSET(s1, v), tid1);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid2,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid2,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Change to the custom memory allocation routines for reading VL data */
    xfer_pid=H5Pcreate(H5P_DATA_XFER);
    CHECK(xfer_pid, FAIL, "H5Pcreate");

    ret=H5Pset_vlen_mem_manager(xfer_pid,test_vltypes_alloc_custom,&mem_used,test_vltypes_free_custom,&mem_used);
    CHECK(ret, FAIL, "H5Pset_vlen_mem_manager");

    /* Make certain the correct amount of memory will be used */
    ret=H5Dvlen_get_buf_size(dataset,tid2,sid1,&size);
    CHECK(ret, FAIL, "H5Dvlen_get_buf_size");

    /* 10 elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    VERIFY(size,10*sizeof(unsigned int),"H5Dvlen_get_buf_size");

    /* Read dataset from disk */
    ret=H5Dread(dataset,tid2,H5S_ALL,H5S_ALL,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Make certain the correct amount of memory has been used */
    /* 10 elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    VERIFY(mem_used,10*sizeof(unsigned int),"H5Dread");

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(wdata[i].i!=rdata[i].i) {
            num_errs++;
            printf("Integer components don't match!, wdata[%d].i=%d, rdata[%d].i=%d\n",(int)i,(int)wdata[i].i,(int)i,(int)rdata[i].i);
            continue;
        } /* end if */
        if(wdata[i].f!=rdata[i].f) {
            num_errs++;
            printf("Float components don't match!, wdata[%d].f=%f, rdata[%d].f=%f\n",(int)i,(double)wdata[i].f,(int)i,(double)rdata[i].f);
            continue;
        } /* end if */
        if(wdata[i].v.len!=rdata[i].v.len) {
            num_errs++;
            printf("VL data length don't match!, wdata[%d].v.len=%d, rdata[%d].v.len=%d\n",(int)i,(int)wdata[i].v.len,(int)i,(int)rdata[i].v.len);
            continue;
        } /* end if */
        for(j=0; j<rdata[i].v.len; j++) {
            if( ((unsigned int *)wdata[i].v.p)[j] != ((unsigned int *)rdata[i].v.p)[j] ) {
                num_errs++;
                printf("VL data values don't match!, wdata[%d].v.p[%d]=%d, rdata[%d].v.p[%d]=%d\n",(int)i,(int)j, (int)((unsigned int *)wdata[i].v.p)[j], (int)i,(int)j, (int)((unsigned int *)rdata[i].v.p)[j]);
                continue;
            } /* end if */
        } /* end for */
    } /* end for */

    /* Reclaim the VL data */
    ret=H5Dvlen_reclaim(tid2,sid1,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Make certain the VL memory has been freed */
    VERIFY(mem_used,0,"H5Dvlen_reclaim");

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid2,sid1,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

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

} /* end test_vltypes_compound_vlen_atomic() */

/****************************************************************
**
**  test_vltypes_vlen_vlen_atomic(): Test basic VL datatype code.
**      Tests VL datatype with VL datatypes of atomic datatypes.
** 
****************************************************************/
static void 
test_vltypes_vlen_vlen_atomic(void)
{
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t rdata[SPACE1_DIM1];   /* Information read in */
    hvl_t *t1, *t2;             /* Temporary pointer to VL information */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1, tid2; /* Datatype IDs         */
    hid_t       xfer_pid;   /* Dataset transfer property list ID */
    hsize_t		dims1[] = {SPACE1_DIM1};
    hsize_t     size;       /* Number of bytes which will be used */
    uintn       i,j,k;      /* counting variables */
    int         mem_used=0; /* Memory used during allocation */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing VL Datatypes with VL Atomic Datatype Component Functionality\n"));

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(hvl_t));
        wdata[i].len=i+1;
        for(t1=wdata[i].p,j=0; j<(i+1); j++, t1++) {
            t1->p=malloc((j+1)*sizeof(unsigned int));
            t1->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)t1->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);
    CHECK(tid1, FAIL, "H5Tvlen_create");

    /* Create the base VL type */
    tid2 = H5Tvlen_create (tid1);
    CHECK(tid2, FAIL, "H5Tvlen_create");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid2,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid2,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    
    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Create file */
    fid1 = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);
    CHECK(tid1, FAIL, "H5Tvlen_create");

    /* Create the base VL type */
    tid2 = H5Tvlen_create (tid1);
    CHECK(tid2, FAIL, "H5Tvlen_create");

    /* Open a dataset */
    dataset=H5Dopen(fid1,"Dataset1");
    CHECK(dataset, FAIL, "H5Dopen");

    /* Change to the custom memory allocation routines for reading VL data */
    xfer_pid=H5Pcreate(H5P_DATA_XFER);
    CHECK(xfer_pid, FAIL, "H5Pcreate");

    ret=H5Pset_vlen_mem_manager(xfer_pid,test_vltypes_alloc_custom,&mem_used,test_vltypes_free_custom,&mem_used);
    CHECK(ret, FAIL, "H5Pset_vlen_mem_manager");

    /* Make certain the correct amount of memory was used */
    ret=H5Dvlen_get_buf_size(dataset,tid2,sid1,&size);
    CHECK(ret, FAIL, "H5Dvlen_get_buf_size");

    /* 10 hvl_t elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    /* 20 unsigned int elements allocated = 1 + 3 + 6 + 10 elements */
    VERIFY(size,10*sizeof(hvl_t)+20*sizeof(unsigned int),"H5Dvlen_get_buf_size");

    /* Read dataset from disk */
    ret=H5Dread(dataset,tid2,H5S_ALL,H5S_ALL,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Make certain the correct amount of memory has been used */
    /* 10 hvl_t elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    /* 20 unsigned int elements allocated = 1 + 3 + 6 + 10 elements */
    VERIFY(mem_used,10*sizeof(hvl_t)+20*sizeof(unsigned int),"H5Dread");

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(wdata[i].len!=rdata[i].len) {
            num_errs++;
            printf("VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
            continue;
        } /* end if */
        for(t1=wdata[i].p, t2=rdata[i].p, j=0; j<rdata[i].len; j++, t1++, t2++) {
            if(t1->len!=t2->len) {
                num_errs++;
                printf("VL data length don't match!, i=%d, j=%d, t1->len=%d, t2->len=%d\n",(int)i,(int)j,(int)t1->len,(int)t2->len);
                continue;
            } /* end if */
            for(k=0; k<t2->len; k++) {
                if( ((unsigned int *)t1->p)[k] != ((unsigned int *)t2->p)[k] ) {
                    num_errs++;
                    printf("VL data values don't match!, t1->p[%d]=%d, t2->p[%d]=%d\n",(int)k, (int)((unsigned int *)t1->p)[k], (int)k, (int)((unsigned int *)t2->p)[k]);
                    continue;
                } /* end if */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Reclaim all the (nested) VL data */
    ret=H5Dvlen_reclaim(tid2,sid1,xfer_pid,rdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Make certain the VL memory has been freed */
    VERIFY(mem_used,0,"H5Dvlen_reclaim");

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid2,sid1,H5P_DEFAULT,wdata);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

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

} /* end test_vltypes_vlen_vlen_atomic() */

/****************************************************************
**
**  test_vltypes(): Main VL datatype testing routine.
** 
****************************************************************/
void 
test_vltypes(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Variable-Length Datatypes\n"));

    /* These next tests use the same file */
    test_vltypes_vlen_atomic();       /* Test VL atomic datatypes */
    test_vltypes_vlen_compound();     /* Test VL compound datatypes */
    test_vltypes_compound_vlen_atomic();     /* Test compound datatypes with VL atomic components */
    test_vltypes_vlen_vlen_atomic();  /* Test VL datatype with VL atomic components */
}   /* test_vltypes() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_vltypes
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              June 8, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_vltypes(void)
{
    remove(FILE);
}

