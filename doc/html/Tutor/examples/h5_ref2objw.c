
#include <hdf5.h>

#define FILE1   "trefer1.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

/* 2-D dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

int 
main(void) {
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    hobj_ref_t      *wbuf;      /* buffer to write to disk */
    int       *tu32;      /* Temporary pointer to int data */
    int        i;          /* counting variables */
    const char *write_comment="Foo!"; /* Comments for group */
    herr_t		ret;		/* Generic return value		*/

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

    /* Allocate write buffers */
    wbuf=(hobj_ref_t *)malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    tu32=malloc(sizeof(int)*SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a group */
    group=H5Gcreate(fid1,"Group1",-1);

    /* Set group's comment */
    ret=H5Gset_comment(group,".",write_comment);

    /* Create a dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset1",H5T_STD_U32LE,sid1,H5P_DEFAULT);

    for(i=0; i<SPACE1_DIM1; i++)
        tu32[i] = i*3;

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,tu32);

    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Create another dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset2",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Create a datatype to refer to */
    tid1 = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));

    /* Insert fields */
    ret=H5Tinsert (tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);

    ret=H5Tinsert (tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);

    ret=H5Tinsert (tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT);

    /* Save datatype for later */
    ret=H5Tcommit (group, "Datatype1", tid1);

    /* Close datatype */
    ret = H5Tclose(tid1);

    /* Close group */
    ret = H5Gclose(group);

    /* Create a dataset to store references */
    dataset=H5Dcreate(fid1,"Dataset3",H5T_STD_REF_OBJ,sid1,H5P_DEFAULT);

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf[0],fid1,"/Group1/Dataset1",H5R_OBJECT,-1);

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf[1],fid1,"/Group1/Dataset2",H5R_OBJECT,-1);

    /* Create reference to group */
    ret = H5Rcreate(&wbuf[2],fid1,"/Group1",H5R_OBJECT,-1);

    /* Create reference to named datatype */
    ret = H5Rcreate(&wbuf[3],fid1,"/Group1/Datatype1",H5R_OBJECT,-1);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    
    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Close file */
    ret = H5Fclose(fid1);
    free(wbuf);
    free(tu32);
    return 0;
}
