#include <stdlib.h>
#include "hdf5.h"

#define FILE2	"trefer2.h5"
#define NPOINTS 10
 
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
main(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dset1,	/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t		sid1,       /* Dataspace ID	#1		*/
                sid2;       /* Dataspace ID	#2		*/
    hsize_t *   coords;             /* Coordinate buffer */
    hsize_t		low[SPACE2_RANK];   /* Selection bounds */
    hsize_t		high[SPACE2_RANK];     /* Selection bounds */
    hdset_reg_ref_t      *rbuf;      /* buffer to to read disk */
    int    *drbuf;      /* Buffer for reading numeric data from disk */
    int        i, j;          /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */

    /* Allocate write & read buffers */
    rbuf=malloc(sizeof(hdset_reg_ref_t)*SPACE1_DIM1);
    drbuf=calloc(sizeof(int),SPACE2_DIM1*SPACE2_DIM2);

    /* Open the file */
    fid1 = H5Fopen(FILE2, H5F_ACC_RDWR, H5P_DEFAULT);

    /* Open the dataset */
    dset1=H5Dopen(fid1,"/Dataset1");

    /* Read selection from disk */
    ret=H5Dread(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);

    /* Try to open objects */
    dset2 = H5Rdereference(dset1,H5R_DATASET_REGION,&rbuf[0]);

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);

    ret=H5Sget_simple_extent_npoints(sid1);
    printf(" Number of elements in the dataset is : %d\n",ret);

    /* Read from disk */
    ret=H5Dread(dset2,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,drbuf);

    for(i=0; i<SPACE2_DIM1; i++) {
        for (j=0; j<SPACE2_DIM2; j++) printf (" %d ", drbuf[i*SPACE2_DIM2+j]);
        printf("\n"); }

    /* Get the hyperslab selection */
    sid2=H5Rget_region(dset1,H5R_DATASET_REGION,&rbuf[0]);

    /* Verify correct hyperslab selected */
    ret = H5Sget_select_npoints(sid2);
    printf(" Number of elements in the hyperslab is : %d \n", ret);
    ret = H5Sget_select_hyper_nblocks(sid2);
    coords=malloc(ret*SPACE2_RANK*sizeof(hsize_t)*2); /* allocate space for the hyperslab blocks */
    ret = H5Sget_select_hyper_blocklist(sid2,0,ret,coords);
    printf(" Hyperslab coordinates are : \n");
    printf (" ( %lu , %lu ) ( %lu , %lu ) \n", \
(unsigned long)coords[0],(unsigned long)coords[1],(unsigned long)coords[2],(unsigned long)coords[3]); 
    free(coords);
    ret = H5Sget_select_bounds(sid2,low,high);

    /* Close region space */
    ret = H5Sclose(sid2);

    /* Get the element selection */
    sid2=H5Rget_region(dset1,H5R_DATASET_REGION,&rbuf[1]);

    /* Verify correct elements selected */
    ret = H5Sget_select_elem_npoints(sid2);
    printf(" Number of selected elements is : %d\n", ret);

    /* Allocate space for the element points */
    coords= malloc(ret*SPACE2_RANK*sizeof(hsize_t)); 
    ret = H5Sget_select_elem_pointlist(sid2,0,ret,coords);
    printf(" Coordinates of selected elements are : \n");
    for (i=0; i<2*NPOINTS; i=i+2) 
         printf(" ( %lu , %lu ) \n", (unsigned long)coords[i],(unsigned long)coords[i+1]); 
          
    free(coords);
    ret = H5Sget_select_bounds(sid2,low,high);

    /* Close region space */
    ret = H5Sclose(sid2);

    /* Close first space */
    ret = H5Sclose(sid1);

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);

    /* Close Dataset */
    ret = H5Dclose(dset1);

    /* Close file */
    ret = H5Fclose(fid1);

    /* Free memory buffers */
    free(rbuf);
    free(drbuf);
    return 0;
}   
