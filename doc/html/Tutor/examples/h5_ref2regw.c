#include <stdlib.h>
#include <hdf5.h>

#define FILE2	"trefer2.h5"
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK     1
#define SPACE1_DIM1     4

/* Dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

/* Element selection information */
#define POINT1_NPOINTS 10

int
main(void)
{
    hid_t	fid1;		/* HDF5 File IDs		*/
    hid_t	dset1,		/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t	sid1,       /* Dataspace ID	#1		*/
                sid2;       /* Dataspace ID	#2		*/
    hsize_t	dims1[] = {SPACE1_DIM1},
            	dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t	start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t	stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t	count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t	block[SPACE2_RANK];     /* Block size of hyperslab */
    hsize_t	coord1[POINT1_NPOINTS][SPACE2_RANK]; 
                                    /* Coordinates for point selection */
    hdset_reg_ref_t      *wbuf;      /* buffer to write to disk */
    int     *dwbuf;      /* Buffer for writing numeric data to disk */
    int        i;          /* counting variables */
    herr_t		ret;		/* Generic return value		*/


    /* Allocate write & read buffers */
    wbuf=calloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    dwbuf=malloc(sizeof(int)*SPACE2_DIM1*SPACE2_DIM2);

    /* Create file */
    fid1 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);

    /* Create a dataset */
    dset2=H5Dcreate(fid1,"Dataset2",H5T_STD_U8LE,sid2,H5P_DEFAULT);

    for(i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++)
        dwbuf[i]=i*3;

    /* Write selection to disk */
    ret=H5Dwrite(dset2,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,dwbuf);

    /* Close Dataset */
    ret = H5Dclose(dset2);

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a dataset */
    dset1=H5Dcreate(fid1,"Dataset1",H5T_STD_REF_DSETREG,sid1,H5P_DEFAULT);

    /* Create references */

    /* Select 6x6 hyperslab for first reference */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=6; count[1]=6;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);

    /* Store first dataset region */
    ret = H5Rcreate(&wbuf[0],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Select sequence of ten points for second reference */
    coord1[0][0]=6; coord1[0][1]=9;
    coord1[1][0]=2; coord1[1][1]=2;
    coord1[2][0]=8; coord1[2][1]=4;
    coord1[3][0]=1; coord1[3][1]=6;
    coord1[4][0]=2; coord1[4][1]=8;
    coord1[5][0]=3; coord1[5][1]=2;
    coord1[6][0]=0; coord1[6][1]=4;
    coord1[7][0]=9; coord1[7][1]=0;
    coord1[8][0]=7; coord1[8][1]=1;
    coord1[9][0]=3; coord1[9][1]=3;
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hsize_t **)coord1);

    /* Store second dataset region */
    ret = H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Write selection to disk */
    ret=H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close all objects */
    ret = H5Sclose(sid1);
    ret = H5Dclose(dset1);
    ret = H5Sclose(sid2);
    
    /* Close file */
    ret = H5Fclose(fid1);

    free(wbuf);
    free(dwbuf);
    return 0;
}   


