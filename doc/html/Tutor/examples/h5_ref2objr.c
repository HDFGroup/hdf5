#include <stdlib.h>
#include <hdf5.h>

#define FILE1   "trefer1.h5"

/* dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

int 
main(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset,	/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hobj_ref_t      *rbuf;      /* buffer to read from disk */
    int                *tu32;      /* temp. buffer read from disk */
    int        i;          /* counting variables */
    char read_comment[10];
    herr_t		ret;		/* Generic return value		*/

    /* Allocate read buffers */
    rbuf = malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    tu32 = malloc(sizeof(int)*SPACE1_DIM1);

    /* Open the file */
    fid1 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);

    /* Open the dataset */
    dataset=H5Dopen(fid1,"/Dataset3");

    /* Read selection from disk */
    ret=H5Dread(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf);

    /* Open dataset object */
    dset2 = H5Rdereference(dataset,H5R_OBJECT,&rbuf[0]);

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);

    ret=H5Sget_simple_extent_npoints(sid1);

    /* Read from disk */
    ret=H5Dread(dset2,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,tu32);
    printf("Dataset data : \n");
     for (i=0; i < SPACE1_DIM1 ; i++) printf (" %d ", tu32[i]);
    printf("\n");
    printf("\n");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);

    /* Open group object */
    group = H5Rdereference(dataset,H5R_OBJECT,&rbuf[2]);

    /* Get group's comment */
    ret=H5Gget_comment(group,".",10,read_comment);
    printf("Group comment is %s \n", read_comment);
    printf(" \n");
    /* Close group */
    ret = H5Gclose(group);

    /* Open datatype object */
    tid1 = H5Rdereference(dataset,H5R_OBJECT,&rbuf[3]);

    /* Verify correct datatype */
    {
        H5T_class_t tclass;

        tclass= H5Tget_class(tid1);
        if ((tclass == H5T_COMPOUND))
           printf ("Number of compound datatype members is %d \n", H5Tget_nmembers(tid1)); 
    printf(" \n");
    }

    /* Close datatype */
    ret = H5Tclose(tid1);

    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Close file */
    ret = H5Fclose(fid1);

    /* Free memory buffers */
    free(rbuf);
    free(tu32);
    return 0;
}   
