#include "hdf5.h"

#define FILE    "iterate.h5"
#define FALSE   0

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

herr_t file_info(hid_t loc_id, const char *name, void *opdata);
                                     /* Operator function */
int 
main(void) {
    hid_t		file;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;      /* Group ID             */
    hid_t		sid;       /* Dataspace ID			*/
    hid_t		tid;       /* Datatype ID			*/
    hsize_t		dims[] = {SPACE1_DIM1};
    herr_t		ret;		/* Generic return value		*/

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

    /* Create file */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, dims, NULL);

    /* Create a group */
    group=H5Gcreate(file,"Group1",-1);

    /* Close a group */
    ret = H5Gclose(group);

    /* Create a dataset  */
    dataset=H5Dcreate(file,"Dataset1",H5T_STD_U32LE,sid,H5P_DEFAULT);

    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Create a datatype */
    tid = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));

    /* Insert fields */
    ret=H5Tinsert (tid, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);

    ret=H5Tinsert (tid, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);

    ret=H5Tinsert (tid, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT);

    /* Save datatype for later */
    ret=H5Tcommit (file, "Datatype1", tid);

    /* Close datatype */
    ret = H5Tclose(tid);

    /* Iterate through the file to see members of the root group */

    printf(" Objects in the root group are:\n");
    printf("\n");

    H5Giterate(file, "/", NULL, file_info, NULL);

    /* Close file */
    ret = H5Fclose(file);

    return 0;
}

/*
 * Operator function.
 */
herr_t file_info(hid_t loc_id, const char *name, void *opdata)
{
    H5G_stat_t statbuf;

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by 
     * the Library. Some magic :-)
     */
    H5Gget_objinfo(loc_id, name, FALSE, &statbuf);
    switch (statbuf.type) {
    case H5G_GROUP: 
         printf(" Object with name %s is a group \n", name);
         break;
    case H5G_DATASET: 
         printf(" Object with name %s is a dataset \n", name);
         break;
    case H5G_TYPE: 
         printf(" Object with name %s is a named datatype \n", name);
         break;
    default:
         printf(" Unable to identify an object ");
    }
    return 0;
 }







