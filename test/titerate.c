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
* Test program:	 titerate
*
* Test the Group & Attribute functionality
*
*************************************************************/

#include <testhdf5.h>

#include <hdf5.h>

#define FILE   "titerate.h5"

/* Number of datasets for group iteration test */
#define NDATASETS 50

/* Number of attributes for group iteration test */
#define NATTR 50

/* Number of groups for second group iteration test */
#define NGROUPS 150

/* General maximum length of names used */
#define NAMELEN     80

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

/* Custom group iteration callback data */
typedef struct {
    char name[NAMELEN];     /* The name of the object */
    int type;               /* The type of the object */
    enum {RET_ZERO, RET_ONE, RET_CHANGE} command;   /* The type of return value */
} iter_info;

/* Local functions */
int iter_strcmp(const void *s1, const void *s2);
int iter_strcmp2(const void *s1, const void *s2);
herr_t giter_cb(hid_t group, const char *name, void *op_data);
herr_t giter_cb2(hid_t group, const char *name, void *op_data);
herr_t aiter_cb(hid_t loc_id, const char *name, void *op_data);

/****************************************************************
**
**  iter_strcmp(): String comparison routine for qsort
** 
****************************************************************/
int iter_strcmp(const void *s1, const void *s2)
{
    return(strcmp(*(const char **)s1,*(const char **)s2));
}

/****************************************************************
**
**  giter_cb(): Custom group iteration callback routine.
** 
****************************************************************/
herr_t giter_cb(hid_t UNUSED group, const char *name, void *op_data)
{
    iter_info *info=(iter_info *)op_data;
    static int count=0;

    strcpy(info->name,name);

    switch(info->command) {
        case RET_ZERO:
            return(0);

        case RET_ONE:
            return(1);

        case RET_CHANGE:
            count++;
            return(count>10 ? 1: 0);

        default:
            printf("invalid iteration command");
            return(-1);
    } /* end switch */
}

/****************************************************************
**
**  test_iter_group(): Test group iteration functionality
** 
****************************************************************/
static void test_iter_group(void)
{
    hid_t file;             /* File ID */
    hid_t dataset;          /* Dataset ID */
    hid_t datatype;         /* Common datatype ID */
    hid_t filespace;        /* Common dataspace ID */
    int i;                  /* counting variable */
    int idx;                /* Index in the group */
    char name[NAMELEN];     /* temporary name buffer */
    char *dnames[NDATASETS];/* Names of the datasets created */
    iter_info info;         /* Custom iteration information */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Group Iteration Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, FAIL, "H5Tcopy");

    filespace=H5Screate(H5S_SCALAR);
    CHECK(filespace, FAIL, "H5Screate");

    for(i=0; i< NDATASETS; i++) {
        sprintf(name,"Dataset %d",i);
        dataset = H5Dcreate(file, name, datatype, filespace, H5P_DEFAULT);
        CHECK(dataset, FAIL, "H5Dcreate");

        /* Keep a copy of the dataset names around for later */
        dnames[i]=strdup(name);
        CHECK(dnames[i], NULL, "strdup");

        ret=H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
    }

    /* Close everything up */
    ret=H5Tclose(datatype);
    CHECK(ret, FAIL, "H5Tclose");

    ret=H5Sclose(filespace);
    CHECK(ret, FAIL, "H5Sclose");

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Sort the dataset names */
    qsort(dnames,NDATASETS,sizeof(char *),iter_strcmp);

    /* Iterate through the datasets in the root group in various ways */
    file=H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    /* Test all objects in group, when callback always returns 0 */
    info.command=RET_ZERO;
    idx=0;
    while(H5Giterate(file,"/",&idx,giter_cb,&info)>0) {
        printf("Group iteration function didn't return zero correctly!\n");
        num_errs++;
    }

    /* Test all objects in group, when callback always returns 1 */
    /* This also tests the "restarting" ability, because the index changes */
    info.command=RET_ONE;
    idx=i=0;
    while(H5Giterate(file,"/",&idx,giter_cb,&info)>0) {
        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx,i,"H5Giterate");

        /* Verify that the correct name is retrieved */
        if(strcmp(info.name,dnames[idx-1])!=0) {
            printf("Group iteration function didn't return one correctly!\n");
            num_errs++;
        } /* end if */
    }

    /* Test all objects in group, when callback changes return value */
    /* This also tests the "restarting" ability, because the index changes */
    info.command=RET_CHANGE;
    idx=i=0;
    while(H5Giterate(file,"/",&idx,giter_cb,&info)>0) {
        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx,i+10,"H5Giterate");

        /* Verify that the correct name is retrieved */
        if(strcmp(info.name,dnames[idx-1])!=0) {
            printf("Group iteration function didn't return changing correctly!\n");
            num_errs++;
        } /* end if */
    }

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free the dataset names */
    for(i=0; i< NDATASETS; i++)
        free(dnames[i]);

} /* test_iter_group() */

/****************************************************************
**
**  aiter_cb(): Custom group iteration callback routine.
** 
****************************************************************/
herr_t aiter_cb(hid_t UNUSED group, const char *name, void *op_data)
{
    iter_info *info=(iter_info *)op_data;
    static int count=0;

    strcpy(info->name,name);

    switch(info->command) {
        case RET_ZERO:
            return(0);

        case RET_ONE:
            return(1);

        case RET_CHANGE:
            count++;
            return(count>10 ? 1: 0);

        default:
            printf("invalid iteration command");
            return(-1);
    } /* end switch */
}

/****************************************************************
**
**  test_iter_attr(): Test attribute iteration functionality
** 
****************************************************************/
static void test_iter_attr(void)
{
    hid_t file;             /* File ID */
    hid_t dataset;          /* Common Dataset ID */
    hid_t datatype;         /* Common datatype ID */
    hid_t filespace;        /* Common dataspace ID */
    hid_t attribute;        /* Attribute ID */
    int i;                  /* counting variable */
    unsigned idx;           /* Index in the attribute list */
    char name[NAMELEN];     /* temporary name buffer */
    char *anames[NATTR];    /* Names of the attributes created */
    iter_info info;         /* Custom iteration information */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attribute Iteration Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, FAIL, "H5Tcopy");

    filespace=H5Screate(H5S_SCALAR);
    CHECK(filespace, FAIL, "H5Screate");

    dataset = H5Dcreate(file, "Dataset", datatype, filespace, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");
    
    for(i=0; i< NDATASETS; i++) {
        sprintf(name,"Attribute %d",i);
        attribute = H5Acreate(dataset, name, datatype, filespace, H5P_DEFAULT);
        CHECK(attribute, FAIL, "H5Acreate");

        /* Keep a copy of the attribute names around for later */
        anames[i]=strdup(name);
        CHECK(anames[i], NULL, "strdup");

        ret=H5Aclose(attribute);
        CHECK(ret, FAIL, "H5Aclose");
    }

    /* Close everything up */
    ret=H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    ret=H5Tclose(datatype);
    CHECK(ret, FAIL, "H5Tclose");

    ret=H5Sclose(filespace);
    CHECK(ret, FAIL, "H5Sclose");

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Iterate through the attributes on the dataset in various ways */
    file=H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    dataset=H5Dopen(file, "Dataset");
    CHECK(dataset, FAIL, "H5Dopen");

    /* Test all attributes on dataset, when callback always returns 0 */
    info.command=RET_ZERO;
    idx=0;
    while(H5Aiterate(dataset,&idx,aiter_cb,&info)>0) {
        printf("Attribute iteration function didn't return zero correctly!\n");
        num_errs++;
    }

    /* Test all attributes on dataset, when callback always returns 1 */
    /* This also tests the "restarting" ability, because the index changes */
    info.command=RET_ONE;
    idx=i=0;
    while(H5Aiterate(dataset,&idx,aiter_cb,&info)>0) {
        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx,(unsigned)i,"H5Aiterate");

        /* Verify that the correct name is retrieved */
        if(strcmp(info.name,anames[idx-1])!=0) {
            printf("Attribute iteration function didn't return one correctly!\n");
            num_errs++;
        } /* end if */
    }

    /* Test all attributes on dataset, when callback changes return value */
    /* This also tests the "restarting" ability, because the index changes */
    info.command=RET_CHANGE;
    idx=i=0;
    while(H5Aiterate(dataset,&idx,aiter_cb,&info)>0) {
        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx,(unsigned)i+10,"H5Aiterate");

        /* Verify that the correct name is retrieved */
        if(strcmp(info.name,anames[idx-1])!=0) {
            printf("Attribute iteration function didn't return changing correctly!\n");
            num_errs++;
        } /* end if */
    }

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    ret=H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Free the attribute names */
    for(i=0; i< NATTR; i++)
        free(anames[i]);

} /* test_iter_attr() */

/****************************************************************
**
**  iter_strcmp2(): String comparison routine for qsort
** 
****************************************************************/
int iter_strcmp2(const void *s1, const void *s2)
{
    return(strcmp((const char *)s1,(const char *)s2));
}

/****************************************************************
**
**  giter_cb2(): Custom group iteration callback routine.
** 
****************************************************************/
herr_t giter_cb2(hid_t loc_id, const char *name, void *opdata)
{
    const iter_info *test_info=(const iter_info *)opdata;
    herr_t		ret;		/* Generic return value		*/
    H5G_stat_t statbuf;

    if(strcmp(name,test_info->name)) {
        num_errs++;
        printf("name=%s, test_info=%s\n",name,test_info->name);
        return(-1);
    } /* end if */

    /*
     * Get type of the object and check it.
     */
    ret=H5Gget_objinfo(loc_id, name, FALSE, &statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");

    if(test_info->type!=statbuf.type) {
        num_errs++;
        printf("test_info->type=%d, statbuf.type=%d\n",test_info->type,statbuf.type);
        return(-1);
    } /* end if */

    return(1);
} /* giter_cb2() */

/****************************************************************
**
**  test_iter_group_large(): Test group iteration functionality
**          for groups with large #'s of objects
** 
****************************************************************/
static void test_iter_group_large(void)
{
    hid_t		file;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;      /* Group ID             */
    hid_t		sid;       /* Dataspace ID			*/
    hid_t		tid;       /* Datatype ID			*/
    hsize_t		dims[] = {SPACE1_DIM1};
    herr_t		ret;		/* Generic return value		*/
    char gname[20];         /* Temporary group name */
    iter_info names[NGROUPS+2]; /* Names of objects in the root group */
    iter_info *curr_name;        /* Pointer to the current name in the root group */
    int                 i;

    /* Compound datatype */
    typedef struct s1_t {
        unsigned int a;
        unsigned int b;
        float c;
    } s1_t;

    memset(names, 0, sizeof names);

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Large Group Iteration Functionality\n"));

    /* Create file */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create a bunch of groups */
    for (i=0; i<NGROUPS; i++) {
        sprintf(gname, "Group_%d", i); 

        /* Add the name to the list of objects in the root group */
        strcpy(names[i].name,gname);
        names[i].type=H5G_GROUP;

        /* Create a group */
        group=H5Gcreate(file,gname,0);
        CHECK(group, FAIL, "H5Gcreate");

        /* Close a group */
        ret = H5Gclose(group);
        CHECK(ret, FAIL, "H5Gclose");
    }

    /* Create a dataset  */
    dataset=H5Dcreate(file,"Dataset1",H5T_STD_U32LE,sid,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Add the name to the list of objects in the root group */
    strcpy(names[NGROUPS].name,"Dataset1");
    names[NGROUPS].type=H5G_DATASET;

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create a datatype */
    tid = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid, FAIL, "H5Tcreate");

    /* Insert fields */
    ret=H5Tinsert (tid, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret=H5Tinsert (tid, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret=H5Tinsert (tid, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret=H5Tcommit (file, "Datatype1", tid);
    CHECK(ret, FAIL, "H5Tcommit");

    /* Add the name to the list of objects in the root group */
    strcpy(names[NGROUPS+1].name,"Datatype1");
    names[NGROUPS+1].type=H5G_TYPE;

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Need to sort the names in the root group, cause that's what the library does */
    qsort(names,NGROUPS+2,sizeof(iter_info),iter_strcmp2);

    /* Iterate through the file to see members of the root group */
    curr_name=&names[0];
    H5Giterate(file, "/", NULL, giter_cb2, curr_name);
    for (i=1; i<100; ) {
        curr_name=&names[i];
        H5Giterate(file, "/", &i, giter_cb2, curr_name);
    } /* end for */

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_iterate_group_large() */

/****************************************************************
**
**  test_iterate(): Main iteration testing routine.
** 
****************************************************************/
void 
test_iterate(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Iteration Operations\n"));

    /* These next tests use the same file */
    test_iter_group();       /* Test group iteration */
    test_iter_group_large(); /* Test group iteration for large # of objects */
    test_iter_attr();        /* Test attribute iteration */
}   /* test_iterate() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_iterate
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              April 5, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_iterate(void)
{
    remove(FILE);
}

