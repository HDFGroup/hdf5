/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/***********************************************************
*
* Test program:	 titerate
*
* Test the Group & Attribute functionality
*
*************************************************************/

#include "testhdf5.h"

#include "hdf5.h"

#define DATAFILE   "titerate.h5"

/* Number of datasets for group iteration test */
#define NDATASETS 50

/* Number of attributes for attribute iteration test */
#define NATTR 50

/* Number of groups for second group iteration test */
#define ITER_NGROUPS 150

/* General maximum length of names used */
#define NAMELEN     80

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

typedef enum {
    RET_ZERO,
    RET_TWO,
    RET_CHANGE
} iter_enum;

/* Custom group iteration callback data */
typedef struct {
    char name[NAMELEN];     /* The name of the object */
    int type;               /* The type of the object */
    iter_enum command;      /* The type of return value */
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
    return(strcmp(*(const char * const *)s1,*(const char * const *)s2));
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

        case RET_TWO:
            return(2);

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
    hid_t root_group,grp;   /* Root group ID */
    int i;                  /* counting variable */
    int idx;                /* Index in the group */
    char name[NAMELEN];     /* temporary name buffer */
    char *dnames[NDATASETS];/* Names of the datasets created */
    char dataset_name[NAMELEN];  /* dataset name */
    iter_info info;         /* Custom iteration information */
    hsize_t num_membs;      /* Number of group members */
    herr_t ret;		    /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Group Iteration Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Test iterating over empty group */
    info.command=RET_ZERO;
    idx=0;
    ret=H5Giterate(file,"/",&idx,giter_cb,&info);
    VERIFY(ret, SUCCEED, "H5Giterate");

    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, FAIL, "H5Tcopy");

    filespace=H5Screate(H5S_SCALAR);
    CHECK(filespace, FAIL, "H5Screate");

    for(i=0; i< NDATASETS; i++) {
        sprintf(name,"Dataset %d",i);
        dataset = H5Dcreate(file, name, datatype, filespace, H5P_DEFAULT);
        CHECK(dataset, FAIL, "H5Dcreate");

        /* Keep a copy of the dataset names around for later */
        dnames[i]=HDstrdup(name);
        CHECK(dnames[i], NULL, "strdup");

        ret=H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
    }

    /* Create a group and named datatype under root group for testing
     * H5Gget_objtype_by_idx.
     */
    grp = H5Gcreate(file, "grp", 0);
    CHECK(ret, FAIL, "H5Gcreate");

    ret = H5Tcommit(file, "dtype", datatype);
    CHECK(ret, FAIL, "H5Tcommit");

    /* Close everything up */
    ret=H5Tclose(datatype);
    CHECK(ret, FAIL, "H5Tclose");

    ret=H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    ret=H5Sclose(filespace);
    CHECK(ret, FAIL, "H5Sclose");

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Sort the dataset names */
    HDqsort(dnames,NDATASETS,sizeof(char *),iter_strcmp);


    /* Iterate through the datasets in the root group in various ways */
    file=H5Fopen(DATAFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    /* These two functions, H5Gget_num_objs and H5Gget_objname_by_idx, actually
     * iterate through B-tree for group members in internal library design.
     */
    {
        root_group = H5Gopen(file, "/");
        CHECK(root_group, FAIL, "H5Gopen");

        ret = H5Gget_num_objs(root_group, &num_membs);
        CHECK(ret, FAIL, "H5Gget_num_objs");
        VERIFY(num_membs,NDATASETS+2,"H5Gget_num_objs");

        for(i=0; i< (int)num_membs; i++) {
#ifdef H5_WANT_H5_V1_4_COMPAT
            int obj_type;         /* Type of object in file */
#else /*H5_WANT_H5_V1_4_COMPAT*/
            H5G_obj_t obj_type;         /* Type of object in file */
#endif /*H5_WANT_H5_V1_4_COMPAT*/

            ret = (herr_t)H5Gget_objname_by_idx(root_group, (hsize_t)i, dataset_name, NAMELEN);
            CHECK(ret, FAIL, "H5Gget_objname_by_idx");

            obj_type = H5Gget_objtype_by_idx(root_group, (hsize_t)i);
            CHECK(obj_type, H5G_UNKNOWN, "H5Gget_objtype_by_idx");
        }

        H5E_BEGIN_TRY {
            ret = (herr_t)H5Gget_objname_by_idx(root_group, (hsize_t)(NDATASETS+3), dataset_name, NAMELEN);
        } H5E_END_TRY;
        VERIFY(ret, FAIL, "H5Gget_objname_by_idx");

        ret = H5Gclose(root_group);
        CHECK(ret, FAIL, "H5Gclose");
    }

    /* These two functions, H5Gget_num_objs and H5Gget_objname_by_idx, actually
     * iterate through B-tree for group members in internal library design.
     *  (Same as test above, but with the file ID instead of opening the root group)
     */
    {
        ret = H5Gget_num_objs(file, &num_membs);
        CHECK(ret, FAIL, "H5Gget_num_objs");
        VERIFY(num_membs,NDATASETS+2,"H5Gget_num_objs");

        for(i=0; i< (int)num_membs; i++) {
#ifdef H5_WANT_H5_V1_4_COMPAT
            int obj_type;         /* Type of object in file */
#else /*H5_WANT_H5_V1_4_COMPAT*/
            H5G_obj_t obj_type;         /* Type of object in file */
#endif /*H5_WANT_H5_V1_4_COMPAT*/

            ret = (herr_t)H5Gget_objname_by_idx(file, (hsize_t)i, dataset_name, NAMELEN);
            CHECK(ret, FAIL, "H5Gget_objname_by_idx");

            obj_type = H5Gget_objtype_by_idx(file, (hsize_t)i);
            CHECK(obj_type, H5G_UNKNOWN, "H5Gget_objtype_by_idx");
        }

        H5E_BEGIN_TRY {
            ret = (herr_t)H5Gget_objname_by_idx(file, (hsize_t)(NDATASETS+3), dataset_name, NAMELEN);
        } H5E_END_TRY;
        VERIFY(ret, FAIL, "H5Gget_objname_by_idx");
    }

    /* Test invalid indices for starting iteration */
    info.command=RET_ZERO;
    idx=-1;
    H5E_BEGIN_TRY {
        ret=H5Giterate(file,"/",&idx,giter_cb,&info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Giterate");

    /* Test skipping exactly as many entries as in the group */
    idx=NDATASETS+2;
    H5E_BEGIN_TRY {
        ret=H5Giterate(file,"/",&idx,giter_cb,&info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Giterate");

    /* Test skipping more entries than are in the group */
    idx=NDATASETS+3;
    H5E_BEGIN_TRY {
        ret=H5Giterate(file,"/",&idx,giter_cb,&info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Giterate");

    /* Test all objects in group, when callback always returns 0 */
    info.command=RET_ZERO;
    idx=0;
    if((ret=H5Giterate(file,"/",&idx,giter_cb,&info))>0)
        TestErrPrintf("Group iteration function didn't return zero correctly!\n");

    /* Test all objects in group, when callback always returns 1 */
    /* This also tests the "restarting" ability, because the index changes */
    info.command=RET_TWO;
    idx=i=0;
    while((ret=H5Giterate(file,"/",&idx,giter_cb,&info))>0) {
        /* Verify return value from iterator gets propagated correctly */
        VERIFY(ret,2,"H5Giterate");

        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx,i,"H5Giterate");

        /* Verify that the correct name is retrieved */
        if(idx<=NDATASETS) {
            if(HDstrcmp(info.name,dnames[idx-1])!=0)
                TestErrPrintf("Group iteration function didn't return one correctly for dataset #%d!\n",idx);
        } /* end if */
        else if(idx==(NDATASETS+1)) {
            if(HDstrcmp(info.name,"dtype")!=0)
                TestErrPrintf("Group iteration function didn't return one correctly for group!\n");
        } /* end if */
        else if(idx==(NDATASETS+2)) {
            if(HDstrcmp(info.name,"grp")!=0)
                TestErrPrintf("Group iteration function didn't return one correctly for group!\n");
        } /* end if */
        else
            TestErrPrintf("Group iteration function walked too far!\n");
    }
    VERIFY(ret,-1,"H5Giterate");

    if(i!=(NDATASETS+2))
        TestErrPrintf("Group iteration function didn't perform multiple iterations correctly!\n");

    /* Test all objects in group, when callback changes return value */
    /* This also tests the "restarting" ability, because the index changes */
    info.command=RET_CHANGE;
    idx=i=0;
    while((ret=H5Giterate(file,"/",&idx,giter_cb,&info))>=0) {
        /* Verify return value from iterator gets propagated correctly */
        VERIFY(ret,1,"H5Giterate");

        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx,i+10,"H5Giterate");

        /* Verify that the correct name is retrieved */
        if(idx<=NDATASETS) {
            if(HDstrcmp(info.name,dnames[idx-1])!=0)
                TestErrPrintf("Group iteration function didn't return one correctly for dataset #%d!\n",idx);
        } /* end if */
        else if(idx==(NDATASETS+1)) {
            if(HDstrcmp(info.name,"dtype")!=0)
                TestErrPrintf("Group iteration function didn't return one correctly for group!\n");
        } /* end if */
        else if(idx==(NDATASETS+2)) {
            if(HDstrcmp(info.name,"grp")!=0)
                TestErrPrintf("Group iteration function didn't return one correctly for group!\n");
        } /* end if */
        else
            TestErrPrintf("Group iteration function walked too far!\n");
    }
    VERIFY(ret,-1,"H5Giterate");

    if(i!=42 || idx!=52)
        TestErrPrintf("Group iteration function didn't perform multiple iterations correctly!\n");

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free the dataset names */
    for(i=0; i< NDATASETS; i++)
        HDfree(dnames[i]);

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

        case RET_TWO:
            return(2);

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
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, FAIL, "H5Tcopy");

    filespace=H5Screate(H5S_SCALAR);
    CHECK(filespace, FAIL, "H5Screate");

    dataset = H5Dcreate(file, "Dataset", datatype, filespace, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    for(i=0; i< NATTR; i++) {
        sprintf(name,"Attribute %d",i);
        attribute = H5Acreate(dataset, name, datatype, filespace, H5P_DEFAULT);
        CHECK(attribute, FAIL, "H5Acreate");

        /* Keep a copy of the attribute names around for later */
        anames[i]=HDstrdup(name);
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
    file=H5Fopen(DATAFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    dataset=H5Dopen(file, "Dataset");
    CHECK(dataset, FAIL, "H5Dopen");

    /* Test invalid indices for starting iteration */
    info.command=RET_ZERO;

    /* Test skipping exactly as many attributes as there are */
    idx=NATTR;
    H5E_BEGIN_TRY {
        ret=H5Aiterate(dataset,&idx,aiter_cb,&info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Aiterate");

    /* Test skipping more attributes than there are */
    idx=NATTR+1;
    H5E_BEGIN_TRY {
        ret=H5Aiterate(dataset,&idx,aiter_cb,&info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Aiterate");

    /* Test all attributes on dataset, when callback always returns 0 */
    info.command=RET_ZERO;
    idx=0;
    if((ret=H5Aiterate(dataset,&idx,aiter_cb,&info))>0)
        TestErrPrintf("Attribute iteration function didn't return zero correctly!\n");

    /* Test all attributes on dataset, when callback always returns 1 */
    /* This also tests the "restarting" ability, because the index changes */
    info.command=RET_TWO;
    idx=i=0;
    while((ret=H5Aiterate(dataset,&idx,aiter_cb,&info))>0) {
        /* Verify return value from iterator gets propagated correctly */
        VERIFY(ret,2,"H5Aiterate");

        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx,(unsigned)i,"H5Aiterate");

        /* Verify that the correct name is retrieved */
        if(HDstrcmp(info.name,anames[idx-1])!=0)
            TestErrPrintf("Attribute iteration function didn't return one correctly!\n");
    }
    VERIFY(ret,-1,"H5Aiterate");
    if(i!=50 || idx!=50)
        TestErrPrintf("Group iteration function didn't perform multiple iterations correctly!\n");


    /* Test all attributes on dataset, when callback changes return value */
    /* This also tests the "restarting" ability, because the index changes */
    info.command=RET_CHANGE;
    idx=i=0;
    while((ret=H5Aiterate(dataset,&idx,aiter_cb,&info))>0) {
        /* Verify return value from iterator gets propagated correctly */
        VERIFY(ret,1,"H5Aiterate");

        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx,(unsigned)i+10,"H5Aiterate");

        /* Verify that the correct name is retrieved */
        if(HDstrcmp(info.name,anames[idx-1])!=0)
            TestErrPrintf("Attribute iteration function didn't return changing correctly!\n");
    }
    VERIFY(ret,-1,"H5Aiterate");
    if(i!=40 || idx!=50)
        TestErrPrintf("Group iteration function didn't perform multiple iterations correctly!\n");

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    ret=H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Free the attribute names */
    for(i=0; i< NATTR; i++)
        HDfree(anames[i]);

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

    if(HDstrcmp(name,test_info->name)) {
        TestErrPrintf("name=%s, test_info=%s\n",name,test_info->name);
        return(-1);
    } /* end if */

    /*
     * Get type of the object and check it.
     */
    ret=H5Gget_objinfo(loc_id, name, FALSE, &statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");

    if(test_info->type!=statbuf.type) {
        TestErrPrintf("test_info->type=%d, statbuf.type=%d\n",test_info->type,statbuf.type);
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
    iter_info names[ITER_NGROUPS+2]; /* Names of objects in the root group */
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
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create a bunch of groups */
    for (i=0; i<ITER_NGROUPS; i++) {
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
    strcpy(names[ITER_NGROUPS].name,"Dataset1");
    names[ITER_NGROUPS].type=H5G_DATASET;

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
    strcpy(names[ITER_NGROUPS+1].name,"Datatype1");
    names[ITER_NGROUPS+1].type=H5G_TYPE;

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Need to sort the names in the root group, cause that's what the library does */
    qsort(names,ITER_NGROUPS+2,sizeof(iter_info),iter_strcmp2);

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
**  test_grp_memb_funcs(): Test group member information
**                         functionality
**
****************************************************************/
static void test_grp_memb_funcs(void)
{
    hid_t file;             /* File ID */
    hid_t dataset;          /* Dataset ID */
    hid_t datatype;         /* Common datatype ID */
    hid_t filespace;        /* Common dataspace ID */
    hid_t root_group,grp;   /* Root group ID */
    int i;                  /* counting variable */
    char name[NAMELEN];     /* temporary name buffer */
    char *dnames[NDATASETS+2];/* Names of the datasets created */
    char *obj_names[NDATASETS+2];/* Names of the objects in group */
    char dataset_name[NAMELEN];  /* dataset name */
    ssize_t name_len;       /* Length of object's name */
    hsize_t num_membs;      /* Number of group members */
    herr_t ret;		    /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Group Member Information Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
        dnames[i]=HDstrdup(name);
        CHECK(dnames[i], NULL, "strdup");

        ret=H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
    }

    /* Create a group and named datatype under root group for testing
     * H5Gget_objtype_by_idx.
     */
    grp = H5Gcreate(file, "grp", 0);
    CHECK(ret, FAIL, "H5Gcreate");

    dnames[NDATASETS]=HDstrdup("grp");
    CHECK(dnames[NDATASETS], NULL, "strdup");

    ret = H5Tcommit(file, "dtype", datatype);
    CHECK(ret, FAIL, "H5Tcommit");

    dnames[NDATASETS+1]=HDstrdup("dtype");
    CHECK(dnames[NDATASETS], NULL, "strdup");

    /* Close everything up */
    ret=H5Tclose(datatype);
    CHECK(ret, FAIL, "H5Tclose");

    ret=H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    ret=H5Sclose(filespace);
    CHECK(ret, FAIL, "H5Sclose");

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Sort the dataset names */
    HDqsort(dnames,NDATASETS+2,sizeof(char *),iter_strcmp);

    /* Iterate through the datasets in the root group in various ways */
    file=H5Fopen(DATAFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    /* These two functions, H5Gget_num_objs and H5Gget_objname_by_idx, actually
     * iterate through B-tree for group members in internal library design.
     */
    root_group = H5Gopen(file, "/");
    CHECK(root_group, FAIL, "H5Gopen");

    ret = H5Gget_num_objs(root_group, &num_membs);
    CHECK(ret, FAIL, "H5Gget_num_objs");
    VERIFY(num_membs,NDATASETS+2,"H5Gget_num_objs");

    for(i=0; i< (int)num_membs; i++) {
#ifdef H5_WANT_H5_V1_4_COMPAT
        int obj_type;         /* Type of object in file */
#else /*H5_WANT_H5_V1_4_COMPAT*/
        H5G_obj_t obj_type;         /* Type of object in file */
#endif /*H5_WANT_H5_V1_4_COMPAT*/

        /* Test with NULL for name, to query length */
        name_len = H5Gget_objname_by_idx(root_group, (hsize_t)i, NULL, NAMELEN);
        CHECK(name_len, FAIL, "H5Gget_objname_by_idx");

        ret = (herr_t)H5Gget_objname_by_idx(root_group, (hsize_t)i, dataset_name, (size_t)(name_len+1));
        CHECK(ret, FAIL, "H5Gget_objname_by_idx");

        /* Double-check that the length is the same */
        VERIFY(ret, name_len, "H5Gget_objname_by_idx");

        /* Keep a copy of the dataset names around for later */
        obj_names[i]=HDstrdup(dataset_name);
        CHECK(obj_names[i], NULL, "strdup");

        obj_type = H5Gget_objtype_by_idx(root_group, (hsize_t)i);
        CHECK(obj_type, H5G_UNKNOWN, "H5Gget_objtype_by_idx");

        if(!HDstrcmp(dataset_name, "grp"))
            VERIFY(obj_type, H5G_GROUP, "H5Gget_objname_by_idx");
        if(!HDstrcmp(dataset_name, "dtype"))
            VERIFY(obj_type, H5G_TYPE, "H5Gget_objname_by_idx");
        if(!HDstrncmp(dataset_name, "Dataset", 7))
            VERIFY(obj_type, H5G_DATASET, "H5Gget_objname_by_idx");
    }

    H5E_BEGIN_TRY {
        ret = (herr_t)H5Gget_objname_by_idx(root_group, (hsize_t)(NDATASETS+3), dataset_name, NAMELEN);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Gget_objname_by_idx");

    /* Sort the dataset names */
    qsort(obj_names,NDATASETS+2,sizeof(char *),iter_strcmp);

    /* Compare object names */
    for(i=0; i< (int)num_membs; i++) {
        ret = HDstrcmp(dnames[i], obj_names[i]);
        VERIFY(ret, 0, "HDstrcmp");
    }

    ret = H5Gclose(root_group);
    CHECK(ret, FAIL, "H5Gclose");


    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free the dataset names */
    for(i=0; i< NDATASETS+2; i++) {
        free(dnames[i]);
        free(obj_names[i]);
    }

} /* test_grp_memb_funcs() */

/****************************************************************
**
**  test_links(): Test soft and hard link iteration
**
****************************************************************/
static void test_links(void)
{
    hid_t file;             /* File ID */
    char obj_name[NAMELEN]; /* Names of the object in group */
    ssize_t name_len;       /* Length of object's name */
    herr_t ret;		    /* Generic return value */
    hid_t    gid, gid1;
    hsize_t i;
#ifdef H5_WANT_H5_V1_4_COMPAT
    int       obj_type;     /* Type of object */
#else /*H5_WANT_H5_V1_4_COMPAT*/
    H5G_obj_t obj_type;     /* Type of object */
#endif /*H5_WANT_H5_V1_4_COMPAT*/
    hsize_t   nobjs;        /* Number of objects */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Soft and Hard Link Iteration Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* create groups */
    gid = H5Gcreate (file, "/g1", 0);
    CHECK(gid, FAIL, "H5Gcreate");

    gid1 = H5Gcreate (file, "/g1/g1.1", 0);
    CHECK(gid1, FAIL, "H5Gcreate");

    /* create soft and hard links to the group "/g1". */
    ret = H5Glink (gid, H5G_LINK_SOFT, "something", "softlink");
    CHECK(ret, FAIL, "H5Glink");

    ret = H5Glink (gid, H5G_LINK_HARD, "/g1", "hardlink");
    CHECK(ret, FAIL, "H5Glink");

    ret = H5Gget_num_objs(gid, &nobjs);
    CHECK(ret, FAIL, "H5Gget_num_objs");
    VERIFY(nobjs,3,"H5Gget_num_objs");

    /* Test these two functions, H5Gget_num_objs and H5Gget_objname_by_idx */
    for(i=0; i<nobjs; i++) {
        /* Get object name */
        name_len = H5Gget_objname_by_idx(gid, i, obj_name, NAMELEN);
        CHECK(name_len, FAIL, "H5Gget_objname_by_idx");

        obj_type = H5Gget_objtype_by_idx(gid, i);
        CHECK(obj_type, H5G_UNKNOWN, "H5Gget_objtype_by_idx");

        if(!HDstrcmp(obj_name, "g1.1"))
            VERIFY(obj_type, H5G_GROUP, "H5Gget_objname_by_idx");
        else if(!HDstrcmp(obj_name, "hardlink"))
            VERIFY(obj_type, H5G_GROUP, "H5Gget_objname_by_idx");
        else if(!HDstrcmp(obj_name, "softlink"))
            VERIFY(obj_type, H5G_LINK, "H5Gget_objname_by_idx");
        else
            CHECK(0, 0, "unknown object name");
    }

    ret=H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret=H5Gclose(gid1);
    CHECK(ret, FAIL, "H5Gclose");

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_links() */

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
    test_grp_memb_funcs();   /* Test group member information functions */
    test_links();            /* Test soft and hard link iteration */
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
    remove(DATAFILE);
}

