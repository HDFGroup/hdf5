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

/* General maximum length of names used */
#define NAMELEN     80

/* Custom group iteration callback data */
typedef struct {
    enum {RET_ZERO, RET_ONE, RET_CHANGE} command;
    char name[NAMELEN];
} iter_info;

/* Local functions */
int iter_strcmp(const void *s1, const void *s2);
herr_t giter_cb(hid_t group, const char *name, void *op_data);
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

