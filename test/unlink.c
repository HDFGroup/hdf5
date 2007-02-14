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

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, September 25, 1998
 *
 * Purpose:	Test H5Gunlink().
 */

#include <time.h>
#include "h5test.h"

const char *FILENAME[] = {
    "unlink",
    "new_move_a",
    "new_move_b",
    "lunlink",
    "filespace",
    "slashes",
    "resurrect_set",
    "resurrect_type",
    "resurrect_group",
    "unlink_chunked",
    NULL
};

#define THE_OBJECT	"/foo"

/* Macros for test_create_unlink() & test_filespace */
#define GROUPNAME       "group"
#define GROUP2NAME      "group2"
#define UNLINK_NGROUPS         1000
#define DATASETNAME     "dataset"
#define DATASET2NAME    "dataset2"
#define ATTRNAME        "attribute"
#define TYPENAME        "datatype"
#define TYPE2NAME       "datatype2"
#define FILESPACE_NDIMS 3
#define FILESPACE_DIM0  20
#define FILESPACE_DIM1  20
#define FILESPACE_DIM2  20
#define FILESPACE_CHUNK0  10
#define FILESPACE_CHUNK1  10
#define FILESPACE_CHUNK2  10
#define FILESPACE_DEFLATE_LEVEL 6
#define FILESPACE_REWRITE       10
#define FILESPACE_NATTR 100
#define FILESPACE_ATTR_NDIMS    2
#define FILESPACE_ATTR_DIM0     5
#define FILESPACE_ATTR_DIM1     5
#define FILESPACE_TOP_GROUPS    10
#define FILESPACE_NESTED_GROUPS 50
#define FILESPACE_NDATASETS     50
#define SLASHES_GROUP_NAME      "Group///"
#define SLASHES_HARDLINK_NAME   "Hard///"
#define SLASHES_SOFTLINK_NAME   "Soft///"
#define SLASHES_SOFTLINK2_NAME  "Soft2///"
#define SLASHES_ROOTLINK_NAME   "Root///"


/*-------------------------------------------------------------------------
 * Function:	test_one
 *
 * Purpose:	Creates a group that has just one entry and then unlinks that
 *		entry.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_one(hid_t file)
{
    hid_t	work=-1, grp=-1;
    herr_t	status;

    /* Create a test group */
    if ((work=H5Gcreate(file, "/test_one", 0))<0) goto error;

    /* Delete by absolute name */
    TESTING("unlink by absolute name");
    if ((grp=H5Gcreate(work, "foo", 0))<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    if (H5Gunlink(file, "/test_one/foo")<0) goto error;
    PASSED();

    /* Delete by local name */
    TESTING("unlink by local name");
    if ((grp=H5Gcreate(work, "foo", 0))<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    if (H5Gunlink(work, "foo")<0) goto error;
    PASSED();

    /* Delete directly - should fail */
    TESTING("unlink without a name");
    if ((grp=H5Gcreate(work, "foo", 0))<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Gunlink(grp, ".");
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    Unlinking object w/o a name should have failed.");
	goto error;
    }
    if (H5Gclose(grp)<0) goto error;
    PASSED();

    /* Cleanup */
    if (H5Gclose(work)<0) goto error;
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(work);
	H5Gclose(grp);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_many
 *
 * Purpose:	Tests many unlinks in a single directory.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_many(hid_t file)
{
    hid_t	work=-1, grp=-1;
    int		i;
    const int	how_many=500;
    char	name[32];

    /* Create a test group */
    if ((work=H5Gcreate(file, "/test_many", 0))<0) goto error;
    if ((grp = H5Gcreate(work, "/test_many_foo", 0))<0) goto error;
    if (H5Gclose(grp)<0) goto error;

    /* Create a bunch of names and unlink them in order */
    TESTING("forward unlink");
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Glink(work, H5G_LINK_HARD, "/test_many_foo", name)<0) goto error;
    }
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Gunlink(work, name)<0) goto error;
    }
    PASSED();

    /* Create a bunch of names and unlink them in reverse order */
    TESTING("backward unlink");
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Glink(work, H5G_LINK_HARD, "/test_many_foo", name)<0) goto error;
    }
    for (i=how_many-1; i>=0; --i) {
	sprintf(name, "obj_%05d", i);
	if (H5Gunlink(work, name)<0) goto error;
    }
    PASSED();

    /* Create a bunch of names and unlink them from both directions */
    TESTING("inward unlink");
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Glink(work, H5G_LINK_HARD, "/test_many_foo", name)<0) goto error;
    }
    for (i=0; i<how_many; i++) {
	if (i%2) {
	    sprintf(name, "obj_%05d", how_many-(1+i/2));
	} else {
	    sprintf(name, "obj_%05d", i/2);
	}
	if (H5Gunlink(work, name)<0) goto error;
    }
    PASSED();

    /* Create a bunch of names and unlink them from the midle */
    TESTING("outward unlink");
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Glink(work, H5G_LINK_HARD, "/test_many_foo", name)<0) goto error;
    }
    for (i=how_many-1; i>=0; --i) {
	if (i%2) {
	    sprintf(name, "obj_%05d", how_many-(1+i/2));
	} else {
	    sprintf(name, "obj_%05d", i/2);
	}
	if (H5Gunlink(work, name)<0) goto error;
    }
    PASSED();


    /* Cleanup */
    if (H5Gclose(work)<0) goto error;
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(work);
	H5Gclose(grp);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_symlink
 *
 * Purpose:	Tests removal of symbolic links.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_symlink(hid_t file)
{
    hid_t	work=-1;

    TESTING("symlink removal");

    /* Create a test group and symlink */
    if ((work=H5Gcreate(file, "/test_symlink", 0))<0) TEST_ERROR;
    if (H5Glink(work, H5G_LINK_SOFT, "link_value", "link")<0) TEST_ERROR;
    if (H5Gunlink(work, "link")<0) TEST_ERROR;

    /* Cleanup */
    if (H5Gclose(work)<0) TEST_ERROR;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(work);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_rename
 *
 * Purpose:	Tests H5Gmove()
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_rename(hid_t file)
{
    hid_t	work=-1, foo=-1, inner=-1;

    /* Create a test group and rename something */
    TESTING("object renaming");
    if ((work=H5Gcreate(file, "/test_rename", 0))<0) goto error;
    if ((foo=H5Gcreate(work, "foo", 0))<0) goto error;
    if (H5Gmove(work, "foo", "bar")<0) goto error;
    if ((inner=H5Gcreate(foo, "inner", 0))<0) goto error;
    if (H5Gclose(inner)<0) goto error;
    if (H5Gclose(foo)<0) goto error;
    if ((inner=H5Gopen(work, "bar/inner"))<0) goto error;
    if (H5Gclose(inner)<0) goto error;
    PASSED();

    /* Try renaming a symlink */
    TESTING("symlink renaming");
    if (H5Glink(work, H5G_LINK_SOFT, "link_value", "link_one")<0) goto error;
    if (H5Gmove(work, "link_one", "link_two")<0) goto error;
    PASSED();

    /* Cleanup */
    if (H5Gclose(work)<0) goto error;
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(work);
	H5Gclose(foo);
	H5Gclose(inner);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_new_move
 *
 * Purpose:     Tests H5Gmove2()
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Thursday, April 25, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_new_move(void)
{
    hid_t 	fapl, file_a, file_b=(-1);
    hid_t	grp_1=(-1), grp_2=(-1), grp_move=(-1), moved_grp=(-1);
    char 	filename[1024];

    TESTING("new move");

    /* Create a second file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        goto error;
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        goto error;

    /* Create groups in first file */
    if((grp_1=H5Gcreate(file_a, "group1", 0))<0) goto error;
    if((grp_2=H5Gcreate(file_a, "group2", 0))<0) goto error;
    if((grp_move=H5Gcreate(grp_1, "group_move", 0))<0) goto error;

    /* Create hard and soft links. */
    if(H5Glink2(grp_1, "group_move", H5G_LINK_HARD, H5G_SAME_LOC, "hard")<0)
	goto error;
    if(H5Glink2(grp_1, "/group1/group_move", H5G_LINK_SOFT, grp_2, "soft")<0)
	goto error;

    /* Move a group within the file.  Both of source and destination use
     * H5G_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Gmove2(H5G_SAME_LOC, "group_move", H5G_SAME_LOC, "group_new_name")
		!=FAIL) goto error;
    } H5E_END_TRY;

    /* Move a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Gmove2(grp_1, "group_move", file_b, "group_new_name")!=FAIL)
	    goto error;
    } H5E_END_TRY;

    /* Move a group across groups in the same file. */
    if(H5Gmove2(grp_1, "group_move", grp_2, "group_new_name")<0)
	goto error;

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen(grp_2, "group_new_name"))<0)
	goto error;

    H5Gclose(grp_1);
    H5Gclose(grp_2);
    H5Gclose(grp_move);
    H5Gclose(moved_grp);
    H5Fclose(file_a);
    H5Fclose(file_b);

    PASSED();
    return 0;

  error:
    H5E_BEGIN_TRY {
 	H5Gclose(grp_1);
	H5Gclose(grp_2);
	H5Gclose(grp_move);
        H5Gclose(moved_grp);
	H5Fclose(file_a);
	H5Fclose(file_b);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    check_new_move
 *
 * Purpose:     Checks result of H5Gmove2()
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Thursday, April 25, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
check_new_move(void)
{
    hid_t 	fapl, file;
    H5G_stat_t	sb_hard1, sb_hard2;
    char 	filename[1024];
    char 	linkval[1024];

    TESTING("check new move function");

    /* Open file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) {
        goto error;
    }

    /* Get hard link info */
    if(H5Gget_objinfo(file, "/group2/group_new_name", TRUE, &sb_hard1)<0)
	goto error;
    if(H5Gget_objinfo(file, "/group1/hard", TRUE, &sb_hard2)<0)
	goto error;

    /* Check hard links */
    if(H5G_GROUP!=sb_hard1.type || H5G_GROUP!=sb_hard2.type) {
        H5_FAILED();
        puts("    Unexpected object type, should have been a group");
        goto error;
    }
    if( sb_hard1.objno[0]!=sb_hard2.objno[0] ||
        sb_hard1.objno[1]!=sb_hard2.objno[1] ) {
        H5_FAILED();
        puts("    Hard link test failed.  Link seems not to point to the ");
        puts("    expected file location.");
        goto error;
    }

    /* Check soft links */
    if (H5Gget_linkval(file, "group2/soft", sizeof linkval, linkval)<0) {
        goto error;
    }
    if (strcmp(linkval, "/group1/group_move")) {
        H5_FAILED();
        puts("    Soft link test failed. Wrong link value");
        goto error;
    }

    /* Cleanup */
    if(H5Fclose(file)<0) goto error;
    PASSED();
    return 0;

  error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_filespace
 *
 * Purpose:     Test proper reuse of space in the file when objects are unlinked
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Saturday, March 22, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_filespace(void)
{
    hid_t 	fapl;           /* File access property list */
    hid_t 	fapl_nocache;   /* File access property list with raw data cache turned off */
    hid_t 	contig_dcpl;    /* Dataset creation property list for contiguous dataset */
    hid_t 	early_chunk_dcpl; /* Dataset creation property list for chunked dataset & early allocation */
    hid_t 	late_chunk_dcpl; /* Dataset creation property list for chunked dataset & late allocation */
    hid_t 	comp_dcpl;      /* Dataset creation property list for compressed, chunked dataset */
    hid_t 	compact_dcpl;   /* Dataset creation property list for compact dataset */
    hid_t 	file;           /* File ID */
    hid_t 	group, group2;  /* Group IDs */
    hid_t 	dataset, dataset2;      /* Dataset IDs */
    hid_t 	space;          /* Dataspace ID */
    hid_t 	type;           /* Datatype ID */
    hid_t 	attr_space;     /* Dataspace ID for attributes */
    hid_t 	attr;           /* Attribute ID */
    char 	filename[1024]; /* Name of file to create */
    char 	objname[128];   /* Name of object to create */
    hsize_t     dims[FILESPACE_NDIMS]= {FILESPACE_DIM0, FILESPACE_DIM1, FILESPACE_DIM2};        /* Dataset dimensions */
    hsize_t     chunk_dims[FILESPACE_NDIMS]= {FILESPACE_CHUNK0, FILESPACE_CHUNK1, FILESPACE_CHUNK2};        /* Chunk dimensions */
    hsize_t     attr_dims[FILESPACE_ATTR_NDIMS]= {FILESPACE_ATTR_DIM0, FILESPACE_ATTR_DIM1};        /* Attribute dimensions */
    int        *data = NULL;    /* Pointer to dataset buffer */
    int        *tmp_data;       /* Temporary pointer to dataset buffer */
    h5_stat_size_t	empty_size;     /* Size of an empty file */
    h5_stat_size_t	file_size;      /* Size of each file created */
    herr_t	status;         /* Function status return value */
    unsigned u,v,w;             /* Local index variables */

    /* Metadata cache parameters */
    int mdc_nelmts;
#ifdef H5_WANT_H5_V1_4_COMPAT
    int rdcc_nelmts;
#else /* H5_WANT_H5_V1_4_COMPAT */
    size_t rdcc_nelmts;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    size_t rdcc_nbytes;
    double rdcc_w0;


    puts("Testing file space gets reused:");

    /* Open file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

/* Create FAPL with raw data cache disabled */
    /* Create file access property list with raw data cache disabled */
    if ((fapl_nocache=H5Pcopy(fapl))<0) TEST_ERROR;

    /* Get the cache settings */
    if(H5Pget_cache(fapl_nocache,&mdc_nelmts,&rdcc_nelmts,&rdcc_nbytes,&rdcc_w0)<0) TEST_ERROR;

    /* Disable the raw data cache */
    rdcc_nelmts=0;
    rdcc_nbytes=0;
    if(H5Pset_cache(fapl_nocache,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0)<0) TEST_ERROR;

/* Create empty file for size comparisons later */

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of an empty file */
    if((empty_size=h5_get_file_size(filename))<0) TEST_ERROR;

/* Create common objects for datasets */

    /* Create dataset creation property list for contigous storage */
    if ((contig_dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) TEST_ERROR;

    /* Make certain that space is allocated early */
    if(H5Pset_alloc_time(contig_dcpl, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR;

    /* Create dataset creation property list for chunked storage & early allocation */
    if ((early_chunk_dcpl=H5Pcopy(contig_dcpl))<0) TEST_ERROR;

    /* Set chunk dimensions */
    if(H5Pset_chunk(early_chunk_dcpl, FILESPACE_NDIMS, chunk_dims) < 0) TEST_ERROR;

    /* Create dataset creation property list for chunked storage & late allocation */
    if ((late_chunk_dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) TEST_ERROR;

    /* Set chunk dimensions */
    if(H5Pset_chunk(late_chunk_dcpl, FILESPACE_NDIMS, chunk_dims) < 0) TEST_ERROR;

    /* Create dataset creation property list for compressed, chunked storage & early allocation */
    if ((comp_dcpl=H5Pcopy(early_chunk_dcpl))<0) TEST_ERROR;

    /* Enable compression & set level */
    if(H5Pset_deflate(comp_dcpl, FILESPACE_DEFLATE_LEVEL) < 0) TEST_ERROR;

    /* Create dataset creation property list for compact storage */
    if ((compact_dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) TEST_ERROR;

    /* Set to compact storage */
    if(H5Pset_layout(compact_dcpl, H5D_COMPACT) < 0) TEST_ERROR;

    /* Create dataspace for datasets */
    if((space = H5Screate_simple(FILESPACE_NDIMS, dims, NULL))<0) TEST_ERROR;

    /* Create buffer for writing dataset */
    if((data = HDmalloc(sizeof(int)*FILESPACE_DIM0*FILESPACE_DIM1*FILESPACE_DIM2))==NULL) TEST_ERROR;

/* Create single dataset (with contiguous storage & late allocation), remove it & verify file size */
    TESTING("    contiguous dataset with late allocation");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT))<0) TEST_ERROR;
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create single dataset (with contiguous storage & early allocation), remove it & verify file size */
    TESTING("    contiguous dataset with early allocation");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, contig_dcpl))<0) TEST_ERROR;
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create single dataset (with chunked storage & late allocation), remove it & verify file size */
    TESTING("    chunked dataset with late allocation");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, late_chunk_dcpl))<0) TEST_ERROR;
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create single dataset (with chunked storage & early allocation), remove it & verify file size */
    TESTING("    chunked dataset with early allocation");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, early_chunk_dcpl))<0) TEST_ERROR;
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create single dataset (with compressed storage & early allocation), remove it & verify file size */
    TESTING("    compressed, chunked dataset");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, comp_dcpl))<0) TEST_ERROR;
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create single dataset (with compressed storage & early allocation), re-write it a bunch of
 * times (which should re-allocate blocks many times) and remove it & verify
 * file size.
 */
    TESTING("    re-writing compressed, chunked dataset");

    /* Create file (using FAPL with disabled raw data cache) */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_nocache))<0) TEST_ERROR;

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, comp_dcpl))<0) TEST_ERROR;

    /* Alternate re-writing dataset with compressible & random data */
    for(u=0; u<FILESPACE_REWRITE; u++) {
        /* Set buffer to some compressible values */
        for (v=0, tmp_data=data; v<(FILESPACE_DIM0*FILESPACE_DIM1*FILESPACE_DIM2); v++)
            *tmp_data++ = v*u;

        /* Write the buffer to the dataset */
        if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data)<0) TEST_ERROR;

        /* Set buffer to different random numbers each time */
        for (v=0, tmp_data=data; v<(FILESPACE_DIM0*FILESPACE_DIM1*FILESPACE_DIM2); v++)
            *tmp_data++ = (int)HDrandom();

        /* Write the buffer to the dataset */
        if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data)<0) TEST_ERROR;
    } /* end for */

    /* Close dataset */
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create single dataset (with compact storage), remove it & verify file size */
    TESTING("    compact dataset");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, compact_dcpl))<0) TEST_ERROR;
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create two datasets (with contiguous storage), alternate adding attributes
 * to each one (which creates many object header continuations),
 * remove both & verify file size.
 */
    TESTING("    object header continuations");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create datasets to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, contig_dcpl))<0) TEST_ERROR;
    if((dataset2 = H5Dcreate (file, DATASET2NAME, H5T_NATIVE_INT, space, contig_dcpl))<0) TEST_ERROR;

    /* Create a dataspace for the attributes */
    if((attr_space = H5Screate_simple(FILESPACE_ATTR_NDIMS, attr_dims, NULL))<0) TEST_ERROR;

    /* Alternate adding attributes to each one */
    for(u=0; u<FILESPACE_NATTR; u++) {
        /* Set the name of the attribute to create */
        sprintf(objname,"%s %u",ATTRNAME,u);

        /* Create an attribute on the first dataset */
        if((attr = H5Acreate (dataset, objname, H5T_NATIVE_INT, attr_space, H5P_DEFAULT))<0) TEST_ERROR;

        /* Don't worry about writing the attribute - it will have a fill value */

        /* Close the attribute on the first dataset */
        if(H5Aclose (attr)<0) TEST_ERROR;

        /* Create an attribute on the second dataset */
        if((attr = H5Acreate (dataset2, objname, H5T_NATIVE_INT, attr_space, H5P_DEFAULT))<0) TEST_ERROR;

        /* Don't worry about writing the attribute - it will have a fill value */

        /* Close the attribute on the second dataset */
        if(H5Aclose (attr)<0) TEST_ERROR;

        /* Flush the file (to fix the sizes of object header buffers, etc) */
        if(H5Fflush(file,H5F_SCOPE_GLOBAL)<0) TEST_ERROR;
    } /* end for */

    /* Close the dataspace for the attributes */
    if(H5Sclose (attr_space)<0) TEST_ERROR;

    /* Close datasets */
    if(H5Dclose (dataset)<0) TEST_ERROR;
    if(H5Dclose (dataset2)<0) TEST_ERROR;

    /* Remove the datasets */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;
    if(H5Gunlink (file, DATASET2NAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create single named datatype, remove it & verify file size */
    TESTING("    named datatype");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create datatype to commit */
    if((type = H5Tcopy (H5T_NATIVE_INT))<0) TEST_ERROR;

    /* Create a single named datatype to remove */
    if(H5Tcommit (file, TYPENAME, type)<0) TEST_ERROR;
    if(H5Tclose (type)<0) TEST_ERROR;

    /* Remove the named datatype */
    if(H5Gunlink (file, TYPENAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create single group, remove it & verify file size */
    TESTING("    single group");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single group to remove */
    if((group = H5Gcreate (file, GROUPNAME, 0))<0) TEST_ERROR;
    if(H5Gclose (group)<0) TEST_ERROR;

    /* Remove the group */
    if(H5Gunlink (file, GROUPNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create many groups, remove them & verify file size */
    TESTING("    multiple groups");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a many groups to remove */
    for(u=0; u<UNLINK_NGROUPS; u++) {
        sprintf(objname,"%s %u",GROUPNAME,u);
        if((group = H5Gcreate (file, objname, 0))<0) TEST_ERROR;
        if(H5Gclose (group)<0) TEST_ERROR;
    } /* end for */

    /* Remove the all the groups */
    for(u=0; u<UNLINK_NGROUPS; u++) {
        sprintf(objname,"%s %u",GROUPNAME,u);
        if(H5Gunlink (file, objname)<0) TEST_ERROR;
    } /* end for */

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create simple group hiearchy, remove it & verify file size */
    TESTING("    simple group hierarchy");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a small group hierarchy to remove */
    if((group = H5Gcreate (file, GROUPNAME, 0))<0) TEST_ERROR;
    if((group2 = H5Gcreate (group, GROUP2NAME, 0))<0) TEST_ERROR;
    if(H5Gclose (group2)<0) TEST_ERROR;
    if(H5Gclose (group)<0) TEST_ERROR;

    /* Remove the second group */
    if(H5Gunlink (file, GROUPNAME "/" GROUP2NAME)<0) TEST_ERROR;

    /* Remove the first group */
    if(H5Gunlink (file, GROUPNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create complex group hiearchy, remove it & verify file size */
    TESTING("    complex group hierarchy");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a complex group hierarchy to remove */
    for(u=0; u<FILESPACE_TOP_GROUPS; u++) {
        /* Create group */
        sprintf(objname,"%s %u",GROUPNAME,u);
        if((group = H5Gcreate (file, objname, 0))<0) TEST_ERROR;

        /* Create nested groups inside top groups */
        for(v=0; v<FILESPACE_NESTED_GROUPS; v++) {
            /* Create group */
            sprintf(objname,"%s %u",GROUP2NAME,v);
            if((group2 = H5Gcreate (group, objname, 0))<0) TEST_ERROR;

            /* Create datasets inside nested groups */
            for(w=0; w<FILESPACE_NDATASETS; w++) {
                /* Create & close a dataset */
                sprintf(objname,"%s %u",DATASETNAME,w);
                if((dataset = H5Dcreate (group2, objname, H5T_NATIVE_INT, space, H5P_DEFAULT))<0) TEST_ERROR;
                if(H5Dclose (dataset)<0) TEST_ERROR;
            } /* end for */

            /* Close nested group */
            if(H5Gclose (group2)<0) TEST_ERROR;
        } /* end for */

        /* Close top group */
        if(H5Gclose (group)<0) TEST_ERROR;
    } /* end for */

    /* Remove complex group hierarchy */
    for(u=0; u<FILESPACE_TOP_GROUPS; u++) {
        /* Open group */
        sprintf(objname,"%s %u",GROUPNAME,u);
        if((group = H5Gopen (file, objname))<0) TEST_ERROR;

        /* Open nested groups inside top groups */
        for(v=0; v<FILESPACE_NESTED_GROUPS; v++) {
            /* Create group */
            sprintf(objname,"%s %u",GROUP2NAME,v);
            if((group2 = H5Gopen (group, objname))<0) TEST_ERROR;

            /* Remove datasets inside nested groups */
            for(w=0; w<FILESPACE_NDATASETS; w++) {
                /* Remove dataset */
                sprintf(objname,"%s %u",DATASETNAME,w);
                if(H5Gunlink (group2, objname)<0) TEST_ERROR;
            } /* end for */

            /* Close nested group */
            if(H5Gclose (group2)<0) TEST_ERROR;

            /* Remove nested group */
            sprintf(objname,"%s %u",GROUP2NAME,v);
            if(H5Gunlink (group, objname)<0) TEST_ERROR;
        } /* end for */

        /* Close top group */
        if(H5Gclose (group)<0) TEST_ERROR;

        /* Remove top group */
        sprintf(objname,"%s %u",GROUPNAME,u);
        if(H5Gunlink (file, objname)<0) TEST_ERROR;
    } /* end for */

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create dataset and duplicate dataset, remove original & verify file size */
    TESTING("    duplicate dataset");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single dataset to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT))<0) TEST_ERROR;
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Create another dataset with same name */
    H5E_BEGIN_TRY {
        dataset=H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, H5P_DEFAULT);
    } H5E_END_TRY;
    if (dataset>=0) {
        H5Dclose(dataset);
        TEST_ERROR;
    } /* end if */

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create group and duplicate group, remove original & verify file size */
    TESTING("    duplicate group");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a single group to remove */
    if((group = H5Gcreate (file, GROUPNAME, 0))<0) TEST_ERROR;
    if(H5Gclose (group)<0) TEST_ERROR;

    /* Create another group with same name */
    H5E_BEGIN_TRY {
        group = H5Gcreate (file, GROUPNAME, 0);
    } H5E_END_TRY;
    if (group>=0) {
        H5Gclose(group);
        TEST_ERROR;
    } /* end if */

    /* Remove the group */
    if(H5Gunlink (file, GROUPNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create named datatype and duplicate named datatype, remove original & verify file size */
    TESTING("    duplicate named datatype");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create datatype to commit */
    if((type = H5Tcopy (H5T_NATIVE_INT))<0) TEST_ERROR;

    /* Create a single named datatype to remove */
    if(H5Tcommit (file, TYPENAME, type)<0) TEST_ERROR;
    if(H5Tclose (type)<0) TEST_ERROR;

    /* Create datatype to commit */
    if((type = H5Tcopy (H5T_NATIVE_INT))<0) TEST_ERROR;

    /* Create another named datatype with same name */
    H5E_BEGIN_TRY {
        status = H5Tcommit (file, TYPENAME, type);
    } H5E_END_TRY;
    if (status>=0) TEST_ERROR;
    if(H5Tclose (type)<0) TEST_ERROR;

    /* Remove the named datatype */
    if(H5Gunlink (file, TYPENAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Create named datatype and duplicate named datatype, remove original & verify file size */
    TESTING("    duplicate attribute");

    /* Create file */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create datasets to remove */
    if((dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, space, contig_dcpl))<0) TEST_ERROR;

    /* Create a dataspace for the attributes */
    if((attr_space = H5Screate_simple(FILESPACE_ATTR_NDIMS, attr_dims, NULL))<0) TEST_ERROR;

    /* Create an attribute on the dataset */
    if((attr = H5Acreate (dataset, ATTRNAME, H5T_NATIVE_INT, attr_space, H5P_DEFAULT))<0) TEST_ERROR;

    /* Don't worry about writing the attribute - it will have a fill value */

    /* Close the attribute on the dataset */
    if(H5Aclose (attr)<0) TEST_ERROR;

    /* Create another attribute with same name */
    H5E_BEGIN_TRY {
        attr = H5Acreate (dataset, ATTRNAME, H5T_NATIVE_INT, attr_space, H5P_DEFAULT);
    } H5E_END_TRY;
    if (attr>=0) {
        H5Aclose(attr);
        TEST_ERROR;
    } /* end if */

    /* Close the dataspace for the attributes */
    if(H5Sclose (attr_space)<0) TEST_ERROR;

    /* Close dataset */
    if(H5Dclose (dataset)<0) TEST_ERROR;

    /* Remove the dataset */
    if(H5Gunlink (file, DATASETNAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))<0) TEST_ERROR;

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR;

    PASSED();

/* Cleanup common objects */

    /* Release dataset buffer */
    HDfree(data);

    /* Close property lists */
    if(H5Pclose(fapl)<0) TEST_ERROR;
    if(H5Pclose(fapl_nocache)<0) TEST_ERROR;
    if(H5Pclose(contig_dcpl)<0) TEST_ERROR;
    if(H5Pclose(early_chunk_dcpl)<0) TEST_ERROR;
    if(H5Pclose(late_chunk_dcpl)<0) TEST_ERROR;
    if(H5Pclose(comp_dcpl)<0) TEST_ERROR;
    if(H5Pclose(compact_dcpl)<0) TEST_ERROR;

    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;

    /* Indicate success */
    /* Don't print final "PASSED", since we aren't on the correct line anymore */
    return 0;

error:
    /* Release dataset buffer */
    if(data)
        HDfree(data);

    return 1;
} /* end test_filespace() */


/*-------------------------------------------------------------------------
 * Function:    test_create_unlink
 *
 * Purpose:     Creates and then unlinks a large number of objects
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Friday, April 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int test_create_unlink(const char *msg, hid_t fapl)
{
    hid_t 	file, group;
    unsigned u;
    char 	groupname[1024];
    char	filename[1024];

    TESTING(msg);

    /* Create file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
    {
        H5_FAILED();
        puts("    Creating file failed");
        goto error;
    }

    /* Create a many groups to remove */
    for(u=0; u<UNLINK_NGROUPS; u++) {
        sprintf(groupname,"%s %u",GROUPNAME,u);
        if((group = H5Gcreate (file, groupname, 0))<0)
        {
            H5_FAILED();
            printf("group %s creation failed\n",groupname);
            goto error;
        }
        if(H5Gclose (group)<0)
        {
            H5_FAILED();
            printf("closing group %s failed\n",groupname);
            goto error;
        }
    } /* end for */

    /* Remove the all the groups */
    for(u=0; u<UNLINK_NGROUPS; u++) {
        sprintf(groupname,"%s %u",GROUPNAME,u);
        if(H5Gunlink (file, groupname)<0)
        {
            H5_FAILED();
            printf("Unlinking group %s failed\n",groupname);
            goto error;
        }
    } /* end for */

    /* Close file */
    if(H5Fclose(file)<0)
    {
        H5_FAILED();
        printf("Closing file failed\n");
        goto error;
    }

    PASSED();
    return 0;

error:
    return 1;
} /* end test_create_unlink() */


/*-------------------------------------------------------------------------
 * Function:    test_link_slashes
 *
 * Purpose:     Tests creating links with various multiple & trailing slashes
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Saturday, August 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_link_slashes(void)
{
    hid_t 	fapl;           /* File access property list */
    hid_t fid;          /* File ID */
    hid_t gid,gid2;     /* Group ID */
    char	filename[1024];

    TESTING("creating links with multiple slashes");

    /* Create file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Create a file */
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0) TEST_ERROR;

    /* Create a group in the root group */
    if((gid = H5Gcreate(fid, SLASHES_GROUP_NAME, 0))<0) TEST_ERROR;

    /* Create a nested group in the root group */
    if((gid2 = H5Gcreate(gid, SLASHES_GROUP_NAME, 0))<0) TEST_ERROR;

    /* Close the nested group */
    if(H5Gclose(gid2)<0) TEST_ERROR;

    /* Create a hard link to the nested group */
    if(H5Glink2(gid, SLASHES_GROUP_NAME, H5G_LINK_HARD, H5G_SAME_LOC, SLASHES_HARDLINK_NAME)<0) TEST_ERROR;

    /* Create a soft link with a relative path to the nested group */
    if(H5Glink2(gid, SLASHES_GROUP_NAME, H5G_LINK_SOFT, H5G_SAME_LOC, SLASHES_SOFTLINK_NAME)<0) TEST_ERROR;

    /* Create a soft link with the full path to the nested group */
    if(H5Glink2(gid, "////"SLASHES_GROUP_NAME""SLASHES_GROUP_NAME, H5G_LINK_SOFT, H5G_SAME_LOC, SLASHES_SOFTLINK2_NAME)<0) TEST_ERROR;

    /* Create a soft link to the root group */
    if(H5Glink2(gid, "////", H5G_LINK_SOFT, H5G_SAME_LOC, SLASHES_ROOTLINK_NAME)<0) TEST_ERROR;

    /* Close the group */
    if(H5Gclose(gid)<0) TEST_ERROR;

    /* Create a hard link to the existing group */
    if(H5Glink2(fid, SLASHES_GROUP_NAME, H5G_LINK_HARD, H5G_SAME_LOC, SLASHES_HARDLINK_NAME)<0) TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    return 1;
} /* end test_link_slashes() */


/*-------------------------------------------------------------------------
 * Function:    test_unlink_slashes
 *
 * Purpose:     Tests deleting links with various multiple & trailing slashes
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Saturday, August 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_slashes(void)
{
    hid_t 	fapl;           /* File access property list */
    hid_t fid;          /* File ID */
    hid_t gid;          /* Group ID */
    char	filename[1024];

    TESTING("deleting links with multiple slashes");

    /* Create file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Open the file */
    if((fid=H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT))<0) TEST_ERROR;

    /* Open the top level group */
    if((gid = H5Gopen(fid, SLASHES_GROUP_NAME))<0) TEST_ERROR;

    /* Delete the root link */
    if(H5Gunlink(gid,SLASHES_ROOTLINK_NAME)<0) TEST_ERROR;

    /* Delete the soft link with the full path */
    if(H5Gunlink(gid,SLASHES_SOFTLINK2_NAME)<0) TEST_ERROR;

    /* Delete the soft link with the relative path */
    if(H5Gunlink(gid,SLASHES_SOFTLINK_NAME)<0) TEST_ERROR;

    /* Delete the hard link */
    if(H5Gunlink(gid,SLASHES_HARDLINK_NAME)<0) TEST_ERROR;

    /* Delete the group itself */
    if(H5Gunlink(gid,SLASHES_GROUP_NAME)<0) TEST_ERROR;

    /* Close the group */
    if(H5Gclose(gid)<0) TEST_ERROR;

    /* Delete the hard link */
    if(H5Gunlink(fid,SLASHES_HARDLINK_NAME)<0) TEST_ERROR;

    /* Delete the group itself */
    if(H5Gunlink(fid,SLASHES_GROUP_NAME)<0) TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    return 1;
} /* end test_unlink_slashes() */

/*
 * Helper routine for test_unlink_rightleaf()
 */
static int delete_node (hid_t pid, hid_t id)
{
    char name[256];     /* Name of object to close */

    /* Get the name of the object to delete */
    if(H5Iget_name(id, name, sizeof(name))<0) return (-1);

    /* Close the object */
    if(H5Gclose (id)<0) return(-1);

    /* Unlink the object */
    if(H5Gunlink (pid, name)<0) return(-1);

    /* If this object is the right-most child, try opening the previous object */
    if(HDstrcmp(name,"/Zone81")==0) {
        hid_t gid;

        if((gid = H5Gopen (pid, "/Zone80"))<0) return(-1);
        if(H5Gclose(gid)<0) return(-1);
    } /* end if */

    /* Indicate success */
    return(0);
}


/*-------------------------------------------------------------------------
 * Function:    test_unlink_rightleaf
 *
 * Purpose:     Tests deleting objects in a way that triggers deletion of the
 *              right child in the leaf of a non-leaf B-tree node
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, January 19, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_rightleaf(hid_t fid)
{
    hid_t rootid,       /* Group ID for root group */
        *gids;          /* Array of IDs for groups created */
    int n,              /* Local index variable */
        ngroups = 150;  /* Number of groups to create */
    char name[256];     /* Name of object to create */

    TESTING("deleting right-most child in non-leaf B-tree node");

    /* Allocate space for the group IDs */
    gids = (hid_t *) HDmalloc (ngroups * sizeof(hid_t));
    if(gids==NULL) TEST_ERROR;

    if((rootid = H5Gopen (fid, "/"))<0) TEST_ERROR;

    /* Create all the groups */
    for (n = 0; n < ngroups; n++) {
        sprintf(name, "Zone%d", n + 1);
        if((gids[n] = H5Gcreate (rootid, name, 0))<0) TEST_ERROR;
    } /* end for */

    /* Unlink & re-create each group */
    for (n = 0; n < ngroups; n++) {
        if(delete_node (rootid, gids[n])<0) TEST_ERROR;
        sprintf(name, "Zone%d", n + 1);
        if((gids[n] = H5Gcreate (rootid, name, 0))<0) TEST_ERROR;
    } /* end for */

    /* Close all the groups */
    for (n = 0; n < ngroups; n++)
        if(H5Gclose(gids[n])<0) TEST_ERROR;

    /* Close root group ID */
    if(H5Gclose(rootid)<0) TEST_ERROR;

    /* Free memory */
    HDfree(gids);

    PASSED();
    return 0;

error:
    return 1;
} /* end test_unlink_rightleaf() */


/*-------------------------------------------------------------------------
 * Function:    test_unlink_rightnode
 *
 * Purpose:     Tests deleting objects in a way that triggers deletion of the
 *              entire right child leaf of a non-leaf B-tree node
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, January 19, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_rightnode(hid_t fid)
{
    hid_t rootid,       /* Group ID for root group */
        *gids;          /* Array of IDs for groups created */
    int n,              /* Local index variable */
        ngroups = 150;  /* Number of groups to create */
    char name[256];     /* Name of object to create */

    TESTING("deleting right-most child in non-leaf B-tree node");

    /* Allocate space for the group IDs */
    gids = (hid_t *) HDmalloc (ngroups * sizeof(hid_t));
    if(gids==NULL) TEST_ERROR;

    if((rootid = H5Gopen (fid, "/"))<0) TEST_ERROR;

    /* Create all the groups */
    for (n = 0; n < ngroups; n++) {
        sprintf(name, "ZoneB%d", n + 1);
        if((gids[n] = H5Gcreate (rootid, name, 0))<0) TEST_ERROR;
    } /* end for */

    /* Close all the groups */
    for (n = 0; n < ngroups; n++)
        if(H5Gclose(gids[n])<0) TEST_ERROR;

    /* Unlink specific objects to trigger deletion of right leaf in non-leaf node */
    if(H5Gunlink(fid,"/ZoneB77")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneB78")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneB79")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneB8")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneB80")<0) TEST_ERROR;

    /* Close root group ID */
    if(H5Gclose(rootid)<0) TEST_ERROR;

    /* Free memory */
    HDfree(gids);

    PASSED();
    return 0;

error:
    return 1;
} /* end test_unlink_rightnode() */


/*-------------------------------------------------------------------------
 * Function:    test_unlink_middlenode
 *
 * Purpose:     Tests deleting objects in a way that triggers deletion of all
 *              the leafs of a "middle" non-leaf B-tree node
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, January 19, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_middlenode(hid_t fid)
{
    hid_t rootid,       /* Group ID for root group */
        *gids;          /* Array of IDs for groups created */
    int n,              /* Local index variable */
        ngroups = 250;  /* Number of groups to create */
    char name[256];     /* Name of object to create */

    TESTING("deleting right-most child in non-leaf B-tree node");

    /* Allocate space for the group IDs */
    gids = (hid_t *) HDmalloc (ngroups * sizeof(hid_t));
    if(gids==NULL) TEST_ERROR;

    if((rootid = H5Gopen (fid, "/"))<0) TEST_ERROR;

    /* Create all the groups */
    for (n = 0; n < ngroups; n++) {
        sprintf(name, "ZoneC%d", n + 1);
        if((gids[n] = H5Gcreate (rootid, name, 0))<0) TEST_ERROR;
    } /* end for */

    /* Close all the groups */
    for (n = 0; n < ngroups; n++)
        if(H5Gclose(gids[n])<0) TEST_ERROR;

    /* Unlink specific objects to trigger deletion of all leafs in "interior" non-leaf node */
    if(H5Gunlink(fid,"/ZoneC11")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC110")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC111")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC112")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC113")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC114")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC115")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC116")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC117")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC118")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC119")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC12")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC120")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC121")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC122")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC123")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC124")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC125")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC126")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC127")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC128")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC129")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC13")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC130")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC131")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC132")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC133")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC134")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC135")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC136")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC137")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC138")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC139")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC14")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC140")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC141")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC142")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC143")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC144")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC145")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC146")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC147")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC148")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC149")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC15")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC150")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC151")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC152")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC153")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC154")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC155")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC156")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC157")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC158")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC159")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC16")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC160")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC161")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC162")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC163")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC164")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC165")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC166")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC167")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC168")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC169")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC17")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC170")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC171")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC172")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC173")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC174")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC175")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC176")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC177")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC178")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC179")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC18")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC180")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC19")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC2")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC20")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC21")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC22")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC23")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC24")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC25")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC26")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC27")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC28")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC29")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC3")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC30")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC31")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC32")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC33")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC34")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC35")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC36")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC37")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC38")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC39")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC4")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC40")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC41")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC42")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC43")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC44")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC45")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC46")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC47")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC48")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC49")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC5")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC50")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC51")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC52")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC53")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC54")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC55")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC56")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC57")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC58")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC59")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC6")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC60")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC61")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC62")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC63")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC64")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC65")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC66")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC67")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC68")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC69")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC7")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC70")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC71")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC72")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC73")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC74")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC75")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC76")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC77")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC78")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC79")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC8")<0) TEST_ERROR;
    if(H5Gunlink(fid,"/ZoneC80")<0) TEST_ERROR;

    /* Close root group ID */
    if(H5Gclose(rootid)<0) TEST_ERROR;

    /* Free memory */
    HDfree(gids);

    PASSED();
    return 0;

error:
    return 1;
} /* end test_unlink_middlenode() */


/*-------------------------------------------------------------------------
 * Function:    test_resurrect_dataset
 *
 * Purpose:     Tests deleting a dataset while its still open and then
 *              "resurrecting" it by creating a link to it again.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, July 14, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_resurrect_dataset(void)
{
    hid_t       f=-1, s=-1, d=-1, fapl=-1;
    char	filename[1024];

    TESTING("resurrecting dataset after deletion");

    /* Create file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[6], fapl, filename, sizeof filename);

    /* Create the file */
    if((f = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a dataset in the file */
    if((s = H5Screate(H5S_SCALAR))<0) TEST_ERROR;
    if((d = H5Dcreate(f, DATASETNAME, H5T_NATIVE_INT, s, H5P_DEFAULT))<0) TEST_ERROR;
    if(H5Sclose(s)<0) TEST_ERROR;

    /* Unlink the dataset while it's open (will mark it for deletion when closed) */
    if(H5Gunlink(f, DATASETNAME)<0) TEST_ERROR;

    /* Check that dataset name is NULL */
    if(H5Iget_name(d, NULL, 0) != 0) TEST_ERROR;

    /* Re-link the dataset to the group hierarchy (shouldn't get deleted now) */
    if(H5Glink2(d, ".", H5G_LINK_HARD, f, DATASET2NAME)<0) TEST_ERROR;

    /* Close things */
    if(H5Dclose(d)<0) TEST_ERROR;
    if(H5Fclose(f)<0) TEST_ERROR;

    /* Re-open the file */
    if((f=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) TEST_ERROR;

    /* Attempt to open the dataset under the new name */
    if((d=H5Dopen(f,DATASET2NAME))<0) TEST_ERROR;

    /* Close things */
    if(H5Dclose(d)<0) TEST_ERROR;
    if(H5Fclose(f)<0) TEST_ERROR;
    if(H5Pclose(fapl)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Sclose(s);
	H5Dclose(d);
	H5Fclose(f);
	H5Pclose(fapl);
    } H5E_END_TRY;
    return 1;
} /* end test_resurrect_dataset() */


/*-------------------------------------------------------------------------
 * Function:    test_resurrect_datatype
 *
 * Purpose:     Tests deleting a datatype while it's still open and then
 *              "resurrecting" it by creating a link to it again.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Wednesday, July 28, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_resurrect_datatype(void)
{
    hid_t       file=-1, type=-1, fapl=-1;
    char        filename[1024];

    TESTING("resurrecting datatype after deletion");

    /* Create file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[7], fapl, filename, sizeof filename);

    /* Create the file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a named datatype in the file */
    if((type = H5Tcopy (H5T_NATIVE_INT))<0) TEST_ERROR;
    if(H5Tcommit (file, TYPENAME, type)<0) TEST_ERROR;

    /* Unlink the datatype while it's open (will mark it for deletion when closed) */
    if(H5Gunlink(file, TYPENAME)<0) TEST_ERROR;

    /* Check that datatype name is NULL */
    if(H5Iget_name(type, NULL, 0) != 0) TEST_ERROR;

    /* Re-link the datatype to the group hierarchy (shouldn't get deleted now) */
    if(H5Glink2(type, ".", H5G_LINK_HARD, file, TYPE2NAME) < 0) TEST_ERROR;

    /* Close things */
    if(H5Tclose(type)<0) TEST_ERROR;
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Re-open the file */
    if((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) TEST_ERROR;

    /* Attempt to open the datatype under the new name */
    if((type=H5Topen(file,TYPE2NAME))<0) TEST_ERROR;

    /* Close things */
    if(H5Tclose(type)<0) TEST_ERROR;
    if(H5Fclose(file)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Tclose(type);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_resurrect_datatype() */


/*-------------------------------------------------------------------------
 * Function:    test_resurrect_group
 *
 * Purpose:     Tests deleting a group while it's still open and then
 *              "resurrecting" it by creating a link to it again.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Wednesday, July 28, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_resurrect_group(void)
{
    hid_t       file=-1, group=-1, fapl=-1;
    char        filename[1024];

    TESTING("resurrecting group after deletion");

    /* Create file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);

    /* Create the file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create a group in the file */
    if((group = H5Gcreate (file, GROUPNAME, 0))<0) TEST_ERROR;

    /* Unlink the group while it's open (will mark it for deletion when closed) */
    if(H5Gunlink(file, GROUPNAME)<0) TEST_ERROR;

    /* Check that group's name is NULL */
    if(H5Iget_name(group, NULL, 0) != 0) TEST_ERROR;

    /* Re-link the group into the group hierarchy (shouldn't get deleted now) */
    if(H5Glink2(group, ".", H5G_LINK_HARD, file, GROUP2NAME)<0) TEST_ERROR;

    /* Close things */
    if(H5Gclose(group)<0) TEST_ERROR;
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Re-open the file */
    if((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) TEST_ERROR;

    /* Attempt to open the datatype under the new name */
    if((group=H5Gopen(file,GROUP2NAME))<0) TEST_ERROR;

    /* Close things */
    if(H5Gclose(group)<0) TEST_ERROR;
    if(H5Fclose(file)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(group);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_resurrect_group() */


/*-------------------------------------------------------------------------
 * Function:    test_unlink_chunked_dataset
 *
 * Purpose:     Tests deleting a chunked dataset
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, September 27, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink_chunked_dataset(void)
{
    hid_t fapl_id=-1;
    hid_t file_id=-1;
    hid_t dset_id=-1;
    hid_t space_id=-1;
    hid_t dcpl_id=-1;
    hsize_t dims[FILESPACE_NDIMS]={FILESPACE_DIM0,FILESPACE_DIM1,FILESPACE_DIM2};
    hsize_t max_dims[FILESPACE_NDIMS]={H5S_UNLIMITED,H5S_UNLIMITED,H5S_UNLIMITED};
    hsize_t chunk_dims[FILESPACE_NDIMS]={FILESPACE_CHUNK0,FILESPACE_CHUNK1,FILESPACE_CHUNK2};
    char filename[1024];

    TESTING("unlinking chunked dataset");

    /* Create file */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[9], fapl_id, filename, sizeof filename);

    /* Create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id))<0) TEST_ERROR;

    /* Create the dataspace */
    if((space_id=H5Screate_simple(FILESPACE_NDIMS,dims,max_dims))<0) TEST_ERROR;

    /* Create the dataset creation filter */
    if((dcpl_id=H5Pcreate(H5P_DATASET_CREATE))<0) TEST_ERROR;

    /* Set to chunked storage */
    if(H5Pset_chunk(dcpl_id,FILESPACE_NDIMS,chunk_dims)<0) TEST_ERROR;

    /* Set to early space allocation */
    if(H5Pset_alloc_time(dcpl_id,H5D_ALLOC_TIME_EARLY)<0) TEST_ERROR;

    /* Create the dataset */
    if((dset_id = H5Dcreate(file_id,DATASETNAME,H5T_NATIVE_INT,space_id,dcpl_id))<0) TEST_ERROR;

    /* Close the dataspace */
    if(H5Sclose(space_id)<0) TEST_ERROR;

    /* Close the dataset creation property list */
    if(H5Pclose(dcpl_id)<0) TEST_ERROR;

    /* Close the dataset */
    if(H5Dclose(dset_id)<0) TEST_ERROR;

    /* Close the file */
    if(H5Fclose(file_id)<0) TEST_ERROR;

    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id))<0) TEST_ERROR;

    /* Delete the dataset */
    if(H5Gunlink(file_id, DATASETNAME)<0) TEST_ERROR;

    /* Close the file */
    if(H5Fclose(file_id)<0) TEST_ERROR;

    /* Close the file access property list */
    if(H5Pclose(fapl_id)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl_id);
	H5Sclose(space_id);
	H5Dclose(dset_id);
	H5Fclose(file_id);
	H5Pclose(fapl_id);
    } H5E_END_TRY;
    return 1;
} /* end test_unlink_chunked_dataset() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test H5Gunlink()
 *
 * Return:	Success:	zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl, fapl2, file;
    int		nerrors = 0;
    char	filename[1024];

    /* Metadata cache parameters */
    int mdc_nelmts;
#ifdef H5_WANT_H5_V1_4_COMPAT
    int rdcc_nelmts;
#else /* H5_WANT_H5_V1_4_COMPAT */
    size_t rdcc_nelmts;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    size_t rdcc_nbytes;
    double rdcc_w0;

    /* Set the random # seed */
    HDsrandom((unsigned long)HDtime(NULL));

    /* Open */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Make copy of regular fapl, to turn down the elements in the metadata cache */
    if((fapl2=H5Pcopy(fapl))<0)
        goto error;

    /* Get FAPL cache settings */
    if(H5Pget_cache(fapl2,&mdc_nelmts,&rdcc_nelmts,&rdcc_nbytes,&rdcc_w0)<0)
        printf("H5Pget_cache failed\n");

    /* Change FAPL cache settings */
    mdc_nelmts=1;
    if(H5Pset_cache(fapl2,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0)<0)
        printf("H5Pset_cache failed\n");

    /* Tests */
    nerrors += test_one(file);
    nerrors += test_many(file);
    nerrors += test_symlink(file);
    nerrors += test_rename(file);
    nerrors += test_new_move();
    nerrors += check_new_move();
    nerrors += test_filespace();

    /* Test creating & unlinking lots of objects with default FAPL */
    nerrors += test_create_unlink("create and unlink large number of objects",fapl);
    /* Test creating & unlinking lots of objects with a 1-element metadata cache FAPL */
    nerrors += test_create_unlink("create and unlink large number of objects with small cache",fapl2);

    nerrors += test_link_slashes();
    nerrors += test_unlink_slashes();

    /* Test specific B-tree removal issues */
    nerrors += test_unlink_rightleaf(file);
    nerrors += test_unlink_rightnode(file);
    nerrors += test_unlink_middlenode(file);

    /* Test "resurrecting" objects */
    nerrors += test_resurrect_dataset();
    nerrors += test_resurrect_datatype();
    nerrors += test_resurrect_group();

    /* Test unlinking chunked datasets */
    nerrors += test_unlink_chunked_dataset();

    /* Close */
    if (H5Pclose(fapl2)<0) TEST_ERROR;
    if (H5Fclose(file)<0) TEST_ERROR;
    if (nerrors) {
	printf("***** %d FAILURE%s! *****\n", nerrors, 1==nerrors?"":"S");
	exit(1);
    }
    puts("All unlink tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;
 error:
    return 1;
}
