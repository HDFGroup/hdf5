/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, November 24, 1998
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */

/* Define this macro to indicate that the testing APIs should be available */
#define H5G_TESTING

#include "h5test.h"
#include "H5Gpkg.h"		/* Groups				*/
#include "H5HLprivate.h"	/* Local Heaps				*/

const char *FILENAME[] = {
    "stab1",
    "stab2",
    NULL
};

#define NAME_BUF_SIZE   1024

/* Definitions for 'lifecycle' test */
#define LIFECYCLE_TOP_GROUP     "top"
#define LIFECYCLE_BOTTOM_GROUP  "bottom %u"
#define LIFECYCLE_LOCAL_HEAP_SIZE_HINT   256
#define LIFECYCLE_MAX_COMPACT   4
#define LIFECYCLE_MIN_DENSE     3
#define LIFECYCLE_EST_NUM_ENTRIES       4
#define LIFECYCLE_EST_NAME_LEN  8

/* Definitions for 'long_compact' test */
#define LONG_COMPACT_LENGTH     ((64 * 1024) + 1024)

/* Definitions for 'read_old' test */
#define READ_OLD_NGROUPS        100
#define READ_OLD_BUFSIZE        1024

/* The group_old.h5 is generated from gen_old_fill.c in HDF5 'test' directory
 * for version 1.6.  To get this data file, simply compile gen_old_group.c with
 * the HDF5 library in that branch and run it. */
#define FILE_OLD_GROUPS "group_old.h5"

/* Definitions for 'no_compact' test */
#define NO_COMPACT_TOP_GROUP     "top"
#define NO_COMPACT_BOTTOM_GROUP  "bottom %u"
#define NO_COMPACT_MAX_COMPACT   0
#define NO_COMPACT_MIN_DENSE     0

/* Definitions for 'no_compact' test */
#define GCPL_ON_ROOT_MIDDLE_GROUP  "/middle"
#define GCPL_ON_ROOT_BOTTOM_GROUP  "/middle/bottom"
#define GCPL_ON_ROOT_MAX_COMPACT   4
#define GCPL_ON_ROOT_MIN_DENSE     2


/*-------------------------------------------------------------------------
 * Function:	test_misc
 *
 * Purpose:	Test miscellaneous group stuff.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modifications:
 *              Robb Matzke, 2002-03-28
 *              File is opened by parent instead of here.
 *-------------------------------------------------------------------------
 */
static int
test_misc(hid_t file)
{
    hid_t	g1=-1, g2=-1, g3=-1;
    char	comment[64];

    /* Test current working groups */
    TESTING("miscellaneous group tests");

    /* Create initial groups for testing, then close */
    if ((g1=H5Gcreate(file, "test_1a", 0))<0) goto error;
    if ((g2=H5Gcreate(g1, "sub_1", 0))<0) goto error;
    if ((g3=H5Gcreate(file, "test_1b", 0))<0) goto error;
    if (H5Gset_comment(g3, ".", "hello world")<0) goto error;
    if (H5Gclose(g1)<0) goto error;
    if (H5Gclose(g2)<0) goto error;
    if (H5Gclose(g3)<0) goto error;

    /* Open all groups with absolute names to check for exsistence */
    if ((g1=H5Gopen(file, "/test_1a"))<0) goto error;
    if ((g2=H5Gopen(file, "/test_1a/sub_1"))<0) goto error;
    if ((g3=H5Gopen(file, "/test_1b"))<0) goto error;
    if (H5Gget_comment(g3, "././.", sizeof comment, comment)<0) goto error;
    if (strcmp(comment, "hello world")) {
	H5_FAILED();
	puts("    Read the wrong comment string from the group.");
	printf("    got: \"%s\"\n    ans: \"hello world\"\n", comment);
	goto error;
    }
    if (H5Gclose(g1)<0) goto error;
    if (H5Gclose(g2)<0) goto error;
    if (H5Gclose(g3)<0) goto error;

    /* Check that creating groups with no-op names isn't allowed */
    H5E_BEGIN_TRY {
        g1=H5Gcreate(file, "/", 0);
    } H5E_END_TRY
    if(g1 >= 0) goto error;

    H5E_BEGIN_TRY {
        g1=H5Gcreate(file, "./././", 0);
    } H5E_END_TRY
    if(g1 >= 0) goto error;

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(g1);
	H5Gclose(g2);
	H5Gclose(g3);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Purpose:     Creates a group with a very long name
 *
 * Return:      Success:	0
 *
 * 		Failure:	number of errors
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov> 2002-03-28
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static int
test_long(hid_t file)
{
    hid_t       g1=-1, g2=-1;
    char        *name1=NULL, *name2=NULL;
    size_t      namesize=40960, i;

    TESTING("long names");

    /* Group names */
    name1 = HDmalloc(namesize);
    for (i=0; i<namesize; i++)
        name1[i] = (char)('A' + i%26);
    name1[namesize-1] = '\0';
    name2 = HDmalloc(2*namesize + 2);
    sprintf(name2, "%s/%s", name1, name1);

    /* Create groups */
    if ((g1=H5Gcreate(file, name1, 0))<0) goto error;
    if ((g2=H5Gcreate(g1, name1, 0))<0) goto error;
    H5Gclose(g1);
    H5Gclose(g2);

    /* Open groups */
    if ((g1=H5Gopen(file, name1))<0) goto error;
    if ((g2=H5Gopen(file, name2))<0) goto error;
    H5Gclose(g1);
    H5Gclose(g2);

    /* Release name buffers */
    HDfree(name2);
    HDfree(name1);

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Gclose(g1);
        H5Gclose(g2);
        if(name2)
            free(name2);
        if(name1)
            free(name1);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_large
 *
 * Purpose:     Creates a really large directory.
 *
 * Return:      Success:	0
 *
 * 		Failure:	number of errors
 *
 * Programmer:  Robb Matzke
 *              robb@maya.nuance.com
 *              Aug 29 1997
 *
 * Modifications:
 *              Robb Matzke, 2002-03-28
 *              File is opened by parent instead of here.
 *-------------------------------------------------------------------------
 */
static int
test_large(hid_t file)
{
    hid_t               cwg=-1, dir=-1;
    int                 i;
    char                name[1024];
    int                 nsyms = 5000;

    TESTING("large directories");

    /*
     * Create a directory that has so many entries that the root
     * of the B-tree ends up splitting.
     */
    if ((cwg=H5Gcreate(file, "/big", (size_t)nsyms*16+2))<0) goto error;
    for (i=0; i<nsyms; i++) {
        sprintf(name, "%05d%05d", rand()%100000, i);
#if 0
	fprintf(stderr, "%s\n", name);
#endif
	if ((dir=H5Gcreate(cwg, name, 0))<0) goto error;
        if (H5Gclose(dir)<0) goto error;
    }
    if (H5Gclose(cwg)<0) goto error;

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(dir);
	H5Gclose(cwg);
    } H5E_END_TRY;
    return 1;
}

#ifdef H5_GROUP_REVISION

/*-------------------------------------------------------------------------
 * Function:    lifecycle
 *
 * Purpose:     Test that adding links to a group follow proper "lifecycle"
 *              of empty->compact->symbol table->compact->empty.  (As group
 *              is created, links are added, then links removed)
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 17, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
lifecycle(hid_t fapl)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    hid_t	gid2 = (-1);            /* Datatype ID */
    hid_t       gcpl = (-1);            /* Group creation property list ID */
    size_t      lheap_size_hint;        /* Local heap size hint */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    unsigned	est_num_entries;	/* Estimated # of entries in group */
    unsigned	est_name_len;		/* Estimated length of entry name */
    unsigned	nmsgs;		        /* Number of messages in group's header */
    H5G_stat_t  obj_stat;               /* Object info */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename[NAME_BUF_SIZE];
    off_t       empty_size;             /* Size of an empty file */
    unsigned    u;                      /* Local index variable */
    off_t       file_size;              /* Size of each file created */

    TESTING("group lifecycle");

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR;

    /* Get size of file as empty */
    if((empty_size = h5_get_file_size(filename)) == 0) TEST_ERROR;

    /* Re-open file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Set up group creation property list */
    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR;

    /* Query default group creation property settings */
    if(H5Pget_local_heap_size_hint(gcpl, &lheap_size_hint) < 0) TEST_ERROR;
    if(lheap_size_hint != H5G_CRT_GINFO_LHEAP_SIZE_HINT) TEST_ERROR;
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR;
    if(max_compact != H5G_CRT_GINFO_MAX_COMPACT) TEST_ERROR;
    if(min_dense != H5G_CRT_GINFO_MIN_DENSE) TEST_ERROR;
    if(H5Pget_est_link_info(gcpl, &est_num_entries, &est_name_len) < 0) TEST_ERROR;
    if(est_num_entries != H5G_CRT_GINFO_EST_NUM_ENTRIES) TEST_ERROR;
    if(est_name_len != H5G_CRT_GINFO_EST_NAME_LEN) TEST_ERROR;

    /* Set GCPL parameters */
    if(H5Pset_local_heap_size_hint(gcpl, LIFECYCLE_LOCAL_HEAP_SIZE_HINT) < 0) TEST_ERROR;
    if(H5Pset_link_phase_change(gcpl, LIFECYCLE_MAX_COMPACT, LIFECYCLE_MIN_DENSE) < 0) TEST_ERROR;
    if(H5Pset_est_link_info(gcpl, LIFECYCLE_EST_NUM_ENTRIES, LIFECYCLE_EST_NAME_LEN) < 0) TEST_ERROR;

    /* Create group for testing lifecycle */
    if((gid = H5Gcreate_expand(fid, LIFECYCLE_TOP_GROUP, gcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Query group creation property settings */
    if(H5Pget_local_heap_size_hint(gcpl, &lheap_size_hint) < 0) TEST_ERROR;
    if(lheap_size_hint != LIFECYCLE_LOCAL_HEAP_SIZE_HINT) TEST_ERROR;
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR;
    if(max_compact != LIFECYCLE_MAX_COMPACT) TEST_ERROR;
    if(min_dense != LIFECYCLE_MIN_DENSE) TEST_ERROR;
    if(H5Pget_est_link_info(gcpl, &est_num_entries, &est_name_len) < 0) TEST_ERROR;
    if(est_num_entries != LIFECYCLE_EST_NUM_ENTRIES) TEST_ERROR;
    if(est_name_len != LIFECYCLE_EST_NAME_LEN) TEST_ERROR;

    /* Use internal testing routine to check that the group has no links or symbol table */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR;

    /* Create first "bottom" group */
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, (unsigned)0);
    if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR;

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR;
    if(nmsgs != 1) TEST_ERROR;

    /* Create several more bottom groups, to push the top group almost to a symbol table */
    /* (Start counting at '1', since we've already created one bottom group */
    for(u = 1; u < LIFECYCLE_MAX_COMPACT; u++) {
        sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
        if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR

        /* Check on bottom group's status */
        if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR;

        /* Close bottom group */
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR;
    if(nmsgs != LIFECYCLE_MAX_COMPACT) TEST_ERROR;

    /* Check that the object header is only one chunk and the space has been allocated correctly */
    if(H5Gget_objinfo(gid, ".", FALSE, &obj_stat) < 0) TEST_ERROR;
#ifdef H5_HAVE_LARGE_HSIZET
    if(obj_stat.ohdr.size != 232) TEST_ERROR;
#else /* H5_HAVE_LARGE_HSIZET */
    if(obj_stat.ohdr.size != 224) TEST_ERROR;
#endif /* H5_HAVE_LARGE_HSIZET */
    if(obj_stat.ohdr.free != 0) TEST_ERROR;
    if(obj_stat.ohdr.nmesgs != 6) TEST_ERROR;
    if(obj_stat.ohdr.nchunks != 1) TEST_ERROR;

    /* Create one more "bottom" group, which should push top group into using a symbol table */
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
    if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR;

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR;
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR;
    if(H5G_lheap_size_test(gid, &lheap_size_hint) < 0) TEST_ERROR;
    if(lheap_size_hint != LIFECYCLE_LOCAL_HEAP_SIZE_HINT) TEST_ERROR;

    /* Check that the object header is still one chunk and the space has been allocated correctly */
    if(H5Gget_objinfo(gid, ".", FALSE, &obj_stat) < 0) TEST_ERROR;
#ifdef H5_HAVE_LARGE_HSIZET
    if(obj_stat.ohdr.size != 232) TEST_ERROR;
#else /* H5_HAVE_LARGE_HSIZET */
    if(obj_stat.ohdr.size != 224) TEST_ERROR;
#endif /* H5_HAVE_LARGE_HSIZET */
    if(obj_stat.ohdr.free != 136) TEST_ERROR;
    if(obj_stat.ohdr.nmesgs != 4) TEST_ERROR;
    if(obj_stat.ohdr.nchunks != 1) TEST_ERROR;

    /* Unlink objects from top group */
    while(u >= LIFECYCLE_MIN_DENSE) {
        sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);

        if(H5Gunlink(gid, objname) < 0) TEST_ERROR

        u--;
    } /* end while */

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR;
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR;

    /* Unlink one more object from the group, which should transform back to using links */
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
    if(H5Gunlink(gid, objname) < 0) TEST_ERROR
    u--;

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR;
    if(nmsgs != (LIFECYCLE_MIN_DENSE - 1)) TEST_ERROR;

    /* Unlink last two objects from top group */
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
    if(H5Gunlink(gid, objname) < 0) TEST_ERROR
    u--;
    sprintf(objname, LIFECYCLE_BOTTOM_GROUP, u);
    if(H5Gunlink(gid, objname) < 0) TEST_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR;

    /* Close top group */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Unlink top group */
    if(H5Gunlink(fid, LIFECYCLE_TOP_GROUP) < 0) TEST_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR;

    /* Get size of file as empty */
    if((file_size = h5_get_file_size(filename)) == 0) TEST_ERROR;

    /* Verify that file is correct size */
    if(file_size != empty_size) TEST_ERROR;


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gcpl);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end lifecycle() */


/*-------------------------------------------------------------------------
 * Function:    long_compact
 *
 * Purpose:     Test that long links are correctly _not_ put into compact
 *              form.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 18, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
long_compact(hid_t fapl)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    hid_t	gid2 = (-1);            /* Group ID */
    char        *objname;               /* Object name */
    char	filename[NAME_BUF_SIZE];
    off_t       empty_size;             /* Size of an empty file */
    off_t       file_size;              /* Size of each file created */

    TESTING("long link names in compact groups");

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Get size of file as empty */
    if((empty_size = h5_get_file_size(filename)) == 0) TEST_ERROR

    /* Construct very long object name template */
    if((objname = HDmalloc(LONG_COMPACT_LENGTH+1)) == NULL) TEST_ERROR
    HDmemset(objname, 'a', LONG_COMPACT_LENGTH);
    objname[LONG_COMPACT_LENGTH] = '\0';

    /* Re-open file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Create top group */
    if((gid = H5Gcreate(fid, "top", (size_t)0)) < 0) TEST_ERROR

    /* Use internal testing routine to check that the group has no links or symbol table */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Create first group with "long" name */
    if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    /* (Should have symbol table to hold links, since name is too long for object header message) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Create second group with "long" name */
    objname[0] = 'b';
    if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR;

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    /* (Should have symbol table to hold links, since name is too long for object header message) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Unlink second object from top group */
    if(H5Gunlink(gid, objname) < 0) TEST_ERROR

    /* Check on top group's status */
    /* (Should still be symbol table to hold links, since name is too long for object header message) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Unlink first object from top group */
    objname[0] = 'a';
    if(H5Gunlink(gid, objname) < 0) TEST_ERROR

    /* Check on top group's status */
    /* (Should have deleted the symbol table now) */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR;

    /* Free object name */
    HDfree(objname);

    /* Close top group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Unlink top group */
    if(H5Gunlink(fid, "top") < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Get size of file as empty */
    if((file_size = h5_get_file_size(filename)) == 0) TEST_ERROR;

    /* Verify that file is correct size */
    if(file_size != empty_size) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end long_compact() */


/*-------------------------------------------------------------------------
 * Function:    read_old
 *
 * Purpose:     Test reading a file with "old style" (symbol table) groups
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 24, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
read_old(hid_t fapl)
{
    int	fd_old = (-1), fd_new = (-1);   /* File descriptors for copying data */
    hid_t fid = (-1);                   /* File ID */
    hid_t gid = (-1);                   /* Group ID */
    hid_t gid2 = (-1);                  /* Group ID */
    char  buf[READ_OLD_BUFSIZE];        /* Buffer for copying data */
    ssize_t nread;                      /* Number of bytes read in */
    char  objname[NAME_BUF_SIZE];       /* Object name */
    unsigned    u;                      /* Local index variable */
    char  *srcdir = HDgetenv("srcdir"); /* where the src code is located */
    char  filename[512]="";             /* old test file name */
    char  filename2[NAME_BUF_SIZE];     /* copy of old test file */

    TESTING("reading old groups");

    /* Generate correct name for test file by prepending the source path */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(FILE_OLD_GROUPS) + 1) < sizeof(filename))) {
        HDstrcpy(filename, srcdir);
        HDstrcat(filename, "/");
    }
    HDstrcat(filename, FILE_OLD_GROUPS);

    /* Create filename */
    h5_fixname(FILENAME[1], fapl, filename2, sizeof(filename2));

    /* Copy old file into temporary file */
    if((fd_old = HDopen(filename, O_RDONLY, 0666)) < 0) TEST_ERROR
    if((fd_new = HDopen(filename2, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0) TEST_ERROR

    /* Copy data */
    while((nread = HDread(fd_old, buf, (size_t)READ_OLD_BUFSIZE)) > 0)
        HDwrite(fd_new, buf, (size_t)nread);

    /* Close files */
    if(HDclose(fd_old) < 0) TEST_ERROR
    if(HDclose(fd_new) < 0) TEST_ERROR


    /* Open copied file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Attempt to open "old" group */
    if((gid = H5Gopen(fid, "old")) < 0) TEST_ERROR

    /* Check on old group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Create a bunch of objects in the group */
    for(u = 0; u < READ_OLD_NGROUPS; u++) {
        sprintf(objname, "Group %u", u);
        if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR

        /* Check on bottom group's status */
        if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR

        /* Close bottom group */
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on old group's status */
    /* (Should stay in old "symbol table" form) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Delete new objects from old group */
    for(u = 0; u < READ_OLD_NGROUPS; u++) {
        sprintf(objname, "Group %u", u);
        if(H5Gunlink(gid, objname) < 0) TEST_ERROR
    } /* end for */

    /* Check on old group's status */
    /* (Should stay in old "symbol table" form) */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR

    /* Close old group */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end read_old() */


/*-------------------------------------------------------------------------
 * Function:    no_compact
 *
 * Purpose:     Test that its possible to create groups that don't use the
 *              compact form directly (and don't use link messages).
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
no_compact(hid_t fapl)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    hid_t	gid2 = (-1);            /* Datatype ID */
    hid_t       gcpl = (-1);            /* Group creation property list ID */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename[NAME_BUF_SIZE];
    off_t       empty_size;             /* Size of an empty file */
    off_t       file_size;              /* Size of each file created */
    size_t      lheap_size_hint;        /* Local heap size */
    size_t      def_lheap_size;         /* Default local heap size */
    unsigned	est_num_entries;	/* Estimated # of entries in group */
    unsigned	est_name_len;		/* Estimated length of entry name */

    TESTING("group without compact form");

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR;

    /* Get size of file as empty */
    if((empty_size = h5_get_file_size(filename)) == 0) TEST_ERROR;

    /* Re-open file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Set up group creation property list */
    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR;

    /* Set GCPL parameters */
    if(H5Pset_link_phase_change(gcpl, NO_COMPACT_MAX_COMPACT, NO_COMPACT_MIN_DENSE) < 0) TEST_ERROR;

    /* Check information for default local heap creation */
    if(H5Pget_local_heap_size_hint(gcpl, &lheap_size_hint) < 0) TEST_ERROR;
    if(lheap_size_hint != H5G_CRT_GINFO_LHEAP_SIZE_HINT) TEST_ERROR;
    if(H5Pget_est_link_info(gcpl, &est_num_entries, &est_name_len) < 0) TEST_ERROR;
    if(est_num_entries != H5G_CRT_GINFO_EST_NUM_ENTRIES) TEST_ERROR;
    if(est_name_len != H5G_CRT_GINFO_EST_NAME_LEN) TEST_ERROR;

    /* Create group for testing lifecycle */
    if((gid = H5Gcreate_expand(fid, NO_COMPACT_TOP_GROUP, gcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR;

    /* Use internal testing routine to check that the group has no links or symbol table */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR;

    /* Create first "bottom" group */
    sprintf(objname, NO_COMPACT_BOTTOM_GROUP, (unsigned)0);
    if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR

    /* Check on bottom group's status */
    if(H5G_is_empty_test(gid2) != TRUE) TEST_ERROR;

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR;
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR;
    if(H5G_lheap_size_test(gid, &lheap_size_hint) < 0) TEST_ERROR;
    def_lheap_size = est_num_entries * (est_name_len + 1);
    def_lheap_size = H5HL_ALIGN(def_lheap_size);
    if(lheap_size_hint != def_lheap_size) TEST_ERROR;

    /* Unlink object from top group */
    sprintf(objname, NO_COMPACT_BOTTOM_GROUP, (unsigned)0);
    if(H5Gunlink(gid, objname) < 0) TEST_ERROR

    /* Check on top group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR;

    /* Close top group */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Unlink top group */
    if(H5Gunlink(fid, NO_COMPACT_TOP_GROUP) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR;

    /* Get size of file as empty */
    if((file_size = h5_get_file_size(filename)) == 0) TEST_ERROR;

    /* Verify that file is correct size */
    if(file_size != empty_size) TEST_ERROR;


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gcpl);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end no_compact() */


/*-------------------------------------------------------------------------
 * Function:    gcpl_on_root
 *
 * Purpose:     Test setting group creation properties for root group.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
gcpl_on_root(hid_t fapl)
{
    hid_t	fid = (-1);             /* File ID */
    hid_t	gid = (-1);             /* Group ID */
    hid_t	gid2 = (-1);            /* Datatype ID */
    hid_t       fcpl = (-1);            /* File creation property list ID */
    hid_t       gcpl = (-1);            /* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    char	filename[NAME_BUF_SIZE];

    TESTING("setting root group creation properties");

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));

    /* Set up file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR;

    /* Set GCPL parameters */
    if(H5Pset_link_phase_change(fcpl, GCPL_ON_ROOT_MAX_COMPACT, GCPL_ON_ROOT_MIN_DENSE) < 0) TEST_ERROR;

    /* Query the group creation properties from the FCPL */
    if(H5Pget_link_phase_change(fcpl, &max_compact, &min_dense) < 0) TEST_ERROR;
    if(max_compact != GCPL_ON_ROOT_MAX_COMPACT) TEST_ERROR;
    if(min_dense != GCPL_ON_ROOT_MIN_DENSE) TEST_ERROR;

    /* Create file with modified root group creation properties */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) TEST_ERROR;

    /* Close FCPL */
    if(H5Pclose(fcpl) < 0) TEST_ERROR;

    /* Open the root group */
    if((gid = H5Gopen(fid, "/")) < 0) TEST_ERROR;

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid)) < 0) TEST_ERROR;
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR;
    if(max_compact != GCPL_ON_ROOT_MAX_COMPACT) TEST_ERROR;
    if(min_dense != GCPL_ON_ROOT_MIN_DENSE) TEST_ERROR;

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR;

    /* Create a group creation property list, with intermediate group creation set */
    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR;
    if(H5Pset_create_intermediate_group(gcpl, TRUE) < 0) TEST_ERROR

    /* Create a group and intermediate groups, to check if root group settings are inherited */
    if((gid2 = H5Gcreate_expand(gid, GCPL_ON_ROOT_BOTTOM_GROUP, gcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR;

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid2)) < 0) TEST_ERROR;
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR;
    if(max_compact != H5G_CRT_GINFO_MAX_COMPACT) TEST_ERROR;
    if(min_dense != H5G_CRT_GINFO_MIN_DENSE) TEST_ERROR;

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR;

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR;

    /* Open the middle group */
    if((gid2 = H5Gopen(fid, GCPL_ON_ROOT_MIDDLE_GROUP)) < 0) TEST_ERROR;

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid2)) < 0) TEST_ERROR;
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR;
    if(max_compact != GCPL_ON_ROOT_MAX_COMPACT) TEST_ERROR;
    if(min_dense != GCPL_ON_ROOT_MIN_DENSE) TEST_ERROR;

    /* Close GCPL */
    if(H5Pclose(gcpl) < 0) TEST_ERROR;

    /* Close bottom group */
    if(H5Gclose(gid2) < 0) TEST_ERROR;

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR;


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gcpl);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Gclose(fcpl);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end gcpl_on_root() */
#endif /* H5_GROUP_REVISION */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test groups
 *
 * Return:	Success:	zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl, fcpl, file;
    int		nerrors=0;
    char        filename[1024];

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /*
     * Use larger symbol table data structures to be more efficient, use
     * defaults to bang harder on the library for testing.
     */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
#if 0
    H5Pset_sym_k(fcpl, 16, 16);
#endif

    /* Open the file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl))<0)
	goto error;

    /* Perform tests */
    nerrors += test_misc(file);
    nerrors += test_long(file);
    nerrors += test_large(file);
#ifdef H5_GROUP_REVISION
    nerrors += lifecycle(fapl);
    nerrors += long_compact(fapl);
    nerrors += read_old(fapl);
    nerrors += no_compact(fapl);
    nerrors += gcpl_on_root(fapl);
#endif /* H5_GROUP_REVISION */
    if (nerrors) goto error;

    /* Cleanup */
    H5Fclose(file);
    puts("All symbol table tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    puts("*** TESTS FAILED ***");
    return 1;
}

