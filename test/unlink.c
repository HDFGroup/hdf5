/*
 * Copyright (C) 1998  NCSA
 *                     All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, September 25, 1998
 *
 * Purpose:	Test H5Gunlink().
 */
#include "h5test.h"

const char *FILENAME[] = {
    "unlink",
    "new_move_a",
    "new_move_b",
    NULL
};

#define THE_OBJECT	"/foo"


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_symlink(hid_t file)
{
    hid_t	work=-1;

    TESTING("symlink removal");
    
    /* Create a test group and symlink */
    if ((work=H5Gcreate(file, "/test_symlink", 0))<0) goto error;
    if (H5Glink(work, H5G_LINK_SOFT, "link_value", "link")<0) goto error;
    if (H5Gunlink(work, "link")<0) goto error;

    /* Cleanup */
    if (H5Gclose(work)<0) goto error;
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
    hid_t 	fapl, file_a, file_b;
    hid_t	grp_1, grp_2, grp_move, moved_grp;
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
    return -1;
}


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
    hid_t	fapl, file;
    int		nerrors = 0;
    char	filename[1024];

    /* Open */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;

    /* Tests */
    nerrors += test_one(file);
    nerrors += test_many(file);
    nerrors += test_symlink(file);
    nerrors += test_rename(file);
    nerrors += test_new_move();
    nerrors += check_new_move();
 
    /* Close */
    if (H5Fclose(file)<0) goto error;
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

    
