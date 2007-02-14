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
 *              Tuesday, November 24, 1998
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include "H5Gpkg.h"

const char *FILENAME[] = {
    "stab1",
    "stab2",
    NULL
};

/* The group_new.h5 is generated from gen_new_fill.c in HDF5 'test' directory
 * for version 1.7 (after "compact group" checkin).  To get this data file,
 * simply compile gen_new_group.c with HDF5 library (after compact group
 * checkin) and run it. */
#define FILE_NEW_GROUPS "group_new.h5"


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
    name1 = malloc(namesize);
    for (i=0; i<namesize; i++)
        name1[i] = (char)('A' + i%26);
    name1[namesize-1] = '\0';
    name2 = malloc(2*namesize + 2);
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

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Gclose(g1);
        H5Gclose(g2);
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


/*-------------------------------------------------------------------------
 * Function:    read_new
 *
 * Purpose:     Test reading a file with "new style" (compact) groups
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
read_new(hid_t fapl)
{
    hid_t		fid = (-1);     /* File ID */
    hid_t		gid = (-1);     /* Group ID */
    char       *srcdir = HDgetenv("srcdir"); /*where the src code is located*/
    char       filename[512]="";  /* test file name */

    TESTING("reading new groups");

    /* Generate correct name for test file by prepending the source path */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(FILE_NEW_GROUPS) + 1) < sizeof(filename))) {
        HDstrcpy(filename, srcdir);
        HDstrcat(filename, "/");
    }
    HDstrcat(filename, FILE_NEW_GROUPS);

    /* Open file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* Attempt to open root group */
    if((gid = H5Gopen(fid, "/")) < 0) TEST_ERROR;

    /* Attempt to open new "empty" group (should fail) */
    H5E_BEGIN_TRY {
        if(H5Gopen(gid, "empty") >= 0) TEST_ERROR;
    } H5E_END_TRY;

    /* Attempt to open new group with link messages (should fail) */
    H5E_BEGIN_TRY {
        if(H5Gopen(gid, "links") >= 0) TEST_ERROR;
    } H5E_END_TRY;

    /* Close root group */
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
} /* end read_new() */


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
    nerrors += read_new(fapl);
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

