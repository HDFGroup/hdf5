/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, November 24, 1998
 */
#include <h5test.h>

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include <H5Gpkg.h>

const char *FILENAME[] = {
    "stab1",
    "stab2",
    NULL
};


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
 *
 *-------------------------------------------------------------------------
 */
static int
test_misc(hid_t fapl)
{
    hid_t	file=-1;
    hid_t	g1=-1, g2=-1, g3=-1;
    char	comment[64], filename[1024];
    
    /* Test current working groups */
    TESTING("miscellaneous group tests");

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;

    if ((g1=H5Gcreate(file, "test_1a", 0))<0) goto error;
    if ((g2=H5Gcreate(g1, "sub_1", 0))<0) goto error;
    if ((g3=H5Gcreate(file, "test_1b", 0))<0) goto error;
    if (H5Gset_comment(g3, ".", "hello world")<0) goto error;

    /* Close all groups */
    if (H5Gclose(g1)<0) goto error;
    if (H5Gclose(g2)<0) goto error;
    if (H5Gclose(g3)<0) goto error;
    
    /* Open all groups with absolute names to check for exsistence */
    if ((g1=H5Gopen(file, "/test_1a"))<0) goto error;
    if ((g2=H5Gopen(file, "/test_1a/sub_1"))<0) goto error;
    if ((g3=H5Gopen(file, "/test_1b"))<0) goto error;
    if (H5Gget_comment(g3, "././.", sizeof comment, comment)<0) goto error;
    if (strcmp(comment, "hello world")) {
	FAILED();
	puts("    Read the wrong comment string from the group.");
	printf("    got: \"%s\"\n    ans: \"hello world\"\n", comment);
	goto error;
    }

    /* Close everything */
    if (H5Gclose(g1)<0) goto error;
    if (H5Gclose(g2)<0) goto error;
    if (H5Gclose(g3)<0) goto error;
    if (H5Fclose(file)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(g1);
	H5Gclose(g2);
	H5Gclose(g3);
	H5Fclose(file);
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
 *
 *-------------------------------------------------------------------------
 */
static int
test_large(hid_t fapl)
{
    hid_t               file=-1, cwg=-1, fcpl=-1, dir=-1;
    int                 i;
    char                name[1024];
    int                 nsyms = 5000;

    TESTING("large directories");

    /*
     * Use larger symbol table data structures to be more efficient, use
     * defaults to bang harder on the library for testing.
     */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_sym_k(fcpl, 16, 16);
    h5_fixname(FILENAME[1], fapl, name, sizeof name);
    if ((file=H5Fcreate(name, H5F_ACC_TRUNC, fcpl, fapl))<0) goto error;

    /*
     * Create a directory that has so many entries that the root
     * of the B-tree ends up splitting.
     */
    if ((cwg=H5Gcreate(file, "/big", (size_t)nsyms*16+2))<0) goto error;
    for (i=0; i<nsyms; i++) {
        sprintf(name, "%05d%05d", rand()%100000, i);
	if ((dir=H5Gcreate(cwg, name, 0))<0) goto error;
        if (H5Gclose(dir)<0) goto error;
    }
    if (H5Gclose(cwg)<0) goto error;

    /* Close everything */
    if (H5Pclose(fcpl)<0) goto error;
    if (H5Fclose(file)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Pclose(fcpl);
	H5Gclose(dir);
	H5Gclose(cwg);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}


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
    hid_t	fapl;
    int		nerrors=0;

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Perform tests */
    nerrors += test_misc(fapl);
    nerrors += test_large(fapl);
    if (nerrors) goto error;

    /* Cleanup */
    puts("All symbol table tests passed.");
    h5_cleanup(fapl);
    return 0;

 error:
    puts("*** TESTS FAILED ***");
    return 1;
}

