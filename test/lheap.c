/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, November 24, 1998
 *
 * Purpose:	Test local heaps used by symbol tables (groups).
 */
#include <h5test.h>
#include <H5HLprivate.h>
#include <H5Iprivate.h>

const char *FILENAME[] = {
    "lheap",
    NULL
};

#define NOBJS   40


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Create a file, create a local heap, write data into the local
 *		heap, close the file, open the file, read data out of the
 *		local heap, close the file.
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
    hid_t	fapl=-1;		/*file access properties	*/
    hid_t	file=-1;		/*hdf5 file 			*/
    H5F_t	*f=NULL;		/*hdf5 file pointer		*/
    char	filename[1024];		/*file name			*/
    haddr_t	heap_addr;		/*local heap address		*/
    size_t	obj[NOBJS];		/*offsets within the heap	*/
    int		i, j;			/*miscellaneous counters	*/
    char	buf[1024];		/*the value to store		*/
    const char	*s;			/*value to read			*/

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /*
     * Test writing to the heap...
     */
    TESTING("local heap write");
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if (NULL==(f=H5I_object(file))) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5HL_create(f, 0, &heap_addr/*out*/)<0) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    for (i = 0; i < NOBJS; i++) {
        sprintf(buf, "%03d-", i);
        for (j=4; j<i; j++) buf[j] = '0' + j%10;
        if (j>4) buf[j] = '\0';

        if ((size_t)(-1)==(obj[i]=H5HL_insert(f, &heap_addr, strlen(buf)+1,
					      buf))) {
	    FAILED();
	    H5Eprint(stdout);
	    goto error;
	}
    }
    if (H5Fclose(file)<0) goto error;
    PASSED();

    /*
     * Test reading from the heap...
     */
    TESTING("local heap read");
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) goto error;
    if (NULL==(f=H5I_object(file))) {
	FAILED();
	H5Eprint(stdout);
	goto error;
    }
    for (i=0; i<NOBJS; i++) {
	sprintf(buf, "%03d-", i);
        for (j=4; j<i; j++) buf[j] = '0' + j%10;
        if (j>4) buf[j] = '\0';
        if (NULL==(s=H5HL_peek(f, &heap_addr, obj[i]))) {
	    FAILED();
	    H5Eprint(stdout);
	    goto error;
	}
	if (strcmp(s, buf)) {
	    FAILED();
	    printf("    i=%d, heap offset=%lu\n", i, (unsigned long)(obj[i]));
	    printf("    got: \"%s\"\n", s);
	    printf("    ans: \"%s\"\n", buf);
	    goto error;
	}
    }
    if (H5Fclose(file)<0) goto error;
    PASSED();


    puts("All local heap tests passed.");
    h5_cleanup(fapl);
    return 0;

 error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}
