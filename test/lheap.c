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
 *
 * Purpose:	Test local heaps used by symbol tables (groups).
 */
#include "h5test.h"
#include "H5HLprivate.h"
#include "H5Iprivate.h"

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
    hid_t	fapl=H5P_DEFAULT;	/*file access properties	*/
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
	H5_FAILED();
	H5Eprint(stdout);
	goto error;
    }
    if (H5HL_create(f, H5P_DATASET_XFER_DEFAULT, 0, &heap_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint(stdout);
	goto error;
    }
    for (i = 0; i < NOBJS; i++) {
        sprintf(buf, "%03d-", i);
        for (j=4; j<i; j++) buf[j] = '0' + j%10;
        if (j>4) buf[j] = '\0';

        if ((size_t)(-1)==(obj[i]=H5HL_insert(f, H5P_DATASET_XFER_DEFAULT, heap_addr, strlen(buf)+1,
					      buf))) {
	    H5_FAILED();
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
	H5_FAILED();
	H5Eprint(stdout);
	goto error;
    }
    for (i=0; i<NOBJS; i++) {
        const H5HL_t *heap = NULL;

	sprintf(buf, "%03d-", i);
        for (j=4; j<i; j++) buf[j] = '0' + j%10;
        if (j>4) buf[j] = '\0';

        if (NULL == (heap = H5HL_protect(f, H5P_DATASET_XFER_DEFAULT, heap_addr))) {
	    H5_FAILED();
	    H5Eprint(stdout);
	    goto error;
	}

        if (NULL == (s = H5HL_offset_into(f, heap, obj[i]))) {
	    H5_FAILED();
	H5Eprint(stdout);
	    goto error;
	}

	if (strcmp(s, buf)) {
	    H5_FAILED();
	    printf("    i=%d, heap offset=%lu\n", i, (unsigned long)(obj[i]));
	    printf("    got: \"%s\"\n", s);
	    printf("    ans: \"%s\"\n", buf);
	    goto error;
	}

        if (H5HL_unprotect(f, H5P_DATASET_XFER_DEFAULT, heap, heap_addr) < 0) {
	    H5_FAILED();
	    H5Eprint(stdout);
	    goto error;
	}
    }
    if (H5Fclose(file)<0) goto error;
    PASSED();

    puts("All local heap tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}
