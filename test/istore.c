/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Wednesday, October 15, 1997
 *
 * Purpose:	Tests various aspects of indexed raw data storage.
 */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

#include <h5test.h>
#include <H5private.h>
#include <H5Dprivate.h>
#include <H5Iprivate.h>
#include <H5Pprivate.h>
#include <H5Fpkg.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Vprivate.h>

const char *FILENAME[] = {
    "istore",
    NULL
};


#define TEST_SMALL	0x0001
#define TEST_MEDIUM	0x0002
#define TEST_LARGE	0x0004

hsize_t align_g[3] = {50, 50, 50};
hssize_t zero[H5O_LAYOUT_NDIMS];


/*-------------------------------------------------------------------------
 * Function:	print_array
 *
 * Purpose:	Prints the values in an array
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_array(uint8_t *array, size_t nx, size_t ny, size_t nz)
{
    size_t	i, j, k;

    for (i = 0; i < nx; i++) {
	if (nz > 1) {
	    printf("i=%lu:\n", (unsigned long)i);
	} else {
	    printf("%03lu:", (unsigned long)i);
	}

	for (j = 0; j < ny; j++) {
	    if (nz > 1)
		printf("%03lu:", (unsigned long)j);
	    for (k = 0; k < nz; k++) {
		printf(" %3d", *array++);
	    }
	    if (nz > 1)
		printf("\n");
	}
	printf("\n");
    }
}


/*-------------------------------------------------------------------------
 * Function:	new_object
 *
 * Purpose:	Creates a new object that refers to a indexed storage of raw
 *		data.  No raw data is stored.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
new_object(H5F_t *f, const char *name, uintn ndims, H5G_entry_t *ent/*out*/)
{
    H5O_layout_t	    layout;
    uintn		    u;

    /* Create the object header */
    if (H5O_create(f, 64, ent)) {
	FAILED();
	puts("    H5O_create() = NULL");
	goto error;
    }

    /* create chunked storage */
    layout.type = H5D_CHUNKED;
    layout.ndims = ndims;
    for (u = 0; u < ndims; u++) {
	if (u < (int)NELMTS(align_g)) {
	    layout.dim[u] = align_g[u];
	} else {
	    layout.dim[u] = 2;
	}
    }
    H5F_arr_create(f, &layout/*in,out*/);
    if (H5O_modify(ent, H5O_LAYOUT, H5O_NEW_MESG, 0, &layout) < 0) {
	FAILED();
	puts("    H5O_modify istore message failure.");
	goto error;
    }

    /* Give the object header a name */
    if (H5G_insert(H5G_entof(H5G_rootof(f)), name, ent) < 0) {
	FAILED();
	printf("    H5G_insert(f, name=\"%s\", ent) failed\n", name);
	goto error;
    }

    /* Close the header */
    H5O_close(ent);
    return 0;

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Creates a named object that refers to indexed storage of raw
 *		data.  No raw data is stored.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create(H5F_t *f, const char *prefix)
{
    H5G_entry_t		    handle;
    uintn		    u;
    char		    name[256];

    TESTING("istore create");

    for (u = 1; u <= H5O_LAYOUT_NDIMS; u++) {
	HDsnprintf(name, sizeof name, "%s_%02u", prefix, u);
	if (new_object(f, name, u, &handle) < 0)
	    return FAIL;
    }

    PASSED();
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	test_extend
 *
 * Purpose:	Creates an empty object and then writes to it in such a way
 *		as to always extend the object's domain without creating
 *		holes and without causing the object to become concave.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_extend(H5F_t *f, const char *prefix,
	    size_t nx, size_t ny, size_t nz)
{
    H5G_entry_t		handle;
    hsize_t		i, j, k, ctr;
    uintn			ndims;
    uint8_t		*buf = NULL, *check = NULL, *whole = NULL;
    char		dims[64], s[256], name[256];
    hssize_t		offset[3];
    hssize_t		max_corner[3];
    hsize_t		size[3];
    hsize_t		whole_size[3];
    hsize_t		nelmts;
    H5O_layout_t	layout;

    if (!nz) {
	if (!ny) {
	    ndims = 1;
	    ny = nz = 1;
	    sprintf(dims, "%lu", (unsigned long) nx);
	} else {
	    ndims = 2;
	    nz = 1;
	    sprintf(dims, "%lux%lu", (unsigned long) nx, (unsigned long) ny);
	}
    } else {
	ndims = 3;
	sprintf(dims, "%lux%lux%lu",
		(unsigned long) nx, (unsigned long) ny, (unsigned long) nz);
    }

    sprintf(s, "Testing istore extend: %s", dims);
    printf("%-70s", s);
    buf = H5MM_malloc(nx * ny * nz);
    check = H5MM_malloc(nx * ny * nz);
    whole = H5MM_calloc(nx*ny*nz);

    /* Build the new empty object */
    sprintf(name, "%s_%s", prefix, dims);
    if (new_object(f, name, ndims, &handle) < 0) {
	printf("    Cannot create %u-d object `%s'\n", ndims, name);
	goto error;
    }
    if (NULL == H5O_read(&handle, H5O_LAYOUT, 0, &layout)) {
	FAILED();
	puts("    Unable to read istore message.");
	goto error;
    }
    if (ndims != layout.ndims) {
	FAILED();
	printf("    Header read error: istore.ndims != %d\n", ndims);
	goto error;
    }
    whole_size[0] = nx;
    whole_size[1] = ny;
    whole_size[2] = nz;
    max_corner[0] = 0;
    max_corner[1] = 0;
    max_corner[2] = 0;

    for (ctr = 0;
	 H5V_vector_lt_s(ndims, max_corner, (hssize_t*)whole_size);
	 ctr++) {

	/* Size and location */
	if (0 == ctr) {
	    offset[0] = offset[1] = offset[2] = 0;
	    size[0] = size[1] = size[2] = 1;
	    nelmts = 1;
	} else {
	    for (i=0, nelmts=1; i<(size_t)ndims; i++) {
		if (ctr % ndims == i) {
		    offset[i] = max_corner[i];
		    size[i] = MIN(1, whole_size[i] - offset[i]);
		} else {
		    offset[i] = 0;
		    size[i] = max_corner[i];
		}
		nelmts *= size[i];
	    }
	}

#if 0
	if (0 == ctr)
	    printf("\n");
	printf("    Insert: ctr=%d, corner=(%d", ctr, offset[0]);
	if (ndims > 1)
	    printf(",%d", offset[1]);
	if (ndims > 2)
	    printf(",%d", offset[2]);
	printf("), size=(%d", size[0]);
	if (ndims > 1)
	    printf(",%d", size[1]);
	if (ndims > 2)
	    printf(",%d", size[2]);
	printf("), %d element%s", nelmts, 1 == nelmts ? "" : "s");
	if (0 == nelmts)
	    printf(" *SKIPPED*");
	printf("\n");
#endif

	/* Fill the source array */
	if (0 == nelmts) continue;
	memset(buf, (signed)(128+ctr), (size_t)nelmts);

	/* Write to disk */
	if (H5F_arr_write(f, H5P_DEFAULT, &layout, NULL, NULL, NULL, size,
			  size, zero, offset, buf)<0) {
	    FAILED();
	    printf("    Write failed: ctr=%lu\n", (unsigned long)ctr);
	    goto error;
	}

	/* Read from disk */
	memset(check, 0xff, (size_t)nelmts);
	if (H5F_arr_read(f, H5P_DEFAULT, &layout, NULL, NULL, NULL, size,
			 size, zero, offset, check)<0) {
	    FAILED();
	    printf("    Read failed: ctr=%lu\n", (unsigned long)ctr);
	    goto error;
	}
	if (memcmp(buf, check, (size_t)nelmts)) {
	    FAILED();
	    printf("    Read check failed: ctr=%lu\n", (unsigned long)ctr);
	    printf("    Wrote:\n");
	    print_array(buf, (size_t)size[0], (size_t)size[1],
			(size_t)size[2]);
	    printf("    Read:\n");
	    print_array(check, (size_t)size[0], (size_t)size[1],
			(size_t)size[2]);
	    goto error;
	}

	/* Write to `whole' buffer for later checking */
	H5V_hyper_copy(ndims, size,
		       whole_size, offset, whole,	/*dst*/
		       size, H5V_ZERO, buf);		/*src*/

	/* Update max corner */
	for (i=0; i<(size_t)ndims; i++) {
	    max_corner[i] = MAX(max_corner[i], offset[i]+(hssize_t)size[i]);
	}
    }

    /* Now read the entire array back out and check it */
    memset(buf, 0xff, nx * ny * nz);
    if (H5F_arr_read(f, H5P_DEFAULT, &layout, NULL, NULL, NULL, whole_size,
		     whole_size, zero, zero, buf)<0) {
	FAILED();
	puts("    Read failed for whole array.");
	goto error;
    }
    for (i=0; i<nx; i++) {
	for (j=0; j<ny; j++) {
	    for (k=0; k<nz; k++) {
		if (whole[i*ny*nz + j*nz + k] != buf[i*ny*nz + j*nz + k]) {
		    FAILED();
		    printf("    Check failed at i=%lu", (unsigned long)i);
		    if (ndims > 1) {
			printf(", j=%lu", (unsigned long)j);
		    }
		    if (ndims > 2) {
			printf(", k=%lu", (unsigned long)k);
		    }
		    printf("\n    Check array is:\n");
		    print_array(whole, nx, ny, nz);
		    printf("    Value read is:\n");
		    print_array(buf, nx, ny, nz);
		    goto error;
		}
	    }
	}
    }

    H5MM_xfree(buf);
    H5MM_xfree(check);
    H5MM_xfree(whole);
    PASSED();
    return SUCCEED;

  error:
    H5MM_xfree(buf);
    H5MM_xfree(check);
    H5MM_xfree(whole);
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	test_sparse
 *
 * Purpose:	Creates a sparse matrix consisting of NBLOCKS randomly placed
 *		blocks each of size NX,NY,NZ.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sparse(H5F_t *f, const char *prefix, size_t nblocks,
	    size_t nx, size_t ny, size_t nz)
{
    uintn		ndims;
    hsize_t		ctr;
    char		dims[64], s[256], name[256];
    hssize_t		offset[3];
    hsize_t		size[3], total = 0;
    H5G_entry_t		handle;
    H5O_layout_t	layout;
    uint8_t		*buf = NULL;

    if (!nz) {
	if (!ny) {
	    ndims = 1;
	    ny = nz = 1;
	    sprintf(dims, "%lu", (unsigned long) nx);
	} else {
	    ndims = 2;
	    nz = 1;
	    sprintf(dims, "%lux%lu", (unsigned long) nx, (unsigned long) ny);
	}
    } else {
	ndims = 3;
	sprintf(dims, "%lux%lux%lu",
		(unsigned long) nx, (unsigned long) ny, (unsigned long) nz);
    }

    sprintf(s, "Testing istore sparse: %s", dims);
    printf("%-70s", s);
    buf = H5MM_malloc(nx * ny * nz);

    /* Build the new empty object */
    sprintf(name, "%s_%s", prefix, dims);
    if (new_object(f, name, ndims, &handle) < 0) {
	printf("    Cannot create %u-d object `%s'\n", ndims, name);
	goto error;
    }
    if (NULL == H5O_read(&handle, H5O_LAYOUT, 0, &layout)) {
	FAILED();
	printf("    Unable to read istore message\n");
	goto error;
    }
    for (ctr=0; ctr<nblocks; ctr++) {
	offset[0] = rand() % 1000000;
	offset[1] = rand() % 1000000;
	offset[2] = rand() % 1000000;
	size[0] = nx;
	size[1] = ny;
	size[2] = nz;
	memset(buf, (signed)(128+ctr), nx * ny * nz);

	/* write to disk */
	if (H5F_arr_write(f, H5P_DEFAULT, &layout, NULL, NULL, NULL, size,
			  size, zero, offset, buf)<0) {
	    FAILED();
	    printf("    Write failed: ctr=%lu\n", (unsigned long)ctr);
	    printf("    offset=(%lu", (unsigned long) (offset[0]));
	    if (ndims > 1)
		printf(",%lu", (unsigned long) (offset[1]));
	    if (ndims > 2)
		printf(",%lu", (unsigned long) (offset[2]));
	    printf("), size=(%lu", (unsigned long) (size[0]));
	    if (ndims > 1)
		printf(",%lu", (unsigned long) (size[1]));
	    if (ndims > 2)
		printf(",%lu", (unsigned long) (size[2]));
	    printf(")\n");
	    goto error;
	}
	total += nx * ny * nz;
#if 0
	printf("ctr: ctr=%d, total=%lu\n", ctr, (unsigned long) total);
#endif

	/* We don't test reading yet.... */
    }

    H5MM_xfree(buf);
    PASSED();
    return SUCCEED;

  error:
    H5MM_xfree(buf);
    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests indexed storage stuff.
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t		fapl=-1, file=-1, fcpl=-1;
    H5F_t		*f;
    herr_t		status;
    int			nerrors = 0;
    uintn		size_of_test;
    char		filename[1024];

    /* Parse arguments or assume `small' */
    if (1 == argc) {
	size_of_test = TEST_SMALL;
    } else {
	intn			i;
	for (i = 1, size_of_test = 0; i < argc; i++) {
	    if (!strcmp(argv[i], "small")) {
		size_of_test |= TEST_SMALL;
	    } else if (!strcmp(argv[i], "medium")) {
		size_of_test |= TEST_MEDIUM;
	    } else if (!strcmp(argv[i], "large")) {
		size_of_test |= TEST_LARGE;
	    } else {
		printf("unrecognized argument: %s\n", argv[i]);
#if 0
		exit(1);
#endif
	    }
	}
    }
    printf("Test sizes: ");
    if (size_of_test & TEST_SMALL)
	printf(" SMALL");
    if (size_of_test & TEST_MEDIUM)
	printf(" MEDIUM");
    if (size_of_test & TEST_LARGE)
	printf(" LARGE");
    printf("\n");

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Use larger file addresses... */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_sizes(fcpl, 8, 0);

    /* Create the test file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl))<0 ||
	NULL==(f=H5I_object(file))) {
	printf("Cannot create file %s; test aborted\n", filename);
	exit(1);
    }
    
    /*
     * For testing file families, fool the library into thinking it already
     * allocated a whole bunch of data.
     */
#ifdef H5_WANT_H5_V1_2_COMPAT
    if (H5F_LOW_FAMILY==H5Pget_driver(fapl)) {
#else /* H5_WANT_H5_V1_2_COMPAT */
    if (H5FD_FAMILY==H5Pget_driver(fapl)) {
#endif /* H5_WANT_H5_V1_2_COMPAT */
	haddr_t addr;
	addr = 8 * ((uint64_t)1<<30);	/*8 GB */
	if (H5FDset_eoa(f->shared->lf, addr)<0) {
	    printf("Cannot create large file family\n");
	    exit(1);
	}
    }

    /*
     * Creation test: Creates empty objects with various raw data sizes
     * and alignments.
     */
    status = test_create(f, "create");
    nerrors += status < 0 ? 1 : 0;

    if (size_of_test & TEST_SMALL) {
	status = test_extend(f, "extend", 10, 0, 0);
	nerrors += status < 0 ? 1 : 0;
	status = test_extend(f, "extend", 10, 10, 0);
	nerrors += status < 0 ? 1 : 0;
	status = test_extend(f, "extend", 10, 10, 10);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_extend(f, "extend", 10000, 0, 0);
	nerrors += status < 0 ? 1 : 0;
	status = test_extend(f, "extend", 2500, 10, 0);
	nerrors += status < 0 ? 1 : 0;
	status = test_extend(f, "extend", 10, 400, 10);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_SMALL) {
	status = test_sparse(f, "sparse", 100, 5, 0, 0);
	nerrors += status < 0 ? 1 : 0;
	status = test_sparse(f, "sparse", 100, 3, 4, 0);
	nerrors += status < 0 ? 1 : 0;
	status = test_sparse(f, "sparse", 100, 2, 3, 4);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_sparse(f, "sparse", 1000, 30, 0, 0);
	nerrors += status < 0 ? 1 : 0;
	status = test_sparse(f, "sparse", 2000, 7, 3, 0);
	nerrors += status < 0 ? 1 : 0;
	status = test_sparse(f, "sparse", 2000, 4, 2, 3);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_LARGE) {
	status = test_sparse(f, "sparse", 800, 50, 50, 50);
	nerrors += status < 0 ? 1 : 0;
    }

    /* Close the test file and exit */
    H5Pclose(fcpl);
    H5Fclose(file);
    
    if (nerrors) {
	printf("***** %d I-STORE TEST%s FAILED! *****\n",
	       nerrors, 1 == nerrors ? "" : "S");
	exit(1);
    }

    printf("All i-store tests passed.\n");
    h5_cleanup(FILENAME, fapl);
    return 0;
}
