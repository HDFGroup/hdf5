/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Friday, October 10, 1997
 *
 * Purpose:	Hyperslab operations are rather complex, so this file
 *		attempts to test them extensively so we can be relatively
 *		sure they really work.	We only test 1d, 2d, and 3d cases
 *		because testing general dimensionalities would require us to
 *		rewrite much of the hyperslab stuff.
 */
#include <H5private.h>
#include <H5MMprivate.h>
#include <H5Vprivate.h>

#ifndef HAVE_FUNCTION
#undef __FUNCTION__
#define __FUNCTION__ ""
#endif
#define AT() printf ("	 at %s:%d in %s()\n",__FILE__,__LINE__,__FUNCTION__);

#define TEST_SMALL	0x0001
#define TEST_MEDIUM	0x0002

#define VARIABLE_SRC	0
#define VARIABLE_DST	1
#define VARIABLE_BOTH	2

/*-------------------------------------------------------------------------
 * Function:	init_full
 *
 * Purpose:	Initialize full array.
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
static uintn
init_full(uint8 *array, size_t nx, size_t ny, size_t nz)
{
    int			    i, j, k;
    uint8		    acc = 128;
    uintn		    total = 0;

    for (i = 0; i < nx; i++) {
	for (j = 0; j < ny; j++) {
	    for (k = 0; k < nz; k++) {
		total += acc;
		*array++ = acc++;
	    }
	}
    }
    return total;
}

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
print_array(uint8 *array, size_t nx, size_t ny, size_t nz)
{
    int			    i, j, k;

    for (i = 0; i < nx; i++) {
	if (nz > 1) {
	    printf("i=%d:\n", i);
	} else {
	    printf("%03d:", i);
	}

	for (j = 0; j < ny; j++) {
	    if (nz > 1)
		printf("%03d:", j);
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
 * Function:	print_ref
 *
 * Purpose:	Prints the reference value
 *
 * Return:	Success:	0
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *		Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_ref(size_t nx, size_t ny, size_t nz)
{
    uint8		   *array;

    array = H5MM_xcalloc(nx * ny * nz, sizeof(uint8));

    printf("Reference array:\n");
    init_full(array, nx, ny, nz);
    print_array(array, nx, ny, nz);
}

/*-------------------------------------------------------------------------
 * Function:	test_fill
 *
 * Purpose:	Tests the H5V_hyper_fill() function.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_fill(size_t nx, size_t ny, size_t nz,
	  size_t di, size_t dj, size_t dk,
	  size_t ddx, size_t ddy, size_t ddz)
{
    uint8	*dst = NULL;		/*destination array		*/
    size_t	hs_size[3];	    	/*hyperslab size		*/
    size_t	dst_size[3];	    	/*destination total size	*/
    size_t	dst_offset[3];	    	/*offset of hyperslab in dest   */
    uintn	ref_value;  		/*reference value		*/
    uintn	acc;	    		/*accumulator		    	*/
    int		i, j, k, dx, dy, dz;	/*counters		   	*/
    size_t	u, v, w;
    int		ndims;			/*hyperslab dimensionality	*/
    char	dim[64], s[256];    	/*temp string		    	*/
    uintn	fill_value;	    	/*fill value		    	*/

    /*
     * Dimensionality.
     */
    if (0 == nz) {
	if (0 == ny) {
	    ndims = 1;
	    ny = nz = 1;
	    sprintf(dim, "%lu", (unsigned long) nx);
	} else {
	    ndims = 2;
	    nz = 1;
	    sprintf(dim, "%lux%lu", (unsigned long) nx, (unsigned long) ny);
	}
    } else {
	ndims = 3;
	sprintf(dim, "%lux%lux%lu",
		(unsigned long) nx, (unsigned long) ny, (unsigned long) nz);
    }
    sprintf(s, "Testing hyperslab fill %-11s variable hyperslab", dim);
    printf("%-70s", s);
    fflush(stdout);

    /* Allocate array */
    dst = H5MM_xcalloc(nx * ny * nz, 1);
    init_full(dst, nx, ny, nz);

    for (i = 0; i < nx; i += di) {
	for (j = 0; j < ny; j += dj) {
	    for (k = 0; k < nz; k += dk) {
		for (dx = 1; dx <= nx - i; dx += ddx) {
		    for (dy = 1; dy <= ny - j; dy += ddy) {
			for (dz = 1; dz <= nz - k; dz += ddz) {

			    /* Describe the hyperslab */
			    dst_size[0] = nx;
			    dst_size[1] = ny;
			    dst_size[2] = nz;
			    dst_offset[0] = i;
			    dst_offset[1] = j;
			    dst_offset[2] = k;
			    hs_size[0] = dx;
			    hs_size[1] = dy;
			    hs_size[2] = dz;

			    for (fill_value=0;
				 fill_value<256;
				 fill_value+=64) {
				/*
				 * Initialize the full array, then subtract the
				 * original * fill values and add the new ones.
				 */
				ref_value = init_full(dst, nx, ny, nz);
				for (u=dst_offset[0];
				     u<dst_offset[0]+dx;
				     u++) {
				    for (v = dst_offset[1];
					 v < dst_offset[1] + dy;
					 v++) {
					for (w = dst_offset[2];
					     w < dst_offset[2] + dz;
					     w++) {
					    ref_value -= dst[u*ny*nz+v*nz+w];
					}
				    }
				}
				ref_value += fill_value * dx * dy * dz;

				/* Fill the hyperslab with some value */
				H5V_hyper_fill(ndims, hs_size, dst_size,
					       dst_offset, dst, fill_value);

				/*
				 * Sum the array and compare it to the
				 * reference value.
				 */
				acc = 0;
				for (u = 0; u < nx; u++) {
				    for (v = 0; v < ny; v++) {
					for (w = 0; w < nz; w++) {
					    acc += dst[u*ny*nz + v*nz + w];
					}
				    }
				}

				if (acc != ref_value) {
				    puts("*FAILED*");
				    if (!isatty(1)) {
					/*
					 * Print debugging info unless output
					 * is going directly to a terminal.
					 */
					AT();
					printf("   acc != ref_value\n");
					printf("   i=%d, j=%d, k=%d, "
					   "dx=%d, dy=%d, dz=%d, fill=%d\n",
					   i, j, k, dx, dy, dz, fill_value);
					print_ref(nx, ny, nz);
					printf("\n   Result is:\n");
					print_array(dst, nx, ny, nz);
				    }
				    goto error;
				}
			    }
			}
		    }
		}
	    }
	}
    }
    puts(" PASSED");
    H5MM_xfree(dst);
    return SUCCEED;

  error:
    H5MM_xfree(dst);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:	test_copy
 *
 * Purpose:	Tests H5V_hyper_copy().
 *
 *		The NX, NY, and NZ arguments are the size for the source and
 *		destination arrays.  You map pass zero for NZ or for NY and
 *		NZ to test the 2-d and 1-d cases respectively.
 *
 *		A hyperslab is copied from/to (depending on MODE) various
 *		places in SRC and DST beginning at 0,0,0 and increasing
 *		location by DI,DJ,DK in the x, y, and z directions.
 *
 *		For each hyperslab location, various sizes of hyperslabs are
 *		tried beginning with 1x1x1 and increasing the size in each
 *		dimension by DDX,DDY,DDZ.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_copy(int mode,
	  size_t nx, size_t ny, size_t nz,
	  size_t di, size_t dj, size_t dk,
	  size_t ddx, size_t ddy, size_t ddz)
{
    uint8	*src = NULL;		/*source array			*/
    uint8	*dst = NULL;		/*destination array		*/
    size_t	hs_size[3];		/*hyperslab size		*/
    size_t	dst_size[3];		/*destination total size	*/
    size_t	src_size[3];		/*source total size		*/
    size_t	dst_offset[3];		/*offset of hyperslab in dest	*/
    size_t	src_offset[3];		/*offset of hyperslab in source */
    uintn	ref_value;		/*reference value		*/
    uintn	acc;			/*accumulator			*/
    int		i, j, k, dx, dy, dz;	/*counters		     	*/
    size_t	u, v, w;
    int		ndims;			/*hyperslab dimensionality	*/
    char	dim[64], s[256];	/*temp string			*/
    const char	*sub;

    /*
     * Dimensionality.
     */
    if (0 == nz) {
	if (0 == ny) {
	    ndims = 1;
	    ny = nz = 1;
	    sprintf(dim, "%lu", (unsigned long) nx);
	} else {
	    ndims = 2;
	    nz = 1;
	    sprintf(dim, "%lux%lu", (unsigned long) nx, (unsigned long) ny);
	}
    } else {
	ndims = 3;
	sprintf(dim, "%lux%lux%lu",
		(unsigned long) nx, (unsigned long) ny, (unsigned long) nz);
    }

    switch (mode) {
    case VARIABLE_SRC:
	/*
	 * The hyperslab "travels" through the source array but the
	 * destination hyperslab is always at the origin of the destination
	 * array.
	 */
	sub = "variable source";
	break;
    case VARIABLE_DST:
	/*
	 * We always read a hyperslab from the origin of the source and copy it
	 * to a hyperslab at various locations in the destination.
	 */
	sub = "variable destination";
	break;
    case VARIABLE_BOTH:
	/*
	 * We read the hyperslab from various locations in the source and copy
	 * it to the same location in the destination.
	 */
	sub = "sync source & dest  ";
	break;
    default:
	abort();
    }

    sprintf(s, "Testing hyperslab copy %-11s %s", dim, sub);
    printf("%-70s", s);
    fflush(stdout);

    /*
     * Allocate arrays
     */
    src = H5MM_xcalloc(nx * ny * nz, 1);
    dst = H5MM_xcalloc(nx * ny * nz, 1);
    init_full(src, nx, ny, nz);

    for (i = 0; i < nx; i += di) {
	for (j = 0; j < ny; j += dj) {
	    for (k = 0; k < nz; k += dk) {
		for (dx = 1; dx <= nx - i; dx += ddx) {
		    for (dy = 1; dy <= ny - j; dy += ddy) {
			for (dz = 1; dz <= nz - k; dz += ddz) {

			    /*
			     * Describe the source and destination hyperslabs
			     * and the arrays to which they belong.
			     */
			    hs_size[0] = dx;
			    hs_size[1] = dy;
			    hs_size[2] = dz;
			    dst_size[0] = src_size[0] = nx;
			    dst_size[1] = src_size[1] = ny;
			    dst_size[2] = src_size[2] = nz;
			    switch (mode) {
			    case VARIABLE_SRC:
				dst_offset[0] = 0;
				dst_offset[1] = 0;
				dst_offset[2] = 0;
				src_offset[0] = i;
				src_offset[1] = j;
				src_offset[2] = k;
				break;
			    case VARIABLE_DST:
				dst_offset[0] = i;
				dst_offset[1] = j;
				dst_offset[2] = k;
				src_offset[0] = 0;
				src_offset[1] = 0;
				src_offset[2] = 0;
				break;
			    case VARIABLE_BOTH:
				dst_offset[0] = i;
				dst_offset[1] = j;
				dst_offset[2] = k;
				src_offset[0] = i;
				src_offset[1] = j;
				src_offset[2] = k;
				break;
			    default:
				abort();
			    }

			    /*
			     * Sum the main array directly to get a reference
			     * value to compare against later.
			     */
			    ref_value = 0;
			    for (u=src_offset[0]; u<src_offset[0]+dx; u++) {
				for (v=src_offset[1];
				     v<src_offset[1]+dy;
				     v++) {
				    for (w=src_offset[2];
					 w<src_offset[2]+dz;
					 w++) {
					ref_value += src[u*ny*nz + v*nz + w];
				    }
				}
			    }

			    /*
			     * Set all loc values to 1 so we can detect writing
			     * outside the hyperslab.
			     */
			    for (u = 0; u < nx; u++) {
				for (v = 0; v < ny; v++) {
				    for (w = 0; w < nz; w++) {
					dst[u * ny * nz + v * nz + w] = 1;
				    }
				}
			    }

			    /*
			     * Copy a hyperslab from the global array to the
			     * local array.
			     */
			    H5V_hyper_copy(ndims, hs_size,
					   dst_size, dst_offset, dst,
					   src_size, src_offset, src);

			    /*
			     * Sum the destination hyperslab.  It should be
			     * the same as the reference value.
			     */
			    acc = 0;
			    for (u=dst_offset[0]; u<dst_offset[0]+dx; u++) {
				for (v=dst_offset[1];
				     v<dst_offset[1]+dy;
				     v++) {
				    for (w = dst_offset[2];
					 w < dst_offset[2] + dz;
					 w++) {
					acc += dst[u * ny * nz + v * nz + w];
				    }
				}
			    }
			    if (acc != ref_value) {
				puts("*FAILED*");
				if (!isatty(1)) {
				    /*
				     * Print debugging info unless output is
				     * going directly to a terminal.
				     */
				    AT();
				    printf("   acc != ref_value\n");
				    printf("   i=%d, j=%d, k=%d, "
					   "dx=%d, dy=%d, dz=%d\n",
					   i, j, k, dx, dy, dz);
				    print_ref(nx, ny, nz);
				    printf("\n	 Destination array is:\n");
				    print_array(dst, nx, ny, nz);
				}
				goto error;
			    }
			    /*
			     * Sum the entire array. It should be a fixed
			     * amount larger than the reference value since
			     * we added the border of 1's to the hyperslab.
			     */
			    acc = 0;
			    for (u = 0; u < nx; u++) {
				for (v = 0; v < ny; v++) {
				    for (w = 0; w < nz; w++) {
					acc += dst[u * ny * nz + v * nz + w];
				    }
				}
			    }
			    if (acc != ref_value + nx*ny*nz - dx*dy*dz) {
				puts("*FAILED*");
				if (!isatty(1)) {
				    /*
				     * Print debugging info unless output is
				     * going directly to a terminal.
				     */
				    AT();
				    printf("   acc != ref_value + nx*ny*nz - "
					   "dx*dy*dz\n");
				    printf("   i=%d, j=%d, k=%d, "
					   "dx=%d, dy=%d, dz=%d\n",
					   i, j, k, dx, dy, dz);
				    print_ref(nx, ny, nz);
				    printf("\n	 Destination array is:\n");
				    print_array(dst, nx, ny, nz);
				}
				goto error;
			    }
			}
		    }
		}
	    }
	}
    }
    puts(" PASSED");
    H5MM_xfree(src);
    H5MM_xfree(dst);
    return SUCCEED;

  error:
    H5MM_xfree(src);
    H5MM_xfree(dst);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:	test_multifill
 *
 * Purpose:	Tests the H5V_stride_copy() function by using it to fill a
 *		hyperslab by replicating a multi-byte sequence.	 This might
 *		be useful to initialize an array of structs with a default
 *		struct value, or to initialize an array of floating-point
 *		values with a default bit-pattern.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multifill(int nx)
{
    int			    i, j;
    size_t		    size;
    ssize_t		    src_stride;
    ssize_t		    dst_stride;
    char		    s[64];

    struct a_struct {
	int			left;
	double			mid;
	int			right;
    } fill		   , *src = NULL, *dst = NULL;

    printf("%-70s", "Testing multi-byte fill value");
    fflush(stdout);

    /* Initialize the source and destination */
    src = H5MM_xmalloc(nx * sizeof(*src));
    dst = H5MM_xmalloc(nx * sizeof(*dst));
    for (i = 0; i < nx; i++) {
	src[i].left = 1111111;
	src[i].mid = 12345.6789;
	src[i].right = 2222222;
	dst[i].left = 3333333;
	dst[i].mid = 98765.4321;
	dst[i].right = 4444444;
    }

    /*
     * Describe the fill value.	 The zero stride says to read the same thing
     * over and over again.
     */
    fill.left = 55555555;
    fill.mid = 3.1415927;
    fill.right = 66666666;
    src_stride = 0;

    /*
     * The destination stride says to fill in one value per array element
     */
    dst_stride = sizeof(fill);

    /*
     * Copy the fill value into each element
     */
    size = nx;
    H5V_stride_copy(1, sizeof(double), &size,
		    &dst_stride, &(dst[0].mid), &src_stride, &(fill.mid));

    /*
     * Check
     */
    s[0] = '\0';
    for (i = 0; i < nx; i++) {
	if (dst[i].left != 3333333) {
	    sprintf(s, "bad dst[%d].left", i);
	} else if (dst[i].mid != fill.mid) {
	    sprintf(s, "bad dst[%d].mid", i);
	} else if (dst[i].right != 4444444) {
	    sprintf(s, "bad dst[%d].right", i);
	}
	if (s[0]) {
	    puts("*FAILED*");
	    if (!isatty(1)) {
		AT();
		printf("   fill={%d,%g,%d}\n   ",
		       fill.left, fill.mid, fill.right);
		for (j = 0; j < sizeof(fill); j++) {
		    printf(" %02x", ((uint8 *) &fill)[j]);
		}
		printf("\n   dst[%d]={%d,%g,%d}\n   ",
		       i, dst[i].left, dst[i].mid, dst[i].right);
		for (j = 0; j < sizeof(dst[i]); j++) {
		    printf(" %02x", ((uint8 *) (dst + i))[j]);
		}
		printf("\n");
	    }
	    goto error;
	}
    }

    puts(" PASSED");
    H5MM_xfree(src);
    H5MM_xfree(dst);
    return SUCCEED;

  error:
    H5MM_xfree(src);
    H5MM_xfree(dst);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:	test_endian
 *
 * Purpose:	Tests the H5V_stride_copy() function by using it to copy an
 *		array of integers and swap the byte ordering from little
 *		endian to big endian or vice versa depending on the hardware.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_endian(size_t nx)
{
    uint8	*src = NULL;		/*source array			*/
    uint8	*dst = NULL;		/*destination array		*/
    ssize_t	src_stride[2];		/*source strides		*/
    ssize_t	dst_stride[2];		/*destination strides		*/
    size_t	size[2];		/*size vector			*/
    int		i, j;

    printf("%-70s", "Testing endian conversion by stride");
    fflush(stdout);

    /* Initialize arrays */
    src = H5MM_xmalloc(nx * 4);
    init_full(src, nx, 4, 1);
    dst = H5MM_xcalloc(nx, 4);

    /* Initialize strides */
    src_stride[0] = 0;
    src_stride[1] = 1;
    dst_stride[0] = 8;
    dst_stride[1] = -1;
    size[0] = nx;
    size[1] = 4;

    /* Copy the array */
    H5V_stride_copy(2, 1, size, dst_stride, dst + 3, src_stride, src);

    /* Compare */
    for (i = 0; i < nx; i++) {
	for (j = 0; j < 4; j++) {
	    if (src[i * 4 + j] != dst[i * 4 + 3 - j]) {
		puts("*FAILED*");
		if (!isatty(1)) {
		    /*
		     * Print debugging info unless output is going directly
		     * to a terminal.
		     */
		    AT();
		    printf("   i=%d, j=%d\n", i, j);
		    printf("   Source array is:\n");
		    print_array(src, nx, 4, 1);
		    printf("\n	 Result is:\n");
		    print_array(dst, nx, 4, 1);
		}
		goto error;
	    }
	}
    }

    puts(" PASSED");
    H5MM_xfree(src);
    H5MM_xfree(dst);
    return SUCCEED;

  error:
    H5MM_xfree(src);
    H5MM_xfree(dst);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:	test_transpose
 *
 * Purpose:	Copy a 2d array from here to there and transpose the elements
 *		as it's copied.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_transpose(size_t nx, size_t ny)
{
    intn	*src = NULL;
    intn	*dst = NULL;
    int		i, j;
    ssize_t	src_stride[2], dst_stride[2];
    size_t	size[2];
    char	s[256];

    sprintf(s, "Testing 2d transpose by stride %4lux%-lud",
	    (unsigned long) nx, (unsigned long) ny);
    printf("%-70s", s);
    fflush(stdout);

    /* Initialize */
    src = H5MM_xmalloc(nx * ny * sizeof(*src));
    for (i = 0; i < nx; i++) {
	for (j = 0; j < ny; j++) {
	    src[i * ny + j] = (intn)(i * ny + j);
	}
    }
    dst = H5MM_xcalloc(nx * ny, sizeof(*dst));

    /* Build stride info */
    size[0] = nx;
    size[1] = ny;
    src_stride[0] = 0;
    src_stride[1] = sizeof(*src);
    dst_stride[0] = (1 - nx * ny) * sizeof(*src);
    dst_stride[1] = nx * sizeof(*src);

    /* Copy and transpose */
    if (nx == ny) {
	H5V_stride_copy(2, sizeof(*src), size,
			dst_stride, dst,
			src_stride, src);
    } else {
	H5V_stride_copy(2, sizeof(*src), size,
			dst_stride, dst,
			src_stride, src);
    }

    /* Check */
    for (i = 0; i < nx; i++) {
	for (j = 0; j < ny; j++) {
	    if (src[i * ny + j] != dst[j * nx + i]) {
		puts("*FAILED*");
		if (!isatty(1)) {
		    AT();
		    printf("   diff at i=%d, j=%d\n", i, j);
		    printf("   Source is:\n");
		    for (i = 0; i < nx; i++) {
			printf("%3d:", i);
			for (j = 0; j < ny; j++) {
			    printf(" %6d", src[i * ny + j]);
			}
			printf("\n");
		    }
		    printf("\n	 Destination is:\n");
		    for (i = 0; i < ny; i++) {
			printf("%3d:", i);
			for (j = 0; j < nx; j++) {
			    printf(" %6d", dst[i * nx + j]);
			}
			printf("\n");
		    }
		}
		goto error;
	    }
	}
    }

    puts(" PASSED");
    H5MM_xfree(src);
    H5MM_xfree(dst);
    return SUCCEED;

  error:
    H5MM_xfree(src);
    H5MM_xfree(dst);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:	test_sub_super
 *
 * Purpose:	Tests H5V_stride_copy() to reduce the resolution of an image
 *		by copying half the pixels in the X and Y directions.  Then
 *		we use the small image and duplicate every pixel to result in
 *		a 2x2 square.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Monday, October 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sub_super(size_t nx, size_t ny)
{
    uint8	*full = NULL;	/*original image		*/
    uint8	*half = NULL;	/*image at 1/2 resolution	*/
    uint8	*twice = NULL;	/*2x2 pixels			*/
    ssize_t	src_stride[4];	/*source stride info		*/
    ssize_t	dst_stride[4];	/*destination stride info	*/
    size_t	size[4];	/*number of sample points	*/
    int		i, j;
    char	s[256];

    sprintf(s, "Testing image sampling %4lux%-4lu to %4lux%-4lu ",
	    (unsigned long) (2 * nx), (unsigned long) (2 * ny),
	    (unsigned long) nx, (unsigned long) ny);
    printf("%-70s", s);
    fflush(stdout);

    /* Initialize */
    full = H5MM_xmalloc(4 * nx * ny);
    init_full(full, 2 * nx, 2 * ny, 1);
    half = H5MM_xcalloc(nx * ny, 1);
    twice = H5MM_xcalloc(4 * nx * ny, 1);

    /* Setup */
    size[0] = nx;
    size[1] = ny;
    src_stride[0] = 2 * ny;
    src_stride[1] = 2;
    dst_stride[0] = 0;
    dst_stride[1] = 1;

    /* Copy */
    H5V_stride_copy(2, sizeof(uint8), size,
		    dst_stride, half, src_stride, full);

    /* Check */
    for (i = 0; i < nx; i++) {
	for (j = 0; j < ny; j++) {
	    if (full[4 * i * ny + 2 * j] != half[i * ny + j]) {
		puts("*FAILED*");
		if (!isatty(1)) {
		    AT();
		    printf("   full[%d][%d] != half[%d][%d]\n",
			   i*2, j*2, i, j);
		    printf("   full is:\n");
		    print_array(full, 2 * nx, 2 * ny, 1);
		    printf("\n	 half is:\n");
		    print_array(half, nx, ny, 1);
		}
		goto error;
	    }
	}
    }
    puts(" PASSED");

    /*
     * Test replicating pixels to produce an image twice as large in each
     * dimension.
     */
    sprintf(s, "Testing image sampling %4lux%-4lu to %4lux%-4lu ",
	    (unsigned long) nx, (unsigned long) ny,
	    (unsigned long) (2 * nx), (unsigned long) (2 * ny));
    printf("%-70s", s);
    fflush(stdout);

    /* Setup stride */
    size[0] = nx;
    size[1] = ny;
    size[2] = 2;
    size[3] = 2;
    src_stride[0] = 0;
    src_stride[1] = 1;
    src_stride[2] = 0;
    src_stride[3] = 0;
    dst_stride[0] = 2 * ny;
    dst_stride[1] = 2 * sizeof(uint8) - 4 * ny;
    dst_stride[2] = 2 * ny - 2 * sizeof(uint8);
    dst_stride[3] = sizeof(uint8);

    /* Copy */
    H5V_stride_copy(4, sizeof(uint8), size,
		    dst_stride, twice, src_stride, half);

    /* Check */
    s[0] = '\0';
    for (i = 0; i < nx; i++) {
	for (j = 0; j < ny; j++) {
	    if (half[i*ny+j] != twice[4*i*ny + 2*j]) {
		sprintf(s, "half[%d][%d] != twice[%d][%d]",
			i, j, 2 * i, 2 * j);
	    } else if (half[i*ny + j] != twice[4*i*ny + 2*j + 1]) {
		sprintf(s, "half[%d][%d] != twice[%d][%d]",
			i, j, 2 * i, 2 * j + 1);
	    } else if (half[i*ny + j] != twice[(2*i +1)*2*ny + 2*j]) {
		sprintf(s, "half[%d][%d] != twice[%d][%d]",
			i, j, 2 * i + 1, 2 * j);
	    } else if (half[i*ny + j] != twice[(2*i+1)*2*ny + 2*j+1]) {
		sprintf(s, "half[%d][%d] != twice[%d][%d]",
			i, j, 2 * i + 1, 2 * j + 1);
	    }
	    if (s[0]) {
		puts("*FAILED*");
		if (!isatty(1)) {
		    AT();
		    printf("   %s\n   Half is:\n", s);
		    print_array(half, nx, ny, 1);
		    printf("\n	 Twice is:\n");
		    print_array(twice, 2 * nx, 2 * ny, 1);
		}
		goto error;
	    }
	}
    }
    puts(" PASSED");

    H5MM_xfree(full);
    H5MM_xfree(half);
    H5MM_xfree(twice);
    return SUCCEED;

  error:
    H5MM_xfree(full);
    H5MM_xfree(half);
    H5MM_xfree(twice);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test various hyperslab operations.  Give the words
 *		`small' and/or `medium' on the command line or only `small'
 *		is assumed.
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Robb Matzke
 *		Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    herr_t		    status;
    int			    nerrors = 0;
    uintn		    size_of_test;

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
	    } else {
		printf("unrecognized argument: %s\n", argv[i]);
		exit(1);
	    }
	}
    }
    printf("Test sizes: ");
    if (size_of_test & TEST_SMALL)
	printf(" SMALL");
    if (size_of_test & TEST_MEDIUM)
	printf(" MEDIUM");
    printf("\n");

    /*
     *------------------------------ 
     * TEST HYPERSLAB FILL OPERATION
     *------------------------------ 
     */
    if (size_of_test & TEST_SMALL) {
	status = test_fill(11, 0, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_fill(11, 10, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_fill(3, 5, 5, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_fill(113, 0, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_fill(15, 11, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_fill(5, 7, 7, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
    }
   /*------------------------------
    * TEST HYPERSLAB COPY OPERATION
    *------------------------------ 
    */

    /* exhaustive, one-dimensional test */
    if (size_of_test & TEST_SMALL) {
	status = test_copy(VARIABLE_SRC, 11, 0, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_DST, 11, 0, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_BOTH, 11, 0, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_copy(VARIABLE_SRC, 179, 0, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_DST, 179, 0, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_BOTH, 179, 0, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
    }
    /* exhaustive, two-dimensional test */
    if (size_of_test & TEST_SMALL) {
	status = test_copy(VARIABLE_SRC, 11, 10, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_DST, 11, 10, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_BOTH, 11, 10, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_copy(VARIABLE_SRC, 13, 19, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_DST, 13, 19, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_BOTH, 13, 19, 0, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
    }
    /* sparse, two-dimensional test */
    if (size_of_test & TEST_MEDIUM) {
	status = test_copy(VARIABLE_SRC, 73, 67, 0, 7, 11, 1, 13, 11, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_DST, 73, 67, 0, 7, 11, 1, 13, 11, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_BOTH, 73, 67, 0, 7, 11, 1, 13, 11, 1);
	nerrors += status < 0 ? 1 : 0;
    }
    /* exhaustive, three-dimensional test */
    if (size_of_test & TEST_SMALL) {
	status = test_copy(VARIABLE_SRC, 3, 5, 5, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_DST, 3, 5, 5, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_BOTH, 3, 5, 5, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_copy(VARIABLE_SRC, 7, 9, 5, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_DST, 7, 9, 5, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
	status = test_copy(VARIABLE_BOTH, 7, 9, 5, 1, 1, 1, 1, 1, 1);
	nerrors += status < 0 ? 1 : 0;
    }
   /*---------------------
    * TEST MULTI-BYTE FILL
    *--------------------- 
    */

    if (size_of_test & TEST_SMALL) {
	status = test_multifill(10);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_multifill(500000);
	nerrors += status < 0 ? 1 : 0;
    }
   /*---------------------------
    * TEST TRANSLATION OPERATORS
    *---------------------------
    */

    if (size_of_test & TEST_SMALL) {
	status = test_endian(10);
	nerrors += status < 0 ? 1 : 0;
	status = test_transpose(9, 9);
	nerrors += status < 0 ? 1 : 0;
	status = test_transpose(3, 11);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_endian(800000);
	nerrors += status < 0 ? 1 : 0;
	status = test_transpose(1200, 1200);
	nerrors += status < 0 ? 1 : 0;
	status = test_transpose(800, 1800);
	nerrors += status < 0 ? 1 : 0;
    }
   /*-------------------------
    * TEST SAMPLING OPERATIONS
    *------------------------- 
    */

    if (size_of_test & TEST_SMALL) {
	status = test_sub_super(5, 10);
	nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
	status = test_sub_super(480, 640);
	nerrors += status < 0 ? 1 : 0;
    }
/*--- END OF TESTS ---*/

    if (nerrors) {
	printf("***** %d HYPERSLAB TEST%s FAILED! *****\n",
	       nerrors, 1 == nerrors ? "" : "S");
	if (isatty(1)) {
	    printf("(Redirect output to a pager or a file to see "
		   "debug output)\n");
	}
	exit(1);
    }
    printf("All hyperslab tests passed.\n");
    return 0;
}
