/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "$Revision$";
#endif

/* $Id$ */

/***********************************************************
*
* Test program:  th5p
*
* Test the dataspace functionality
*
*************************************************************/

#include <testhdf5.h>

#include <H5private.h>
#include <H5Bprivate.h>
#include <H5Mprivate.h>
#include <H5Pprivate.h>

#define FILE   "th5p1.h5"

/* 3-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK     3
#define SPACE1_DIM1     3
#define SPACE1_DIM2     15
#define SPACE1_DIM3     13

/* 4-D dataset with one unlimited dimension */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK     4
#define SPACE2_DIM1     0
#define SPACE2_DIM2     15
#define SPACE2_DIM3     13
#define SPACE2_DIM4     23

/****************************************************************
**
**  test_h5p_basic(): Test basic H5P (dataspace) code.
** 
****************************************************************/
static void 
test_h5p_basic(void)
{
    hid_t                   fid1;       /* HDF5 File IDs */
    hid_t                   sid1, sid2;         /* Dataspace ID */
    uint32                  rank;       /* Logical rank of dataspace */
    size_t                  dims1[] =
    {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3},    /* dataspace dim sizes */
                            dims2[] =
    {SPACE2_DIM1, SPACE2_DIM2, SPACE2_DIM3, SPACE2_DIM4}, tdims[4];     /* Dimension array to test with */
    uintn                   n;  /* number of dataspace elements */
    herr_t                  ret;        /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Datatype Manipulation\n"));

    /* Create file */
    fid1 = H5Fcreate(FILE, H5ACC_OVERWRITE, 0, 0);
    CHECK(fid1, FAIL, "H5Fcreate");

    sid1 = H5Pcreate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Pcreate_simple");

    n = H5Pget_npoints(sid1);
    CHECK(n, UFAIL, "H5Pget_npoints");
    VERIFY(n, SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3, "H5Pget_npoints");

    rank = H5Pget_ndims(sid1);
    CHECK(rank, UFAIL, "H5Pget_lrank");
    VERIFY(rank, SPACE1_RANK, "H5Pget_lrank");

    ret = H5Pget_dims(sid1, tdims);
    CHECK(ret, FAIL, "H5Pget_ldims");
    VERIFY(HDmemcmp(tdims, dims1, SPACE1_RANK * sizeof(uint32)), 0, "H5Pget_ldims");

    sid2 = H5Pcreate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Pcreate_simple");

    n = H5Pget_npoints(sid2);
    CHECK(n, UFAIL, "H5Pnelem");
    VERIFY(n, SPACE2_DIM1 * SPACE2_DIM2 * SPACE2_DIM3 * SPACE2_DIM4, "H5Pnelem");

    rank = H5Pget_ndims(sid2);
    CHECK(rank, UFAIL, "H5Pget_lrank");
    VERIFY(rank, SPACE2_RANK, "H5Pget_lrank");

    ret = H5Pget_dims(sid2, tdims);
    CHECK(ret, FAIL, "H5Pget_ldims");
    VERIFY(HDmemcmp(tdims, dims2, SPACE2_RANK * sizeof(uint32)), 0, "H5Pget_ldims");

    ret = H5Mclose(sid1);
    CHECK(ret, FAIL, "H5Mrelease");

    ret = H5Mclose(sid2);
    CHECK(ret, FAIL, "H5Mrelease");

    /* Close first file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}                               /* test_h5p_basic() */

/****************************************************************
**
**  test_h5p(): Main H5P (dataspace) testing routine.
** 
****************************************************************/
void 
test_h5p(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspaces\n"));

    test_h5p_basic();           /* Test basic H5P code */
}                               /* test_h5p() */
