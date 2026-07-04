/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5REPACK_GENTEST_H
#define H5REPACK_GENTEST_H

#include "hdf5.h"

int generate_int32le_1d(bool external);
int generate_int32le_2d(bool external);
int generate_int32le_3d(bool external);
int generate_uint8be(bool external);
int generate_f32le(bool external);
int make_h5repack_testfiles(void);
int verify_userblock(const char *filename);
int gen_filespaces(void);

/* fill value test */
#define H5REPACK_FNAME0    "h5repack_fill.h5"
#define H5REPACK_FNAME0OUT "h5repack_fill_out.h5"
/* HDF5 objects and all dataset datatypes */
#define H5REPACK_FNAME1    "h5repack_objs.h5"
#define H5REPACK_FNAME1OUT "h5repack_objs_out.h5"
/* attributes, all datatypes  */
#define H5REPACK_FNAME2    "h5repack_attr.h5"
#define H5REPACK_FNAME2OUT "h5repack_attr_out.h5"
/* hard links  */
#define H5REPACK_FNAME3    "h5repack_hlink.h5"
#define H5REPACK_FNAME3OUT "h5repack_hlink_out.h5"
/* layout  */
#define H5REPACK_FNAME4    "h5repack_layout.h5"
#define H5REPACK_FNAME4OUT "h5repack_layout_out.h5"
/* H5D_ALLOC_TIME_EARLY  */
#define H5REPACK_FNAME5    "h5repack_early.h5"
#define H5REPACK_FNAME5OUT "h5repack_early_out.h5"
#define H5REPACK_FNAME6    "h5repack_early2.h5"
#ifdef H5_HAVE_FILTER_SZIP
/* SZIP filter  */
#define H5REPACK_FNAME7    "h5repack_szip.h5"
#define H5REPACK_FNAME7OUT "h5repack_szip_out.h5"
#endif
/* GZIP filter  */
#define H5REPACK_FNAME8    "h5repack_deflate.h5"
#define H5REPACK_FNAME8OUT "h5repack_deflate_out.h5"
/* GZIP filter  */
#define H5REPACK_FNAME9    "h5repack_shuffle.h5"
#define H5REPACK_FNAME9OUT "h5repack_shuffle_out.h5"
/* Fletcher filter  */
#define H5REPACK_FNAME10    "h5repack_fletcher.h5"
#define H5REPACK_FNAME10OUT "h5repack_fletcher_out.h5"
/* All filters  */
#define H5REPACK_FNAME11 "h5repack_filters.h5"
#if defined(H5_HAVE_FILTER_DEFLATE)
#define H5REPACK_FNAME11OUT "h5repack_filters_out.h5"
#endif
/* NBit filter  */
#define H5REPACK_FNAME12    "h5repack_nbit.h5"
#define H5REPACK_FNAME12OUT "h5repack_nbit_out.h5"
/* Scale Offset filter  */
#define H5REPACK_FNAME13    "h5repack_soffset.h5"
#define H5REPACK_FNAME13OUT "h5repack_soffset_out.h5"
/* Big file to test read by hyperslabs  */
#define H5REPACK_FNAME14    "h5repack_big.h5"
#define H5REPACK_FNAME14OUT "h5repack_big_out.h5"
/* external file  */
#define H5REPACK_FNAME15    "h5repack_ext.h5"
#define H5REPACK_FNAME15OUT "h5repack_ext_out.h5"
/* File w/userblock */
#define H5REPACK_FNAME16    "h5repack_ub.h5"
#define H5REPACK_FNAME16OUT "h5repack_ub_out.h5"
/* Named datatypes */
#define H5REPACK_FNAME17    "h5repack_named_dtypes.h5"
#define H5REPACK_FNAME17OUT "h5repack_named_dtypes_out.h5"

#define H5REPACK_FNAME18 "h5repack_layout2.h5"

#define H5REPACK_FNAME19 "h5repack_layout3.h5"

#define H5REPACK_FNAME_UB "ublock.bin"

/* obj and region references */
#define H5REPACK_FNAME_REF "h5repack_refs.h5"

/* obj and region references in attr of compound and vlen type */
#define H5REPACK_FNAME_ATTR_REF "h5repack_attr_refs.h5"

#define FSPACE_OUT "h5repack_fspace_OUT.h5" /* The output file */

#define H5REPACK_EXTFILE "h5repack_ext.bin"

static const char *H5REPACK_FSPACE_FNAMES[] = {
    "h5repack_latest.h5",             /* 0 */
    "h5repack_default.h5",            /* 1 */
    "h5repack_page_persist.h5",       /* 2 */
    "h5repack_fsm_aggr_persist.h5",   /* 3 */
    "h5repack_page_threshold.h5",     /* 4 */
    "h5repack_fsm_aggr_threshold.h5", /* 5 */
    "h5repack_aggr.h5",               /* 6 */
    "h5repack_none.h5"                /* 7 */
};

/* Filenames for generated h5repack test HDF5 files */
static const char *H5REPACK_TEST_H5_FILES[] = {
    H5REPACK_FNAME0,     H5REPACK_FNAME0OUT,      H5REPACK_FNAME1,  H5REPACK_FNAME1OUT,
    H5REPACK_FNAME2,     H5REPACK_FNAME2OUT,      H5REPACK_FNAME3,  H5REPACK_FNAME3OUT,
    H5REPACK_FNAME4,     H5REPACK_FNAME4OUT,      H5REPACK_FNAME5,  H5REPACK_FNAME5OUT,
    H5REPACK_FNAME6,
#ifdef H5_HAVE_FILTER_SZIP
    H5REPACK_FNAME7,     H5REPACK_FNAME7OUT,
#endif
    H5REPACK_FNAME8,     H5REPACK_FNAME8OUT,      H5REPACK_FNAME9,  H5REPACK_FNAME9OUT,
    H5REPACK_FNAME10,    H5REPACK_FNAME10OUT,     H5REPACK_FNAME11,
#if defined(H5_HAVE_FILTER_SZIP) && defined(H5_HAVE_FILTER_DEFLATE)
    H5REPACK_FNAME11OUT,
#endif
    H5REPACK_FNAME12,    H5REPACK_FNAME12OUT,     H5REPACK_FNAME13, H5REPACK_FNAME13OUT,
    H5REPACK_FNAME14,    H5REPACK_FNAME14OUT,     H5REPACK_FNAME15, H5REPACK_FNAME15OUT,
    H5REPACK_FNAME17,    H5REPACK_FNAME17OUT,     H5REPACK_FNAME18, H5REPACK_FNAME19,
    H5REPACK_FNAME_REF,  H5REPACK_FNAME_ATTR_REF, FSPACE_OUT,       NULL};

static const char *H5REPACK_TEST_MISC_FILES[] = {H5REPACK_EXTFILE};

static const char *H5REPACK_DEFAULT_DRIVER_MISC_FILES[] = {H5REPACK_FNAME_UB};

static const char *H5REPACK_DEFAULT_DRIVER_FILES[] = {H5REPACK_FNAME16, H5REPACK_FNAME16OUT, NULL};

#define USERBLOCK_SIZE 2048

#endif /* H5REPACK_GENTEST_H */