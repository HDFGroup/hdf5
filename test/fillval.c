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
 * Programmer:  Robb Matzke <robb@arborea.spizella.com>
 *              Thursday, October  1, 1998
 *
 * Purpose:	Tests dataset fill values.
 */
#include "h5test.h"

/*
 * Define NO_FILLING if you want to compare how this test works when there is
 * no fill value (that is, when the fill value is zero).
 */
/* #define NO_FILLING */

const char *FILENAME[] = {
    "fillval_1",
    "fillval_2",
    "fillval_3",
    "fillval_4",
    "fillval_5",
    "fillval_6",
    "fillval_7",
    "fillval_8",
    NULL
};

typedef struct {
    float  a;
    int    x;
    double y;
    char   z;
} comp_datatype;

/* The fill_old.h5 is generated from gen_old_fill.c in HDF5 'test' directory
 * for version 1.4(after 1.4.3).  To get this data file, simply compile
 * gen_old_fill.c with HDF5 library (before v1.5) and run it. */
#define FILE_COMPATIBLE "fill_old.h5"
#define FILE_NAME_RAW	"fillval.raw"


/*-------------------------------------------------------------------------
 * Function:    create_compound_type
 *
 * Purpose:     create a compound datatype
 *
 * Return:      Success:        datatype ID
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Monday, Jan 26, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t create_compound_type(void)
{
    hid_t ret_value=-1;

    if((ret_value = H5Tcreate(H5T_COMPOUND, sizeof(comp_datatype))) < 0)
        goto error;
    if(H5Tinsert(ret_value, "a", HOFFSET(comp_datatype, a), H5T_NATIVE_FLOAT) < 0)
        goto error;
    if(H5Tinsert(ret_value, "x", HOFFSET(comp_datatype, x), H5T_NATIVE_INT) < 0)
        goto error;
    if(H5Tinsert(ret_value, "y", HOFFSET(comp_datatype, y), H5T_NATIVE_DOUBLE) < 0)
        goto error;
    if(H5Tinsert(ret_value, "z", HOFFSET(comp_datatype, z), H5T_NATIVE_CHAR) < 0)
	goto error;

    return ret_value;

 error:
    H5E_BEGIN_TRY {
        H5Tclose(ret_value);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_getset
 *
 * Purpose:	Tests the H5Pget_fill_value() and H5Pset_fill_value()
 *		functions.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_getset(void)
{
    hid_t	dcpl=-1;
    int		fill_i;
    hid_t	type_ss=-1, type_si=-1;
    struct fill_si {
	int 	v1, v2;
    }		fill_si;
    struct fill_ss {
	short	v1, v2;
    }		fill_ss, fill_ss_rd;

    TESTING("property lists");

    /*
     * Create the dataset creation property list and the data types that will
     * be used during this test.
     */
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if ((type_ss=H5Tcreate(H5T_COMPOUND, sizeof fill_ss))<0 ||
	H5Tinsert(type_ss, "v1", HOFFSET(struct fill_ss, v1),
		  H5T_NATIVE_SHORT)<0 ||
	H5Tinsert(type_ss, "v2", HOFFSET(struct fill_ss, v2),
		  H5T_NATIVE_SHORT)<0) {
	goto error;
    }
    if ((type_si=H5Tcreate(H5T_COMPOUND, sizeof fill_si))<0 ||
	H5Tinsert(type_si, "v1", HOFFSET(struct fill_si, v1),
		  H5T_NATIVE_INT)<0 ||
	H5Tinsert(type_si, "v2", HOFFSET(struct fill_si, v2),
		  H5T_NATIVE_INT)<0) {
	goto error;
    }

    /*
     * Reading the fill value from a dataset creation property list that has
     * no fill value should result in a failure.
     */
    H5E_BEGIN_TRY {
	H5Pget_fill_value(dcpl, H5T_NATIVE_INT, &fill_i);
    } H5E_END_TRY;
    if (fill_i != 0) {
	H5_FAILED();
	puts("    H5Pget_fill_value() should return default 0");
	goto error;
    }

    /*
     * Set the fill value using a struct as the data type.
     */
    fill_ss.v1 = 1111;
    fill_ss.v2 = 2222;
    if (H5Pset_fill_value(dcpl, type_ss, &fill_ss)<0) goto error;

    /*
     * Get the fill value using the same data type that was used to set it.
     */
    if (H5Pget_fill_value(dcpl, type_ss, &fill_ss_rd)<0) goto error;
    if (fill_ss.v1!=fill_ss_rd.v1 || fill_ss.v2!=fill_ss_rd.v2) {
	H5_FAILED();
	puts("    Failed to get fill value using same data type that was ");
	puts("    used to set the fill value.");
	goto error;
    }

    /*
     * Get the fill value using some other data type.
     */
    if (H5Pget_fill_value(dcpl, type_si, &fill_si)<0) goto error;
    if (fill_ss.v1!=fill_si.v1 || fill_ss.v2!=fill_si.v2) {
	H5_FAILED();
	puts("    Failed to get fill value using a data type other than what");
	puts("    was used to set the fill value.");
	goto error;
    }

    /*
     * Reset the fill value
     */
    if (H5Pset_fill_value(dcpl, type_si, &fill_si)<0) goto error;
    if (H5Pget_fill_value(dcpl, type_ss, &fill_ss)<0) goto error;
    if (fill_si.v1!=fill_ss.v1 || fill_si.v2!=fill_ss.v2) {
	H5_FAILED();
	puts("    Resetting the fill value was unsuccessful.");
	goto error;
    }

    /* Success */
    if (H5Pclose(dcpl)<0) goto error;
    if (H5Tclose(type_si)<0) goto error;
    if (H5Tclose(type_ss)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Tclose(type_si);
	H5Tclose(type_ss);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_getset_vl
 *
 * Purpose:	Tests the H5Pget_fill_value() and H5Pset_fill_value()
 *		functions, using variable-length datatype.
 *
 * Return:	Success:	0
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 31, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_getset_vl(hid_t fapl)
{
    hsize_t dims[1] = {2};
    hid_t fileid = (-1), spaceid = (-1), typeid = (-1), datasetid = (-1), plistid = (-1);
    char fill_value[] = "aaaa";
    char orig_fill_value[] = "aaaa";
    char *f1 = fill_value;
    char *f2;
    char filename[1024];

    TESTING("property lists, with variable-length datatype");

    /* Create string type. */
    if((typeid =  H5Tcopy(H5T_C_S1)) < 0) TEST_ERROR
    if(H5Tset_size(typeid, H5T_VARIABLE) < 0) TEST_ERROR

    /* Set up dataset creation property list, with fill value */
    if((plistid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_fill_value(plistid, typeid, &f1) < 0) TEST_ERROR

    /* Modify original fill value string */
    fill_value[0] = 'b';

    /* Retrieve fill value from property */
    if(H5Pget_fill_value(plistid, typeid, &f2) < 0) TEST_ERROR

    /* Verify that the fill value is the original value */
    if(HDstrcmp(f2, orig_fill_value)) TEST_ERROR

    /* Release the fill value retrieved */
    HDfree(f2);

    /* Open file. */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((fileid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Write an dataset of this type. */
    if((spaceid = H5Screate_simple(1, dims, NULL)) < 0) TEST_ERROR
    if((datasetid = H5Dcreate(fileid, "Dataset", typeid, spaceid, plistid)) < 0) TEST_ERROR

    /* Close IDs (except datatype) */
    if(H5Dclose(datasetid) < 0) TEST_ERROR
    if(H5Pclose(plistid) < 0) TEST_ERROR
    if(H5Sclose(spaceid) < 0) TEST_ERROR
    if(H5Fclose(fileid) < 0) TEST_ERROR


    /* Re-open file, group & dataset */
    if((fileid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR
    if((datasetid = H5Dopen(fileid, "Dataset")) < 0) TEST_ERROR

    /* Get dataset's creation property list */
    if((plistid = H5Dget_create_plist(datasetid)) < 0) TEST_ERROR

    /* Query fill value */
    if(H5Pget_fill_value(plistid, typeid, &f2) < 0) TEST_ERROR

    /* Verify that the fill value is the original value */
    if(HDstrcmp(f2, orig_fill_value)) TEST_ERROR

    /* Release the fill value retrieved */
    HDfree(f2);

    /* Close IDs */
    if(H5Dclose(datasetid) < 0) TEST_ERROR
    if(H5Fclose(fileid) < 0) TEST_ERROR
    if(H5Pclose(plistid) < 0) TEST_ERROR
    if(H5Tclose(typeid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    } H5E_END_TRY;
    return 1;
} /* end test_getset_vl() */


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Tests creating datasets that have fill values.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *		Many new cases have been added to this test since
 *		the fill value design has been modified.
 *
 *-------------------------------------------------------------------------
 */
static int
test_create(hid_t fapl, const char *base_name, H5D_layout_t layout)
{
    hid_t	file=-1, space=-1, dcpl=-1, comp_type_id=-1;
    hid_t	dset1=-1, dset2=-1, dset3=-1, dset4=-1, dset5=-1,
		dset6=-1, /* dset7=-1, */ dset8=-1, dset9=-1;
    hsize_t     cur_size[5] = {2, 8, 8, 4, 2};
    hsize_t	ch_size[5] = {1, 1, 1, 4, 1};
    short	rd_s, fill_s = 0x1234;
    long	rd_l, fill_l = 0x4321;
    char	filename[1024];
    H5D_space_status_t	allocation;
    H5D_alloc_time_t    alloc_time;
    H5D_fill_time_t	fill_time;
    comp_datatype       rd_c, fill_ctype;

    if (H5D_CHUNKED==layout) {
	TESTING("chunked dataset creation");
    } else if (H5D_COMPACT==layout) {
        TESTING("compact dataset creation");
    } else {
	TESTING("contiguous dataset creation");
    }

    /*
     * Create a file.
     */
    h5_fixname(base_name, fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if ((space=H5Screate_simple(5, cur_size, cur_size))<0) goto error;
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5D_CHUNKED==layout) {
	if (H5Pset_chunk(dcpl, 5, ch_size)<0) goto error;
    } else if (H5D_COMPACT==layout) {
        if (H5Pset_layout(dcpl, H5D_COMPACT)<0) goto error;
    }

    /* Create a compound datatype */
    if((comp_type_id = create_compound_type())<0) goto error;

    /* I. Test cases for late space allocation except compact dataset */

    if(H5D_COMPACT!=layout) {
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE) < 0) goto error;
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;

        /* 1. Compound datatype test */
        if(H5Pget_fill_value(dcpl, comp_type_id, &fill_ctype)<0) goto error;
        fill_ctype.y = 4444;
        if(H5Pset_fill_value(dcpl, comp_type_id, &fill_ctype)<0) goto error;
        if((dset9 = H5Dcreate(file, "dset9", comp_type_id, space, dcpl))<0)
            goto error;

        /* The three datasets test three fill
         * conversion paths: small to large, large to small, and no conversion.
         * They depend on `short' being smaller than `long'.
         */
        /* 2. Small to large fill conversion */
#ifndef NO_FILLING
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_SHORT, &fill_s)<0) goto error;
#endif
        if ((dset1=H5Dcreate(file, "dset1", H5T_NATIVE_LONG, space, dcpl))<0)
	   goto error;

        /* 3. Large to small fill conversion */
#ifndef NO_FILLING
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_LONG, &fill_l)<0) goto error;
#endif
        if ((dset2=H5Dcreate(file, "dset2", H5T_NATIVE_SHORT, space, dcpl))<0)
	   goto error;

        /* 4. No conversion */
#ifndef NO_FILLING
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_LONG, &fill_l)<0) goto error;
#endif
        if ((dset3=H5Dcreate(file, "dset3", H5T_NATIVE_LONG, space, dcpl))<0)
	   goto error;

        /* 5. late space allocation and never write fill value */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
        if ((dset4=H5Dcreate(file, "dset4", H5T_NATIVE_LONG, space, dcpl))<0)
            goto error;

        /* 6. fill value is undefined while fill write time is H5D_FILL_TIME_ALLOC.
         * Supposed to fail. */
        if(H5Pset_fill_value(dcpl, -1, NULL)<0) goto error;
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
        H5E_BEGIN_TRY {
            if(H5Dcreate(file, "dset7", H5T_NATIVE_LONG, space, dcpl)!=FAIL)
                goto error;
        } H5E_END_TRY;
    }

    /* II. Test early space allocation cases */

    if (H5Pclose(dcpl)<0)  goto error;
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5D_CHUNKED==layout) {
        if (H5Pset_chunk(dcpl, 5, ch_size)<0) goto error;
    } else if (H5D_COMPACT==layout) {
        if (H5Pset_layout(dcpl, H5D_COMPACT)<0) goto error;
    }
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) goto error;

    /* 1. Compound datatype test */
    if(H5Pget_fill_value(dcpl, comp_type_id, &fill_ctype)<0) goto error;
    fill_ctype.y = 4444;
    if(H5Pset_fill_value(dcpl, comp_type_id, &fill_ctype)<0) goto error;
    if((dset8 = H5Dcreate(file, "dset8", comp_type_id, space, dcpl))<0)
        goto error;


    if(H5Pset_fill_value(dcpl, H5T_NATIVE_LONG, &fill_l)<0) goto error;

    /* 2. Never write fill value */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
    if((dset5 = H5Dcreate(file, "dset5", H5T_NATIVE_INT, space, dcpl))<0)
        goto error;

    /* 3. Write fill value at space allocation time */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    if((dset6 = H5Dcreate(file, "dset6", H5T_NATIVE_LONG, space, dcpl))<0)
	goto error;

    /* 4. fill value is undefined while fill write time is H5D_FILL_TIME_ALLOC.
     * Supposed to fail. */
    if(H5Pset_fill_value(dcpl, -1, NULL)<0) goto error;
    H5E_BEGIN_TRY {
        if(H5Dcreate(file, "dset7", H5T_NATIVE_LONG, space, dcpl)!=FAIL)
            goto error;
    } H5E_END_TRY;

    /* Close everything */
    if(H5D_COMPACT != layout) {
        if (H5Dclose(dset1)<0) goto error;
        if (H5Dclose(dset2)<0) goto error;
        if (H5Dclose(dset3)<0) goto error;
        if (H5Dclose(dset4)<0) goto error;
        if (H5Dclose(dset9)<0) goto error;
    }
    if (H5Dclose(dset5)<0) goto error;
    if (H5Dclose(dset6)<0) goto error;
    if (H5Dclose(dset8)<0) goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Pclose(dcpl)<0)  goto error;
    if (H5Fclose(file)<0)  goto error;

    /* Open the file and get the dataset fill value from each dataset */
    if ((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0)
	goto error;

    /* I. Check cases for late space allocation except compact dataset */
    if(H5D_COMPACT != layout) {
        /* 1. Large to small conversion */
        if ((dset1=H5Dopen(file, "dset1"))<0) goto error;
        if ((dcpl=H5Dget_create_plist(dset1))<0) goto error;
#ifndef NO_FILLING
        if (H5Pget_fill_value(dcpl, H5T_NATIVE_SHORT, &rd_s)<0) goto error;
        if (rd_s!=fill_s) {
	   H5_FAILED();
	   printf("    %d: Got a different fill value than what was set.",__LINE__);
	   printf("    Got %d, set %d\n", rd_s, fill_s);
	   goto error;
        }
#endif
        if (H5Dclose(dset1)<0) goto error;
        if (H5Pclose(dcpl)<0) goto error;

        /* 2. Small to large conversion */
        if ((dset2=H5Dopen(file, "dset2"))<0) goto error;
        if ((dcpl=H5Dget_create_plist(dset2))<0) goto error;
#ifndef NO_FILLING
        if (H5Pget_fill_value(dcpl, H5T_NATIVE_LONG, &rd_l)<0) goto error;
        if (rd_l!=fill_l) {
	   H5_FAILED();
	   printf("    %d: Got a different fill value than what was set.",__LINE__);
	   printf("    Got %ld, set %ld\n", rd_l, fill_l);
	   goto error;
        }
#endif
        if (H5Dclose(dset2)<0) goto error;
        if (H5Pclose(dcpl)<0) goto error;

        /* 3. No conversion */
        if ((dset3=H5Dopen(file, "dset3"))<0) goto error;
        if ((dcpl=H5Dget_create_plist(dset3))<0) goto error;
#ifndef NO_FILLING
        if (H5Pget_fill_value(dcpl, H5T_NATIVE_LONG, &rd_l)<0) goto error;
        if (rd_l!=fill_l) {
	   H5_FAILED();
	   printf("    %d: Got a different fill value than what was set.",__LINE__);
	   printf("    Got %ld, set %ld\n", rd_l, fill_l);
	   goto error;
        }
#endif
        if(H5Pget_alloc_time(dcpl, &alloc_time)<0) goto error;
        if(H5Pget_fill_time(dcpl, &fill_time)<0) goto error;
        if(alloc_time != H5D_ALLOC_TIME_LATE) {
            H5_FAILED();
            puts("    Got non-H5D_ALLOC_TIME_LATE space allocation time.");
            printf("    Got %d\n", alloc_time);
        }
        if(fill_time != H5D_FILL_TIME_ALLOC) {
            H5_FAILED();
            puts("    Got non-H5D_FILL_TIME_ALLOC fill value write time.");
            printf("    Got %d\n", fill_time);
        }
        if (H5Dclose(dset3)<0) goto error;
        if (H5Pclose(dcpl)<0) goto error;

        /* 4. late space allocation and never write fill value */
        if ((dset4=H5Dopen(file, "dset4"))<0) goto error;
        if (H5Dget_space_status(dset4, &allocation)<0) goto error;
        if (layout == H5D_CONTIGUOUS && allocation != H5D_SPACE_STATUS_NOT_ALLOCATED) {
            H5_FAILED();
            puts("    Got allocated space instead of unallocated.");
            printf("    Got %d\n", allocation);
            goto error;
        }
        if ((dcpl=H5Dget_create_plist(dset4))<0) goto error;
        if(H5Pget_alloc_time(dcpl, &alloc_time)<0) goto error;
        if(H5Pget_fill_time(dcpl, &fill_time)<0) goto error;
        if(alloc_time != H5D_ALLOC_TIME_LATE) {
	   H5_FAILED();
	   puts("    Got non-H5D_ALLOC_TIME_LATE space allocation time.");
	   printf("    Got %d\n", alloc_time);
        }
        if(fill_time != H5D_FILL_TIME_NEVER) {
	   H5_FAILED();
	   puts("    Got non-H5D_FILL_TIME_NEVER fill value write time.");
    	   printf("    Got %d\n", fill_time);
        }
        if (H5Dclose(dset4)<0) goto error;
        if (H5Pclose(dcpl)<0) goto error;

        /* 5. Compound datatype test */
        if ((dset9=H5Dopen(file, "dset9"))<0) goto error;
        if ((dcpl=H5Dget_create_plist(dset9))<0) goto error;
        if (H5Pget_fill_value(dcpl, comp_type_id, &rd_c)<0) goto error;
        if( rd_c.a!=0 || rd_c.y != fill_ctype.y || rd_c.x != 0 || rd_c.z != '\0') {
           H5_FAILED();
           puts("    Got wrong fill value");
           printf("    Got rd_c.a=%f, rd_c.y=%f and rd_c.x=%d, rd_c.z=%c\n",
                  rd_c.a, rd_c.y, rd_c.x, rd_c.z);
        }
        if (H5Dclose(dset9)<0) goto error;
        if (H5Pclose(dcpl)<0) goto error;
    }

    /* II. Check early space allocation cases */

    /* 1. Never write fill value */
    if ((dset5=H5Dopen(file, "dset5"))<0) goto error;
    if ((dcpl=H5Dget_create_plist(dset5))<0) goto error;
    if (H5Dget_space_status(dset5, &allocation)<0) goto error;
    if (layout == H5D_CONTIGUOUS && allocation != H5D_SPACE_STATUS_ALLOCATED) {
        H5_FAILED();
        printf("    %d: Got unallocated space instead of allocated.\n",__LINE__);
        printf("    Got %d\n", allocation);
        goto error;
    }
    if(H5Pget_alloc_time(dcpl, &alloc_time)<0) goto error;
    if(alloc_time != H5D_ALLOC_TIME_EARLY) {
        H5_FAILED();
        puts("    Got non-H5D_ALLOC_TIME_EARLY space allocation time.");
        printf("    Got %d\n", alloc_time);
    }
    if(H5Pget_fill_time(dcpl, &fill_time)<0) goto error;
    if(fill_time != H5D_FILL_TIME_NEVER) {
        H5_FAILED();
        puts("    Got non-H5D_FILL_TIME_NEVER fill value write time.");
        printf("    Got %d\n", fill_time);
    }
    if (H5Dclose(dset5)<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;

    /* 2. test writing fill value at space allocation time */
    if ((dset6=H5Dopen(file, "dset6"))<0) goto error;
    if ((dcpl=H5Dget_create_plist(dset6))<0) goto error;
    if (H5Dget_space_status(dset6, &allocation)<0) goto error;
    if (layout == H5D_CONTIGUOUS && allocation != H5D_SPACE_STATUS_ALLOCATED) {
        H5_FAILED();
        printf("    %d: Got unallocated space instead of allocated.\n",__LINE__);
        printf("    Got %d\n", allocation);
        goto error;
    }
    if (H5Pget_fill_value(dcpl, H5T_NATIVE_LONG, &rd_l)<0) goto error;
    if (rd_l!=fill_l) {
        H5_FAILED();
	printf("    %d: Got a different fill value than what was set.",__LINE__);
        printf("    Got %ld, set %ld\n", rd_l, fill_l);
        goto error;
    }
    if(H5Pget_alloc_time(dcpl, &alloc_time)<0) goto error;
    if(alloc_time != H5D_ALLOC_TIME_EARLY) {
        H5_FAILED();
        puts("    Got non-H5D_ALLOC_TIME_EARLY space allocation time.");
        printf("    Got %d\n", alloc_time);
    }
    if(H5Pget_fill_time(dcpl, &fill_time)<0) goto error;
    if(fill_time != H5D_FILL_TIME_ALLOC) {
        H5_FAILED();
        puts("    Got non-H5D_FILL_TIME_ALLOC fill value write time.");
        printf("    Got %d\n", fill_time);
    }
    if (H5Dclose(dset6)<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;

    /* 3. Compound datatype test */
    if ((dset8=H5Dopen(file, "dset8"))<0) goto error;
    if ((dcpl=H5Dget_create_plist(dset8))<0) goto error;
    if (H5Pget_fill_value(dcpl, comp_type_id, &rd_c)<0) goto error;
    if( rd_c.a != 0 || rd_c.y != fill_ctype.y || rd_c.x != 0 || rd_c.z!='\0') {
        H5_FAILED();
        puts("    Got wrong fill value");
        printf("    Got rd_c.a=%f, rd_c.y=%f and rd_c.x=%d, rd_c.z=%c\n",
		rd_c.a, rd_c.y, rd_c.x, rd_c.z);
    }
    if (H5Dclose(dset8)<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;

    if (H5Fclose(file)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Sclose(space);
        if(H5D_COMPACT != layout) {
	   H5Dclose(dset1);
	   H5Dclose(dset2);
	   H5Dclose(dset3);
           H5Dclose(dset4);
           H5Dclose(dset9);
        }
        H5Dclose(dset5);
        H5Dclose(dset6);
	H5Dclose(dset8);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	test_rdwr_cases
 *
 * Purpose:	Tests fill values read and write for datasets.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 * 		This function is called by test_rdwr to write and read
 *		dataset for different cases.
 *
 *-------------------------------------------------------------------------
 */
static int
test_rdwr_cases(hid_t file, hid_t dcpl, const char *dname, void *_fillval,
		H5D_fill_time_t fill_time, H5D_layout_t layout,
		H5T_class_t datatype, hid_t ctype_id)
{
    hid_t	fspace=-1, mspace=-1, dset1=-1, dset2=-1;
    hsize_t	cur_size[5] = {2, 8, 8, 4, 2};
    hsize_t	one[5] = {1, 1, 1, 1, 1};
    hsize_t	hs_size[5], hs_stride[5];
    hsize_t	hs_offset[5], nelmts;
    int		fillval=(-1), val_rd, should_be;
    int		i, j, *buf=NULL, odd;
    unsigned    u;
    comp_datatype       rd_c, fill_c, should_be_c;
    comp_datatype	*buf_c=NULL;
    H5D_space_status_t  allocation;

    if (datatype==H5T_INTEGER)
        fillval = *(int*)_fillval;
    else if(datatype==H5T_COMPOUND) {
	fill_c.a=((comp_datatype*)_fillval)->a;
        fill_c.x=((comp_datatype*)_fillval)->x;
        fill_c.y=((comp_datatype*)_fillval)->y;
        fill_c.z=((comp_datatype*)_fillval)->z;
    } else {
        puts("Invalid type for test");
        goto error;
    }

    /* Create dataset */
    if ((fspace=H5Screate_simple(5, cur_size, cur_size))<0) goto error;
    if (datatype==H5T_INTEGER && (dset1=H5Dcreate(file, dname, H5T_NATIVE_INT,
	fspace, dcpl))<0) goto error;
    if (datatype==H5T_COMPOUND && (dset2=H5Dcreate(file, dname, ctype_id,
        fspace, dcpl))<0) goto error;

    /* Read some data and make sure it's the fill value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0; j<5; j++)
	    hs_offset[j] = rand() % cur_size[j];
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;

   	/* case for atomic datatype */
	if (datatype==H5T_INTEGER) {
            if(H5Dread(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		&val_rd)<0) goto error;
	    if (fill_time!=H5D_FILL_TIME_NEVER && val_rd!=fillval) {
	        H5_FAILED();
	        puts("    Value read was not a fill value.");
	        HDfprintf(stdout,"    Elmt={%Hu,%Hu,%Hu,%Hu,%Hu}, read: %u, "
		       "Fill value: %u\n",
		       hs_offset[0], hs_offset[1],
		       hs_offset[2], hs_offset[3],
		       hs_offset[4], val_rd, fillval);
	        goto error;
	    }
	/* case for compound datatype */
	} else if(datatype==H5T_COMPOUND) {
            if(H5Dread(dset2, ctype_id, mspace, fspace, H5P_DEFAULT,
                &rd_c)<0) goto error;
            if (fill_time!=H5D_FILL_TIME_NEVER && (rd_c.a!=fill_c.a ||
		rd_c.x!=fill_c.x || rd_c.y!=fill_c.y ||
		rd_c.z!=fill_c.z)) {
                H5_FAILED();
                puts("    Value read was not a fill value.");
                HDfprintf(stdout,"    Elmt={%Hu,%Hu,%Hu,%Hu,%Hu}, read: %f, %d, %f, %c"
                       "Fill value: %f, %d, %f, %c\n",
                       hs_offset[0], hs_offset[1],
                       hs_offset[2], hs_offset[3],
                       hs_offset[4], rd_c.a, rd_c.x, rd_c.y, rd_c.z,
			fill_c.a, fill_c.x, fill_c.y, fill_c.z);
                goto error;
            }
        }
    }
    if (H5Sclose(mspace)<0) goto error;

    /* Select all odd data locations in the file dataset */
    for (i=0, nelmts=1; i<5; i++) {
	hs_size[i] = cur_size[i]/2;
	hs_offset[i] = 0;
	hs_stride[i] = 2;
	nelmts *= hs_size[i];
    }
    if ((mspace=H5Screate_simple(5, hs_size, hs_size))<0) goto error;
    if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, hs_stride,
                            hs_size, NULL)<0) goto error;

    /* Read non-contiguous selection from empty dataset */

    /* case for atomic datatype */
    if(datatype == H5T_INTEGER) {
        /*check for overflow*/
        assert((nelmts * sizeof(int)) == (hsize_t)((size_t)(nelmts * sizeof(int))));
        buf = malloc((size_t)(nelmts * sizeof(int)));

        if(H5Dread(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, buf) < 0)
            goto error;

        /* Verify values, except if no fill value written */
        if(fill_time != H5D_FILL_TIME_NEVER) {
            for(u = 0; u < nelmts; u++) {
                if(buf[u] != fillval) {
                    H5_FAILED();
                    puts("    Value read was not a fill value.");
                    HDfprintf(stdout,"    Elmt={%Hu, %Hu, %Hu, %Hu, %Hu}, read: %u, "
                           "Fill value: %u\n",
                           hs_offset[0], hs_offset[1],
                           hs_offset[2], hs_offset[3],
                           hs_offset[4], buf[u], fillval);
                    goto error;
                } /* end if */
            } /* end for */
        } /* end if */
    }
    /* case for compound datatype */
    else if(datatype == H5T_COMPOUND) {
        /*check for overflow*/
        assert((nelmts * sizeof(comp_datatype))==
	    (hsize_t)((size_t)(nelmts * sizeof(comp_datatype))));
	buf_c = (comp_datatype *)malloc((size_t)nelmts * sizeof(comp_datatype));

        if(H5Dread(dset2, ctype_id, mspace, fspace, H5P_DEFAULT, buf_c) < 0)
            goto error;

        /* Verify values, except if no fill value written */
        if(fill_time != H5D_FILL_TIME_NEVER) {
            for(u = 0; u < nelmts; u++) {
                if(buf_c[u].a != fill_c.a || buf_c[u].x != fill_c.x ||
                        buf_c[u].y != fill_c.y || buf_c[u].z != fill_c.z) {
                    H5_FAILED();
                    puts("    Value read was not a fill value.");
                    HDfprintf(stdout,"    Elmt={%Hu, %Hu, %Hu, %Hu, %Hu}, read: %f, %d, %f, %c"
                            "Fill value: %f, %d, %f, %c\n",
                            hs_offset[0], hs_offset[1],
                            hs_offset[2], hs_offset[3],
                            hs_offset[4],
                            buf_c[u].a, buf_c[u].x, buf_c[u].y, buf_c[u].z,
                            fill_c.a, fill_c.x, fill_c.y, fill_c.z);
                    goto error;
                } /* end if */
            } /* end for */
        } /* end if */
    }

    /* Write to all odd data locations */

    /* case for atomic datatype */
    if(datatype == H5T_INTEGER) {
        for(u = 0; u < nelmts; u++)
            buf[u] = 9999;
        if(H5Dwrite(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, buf) < 0)
            goto error;
    }
    /* case for compound datatype */
    else if(datatype == H5T_COMPOUND) {
        memset(buf_c, 0, ((size_t)nelmts * sizeof(comp_datatype)));
        for(u = 0; u < nelmts; u++) {
	    buf_c[u].a = (float)1111.11;
 	    buf_c[u].x = 2222;
	    buf_c[u].y = 3333.3333;
	    buf_c[u].z = 'd';
	}
        if(H5Dwrite(dset2, ctype_id, mspace, fspace, H5P_DEFAULT, buf_c) < 0)
            goto error;
    }

    /* Check if space is allocated */
    if (datatype==H5T_INTEGER && H5Dget_space_status(dset1, &allocation)<0)
	goto error;
    if (datatype==H5T_COMPOUND && H5Dget_space_status(dset2, &allocation)<0)
        goto error;
    if (layout == H5D_CONTIGUOUS && allocation != H5D_SPACE_STATUS_ALLOCATED) {
        H5_FAILED();
        printf("    %d: Got unallocated space instead of allocated.\n",__LINE__);
        printf("    Got %d\n", allocation);
        goto error;
    }
    free(buf);
    buf = NULL;
    H5Sclose(mspace);

    /* Read some data and make sure it's the right value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0, odd=0; j<5; j++) {
	    hs_offset[j] = rand() % cur_size[j];
	    odd += (int)(hs_offset[j]%2);
	}
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;

	/* case for atomic datatype */
        if (datatype==H5T_INTEGER) {
	    if (H5Dread(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;
            if(fill_time == H5D_FILL_TIME_ALLOC) {
                should_be = odd ? fillval : 9999;
                if (val_rd!=should_be) {
                    H5_FAILED();
                    puts("    Value read was not correct.");
                    printf("    Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
                           "should be: %u\n",
                           (long)hs_offset[0], (long)hs_offset[1],
                           (long)hs_offset[2], (long)hs_offset[3],
                           (long)hs_offset[4], val_rd, should_be);
                    goto error;
                }
	    }
	    else if(fill_time == H5D_FILL_TIME_NEVER && !odd) {
	        should_be = 9999;
	        if (val_rd!=should_be) {
	            H5_FAILED();
	            puts("    Value read was not correct.");
	            printf("    Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
		           "should be: %u\n",
		           (long)hs_offset[0], (long)hs_offset[1],
		           (long)hs_offset[2], (long)hs_offset[3],
		           (long)hs_offset[4], val_rd, should_be);
	            goto error;
	        }
	    } else if(fill_time == H5D_FILL_TIME_NEVER && odd) {
 	        /*Trash data. Don't compare*/
	    }
	} /* end for datatype==H5T_INTEGER */
	/* case for compound datatype */
	else if (datatype==H5T_COMPOUND) {
            if (H5Dread(dset2, ctype_id, mspace, fspace, H5P_DEFAULT,
                    &rd_c)<0) goto error;
            if(fill_time == H5D_FILL_TIME_ALLOC) {
		if(odd) {
		    should_be_c.a=fill_c.a;
		    should_be_c.x=fill_c.x;
		    should_be_c.y=fill_c.y;
		    should_be_c.z=fill_c.z;
		} else {
		    should_be_c.a=buf_c[0].a;
		    should_be_c.x=buf_c[0].x;
		    should_be_c.y=buf_c[0].y;
		    should_be_c.z=buf_c[0].z;
		}
		if( rd_c.a!=should_be_c.a || rd_c.x!=should_be_c.x ||
		    rd_c.y!=should_be_c.y || rd_c.z!=should_be_c.z)  {
                    H5_FAILED();
                    puts("    Value read was not correct.");
                    printf("    Elmt={%ld,%ld,%ld,%ld,%ld}, read: %f,%d,%f,%c "
                           "should be: %f,%d,%f,%c\n",
                           (long)hs_offset[0], (long)hs_offset[1],
                           (long)hs_offset[2], (long)hs_offset[3],
                           (long)hs_offset[4],
			   rd_c.a, rd_c.x, rd_c.y, rd_c.z, should_be_c.a,
		           should_be_c.x,should_be_c.y,should_be_c.z);
                    goto error;
 		}
	    } /* end for fill_time == H5D_FILL_TIME_ALLOC */
	    else if(fill_time == H5D_FILL_TIME_NEVER && !odd) {
                should_be_c.a=buf_c[0].a;
                should_be_c.x=buf_c[0].x;
                should_be_c.y=buf_c[0].y;
                should_be_c.z=buf_c[0].z;
                if( rd_c.a!=should_be_c.a || rd_c.x!=should_be_c.x ||
                    rd_c.y!=should_be_c.y || rd_c.z!=should_be_c.z)  {
                    H5_FAILED();
                    puts("    Value read was not correct.");
                    printf("    Elmt={%ld,%ld,%ld,%ld,%ld}, read: %f,%d,%f,%c "
                           "should be: %f,%d,%f,%c\n",
                           (long)hs_offset[0], (long)hs_offset[1],
                           (long)hs_offset[2], (long)hs_offset[3],
                           (long)hs_offset[4],
                           rd_c.a, rd_c.x, rd_c.y, rd_c.z, should_be_c.a,
                           should_be_c.x,should_be_c.y,should_be_c.z);
                    goto error;
                }
	    } /* end for fill_time == H5D_FILL_TIME_NEVER */
            else if(fill_time == H5D_FILL_TIME_NEVER && odd) {
                /*Trash data. Don't compare*/
            }
	} /* end for datatype==H5T_COMPOUND */
    }

    if (H5Sclose(mspace)<0) goto error;
    if (datatype==H5T_INTEGER && H5Dclose(dset1)<0) goto error;
    if (datatype==H5T_COMPOUND && H5Dclose(dset2)<0) goto error;
    if (H5Sclose(fspace)<0) goto error;
    return 0;

 error:
    H5E_BEGIN_TRY {
	if(datatype==H5T_INTEGER) H5Dclose(dset1);
	if(datatype==H5T_COMPOUND) H5Dclose(dset2);
	H5Sclose(fspace);
	H5Sclose(mspace);
    } H5E_END_TRY;

    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_rdwr
 *
 * Purpose:     Tests fill values for datasets.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *		Many new cases have been added to this test since the
 *		fill value design is modified.
 *
 *-------------------------------------------------------------------------
 */
static int
test_rdwr(hid_t fapl, const char *base_name, H5D_layout_t layout)
{
    char        filename[1024];
    hid_t 	file=-1, dcpl=-1, ctype_id=-1;
    hsize_t     ch_size[5] = {2, 8, 8, 4, 2};
    int		nerrors=0;
    int         fillval = 0x4c70f1cd;
    comp_datatype       fill_ctype={0,0,0,0};

    if (H5D_CHUNKED==layout) {
        TESTING("chunked dataset I/O");
    } else if (H5D_COMPACT==layout) {
        TESTING("compact dataset I/O");
    } else {
        TESTING("contiguous dataset I/O");
    }

    h5_fixname(base_name, fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        goto error;

    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5D_CHUNKED==layout) {
        if (H5Pset_chunk(dcpl, 5, ch_size)<0) goto error;
    } else if (H5D_COMPACT==layout) {
        if (H5Pset_layout(dcpl, H5D_COMPACT)<0) goto error;
    }
    if ((ctype_id=create_compound_type())<0) goto error;


    /* I. Test H5D_ALLOC_TIME_LATE space allocation cases */
    if(H5D_COMPACT != layout) {
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE) < 0) goto error;

        /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value to be default */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
        fillval = 0;
        nerrors += test_rdwr_cases(file, dcpl, "dset1", &fillval, H5D_FILL_TIME_ALLOC,
	           			layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_NEVER as fill write time and fill value to be default */
        if (H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset2", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is user-defined */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
        fillval = 0x4c70f1cd;
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval)<0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset3", &fillval, H5D_FILL_TIME_ALLOC,
				layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_NEVER as fill write time and fill value is user-defined */
        if (H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval)<0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset4", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is undefined */
        /* This case has been tested in test_create() function */

        /* case for H5D_FILL_TIME_NEVER as fill write time and fill value is undefined */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
        if (H5Pset_fill_value(dcpl, -1, NULL)<0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset5", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

        /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is user-defined
         * as compound type */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
        fill_ctype.y = 4444.4444;
        if(H5Pset_fill_value(dcpl, ctype_id, &fill_ctype)<0) goto error;
        nerrors += test_rdwr_cases(file, dcpl, "dset11", &fill_ctype, H5D_FILL_TIME_ALLOC,
				layout, H5T_COMPOUND, ctype_id);

        if (H5Pclose(dcpl)<0) goto error;
        if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
        if (H5D_CHUNKED==layout) {
            if (H5Pset_chunk(dcpl, 5, ch_size)<0) goto error;
        }
    }


    /* II.  Test H5D_ALLOC_TIME_EARLY space allocation cases */
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) goto error;

    /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value to be default */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    fillval = 0;
    nerrors += test_rdwr_cases(file, dcpl, "dset6", &fillval, H5D_FILL_TIME_ALLOC,
				layout, H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_NEVER as fill write time and fill value to be default */
    if (H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset7", &fillval, H5D_FILL_TIME_NEVER, layout,
        			H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is user-defined */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    fillval = 0x4c70f1cd;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval)<0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset8", &fillval, H5D_FILL_TIME_ALLOC,
				layout, H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_NEVER as fill write time and fill value is user-defined */
    if (H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval)<0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset9", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is undefined */
    /* This case has been tested in test_create() function */

    /* case for H5D_FILL_TIME_NEVER as fill write time and fill value is undefined */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) goto error;
    if (H5Pset_fill_value(dcpl, -1, NULL)<0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset10", &fillval, H5D_FILL_TIME_NEVER,
				layout, H5T_INTEGER, -1);

    /* case for H5D_FILL_TIME_ALLOC as fill write time and fill value is user-defined
     * as compound type */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    fill_ctype.y = 4444.4444;
    if(H5Pset_fill_value(dcpl, ctype_id, &fill_ctype)<0) goto error;
    nerrors += test_rdwr_cases(file, dcpl, "dset12", &fill_ctype, H5D_FILL_TIME_ALLOC,
                                layout, H5T_COMPOUND, ctype_id);


    if(nerrors)
	goto error;
    if (H5Pclose(dcpl)<0) goto error;
    if (H5Tclose(ctype_id)<0) goto error;
    if (H5Fclose(file)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
	H5Tclose(ctype_id);
        H5Fclose(file);
    } H5E_END_TRY;
    return nerrors;
}


/*-------------------------------------------------------------------------
 * Function:	test_extend
 *
 * Purpose:	Test that filling works okay when a dataset is extended.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, October  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend(hid_t fapl, const char *base_name, H5D_layout_t layout)
{
    hid_t	file=-1, fspace=-1, mspace=-1, dcpl=-1, dset=-1;
    hsize_t	cur_size[5] = {32, 16, 8, 4, 2};
    hsize_t	max_size[5] = {128, 64, 32, 16, 8};
    hsize_t	ch_size[5] = {1, 16, 8, 4, 2};
    hsize_t	one[5] = {1, 1, 1, 1, 1};
    hsize_t	hs_size[5], hs_stride[5];
    hsize_t	hs_offset[5], nelmts;
#ifdef NO_FILLING
    int		fillval = 0;
#else
    int		fillval = 0x4c70f1cd;
#endif
    int		val_rd, should_be;
    int		i, j, *buf=NULL, odd, fd;
    unsigned    u;
    char	filename[1024];

    if (H5D_CHUNKED==layout) {
	TESTING("chunked dataset extend");
    } else {
	TESTING("contiguous dataset extend");
    }

    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5D_CHUNKED==layout) {
	if (H5Pset_chunk(dcpl, 5, ch_size)<0) goto error;
    }
#ifndef NO_FILLING
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval)<0) goto error;
#endif

#if 1
    /*
     * Remove this once contiguous datasets can support extensions in other
     * than the slowest varying dimension.  The purpose of this block is to
     * make only the slowest varying dimension extendible and yet have the
     * same total number of elements as originally.
     *
     * If this is removed prematurely then you will get an error `only the
     * first dimension can be extendible' as long as the test isn't skipped
     * below.
     */
    if (H5D_CONTIGUOUS==layout) {
	max_size[0] = (max_size[0]*max_size[1]*max_size[2]*
		       max_size[3]*max_size[4]) /
		      (cur_size[1]*cur_size[2]*cur_size[3]*cur_size[4]);
	max_size[1] = cur_size[1];
	max_size[2] = cur_size[2];
	max_size[3] = cur_size[3];
	max_size[4] = cur_size[4];
    }
#endif

#if 1
    /*
     * Remove this once internal contiguous datasets can support
     * extending. If it's removed prematurely you will get an error
     * `extendible contiguous non-external dataset' as long as the test isn't
     * skipped below.
     */
    if (H5D_CONTIGUOUS==layout) {
	nelmts = max_size[0]*max_size[1]*max_size[2]*max_size[3]*max_size[4];
	if ((fd=open(FILE_NAME_RAW, O_RDWR|O_CREAT|O_TRUNC, 0666))<0 ||
	    close(fd)<0) goto error;
	if (H5Pset_external(dcpl, FILE_NAME_RAW, (off_t)0, (hsize_t)nelmts*sizeof(int))<0)
	    goto error;
    }
#endif

#if 1
    /*
     * Remove this when contiguous datasets can be exended to some
     * predetermined fininte size, even if it's just in the slowest varying
     * dimension.  If it's removed prematurely then you'll get one of the
     * errors described above or `unable to select fill value region'.
     */
    if (H5D_CONTIGUOUS==layout) {
	SKIPPED();
	puts("    Not implemented yet -- needs H5S_SELECT_DIFF operator");
	goto skip;
    }
#endif

    /* Create a file and dataset */
    h5_fixname(base_name, fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if ((fspace=H5Screate_simple(5, cur_size, max_size))<0) goto error;
    if ((dset=H5Dcreate(file, "dset", H5T_NATIVE_INT, fspace, dcpl))<0)
	goto error;

    /* Read some data and make sure it's the fill value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0; j<5; j++) {
	    hs_offset[j] = rand() % cur_size[j];
	}
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;
	if (H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;
	if (val_rd!=fillval) {
	    H5_FAILED();
	    puts("    Value read was not a fill value.");
	    HDfprintf(stdout,"    Elmt={%Hu,%Hu,%Hu,%Hu,%Hu}, read: %u, "
		   "Fill value: %u\n",
		   hs_offset[0], hs_offset[1],
		   hs_offset[2], hs_offset[3],
		   hs_offset[4], val_rd, fillval);
	    goto error;
	}
    }
    if (H5Sclose(mspace)<0) goto error;

    /* Write to all odd data locations */
    for (i=0, nelmts=1; i<5; i++) {
	hs_size[i] = cur_size[i]/2;
	hs_offset[i] = 0;
	hs_stride[i] = 2;
	nelmts *= hs_size[i];
    }
    if ((mspace=H5Screate_simple(5, hs_size, hs_size))<0) goto error;
    assert((nelmts*sizeof(int))==(hsize_t)((size_t)(nelmts*sizeof(int)))); /*check for overflow*/
    buf = malloc((size_t)(nelmts*sizeof(int)));
    for (u=0; u<nelmts; u++) buf[u] = 9999;
    if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, hs_stride,
			    hs_size, NULL)<0) goto error;
    if (H5Dwrite(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		 buf)<0) goto error;
    free(buf);
    buf = NULL;
    H5Sclose(mspace);

    /* Read some data and make sure it's the right value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0, odd=0; j<5; j++) {
	    hs_offset[j] = rand() % cur_size[j];
	    odd += (int)(hs_offset[j]%2);
	}
	should_be = odd ? fillval : 9999;
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;
	if (H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;

	if (val_rd!=should_be) {
	    H5_FAILED();
	    puts("    Value read was not correct.");
	    HDfprintf(stdout,"    Elmt={%Hu,%Hu,%Hu,%Hu,%Hu}, read: %u, "
		   "should be: %u\n",
		   hs_offset[0], hs_offset[1],
		   hs_offset[2], hs_offset[3],
		   hs_offset[4], val_rd, should_be);
	    goto error;
	}
    }
    if (H5Sclose(mspace)<0) goto error;

    /* Extend the dataset */
    if (H5Dextend(dset, max_size)<0) goto error;
    if (H5Sclose(fspace)<0) goto error;
    if ((fspace=H5Dget_space(dset))<0) goto error;

    /* Read some data and make sure it's the right value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0, odd=0; j<5; j++) {
	    hs_offset[j] = rand() % max_size[j];
	    if ((hsize_t)hs_offset[j]>=cur_size[j]) {
		odd = 1;
	    } else {
  		odd += (int)(hs_offset[j]%2);
	    }
	}

	should_be = odd ? fillval : 9999;
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;
	if (H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;

	if (val_rd!=should_be) {
	    H5_FAILED();
	    puts("    Value read was not correct.");
	    HDfprintf(stdout,"    Elmt={%Hu,%Hu,%Hu,%Hu,%Hu}, read: %u, "
		   "should be: %u\n",
		   hs_offset[0], hs_offset[1],
		   hs_offset[2], hs_offset[3],
		   hs_offset[4], val_rd, should_be);
	    goto error;
	}
    }
    if (H5Sclose(mspace)<0) goto error;

    if (H5Dclose(dset)<0) goto error;
    if (H5Sclose(fspace)<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;
    if (H5Fclose(file)<0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(fspace);
	H5Sclose(mspace);
	H5Pclose(dcpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;

 skip:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(fspace);
	H5Sclose(mspace);
	H5Pclose(dcpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    test_compatible
 *
 * Purpose:     Tests fill value and dataspace for datasets created by v1.4
 *              library.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Raymond Lu
 *              Feb 27, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_compatible(void)
{
  hid_t      file=-1, dset1=-1, dset2=-1;
  hid_t      dcpl1=-1, dcpl2=-1, fspace=-1, mspace=-1;
  int        rd_fill=0, fill_val=4444, val_rd=0;
  hsize_t    dims[2], one[2]={1,1};
  hsize_t   hs_offset[2]={3,4};
  H5D_fill_value_t status;
  char       *srcdir = getenv("srcdir"); /*where the src code is located*/
  char       testfile[512]="";  /* test file name */

  TESTING("contiguous dataset compatibility with v. 1.4");

  /* Generate correct name for test file by prepending the source path */
  if(srcdir && ((strlen(srcdir) + strlen(FILE_COMPATIBLE) + 1) <
     sizeof(testfile))) {
     strcpy(testfile, srcdir);
     strcat(testfile, "/");
  }
  strcat(testfile, FILE_COMPATIBLE);

  if ((file=H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT))<0) {
      printf("    Could not open file %s. Try set $srcdir to point at the "
	      "source directory of test\n", testfile);
      goto error;
  }

  if((dset1=H5Dopen(file, "dset1"))<0) goto error;
  if ((dcpl1=H5Dget_create_plist(dset1))<0) goto error;
  if (H5Pfill_value_defined(dcpl1, &status)<0) goto error;
  if(status != H5D_FILL_VALUE_UNDEFINED) {
      H5_FAILED();
      printf("    %d: Got a different fill value than what was set.",__LINE__);
      printf("    Got status=%ld, suppose to be H5D_FILL_VALUE_UNDEFINED\n",
	    (long)status);
      goto error;
  }
  if((fspace = H5Dget_space(dset1))<0) goto error;
  if(H5Sget_simple_extent_dims(fspace, dims, NULL)<0) goto error;
  if(dims[0] != 8 || dims[1] != 8) {
      H5_FAILED();
      puts("    Got a different dimension size than what was set.");
      printf("    Got dims[0]=%ld, dims[1]=%ld, set 8x8\n", (long)dims[0], (long)dims[1]);
      goto error;
  }
  if((mspace=H5Screate_simple(2, one, NULL))<0) goto error;
  if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL)<0)
      goto error;
  if(H5Dread(dset1, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, &val_rd)<0)
      goto error;
  if (val_rd != 0) {
      H5_FAILED();
      puts("    Got a different value than what was set.");
      printf("    Got %ld, set 0\n", (long)val_rd);
      goto error;
  }
  if(H5Pclose(dcpl1)<0) goto error;
  if(H5Sclose(fspace)<0) goto error;
  if(H5Sclose(mspace)<0) goto error;
  if(H5Dclose(dset1)<0) goto error;


  if((dset2=H5Dopen(file, "dset2"))<0) goto error;
  if ((dcpl2=H5Dget_create_plist(dset2))<0) goto error;
  if (H5Pfill_value_defined(dcpl2, &status)<0) goto error;
  if(status != H5D_FILL_VALUE_USER_DEFINED) {
      H5_FAILED();
      printf("    %d: Got a different fill value than what was set.",__LINE__);
      printf("    Got status=%ld, suppose to be H5D_FILL_VALUE_USER_DEFINED\n",
            (long)status);
      goto error;
  }
  if (H5Pget_fill_value(dcpl2, H5T_NATIVE_INT, &rd_fill)<0) goto error;
  if (rd_fill != fill_val) {
      H5_FAILED();
      printf("    %d: Got a different fill value than what was set.",__LINE__);
      printf("    Got %ld, set %ld\n", (long)rd_fill, (long)fill_val);
      goto error;
  }
  fspace = -1;
  if((fspace = H5Dget_space(dset2))<0) goto error;
  dims[0] = dims[1] = (hsize_t)-1;
  if(H5Sget_simple_extent_dims(fspace, dims, NULL)<0) goto error;
  if(dims[0] != 8 || dims[1] != 8) {
      H5_FAILED();
      puts("    Got a different dimension size than what was set.");
      printf("    Got dims[0]=%ld, dims[1]=%ld, set 8x8\n", (long)dims[0], (long)dims[1]);
      goto error;
  }
  if((mspace=H5Screate_simple(2, one, NULL))<0) goto error;
  if(H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL, one, NULL)<0)
      goto error;
  if(H5Dread(dset2, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, &val_rd)<0)
      goto error;
  if (val_rd != fill_val) {
      H5_FAILED();
      puts("    Got a different value than what was set.");
      printf("    Got %ld, set %ld\n", (long)val_rd, (long)fill_val);
      goto error;
  }
  if(H5Pclose(dcpl2)<0) goto error;
  if(H5Sclose(fspace)<0) goto error;
  if(H5Sclose(mspace)<0) goto error;
  if(H5Dclose(dset2)<0) goto error;

  if(H5Fclose(file)<0) goto error;
  PASSED();
  return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl1);
        H5Sclose(fspace);
        H5Sclose(mspace);
        H5Dclose(dset1);
        H5Pclose(dcpl2);
        H5Sclose(fspace);
        H5Dclose(dset2);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests fill values
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    int		nerrors=0, argno, test_contig=1, test_chunk=1, test_compact=1;
    hid_t	fapl=-1;

    if (argc>=2) {
	test_contig = test_chunk = test_compact = 0;
	for (argno=1; argno<argc; argno++) {
	    if (!strcmp(argv[argno], "contiguous")) {
		test_contig = 1;
	    } else if (!strcmp(argv[argno], "chunked")) {
		test_chunk = 1;
	    } else if (!strcmp(argv[argno], "compact")) {
                test_compact =1;
            } else {
		fprintf(stderr, "usage: %s [contiguous] [chunked] [compact]\n", argv[0]);
		exit(1);
	    }
	}
    }

    h5_reset();
    fapl = h5_fileaccess();

    nerrors += test_getset();
    nerrors += test_getset_vl(fapl);

    /* Chunked storage layout tests */
    if (test_chunk) {
	nerrors += test_create(fapl, FILENAME[0], H5D_CHUNKED);
	nerrors += test_rdwr  (fapl, FILENAME[2], H5D_CHUNKED);
	nerrors += test_extend(fapl, FILENAME[4], H5D_CHUNKED);
    }

    /* Contiguous storage layout tests */
    if (test_contig) {
	nerrors += test_create(fapl, FILENAME[1], H5D_CONTIGUOUS);
	nerrors += test_rdwr  (fapl, FILENAME[3], H5D_CONTIGUOUS);
	nerrors += test_extend(fapl, FILENAME[5], H5D_CONTIGUOUS);
	nerrors += test_compatible();
    }

    /* Compact dataset storage tests */
    if (test_compact) {
        nerrors += test_create(fapl, FILENAME[6], H5D_COMPACT);
        nerrors += test_rdwr  (fapl, FILENAME[7], H5D_COMPACT);
    }

    if (nerrors) goto error;
    puts("All fill value tests passed.");
    if (h5_cleanup(FILENAME, fapl)) remove(FILE_NAME_RAW);
    return 0;

 error:
    puts("***** FILL VALUE TESTS FAILED *****");
    return 1;
}
