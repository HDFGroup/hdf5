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
 * Programmer:	Raymond Lu
 *              October 14, 2001
 *
 * Purpose:	Tests the H5Tget_native_type function.
 */

#include "h5test.h"

const char *FILENAME[] = {
    "ntypes",
    NULL
};

#define DIM0    100
#define DIM1    200
#define DIM3    20


int	ipoints2[DIM0][DIM1], icheck2[DIM0][DIM1];
short	spoints2[DIM0][DIM1], scheck2[DIM0][DIM1];
int	ipoints3[DIM0][DIM1][5], icheck3[DIM0][DIM1][5];

#define DSET_ATOMIC_NAME_1	"atomic_type_1"
#define DSET_ATOMIC_NAME_2	"atomic_type_2"
#define DSET_ATOMIC_NAME_3	"atomic_type_3"
#define DSET_ATOMIC_NAME_4	"atomic_type_4"
#define DSET_ATOMIC_NAME_5	"atomic_type_5"
#define DSET_COMPOUND_NAME      "compound_type"
#define DSET_COMPOUND_NAME_2    "compound_type_2"
#define DSET_COMPOUND_NAME_3    "compound_type_3"
#define DSET_COMPOUND_NAME_4    "compound_type_4"
#define DSET_ENUM_NAME	        "enum_type"
#define DSET_ARRAY_NAME	        "array_type"
#define DSET_ARRAY2_NAME	"array_type_2"
#define DSET_VL_NAME	        "vl_type"
#define DSET_VLSTR_NAME         "vlstr_type"
#define DSET_STR_NAME           "str_type"
#define DSET_OPAQUE_NAME        "opaque_type"
#define DSET_BITFIELD_NAME      "bitfield_type"

#define SPACE1_DIM1             4
#define SPACE1_RANK             1
#define SPACE2_RANK	        2
#define SPACE2_DIM1	        10
#define SPACE2_DIM2	        10


/*-------------------------------------------------------------------------
 * Function:	test_atomic_dtype
 *
 * Purpose:	Test H5Tget_native_type for atomic datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_atomic_dtype(hid_t file)
{
    hid_t		dataset, space;
    hid_t               dtype, native_type;
    int			i, j, n;
    hsize_t		dims[2];
    void                *tmp;

    TESTING("atomic datatype");

    /* Initialize the dataset */
    for (i = n = 0; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++) {
	    ipoints2[i][j] = n++;
	}
    }

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL))<0) TEST_ERROR;

    /*------------------- Test data values ------------------------*/
    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_1, H5T_STD_I32BE, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ipoints2)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_ATOMIC_NAME_1))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_INT))
        TEST_ERROR;
    if(H5Tget_size(native_type) < H5Tget_size(H5T_STD_I32BE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(native_type))
        TEST_ERROR;

    /* Read the dataset back.  The temporary buffer is for special platforms
     * like Cray. */
    tmp = malloc((size_t)(DIM0*DIM1*H5Tget_size(native_type)));

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp)<0)
	TEST_ERROR;

    /* Copy data from temporary buffer to destination buffer */
    memcpy(icheck2, tmp, (size_t)(DIM0*DIM1*H5Tget_size(native_type)));
    free(tmp);

    /* Convert to the integer type */
    if(H5Tconvert(native_type, H5T_NATIVE_INT, (DIM0*DIM1), icheck2, NULL, H5P_DEFAULT)<0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++) {
	    if (ipoints2[i][j] != icheck2[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    if(H5Dclose(dataset)<0) TEST_ERROR;
    if(H5Tclose(native_type)<0) TEST_ERROR;
    if(H5Tclose(dtype)<0) TEST_ERROR;

    /*------------------ Test different data types ----------------*/

    /* Create the dataset of H5T_STD_I64LE */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_2, H5T_STD_I64LE, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_LLONG))
        TEST_ERROR;
    if(H5Tget_size(native_type) < H5Tget_size(H5T_STD_I64LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(native_type))
        TEST_ERROR;

    if(H5Dclose(dataset)<0) TEST_ERROR;
    if(H5Tclose(native_type)<0) TEST_ERROR;
    if(H5Tclose(dtype)<0) TEST_ERROR;


    /* Create the dataset of H5T_STD_I8LE */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_3, H5T_STD_I8LE, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_ASCEND))<0)
        TEST_ERROR;

    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_CHAR))
        TEST_ERROR;
    if(H5Tget_size(native_type) < H5Tget_size(H5T_STD_I8LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(native_type))
        TEST_ERROR;

    if(H5Dclose(dataset)<0) TEST_ERROR;
    if(H5Tclose(native_type)<0) TEST_ERROR;
    if(H5Tclose(dtype)<0) TEST_ERROR;


    /* Create the dataset of H5T_IEEE_F32BE */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_4, H5T_IEEE_F32BE, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DESCEND))<0)
        TEST_ERROR;

    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_FLOAT))
        TEST_ERROR;
    if(H5Tget_size(native_type) < H5Tget_size(H5T_IEEE_F32BE))
        TEST_ERROR;
    if(H5T_FLOAT!=H5Tget_class(native_type))
        TEST_ERROR;

    if(H5Dclose(dataset)<0) TEST_ERROR;
    if(H5Tclose(native_type)<0) TEST_ERROR;
    if(H5Tclose(dtype)<0) TEST_ERROR;


    /* Create the dataset of H5T_IEEE_F64BE */
    if ((dataset = H5Dcreate(file, DSET_ATOMIC_NAME_5, H5T_IEEE_F64BE, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DESCEND))<0)
        TEST_ERROR;

    /* Verify the datatype retrieved and converted */
    if(H5Tget_order(native_type) != H5Tget_order(H5T_NATIVE_DOUBLE))
        TEST_ERROR;
    if(H5Tget_size(native_type) < H5Tget_size(H5T_IEEE_F64BE))
        TEST_ERROR;
    if(H5T_FLOAT!=H5Tget_class(native_type))
        TEST_ERROR;

    if(H5Dclose(dataset)<0) TEST_ERROR;
    if(H5Tclose(native_type)<0) TEST_ERROR;
    if(H5Tclose(dtype)<0) TEST_ERROR;


    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;

    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_dtype2
 *
 * Purpose:	Test H5Tget_native_type for compound datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compound_dtype2(hid_t file)
{
    typedef struct s2 {
        short           c2;
        long            l2;
    } s2;
    typedef struct s1 {
        char            c;
        int             i;
        s2              st;
        unsigned long_long       l;
    } s1;
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2, tid_m, tid_m2,
                        mem_id, nest_mem_id;
    int			i, j, n;
    hsize_t		dims[2];
    s1                 *temp_point, *temp_check;
    s1 	               *points=NULL, *check=NULL;
    void                *tmp, *bkg;

    TESTING("nested compound datatype");

    /* Allocate space for the points & check arrays */
    if((points=malloc(sizeof(s1)*DIM0*DIM1))==NULL)
        TEST_ERROR;
    if((check=calloc(sizeof(s1),DIM0*DIM1))==NULL)
        TEST_ERROR;

    /* Initialize the dataset */
    for (i = n = 0, temp_point=points; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++,temp_point++) {
	    temp_point->c = 't';
	    temp_point->i = n++;
	    temp_point->st.c2 = i+j;
	    temp_point->st.l2 = (i*5+j*50)*n;
	    temp_point->l = (i*10+j*100)*n;
	}
    }

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL))<0) TEST_ERROR;

    /* Create compound datatype for disk storage */
#if H5_SIZEOF_LONG==4
    if((tid2=H5Tcreate(H5T_COMPOUND, 6))<0) TEST_ERROR;
    if((tid=H5Tcreate(H5T_COMPOUND, 19))<0) TEST_ERROR;
#elif H5_SIZEOF_LONG==8
    if((tid2=H5Tcreate(H5T_COMPOUND, 10))<0) TEST_ERROR;
    if((tid=H5Tcreate(H5T_COMPOUND, 23))<0) TEST_ERROR;
#else
#error "Unknown 'long' size"
#endif

    /* Insert and pack members */
    if(H5Tinsert(tid2, "c2", 0, H5T_STD_I16BE)<0) TEST_ERROR;
#if H5_SIZEOF_LONG==4
    if(H5Tinsert(tid2, "l2", 2, H5T_STD_I32LE)<0) TEST_ERROR;
#elif H5_SIZEOF_LONG==8
    if(H5Tinsert(tid2, "l2", 2, H5T_STD_I64LE)<0) TEST_ERROR;
#else
#error "Unknown 'long' size"
#endif

    if(H5Tinsert(tid, "c", 0, H5T_STD_U8LE)<0) TEST_ERROR;
    if(H5Tinsert(tid, "i", 1, H5T_STD_I32LE)<0) TEST_ERROR;
    if(H5Tinsert(tid, "st", 5, tid2)<0) TEST_ERROR;
#if H5_SIZEOF_LONG==4
    if(H5Tinsert(tid, "l", 11, H5T_STD_U64BE)<0) TEST_ERROR;
#elif H5_SIZEOF_LONG==8
    if(H5Tinsert(tid, "l", 15, H5T_STD_U64BE)<0) TEST_ERROR;
#else
#error "Unknown 'long' size"
#endif

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_COMPOUND_NAME_2, tid, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Create compound datatype for memory */
    if((tid_m2=H5Tcreate(H5T_COMPOUND, sizeof(s2)))<0) TEST_ERROR;
    if((tid_m=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) TEST_ERROR;

    /* Insert members */
    if(H5Tinsert(tid_m2, "c2", HOFFSET(s2, c2), H5T_NATIVE_SHORT)<0) TEST_ERROR;
    if(H5Tinsert(tid_m2, "l2", HOFFSET(s2, l2), H5T_NATIVE_LONG)<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "c", HOFFSET(s1, c), H5T_NATIVE_UCHAR)<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "i", HOFFSET(s1, i), H5T_NATIVE_INT)<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "st", HOFFSET(s1, st), tid_m2)<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "l", HOFFSET(s1, l), H5T_NATIVE_ULLONG)<0) TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_COMPOUND_NAME_2))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Verify the datatype of each field retrieved and converted */
    /* check the char member */
    if((mem_id = H5Tget_member_type(native_type, 0))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_UCHAR))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_U8LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* check the integer member */
    if((mem_id = H5Tget_member_type(native_type, 1))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_INT))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_I32LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* check the long long member */
    if((mem_id = H5Tget_member_type(native_type, 3))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_ULLONG))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_U64BE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* check the nested compound member */
    if((nest_mem_id = H5Tget_member_type(native_type, 2))<0)
        TEST_ERROR;

    if((mem_id = H5Tget_member_type(nest_mem_id, 0))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_SHORT))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_I16BE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    if((mem_id = H5Tget_member_type(nest_mem_id, 1))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_LONG))
        TEST_ERROR;
#if H5_SIZEOF_LONG==4
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_I32LE)) TEST_ERROR;
#elif H5_SIZEOF_LONG==8
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_I64LE)) TEST_ERROR;
#else
#error "Unknown 'long' size"
#endif
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* Read the dataset back.  Temporary buffer is for special platforms like
     * Cray */
    tmp = malloc(DIM0*DIM1*H5Tget_size(native_type));
    if((bkg=calloc(sizeof(s1),DIM0*DIM1))==NULL)
        TEST_ERROR;

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp)<0)
	TEST_ERROR;

    memcpy(check, tmp, DIM0*DIM1*H5Tget_size(native_type));
    free(tmp);

    if (H5Tconvert(native_type, tid_m, (DIM0*DIM1), check, bkg, H5P_DEFAULT))
        TEST_ERROR;

    free(bkg);

    /* Check that the values read are the same as the values written */
    for (i = 0, temp_point=points, temp_check=check; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++, temp_point++,temp_check++) {
	    if (temp_point->c != temp_check->c ||
	        temp_point->i != temp_check->i ||
	        temp_point->st.c2 != temp_check->st.c2 ||
	        temp_point->st.l2 != temp_check->st.l2 ||
	        temp_point->l != temp_check->l ) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    /* Close temporary datatypes */
    if(H5Tclose(tid2)<0) TEST_ERROR;
    if(H5Tclose(tid)<0) TEST_ERROR;
    if(H5Tclose(tid_m2)<0) TEST_ERROR;

    /* Close HDF5 objects */
    H5Dclose(dataset);
    H5Tclose(dtype);
    H5Tclose(native_type);
    H5Tclose(tid_m);

    /* Free memory for test data */
    free(points);
    free(check);

    PASSED();
    return 0;

error:
    if(points!=NULL)
        free(points);
    if(check!=NULL)
        free(check);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_dtype
 *
 * Purpose:	Test H5Tget_native_type for compound datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compound_dtype(hid_t file)
{
    typedef struct {
        char            c;
        unsigned int    i;
        long_long       l;
    } s1;
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2, mem_id;
    int			i, j, n;
    hsize_t		dims[2];
    s1                  *temp_point, *temp_check;
    s1	                *points, *check;
    void                *tmp, *bkg;

    TESTING("compound datatype");


    /* Allocate space for the points & check arrays */
    if((points=malloc(sizeof(s1)*DIM0*DIM1))==NULL)
        TEST_ERROR;
    if((check=calloc(sizeof(s1),DIM0*DIM1))==NULL)
        TEST_ERROR;

    /* Initialize the dataset */
    for (i = n = 0, temp_point=points; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++,temp_point++) {
	    temp_point->c = 't';
	    temp_point->i = n++;
	    temp_point->l = (i*10+j*100)*n;
	}
    }

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL))<0) TEST_ERROR;

    /* Create compound datatype for disk storage */
    if((tid=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) TEST_ERROR;

    /* Insert members */
    if(H5Tinsert(tid, "c", 0, H5T_STD_U8LE)<0) TEST_ERROR;
    if(H5Tinsert(tid, "i", 1, H5T_STD_U32LE)<0) TEST_ERROR;
    if(H5Tinsert(tid, "l", 5, H5T_STD_I64BE)<0) TEST_ERROR;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_COMPOUND_NAME, tid, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Create compound datatype for datatype in memory */
    if((tid2=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) TEST_ERROR;
    if(H5Tinsert(tid2, "c", HOFFSET(s1, c), H5T_NATIVE_UCHAR)<0) TEST_ERROR;
    if(H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_UINT)<0) TEST_ERROR;
    if(H5Tinsert(tid2, "l", HOFFSET(s1, l), H5T_NATIVE_LLONG)<0) TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_COMPOUND_NAME))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Verify the datatype of each field retrieved and converted */
    if((mem_id = H5Tget_member_type(native_type, 0))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_UCHAR))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_U8LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    if((mem_id = H5Tget_member_type(native_type, 1))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_UINT))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_U32LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    if((mem_id = H5Tget_member_type(native_type, 2))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_LLONG))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_I64BE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* Read the dataset back.  Temporary buffer is for special platforms like
     * Cray */
    tmp = malloc(DIM0*DIM1*H5Tget_size(native_type));
    bkg = calloc(sizeof(s1),DIM0*DIM1);

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp)<0)
	TEST_ERROR;

    memcpy(check, tmp, DIM0*DIM1*H5Tget_size(native_type));
    free(tmp);

    if (H5Tconvert(native_type, tid2, (DIM0*DIM1), check, bkg, H5P_DEFAULT)<0)
        TEST_ERROR;

    free(bkg);

    /* Check that the values read are the same as the values written */
    for (i = 0, temp_point=points, temp_check=check; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++, temp_point++,temp_check++) {
	    if (temp_point->c != temp_check->c ||
	        temp_point->i != temp_check->i ||
	        temp_point->l != temp_check->l ) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    /* Close datatype */
    if(H5Tclose(tid)<0) TEST_ERROR;

    H5Dclose(dataset);
    H5Tclose(dtype);
    H5Tclose(native_type);
    H5Tclose(tid2);

    /* Free memory for test data */
    free(points);
    free(check);

    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_dtype3
 *
 * Purpose:	Test H5Tget_native_type for compound datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compound_dtype3(hid_t file)
{
    typedef struct {
        char            c;
        int             a[5];
        long_long       l;
    } s1;
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2, tid_m, tid_m2,
                        mem_id, nest_mem_id;
    hsize_t             array_dims[1]={5};
    int			i, j, k, n;
    hsize_t		dims[2];
    s1                  *temp_point, *temp_check;
    s1 	                *points=NULL, *check=NULL;
    void                *tmp, *bkg;

    TESTING("compound datatype with array as field");

    /* Allocate space for the points & check arrays */
    if((points=malloc(sizeof(s1)*DIM0*DIM1))==NULL)
        TEST_ERROR;
    if((check=calloc(sizeof(s1),DIM0*DIM1))==NULL)
        TEST_ERROR;

    /* Initialize the dataset */
    for (i = n = 0, temp_point=points; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++,temp_point++) {
	    temp_point->c = 't';
	    temp_point->l = (i*10+j*100)*n;
	    for (k = 0; k < 5; k++)
	        (temp_point->a)[k] = n++;
	}
    }

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL))<0) TEST_ERROR;

    /* Create array datatype */
    if((tid2=H5Tarray_create(H5T_STD_I32LE, 1, array_dims, NULL))<0) TEST_ERROR;

    /* Create compound datatype for disk storage */
    if((tid=H5Tcreate(H5T_COMPOUND, 29))<0) TEST_ERROR;

    /* Insert members */
    if(H5Tinsert(tid, "c", 0, H5T_STD_U8LE)<0) TEST_ERROR;
    if(H5Tinsert(tid, "a", 1, tid2)<0) TEST_ERROR;
    if(H5Tinsert(tid, "l", 21, H5T_STD_I64BE)<0) TEST_ERROR;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_COMPOUND_NAME_3, tid, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Create array datatype */
    if((tid_m2=H5Tarray_create(H5T_NATIVE_INT, 1, array_dims, NULL))<0) TEST_ERROR;

    /* Create compound datatype for datatype in memory */
    if((tid_m=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "c", HOFFSET(s1, c), H5T_NATIVE_UCHAR)<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "a", HOFFSET(s1, a), tid_m2)<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "l", HOFFSET(s1, l), H5T_NATIVE_LLONG)<0) TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid)<0) TEST_ERROR;
    if(H5Tclose(tid2)<0) TEST_ERROR;

    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_COMPOUND_NAME_3))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Verify the datatype of each field retrieved and converted */
    /* check the char member */
    if((mem_id = H5Tget_member_type(native_type, 0))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_UCHAR))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_U8LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* check the array member */
    if((mem_id = H5Tget_member_type(native_type, 1))<0)
        TEST_ERROR;
    if(H5T_ARRAY!=H5Tget_class(mem_id))
        TEST_ERROR;
    if((nest_mem_id = H5Tget_super(mem_id))<0)
        TEST_ERROR;
    if(H5Tget_order(nest_mem_id) != H5Tget_order(H5T_NATIVE_INT))
        TEST_ERROR;
    if(H5Tget_size(nest_mem_id) < H5Tget_size(H5T_STD_I32LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(nest_mem_id))
        TEST_ERROR;
    H5Tclose(nest_mem_id);
    H5Tclose(mem_id);

    /* check the long long member */
    if((mem_id = H5Tget_member_type(native_type, 2))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_LLONG))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_I64BE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* Read the dataset back.  Temporary buffer is for special platforms like
     * Cray */
    tmp = malloc(DIM0*DIM1*H5Tget_size(native_type));
    if((bkg=calloc(sizeof(s1),DIM0*DIM1))==NULL)
        TEST_ERROR;

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp)<0)
	TEST_ERROR;

    memcpy(check, tmp, DIM0*DIM1*H5Tget_size(native_type));
    free(tmp);

    if (H5Tconvert(native_type, tid_m, (DIM0*DIM1), check, bkg, H5P_DEFAULT))
        TEST_ERROR;

    free(bkg);

    /* Check that the values read are the same as the values written */
    for (i = 0, temp_point=points, temp_check=check; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++, temp_point++,temp_check++) {
	    if (temp_point->c != temp_check->c ||
	        temp_point->l != temp_check->l ) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }

	    for (k = 0; k < 5; k++) {
                if(temp_point->a[k] != temp_check->a[k]) {
		      H5_FAILED();
		      printf("    Read different values than written.\n");
		      printf("    At index %d,%d,%d\n", i, j, k);
		      goto error;
                }
            }
	}
    }

    H5Dclose(dataset);
    H5Tclose(dtype);
    H5Tclose(native_type);
    H5Tclose(tid_m);
    H5Tclose(tid_m2);

    /* Free memory for test data */
    free(points);
    free(check);

    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_opaque
 *
 * Purpose:	Test H5Tget_native_type for compound datatype with opaque field
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *		January 31, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compound_opaque(hid_t file)
{
    typedef struct {
        char            c;
        unsigned char   o[5];
        long_long       l;
    } s1;
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2, tid_m,
                        mem_id;
    int			i, j, k, n;
    hsize_t		dims[2];
    s1                  *temp_point, *temp_check;
    s1 	                *points=NULL, *check=NULL;
    void                *tmp, *bkg;

    TESTING("compound datatype with opaque field");

    /* Allocate space for the points & check arrays */
    if((points=HDmalloc(sizeof(s1)*DIM0*DIM1))==NULL)
        TEST_ERROR;
    if((check=HDcalloc(sizeof(s1),DIM0*DIM1))==NULL)
        TEST_ERROR;

    /* Initialize the dataset */
    for (i = n = 0, temp_point=points; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++,temp_point++) {
	    temp_point->c = 't';
	    temp_point->l = (i*10+j*100)*n;
	    for (k = 0; k < 5; k++)
	        (temp_point->o)[k] = n++;
	}
    }

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL))<0) TEST_ERROR;

    /* Create opaque datatype */
    if((tid2=H5Tcreate(H5T_OPAQUE, sizeof(temp_point->o)))<0) TEST_ERROR;
    if(H5Tset_tag(tid2, "testing opaque field")<0) TEST_ERROR;

    /* Create compound datatype for disk storage */
    if((tid=H5Tcreate(H5T_COMPOUND, 14))<0) TEST_ERROR;

    /* Insert members */
    if(H5Tinsert(tid, "c", 0, H5T_STD_U8LE)<0) TEST_ERROR;
    if(H5Tinsert(tid, "o", 1, tid2)<0) TEST_ERROR;
    if(H5Tinsert(tid, "l", 6, H5T_STD_I64BE)<0) TEST_ERROR;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_COMPOUND_NAME_4, tid, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Create compound datatype for datatype in memory */
    if((tid_m=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "c", HOFFSET(s1, c), H5T_NATIVE_UCHAR)<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "o", HOFFSET(s1, o), tid2)<0) TEST_ERROR;
    if(H5Tinsert(tid_m, "l", HOFFSET(s1, l), H5T_NATIVE_LLONG)<0) TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid)<0) TEST_ERROR;
    if(H5Tclose(tid2)<0) TEST_ERROR;

    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_COMPOUND_NAME_4))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Verify the datatype of each field retrieved and converted */
    /* check the char member */
    if((mem_id = H5Tget_member_type(native_type, 0))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_UCHAR))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_U8LE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* check the array member */
    if((mem_id = H5Tget_member_type(native_type, 1))<0)
        TEST_ERROR;
    if(H5T_OPAQUE!=H5Tget_class(mem_id))
        TEST_ERROR;
    if(H5Tget_size(mem_id) != sizeof(temp_point->o))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* check the long long member */
    if((mem_id = H5Tget_member_type(native_type, 2))<0)
        TEST_ERROR;
    if(H5Tget_order(mem_id) != H5Tget_order(H5T_NATIVE_LLONG))
        TEST_ERROR;
    if(H5Tget_size(mem_id) < H5Tget_size(H5T_STD_I64BE))
        TEST_ERROR;
    if(H5T_INTEGER!=H5Tget_class(mem_id))
        TEST_ERROR;
    H5Tclose(mem_id);

    /* Read the dataset back.  Temporary buffer is for special platforms like
     * Cray */
    tmp = HDmalloc(DIM0*DIM1*H5Tget_size(native_type));
    if((bkg=HDcalloc(sizeof(s1),DIM0*DIM1))==NULL)
        TEST_ERROR;

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp)<0)
	TEST_ERROR;

    HDmemcpy(check, tmp, DIM0*DIM1*H5Tget_size(native_type));
    HDfree(tmp);

    if (H5Tconvert(native_type, tid_m, (DIM0*DIM1), check, bkg, H5P_DEFAULT))
        TEST_ERROR;

    HDfree(bkg);

    /* Check that the values read are the same as the values written */
    for (i = 0, temp_point=points, temp_check=check; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++, temp_point++,temp_check++) {
	    if (temp_point->c != temp_check->c ||
	        temp_point->l != temp_check->l ) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }

	    for (k = 0; k < 5; k++) {
                if(temp_point->o[k] != temp_check->o[k]) {
		      H5_FAILED();
		      printf("    Read different values than written.\n");
		      printf("    At index %d,%d,%d\n", i, j, k);
		      goto error;
                }
            }
	}
    }

    H5Dclose(dataset);
    H5Tclose(dtype);
    H5Tclose(native_type);
    H5Tclose(tid_m);

    /* Free memory for test data */
    HDfree(points);
    HDfree(check);

    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_enum_dtype
 *
 * Purpose:	Test H5Tget_native_type for enumerate datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_enum_dtype(hid_t file)
{
    hid_t		dataset, space;
    hid_t               tid, tid_m, dtype, native_type;
    int			i, j, n;
    hsize_t		dims[2];
    void                *tmp;
    short               colors[8];
    unsigned char       sub_colors[16];
    const char          *mname[] = { "RED",
                                     "GREEN",
                                     "BLUE",
                                     "YELLOW",
                                     "PINK",
                                     "PURPLE",
                                     "ORANGE",
                                     "WHITE" };

    TESTING("enum datatype");

    /* Initialize the dataset */
    for (i = 0; i < DIM0; i++) {
        for (j=0, n=0; j < DIM1; j++, n++)
	    spoints2[i][j] = (i*10+j*100+n)%8;
    }

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL))<0) TEST_ERROR;

    /* Construct enum type based on native type */
    if((tid=H5Tenum_create(H5T_STD_I16LE))<0) TEST_ERROR;

    for (i = 0; i < 8; i++) {
        sub_colors[i*2]=i;
        sub_colors[i*2+1]=0;
        if(H5Tenum_insert(tid, mname[i], &(sub_colors[i*2]))<0) TEST_ERROR;
    }

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_ENUM_NAME, tid, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Construct enum type based on native type in memory */
    if((tid_m=H5Tenum_create(H5T_NATIVE_SHORT))<0) TEST_ERROR;

    for (i = 0; i < 8; i++) {
        colors[i] = i;
        if(H5Tenum_insert(tid_m, mname[i], &(colors[i]))<0) TEST_ERROR;
    }

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, spoints2)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid)<0) TEST_ERROR;

    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;

    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_ENUM_NAME))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Read the dataset back.  Temporary buffer is for special platforms like
     * Cray */
    tmp = malloc(DIM0*DIM1*H5Tget_size(native_type));

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp)<0)
	TEST_ERROR;

    memcpy(scheck2, tmp, DIM0*DIM1*H5Tget_size(native_type));
    free(tmp);

    if (H5Tconvert(native_type, tid_m, (DIM0*DIM1), scheck2, NULL, H5P_DEFAULT)<0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++) {
	    if (spoints2[i][j] != scheck2[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
                printf(" spoints2[i][j]=%hd, scheck2[i][j]=%hd\n", spoints2[i][j],
                        scheck2[i][j]);
		goto error;
	    }
	}
    }

    H5Dclose(dataset);
    H5Tclose(dtype);
    H5Tclose(native_type);
    H5Tclose(tid_m);
    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_array_dtype
 *
 * Purpose:	Test H5Tget_native_type for array datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_array_dtype(hid_t file)
{
    typedef struct {
        char    c;
        int     i;
        long_long l;
    } s1;
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid2, tid3, tid_m;
    int			i, j, k, n;
    hsize_t		space_dims[2], array_dims[1]={5};
    s1                 *temp_point, *temp_check;
    s1 	               *points=NULL, *check=NULL;
    void               *tmp;

    TESTING("array of compound datatype");

    /* Allocate space for the points & check arrays */
    if((points=malloc(sizeof(s1)*DIM0*DIM1*5))==NULL)
        TEST_ERROR;
    if((check=calloc(sizeof(s1),DIM0*DIM1*5))==NULL)
        TEST_ERROR;

    /* Initialize the dataset */
    for(i = n = 0, temp_point=points; i < DIM0; i++)
	for(j = 0; j < DIM1; j++)
            for(k = 0; k < 5; k++,temp_point++) {
                temp_point->c= 't';
                temp_point->i= n++;
                temp_point->l= (i*10+j*100)*n;
            }

    /* Create the data space */
    space_dims[0] = DIM0;
    space_dims[1] = DIM1;
    if ((space = H5Screate_simple(2, space_dims, NULL))<0) TEST_ERROR;

    /* Create compound datatype for disk storage */
    if((tid2=H5Tcreate(H5T_COMPOUND, 13))<0) TEST_ERROR;

    /* Insert members */
    if(H5Tinsert(tid2, "c", 0, H5T_STD_U8BE)<0) TEST_ERROR;
    if(H5Tinsert(tid2, "i", 1, H5T_STD_U32LE)<0) TEST_ERROR;
    if(H5Tinsert(tid2, "l", 5, H5T_STD_I64BE)<0) TEST_ERROR;

    /* Create array datatype for disk storage */
    if((tid=H5Tarray_create(tid2, 1, array_dims, NULL))<0) TEST_ERROR;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_ARRAY_NAME, tid, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Create compound datatype for datatype in memory */
    if((tid3=H5Tcreate(H5T_COMPOUND, sizeof(s1)))<0) TEST_ERROR;
    if(H5Tinsert(tid3, "c", HOFFSET(s1, c), H5T_NATIVE_UCHAR)<0) TEST_ERROR;
    if(H5Tinsert(tid3, "i", HOFFSET(s1, i), H5T_NATIVE_UINT)<0) TEST_ERROR;
    if(H5Tinsert(tid3, "l", HOFFSET(s1, l), H5T_NATIVE_LLONG)<0) TEST_ERROR;

    /* Create array datatype for memory */
    if((tid_m=H5Tarray_create(tid3, 1, array_dims, NULL))<0) TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid)<0) TEST_ERROR;
    if(H5Tclose(tid2)<0) TEST_ERROR;

    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;

    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_ARRAY_NAME))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Read the dataset back. Temporary buffer is for special platforms like
     * Cray */
    tmp = malloc(DIM0*DIM1*H5Tget_size(native_type));

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp)<0)
	TEST_ERROR;

    memcpy(check, tmp, DIM0*DIM1*H5Tget_size(native_type));
    free(tmp);

    if (H5Tconvert(native_type, tid_m, (DIM0*DIM1), check, NULL, H5P_DEFAULT)<0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0, temp_point=points, temp_check=check; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++) {
            for (k = 0; k < 5; k++, temp_point++,temp_check++) {
                if (temp_point->c != temp_check->c ||
	            temp_point->i != temp_check->i ||
	            temp_point->l != temp_check->l ) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    At index %d,%d\n", i, j);
                    goto error;
                }
	    }
	}
    }

    /* Close HDF5 objects */
    if(H5Dclose(dataset)) TEST_ERROR;
    if(H5Tclose(native_type)) TEST_ERROR;
    if(H5Tclose(dtype)) TEST_ERROR;
    if(H5Tclose(tid_m)<0) TEST_ERROR;
    if(H5Tclose(tid3)<0) TEST_ERROR;

    /* Free memory for test data */
    free(points);
    free(check);

    PASSED();
    return 0;

error:
    if(points!=NULL)
        free(points);
    if(check!=NULL)
        free(check);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_array_dtype2
 *
 * Purpose:	Test H5Tget_native_type for array datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_array_dtype2(hid_t file)
{
    hid_t		dataset, space;
    hid_t               dtype, native_type, tid, tid_m;
    int			i, j, k, n;
    hsize_t		space_dims[2], array_dims[1]={5};
    void                *tmp;

    TESTING("array of atomic datatype");

    /* Initialize the dataset */
    for(i = n = 0;i < DIM0; i++)
	for(j = 0; j < DIM1; j++)
            for(k = 0; k < 5; k++)
                ipoints3[i][j][k] = n++;

    /* Create the data space */
    space_dims[0] = DIM0;
    space_dims[1] = DIM1;
    if ((space = H5Screate_simple(2, space_dims, NULL))<0) TEST_ERROR;

    /* Create array datatype for disk storage */
    if((tid=H5Tarray_create(H5T_STD_I32LE, 1, array_dims, NULL))<0) TEST_ERROR;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_ARRAY2_NAME, tid, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Create array datatype for memory */
    if((tid_m=H5Tarray_create(H5T_NATIVE_INT, 1, array_dims, NULL))<0) TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, tid_m, H5S_ALL, H5S_ALL, H5P_DEFAULT, ipoints3)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid)<0) TEST_ERROR;

    /* Close dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_ARRAY2_NAME))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Read the dataset back.  Temporary buffer is for special platforms like
     * Cray */
    tmp = malloc(DIM0*DIM1*H5Tget_size(native_type));

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp)<0)
	TEST_ERROR;

    memcpy(icheck3, tmp, DIM0*DIM1*H5Tget_size(native_type));
    free(tmp);

    if (H5Tconvert(native_type, tid_m, (DIM0*DIM1), icheck3, NULL, H5P_DEFAULT)<0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < DIM0; i++) {
	for (j = 0; j < DIM1; j++) {
            for (k = 0; k < 5; k++) {
                if(icheck3[i][j][k] != ipoints3[i][j][k]) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    At index %d,%d\n", i, j);
                    goto error;
                }
	    }
	}
    }

    /* Close HDF5 objects */
    if(H5Dclose(dataset)) TEST_ERROR;
    if(H5Tclose(native_type)) TEST_ERROR;
    if(H5Tclose(dtype)) TEST_ERROR;
    if(H5Tclose(tid_m)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_vl_dtype
 *
 * Purpose:	Test H5Tget_native_type for variable length datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_vl_dtype(hid_t file)
{
    hvl_t               wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t               rdata[SPACE1_DIM1];   /* Information read in */
    hvl_t               *t1, *t2;             /* Temporary pointer to VL information */
    hsize_t		dims1[] = {SPACE1_DIM1};
    hid_t		dataset, space;
    hid_t               dtype, native_type, nat_super_type, tid, tid2, tid_m, tid_m2;
    size_t		i, j, k;
    void*               *tmp;

    TESTING("variable length datatype");

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(hvl_t));
        if(wdata[i].p==NULL) {
            H5_FAILED();
            printf("    Cannot allocate memory for VL data! i=%u\n",(unsigned)i);
            goto error;
        } /* end if */
        wdata[i].len=i+1;
        for(t1=wdata[i].p,j=0; j<(i+1); j++, t1++) {
            t1->p=malloc((j+1)*sizeof(unsigned int));
            if(t1->p==NULL) {
                H5_FAILED();
                printf("    Cannot allocate memory for VL data! i=%u, j=%u\n",(unsigned)i,(unsigned)j);
                goto error;
            } /* end if */
            t1->len=j+1;
            for(k=0; k<(j+1); k++) {
                ((unsigned int *)t1->p)[k]=(unsigned int)(i*100+j*10+k);
            }
        } /* end for */
    } /* end for */

    /* Create dataspace for datasets */
    if((space = H5Screate_simple(SPACE1_RANK, dims1, NULL))<0) TEST_ERROR;

    /* Create the base VL type */
    if((tid2 = H5Tvlen_create (H5T_STD_U32LE))<0) TEST_ERROR;

    /* Create a VL datatype for disk storage */
    tid = H5Tvlen_create (tid2);

    /* Create a dataset */
    if((dataset=H5Dcreate(file, DSET_VL_NAME, tid, space, H5P_DEFAULT))<0)
        TEST_ERROR;

    /* Create a base VL datatype for memory */
    if((tid_m2 = H5Tvlen_create (H5T_NATIVE_UINT))<0) TEST_ERROR;

    /* Create a VL datatype for memory */
    if((tid_m = H5Tvlen_create (tid_m2))<0) TEST_ERROR;

    /* Write dataset to disk */
    if(H5Dwrite(dataset,tid_m,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata)<0) TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid2)<0) TEST_ERROR;
    if(H5Tclose(tid)<0) TEST_ERROR;

    /* Open a dataset */
    if((dataset=H5Dopen(file, DSET_VL_NAME))<0) TEST_ERROR;

    /* Get native datatype for dataset */
    if((dtype = H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Also get native base type for this nested VL type.  Should be an integer type. */
    if((nat_super_type=H5Tget_super(native_type))<0)
        TEST_ERROR;
    if((nat_super_type=H5Tget_super(nat_super_type))<0)
        TEST_ERROR;

    /* Read dataset from disk */
    if(H5Dread(dataset,native_type,H5S_ALL,H5S_ALL,H5P_DEFAULT,rdata)<0) TEST_ERROR;

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(wdata[i].len!=rdata[i].len) {
            H5_FAILED();
            printf("    VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
            goto error;
        } /* end if */
        for(t1=wdata[i].p, t2=rdata[i].p, j=0; j<rdata[i].len; j++, t1++, t2++) {
            if(t1->len!=t2->len) {
                H5_FAILED();
                printf("    VL data length don't match!, wdata[%d].len=%d, rdata[%d].len=%d\n",(int)i,(int)wdata[i].len,(int)i,(int)rdata[i].len);
                goto error;
            } /* end if */

            /* use temporary buffer to convert datatype.  This is for special
             * platforms like Cray */
            tmp=malloc(t2->len*sizeof(unsigned int));
            memcpy(tmp, t2->p, t2->len*H5Tget_size(nat_super_type));

            if (H5Tconvert(nat_super_type, H5T_NATIVE_UINT, t2->len, tmp, NULL, H5P_DEFAULT))
                TEST_ERROR;

            for(k=0; k<t2->len; k++) {
                if( ((unsigned int *)t1->p)[k] != ((unsigned int *)tmp)[k] ) {
                    H5_FAILED();
                    printf("    VL data don't match!, wdata[%u].p=%d, rdata[%u].p=%u\n",
                        (unsigned)i,((unsigned int*)t1->p)[k],(unsigned)i,((unsigned int*)tmp)[k]);
                    goto error;
                }
            } /* end for */

            free(tmp);
        } /* end for */
    } /* end for */

    /* Reclaim the read VL data */
    if(H5Dvlen_reclaim(native_type,space,H5P_DEFAULT,rdata)<0) TEST_ERROR;

    /* Reclaim the write VL data */
    if(H5Dvlen_reclaim(native_type,space,H5P_DEFAULT,wdata)<0) TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(native_type)<0) TEST_ERROR;
    if(H5Tclose(dtype)<0) TEST_ERROR;
    if(H5Tclose(tid_m)<0) TEST_ERROR;
    if(H5Tclose(tid_m2)<0) TEST_ERROR;


    /* Close disk dataspace */
    if(H5Sclose(space)<0) TEST_ERROR;

    PASSED();
    return 0;

 error:
    return -1;
} /* end test_vl_type() */


/*-------------------------------------------------------------------------
 * Function:	test_vlstr_dtype
 *
 * Purpose:	Test H5Tget_native_type for variable length string datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_vlstr_dtype(hid_t file)
{
    const char *wdata[SPACE1_DIM1]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure."
        };   /* Information to write */
    char *rdata[SPACE1_DIM1];   /* Information read in */
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1,dtype,native_type;       /* Datatype ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    unsigned       i;          /* counting variable */

    /* Output message about test being performed */
    TESTING("variable length string datatype");

    /* Create dataspace for datasets */
    if((sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL))<0) TEST_ERROR;

    /* Create a datatype to refer to */
    if((tid1 = H5Tcopy (H5T_C_S1))<0) TEST_ERROR;

    if(H5Tset_size (tid1,H5T_VARIABLE)<0) TEST_ERROR;
    if(H5T_STRING!=H5Tget_class(tid1) || !H5Tis_variable_str(tid1))
        TEST_ERROR;

    /* Create a dataset */
    if((dataset=H5Dcreate(file,DSET_VLSTR_NAME,tid1,sid1,H5P_DEFAULT))<0) TEST_ERROR;

    /* Write dataset to disk */
    if(H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata)<0) TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Open a dataset */
    if((dataset=H5Dopen(file, DSET_VLSTR_NAME))<0) TEST_ERROR;

    /* Get datatype for dataset */
    if((dtype = H5Dget_type(dataset))<0) TEST_ERROR;

    /* Construct native type */
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Check if the data type is equal */
    if(!H5Tequal(native_type, tid1))
        TEST_ERROR;

    /* Read dataset from disk */
    if(H5Dread(dataset,native_type,H5S_ALL,H5S_ALL,H5P_DEFAULT,rdata)<0) TEST_ERROR;

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(strlen(wdata[i])!=strlen(rdata[i])) {
            H5_FAILED();
            printf("    VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",
                   (int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            goto error;
        } /* end if */
        if( strcmp(wdata[i],rdata[i]) != 0 ) {
            H5_FAILED();
            printf("    VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",
                   (int)i,wdata[i],(int)i,rdata[i]);
            goto error;
        } /* end if */
    } /* end for */

    /* Close Dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid1)<0) TEST_ERROR;
    if(H5Tclose(native_type)<0) TEST_ERROR;

    /* Close disk dataspace */
    if(H5Sclose(sid1)<0) TEST_ERROR;

    /* Free memory for rdata */
    for(i=0; i<SPACE1_DIM1; i++) {
        HDfree(rdata[i]);
    }

    PASSED();
    return 0;

error:
    return -1;
} /* end test_vlstr_dtype() */


/*-------------------------------------------------------------------------
 * Function:	test_str_dtype
 *
 * Purpose:	Test H5Tget_native_type for fixed-length string datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_str_dtype(hid_t file)
{
    const char wdata[SPACE1_DIM1][4]= {
        "one",
        "two",
        "3rd",
        "4th"
        };   /* Information to write */
    char rdata[SPACE1_DIM1][4];   /* Information read in */
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1,dtype,native_type;       /* Datatype ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    unsigned       i;          /* counting variable */

    /* Output message about test being performed */
    TESTING("fixed-length string datatype");

    /* Create dataspace for datasets */
    if((sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL))<0) TEST_ERROR;

    /* Create a datatype to refer to */
    if((tid1 = H5Tcopy (H5T_C_S1))<0) TEST_ERROR;

    if(H5Tset_size (tid1,4)<0) TEST_ERROR;
    if(H5T_STRING!=H5Tget_class(tid1))
        TEST_ERROR;

    /* Create a dataset */
    if((dataset=H5Dcreate(file,DSET_STR_NAME,tid1,sid1,H5P_DEFAULT))<0) TEST_ERROR;

    /* Write dataset to disk */
    if(H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata)<0) TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Open a dataset */
    if((dataset=H5Dopen(file, DSET_STR_NAME))<0) TEST_ERROR;

    /* Get datatype for dataset */
    if((dtype = H5Dget_type(dataset))<0) TEST_ERROR;

    /* Construct native type */
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Check if the data type is equal */
    if(!H5Tequal(native_type, tid1) || H5T_STRING!=H5Tget_class(native_type))
        TEST_ERROR;

    /* Read dataset from disk */
    if(H5Dread(dataset,native_type,H5S_ALL,H5S_ALL,H5P_DEFAULT,rdata)<0) TEST_ERROR;

    /* Compare data read in */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(strlen(wdata[i])!=strlen(rdata[i])) {
            H5_FAILED();
            printf("    data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",
                   (int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
            goto error;
        } /* end if */
        if( strcmp(wdata[i],rdata[i]) != 0 ) {
            H5_FAILED();
            printf("    data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",
                   (int)i,wdata[i],(int)i,rdata[i]);
            goto error;
        } /* end if */
    } /* end for */

    /* Close Dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid1)<0) TEST_ERROR;
    if(H5Tclose(native_type)<0) TEST_ERROR;

    /* Close disk dataspace */
    if(H5Sclose(sid1)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* end test_str_dtype() */




/*-------------------------------------------------------------------------
 * Function:	test_refer_dtype
 *
 * Purpose:	Test H5Tget_native_type for reference datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_refer_dtype(hid_t file)
{

    /* Compound datatype */
    typedef struct s1_t {
        unsigned int a;
        unsigned int b;
        float c;
    } s1_t;

    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1, dtype, native_type;       /* Datatype ID	*/
    hsize_t		dims1[] = {1};
    hobj_ref_t          *wbuf,      /* buffer to write to disk */
                        *rbuf;       /* buffer read from disk */

    /* Output message about test being performed */
    TESTING("reference datatype");

    /* Allocate write & read buffers */
    wbuf=HDmalloc(MAX(sizeof(unsigned),sizeof(hobj_ref_t)));
    rbuf=HDmalloc(MAX(sizeof(unsigned),sizeof(hobj_ref_t)));

    /* Create dataspace for datasets */
    if((sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL))<0)
        TEST_ERROR;

    /* Create a group */
    if((group=H5Gcreate(file,"Group1",(size_t)-1))<0)
        TEST_ERROR;

    /* Create a datatype to refer to */
    if((tid1 = H5Tcreate (H5T_COMPOUND, sizeof(s1_t)))<0)
        TEST_ERROR;

    /* Insert fields */
    if(H5Tinsert (tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT)<0)
        TEST_ERROR;

    if(H5Tinsert (tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT)<0)
        TEST_ERROR;

    if(H5Tinsert (tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT)<0)
        TEST_ERROR;

    /* Save datatype for later */
    if(H5Tcommit (group, "Datatype1", tid1)<0)
        TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid1)<0)
        TEST_ERROR;

    /* Close group */
    if(H5Gclose(group)<0)
        TEST_ERROR;

    /* Create a dataset */
    if((dataset=H5Dcreate(file,"Dataset3",H5T_STD_REF_OBJ,sid1,H5P_DEFAULT))<0)
        TEST_ERROR;

    /* Create reference to named datatype */
    if(H5Rcreate(wbuf,file,"/Group1/Datatype1",H5R_OBJECT,-1)<0)
        TEST_ERROR;
#ifdef H5_WANT_H5_V1_4_COMPAT
    if(H5Rget_object_type(dataset,wbuf)!=H5G_TYPE)
        TEST_ERROR;
#else /* H5_WANT_H5_V1_4_COMPAT */
    if(H5Rget_obj_type(dataset,H5R_OBJECT,wbuf)!=H5G_TYPE)
        TEST_ERROR;
#endif /* H5_WANT_H5_V1_4_COMPAT */

    /* Write selection to disk */
    if(H5Dwrite(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf)<0)
        TEST_ERROR;

    /* Close disk dataspace */
    if(H5Sclose(sid1)<0)
        TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dataset)<0)
        TEST_ERROR;


    /* Open the dataset */
    if((dataset=H5Dopen(file,"/Dataset3"))<0)
        TEST_ERROR;

    /* Get datatype for dataset */
    if((dtype = H5Dget_type(dataset))<0)
        TEST_ERROR;

    /* Construct native type */
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Check if the data type is equal */
    if(!H5Tequal(native_type, H5T_STD_REF_OBJ))
        TEST_ERROR;

    /* Read selection from disk */
    if(H5Dread(dataset,native_type,H5S_ALL,H5S_ALL,H5P_DEFAULT,rbuf)<0)
        TEST_ERROR;

    /* Open datatype object */
    if((tid1 = H5Rdereference(dataset,H5R_OBJECT,rbuf))<0)
        TEST_ERROR;

    /* Verify correct datatype */
    if(H5Tget_class(tid1)!=H5T_COMPOUND)
        TEST_ERROR;

    if(H5Tget_nmembers(tid1)!=3)
        TEST_ERROR;

    /* Close datatype */
    if(H5Tclose(tid1)<0)
        TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dataset)<0)
        TEST_ERROR;

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);

    PASSED();
    return 0;

error:
    return -1;
}   /* test_refer_dtype() */


/*-------------------------------------------------------------------------
 * Function:	test_refer_dtype2
 *
 * Purpose:	Test H5Tget_native_type for reference
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_refer_dtype2(hid_t file)
{
    hid_t		dset1,	/* Dataset ID			*/
                        dset2;      /* Dereferenced dataset ID */
    hid_t		sid1,       /* Dataspace ID	#1		*/
                        sid2;       /* Dataspace ID	#2		*/
    hid_t               dtype, native_type;
    hsize_t		dims1[] = {1},
            	        dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t	        start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE2_RANK];     /* Block size of hyperslab */
    hdset_reg_ref_t     wbuf,        /* buffer to write to disk */
                        rbuf;        /* buffer read from disk */
    uint8_t             *dwbuf,      /* Buffer for writing numeric data to disk */
                        *drbuf;      /* Buffer for reading numeric data from disk */
    uint8_t             *tu8;        /* Temporary pointer to uint8 data */
    int                 i;           /* counting variables */

    /* Output message about test being performed */
    TESTING("dataset region reference");

    /* Allocate write & read buffers */
    dwbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    drbuf=calloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Create dataspace for datasets */
    if((sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL))<0)
        TEST_ERROR;

    /* Create a dataset */
    if((dset2=H5Dcreate(file,"Dataset2",H5T_STD_U8LE,sid2,H5P_DEFAULT))<0)
        TEST_ERROR;

    for(tu8=dwbuf,i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++)
        *tu8++=i*3;

    /* Write selection to disk */
    if(H5Dwrite(dset2,H5T_STD_U8LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,dwbuf)<0)
        TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dset2)<0)
        TEST_ERROR;


    /* Create dataspace for the reference dataset */
    if((sid1=H5Screate_simple(SPACE1_RANK, dims1, NULL))<0)
        TEST_ERROR;

    /* Create a reference dataset */
    if((dset1=H5Dcreate(file,"Dataset1",H5T_STD_REF_DSETREG,sid1,H5P_DEFAULT))<0)
        TEST_ERROR;

    /* Create references */
    /* Select 6x6 hyperslab for first reference */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=1; count[1]=1;
    block[0]=6; block[1]=6;

    if(H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block)<0)
        TEST_ERROR;

    if((int)H5Sget_select_npoints(sid2) != 36)
        TEST_ERROR;

    /* Store first dataset region */
    if(H5Rcreate(&wbuf,file,"/Dataset2",H5R_DATASET_REGION,sid2)<0)
        TEST_ERROR;
    if(H5Rget_obj_type(dset1,H5R_DATASET_REGION,&wbuf) != H5G_DATASET)
        TEST_ERROR;

    /* Write selection to disk */
    if(H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,&wbuf)<0)
        TEST_ERROR;

    /* Close disk dataspace */
    if(H5Sclose(sid1)<0)
        TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dset1)<0)
        TEST_ERROR;

    /* Close uint8 dataset dataspace */
    if(H5Sclose(sid2)<0)
        TEST_ERROR;




    /* Open the dataset */
    if((dset1=H5Dopen(file,"/Dataset1"))<0)
        TEST_ERROR;

    /* Get datatype for dataset */
    if((dtype = H5Dget_type(dset1))<0)
        TEST_ERROR;

    /* Construct native type */
    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /* Check if the data type is equal */
    if(!H5Tequal(native_type, H5T_STD_REF_DSETREG))
        TEST_ERROR;

    /* Read selection from disk */
    if(H5Dread(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,&rbuf)<0)
        TEST_ERROR;

    /* Try to open objects */
    if((dset2=H5Rdereference(dset1,H5R_DATASET_REGION,&rbuf))<0)
        TEST_ERROR;

    /* Check what H5Rget_obj_type function returns */
    if(H5Rget_obj_type(dset1, H5R_DATASET_REGION,&rbuf) != H5G_DATASET)
        TEST_ERROR;

    /* Check information in referenced dataset */
    if((sid1 = H5Dget_space(dset2))<0)
        TEST_ERROR;

    if((int)H5Sget_simple_extent_npoints(sid1)!=100)
        TEST_ERROR;

    /* Read from disk */
    if(H5Dread(dset2,H5T_STD_U8LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,drbuf)<0)
        TEST_ERROR;

    for(tu8=(uint8_t *)drbuf,i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++,tu8++)
        if(*tu8 != (uint8_t)(i*3))
            TEST_ERROR;

    /* Get the hyperslab selection */
    if((sid2=H5Rget_region(dset1,H5R_DATASET_REGION,&rbuf))<0)
        TEST_ERROR;

    /* Verify correct hyperslab selected */
    if((int)H5Sget_select_npoints(sid2) != 36)
        TEST_ERROR;
    if((int)H5Sget_select_hyper_nblocks(sid2) != 1)
        TEST_ERROR;

    /* Close region space */
    if(H5Sclose(sid2)<0)
        TEST_ERROR;

    /* Close first space */
    if(H5Sclose(sid1)<0)
        TEST_ERROR;

    /* Close dereferenced Dataset */
    if(H5Dclose(dset2)<0)
        TEST_ERROR;

    /* Close Dataset */
    if(H5Dclose(dset1)<0)
        TEST_ERROR;

    /* Free memory buffers */
    free(dwbuf);
    free(drbuf);

    PASSED();
    return 0;

error:
    return -1;
}   /* test_refer_dtype2() */


/*-------------------------------------------------------------------------
 * Function:	test_opaque_dtype
 *
 * Purpose:	Test H5Tget_native_type for opaque datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_opaque_dtype(hid_t file)
{
    hid_t		type=-1, space=-1, dset=-1;
    hid_t               dataset, dtype, native_type;
    size_t              i;
    unsigned char	wbuf[32], rbuf[32];
    hsize_t		nelmts;

    TESTING("opaque datatype");

    /* opaque_1 */
    nelmts = sizeof(wbuf);
    if ((type=H5Tcreate(H5T_OPAQUE, 1))<0 ||
            H5Tset_tag(type, "testing 1-byte opaque type")<0 ||
            (space=H5Screate_simple(1, &nelmts, NULL))<0 ||
            (dset=H5Dcreate(file, DSET_OPAQUE_NAME, type, space, H5P_DEFAULT))<0)
	TEST_ERROR;

    for (i=0; i<sizeof(wbuf); i++)
        wbuf[i] = (unsigned char)0xff ^ (unsigned char)i;

    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf)<0)
	TEST_ERROR;
    if (H5Sclose(space)<0) TEST_ERROR;
    if (H5Dclose(dset)<0) TEST_ERROR;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_OPAQUE_NAME))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    if((native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    if(!H5Tequal(native_type, type)) TEST_ERROR;

    if (H5Dread(dataset, native_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf)<0)
        TEST_ERROR;

    for(i=0; i<sizeof(rbuf); i++) {
	if (rbuf[i] != wbuf[i]) {
	    H5_FAILED();
	    printf("    Read different values than written.\n");
            printf("    At index %u\n", (unsigned)i);
	    goto error;
        }
    }

    if (H5Tclose(type)<0) TEST_ERROR;
    if (H5Tclose(dtype)<0) TEST_ERROR;
    if (H5Tclose(native_type)<0) TEST_ERROR;
    if (H5Dclose(dataset)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* test_opaque_dtype */


/*-------------------------------------------------------------------------
 * Function:	test_bitfield_dtype
 *
 * Purpose:	Test H5Tget_native_type for bitfield datatype
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_bitfield_dtype(hid_t file)
{
    hid_t		type=-1, space=-1, dset=-1;
    hid_t               dataset, dtype, native_type;
    size_t		i;
    unsigned char	wbuf[32];
    hsize_t		nelmts;

    TESTING("bitfield datatype");

    /* opaque_1 */
    nelmts = sizeof(wbuf);
    if ((type=H5Tcopy(H5T_STD_B8LE))<0 ||
            (space=H5Screate_simple(1, &nelmts, NULL))<0 ||
            (dset=H5Dcreate(file, DSET_BITFIELD_NAME, type, space, H5P_DEFAULT))<0)
	TEST_ERROR;

    for (i=0; i<sizeof(wbuf); i++)
        wbuf[i] = (unsigned char)0xff ^ (unsigned char)i;

    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf)<0)
	TEST_ERROR;
    if (H5Sclose(space)<0) TEST_ERROR;
    if (H5Dclose(dset)<0) TEST_ERROR;


    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_BITFIELD_NAME))<0) TEST_ERROR;

    if((dtype=H5Dget_type(dataset))<0) TEST_ERROR;

    H5E_BEGIN_TRY {
        native_type=H5Tget_native_type(dtype, H5T_DIR_DEFAULT);
    } H5E_END_TRY;
    if(native_type>0) {
        H5_FAILED();
        puts("  Bit field isn't supported.  Should have failed.");
        TEST_ERROR;
    }

    if (H5Tclose(type)<0) TEST_ERROR;
    if (H5Tclose(dtype)<0) TEST_ERROR;
    if (H5Dclose(dataset)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* test_bitfield_dtype */





/*-------------------------------------------------------------------------
 * Function: test_ninteger
 *
 * Purpose: Test the native integer function; made to check the case
 * like the Cray SV1, where the size of short is 8 but precision is 32
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *  September 3, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_ninteger(void)
{
    hid_t     fid1=(-1);             /* file ID */
    hid_t     fid2=(-1);             /* file ID */
    hid_t     did1=(-1);             /* dataset ID */
    hid_t     did2=(-1);             /* dataset ID */
    hid_t     sid1=(-1);             /* dataspace ID */
    hid_t     dcpl1=(-1);            /* dataset creation property list ID */
    hid_t     dcpl2=(-1);            /* dataset creation property list ID */
    hid_t     tid1=(-1);             /* file datatype */
    hid_t     tid2=(-1);             /* file datatype */
    hid_t     nid1=(-1);             /* native datatype */
    hid_t     nid2=(-1);             /* native datatype */
    hsize_t   dims[1]={DIM3};        /* dataspace dimensions */
    hsize_t   nelmts;                /* number of elements in dataset */
    int       rank=1;                /* rank of dataset */
    int       buf[DIM3];
    int       chk[DIM3];
    int       i;

    for (i=0; i<DIM3; i++){
     buf[i]=i;
    }

    TESTING("native integer ");

   /*-------------------------------------------------------------------------
    * step1: create a file
    *-------------------------------------------------------------------------
    */
    /* create a file using default properties */
    if ((fid1=H5Fcreate("tstint1.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
     goto error;

    /* create a data space */
    if ((sid1 = H5Screate_simple(rank,dims,NULL))<0) goto error;

    /* create dcpl  */
    if((dcpl1 = H5Pcreate(H5P_DATASET_CREATE))<0) goto error;

    /* create a dataset */
    if ((did1 = H5Dcreate(fid1,"dset",H5T_NATIVE_INT,sid1,dcpl1)) <0) goto error;

    /* write */
    if(H5Dwrite(did1,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
     goto error;

    /* close  */
    if (H5Sclose (sid1)<0) goto error;
    if (H5Pclose (dcpl1)<0) goto error;
    if (H5Dclose (did1)<0) goto error;
    if (H5Fclose (fid1)<0) goto error;

   /*-------------------------------------------------------------------------
    * step 2: open and create another file copying the data from file1
    *-------------------------------------------------------------------------
    */

     /* open */
    if ((fid1=H5Fopen("tstint1.h5",H5F_ACC_RDONLY,H5P_DEFAULT))<0)
     goto error;

    /* open dataset */
    if ((did1=H5Dopen(fid1,"dset"))<0)
     goto error;

    if ((sid1=H5Dget_space(did1))<0)
     goto error;

    /* get dcpl */
    if ((dcpl1=H5Dget_create_plist(did1))<0)
     goto error;

    /* get file datatype */
    if ((tid1=H5Dget_type (did1))<0)
     goto error;

    /* get native datatype */
    if ((nid1=H5Tget_native_type(tid1,H5T_DIR_DEFAULT))<0)
     goto error;

    /* get size */
    if (H5Tget_size(nid1)==0)
     goto error;

    /* get rank */
    if ((rank=H5Sget_simple_extent_ndims(sid1))<0)
     goto error;
    HDmemset(dims, 0, sizeof dims);

    /* get dimension */
    if (H5Sget_simple_extent_dims(sid1,dims,NULL)<0)
     goto error;
    nelmts=1;
    for (i=0; i<rank; i++)
     nelmts*=dims[i];

    /* read */
    if (H5Dread(did1,nid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,chk)<0)
     goto error;

    /* create a file using default properties */
    if ((fid2=H5Fcreate("tstint2.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
     goto error;

    /* create a dataset using the native type */
    if ((did2 = H5Dcreate(fid2,"dset",nid1,sid1,dcpl1)) <0) goto error;

    /* write */
    if(H5Dwrite(did2,nid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,chk)<0)
     goto error;

    /* get dcpl */
    if ((dcpl2=H5Dget_create_plist(did2))<0)
     goto error;

    /* get file datatype */
    if ((tid2=H5Dget_type (did2))<0)
     goto error;

    /* get native datatype */
    if ((nid2=H5Tget_native_type(tid2,H5T_DIR_DEFAULT))<0)
     goto error;

    /* check */
    if (H5Tget_precision(nid1)!=H5Tget_precision(nid2)) {
     printf("    Precision differ.\n");
     goto error;
    }

    /* compare dataset creation property lists */
    if(H5Pequal(dcpl1,dcpl2)<=0) {
     printf("    Property lists differ.\n");
     goto error;
    }

    /* check */
    for (i = 0; i < DIM3; i++) {
     if (buf[i] != chk[i]) {
      H5_FAILED();
      printf("    Read different values than written.\n");
      printf("    At index %d\n", i);
      goto error;
     }
    }

    /* close  */
    if (H5Sclose (sid1)<0) goto error;
    if (H5Pclose (dcpl1)<0) goto error;
    if (H5Pclose (dcpl2)<0) goto error;
    if (H5Tclose (tid1)<0) goto error;
    if (H5Tclose (tid2)<0) goto error;
    if (H5Tclose (nid1)<0) goto error;
    if (H5Tclose (nid2)<0) goto error;
    if (H5Dclose (did1)<0) goto error;
    if (H5Dclose (did2)<0) goto error;
    if (H5Fclose (fid1)<0) goto error;
    if (H5Fclose (fid2)<0) goto error;


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
     H5Pclose(dcpl1);
     H5Pclose(dcpl2);
     H5Tclose(tid1);
     H5Tclose(tid2);
     H5Tclose(nid1);
     H5Tclose(nid2);
     H5Dclose(did1);
     H5Dclose(did2);
     H5Sclose(sid1);
     H5Fclose(fid1);
     H5Fclose(fid2);
    } H5E_END_TRY;
    return -1;
} /* end test_ninteger() */



/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test H5Tget_native_type for different datatype
 *
 * Programmer:	Raymond Lu
 *		October 15, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t		file, fapl;
    int			nerrors=0;
    char		filename[1024];

    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	    goto error;

    nerrors += test_atomic_dtype(file)<0 	?1:0;
    nerrors += test_compound_dtype(file)<0 	?1:0;
    nerrors += test_compound_dtype2(file)<0 	?1:0;
    nerrors += test_compound_dtype3(file)<0 	?1:0;
    nerrors += test_compound_opaque(file)<0 	?1:0;
    nerrors += test_enum_dtype(file)<0 	        ?1:0;
    nerrors += test_array_dtype(file)<0 	?1:0;
    nerrors += test_array_dtype2(file)<0 	?1:0;
    nerrors += test_vl_dtype(file)<0 	        ?1:0;
    nerrors += test_vlstr_dtype(file)<0 	?1:0;
    nerrors += test_str_dtype(file)<0           ?1:0;
    nerrors += test_refer_dtype(file)<0 	?1:0;
    nerrors += test_refer_dtype2(file)<0 	?1:0;
    nerrors += test_opaque_dtype(file)<0 	?1:0;
    nerrors += test_bitfield_dtype(file)<0 	?1:0;
    nerrors += test_ninteger()<0  ?1:0;

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    printf("All native datatype tests passed.\n");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    nerrors = MAX(1, nerrors);
    printf("***** %d DATASET TEST%s FAILED! *****\n",
	   nerrors, 1 == nerrors ? "" : "S");
    return 1;
}
