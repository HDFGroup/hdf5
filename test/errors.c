/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:	Raymond Lu
 *              October 14, 2001	
 *
 * Purpose:	Tests the H5Tget_native_type function.
 */

#include "h5test.h"

const char *FILENAME[] = {
    "errors",
    NULL
};

#define DIM0    100
#define DIM1    200

int	ipoints2[DIM0][DIM1], icheck2[DIM0][DIM1];

hid_t   ERR_CLS;
hid_t   ERR_MAJ_TEST;
hid_t   ERR_MIN_SUBROUTINE;
hid_t   ERR_STACK;

#define DSET_NAME               "a_dataset"

#define ERR_CLS_NAME            "Error Test"
#define PROG_NAME               "Error Program"
#define PROG_VERS               "1.0"

#define SPACE1_DIM1             4
#define SPACE1_RANK             1
#define SPACE2_RANK	        2
#define SPACE2_DIM1	        10
#define SPACE2_DIM2	        10


/*-------------------------------------------------------------------------
 * Function:	test_error
 *
 * Purpose:	Test error API functions
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 10, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_error(hid_t file)
{
    hid_t		dataset, space;
    hid_t               dtype, native_type;
    int			i, j, n;
    hsize_t		dims[2];
    void                *tmp;

    TESTING("error API based on atomic datatype");

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
    if ((dataset = H5Dcreate(file, DSET_NAME, H5T_STD_I32BE, space,
			     H5P_DEFAULT))<0) TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ipoints2)<0)
	TEST_ERROR;

    /* Close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    /* Open dataset again to check H5Tget_native_type */
    if((dataset=H5Dopen(file, DSET_NAME))<0) TEST_ERROR;

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
    if(H5Tconvert(native_type, H5T_NATIVE_INT, (hsize_t)(DIM0*DIM1), icheck2, NULL, H5P_DEFAULT)<0)
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
    if(H5Tclose(dtype)<0) TEST_ERROR;
    if(H5Sclose(space)<0) TEST_ERROR;
        
    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	init_error
 *
 * Purpose:	Initialize error information.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 10, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifndef NEW_ERR 
static herr_t 
init_error(void)
{
    if((ERR_CLS = H5Eregister_class(ERR_CLS_NAME, PROG_NAME, PROG_VERS))<0)
        goto error;

    if((ERR_MAJ_TEST = H5Ecreate_msg(ERR_CLS, H5E_MAJOR_new, "Error in test"))<0)
        goto error;
    if((ERR_MIN_SUBROUTINE = H5Ecreate_msg(ERR_CLS, H5E_MINOR_new, "Error in subroutine"))<0)
        goto error;
    
    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	error_stack
 *
 * Purpose:	Manipulates current error stack.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 14, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
error_stack(void)
{
    if((ERR_STACK = H5Eget_current_stack())<0)
        goto error;

    if(H5Eclose_stack(ERR_STACK)<0)
        goto error;
    
    PASSED();
    return 0;

  error:
    return -1;
}



/*-------------------------------------------------------------------------
 * Function:	close_error
 *
 * Purpose:	Closes error information.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 10, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
close_error(void)
{
    /*
    if(H5Eclose_msg(ERR_MAJ_TEST)<0)
        goto error;
    if(H5Eclose_msg(ERR_MIN_SUBROUTINE)<0)
        goto error;
    */
    
    if(H5Eunregister_class(ERR_CLS)<0)
        goto error;

    PASSED();
    return 0;

  error:
    return -1;
}

#endif /* NEW_ERR */
    

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test error API.
 *
 * Programmer:	Raymond Lu
 *		July 10, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t		file, fapl;
    char		filename[1024];
    const char          *FUNC="main()";
    
    h5_reset();

#ifndef NEW_ERR 
    if(init_error()<0)
        goto error;
#endif /* NEW_ERR */
    
    fapl = h5_fileaccess();
    
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    
#ifndef NEW_ERR 
    if(error_stack()<0)
        goto error;
#endif /* NEW_ERR */

    /*if(test_error(file)<0) {*/
#ifndef NEW_ERR
        /*H5Epush(H5E_DEFAULT, __FILE__, FUNC, __LINE__, ERR_MAJ_TEST, ERR_MIN_SUBROUTINE, "Error test failed");*/
#endif /* NEW_ERR */        
    /*    goto error;
    }*/
    
    if (H5Fclose(file)<0) goto error;
    h5_cleanup(FILENAME, fapl);

#ifndef NEW_ERR 
    if(close_error()<0)
        goto error;
#endif /* NEW_ERR */
    printf("All error API test based on native datatype test passed.\n");
    
    return 0;

 error:
    printf("***** ERROR TEST FAILED! *****\n");
    return 1;
}
