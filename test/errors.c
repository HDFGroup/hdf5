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
hid_t   ERR_STACK;

hid_t   ERR_MAJ_TEST;
hid_t   ERR_MAJ_IO;
hid_t   ERR_MAJ_API;

hid_t   ERR_MIN_SUBROUTINE;
hid_t   ERR_MIN_ERRSTACK;
hid_t   ERR_MIN_CREATE;
hid_t   ERR_MIN_GETNUM;

#define DSET_NAME               "a_dataset"

#define ERR_CLS_NAME            "Error Test"
#define PROG_NAME               "Error Program"
#define PROG_VERS               "1.0"

#define ERR_MAJ_TEST_MSG             "Error in test"
#define ERR_MAJ_IO_MSG               "Error in IO"
#define ERR_MAJ_API_MSG              "Error in API"
#define ERR_MIN_SUBROUTINE_MSG       "Error in subroutine"
#define ERR_MIN_ERRSTACK_MSG         "Error in error stack"
#define ERR_MIN_CREATE_MSG           "Error in H5Dcreate"
#define ERR_MIN_GETNUM_MSG           "Error in H5Eget_num"

#define MSG_SIZE                64
#define SPACE1_DIM1             4
#define SPACE1_RANK             1
#define SPACE2_RANK	        2
#define SPACE2_DIM1	        10
#define SPACE2_DIM2	        10

herr_t custom_print_cb(int n, H5E_error_t_new *err_desc, void* client_data);


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
    const char          *FUNC_test_error="test_error";
    
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
    /* Make it H5Dcreate fail */
    /*file = 0;*/
    if ((dataset = H5Dcreate(file, DSET_NAME, H5T_STD_I32BE, space,
			     H5P_DEFAULT))<0) {
#ifndef NEW_ERR
        H5Epush_new(H5E_DEFAULT, __FILE__, FUNC_test_error, __LINE__, ERR_MAJ_IO, ERR_MIN_CREATE, 
                "H5Dcreate failed");
#endif /* NEW_ERR */        
        goto error;
        /*TEST_ERROR;*/
    }

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
		TEST_ERROR ;
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
    ssize_t cls_size = strlen(ERR_CLS_NAME)+1;
    char *cls_name = malloc(strlen(ERR_CLS_NAME)+1);
    ssize_t msg_size = strlen(ERR_MIN_SUBROUTINE_MSG) + 1;
    char *msg = malloc(strlen(ERR_MIN_SUBROUTINE_MSG)+1);
    H5E_type_t *msg_type= malloc(sizeof(H5E_type_t));
    
    if((ERR_CLS = H5Eregister_class(ERR_CLS_NAME, PROG_NAME, PROG_VERS))<0)
        TEST_ERROR;
    if((ERR_CLS = H5Eregister_class(ERR_CLS_NAME, PROG_NAME, PROG_VERS))<0)
        TEST_ERROR;

    if(cls_size != H5Eget_class_name(ERR_CLS, cls_name, (size_t)cls_size) + 1) 
        TEST_ERROR;
    if(strcmp(ERR_CLS_NAME, cls_name)) 
        TEST_ERROR;
   
    if((ERR_MAJ_TEST = H5Ecreate_msg(ERR_CLS, H5E_MAJOR_new, ERR_MAJ_TEST_MSG))<0)
        TEST_ERROR;
    if((ERR_MAJ_IO = H5Ecreate_msg(ERR_CLS, H5E_MAJOR_new, ERR_MAJ_IO_MSG))<0)
        TEST_ERROR;
    if((ERR_MAJ_API = H5Ecreate_msg(ERR_CLS, H5E_MAJOR_new, ERR_MAJ_API_MSG))<0)
        TEST_ERROR;

    if((ERR_MIN_SUBROUTINE = H5Ecreate_msg(ERR_CLS, H5E_MINOR_new, ERR_MIN_SUBROUTINE_MSG))<0)
        TEST_ERROR;
    if((ERR_MIN_ERRSTACK = H5Ecreate_msg(ERR_CLS, H5E_MINOR_new, ERR_MIN_ERRSTACK_MSG))<0)
        TEST_ERROR;
    if((ERR_MIN_CREATE = H5Ecreate_msg(ERR_CLS, H5E_MINOR_new, ERR_MIN_CREATE_MSG))<0)
        TEST_ERROR;
    if((ERR_MIN_GETNUM = H5Ecreate_msg(ERR_CLS, H5E_MINOR_new, ERR_MIN_GETNUM_MSG))<0)
        TEST_ERROR;

    if(msg_size != H5Eget_msg(ERR_MIN_SUBROUTINE, msg_type, msg, (size_t)msg_size) + 1)
        TEST_ERROR;
    if(*msg_type != H5E_MINOR_new)
        TEST_ERROR;
    if(strcmp(msg, ERR_MIN_SUBROUTINE_MSG))
        TEST_ERROR;

    free(cls_name);
    free(msg);
    free(msg_type);

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
    int err_num;
    const char          *FUNC_error_stack="error_stack";
    
    if((err_num = H5Eget_num(H5E_DEFAULT))<0)
        TEST_ERROR;
    if(err_num)
        TEST_ERROR;
    
    if((ERR_STACK = H5Eget_current_stack())<0)
        TEST_ERROR;

    /* Make it push error */
    if((err_num = H5Eget_num(ERR_STACK))<0) {
        H5Epush_new(ERR_STACK, __FILE__, FUNC_error_stack, __LINE__, ERR_MAJ_API, ERR_MIN_GETNUM, 
                "Get number test failed, returned %d", err_num);
        goto error;
        /*TEST_ERROR;*/
    } 
    if(err_num)
        TEST_ERROR;
  
    if(H5Eclear_new(ERR_STACK)<0)
        TEST_ERROR; 
    if(H5Eclose_stack(ERR_STACK)<0)
        TEST_ERROR;
    
    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    dump_error	
 *
 * Purpose:	Prints error stack in default and customized ways.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 17, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
dump_error(hid_t estack)
{
    /* Print errors in library default way */
    fprintf(stderr, "********* Print error stack in HDF5 default way *********\n");
    H5Eprint_new(estack, stderr);
    
    /* Customized way to print errors */
    fprintf(stderr, "\n********* Print error stack in customized way *********\n");
    H5Ewalk_new(estack, H5E_WALK_UPWARD, custom_print_cb, stderr);
    
    PASSED();
    return 0;

  error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    custom_print_cb
 *
 * Purpose:	Callback function to print error stack in customized way.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 17, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t 
custom_print_cb(int n, H5E_error_t_new *err_desc, void* client_data)
{
    FILE		*stream  = (FILE *)client_data;
    char                maj[MSG_SIZE];
    char                min[MSG_SIZE];
    char                cls[MSG_SIZE];
    const int		indent = 4;

    /* Get descriptions for the major and minor error numbers */
    H5Eget_class_name(err_desc->cls_id, cls, MSG_SIZE);
    H5Eget_msg(err_desc->maj_id, NULL, maj, MSG_SIZE);
    H5Eget_msg(err_desc->maj_id, NULL, min, MSG_SIZE);

    fprintf (stream, "%*serror #%03d: %s in %s(): line %u\n",
	     indent, "", n, err_desc->file_name,
	     err_desc->func_name, err_desc->line);
    fprintf (stream, "%*sclass: %s\n", indent*2, "", cls);
    fprintf (stream, "%*smajor: %s\n", indent*2, "", maj);
    fprintf (stream, "%*sminor: %s\n", indent*2, "", min);

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
        TEST_ERROR ;
    if(H5Eclose_msg(ERR_MIN_SUBROUTINE)<0)
        TEST_ERROR ;
    */
    
    if(H5Eunregister_class(ERR_CLS)<0)
        TEST_ERROR ;

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
    const char          *FUNC_main="main";

    h5_reset();
#ifndef NEW_ERR 
    if(init_error()<0)
        TEST_ERROR ;
#endif /* NEW_ERR */
    
    fapl = h5_fileaccess();
    
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	TEST_ERROR ;
    
#ifndef NEW_ERR 
    if(error_stack()<0) {
        H5Epush_new(ERR_STACK, __FILE__, FUNC_main, __LINE__, ERR_MAJ_TEST, ERR_MIN_ERRSTACK, 
                "Error stack test failed"); 
        H5Epop(ERR_STACK, 1);
        dump_error(ERR_STACK);
        /*H5Eprint_new(ERR_STACK, stderr);*/
        H5Eclose_stack(ERR_STACK);
        TEST_ERROR;
    }
#endif /* NEW_ERR */

    if(test_error(file)<0) {
#ifndef NEW_ERR
        H5Epush_new(H5E_DEFAULT, __FILE__, FUNC_main, __LINE__, ERR_MAJ_TEST, ERR_MIN_SUBROUTINE, 
                "Error test failed, %s", "it's wrong");
        H5Eprint_new(H5E_DEFAULT, stderr);
#endif /* NEW_ERR */
        goto error;        
        /*TEST_ERROR;*/
    }
    
    if (H5Fclose(file)<0) TEST_ERROR ;
    h5_cleanup(FILENAME, fapl);

#ifndef NEW_ERR 
    if(close_error()<0)
        TEST_ERROR ;
#endif /* NEW_ERR */
    printf("All error API test based on native datatype test passed.\n");
    
    return 0;

 error:
    printf("***** ERROR TEST FAILED! *****\n");
    return 1;
}
