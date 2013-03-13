#include "dynlib1.h"

/* This message derives from H5Z */
const H5Z_class2_t H5Z_DYNLIB1[1] = {{
    H5Z_CLASS_T_VERS,                /* H5Z_class_t version             */
    H5Z_FILTER_DYNLIB1,		     /* Filter id number		*/
    1, 1,                            /* Encoding and decoding enabled   */
    "dynlib1",			     /* Filter name for debugging	*/
    NULL,                            /* The "can apply" callback        */
    NULL,                            /* The "set local" callback        */
    (H5Z_func_t)H5Z_filter_dynlib1,    /* The actual filter function	*/
}};

const H5PL_type_t   H5PL_get_plugin_type(void) {return H5PL_TYPE_FILTER;}
const int           H5PL_get_plugin_version(void) {return (int)FILTER_DYNLIB1_VERS;}
const char*         H5PL_get_plugin_name(void) {return "dynlib1";}
const H5Z_class2_t* H5PL_get_plugin_info(void) {return H5Z_DYNLIB1;}

/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_dynlib1
 *
 * Purpose:	A dynlib1 compression method that doesn't do anything.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_filter_dynlib1(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    int *int_ptr=(int *)*buf;          /* Pointer to the data values */
    size_t buf_left=*buf_size;  /* Amount of data buffer left to process */
    int         add_on = 0;

/*fprintf(stderr, "cd_nelmts=%d, cd_values=%d\n", cd_nelmts, cd_values[0]);*/
    /* Check for the correct number of parameters */
    if(cd_nelmts==0)
        return(0);

    /* Check that permanent parameters are set correctly */
    if(cd_values[0]<0 || cd_values[0]>9)
        return(0);
  
    add_on = cd_values[0];

/*fprintf(stderr, "add_on=%d\n", add_on);*/

    if(flags & H5Z_FLAG_REVERSE) { /*read*/
        /* Substract the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ -= add_on;
            buf_left -= sizeof(int);
        } /* end while */
    } /* end if */
    else { /*write*/
        /* Add the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ += add_on;
            buf_left -= sizeof(int);
        } /* end while */
    } /* end else */

/*fprintf(stderr, "nbytes=%d\n", nbytes);*/
 
    return nbytes;
}
