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

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "H5LDprivate.h"

/*-------------------------------------------------------------------------
 *
 * internal functions
 *
 *-------------------------------------------------------------------------
 */
static size_t H5LD_get_dset_type_size(hid_t did, const char *fields);
static herr_t H5LD_get_dset_elmts(hid_t did, const hsize_t *prev_dims, const hsize_t *cur_dims,
				  const char *fields, void *buf);
static herr_t H5LD_get_dset_dims(hid_t did, hsize_t *cur_dims);
static herr_t H5LD_construct_info(H5LD_memb_t *memb, hid_t par_tid);

/*-------------------------------------------------------------------------
 * Function: H5LD_clean_vector
 *
 * Purpose: Process the vector of info:
 *		1) free the array of pointers to member names in listv[n]
 *		2) close the type id of the last member in listv[n]
 *		3) free the H5LD_memb_t structure itself as pointed to by listv[n]
 *
 * Return: void
 *
 * Programmer:  Vailin Choi; Aug 2010
 *
 *-------------------------------------------------------------------------
 */
void
H5LD_clean_vector(H5LD_memb_t *listv[])
{
    unsigned n;		/* Local index variable */

    HDassert(listv);

    /* Go through info for each field stored in listv[] */
    for(n = 0; listv[n] != NULL; n++) {
	if(listv[n]->names) {
	    HDfree(listv[n]->names);
	    listv[n]->names = NULL;
	}

	/* Close the type id of the last member in the field */
	if(!(listv[n]->last_tid < 0)) {
	    H5Tclose(listv[n]->last_tid);
	    listv[n]->last_tid = -1;
	}
	/* Free the H5LD_memb_t structure for the field */
	HDfree(listv[n]);
	listv[n] = NULL;
    }
} /* H5LD_clean_vector() */

/*-------------------------------------------------------------------------
 * Function: H5LD_construct_info()
 *
 * Purpose: Get the remaining info for a field:
 *		1) Get the type id of the last member in the field
 *		2) Get the total offset of all the members in the field
 *		3) Get the type size of the last member in the field
 *
 * Return: Success: 0
 *	   Failure: negative
 *
 * Programmer:  Vailin Choi; Aug 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5LD_construct_info(H5LD_memb_t *memb, hid_t par_tid)
{
    hid_t tmp_tid=-1;	/* Dataset type id */
    hid_t memb_tid=-1;	/* Type id for a member in a field */
    unsigned i;		/* Local index variable */
    int idx;		/* Index # of a member in a compound data type */
    herr_t ret_value = SUCCEED;	/* Return value */

    tmp_tid = H5Tcopy(par_tid);

    /* Validate all the members in a field */
    for(i = 0; memb->names[i] != NULL; i++) {
        /* Get the member index and member type id */
        if((idx = H5Tget_member_index(tmp_tid, memb->names[i])) < 0 ||
           (memb_tid = H5Tget_member_type(tmp_tid, (unsigned)idx)) < 0) {
            ret_value = FAIL;
            goto error;
        }

	/* Sum up the offset of all the members in the field */
        memb->tot_offset += H5Tget_member_offset(tmp_tid, (unsigned)idx);
        if(H5Tclose(tmp_tid) < 0) {
            ret_value = FAIL;
            goto error;
	}
        tmp_tid = memb_tid;
    }

    /* Get the type size of the last member in the field */
    memb->last_tsize = H5Tget_size(tmp_tid);
    /* Save the type id of the last member in the field */
    memb->last_tid = H5Tcopy(tmp_tid);

error:
    H5E_BEGIN_TRY
	H5Tclose(tmp_tid);
    H5E_END_TRY

    return(ret_value);
} /* H5LD_construct_info() */


/*-------------------------------------------------------------------------
 * Function: H5LD_construct_vector
 *
 * Purpose: Process the comma-separated list of fields in "fields" as follows:
 * 	Example:
 *		"fields": "a.b.c,d"
 *		listv[0]->tot_offset = total offset of "a" & "b" & "c"
 *		listv[0]->last_tid = type id of "c"
 *		listv[0]->last_tsize = type size of "c"
 *		listv[0]->names[0] = "a"
 *		listv[0]->names[1] = "b"
 *		listv[0]->names[2] = "c"
 *		listv[0]->names[3] = NULL
 *
 *		listv[1]->tot_offset = offset of "d"
 *		listv[1]->last_tid = type id of "d"
 *		listv[1]->last_tsize = type size of "d"
 *		listv[1]->names[0] = "d"
 *		listv[1]->names[1] = NULL
 *
 * Return: Success: # of comma-separated fields in "fields"
 *	   Failure: negative value
 *
 * Programmer:  Vailin Choi; Aug 2010
 *
*-------------------------------------------------------------------------
*/
int
H5LD_construct_vector(char *fields, H5LD_memb_t *listv[]/*OUT*/, hid_t par_tid)
{
    int n;		/* The # of comma-separated fields in "fields" */
    hbool_t valid;	/* Whether a field being processed is valid or not */
    hbool_t end_of_fields = FALSE;	/* end of "fields" */
    char *fields_ptr;	/* Pointer to "fields" */
    char *cur;		/* Pointer to a member in a field */
    int ret_value = SUCCEED;		/* Return value */

    HDassert(listv);
    HDassert(fields);

    fields_ptr = fields;
    n = 0;

    /* Process till end of "fields" */
    while(!end_of_fields) {

	hbool_t gotcomma = FALSE;	/* A comma encountered */
	hbool_t gotmember = FALSE;	/* Getting member in a field */
	H5LD_memb_t *memb = NULL; 	/* Pointer to structure for storing a field's info */
	size_t len;			/* Estimated # of members in a field */
	int j = 0;			/* The # of members in a field */

	valid = TRUE;
	len = HDstrlen(fields_ptr)/2 + 2;

	/* Allocate memory for an H5LD_memb_t for storing a field's info */
	if((memb = (H5LD_memb_t *)HDcalloc((size_t)1, sizeof(H5LD_memb_t))) == NULL) {
	    ret_value = FAIL;
	    break;
	} 
	/* Allocate memory for an array of pointers to member names */
	if((memb->names = (char **)HDcalloc(len, sizeof(char *))) == NULL) {
	    ret_value = FAIL;
	    break;
	}

	memb->names[j] = fields_ptr;
	memb->last_tid = -1;
	cur = fields_ptr;

	/* Continue processing till: not valid or comma encountered or "fields" ended */
	while(valid && !gotcomma && !end_of_fields) {

	    switch(*fields_ptr) {

	      case '\0':	/* end of list */
		if(gotmember) { /* getting something and end of "fields" */
		    *cur++ = '\0';;
		    memb->names[++j] = NULL;
		} else /* getting nothing but end of list */
		    valid = FALSE;
		end_of_fields = TRUE;
		break;
	
	      case '\\': /* escape character */
		++fields_ptr; /* skip it */
		if(*fields_ptr == '\0')
		    valid = FALSE;
		else {
		    *cur++ = *fields_ptr++;
		    gotmember = TRUE;
		}
		break;

	      case '.': /* nested field separator */
		*fields_ptr++ = *cur++ = '\0';;
		if(gotmember) {
		    memb->names[++j] = cur;
		    gotmember = FALSE;
		} else
		    valid = FALSE;
		break;

	      case ',': /* field separator */
		*fields_ptr++ = *cur++ = '\0';;
		if(gotmember) {
		    memb->names[++j] = NULL;
		    gotmember = FALSE;
		} else
		    valid = FALSE;
		gotcomma = TRUE;
		break;

	      default: 
		*cur++ = *fields_ptr++;
		gotmember = TRUE;
		break;
	    }
	} /* while (valid && !gotcomma && !end_of_fields) */

	/* If valid, put into listv and continue processing further info */
	if(valid) {
	    listv[n++] = memb;
	    if((ret_value = H5LD_construct_info(memb, par_tid)) < 0)
		break;
	} else {
	    if(memb) HDfree(memb);
	    ret_value = FAIL;
	    break;
	}
    } /* while !end_of_fields */

    listv[n] = NULL;
    if(ret_value == FAIL)
	(void) H5LD_clean_vector(listv);
    else
	ret_value = n;

    return(ret_value);
}  /* H5LD_construct_vector() */

/*-------------------------------------------------------------------------
 * Function: H5LD_get_dset_dims
 *
 * Purpose: To return the current size for each dimension of the
 *	    dataset's dataspace
 *
 * Return: Success: 0
 *	   Failure: negative value
 *
 * Programmer:  Vailin Choi; March 2010
 *
*-------------------------------------------------------------------------
*/
static herr_t
H5LD_get_dset_dims(hid_t did, hsize_t *cur_dims)
{
    hid_t sid=-1;              	/* dataspace id */
    int ret_value = SUCCEED;    /* return_value */

    /* Verify parameter */
    if(cur_dims == NULL) {
        ret_value = FAIL;
        goto out;
    }

    /* Get the dataset's dataspace */
    if((sid = H5Dget_space(did)) < 0) {
        ret_value = FAIL;
        goto out;
    }

    /* Get the current dimension size */
    if(H5Sget_simple_extent_dims(sid, cur_dims, NULL) < 0)
        ret_value = FAIL;

out:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
    } H5E_END_TRY;
    return(ret_value);
} /* H5LD_get_dset_dims() */

/*-------------------------------------------------------------------------
 * Function: H5LD_get_dset_type_size
 *
 * Purpose: To return the size of the dataset's data type in bytes
 *	null "fields": return the size of the dataset's data type
 *	non-null "fields": return the size of the dataset's data type
 *			   with respect to the selection in "fields"
 *
 * Return: Success: size of the dataset's data type
 *	   Failure: 0 (valid datatypes are never zero size)
 *
 * Programmer:  Vailin Choi; March 2010
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5LD_get_dset_type_size(hid_t did, const char *fields)
{
    hid_t dset_tid=-1;		/* Dataset's type identifier */
    hid_t tid=-1;		/* Native Type identifier */
    size_t tot = 0;		/* Data type size of all the fields in "fields" */
    H5LD_memb_t **listv = NULL;	/* Vector for storing information in "fields" */
    char *dup_fields = NULL;	/* A copy of "fields" */
    int n = 0, num = 0;		/* Local index variable */
    size_t len;			/* Estimate the number of comma-separated fields in "fields" */
    size_t ret_value = 0;       /* Return value */

    /* Get the data type of the dataset */
    if(((dset_tid = H5Dget_type(did)) < 0) || (tid = H5Tget_native_type(dset_tid, H5T_DIR_DEFAULT)) < 0)
        goto out;

    if(fields == NULL) /* If no "fields" is specified */
        ret_value = H5Tget_size(tid);
    else { /* "fields" are specified */
        HDassert(fields && *fields);

        /* Should be a compound data type if "fields" exists */
        if(H5Tget_class(dset_tid) != H5T_COMPOUND)
            goto out;

	/* Get a copy of "fields" */
        if((dup_fields = HDstrdup(fields)) == NULL)
            goto out;

	/* Allocate memory for a list of H5LD_memb_t pointers to store "fields" info */
	len = HDstrlen(fields)/2 + 2;
	if((listv = (H5LD_memb_t **)HDcalloc(len, sizeof(H5LD_memb_t *))) == NULL)
            goto out;

	/* Process and store info for "fields" */
	if((num = H5LD_construct_vector(dup_fields, listv/*OUT*/, tid)) < 0)
            goto out;

	/* Sum up the size of all the data types in "fields" */
	for(n = 0; n < num; n++)
	    tot += listv[n]->last_tsize;

	/* Clean up the vector of H5LD_memb_t structures */
	(void) H5LD_clean_vector(listv);

	/* Return the total size */
	ret_value = tot;
    }

out:
    H5E_BEGIN_TRY
        H5Tclose(tid);
        H5Tclose(dset_tid);
    H5E_END_TRY

    /* Free the array of H5LD_memb_t pointers */
    if(listv) HDfree(listv);
    /* Free memory */
    if(dup_fields) HDfree(dup_fields);

    return(ret_value);
} /* H5LD_get_dset_type_size() */

/*-------------------------------------------------------------------------
 * Function: H5LD_get_dset_elmts
 *
 * Purpose: To retrieve selected data from the dataset
 *
 * Return: Success: 0
 *	   Failure: negative
 *
 * Programmer:  Vailin Choi; August 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5LD_get_dset_elmts(hid_t did, const hsize_t *prev_dims, const hsize_t *cur_dims, const char *fields, void *buf)
{
    int ndims;			/* Number of dimensions for the dataset */
    hid_t dtid=-1, tid=-1;	/* Dataset type ids */
    size_t tot_tsize;		/* Total data type size */
    hid_t sid=-1, mid=-1;	/* Dataspace and memory space ids */
    hsize_t num_elmts;	/* Number of dataset elements in the selection */
    hsize_t start[H5S_MAX_RANK];/* Starting offset */
    hsize_t count[H5S_MAX_RANK];/* ??offset */
    H5LD_memb_t **listv = NULL;	/* Vector for storing information in "fields" */
    char *dup_fields = NULL;	/* A copy of "fields" */
    char *tmp_buf = NULL;	/* Temporary buffer for data read */
    char *sav_buf = NULL;	/* Saved pointer temporary buffer */
    int n = 0, i = 0; 		/* Local index variable */
    size_t len;			/* Estimate the number of comma-separated fields in "fields" */
    unsigned int ctr = 0;	/* Counter for # of curr_dims > prev_dims */
    herr_t ret_value = SUCCEED;	/* Return value */

    /* Verify parameters */
    if(prev_dims == NULL || cur_dims == NULL || buf == NULL) {
	ret_value = FAIL;
	goto done;
    }

    /* Get dataset's dataspace */
    if((sid = H5Dget_space(did)) < 0) {
	ret_value = FAIL;
	goto done;
    }
    
    /* Get the number of dimensions */
    if((ndims = H5Sget_simple_extent_ndims(sid)) < 0) {
	ret_value = FAIL;
	goto done;
    }

    /* Verify that cur_dims must have one dimension whose size is greater than prev_dims */
    HDmemset(start, 0, sizeof start);
    HDmemset(count, 0, sizeof count);
    for(i = 0; i < ndims; i++)
	if(cur_dims[i] > prev_dims[i]) {
	    ++ctr;
	    count[i] = cur_dims[i] - prev_dims[i];
	    start[i] = prev_dims[i];
	} else { /* < or = */
	    start[i] = 0;
	    count[i] = MIN(prev_dims[i], cur_dims[i]);
	}

    if(!ctr) {
	ret_value = FAIL;
	goto done;
    }

    if(ctr == 1) { /* changes for only one dimension */
	/* Make the selection in the dataset based on "cur_dims" and "prev_dims" */
	if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
	    ret_value = FAIL;
	    goto done;
	}
    } else { /* changes for more than one dimensions */

	HDmemset(start, 0, sizeof start);
	/* Make the selection in the dataset based on "cur_dims" and "prev_dims" */
	if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, cur_dims, NULL) < 0) {
	    ret_value = FAIL;
	    goto done;
	}
	if(H5Sselect_hyperslab(sid, H5S_SELECT_NOTB, start, NULL, prev_dims, NULL) < 0) {
	    ret_value = FAIL;
	    goto done;
	}
    }

    /* Get the number of elements in the selection */
    if((num_elmts = H5Sget_select_npoints(sid)) == 0) {
        ret_value = FAIL;
        goto done;
    }

    /* Create the memory space for the selection */
    if((mid = H5Screate_simple(1, &num_elmts, NULL)) < 0) {
	ret_value = FAIL;
	goto done;
    }

    /* Get the native data type size */
    if(((dtid = H5Dget_type(did)) < 0) || (tid = H5Tget_native_type(dtid, H5T_DIR_DEFAULT)) < 0) {
	ret_value = FAIL;
        goto done;
    }

    if(fields == NULL) { /* nothing in "fields" */
	/* Read and store all the elements in "buf" */
	if(H5Dread(did, tid, mid, sid, H5P_DEFAULT, buf) < 0) {
	    ret_value = FAIL;
	    goto done;
	}
    } else { /* "fields" is specified */
	unsigned char *buf_p = (unsigned char *)buf;   /* Pointer to the destination buffer */

	/* should be a compound data type if "fields" exists */
	if(H5Tget_class(tid) != H5T_COMPOUND) {
	    ret_value = FAIL;
	    goto done;
	}

	/* Get the total size of the dataset's data types */
	if((tot_tsize = H5LD_get_dset_type_size(did, NULL)) == 0) {
	    ret_value = FAIL;
	    goto done;
	}
	
	/* Allocate memory for reading in the elements in the dataset selection */
	if((sav_buf = tmp_buf = (char *)HDcalloc((size_t)num_elmts, tot_tsize)) == NULL) {
	    ret_value = FAIL;
	    goto done;
	}

	/* Read the dataset elements in the selection */
	if(H5Dread(did, tid, mid, sid, H5P_DEFAULT, tmp_buf) < 0) {
	    ret_value = FAIL;
	    goto done;
	}

	/* Make a copy of "fields" */
	if((dup_fields = HDstrdup(fields)) == NULL) {
	    ret_value = FAIL;
	    goto done;
	}

	/* Allocate memory for the vector of H5LD_memb_t pointers */
	len = HDstrlen(fields)/2 + 2;
        if((listv = (H5LD_memb_t **)HDcalloc(len, sizeof(H5LD_memb_t *))) == NULL) {
	    ret_value = FAIL;
	    goto done;
	}

	/* Process and store information for "fields" */
        if(H5LD_construct_vector(dup_fields, listv, tid) < 0) {
	    ret_value = FAIL;
            goto done;
        }

	/* Copy data for each dataset element in the selection */
	for(i = 0; i < (int)num_elmts; i++) {
	    /* Copy data for "fields" to the input buffer */
	    for(n = 0; listv[n] != NULL; n++) {
		HDmemcpy(buf_p, tmp_buf + listv[n]->tot_offset, listv[n]->last_tsize);
		buf_p += listv[n]->last_tsize;
	    }
	    tmp_buf += tot_tsize;
	}

	/* Clean up the vector of H5LD_memb_t structures */
	(void) H5LD_clean_vector(listv);
    }

done:
    H5E_BEGIN_TRY
	H5Tclose(dtid);
	H5Tclose(tid);
	H5Sclose(sid);
	H5Sclose(mid);
    H5E_END_TRY

    /* Free the array of H5LD_memb_t pointers */
    if(listv) HDfree(listv);
    /* Free memory */
    if(dup_fields) HDfree(dup_fields);
    if(sav_buf) HDfree(sav_buf);

    return(ret_value);
} /* H5LD_get_dset_elmts() */

/*-------------------------------------------------------------------------
 *
 * Public functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5LDget_dset_dims
 *
 * Purpose: To retrieve the current dimension sizes for a dataset
 *
 * Return: Success: 0
 *	   Failure: negative value
 *
 * Programmer:  Vailin Choi; March 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5LDget_dset_dims(hid_t did, hsize_t *cur_dims)
{
    int ret_value;	/* return_value */

    ret_value = H5LD_get_dset_dims(did, cur_dims);
	
    return(ret_value);
} /* H5LDget_dset_dims() */


/*-------------------------------------------------------------------------
 * Function: H5LDget_dset_type_size
 *
 * Purpose:  To return the size in bytes of the data type for the dataset
 *
 * Return: Success: size in bytes of the dataset's data type
 *	   Failure: 0 (valid datatypes are never zero size)
 *
 * Programmer:  Vailin Choi; March 2010
 *
 *-------------------------------------------------------------------------
 */
size_t
H5LDget_dset_type_size(hid_t did, const char *fields)
{
    size_t ret_value;	/* Return value */

    ret_value = H5LD_get_dset_type_size(did, fields);

    return(ret_value);
} /* H5LDget_dset_type_size() */

/*-------------------------------------------------------------------------
 * Function: H5LDget_dset_elmts
 *
 * Purpose: To retrieve selected data from the dataset
 *
 * Return: Success: 0
 *	   Failure: negative value
 *
 * Programmer:  Vailin Choi; March 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5LDget_dset_elmts(hid_t did, const hsize_t *prev_dims, const hsize_t *cur_dims, const char *fields, void *buf)
{
    herr_t ret_value; 	/* Return value */

    ret_value = H5LD_get_dset_elmts(did, prev_dims, cur_dims, fields, buf);

    return(ret_value);
} /* H5LDget_dset_elmts() */
