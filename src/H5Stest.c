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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Saturday, May 31, 2003
 *
 * Purpose:	Dataspace selection testing functions.
 */

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */
#define H5S_TESTING		/*suppress warning about H5S testing funcs*/


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Spkg.h"		/* Dataspaces 				*/


/*--------------------------------------------------------------------------
 NAME
    H5S_select_shape_same_test
 PURPOSE
    Determine if two dataspace selections are the same shape
 USAGE
    htri_t H5S_select_shape_same_test(sid1, sid2)
        hid_t sid1;          IN: 1st dataspace to compare
        hid_t sid2;          IN: 2nd dataspace to compare
 RETURNS
    Non-negative TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Checks to see if the current selection in the dataspaces are the same
    dimensionality and shape.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_select_shape_same_test(hid_t sid1, hid_t sid2)
{
    H5S_t	*space1;                /* Pointer to 1st dataspace */
    H5S_t	*space2;                /* Pointer to 2nd dataspace */
    htri_t      ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get dataspace structures */
    if(NULL == (space1 = (H5S_t *)H5I_object_verify(sid1, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")
    if(NULL == (space2 = (H5S_t *)H5I_object_verify(sid2, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Check if the dataspace selections are the same shape */
    if((ret_value = H5S_select_shape_same(space1, space2)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOMPARE, FAIL, "unable to compare dataspace selections")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5S_select_shape_same_test() */


/*--------------------------------------------------------------------------
 NAME
    H5S_get_rebuild_status_test
 PURPOSE
    Determine the status of the diminfo_valid field (whether we know the
    selection information for an equivalent single hyperslab selection)
    before and after calling H5S_hyper_rebuild.
 USAGE
    herr_t H5S_inquiry_rebuild_status(hid_t space_id)
        hid_t space_id;          IN:  dataspace id
        H5S_diminfo_valid_t *status1; OUT: status before calling
                                           H5S_hyper_rebuild
        H5S_diminfo_valid_t *status2; OUT: status after calling
                                           H5S_hyper_rebuild
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Query the status of rebuilding the hyperslab
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_get_rebuild_status_test(hid_t space_id, H5S_diminfo_valid_t *status1,
    H5S_diminfo_valid_t *status2)
{
    H5S_t *space;               /* Pointer to 1st dataspace */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(status1);
    HDassert(status2);

     /* Get dataspace structures */
    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    *status1 = space->select.sel_info.hslab->diminfo_valid;

    /* Fully rebuild diminfo, if necessary */
    if(*status1 == H5S_DIMINFO_VALID_NO)
        if(H5S_hyper_rebuild(space) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't update hyperslab info")

    *status2 = space->select.sel_info.hslab->diminfo_valid;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5S_get_rebuild_status_test() */


/*--------------------------------------------------------------------------
 NAME
    H5S_get_diminfo_status_test
 PURPOSE
    Determine the status of the diminfo_valid field (whether we know the
    selection information for an equivalent single hyperslab selection)
 USAGE
    herr_t H5S_inquiry_rebuild_status(hid_t space_id)
        hid_t space_id;          IN:  dataspace id
        H5S_diminfo_valid_t *status; OUT: status of diminfo_valid
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Query the status of rebuilding the hyperslab
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_get_diminfo_status_test(hid_t space_id, H5S_diminfo_valid_t *status)
{
    H5S_t *space;               /* Pointer to 1st dataspace */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(status);

     /* Get dataspace structures */
    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    *status = space->select.sel_info.hslab->diminfo_valid;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5S_get_diminfo_status_test() */

/*--------------------------------------------------------------------------
 NAME
    H5S_check_spans_tail_ptr
 PURPOSE
    Determine if the tail pointer of the spans are correctly set
 USAGE
    herr_t H5S_check_spans_tail_ptr(span_lst)
        const H5S_hyper_span_info_t *span_lst;  IN: the spans to check for taill pointers        
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Checks to see if the current selection in the dataspaces has tail pointers of each
    dimension correctly set.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Only check the hyperslab selection
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_check_spans_tail_ptr(const H5S_hyper_span_info_t *span_lst)
{
    H5S_hyper_span_t *cur_elem;
    H5S_hyper_span_t *actual_tail = NULL;
    htri_t ret_value = TRUE;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(span_lst);
    
    cur_elem = span_lst->head;
    while(cur_elem) {
        actual_tail = cur_elem;        

        /* check the next dimension of lower order */
        if(NULL != cur_elem->down)
            if((ret_value = H5S_check_spans_tail_ptr(cur_elem->down)) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "the seletion has inconsistent tail pointers")

        cur_elem = cur_elem->next;
    } /* end while */
    if(actual_tail != span_lst->tail)
        HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "the seletion has inconsistent tail pointers")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5S_check_spans_tail_ptr */

/*--------------------------------------------------------------------------
 NAME
    H5S_check_points_tail_ptr
 PURPOSE
    Determine if the tail pointer of the points list are correctly set
 USAGE
    herr_t H5S_check_points_tail_ptr(pnt_lst)
        const H5S_pnt_list_t *pnt_lst;  IN: the points list to check for taill pointers        
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Checks to see if the current selection in the dataspaces has tail pointers correctly set.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Only check the points selection
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_check_points_tail_ptr(const H5S_pnt_list_t *pnt_lst)
{
    H5S_pnt_node_t *cur_elem;
    H5S_pnt_node_t *actual_tail = NULL;
    htri_t ret_value = TRUE;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(pnt_lst);
    
    cur_elem = pnt_lst->head;
    while(cur_elem) {
        actual_tail = cur_elem;
        cur_elem = cur_elem->next;
    } /* end while */
    if(actual_tail != pnt_lst->tail)
        HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "the seletion has inconsistent tail pointers")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5S_check_points_tail_ptr */


/*--------------------------------------------------------------------------
 NAME
    H5S_check_internal_consistency
 PURPOSE
    Determine if internal data structures are consistent
 USAGE
    herr_t H5S_check_internal_consistency(space)
        const H5S_t *space;         IN: 1st Dataspace pointer to compare        
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Checks to see if the current selection in the dataspaces has consistent
    state of internal data structure.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Currently only check the hyperslab selection
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5S_check_internal_consistency(const H5S_t *space)
{
    hsize_t low_bounds[H5S_MAX_RANK];
    hsize_t high_bounds[H5S_MAX_RANK];
    unsigned u;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(space);

    if(space->select.type->type == H5S_SEL_NONE)
        HGOTO_DONE(ret_value);
            
    /* Initialize the inputs */
    for(u = 0; u < space->extent.rank; u++) {
        low_bounds[u] = HSIZET_MAX;
        high_bounds[u] = 0;
    } /* end for */

    /* Check the bound box */
    if(H5S_get_select_bounds(space, low_bounds, high_bounds) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "the bound box could not be retrieved")

    for(u = 0; u < space->extent.rank; u++) {
        if((hsize_t)((hssize_t)space->select.low_bounds[u] + space->select.offset[u]) != low_bounds[u]) 
            HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "the lower bound box of the selection is inconsistent")
        if((hsize_t)((hssize_t)space->select.high_bounds[u] + space->select.offset[u]) != high_bounds[u]) 
            HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "the higher bound box of the selection is inconsistent")
    } /* end for */

    if(space->select.type->type == H5S_SEL_HYPERSLABS) {
        H5S_hyper_sel_t *hslab = space->select.sel_info.hslab;

        /* check the tail pointer */
        if((NULL != hslab) && (NULL != hslab->span_lst))
            if(H5S_check_spans_tail_ptr(hslab->span_lst) < 0)           
                HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "the seletion has inconsistent tail pointers")
    } /* end if */
    else if(space->select.type->type == H5S_SEL_POINTS) {
        H5S_pnt_list_t *pnt_lst = space->select.sel_info.pnt_lst;

        if(NULL != pnt_lst)
            if(H5S_check_points_tail_ptr(pnt_lst) < 0)           
                HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "the seletion has inconsistent tail pointers")        
    } /* end else-if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5S_check_internal_consistency */

/*--------------------------------------------------------------------------
 NAME
    H5S_internal_consistency_test
 PURPOSE
    Determine if states of internal data structures are consistent
 USAGE
    htri_t H5S_internal_consistency_test(hid_t space_id)
        hid_t space_id;          IN:  dataspace id
 RETURNS
    Non-negative TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Check the states of internal data structures of the hyperslab, and see
    whether they are consistent or not
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5S_internal_consistency_test(hid_t space_id)
{
    H5S_t *space;               /* Pointer to 1st dataspace */
    htri_t ret_value = TRUE;           /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

     /* Get dataspace structures */
    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Check if the dataspace selections are the same shape */
    if(FAIL == H5S_check_internal_consistency(space))
        HGOTO_ERROR(H5E_DATASPACE, H5E_INCONSISTENTSTATE, FAIL, "The dataspace has inconsistent internal state")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5S_internal_consistency_test() */

