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

/* This files contains C stubs for H5D Fortran APIs */

#include "H5DSprivate.h"
#include "H5LTf90proto.h"
#include "H5Eprivate.h"

/*-------------------------------------------------------------------------
* Function: h5dsset_scale_c
*
* Purpose: Calls H5DSset_scale
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 17, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsset_scale_c(hid_t_f *dsid, _fcd dimname, int_f *dimnamelen) 
{
  char *c_dimname = NULL;
  int_f ret_value = 0;
  
  /*
   * convert FORTRAN name to C name
   */
  
  if(*dimnamelen != 0)
    if(NULL == (c_dimname = (char *)HD5f2cstring(dimname, (size_t)*dimnamelen)))
      HGOTO_DONE(FAIL)

   /*
    * call H5DSset_scale function.
    */

   if(H5DSset_scale( (hid_t)*dsid, c_dimname) < 0)
     HGOTO_DONE(FAIL)

   done:
     if(c_dimname)
       HDfree(c_dimname);

  return ret_value;

} /* end nh5dsset_scale_c() */


/*-------------------------------------------------------------------------
* Function: H5DSattach_scale_c
*
* Purpose: Calls H5DSattach_scale
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 17, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsattach_scale_c( hid_t_f *did, hid_t_f *dsid, int_f *idx) 
{
  int_f ret_value = 0;
  
  /*
   * call H5DSset_scale function.
   */
  
  if(H5DSattach_scale( (hid_t)*did, (hid_t)*dsid, (unsigned)*idx ) < 0)
    HGOTO_DONE(FAIL)
      
  done:
    return ret_value;

} /* end nh5dsattach_scale_c() */


/*-------------------------------------------------------------------------
* Function: H5DSdetach_scale_c
*
* Purpose: Calls H5DSdetach_scale
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 17, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsdetach_scale_c( hid_t_f *did, hid_t_f *dsid, int_f *idx) 
{
  int_f ret_value = 0;
  
  /*
   * call H5DSset_scale function.
   */
  
  if(H5DSdetach_scale( (hid_t)*did, (hid_t)*dsid, (unsigned)*idx ) < 0)
    HGOTO_DONE(FAIL)
      
  done:
    return ret_value;

} /* end nh5dsdetach_scale_c() */


/*-------------------------------------------------------------------------
* Function: H5DSis_attached_c
*
* Purpose: Calls H5DSis_attached
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 17, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsis_attached_c( hid_t_f *did, hid_t_f *dsid, int_f *idx, int_f *is_attached) 
{
  int_f ret_value = 0;
  htri_t c_is_attached;
  
  /*
   * call H5DSis_attached function.
   */

  if((c_is_attached = H5DSis_attached( (hid_t)*did, (hid_t)*dsid, (unsigned)*idx )) < 0)
    HGOTO_DONE(FAIL)

  *is_attached = (int_f)c_is_attached;
      
  done:
      return ret_value;

} /* end nh5dsis_attached_c() */

/*-------------------------------------------------------------------------
* Function: H5DSis_scale_c
*
* Purpose: Calls H5DSis_scale
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 18, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsis_scale_c( hid_t_f *did, int_f *is_scale) 
{
  int_f ret_value = 0;
  htri_t c_is_scale;
  
  /*
   * call H5DSis_scale function.
   */
  
  if((c_is_scale=H5DSis_scale( (hid_t)*did )) < 0)
    HGOTO_DONE(FAIL)

  *is_scale = (int_f)c_is_scale;
      
  done:
    return ret_value;

} /* end nh5dsis_scale_c() */


/*-------------------------------------------------------------------------
* Function: h5dsset_label_c
*
* Purpose: Calls H5DSset_label
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 18, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsset_label_c(hid_t_f *did, int_f *idx, _fcd label, int_f *labellen) 
{
  char *c_label = NULL;
  int_f ret_value = 0;
  
  /*
   * convert FORTRAN name to C name
   */
  
  if(NULL == (c_label = (char *)HD5f2cstring(label, (size_t)*labellen)))
    HGOTO_DONE(FAIL)

  /*
   * call H5DSset_label function.
   */

  if(H5DSset_label( (hid_t)*did, (unsigned)*idx, c_label) < 0)
    HGOTO_DONE(FAIL)

  done:
     if(c_label)
       HDfree(c_label);

  return ret_value;

} /* end nh5dsset_label_c() */

/*-------------------------------------------------------------------------
* Function: h5dsget_label_c
*
* Purpose: Calls H5DSget_label
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 18, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsget_label_c(hid_t_f *did, int_f *idx, _fcd label, size_t_f *size) 
{
  char *c_label = NULL;
  ssize_t size_c = -1;
  int_f ret_value = 0;

  /*
   * Allocate buffer to hold label
   */
  if ((c_label = HDmalloc((size_t)*size + 1)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * call H5DSget_label function.
   */
      
  if( (size_c = (size_t_f)H5DSget_label( (hid_t)*did, (unsigned)*idx, c_label, (size_t)*size+1)) < 0)
    HGOTO_DONE(FAIL)

  /*
   * Convert C name to FORTRAN and place it in the given buffer
   */
      
  HD5packFstring(c_label, _fcdtocp(label), (size_t)*size);

done:
     *size = (size_t_f)size_c; /* Don't subtract '1'  because H5DSget_label doesn't include the 
				* trailing NULL in the length calculation, Ref. HDFFV-7596 */
     if(c_label) HDfree(c_label);
     return ret_value;

} /* end nh5dsget_label_c() */

/*-------------------------------------------------------------------------
* Function: h5dsget_scale_name_c
*
* Purpose: Calls H5DSget_scale_name
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 18, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsget_scale_name_c(hid_t_f *did, _fcd name, size_t_f *size) 
{
  char *c_scale_name = NULL;
  ssize_t size_c = -1;
  int_f ret_value = 0;

  /*
   * Allocate buffer to hold name
   */
  if ((c_scale_name = HDmalloc((size_t)*size + 1)) == NULL)
    HGOTO_DONE(FAIL);

  /*
   * call H5DSget_scale_name function.
   */
      
  if( (size_c = (size_t_f)H5DSget_scale_name( (hid_t)*did, c_scale_name, (size_t)*size+1)) < 0)
    HGOTO_DONE(FAIL)

  /*
   * Convert C name to FORTRAN and place it in the given buffer
   */
  HD5packFstring(c_scale_name, _fcdtocp(name), (size_t)*size);
  *size = (size_t_f)size_c;

done:
  if(c_scale_name) HDfree(c_scale_name);
  return ret_value;

} /* end nh5dsget_scale_name_c() */

/*-------------------------------------------------------------------------
* Function: H5DSget_num_scales_c
*
* Purpose: Calls H5DSget_num_scales
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: April 18, 2011
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5dsget_num_scales_c( hid_t_f *did, int_f *idx, int_f *num_scales) 
{
  int_f ret_value = 0;
  
  /*
   * call H5DSset_scale function.
   */
  
  if( (*num_scales = (int_f)H5DSget_num_scales( (hid_t)*did, (unsigned)*idx)) < 0)
    HGOTO_DONE(FAIL)
      
  done:
    return ret_value;

} /* end nh5dsget_num_scales_c() */
