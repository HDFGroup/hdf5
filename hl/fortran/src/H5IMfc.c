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

/* This files contains C stubs for H5D Fortran APIs */

#include "H5IM.h"
#include "H5IMcc.h"
#include "H5LTf90proto.h"
#include "../../../fortran/src/H5f90i_gen.h"


/*-------------------------------------------------------------------------
 * Function: h5immake_image_8bit_c
 *
 * Purpose: Call H5IMmake_image_8bit
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5immake_image_8bit_c (hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       hsize_t_f *width,
                       hsize_t_f *height,
                       int_f *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

/*
 * Call H5IMmake_image_8bitf function.
 */
 c_loc_id = (hid_t)*loc_id;
 ret = H5IMmake_image_8bitf(c_loc_id,c_name,*width,*height,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imread_image_c
 *
 * Purpose: Call H5IMread_image
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5imread_image_c (hid_t_f *loc_id,
                   int_f *namelen,
                   _fcd name,
                   int_f *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;


/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

/*
 * Call H5IMread_image function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMread_imagef(c_loc_id,c_name,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5immake_image_24bit_c
 *
 * Purpose: Call H5IMmake_image_24bit
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5immake_image_24bit_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                          int_f *ilen,
                         _fcd il,
                         hsize_t_f *width,
                         hsize_t_f *height,
                         void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 char    *c_il;
 int     c_ilen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_ilen = *ilen;
 c_il = (char *)HD5f2cstring(il, c_ilen);
 if (c_il == NULL) return ret_value;

/*
 * Call H5IMmake_image_24bitf function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMmake_image_24bitf(c_loc_id,c_name,*width,*height,c_il,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imget_image_info_c
 *
 * Purpose: Call H5IMget_image_info
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5imget_image_info_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        hsize_t_f *width,
                        hsize_t_f *height,
                        hsize_t_f *planes,
                        hsize_t_f *npals,
                        int_f *ilen,
                        _fcd interlace)
{
 int          ret_value = -1;
 herr_t       ret;
 hid_t        c_loc_id;
 char         *c_name;
 int          c_namelen;
 hsize_t      c_width;
 hsize_t      c_height;
 hsize_t      c_planes;
 hssize_t     c_npals;
 char         *c_buf=NULL;           /* Buffer to hold C string */

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

/*
 * Allocate buffer to hold name of an attribute
 */
 if ((c_buf = malloc((size_t)*ilen +1)) == NULL)
  return ret_value;

/*
 * Call H5IMget_image_info function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMget_image_info(c_loc_id,c_name,&c_width,&c_height,&c_planes,c_buf,&c_npals);

 *width  = (hsize_t_f) c_width;
 *height = (hsize_t_f) c_height;
 *planes = (hsize_t_f) c_planes;
 *npals  = (hsize_t_f) c_npals;


/*
 * Convert C name to FORTRAN and place it in the given buffer
 */
 HD5packFstring(c_buf, _fcdtocp(interlace), (size_t)*ilen);

 if(c_buf) free(c_buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5imis_image_c
 *
 * Purpose: Call H5IMis_image
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5imis_image_c(hid_t_f *loc_id,
                int_f *namelen,
                _fcd name)
{
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return -1;

/*
 * Call H5LTget_dataset_ndims function.
 */
 c_loc_id = (hid_t)*loc_id;

 return( H5IMis_image(c_loc_id, c_name));

}


/*-------------------------------------------------------------------------
 * Function: h5immake_palette_c
 *
 * Purpose: Call H5IMmake_palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5immake_palette_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     hsize_t_f *dims,
                     void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 hsize_t *c_dims;
 int     i;
 int     rank=2;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_dims =  malloc(sizeof(hsize_t) * (rank ));
 if (!c_dims) return ret_value;

 for (i = 0; i < rank ; i++) {
  c_dims[i] =  dims[i];
 }

/*
 * Call H5IMmake_palette function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMmake_palettef(c_loc_id,c_name,c_dims,buf);

 free (c_dims);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5imlink_palette_c
 *
 * Purpose: Call H5IMlink_palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5imlink_palette_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     int_f *ilen,
                     _fcd pal_name)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 char    *c_namepal;
 int     c_namelenpal;


/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_namelenpal = *ilen;
 c_namepal = (char *)HD5f2cstring(pal_name, c_namelenpal);
 if (c_namepal == NULL) return ret_value;

/*
 * Call H5IMlink_palette function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMlink_palette(c_loc_id,c_name,c_namepal);


 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5imunlink_palette_c
 *
 * Purpose: Call H5IMunlink_palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5imunlink_palette_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     int_f *ilen,
                     _fcd pal_name)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 char    *c_namepal;
 int     c_namelenpal;


/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_namelenpal = *ilen;
 c_namepal = (char *)HD5f2cstring(pal_name, c_namelenpal);
 if (c_namepal == NULL) return ret_value;

/*
 * Call H5IMunlink_palette function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMunlink_palette(c_loc_id,c_name,c_namepal);


 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}



/*-------------------------------------------------------------------------
 * Function: h5imget_npalettes_c
 *
 * Purpose: Call H5IMget_npalettes
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 06 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5imget_npalettes_c(hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     hsize_t_f *npals)
{
 int          ret_value = -1;
 herr_t       ret;
 hid_t        c_loc_id;
 char         *c_name;
 int          c_namelen;
 hssize_t     c_npals;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 /*
 * Call H5IMget_image_info function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMget_npalettes(c_loc_id,c_name,&c_npals);

 *npals  = (hsize_t_f) c_npals;

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}



/*-------------------------------------------------------------------------
 * Function: h5imget_palette_info_c
 *
 * Purpose: Call H5IMget_palette_info
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 06 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */


int_f
nh5imget_palette_info_c(hid_t_f *loc_id,
                        int_f *namelen,
                       _fcd name,
                       int_f *pal_number,
                       hsize_t_f *dims)
{
 int          ret_value = -1;
 herr_t       ret;
 hid_t        c_loc_id;
 char         *c_name;
 int          c_namelen;
 hsize_t      c_dims[2];
 int          i;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 /*
 * Call H5IMget_image_info function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMget_palette_info(c_loc_id,c_name,*pal_number,c_dims);

 for (i = 0; i < 2 ; i++) {
  dims[i] = (hsize_t_f) c_dims[i];
 }


 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5imget_palette_c
 *
 * Purpose: Call H5IMget_palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 06 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */


int_f
nh5imget_palette_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *pal_number,
                        void *buf)
{
 int          ret_value = -1;
 herr_t       ret;
 hid_t        c_loc_id;
 char         *c_name;
 int          c_namelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 /*
 * Call H5IMget_image_info function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5IMget_palettef(c_loc_id,c_name,*pal_number,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5imis_palette_c
 *
 * Purpose: Call H5IMis_palette
 *
 * Return: true, false, fail
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5imis_palette_c(hid_t_f *loc_id,
                  int_f *namelen,
                 _fcd name)
{
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return -1;

/*
 * Call H5IMis_palette function.
 */
 c_loc_id = (hid_t)*loc_id;

 return( H5IMis_palette(c_loc_id, c_name));

}
