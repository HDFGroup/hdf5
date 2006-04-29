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

#include "H5TBprivate.h"
#include "H5LTf90proto.h"

/*-------------------------------------------------------------------------
 * Function: h5tbmake_table_c
 *
 * Purpose: Call H5TBmake_table
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
nh5tbmake_table_c(int_f *namelen1,
                  _fcd name1,
                  hid_t_f *loc_id,
                  int_f *namelen,
                  _fcd name,
                  hsize_t_f *nfields,
                  hsize_t_f *nrecords,
                  size_t_f *type_size,
                  size_t_f *field_offset,
                  hid_t_f *field_types,
                  hsize_t_f *chunk_size,
                  int_f *compress,
                  int_f *namelen2,       /* field_names lenghts */
                  _fcd field_names)      /* field_names */
{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 char    *c_name1;
 int     c_namelen1;
 hsize_t num_elem;
 hsize_t i;
 int     max_len=1;
 hid_t   c_loc_id     = *loc_id;
 hsize_t c_nfields    = *nfields;
 hsize_t c_nrecords   = *nrecords;
 hsize_t c_chunk_size = *chunk_size;
 size_t  c_type_size  = *type_size;
 size_t  *c_field_offset;
 hid_t   *c_field_types;
 char    **c_field_names;
 char    *tmp, *tmp_p;

 num_elem = *nfields;

 for (i=0; i < num_elem; i++) {
  if (namelen2[i] > max_len) max_len = namelen2[i];
 }

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_namelen1 = *namelen1;
 c_name1 = (char *)HD5f2cstring(name1, c_namelen1);
 if (c_name1 == NULL) return ret_value;

 c_field_offset =  (size_t*)malloc(sizeof(size_t) * (size_t)c_nfields);
 if (!c_field_offset) return ret_value;

 c_field_types =  (hid_t*)malloc(sizeof(hid_t) * (size_t)c_nfields);
 if (!c_field_types) return ret_value;

 for (i=0; i < num_elem; i++) {
  c_field_offset[i] = field_offset[i];
  c_field_types[i]  = field_types[i];
 }

/*
 * Allocate array of character pointers
 */
 c_field_names = (char **)malloc((size_t)num_elem * sizeof(char *));
 if (c_field_names == NULL) return ret_value;

 /* Copy data to long C string */
 tmp = (char *)HD5f2cstring(field_names, (int)(max_len*num_elem));
 if (tmp == NULL) {
  free(c_field_names);
  return ret_value;
 }

/*
 * Move data from temorary buffer
 */
 tmp_p = tmp;
 for (i=0; i < num_elem; i++) {
  c_field_names[i] = (char *) malloc((size_t)namelen2[i]+1);
  memcpy(c_field_names[i], tmp_p, (size_t)namelen2[i]);
  c_field_names[i][namelen2[i]] = '\0';
  tmp_p = tmp_p + max_len;
 }

/*
 * Call H5TBmake_table function.
 */

 ret = H5TBmake_table(c_name1,c_loc_id,c_name,c_nfields,c_nrecords,c_type_size,
   c_field_names,c_field_offset,c_field_types,c_chunk_size,NULL,*compress,NULL);

 for (i=0; i < num_elem; i++) {
  free (c_field_names[i]);
 }
 free(c_field_names);
 free(tmp);
 free(c_field_offset);
 free(c_field_types);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}




/*-------------------------------------------------------------------------
 * Function: h5tbwrite_field_name_c
 *
 * Purpose: Call H5TBwrite_fields_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 12, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5tbwrite_field_name_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
																		      int_f *namelen1,
                        _fcd field_name,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 char    *c_name1;
 int     c_namelen1;
 hid_t   c_loc_id     = *loc_id;
 hsize_t c_start      = *start;
 hsize_t c_nrecords   = *nrecords;
 size_t  c_type_size  = *type_size;
	size_t  c_type_sizes[1];

	c_type_sizes[0] = c_type_size;


/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_namelen1 = *namelen1;
 c_name1 = (char *)HD5f2cstring(field_name, c_namelen1);
 if (c_name1 == NULL) return ret_value;

/*
 * Call H5TBwrite_fields_name function.
 */

 ret = H5TBwrite_fields_name(c_loc_id,c_name,c_name1,c_start,c_nrecords,c_type_size,
   0,c_type_sizes,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5tbread_field_name_c
 *
 * Purpose: Call H5TBread_fields_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 12, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5tbread_field_name_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
																		      int_f *namelen1,
                        _fcd field_name,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 char    *c_name1;
 int     c_namelen1;
 hid_t   c_loc_id     = *loc_id;
 hsize_t c_start      = *start;
 hsize_t c_nrecords   = *nrecords;
 size_t  c_type_size  = *type_size;
	size_t  c_type_sizes[1];

	c_type_sizes[0] = c_type_size;


/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_namelen1 = *namelen1;
 c_name1 = (char *)HD5f2cstring(field_name, c_namelen1);
 if (c_name1 == NULL) return ret_value;

/*
 * Call H5TBread_fields_name function.
 */

 ret = H5TBread_fields_name(c_loc_id,c_name,c_name1,c_start,c_nrecords,c_type_size,
   0,c_type_sizes,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}



/*-------------------------------------------------------------------------
 * Function: h5tbwrite_field_index_c
 *
 * Purpose: Call H5TBwrite_fields_index
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 12, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5tbwrite_field_index_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *field_index,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 hid_t   c_loc_id     = *loc_id;
 hsize_t c_start      = *start;
 hsize_t c_nrecords   = *nrecords;
 size_t  c_type_size  = *type_size;
	size_t  c_type_sizes[1];
	int     c_field_index[1];

	c_type_sizes[0] = c_type_size;
	c_field_index[0] = *field_index - 1; /* C zero based index */


/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;


/*
 * Call H5TBwrite_fields_name function.
 */

 ret = H5TBwrite_fields_index(c_loc_id,c_name,(hsize_t)1,c_field_index,c_start,c_nrecords,c_type_size,
   0,c_type_sizes,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5tbread_field_index_c
 *
 * Purpose: Call H5TBread_fields_index
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 12, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5tbread_field_index_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *field_index,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 hid_t   c_loc_id     = *loc_id;
 hsize_t c_start      = *start;
 hsize_t c_nrecords   = *nrecords;
 size_t  c_type_size  = *type_size;
	size_t  c_type_sizes[1];
	int     c_field_index[1];

	c_type_sizes[0] = c_type_size;
	c_field_index[0] = *field_index - 1; /* C zero based index */


/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

/*
 * Call H5TBread_fields_index function.
 */

 ret = H5TBread_fields_index(c_loc_id,c_name,(hsize_t)1,c_field_index,c_start,c_nrecords,c_type_size,
   0,c_type_sizes,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5tbinsert_field_c
 *
 * Purpose: Call H5TBinsert_field
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 13, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5tbinsert_field_c(hid_t_f *loc_id,
                    int_f *namelen,
                    _fcd name,
																				int_f *namelen1,
                    _fcd field_name,
                    hid_t_f *field_type,
                    int_f *position,
                    void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 char    *c_name1;
 int     c_namelen1;
 hid_t   c_loc_id     = *loc_id;
 hid_t   c_field_type = *field_type;
	hsize_t c_position   = *position;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_namelen1 = *namelen1;
 c_name1 = (char *)HD5f2cstring(field_name, c_namelen1);
 if (c_name1 == NULL) return ret_value;

/*
 * Call H5TBinsert_field function.
 */

 ret = H5TBinsert_field(c_loc_id,c_name,c_name1,c_field_type,c_position,NULL,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: h5tbdelete_field_c
 *
 * Purpose: Call H5TBdelete_field
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 13, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5tbdelete_field_c(hid_t_f *loc_id,
                    int_f *namelen,
                    _fcd name,
																				int_f *namelen1,
																				_fcd field_name)
{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 char    *c_name1;
 int     c_namelen1;
 hid_t   c_loc_id     = *loc_id;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

 c_namelen1 = *namelen1;
 c_name1 = (char *)HD5f2cstring(field_name, c_namelen1);
 if (c_name1 == NULL) return ret_value;

/*
 * Call H5TBinsert_field function.
 */

 ret = H5TBdelete_field(c_loc_id,c_name,c_name1);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}



/*-------------------------------------------------------------------------
 * Function: h5tbget_table_info_c
 *
 * Purpose: Call H5TBread_fields_index
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 12, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5tbget_table_info_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        hsize_t_f *nfields,
                        hsize_t_f *nrecords)
{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 hid_t   c_loc_id = *loc_id;
 hsize_t c_nfields;
	hsize_t c_nrecords;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;

/*
 * Call H5TBread_fields_index function.
 */

 ret = H5TBget_table_info(c_loc_id,c_name,&c_nfields,&c_nrecords);

	*nfields = (hsize_t_f) c_nfields;;
	*nrecords = (hsize_t_f) c_nrecords;

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}



/*-------------------------------------------------------------------------
 * Function: h5tbget_field_info_c
 *
 * Purpose: Call H5TBget_field_info
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 13, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */
int_f
nh5tbget_field_info_c(hid_t_f *loc_id,
                  int_f *namelen,
                  _fcd name,
                  hsize_t_f *nfields,
                  size_t_f *field_sizes,
                  size_t_f *field_offsets,
                  size_t_f *type_size,
																		int_f *namelen2,       /* field_names lenghts */
                  _fcd field_names)      /* field_names */

{
 int     ret_value = -1;
 herr_t  ret;
 char    *c_name;
 int     c_namelen;
 hsize_t num_elem;
 hsize_t i;
 int     max_len=1;
 hid_t   c_loc_id   = *loc_id;
	hsize_t c_nfields  = *nfields;
 size_t  *c_field_sizes;
	size_t  *c_field_offsets;
	size_t  c_type_size;
	char    **c_field_names;
 char    *tmp, *tmp_p;
	int     c_lenmax=HLTB_MAX_FIELD_LEN;
	size_t length = 0;

 num_elem = c_nfields;

 for (i=0; i < num_elem; i++) {
  if (namelen2[i] > max_len) max_len = namelen2[i];
 }

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen);
 if (c_name == NULL) return ret_value;


 c_field_offsets =  (size_t*)malloc(sizeof(size_t) * (size_t)c_nfields);
 if (!c_field_offsets) return ret_value;

	c_field_sizes =  (size_t*)malloc(sizeof(size_t) * (size_t)c_nfields);
 if (!c_field_sizes) return ret_value;

	c_field_names = malloc( sizeof(char*) * (size_t)c_nfields );
 if (!c_field_names) return ret_value;
	for ( i = 0; i < c_nfields; i++)
 {
  c_field_names[i] = malloc( sizeof(char) * HLTB_MAX_FIELD_LEN );
 }

/*
 * Call H5TBget_field_info function.
 */

 ret = H5TBget_field_info(c_loc_id,c_name,c_field_names,c_field_sizes,c_field_offsets,
		&c_type_size);

	/* return values*/

	/* names array */
	tmp = (char *)malloc(c_lenmax* (size_t) c_nfields + 1);
	tmp_p = tmp;
	memset(tmp,' ', c_lenmax* (size_t) c_nfields);
	tmp[c_lenmax*c_nfields] = '\0';
	for (i=0; i < c_nfields; i++) {
		memcpy(tmp_p, c_field_names[i], strlen(c_field_names[i]));
		namelen2[i] = (int_f)strlen(c_field_names[i]);
		length = MAX(length, strlen(c_field_names[i]));
		tmp_p = tmp_p + c_lenmax;
 }
 HD5packFstring(tmp, _fcdtocp(field_names), (int)(c_lenmax*c_nfields));


	*type_size = (size_t_f)c_type_size;
	for (i=0; i < num_elem; i++)
	{
		field_sizes[i]   = (size_t_f)c_field_sizes[i];
		field_offsets[i] = (size_t_f)c_field_offsets[i];
 }



	/* free */

 for (i=0; i < num_elem; i++) {
  free (c_field_names[i]);
 }
 free(c_field_names);
 free(tmp);
 free(c_field_offsets);
 free(c_field_sizes);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}

