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

#include <math.h>
#include "h5diff.h"
#include "ph5diff.h"
#include "H5private.h" 

/* local functions */
static void    close_obj(H5G_obj_t obj_type, hid_t obj_id);
static int     diff_region(hid_t region1_id, hid_t region2_id);
static hbool_t is_zero(const void *_mem, size_t size);

/*-------------------------------------------------------------------------
 * Function: print_data
 *
 * Purpose: print data only in report or verbose modes,
 *  and do not print in quiet mode
 *-------------------------------------------------------------------------
 */
static int print_data(diff_opt_t *options)
{ 
 return ( (options->m_report || options->m_verbose) && !options->m_quiet)?1:0;
}


/*-------------------------------------------------------------------------
 * Function: diff_array
 *
 * Purpose: compare two memory buffers;
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 12, 2003
 *    
 *-------------------------------------------------------------------------
 */

hsize_t diff_array( void *_mem1, 
                    void *_mem2, 
                    hsize_t nelmts, 
                    int rank, 
                    hsize_t *dims, 
                    diff_opt_t *options, 
                    const char *name1, 
                    const char *name2,
                    hid_t m_type,
                    hid_t container1_id,
                    hid_t container2_id) /* dataset where the reference came from*/
{ 
    hsize_t       nfound=0;          /* number of differences found */
    size_t        size;              /* size of datum */
    unsigned char *mem1 = (unsigned char*)_mem1;
    unsigned char *mem2 = (unsigned char*)_mem2;
    unsigned char *tmp1;
    unsigned char *tmp2;
    hsize_t       acc[32];    /* accumulator position */
    hsize_t       pos[32];    /* matrix position */
    int           ph=1;       /* print header  */
    hsize_t       i;
    int           j;

    /* get the size. */
    size = H5Tget_size( m_type );

    acc[rank-1]=1;
    for(j=(rank-2); j>=0; j--)
    {
	acc[j]=acc[j+1]*(int)dims[j+1];
    }
    for ( j = 0; j < rank; j++)
	pos[j]=0;


    if(H5Tis_variable_str(m_type)) 
    {
	tmp1 = ((unsigned char**)mem1)[0]; 
	tmp2 = ((unsigned char**)mem2)[0]; 
	nfound+=diff_datum(
		tmp1,
		tmp2, 
		m_type,
		(hsize_t)0,
		rank,
		acc,
		pos,
		options,
		name1,
		name2,
		container1_id,
		container2_id,
		&ph);
    }

    else

    {

	switch (H5Tget_class(m_type)) 
	{
	    default:
		assert(0);
		break;

	    case H5T_FLOAT:
		if (H5Tequal(m_type, H5T_NATIVE_FLOAT)) 
		    nfound=diff_float(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_DOUBLE)) 
		    nfound=diff_double(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		break;

	    case H5T_INTEGER:

		if (H5Tequal(m_type, H5T_NATIVE_SCHAR))
		    nfound=diff_schar(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_UCHAR)) 
		    nfound=diff_uchar(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_SHORT))
		    nfound=diff_short(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_USHORT))
		    nfound=diff_ushort(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_INT)) 
		    nfound=diff_int(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_UINT)) 
		    nfound=diff_uint(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_LONG)) 
		    nfound=diff_long(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_ULONG)) 
		    nfound=diff_ulong(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_LLONG)) 
		    nfound=diff_llong(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);
		else if (H5Tequal(m_type, H5T_NATIVE_ULLONG)) 
		    nfound=diff_ullong(mem1,mem2,nelmts,rank,acc,pos,options,name1,name2,&ph);

		break;

	    case H5T_COMPOUND:
	    case H5T_STRING:
	    case H5T_BITFIELD:
	    case H5T_OPAQUE:
	    case H5T_ENUM:
	    case H5T_ARRAY:
	    case H5T_VLEN:
	    case H5T_REFERENCE:

		for ( i = 0; i < nelmts; i++)
		{
		    nfound+=diff_datum(
			    mem1 + i * size,
			    mem2 + i * size, /* offset */
			    m_type,
			    i,
			    rank,
			    acc,
			    pos,
			    options,
			    name1,
			    name2,
			    container1_id,
			    container2_id,
			    &ph);
		    if (options->n && nfound>=options->count)
			return nfound;
		} /* i */
	} /* switch */
    } /* else */

    return nfound;
}




/*-------------------------------------------------------------------------
 * Function: diff_datum
 *
 * Purpose: Compare the values pointed to in _MEM1 and _MEM2 of type M_TYPE
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 29, 2003
 *
 * The compare of the 2 buffers read from the files is made datum by datum.
 *
 * H5T_INTEGER and H5T_FLOAT
 *  Copy the buffer into a compatible local datum and do a numerical 
 *  compare of this datum
 * H5T_COMPOUND
 *  Recursively call this function for each member
 * H5T_ARRAY
 *  Recursively call this function for each element 
 * H5T_VLEN
 *  Recursively call this function for each element 
 * H5T_STRING
 *  compare byte by byte in a cycle from 0 to type_size. this type_size is the 
 *  value obtained by the get_size function but it is the string lenght for 
 *  variable sized strings
 * H5T_OPAQUE
 *  compare byte by byte in a cycle from 0 to type_size
 * H5T_BITFIELD
 *  compare byte by byte in a cycle from 0 to type_size
 * H5T_ENUM
 *  for each pair of elements being compared, both bit patterns are converted to 
 *  their corresponding enumeration constant and a string comparison is made
 * H5T_REFERENCE
 *  Dereference the object and compare the type (basic object type). 
 *-------------------------------------------------------------------------
 */

hsize_t diff_datum(void       *_mem1, 
	void       *_mem2, 
	hid_t      m_type,
	hsize_t    i, 
	int        rank, 
	hsize_t    *acc,  
	hsize_t    *pos, 
	diff_opt_t *options, 
	const char *obj1, 
	const char *obj2,
	hid_t      container1_id,
	hid_t      container2_id, /*where the reference came from*/
	int        *ph)           /*print header */ 
{
    static char   fmt_llong[255];
    static char   fmt_ullong[255];
    static char   fmt_llongp[255];
    static char   fmt_ullongp[255];
    hsize_t       dims[H5S_MAX_RANK];
    unsigned char *mem1 = (unsigned char*)_mem1;
    unsigned char *mem2 = (unsigned char*)_mem2;
    unsigned      u;
    hid_t         memb_type;
    size_t        type_size; 
    size_t        offset;
    int           nmembs;
    int           j;
    hsize_t       nelmts;
    hsize_t       ndims;
    size_t        size;
    int           iszero1;
    int           iszero2;
    H5G_obj_t     obj1_type;
    H5G_obj_t     obj2_type;
    hid_t         obj1_id;
    hid_t         obj2_id;
    H5G_stat_t    sb1;
    H5G_stat_t    sb2;
    hsize_t       nfound=0;   /* differences found */
    int           ret=0;      /* check return error */

    if (!fmt_llong[0]) {

	/* Build default formats for long long types */
	sprintf(fmt_llong,  "%%%sd              %%%sd               %%%sd\n", 
		H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
	sprintf(fmt_ullong, "%%%su              %%%su               %%%su\n", 
		H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
	sprintf(fmt_llongp,  "%%%sd             %%%sd               %%%sd               %%%sd\n", 
		H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
	sprintf(fmt_ullongp, "%%%su             %%%su               %%%su               %%%su\n", 
		H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);

    }

    type_size = H5Tget_size( m_type );

    switch (H5Tget_class(m_type)) 
    {
	default:
	    assert(0);
	    break;
	case H5T_TIME:
	    assert(0);
	    break;

	    /*-------------------------------------------------------------------------
	     * H5T_COMPOUND
	     *-------------------------------------------------------------------------
	     */
	case H5T_COMPOUND:
	    nmembs = H5Tget_nmembers(m_type);
	    for (j = 0; j < nmembs; j++) 
	    {
		offset    = H5Tget_member_offset(m_type, (unsigned)j);
		memb_type = H5Tget_member_type(m_type, (unsigned)j);
		nfound+=diff_datum(
			mem1+offset,
			mem2+offset,
			memb_type,
			i,
			rank,
			acc,
			pos,
			options,
			obj1,
			obj2,
			container1_id,
			container2_id,
			ph);
		H5Tclose(memb_type);
	    }
	    break;

	    /*-------------------------------------------------------------------------
	     * H5T_STRING
	     *-------------------------------------------------------------------------
	     */
	case H5T_STRING:
  {
   
   H5T_str_t pad;
   char      *s;
   
   if(H5Tis_variable_str(m_type)) 
   {
   /* mem1 is the pointer into the struct where a `char*' is stored. So we have
    * to dereference the pointer to get the `char*' to pass to HDstrlen(). */
    s = *(char**)mem1;
    if(s!=NULL)
     size = HDstrlen(s);
   }
   else 
   {
    s = mem1;
    size = H5Tget_size(m_type);
   }
   
   pad = H5Tget_strpad(m_type);
   
   /* check for NULL pointer for string */
   if(s!=NULL)
    for (u=0; u<size && (s[u] || pad!=H5T_STR_NULLTERM); u++)
     nfound+=diff_char(
     mem1 + u,
     mem2 + u, /* offset */
     i,        /* index position */
     rank, 
     acc,
     pos,
     options, 
     obj1, 
     obj2,
     ph);
    
  }
  break;

	    /*-------------------------------------------------------------------------
	     * H5T_BITFIELD
	     *-------------------------------------------------------------------------
	     */
	case H5T_BITFIELD:
	    {
		/* byte-by-byte comparison */
		for (u=0; u<type_size; u++)
		    nfound+=diff_native_uchar(
			    mem1 + u,
			    mem2 + u, /* offset */
			    i,        /* index position */
			    rank, 
			    acc,
			    pos,
			    options, 
			    obj1, 
			    obj2,
			    ph);

	    }
	    break;
	    /*-------------------------------------------------------------------------
	     * H5T_OPAQUE
	     *-------------------------------------------------------------------------
	     */
	case H5T_OPAQUE:

	    /* byte-by-byte comparison */
	    for (u=0; u<type_size; u++)
		nfound+=diff_native_uchar(
			mem1 + u,
			mem2 + u, /* offset */
			i,        /* index position */
			rank, 
			acc,
			pos,
			options, 
			obj1, 
			obj2,
			ph);

	    break;


	    /*-------------------------------------------------------------------------
	     * H5T_ENUM
	     *-------------------------------------------------------------------------
	     */
	case H5T_ENUM:

	    /* For enumeration types we compare the names instead of the
	       integer values.  For each pair of elements being
	       compared, we convert both bit patterns to their corresponding 
	       enumeration constant and do a string comparison */

	    {
		char enum_name1[1024];
		char enum_name2[1024];

		/* disable error reporting */
		H5E_BEGIN_TRY {

		    if ((H5Tenum_nameof(m_type, mem1, enum_name1, sizeof enum_name1) >= 0) &&
			    (H5Tenum_nameof(m_type, mem2, enum_name2, sizeof enum_name2) >= 0))
		    {
			if (HDstrcmp(enum_name1,enum_name2)!=0)
			{
			    nfound=1;
			    if ( print_data(options) ) 
			    {
				print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
				parallel_print(SPACES);
				parallel_print(SFORMAT,enum_name1,enum_name2);
			    }
			}
			else
			{
			    for (u=0; u<type_size; u++)
				nfound+=diff_native_uchar(
					mem1 + u,
					mem2 + u, /* offset */
					i,        /* index position */
					rank, 
					acc,
					pos,
					options, 
					obj1, 
					obj2,
					ph);
			}
		    }
		    /* enable error reporting */
		} H5E_END_TRY;
	    }


	    break;
	    /*-------------------------------------------------------------------------
	     * H5T_ARRAY
	     *-------------------------------------------------------------------------
	     */
	case H5T_ARRAY:
	    /* get the array's base datatype for each element */
	    memb_type = H5Tget_super(m_type);
	    size      = H5Tget_size(memb_type);
	    ndims     = H5Tget_array_ndims(m_type);
	    H5Tget_array_dims(m_type, dims, NULL);
	    assert(ndims >= 1 && ndims <= H5S_MAX_RANK);

	    /* calculate the number of array elements */
	    for (u = 0, nelmts = 1; u <ndims; u++)
		nelmts *= dims[u];
	    for (u = 0; u < nelmts; u++) 
		nfound+=diff_datum(
			mem1 + u * size,
			mem2 + u * size, /* offset */
			memb_type,
			i,               /* index position */
			rank,
			acc,
			pos,
			options,
			obj1,
			obj2,
			container1_id,
			container2_id,
			ph);
	    H5Tclose(memb_type);
	    break;

	    /*-------------------------------------------------------------------------
	     * H5T_VLEN
	     *-------------------------------------------------------------------------
	     */
	case H5T_VLEN:

	    /* get the VL sequences's base datatype for each element */
	    memb_type = H5Tget_super(m_type);
	    size      = H5Tget_size(memb_type);

	    /* get the number of sequence elements */
	    nelmts = ((hvl_t *)mem1)->len;

	    for (j = 0; j < (int)nelmts; j++) 
		nfound+=diff_datum(
			((char *)(((hvl_t *)mem1)->p)) + j * size,
			((char *)(((hvl_t *)mem2)->p)) + j * size, /* offset */
			memb_type,
			i,         /* index position */
			rank,
			acc,
			pos,
			options,
			obj1,
			obj2,
			container1_id,
			container2_id,
			ph);

	    H5Tclose(memb_type);

	    break;


	case H5T_REFERENCE:

	    iszero1=is_zero(_mem1, H5Tget_size(m_type));
	    iszero2=is_zero(_mem2, H5Tget_size(m_type));
	    if (iszero1==1 && iszero2==1)
		return 0;
	    else if (iszero1!=iszero2)
		return 1;
	    else
	    {

		/*-------------------------------------------------------------------------
		 * H5T_STD_REF_DSETREG
		 * Dataset region reference
		 *-------------------------------------------------------------------------
		 */

		if (H5Tequal(m_type, H5T_STD_REF_DSETREG)) 
		{
		    hid_t  region1_id;
		    hid_t  region2_id;

		    if ((obj1_id = H5Rdereference(container1_id, H5R_DATASET_REGION, _mem1))<0)
			ret= -1;
		    if ((obj2_id = H5Rdereference(container2_id, H5R_DATASET_REGION, _mem2))<0)
			ret= -1;
		    if (H5Gget_objinfo(obj1_id, ".", FALSE, &sb1)<0)
			ret= -1;
		    if (H5Gget_objinfo(obj2_id, ".", FALSE, &sb2)<0)
			ret= -1;
		    if ((region1_id = H5Rget_region(container1_id, H5R_DATASET_REGION, _mem1))<0)
			ret= -1;
		    if ((region2_id = H5Rget_region(container2_id, H5R_DATASET_REGION, _mem2))<0)
			ret= -1;

		    if (ret==-1) {
			options->err_stat=1;
			return 0;
		    }

		    if (diff_region(region1_id,region2_id))
		    {
			parallel_print("Different region referenced\n");
		    }

		    close_obj(H5G_DATASET,obj1_id);
		    close_obj(H5G_DATASET,obj2_id);
		    H5Sclose(region1_id);
		    H5Sclose(region2_id);

		}/*dataset reference*/


		/*-------------------------------------------------------------------------
		 * H5T_STD_REF_OBJ
		 * Object references. get the type and OID of the referenced object
		 *-------------------------------------------------------------------------
		 */
		else if (H5Tequal(m_type, H5T_STD_REF_OBJ)) 
		{

		    if ((obj1_type = H5Rget_obj_type(container1_id, H5R_OBJECT, _mem1))<0)
			ret= -1;
		    if ((obj2_type = H5Rget_obj_type(container2_id, H5R_OBJECT, _mem2))<0)
			ret= -1;
		    if (ret==-1) {
			options->err_stat=1;
			return 0;
		    }

		    /* check object type */
		    if (obj1_type!=obj2_type)
		    {
			parallel_print("Different object types referenced: <%s> and <%s>", obj1, obj2);
			return 1;
		    }

		    if ((obj1_id = H5Rdereference(container1_id, H5R_OBJECT, _mem1))<0)
			ret= -1;
		    if ((obj2_id = H5Rdereference(container2_id, H5R_OBJECT, _mem2))<0)
			ret= -1;
		    if (ret==-1) {
			options->err_stat=1;
			return 0;
		    }


		    /*deep compare */
		    switch (obj1_type) {
			case H5G_DATASET:
			    nfound=diff_datasetid(obj1_id,
				    obj2_id,
				    NULL,
				    NULL,
				    options);
			    break;
			default:
			    parallel_print("Warning: Comparison not possible of object types referenced: <%s> and <%s>", 
				    obj1, obj2);
       options->not_cmp=1;
			    break;
		    }

		    close_obj(obj1_type,obj1_id);
		    close_obj(obj2_type,obj2_id);

		}/*object reference*/

	    }/*is zero*/


	    break;


	case H5T_INTEGER:


	    /*-------------------------------------------------------------------------
	     * H5T_NATIVE_SCHAR
	     *-------------------------------------------------------------------------
	     */
	    if (H5Tequal(m_type, H5T_NATIVE_SCHAR)) 
	    {
		char        temp1_char;
		char        temp2_char;
		assert(type_size==sizeof(char));
		memcpy(&temp1_char, mem1, sizeof(char));
		memcpy(&temp2_char, mem2, sizeof(char));
		/* -d and !-p */
		if (options->d && !options->p)
		{
		    if (abs(temp1_char-temp2_char) > options->delta)
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
			}
			nfound++;
		    }
		}
		/* !-d and -p */
		else if (!options->d && options->p)
		{
		    if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options->percent )
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IPFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char), abs(1-temp2_char/temp1_char));
			}
			nfound++;
		    }
		}
		/* -d and -p */
		else if ( options->d && options->p)
		{
		    if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options->percent && 
			    abs(temp1_char-temp2_char) > options->delta )
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IPFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char), abs(1-temp2_char/temp1_char));
			}
			nfound++;
		    }
		}
		else if (temp1_char != temp2_char)
		{
		    if ( print_data(options) ) 
		    {
			print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			parallel_print(SPACES);
			parallel_print(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
		    }
		    nfound++;
		}

	    } /*H5T_NATIVE_SCHAR*/

	    /*-------------------------------------------------------------------------
	     * H5T_NATIVE_UCHAR
	     *-------------------------------------------------------------------------
	     */
	    else if (H5Tequal(m_type, H5T_NATIVE_UCHAR)) 
	    {
		unsigned char      temp1_uchar;
		unsigned char      temp2_uchar;
		assert(type_size==sizeof(unsigned char));

		memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
		memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
		/* -d and !-p */
		if (options->d && !options->p)
		{
		    if (abs(temp1_uchar-temp2_uchar) > options->delta)
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
			}
			nfound++;
		    }
		}
		/* !-d and -p */
		else if (!options->d && options->p)
		{
		    if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent )
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IPFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar), abs(1-temp2_uchar/temp1_uchar));
			}
			nfound++;
		    }
		}
		/* -d and -p */
		else if ( options->d && options->p)
		{
		    if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent && 
			    abs(temp1_uchar-temp2_uchar) > options->delta )
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IPFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar), abs(1-temp2_uchar/temp1_uchar));
			}
			nfound++;
		    }
		}
		else if (temp1_uchar != temp2_uchar)
		{
		    if ( print_data(options) ) 
		    {
			print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			parallel_print(SPACES);
			parallel_print(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));

		    }
		    nfound++;
		}

	    } /*H5T_NATIVE_UCHAR*/


	    /*-------------------------------------------------------------------------
	     * H5T_NATIVE_SHORT
	     *-------------------------------------------------------------------------
	     */

	    else if (H5Tequal(m_type, H5T_NATIVE_SHORT))
	    {
		short       temp1_short;
		short       temp2_short;
		assert(type_size==sizeof(short));

		memcpy(&temp1_short, mem1, sizeof(short));
		memcpy(&temp2_short, mem2, sizeof(short));
		/* -d and !-p */
		if (options->d && !options->p)
		{
		    if (abs(temp1_short-temp2_short) > options->delta)
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
			}
			nfound++;
		    }
		}
		/* !-d and -p */
		else if (!options->d && options->p)
		{
		    if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options->percent )
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IPFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short),  abs(1-temp2_short/temp1_short));
			}
			nfound++;
		    }
		}
		/* -d and -p */
		else if ( options->d && options->p)
		{
		    if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options->percent && 
			    abs(temp1_short-temp2_short) > options->delta )
		    {
			if ( print_data(options) ) 
			{
			    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IPFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short),  abs(1-temp2_short/temp1_short));

			}
			nfound++;
		    }
		}
		else if (temp1_short != temp2_short)
		{
		    if ( print_data(options) ) 
		    {
			print_pos(ph,0,i,acc,pos,rank,obj1,obj2);

			parallel_print(SPACES);
			parallel_print(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
		    }
		    nfound++;
		}


	    } /*H5T_NATIVE_SHORT*/

	    /*-------------------------------------------------------------------------
	     * H5T_NATIVE_USHORT
	     *-------------------------------------------------------------------------
	     */

	    else if (H5Tequal(m_type, H5T_NATIVE_USHORT))
	    {
		unsigned short       temp1_ushort;
		unsigned short       temp2_ushort;
		assert(type_size==sizeof(short));

		memcpy(&temp1_ushort, mem1, sizeof(unsigned short));
		memcpy(&temp2_ushort, mem2, sizeof(unsigned short));
		/* -d and !-p */
		if (options->d && !options->p)
		{
		    if (abs(temp1_ushort-temp2_ushort) > options->delta)
		    {

			if ( print_data(options) ) 
			{
			    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
			    parallel_print(IFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort));
			}
			nfound++;
		    }
		}
		/* !-d and -p */
		else if (!options->d && options->p)
		{
		    if ( temp1_ushort!=0 && abs(1-temp2_ushort/temp1_ushort) > options->percent )
		    {

			if ( print_data(options) ) 
			{
			    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);

			    parallel_print(SPACES);
			    parallel_print(IPFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort),  abs(1-temp2_ushort/temp1_ushort));
			}
			nfound++;
		    }
		}
		/* -d and -p */
		else if ( options->d && options->p)
		{
		    if ( temp1_ushort!=0 && abs(1-temp2_ushort/temp1_ushort) > options->percent && 
			    abs(temp1_ushort-temp2_ushort) > options->delta )
		    {

			if ( print_data(options) ) 
			{
			    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);

			    parallel_print(SPACES);
			    parallel_print(IPFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort),  abs(1-temp2_ushort/temp1_ushort));
			}
			nfound++;
		    }
		}
		else if (temp1_ushort != temp2_ushort)
		{

		    if ( print_data(options) ) 
		    {
			print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			parallel_print(SPACES);
			parallel_print(IFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort));
		    }
		    nfound++;
		}


	    } /*H5T_NATIVE_USHORT*/


	    /*-------------------------------------------------------------------------
	     * H5T_NATIVE_INT
	     *-------------------------------------------------------------------------
	     */

	    else if (H5Tequal(m_type, H5T_NATIVE_INT)) 
	    {
		int         temp1_int;
		int         temp2_int;
		assert(type_size==sizeof(int));

		memcpy(&temp1_int, mem1, sizeof(int));
		memcpy(&temp2_int, mem2, sizeof(int));
		/* -d and !-p */
		if (options->d && !options->p)
		{
		    if (abs(temp1_int-temp2_int) > options->delta)
		    {

			if ( print_data(options) ) 
			{
			    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			    parallel_print(SPACES);
	   parallel_print(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options->percent )
     {
     
      if ( print_data(options) ) 
      {
       print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
       
	   parallel_print(SPACES);
	   parallel_print(IPFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int),  abs(1-temp2_int/temp1_int));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options->d && options->p)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options->percent && 
      abs(temp1_int-temp2_int) > options->delta )
     {
     
      if ( print_data(options) ) 
      {
       print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
       
	   parallel_print(SPACES);
	   parallel_print(IPFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int),  abs(1-temp2_int/temp1_int));
      }
      nfound++;
     }
    }
    else if (temp1_int != temp2_int)
    {
   
     if ( print_data(options) ) 
     {
      print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	   parallel_print(SPACES);
	   parallel_print(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
     }
     nfound++;
    }
    
     
  } /*H5T_NATIVE_INT*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_UINT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_UINT)) 
  {
   unsigned int         temp1_uint;
   unsigned int         temp2_uint;
   assert(type_size==sizeof(int));
  
    memcpy(&temp1_uint, mem1, sizeof(unsigned int));
    memcpy(&temp2_uint, mem2, sizeof(unsigned int));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (abs((int)(temp1_uint-temp2_uint)) > options->delta)
     {
     
      if ( print_data(options) ) 
      {
	  print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	      parallel_print(SPACES);
	      parallel_print(UIFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_uint!=0 && abs((int)(1-temp2_uint/temp1_uint)) > options->percent )
     {
     
      if ( print_data(options) ) 
      {
       print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
      
	   parallel_print(SPACES);
	   parallel_print(IPFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)),  abs((int)(1-temp2_uint/temp1_uint)));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options->d && options->p)
    {
     if ( temp1_uint!=0 && abs((int)(1-temp2_uint/temp1_uint)) > options->percent && 
      abs((int)(temp1_uint-temp2_uint)) > options->delta )
     {
     
      if ( print_data(options) ) 
      {
       print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
     
	   parallel_print(SPACES);
	   parallel_print(IPFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)),  abs((int)(1-temp2_uint/temp1_uint)));
      }
      nfound++;
     }
    }
    else if (temp1_uint != temp2_uint)
    {

	if ( print_data(options) ) 
	{
	    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);

		parallel_print(SPACES);
		parallel_print(UIFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)));
	}
	nfound++;
    }



  } /*H5T_NATIVE_UINT*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_LONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_LONG)) 
  {
   long        temp1_long;
   long        temp2_long;
   assert(type_size==sizeof(long));
 
    memcpy(&temp1_long, mem1, sizeof(long));
    memcpy(&temp2_long, mem2, sizeof(long));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (labs(temp1_long-temp2_long) > (long)options->delta)
     {
    
      if ( print_data(options) ) 
      {
       print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	   parallel_print(SPACES);
	   parallel_print(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options->percent )
     {
     
      if ( print_data(options) ) 
      {
       print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
	   parallel_print(SPACES);
	   parallel_print(LPIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long), labs(1-temp2_long/temp1_long));
      }
	   nfound++;
     }
    }
    /* -d and -p */
    else if ( options->d && options->p)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options->percent && 
      labs(temp1_long-temp2_long) > (long)options->delta )
     {
     
      if ( print_data(options) ) 
      {
	  print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
	      parallel_print(SPACES);
	      parallel_print(LPIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long), labs(1-temp2_long/temp1_long));
      }
      nfound++;
     }
    }
    else if (temp1_long != temp2_long)
    {

	if ( print_data(options) ) 
	{
	    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	    parallel_print(SPACES);
	    parallel_print(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
	}
	nfound++;
    }

  
   
  } /*H5T_NATIVE_LONG*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_ULONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_ULONG)) 
  {
   unsigned long        temp1_ulong;
   unsigned long        temp2_ulong;
   assert(type_size==sizeof(unsigned long));
  
    memcpy(&temp1_ulong, mem1, sizeof(unsigned long));
    memcpy(&temp2_ulong, mem2, sizeof(unsigned long));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (labs((long)(temp1_ulong-temp2_ulong)) > (long)options->delta)
     {
     
      if ( print_data(options) ) 
      {
       print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	   parallel_print(SPACES);
	   parallel_print(ULIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_ulong!=0 && labs((long)(1-temp2_ulong/temp1_ulong)) > (long)options->percent )
     {
      
      if ( print_data(options) ) 
      {
	  print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
	      parallel_print(SPACES);
	      parallel_print(ULPIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)), labs((long)(1-temp2_ulong/temp1_ulong)));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options->d && options->p)
    {
     if ( temp1_ulong!=0 && labs((long)(1-temp2_ulong/temp1_ulong)) > (long)options->percent && 
      labs((long)(temp1_ulong-temp2_ulong)) > (long)options->delta )
     {

	 if ( print_data(options) ) 
	 {
	     print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		 parallel_print(SPACES);
		 parallel_print(ULPIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)), labs((long)(1-temp2_ulong/temp1_ulong)));
	 }
	 nfound++;
     }
    }
    else if (temp1_ulong != temp2_ulong)
    {

     if ( print_data(options) ) 
     {
      print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	  parallel_print(SPACES);
	  parallel_print(ULIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)));
     }
     nfound++;
    }
  
   
  } /*H5T_NATIVE_ULONG*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_LLONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_LLONG)) 
  {
   long_long        temp1_llong;
   long_long        temp2_llong;
   assert(type_size==sizeof(long_long));
  
    memcpy(&temp1_llong, mem1, sizeof(long_long));
    memcpy(&temp2_llong, mem2, sizeof(long_long));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (labs((long)(temp1_llong-temp2_llong)) > (long)options->delta)
     {
    
      if ( print_data(options) ) 
      {
       print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	   parallel_print(SPACES);
	   parallel_print(fmt_llong,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_llong!=0 && labs((long)(1-temp2_llong/temp1_llong)) > (long)options->percent )
     {
     
      if ( print_data(options) ) 
      {
	  print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
	      parallel_print(SPACES);
	      parallel_print(fmt_llongp,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)), (long_long)labs((long)(1-temp2_llong/temp1_llong)));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options->d && options->p)
    {
	if ( temp1_llong!=0 && labs((long)(1-temp2_llong/temp1_llong)) > (long)options->percent && 
		labs((long)(temp1_llong-temp2_llong)) > (long)options->delta )
	{

	    if ( print_data(options) ) 
	    {
		print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_llongp,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)), (long_long)labs((long)(1-temp2_llong/temp1_llong)));
	    }
	    nfound++;
	}
    }
    else if (temp1_llong != temp2_llong)
    {

	if ( print_data(options) ) 
	{
	    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);

	   parallel_print(SPACES);
	   parallel_print(fmt_llong,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)));
	}
     nfound++;
    }
 
   
  } /*H5T_NATIVE_LLONG*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_ULLONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_ULLONG)) 
  {
   unsigned long_long        temp1_ullong;
   unsigned long_long        temp2_ullong;
   assert(type_size==sizeof(unsigned long_long));
  
    memcpy(&temp1_ullong, mem1, sizeof(unsigned long_long));
    memcpy(&temp2_ullong, mem2, sizeof(unsigned long_long));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (labs((long)(temp1_ullong-temp2_ullong)) > (long)options->delta)
     {
     
      if ( print_data(options) ) 
      {
       print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	   parallel_print(SPACES);
       parallel_print(fmt_ullong,temp1_ullong,temp2_ullong, (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_ullong!=0 && labs((long)(1-temp2_ullong/temp1_ullong)) > (long)options->percent )
     {
   
      if ( print_data(options) ) 
      {
       print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
	   parallel_print(SPACES);
       parallel_print(fmt_ullongp,temp1_ullong,temp2_ullong, (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)), (unsigned long_long)labs((long)(1-temp2_ullong/temp1_ullong)));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options->d && options->p)
    {
     if ( temp1_ullong!=0 && labs((long)(1-temp2_ullong/temp1_ullong)) > (long)options->percent && 
      labs((long)(temp1_ullong-temp2_ullong)) > (long)options->delta )
     {
    
      if ( print_data(options) ) 
      {
       print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
      
	   parallel_print(SPACES);
       parallel_print(fmt_ullongp,temp1_ullong,temp2_ullong, (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)), (unsigned long_long)labs((long)(1-temp2_ullong/temp1_ullong)));
      }
      nfound++;
     }
    }
    else if (temp1_ullong != temp2_ullong)
    {
   
     if ( print_data(options) ) 
     {
      print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	   parallel_print(SPACES);
       parallel_print(fmt_ullong,temp1_ullong,temp2_ullong, (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)));
     }
     nfound++;
    }
  
  } /*H5T_NATIVE_ULLONG*/

   
  break; /* H5T_INTEGER class */
	case H5T_FLOAT:

  /*-------------------------------------------------------------------------
   * H5T_NATIVE_FLOAT
   *-------------------------------------------------------------------------
   */

  if (H5Tequal(m_type, H5T_NATIVE_FLOAT)) 
  {
      float       temp1_float;
      float       temp2_float;
      assert(type_size==sizeof(float));

      memcpy(&temp1_float, mem1, sizeof(float));
      memcpy(&temp2_float, mem2, sizeof(float));


      /* -d and !-p */
      if (options->d && !options->p)
      {
	  if (fabs(temp1_float-temp2_float) > options->delta)
	  {

	      if ( print_data(options) ) 
	      {
		  print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		  parallel_print(SPACES);
		  parallel_print(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
	      }
	      nfound++;
	  }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
	  if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options->percent )
	  {

	      if ( print_data(options) ) 
	      {
		  print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		  parallel_print(SPACES);
		  parallel_print(FPFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float), fabs(1-temp2_float/temp1_float));
	      }
	      nfound++;
	  }
      }
  
  /* -d and -p */
  else if ( options->d && options->p)
  {
      if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options->percent && 
	      fabs(temp1_float-temp2_float) > options->delta )
      {

	  if ( print_data(options) ) 
		  {
		      print_pos(ph,1,i,acc,pos,rank,obj1,obj2);

		      parallel_print(SPACES);
		      parallel_print(FPFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float), fabs(1-temp2_float/temp1_float));
		  }
		      nfound++;
		  }
	      }
	      else if (temp1_float != temp2_float)
	      {

		  if ( print_data(options) ) 
		  {
		      print_pos(ph,0,i,acc,pos,rank,obj1,obj2);

		      parallel_print(SPACES);
		      parallel_print(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
		  }
		  nfound++;
	      }

	  } /*H5T_NATIVE_FLOAT*/

	  /*-------------------------------------------------------------------------
	   * H5T_NATIVE_DOUBLE
	   *-------------------------------------------------------------------------
	   */

	  else if (H5Tequal(m_type, H5T_NATIVE_DOUBLE)) 
	  {
	      double      temp1_double;
	      double      temp2_double;
	      assert(type_size==sizeof(double));

	      memcpy(&temp1_double, mem1, sizeof(double));
	      memcpy(&temp2_double, mem2, sizeof(double));
	      /* -d and !-p */
	      if (options->d && !options->p)
	      {
		  if (fabs(temp1_double-temp2_double) > options->delta)
		  {

		      if ( print_data(options) ) 
		      {
			  print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
			  parallel_print(SPACES);
			  parallel_print(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
		      }
			  nfound++;
		      }
		  }
		  /* !-d and -p */
		  else if (!options->d && options->p)
		  {
		      if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options->percent )
		      {

			  if ( print_data(options) ) 
			  {
			      print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
			      parallel_print(SPACES);
			      parallel_print(FPFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double), fabs(1-temp2_double/temp1_double));
			  }
			      nfound++;
			  }
		      }
		      /* -d and -p */
		      else if ( options->d && options->p)
		      {
			  if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options->percent && 
				  fabs(temp1_double-temp2_double) > options->delta )
			  {

			      if ( print_data(options) ) 
			      {
				  print_pos(ph,1,i,acc,pos,rank,obj1,obj2);

				  parallel_print(SPACES);
				  parallel_print(FPFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double), fabs(1-temp2_double/temp1_double));
			      }
				  nfound++;
			      }
			  }
			  else if (temp1_double != temp2_double)
			  {

			      if ( print_data(options) ) 
			      {
				  print_pos(ph,0,i,acc,pos,rank,obj1,obj2);

				  parallel_print(SPACES);
				  parallel_print(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
			      
			      }
			      nfound++;
			  }

		      } /*H5T_NATIVE_DOUBLE*/


		      break;   /* H5T_FLOAT class */

		  } /* switch */


		  return nfound;
	      }



	      /*-------------------------------------------------------------------------
	       * Function: diff_native_uchar
	       *
	       * Purpose: do a byte-by-byte comparison and print in numerical format
	       *
	       * Return: number of differences found
	       *
	       * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
		   *
		   * Date: October 29, 2003
		   *
		   *-------------------------------------------------------------------------
		   */

hsize_t diff_native_uchar(unsigned char *mem1,
			  unsigned char *mem2,
			  hsize_t       i, 
			  int           rank, 
			  hsize_t       *acc,  
			  hsize_t       *pos,
			  diff_opt_t    *options, 
			  const char    *obj1, 
			  const char    *obj2,
			  int           *ph)
{
    hsize_t            nfound=0;  /* differences found */
    unsigned char      temp1_uchar;
    unsigned char      temp2_uchar;

    memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
    memcpy(&temp2_uchar, mem2, sizeof(unsigned char));

    /* -d and !-p */
    if (options->d && !options->p)
    {
	if (abs(temp1_uchar-temp2_uchar) > options->delta)
	{
	    if ( print_data(options) ) 
	    {
		print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		parallel_print(SPACES);
		parallel_print(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
	    }
	    nfound++;
	}
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
	if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent )
	{
	    if ( print_data(options) ) 
	    {
		print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		parallel_print(SPACES);
		parallel_print(IPFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar), abs(1-temp2_uchar/temp1_uchar));
	    }
	    nfound++;
	}
    }
    /* -d and -p */
    else if ( options->d && options->p)
    {
	if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent && 
		abs(temp1_uchar-temp2_uchar) > options->delta )
	{
	    if ( print_data(options) ) 
	    {
		print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		parallel_print(SPACES);
		parallel_print(IPFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar), abs(1-temp2_uchar/temp1_uchar));
	    }
	    nfound++;
	}
    }
    else if (temp1_uchar != temp2_uchar)
    {
	if ( print_data(options) ) 
	{
	    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	    parallel_print(SPACES);
	    parallel_print(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
	}
	nfound++;
    }

    return nfound;
} 


/*-------------------------------------------------------------------------
 * Function: diff_char
 *
 * Purpose: do a byte-by-byte comparison and print in char format
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 29, 2003
 *
 *-------------------------------------------------------------------------
 */

hsize_t diff_char(unsigned char *mem1,
                  unsigned char *mem2,
                  hsize_t       i, 
                  int           rank, 
                  hsize_t       *acc,  
                  hsize_t       *pos,
                  diff_opt_t    *options, 
                  const char    *obj1, 
                  const char    *obj2,
                  int           *ph)
{
    hsize_t            nfound=0;  /* differences found */
    unsigned char      temp1_uchar;
    unsigned char      temp2_uchar;

    memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
    memcpy(&temp2_uchar, mem2, sizeof(unsigned char));

    if (temp1_uchar != temp2_uchar)
    {
	if ( print_data(options) ) 
	{
	    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
	    parallel_print(SPACES);
	    parallel_print(CFORMAT,temp1_uchar,temp2_uchar);
	}
	nfound++;
    }

    return nfound;
} 




/*-------------------------------------------------------------------------
     * Function:	is_zero
 *
 * Purpose:	Determines if memory is initialized to all zero bytes.
 *
 * Return:	TRUE if all bytes are zero; FALSE otherwise
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
is_zero(const void *_mem, size_t size)
{
 const unsigned char *mem = (const unsigned char *)_mem;
 
 while (size-- > 0)
  if (mem[size])
   return FALSE;
  
  return TRUE;
}

/*-------------------------------------------------------------------------
 * Function:	close_obj
 *
 * Purpose:	Auxialiary function to close an object
 *
 *-------------------------------------------------------------------------
 */

static 
void close_obj(H5G_obj_t obj_type, hid_t obj_id)
{
 
 switch (obj_type) {
 case H5G_GROUP:
  H5Gclose(obj_id);
  break;
 case H5G_DATASET:
  H5Dclose(obj_id);
  break;
 case H5G_TYPE:
  H5Tclose(obj_id);
  break;
 default:
  assert(0);
  break;
 }
}



/*-------------------------------------------------------------------------
 * Function: diff_region
 *
 * Purpose: diff a dataspace region
 *
 * Return: 0, diff not found, 1 found
 *
 *-------------------------------------------------------------------------
 */
static int diff_region(hid_t region1_id, hid_t region2_id)
{
 hssize_t 	nblocks1, npoints1;
 hssize_t 	nblocks2, npoints2;
 hsize_t   alloc_size;
 hsize_t   *ptdata1;
 hsize_t   *ptdata2;
 int		     ndims1 = H5Sget_simple_extent_ndims(region1_id);
 int		     ndims2 = H5Sget_simple_extent_ndims(region2_id);
 int       ret=0;

#if defined (H5DIFF_DEBUG)
 int i;
#endif
 
/*
 * These two functions fail if the region does not have blocks or points,
 * respectively. They do not currently know how to translate from one to
 * the other.
 */
 H5E_BEGIN_TRY {
  nblocks1 = H5Sget_select_hyper_nblocks(region1_id);
  nblocks2 = H5Sget_select_hyper_nblocks(region2_id);

  npoints1 = H5Sget_select_elem_npoints(region1_id);
  npoints2 = H5Sget_select_elem_npoints(region2_id);
 } H5E_END_TRY;

 if (nblocks1!=nblocks2 || npoints1!=npoints2 || ndims1!=ndims2)
  return 1;
 
 /* compare block information */
 if (nblocks1 > 0) 
 {

  alloc_size = nblocks1 * ndims1 * 2 * sizeof(ptdata1[0]);
  assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/

  ptdata1 = malloc((size_t)alloc_size);
  H5_CHECK_OVERFLOW(nblocks1, hssize_t, hsize_t);
  H5Sget_select_hyper_blocklist(region1_id, (hsize_t)0, (hsize_t)nblocks1, ptdata1);

  ptdata2 = malloc((size_t)alloc_size);
  H5_CHECK_OVERFLOW(nblocks2, hssize_t, hsize_t);
  H5Sget_select_hyper_blocklist(region2_id, (hsize_t)0, (hsize_t)nblocks2, ptdata2);

  ret=HDmemcmp(ptdata1,ptdata2,(size_t)alloc_size);

#if defined (H5DIFF_DEBUG)
  for (i = 0; i < nblocks1; i++) 
  {
   int j;
   
   /* start coordinates and opposite corner */
   for (j = 0; j < ndims1; j++)
    parallel_print("%s%lu", j ? "," : "(",
    (unsigned long)ptdata1[i * 2 * ndims1 + j]);
   
   for (j = 0; j < ndims1; j++)
    parallel_print("%s%lu", j ? "," : ")-(",
    (unsigned long)ptdata1[i * 2 * ndims1 + j + ndims1]);
   
   parallel_print(")\n");
  }
#endif 
    
   
  HDfree(ptdata1);
  HDfree(ptdata2);
 }
 
 /* Print point information */
 if (npoints1 > 0) 
 {
  alloc_size = npoints1 * ndims1 * sizeof(ptdata1[0]);
  assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/

  ptdata1 = malloc((size_t)alloc_size);
  H5_CHECK_OVERFLOW(npoints1,hssize_t,hsize_t);
  H5Sget_select_elem_pointlist(region1_id, (hsize_t)0, (hsize_t)npoints1, ptdata1);

  ptdata2 = malloc((size_t)alloc_size);
  H5_CHECK_OVERFLOW(npoints1,hssize_t,hsize_t);
  H5Sget_select_elem_pointlist(region2_id, (hsize_t)0, (hsize_t)npoints2, ptdata2);
  
  ret=HDmemcmp(ptdata1,ptdata2,(size_t)alloc_size);

#if defined (H5DIFF_DEBUG)
  for (i = 0; i < npoints1; i++) 
  {
   int j;
   
   parallel_print("%sPt%lu: " ,
    i ? "," : "",
    (unsigned long)i);
   
   for (j = 0; j < ndims1; j++)
    parallel_print("%s%lu", j ? "," : "(",
    (unsigned long)(ptdata1[i * ndims1 + j]));
   
   parallel_print(")");
  }
#endif 
  
  HDfree(ptdata1);
  HDfree(ptdata2);
 }
 
 return ret;
}



/*-------------------------------------------------------------------------
 * Function: diff_float
 *
 * Purpose: diff a H5T_NATIVE_FLOAT type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_float(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank, 
	hsize_t       *acc,  
	hsize_t       *pos,
	diff_opt_t    *options, 
	const char    *obj1, 
	const char    *obj2,
	int           *ph)

{
    hsize_t     nfound=0;          /* number of differences found */
    float       temp1_float;
    float       temp2_float;
    hsize_t     i;


    /* -d and !-p */
    if (options->d && !options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_float, mem1, sizeof(float));
	    memcpy(&temp2_float, mem2, sizeof(float));

	    if (fabs(temp1_float-temp2_float) > options->delta)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
		}
		nfound++;
	    }
	    mem1+=sizeof(float);
	    mem2+=sizeof(float);
	    if (options->n && nfound>=options->count)
		return nfound;
	}
    }

    /* !-d and -p */
    else if (!options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_float, mem1, sizeof(float));
	    memcpy(&temp2_float, mem2, sizeof(float));

	    if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options->percent )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(FPFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float),
			    fabs(1-temp2_float/temp1_float));
		}
		nfound++;
	    }
	    mem1+=sizeof(float);
	    mem2+=sizeof(float);
	    if (options->n && nfound>=options->count)
		return nfound;
	}
    } 

    /* -d and -p */
    else if ( options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_float, mem1, sizeof(float));
	    memcpy(&temp2_float, mem2, sizeof(float));

	    if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options->percent && 
		    fabs(temp1_float-temp2_float) > options->delta )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(FPFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float),
			    fabs(1-temp2_float/temp1_float));
		}
		nfound++;
	    }
	    mem1+=sizeof(float);
	    mem2+=sizeof(float);
	    if (options->n && nfound>=options->count)
		return nfound;
	}
    } 
    else 
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_float, mem1, sizeof(float));
	    memcpy(&temp2_float, mem2, sizeof(float));

	    if (temp1_float != temp2_float)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
		}
		nfound++;
	    }

	    mem1+=sizeof(float);
	    mem2+=sizeof(float);
	    if (options->n && nfound>=options->count)
		return nfound;
	} /* nelmts */

    }

    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_double
 *
 * Purpose: diff a H5T_NATIVE_DOUBLE type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_double(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank, 
	hsize_t       *acc,  
	hsize_t       *pos,
	diff_opt_t    *options, 
	const char    *obj1, 
	const char    *obj2,
	int           *ph)

{
    hsize_t     nfound=0;          /* number of differences found */
    double      temp1_double;
    double      temp2_double;
    hsize_t     i;


    /* -d and !-p */
    if (options->d && !options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_double, mem1, sizeof(double));
	    memcpy(&temp2_double, mem2, sizeof(double));

	    if (fabs(temp1_double-temp2_double) > options->delta)
	    {

		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
		}
		nfound++;
	    }
	    mem1+=sizeof(double);
	    mem2+=sizeof(double);
	    if (options->n && nfound>=options->count)
		return nfound;
	}
    }

    /* !-d and -p */
    else if (!options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_double, mem1, sizeof(double));
	    memcpy(&temp2_double, mem2, sizeof(double));

	    if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options->percent )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(FPFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double),
			    fabs(1-temp2_double/temp1_double));
		}
		nfound++;
	    }
	    mem1+=sizeof(double);
	    mem2+=sizeof(double);
	    if (options->n && nfound>=options->count)
		return nfound;
	}
    } 

    /* -d and -p */
    else if ( options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_double, mem1, sizeof(double));
	    memcpy(&temp2_double, mem2, sizeof(double));

	    if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options->percent && 
		    fabs(temp1_double-temp2_double) > options->delta )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(FPFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double),
			    fabs(1-temp2_double/temp1_double));
		}
		nfound++;
	    }
	    mem1+=sizeof(double);
	    mem2+=sizeof(double);
	    if (options->n && nfound>=options->count)
		return nfound;
	}
    } 
    else 
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_double, mem1, sizeof(double));
	    memcpy(&temp2_double, mem2, sizeof(double));

	    if (temp1_double != temp2_double)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
		}
		nfound++;
	    }

	    mem1+=sizeof(double);
	    mem2+=sizeof(double);
	    if (options->n && nfound>=options->count)
		return nfound;
	} /* nelmts */

    }

    return nfound;
}




/*-------------------------------------------------------------------------
 * Function: diff_schar
 *
 * Purpose: diff a H5T_NATIVE_SCHAR type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_schar(unsigned char *mem1,
																	 	unsigned char *mem2,
																		 hsize_t       nelmts,
																		 int           rank, 
																		 hsize_t       *acc,  
																		 hsize_t       *pos,
																		 diff_opt_t    *options, 
																		 const char    *obj1, 
																		 const char    *obj2,
																		 int           *ph)
																			
{
	hsize_t     nfound=0;          /* number of differences found */
	char        temp1_char;
 char        temp2_char;
	hsize_t     i;
	
	
	/* -d and !-p */
	if (options->d && !options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_char, mem1, sizeof(char));
			memcpy(&temp2_char, mem2, sizeof(char));
			
			if (abs(temp1_char-temp2_char) > options->delta)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
				}
				nfound++;
			}
		mem1+=sizeof(char);
  mem2+=sizeof(char);
			if (options->n && nfound>=options->count)
					return nfound;
		}
	}
	
	/* !-d and -p */
	else if (!options->d && options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_char, mem1, sizeof(char));
			memcpy(&temp2_char, mem2, sizeof(char));
			
			if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options->percent )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IPFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char),
						abs(1-temp2_char/temp1_char));
				}
				nfound++;
			}
		mem1+=sizeof(char);
  mem2+=sizeof(char);
			if (options->n && nfound>=options->count)
					return nfound;
		}
	} 
	
	/* -d and -p */
	else if ( options->d && options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_char, mem1, sizeof(char));
			memcpy(&temp2_char, mem2, sizeof(char));
			
			if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options->percent && 
				abs(temp1_char-temp2_char) > options->delta )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IPFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char),
						abs(1-temp2_char/temp1_char));
				}
				nfound++;
			}
		mem1+=sizeof(char);
  mem2+=sizeof(char);
			if (options->n && nfound>=options->count)
					return nfound;
		}
	
	} 
	else 
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_char, mem1, sizeof(char));
			memcpy(&temp2_char, mem2, sizeof(char));
			
			if (temp1_char != temp2_char)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
				}
				nfound++;
			}
			
			mem1+=sizeof(char);
			mem2+=sizeof(char);
				if (options->n && nfound>=options->count)
					return nfound;
		} /* nelmts */
		
	}
	
	return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_uchar
 *
 * Purpose: diff a H5T_NATIVE_UCHAR type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_uchar(unsigned char *mem1,
																	 	unsigned char *mem2,
																		 hsize_t       nelmts,
																		 int           rank, 
																		 hsize_t       *acc,  
																		 hsize_t       *pos,
																		 diff_opt_t    *options, 
																		 const char    *obj1, 
																		 const char    *obj2,
																		 int           *ph)
																			
{
	hsize_t       nfound=0;          /* number of differences found */
	unsigned char temp1_uchar;
 unsigned char temp2_uchar;
	hsize_t       i;
	
	
	/* -d and !-p */
	if (options->d && !options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
			memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
			
			if (abs(temp1_uchar-temp2_uchar) > options->delta)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
				}
				nfound++;
			}
		mem1+=sizeof(unsigned char);
  mem2+=sizeof(unsigned char);
			if (options->n && nfound>=options->count)
					return nfound;
		}
	
	}
	
	/* !-d and -p */
	else if (!options->d && options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
			memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
			
			if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IPFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar),
						abs(1-temp2_uchar/temp1_uchar));
				}
				nfound++;
			}
		mem1+=sizeof(unsigned char);
  mem2+=sizeof(unsigned char);
			if (options->n && nfound>=options->count)
					return nfound;
		}
	} 
	
	/* -d and -p */
	else if ( options->d && options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
			memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
			
			if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent && 
				abs(temp1_uchar-temp2_uchar) > options->delta )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IPFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar),
						abs(1-temp2_uchar/temp1_uchar));
				}
				nfound++;
			}
			mem1+=sizeof(unsigned char);
  mem2+=sizeof(unsigned char);
			if (options->n && nfound>=options->count)
					return nfound;
		}
		
	} 
	else 
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
			memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
			
			if (temp1_uchar != temp2_uchar)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
				}
				nfound++;
			}
			
			mem1+=sizeof(unsigned char);
			mem2+=sizeof(unsigned char);
				if (options->n && nfound>=options->count)
					return nfound;
		} /* nelmts */
		
	}
	
	return nfound;
}




/*-------------------------------------------------------------------------
 * Function: diff_short
 *
 * Purpose: diff a H5T_NATIVE_SHORT type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_short(unsigned char *mem1,
																	 	unsigned char *mem2,
																		 hsize_t       nelmts,
																		 int           rank, 
																		 hsize_t       *acc,  
																		 hsize_t       *pos,
																		 diff_opt_t    *options, 
																		 const char    *obj1, 
																		 const char    *obj2,
																		 int           *ph)
																			
{
	hsize_t       nfound=0;          /* number of differences found */
	short         temp1_short;
 short         temp2_short;
	hsize_t       i;
	
	
	/* -d and !-p */
	if (options->d && !options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_short, mem1, sizeof(short));
			memcpy(&temp2_short, mem2, sizeof(short));
			
			if (abs(temp1_short-temp2_short) > options->delta)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
				}
				nfound++;
			}
			mem1+=sizeof(short);
  mem2+=sizeof(short);
			if (options->n && nfound>=options->count)
					return nfound;
		}
		
	}
	
	/* !-d and -p */
	else if (!options->d && options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_short, mem1, sizeof(short));
			memcpy(&temp2_short, mem2, sizeof(short));
			
			if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options->percent )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IPFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short),
						abs(1-temp2_short/temp1_short));
				}
				nfound++;
			}
			mem1+=sizeof(short);
  mem2+=sizeof(short);
			if (options->n && nfound>=options->count)
					return nfound;
		}
		
		
	} 
	
	/* -d and -p */
	else if ( options->d && options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_short, mem1, sizeof(short));
			memcpy(&temp2_short, mem2, sizeof(short));
			
			if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options->percent && 
				abs(temp1_short-temp2_short) > options->delta )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IPFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short),
						abs(1-temp2_short/temp1_short));
				}
				nfound++;
			}
			mem1+=sizeof(short);
  mem2+=sizeof(short);
			if (options->n && nfound>=options->count)
					return nfound;
		}
		
	} 
	else 
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_short, mem1, sizeof(short));
			memcpy(&temp2_short, mem2, sizeof(short));
			
			if (temp1_short != temp2_short)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
				}
				nfound++;
			}
			
			mem1+=sizeof(short);
			mem2+=sizeof(short);
				if (options->n && nfound>=options->count)
					return nfound;
		} /* nelmts */
		
	}
	
	return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff_ushort
 *
 * Purpose: diff a H5T_NATIVE_USHORT type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_ushort(unsigned char *mem1,
																	  	unsigned char *mem2,
																		  hsize_t       nelmts,
																		  int           rank, 
																		  hsize_t       *acc,  
																		  hsize_t       *pos,
																		  diff_opt_t    *options, 
																		  const char    *obj1, 
																		  const char    *obj2,
																		  int           *ph)
																			
{
	hsize_t        nfound=0;          /* number of differences found */
	unsigned short temp1_ushort;
 unsigned short temp2_ushort;
	hsize_t        i;
	
	
	/* -d and !-p */
	if (options->d && !options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_ushort, mem1, sizeof(unsigned short));
			memcpy(&temp2_ushort, mem2, sizeof(unsigned short));
			
			if (abs(temp1_ushort-temp2_ushort) > options->delta)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort));
				}
				nfound++;
			}
				mem1+=sizeof(unsigned short);
  mem2+=sizeof(unsigned short);
			if (options->n && nfound>=options->count)
					return nfound;
		}
	
	}
	
	/* !-d and -p */
	else if (!options->d && options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_ushort, mem1, sizeof(unsigned short));
			memcpy(&temp2_ushort, mem2, sizeof(unsigned short));
			
			if ( temp1_ushort!=0 && abs(1-temp2_ushort/temp1_ushort) > options->percent )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IPFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort),
						abs(1-temp2_ushort/temp1_ushort));
				}
				nfound++;
			}
				mem1+=sizeof(unsigned short);
  mem2+=sizeof(unsigned short);
			if (options->n && nfound>=options->count)
					return nfound;
		}
		
	
	} 
	
	/* -d and -p */
	else if ( options->d && options->p)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_ushort, mem1, sizeof(unsigned short));
			memcpy(&temp2_ushort, mem2, sizeof(unsigned short));
			
			if ( temp1_ushort!=0 && abs(1-temp2_ushort/temp1_ushort) > options->percent && 
				abs(temp1_ushort-temp2_ushort) > options->delta )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IPFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort),
						abs(1-temp2_ushort/temp1_ushort));
				}
				nfound++;
			}
				mem1+=sizeof(unsigned short);
  mem2+=sizeof(unsigned short);
			if (options->n && nfound>=options->count)
					return nfound;
		}
	
	} 
	else 
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_ushort, mem1, sizeof(unsigned short));
			memcpy(&temp2_ushort, mem2, sizeof(unsigned short));
			
			if (temp1_ushort != temp2_ushort)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(IFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort));
				}
				nfound++;
			}
			
			mem1+=sizeof(unsigned short);
			mem2+=sizeof(unsigned short);
				if (options->n && nfound>=options->count)
					return nfound;
		} /* nelmts */
		
	}
	
	return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_int
 *
 * Purpose: diff a H5T_NATIVE_INT type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_int(unsigned char *mem1,
																	unsigned char *mem2,
																	hsize_t       nelmts,
																	int           rank, 
																	hsize_t       *acc,  
																	hsize_t       *pos,
																	diff_opt_t    *options, 
																	const char    *obj1, 
																	const char    *obj2,
																	int           *ph)
																			
{
    hsize_t       nfound=0;          /* number of differences found */
    int           temp1_int;
    int           temp2_int;
    hsize_t       i;


    /* -d and !-p */
    if (options->d && !options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_int, mem1, sizeof(int));
	    memcpy(&temp2_int, mem2, sizeof(int));

	    if (abs(temp1_int-temp2_int) > options->delta)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
		}
		nfound++;
	    }
	    mem1+=sizeof(int);
	    mem2+=sizeof(int);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    }

    /* !-d and -p */
    else if (!options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_int, mem1, sizeof(int));
	    memcpy(&temp2_int, mem2, sizeof(int));

	    if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options->percent )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(IPFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int),
			    abs(1-temp2_int/temp1_int));
		}
		nfound++;
	    }
	    mem1+=sizeof(int);
	    mem2+=sizeof(int);
	    if (options->n && nfound>=options->count)
		return nfound;
	}


    } 

    /* -d and -p */
    else if ( options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_int, mem1, sizeof(int));
	    memcpy(&temp2_int, mem2, sizeof(int));

	    if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options->percent && 
		    abs(temp1_int-temp2_int) > options->delta )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(IPFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int),
			    abs(1-temp2_int/temp1_int));
		}
		nfound++;
	    }
	    mem1+=sizeof(int);
	    mem2+=sizeof(int);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    } 
    else 
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_int, mem1, sizeof(int));
	    memcpy(&temp2_int, mem2, sizeof(int));

	    if (temp1_int != temp2_int)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
		}
		nfound++;
	    }

	    mem1+=sizeof(int);
	    mem2+=sizeof(int);
	    if (options->n && nfound>=options->count)
		return nfound;
	} /* nelmts */

    }

    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_uint
 *
 * Purpose: diff a H5T_NATIVE_UINT type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_uint(unsigned char *mem1,
																	 unsigned char *mem2,
																		hsize_t       nelmts,
																		int           rank, 
																		hsize_t       *acc,  
																		hsize_t       *pos,
																		diff_opt_t    *options, 
																		const char    *obj1, 
																		const char    *obj2,
																		int           *ph)
																			
{
    hsize_t        nfound=0;          /* number of differences found */
    unsigned int temp1_uint;
    unsigned int temp2_uint;
    hsize_t        i;


    /* -d and !-p */
    if (options->d && !options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_uint, mem1, sizeof(unsigned int));
	    memcpy(&temp2_uint, mem2, sizeof(unsigned int));

	    if (abs(temp1_uint-temp2_uint) > options->delta)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(IFORMAT,temp1_uint,temp2_uint,abs(temp1_uint-temp2_uint));
		}
		nfound++;
	    }
	    mem1+=sizeof(unsigned int);
	    mem2+=sizeof(unsigned int);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    }

    /* !-d and -p */
    else if (!options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_uint, mem1, sizeof(unsigned int));
	    memcpy(&temp2_uint, mem2, sizeof(unsigned int));

	    if ( temp1_uint!=0 && abs(1-temp2_uint/temp1_uint) > options->percent )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(IPFORMAT,temp1_uint,temp2_uint,abs(temp1_uint-temp2_uint),
			    abs(1-temp2_uint/temp1_uint));
		}
		nfound++;
	    }
	    mem1+=sizeof(unsigned int);
	    mem2+=sizeof(unsigned int);
	    if (options->n && nfound>=options->count)
		return nfound;
	}


    } 

    /* -d and -p */
    else if ( options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_uint, mem1, sizeof(unsigned int));
	    memcpy(&temp2_uint, mem2, sizeof(unsigned int));

	    if ( temp1_uint!=0 && abs(1-temp2_uint/temp1_uint) > options->percent && 
		    abs(temp1_uint-temp2_uint) > options->delta )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(IPFORMAT,temp1_uint,temp2_uint,abs(temp1_uint-temp2_uint),
			    abs(1-temp2_uint/temp1_uint));
		}
		nfound++;
	    }
	    mem1+=sizeof(unsigned int);
	    mem2+=sizeof(unsigned int);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    } 
    else 
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_uint, mem1, sizeof(unsigned int));
	    memcpy(&temp2_uint, mem2, sizeof(unsigned int));

	    if (temp1_uint != temp2_uint)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(IFORMAT,temp1_uint,temp2_uint,abs(temp1_uint-temp2_uint));
		}
		nfound++;
	    }

	    mem1+=sizeof(unsigned int);
	    mem2+=sizeof(unsigned int);
	    if (options->n && nfound>=options->count)
		return nfound;
	} /* nelmts */

    }

    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_long
 *
 * Purpose: diff a H5T_NATIVE_LONG type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_long(unsigned char *mem1,
															 		unsigned char *mem2,
															 		hsize_t       nelmts,
															 		int           rank, 
																 	hsize_t       *acc,  
																 	hsize_t       *pos,
																 	diff_opt_t    *options, 
																 	const char    *obj1, 
																	 const char    *obj2,
																	 int           *ph)
																			
{
    hsize_t       nfound=0;          /* number of differences found */
    long          temp1_long;
    long          temp2_long;
    hsize_t       i;


    /* -d and !-p */
    if (options->d && !options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_long, mem1, sizeof(long));
	    memcpy(&temp2_long, mem2, sizeof(long));

	    if (labs(temp1_long-temp2_long) > options->delta)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
		}
		nfound++;
	    }
	    mem1+=sizeof(long);
	    mem2+=sizeof(long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    }

    /* !-d and -p */
    else if (!options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_long, mem1, sizeof(long));
	    memcpy(&temp2_long, mem2, sizeof(long));

	    if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > options->percent )
	    {
		if ( print_data(options) ) 
		{
			memcpy(&temp1_long, mem1, sizeof(long));
			memcpy(&temp2_long, mem2, sizeof(long));
			
			if (labs(temp1_long-temp2_long) > (long)options->delta)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
				}
				nfound++;
			}
				mem1+=sizeof(long);
  mem2+=sizeof(long);
			if (options->n && nfound>=options->count)
					return nfound;
		}
		nfound++;
	    }
	    mem1+=sizeof(long);
	    mem2+=sizeof(long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}


    } 

    /* -d and -p */
    else if ( options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_long, mem1, sizeof(long));
	    memcpy(&temp2_long, mem2, sizeof(long));

	    if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > options->percent && 
		    labs(temp1_long-temp2_long) > options->delta )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(LPIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long),
			    labs(1-temp2_long/temp1_long));
		}
		nfound++;
	    }
	    mem1+=sizeof(long);
	    mem2+=sizeof(long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    } 
    else 
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_long, mem1, sizeof(long));
	    memcpy(&temp2_long, mem2, sizeof(long));

	    if (temp1_long != temp2_long)
	    {
		if ( print_data(options) ) 
		{
			memcpy(&temp1_long, mem1, sizeof(long));
			memcpy(&temp2_long, mem2, sizeof(long));
			
			if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > options->percent && 
				labs(temp1_long-temp2_long) > (long)options->delta )
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(LPIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long),
						labs(1-temp2_long/temp1_long));
				}
				nfound++;
			}
				mem1+=sizeof(long);
  mem2+=sizeof(long);
			if (options->n && nfound>=options->count)
					return nfound;
		}
		nfound++;
	    }

	    mem1+=sizeof(long);
	    mem2+=sizeof(long);
	    if (options->n && nfound>=options->count)
		return nfound;
	} /* nelmts */

    }

    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_ulong
 *
 * Purpose: diff a H5T_NATIVE_ULONG type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_ulong(unsigned char *mem1,
																	  unsigned char *mem2,
																	 	hsize_t       nelmts,
																	 	int           rank, 
																	 	hsize_t       *acc,  
																		 hsize_t       *pos,
																		 diff_opt_t    *options, 
																		 const char    *obj1, 
																		 const char    *obj2,
																	 	int           *ph)
																			
{
    hsize_t        nfound=0;          /* number of differences found */
    unsigned long  temp1_ulong;
    unsigned long  temp2_ulong;
    hsize_t        i;


    /* -d and !-p */
    if (options->d && !options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
		
		for ( i = 0; i < nelmts; i++)
		{
			memcpy(&temp1_ulong, mem1, sizeof(unsigned long));
			memcpy(&temp2_ulong, mem2, sizeof(unsigned long));
			
			if (labs((long)(temp1_ulong-temp2_ulong)) > (long)options->delta)
			{
				if ( print_data(options) ) 
				{
					print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
					parallel_print(SPACES);
					parallel_print(LIFORMAT,temp1_ulong,temp2_ulong,labs(temp1_ulong-temp2_ulong));
				}
				nfound++;
			}
				mem1+=sizeof(unsigned long);
  mem2+=sizeof(unsigned long);
			if (options->n && nfound>=options->count)
					return nfound;
		}
	
	}

    }

    /* !-d and -p */
    else if (!options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_ulong, mem1, sizeof(unsigned long));
	    memcpy(&temp2_ulong, mem2, sizeof(unsigned long));

	    if ( temp1_ulong!=0 && labs(1-temp2_ulong/temp1_ulong) > options->percent )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(LPIFORMAT,temp1_ulong,temp2_ulong,labs(temp1_ulong-temp2_ulong),
			    labs(1-temp2_ulong/temp1_ulong));
		}
		nfound++;
	    }
	    mem1+=sizeof(unsigned long);
	    mem2+=sizeof(unsigned long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}


    } 

    /* -d and -p */
    else if ( options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_ulong, mem1, sizeof(unsigned long));
	    memcpy(&temp2_ulong, mem2, sizeof(unsigned long));

	    if ( temp1_ulong!=0 && labs(1-temp2_ulong/temp1_ulong) > options->percent && 
		    labs(temp1_ulong-temp2_ulong) > options->delta )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(LPIFORMAT,temp1_ulong,temp2_ulong,labs(temp1_ulong-temp2_ulong),
			    labs(1-temp2_ulong/temp1_ulong));
		}
		nfound++;
	    }
	    mem1+=sizeof(unsigned long);
	    mem2+=sizeof(unsigned long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    } 
    else 
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_ulong, mem1, sizeof(unsigned long));
	    memcpy(&temp2_ulong, mem2, sizeof(unsigned long));

	    if (temp1_ulong != temp2_ulong)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(LIFORMAT,temp1_ulong,temp2_ulong,labs(temp1_ulong-temp2_ulong));
		}
		nfound++;
	    }

	    mem1+=sizeof(unsigned long);
	    mem2+=sizeof(unsigned long);
	    if (options->n && nfound>=options->count)
		return nfound;
	} /* nelmts */

    }

    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_llong
 *
 * Purpose: diff a H5T_NATIVE_LLONG type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_llong(unsigned char *mem1,
															  		unsigned char *mem2,
																  	hsize_t       nelmts,
																	  int           rank, 
																	  hsize_t       *acc,  
																	  hsize_t       *pos,
																	  diff_opt_t    *options, 
																	  const char    *obj1, 
																	  const char    *obj2,
																	  int           *ph)
																			
{
    hsize_t       nfound=0;          /* number of differences found */
    long_long     temp1_llong;
    long_long     temp2_llong;
    hsize_t       i;
    static char   fmt_llong[255];
    static char   fmt_llongp[255];

    if (!fmt_llong[0]) {
	/* build default formats for long long types */
	sprintf(fmt_llong,  "%%%sd              %%%sd               %%%sd\n", 
		H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
	sprintf(fmt_llongp,  "%%%sd             %%%sd               %%%sd               %%%sd\n", 
		H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
    }


    /* -d and !-p */
    if (options->d && !options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_llong, mem1, sizeof(long_long));
	    memcpy(&temp2_llong, mem2, sizeof(long_long));

	    if (labs( (long) (temp1_llong-temp2_llong)) > options->delta)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_llong,temp1_llong,temp2_llong,labs((long)(temp1_llong-temp2_llong)));
		}
		nfound++;
	    }
	    mem1+=sizeof(long_long);
	    mem2+=sizeof(long_long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    }

    /* !-d and -p */
    else if (!options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_llong, mem1, sizeof(long_long));
	    memcpy(&temp2_llong, mem2, sizeof(long_long));

	    if ( temp1_llong!=0 && labs((long)(1-temp2_llong/temp1_llong)) > options->percent )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_llongp,temp1_llong,temp2_llong,labs((long)(temp1_llong-temp2_llong)),
			    labs((long)(1-temp2_llong/temp1_llong)));
		}
		nfound++;
	    }
	    mem1+=sizeof(long_long);
	    mem2+=sizeof(long_long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}


    } 

    /* -d and -p */
    else if ( options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_llong, mem1, sizeof(long_long));
	    memcpy(&temp2_llong, mem2, sizeof(long_long));

	    if ( temp1_llong!=0 && labs((long)(1-temp2_llong/temp1_llong)) > options->percent && 
		    labs((long)(temp1_llong-temp2_llong)) > options->delta )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_llongp,temp1_llong,temp2_llong,labs((long)(temp1_llong-temp2_llong)),
			    labs((long)(1-temp2_llong/temp1_llong)));
		}
		nfound++;
	    }
	    mem1+=sizeof(long_long);
	    mem2+=sizeof(long_long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    } 
    else 
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_llong, mem1, sizeof(long_long));
	    memcpy(&temp2_llong, mem2, sizeof(long_long));

	    if (temp1_llong != temp2_llong)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_llong,temp1_llong,temp2_llong,labs((long)(temp1_llong-temp2_llong)));
		}
		nfound++;
	    }

	    mem1+=sizeof(long_long);
	    mem2+=sizeof(long_long);
	    if (options->n && nfound>=options->count)
		return nfound;
	} /* nelmts */

    }

    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_ullong
 *
 * Purpose: diff a H5T_NATIVE_ULLONG type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_ullong(unsigned char *mem1,
															  		 unsigned char *mem2,
															   		hsize_t       nelmts,
																		  int           rank, 
																		  hsize_t       *acc,  
																		  hsize_t       *pos,
																		  diff_opt_t    *options, 
																		  const char    *obj1, 
																		  const char    *obj2,
																		  int           *ph)

{
    hsize_t             nfound=0;          /* number of differences found */
    unsigned long_long  temp1_ullong;
    unsigned long_long  temp2_ullong;
    hsize_t             i;
    static char         fmt_ullong[255];
    static char         fmt_ullongp[255];

    if (!fmt_ullong[0]) {
	/* build default formats for long long types */
	sprintf(fmt_ullong, "%%%su              %%%su               %%%su\n", 
		H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
	sprintf(fmt_ullongp, "%%%su             %%%su               %%%su               %%%su\n", 
		H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
    }


    /* -d and !-p */
    if (options->d && !options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_ullong, mem1, sizeof(unsigned long_long));
	    memcpy(&temp2_ullong, mem2, sizeof(unsigned long_long));

	    if (labs((long)(temp1_ullong-temp2_ullong)) > options->delta)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_ullong,temp1_ullong,temp2_ullong,labs((long)(temp1_ullong-temp2_ullong)));
		}
		nfound++;
	    }
	    mem1+=sizeof(unsigned long_long);
	    mem2+=sizeof(unsigned long_long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    }

    /* !-d and -p */
    else if (!options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_ullong, mem1, sizeof(unsigned long_long));
	    memcpy(&temp2_ullong, mem2, sizeof(unsigned long_long));

	    if ( temp1_ullong!=0 && labs((long)(1-temp2_ullong/temp1_ullong)) > options->percent )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_ullongp,temp1_ullong,temp2_ullong,labs((long)(temp1_ullong-temp2_ullong)),
			    labs((long)(1-temp2_ullong/temp1_ullong)));
		}
		nfound++;
	    }
	    mem1+=sizeof(unsigned long_long);
	    mem2+=sizeof(unsigned long_long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}


    } 

    /* -d and -p */
    else if ( options->d && options->p)
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_ullong, mem1, sizeof(unsigned long_long));
	    memcpy(&temp2_ullong, mem2, sizeof(unsigned long_long));

	    if ( temp1_ullong!=0 && labs((long)(1-temp2_ullong/temp1_ullong)) > options->percent && 
		    labs((long)(temp1_ullong-temp2_ullong)) > options->delta )
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,1,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_ullongp,temp1_ullong,temp2_ullong,labs((long)(temp1_ullong-temp2_ullong)),
			    labs((long)(1-temp2_ullong/temp1_ullong)));
		}
		nfound++;
	    }
	    mem1+=sizeof(unsigned long_long);
	    mem2+=sizeof(unsigned long_long);
	    if (options->n && nfound>=options->count)
		return nfound;
	}

    } 
    else 
    {

	for ( i = 0; i < nelmts; i++)
	{
	    memcpy(&temp1_ullong, mem1, sizeof(unsigned long_long));
	    memcpy(&temp2_ullong, mem2, sizeof(unsigned long_long));

	    if (temp1_ullong != temp2_ullong)
	    {
		if ( print_data(options) ) 
		{
		    print_pos(ph,0,i,acc,pos,rank,obj1,obj2);
		    parallel_print(SPACES);
		    parallel_print(fmt_ullong,temp1_ullong,temp2_ullong,labs((long)(temp1_ullong-temp2_ullong)));
		}
		nfound++;
	    }

	    mem1+=sizeof(unsigned long_long);
	    mem2+=sizeof(unsigned long_long);
	    if (options->n && nfound>=options->count)
		return nfound;
	} /* nelmts */

    }

    return nfound;
}

