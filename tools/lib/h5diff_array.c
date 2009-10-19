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

#include <math.h>
#include "h5diff.h"
#include "H5private.h"

#include <sys/timeb.h>
#include <time.h>


/*-------------------------------------------------------------------------
 * printf formatting
 *-------------------------------------------------------------------------
 */

#define F_FORMAT      "%-15g %-15g %-15g\n"

#if H5_SIZEOF_LONG_DOUBLE !=0
#define LD_FORMAT     "%-15Lf %-15Lf %-15Lf\n"
#endif

#define I_FORMAT      "%-15d %-15d %-15d\n"
#define C_FORMAT      "%-16c %-17c\n"
#define S_FORMAT      "%-16s %-17s\n"
#define UI_FORMAT     "%-15u %-15u %-15u\n"
#define LI_FORMAT     "%-15ld %-15ld %-15ld\n"
#define ULI_FORMAT    "%-15lu %-15lu %-15lu\n"
#define LLI_FORMAT    "%-15"H5_PRINTF_LL_WIDTH"d %-15"H5_PRINTF_LL_WIDTH"d %-15"H5_PRINTF_LL_WIDTH"d\n"
#define ULLI_FORMAT   "%-15"H5_PRINTF_LL_WIDTH"u %-15"H5_PRINTF_LL_WIDTH"u %-15"H5_PRINTF_LL_WIDTH"u\n"

/* with -p option */
#define F_FORMAT_P    "%-15.10g %-15.10g %-15.10g %-14.10g\n"

#if H5_SIZEOF_LONG_DOUBLE !=0
#define LD_FORMAT_P    "%-15.10Lf %-15.10Lf %-15.10Lf %-14.10Lf\n"
#endif

#define I_FORMAT_P    "%-15d %-15d %-15d %-14f\n"
#define UI_FORMAT_P   "%-15u %-15u %-15u %-14f\n"
#define LI_FORMAT_P   "%-15ld %-15ld %-15ld %-14f\n"
#define ULI_FORMAT_P  "%-15lu %-15lu %-15lu %-14f\n"
#define LLI_FORMAT_P  "%-15"H5_PRINTF_LL_WIDTH"d %-15"H5_PRINTF_LL_WIDTH"d %-15"H5_PRINTF_LL_WIDTH"d %-14f\n"
#define ULLI_FORMAT_P "%-15"H5_PRINTF_LL_WIDTH"u %-15"H5_PRINTF_LL_WIDTH"u %-15"H5_PRINTF_LL_WIDTH"d %-14f\n"
#define SPACES        "          "

/* not comparable */
#define F_FORMAT_P_NOTCOMP  "%-15.10g %-15.10g %-15.10g not comparable\n"


#if H5_SIZEOF_LONG_DOUBLE !=0
#define LD_FORMAT_P_NOTCOMP  "%-15.10Lf %-15.10Lf %-15.10Lf not comparable\n"
#endif

#define I_FORMAT_P_NOTCOMP  "%-15d %-15d %-15d not comparable\n"
#define UI_FORMAT_P_NOTCOMP   "%-15u %-15u %-15u not comparable\n"
#define LI_FORMAT_P_NOTCOMP   "%-15ld %-15ld %-15ld not comparable\n"
#define ULI_FORMAT_P_NOTCOMP  "%-15lu %-15lu %-15lu not comparable\n"
#define LLI_FORMAT_P_NOTCOMP  "%-15"H5_PRINTF_LL_WIDTH"d %-15"H5_PRINTF_LL_WIDTH"d %-15"H5_PRINTF_LL_WIDTH"d not comparable\n"
#define ULLI_FORMAT_P_NOTCOMP "%-15"H5_PRINTF_LL_WIDTH"u %-15"H5_PRINTF_LL_WIDTH"u %-15"H5_PRINTF_LL_WIDTH"d not comparable\n"


/* if system EPSILON is defined, use the system EPSILON; otherwise, use
   constants that are close to most EPSILON values */

#ifndef FLT_EPSILON
#define FLT_EPSILON 1.19209E-07
#endif

#ifndef DBL_EPSILON
#define DBL_EPSILON 2.22045E-16
#endif


/*-------------------------------------------------------------------------
 * -p relative error formula
 *
 * We assume the true value of a quantity to be A (value in first dataset) 
 *  and the measured or inferred value to be B (value in second dataset). 
 *  The relative error is defined by 
 *
 *  B - A
 * --------
 *    A
 *
 *-------------------------------------------------------------------------
 */

static int   not_comparable;

#define PER(A,B) { per=-1;                           \
    not_comparable=0;                                \
    both_zero=0;                                     \
    if (A==0 && B==0)                                \
    both_zero=1;                                     \
    if (A!=0)                                        \
    per = (double)ABS( ( double)(B-A) / (double)A ); \
    else                                             \
    not_comparable=1;                                \
}


#define PER_UNSIGN(TYPE,A,B) { per=-1;               \
    not_comparable=0;                                \
    both_zero=0;                                     \
    if (A==0 && B==0)                                \
    both_zero=1;                                     \
    if (A!=0)                                        \
    per = ABS((double)((TYPE)(B-A)) / (double)A) ;   \
    else                                             \
    not_comparable=1;                                \
}



#define BOTH_ZERO(A,B) { both_zero=0;                \
    if (A==0 && B==0)                                \
    both_zero=1;                                     \
}

#define IS_ZERO(A) { is_zero=0;                      \
    if (A==0)                                        \
    is_zero=1;                                       \
}

#define PDIFF(a,b)      ( (b>a) ? (b-a) : (a-b))

#define NOT_SUPPORTED() { printf("Warning: feature not supported\n"); }


/*-------------------------------------------------------------------------
 * local prototypes
 *-------------------------------------------------------------------------
 */

static void    close_obj(H5G_obj_t1 obj_type, hid_t obj_id);
static hsize_t diff_region(hid_t obj1_id, hid_t obj2_id,hid_t region1_id, hid_t region2_id, diff_opt_t *options);
static hbool_t all_zero(const void *_mem, size_t size);
static hsize_t character_compare(unsigned char *mem1,unsigned char *mem2,hsize_t i,unsigned u,int rank,hsize_t *dims,hsize_t *acc,hsize_t *pos,diff_opt_t *options,const char *obj1,const char *obj2,int *ph);
static hsize_t character_compare_opt(unsigned char *mem1,unsigned char *mem2,hsize_t i,int rank,hsize_t *dims,hsize_t *acc,hsize_t *pos,diff_opt_t *options,const char *obj1,const char *obj2,int *ph);
static hbool_t equal_float(float value, float expected, diff_opt_t *options);
static hbool_t equal_double(double value, double expected, diff_opt_t *options);
#if H5_SIZEOF_LONG_DOUBLE !=0
static hbool_t equal_ldouble(long double value, long double expected, diff_opt_t *options);
#endif
static int print_data(diff_opt_t *options);
static void print_pos(int *ph,int pp,hsize_t curr_pos,hsize_t *acc,hsize_t *pos,int rank,hsize_t *dims,const char *obj1,const char *obj2 );
static void print_char_pos(int *ph,int pp,hsize_t curr_pos,unsigned u,hsize_t *acc,hsize_t *pos,int rank,hsize_t *dims,const char *obj1,const char *obj2 );
static void h5diff_print_char(char ch);


/*-------------------------------------------------------------------------
 * NaN detection
 *-------------------------------------------------------------------------
 */
#if H5_SIZEOF_LONG_DOUBLE !=0
typedef enum dtype_t
{
    FLT_FLOAT, 
    FLT_DOUBLE,
    FLT_LDOUBLE
} dtype_t;
#else

typedef enum dtype_t
{
    FLT_FLOAT, 
    FLT_DOUBLE
} dtype_t;
#endif

static int my_isnan(dtype_t type, void *val);



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
                    hsize_t hyper_start,
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
    hsize_t       acc[32];    /* accumulator position */
    hsize_t       pos[32];    /* matrix position */
    int           ph=1;       /* print header  */
    hsize_t       i;
    int           j;
    
    
    /* get the size. */
    size = H5Tget_size( m_type );

    if ( rank > 0 )
    {
        
        acc[rank-1]=1;
        for(j=(rank-2); j>=0; j--)
        {
            acc[j]=acc[j+1]*dims[j+1];
        }
        for ( j = 0; j < rank; j++)
            pos[j]=0;
        
    }
    
    if(H5Tis_variable_str(m_type))
    {
        for ( i = 0; i < nelmts; i++)
        {
            nfound+=diff_datum(
                ((unsigned char**)mem1)[(size_t)i],
                ((unsigned char**)mem2)[(size_t)i],
                m_type,
                i,
                rank,
                dims,
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
    }
    
    else
    {
        switch (H5Tget_class(m_type))
        {
        default:
            assert(0);
            break;
            
       /*-------------------------------------------------------------------------
        * float and integer atomic types
        *-------------------------------------------------------------------------
        */
            
        case H5T_FLOAT:
            
            if (H5Tequal(m_type, H5T_NATIVE_FLOAT))
                nfound=diff_float(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_DOUBLE))
                nfound=diff_double(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);

#if H5_SIZEOF_LONG_DOUBLE !=0
            else if (H5Tequal(m_type, H5T_NATIVE_LDOUBLE))
                nfound=diff_ldouble(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
#endif

            break;
            
        case H5T_INTEGER:
            
            if (H5Tequal(m_type, H5T_NATIVE_SCHAR))
                nfound=diff_schar(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_UCHAR))
                nfound=diff_uchar(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_SHORT))
                nfound=diff_short(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_USHORT))
                nfound=diff_ushort(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_INT))
                nfound=diff_int(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_UINT))
                nfound=diff_uint(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_LONG))
                nfound=diff_long(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_ULONG))
                nfound=diff_ulong(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_LLONG))
                nfound=diff_llong(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            else if (H5Tequal(m_type, H5T_NATIVE_ULLONG))
                nfound=diff_ullong(mem1,mem2,nelmts,hyper_start,rank,dims,acc,pos,options,name1,name2,&ph);
            
            break;
            
        /*-------------------------------------------------------------------------
         * Other types than float and integer
         *-------------------------------------------------------------------------
         */
            
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
                    dims,
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
 * Purpose: compare the values pointed to in _MEM1 and _MEM2 of type M_TYPE
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 29, 2003
 *
 * The comparison of the 2 buffers read from the files is made datum by datum.
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
                   hsize_t    *dims,
                   hsize_t    *acc,
                   hsize_t    *pos,
                   diff_opt_t *options,
                   const char *obj1,
                   const char *obj2,
                   hid_t      container1_id,
                   hid_t      container2_id, /*where the reference came from*/
                   int        *ph)           /*print header */
{
 unsigned char *mem1 = (unsigned char*)_mem1;
 unsigned char *mem2 = (unsigned char*)_mem2;
 unsigned      u;
 hid_t         memb_type;
 size_t        type_size;
 size_t        offset;
 int           nmembs;
 int           j;
 hsize_t       nelmts;
 size_t        size=0;
 int           iszero1;
 int           iszero2;
 hsize_t       nfound=0;   /* differences found */
 int           ret=0;      /* check return error */
 double        per;
 int           both_zero;

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
    dims,
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
            
            /* Get pointer to first string to compare */
            s = (char *)mem1;

            /* check for NULL pointer for string */
            if(s!=NULL)
            {
                if(H5Tis_variable_str(m_type))
                    size = HDstrlen(s);
                else
                    size = H5Tget_size(m_type);
                
                pad = H5Tget_strpad(m_type);
            
                for (u=0; u<size && (s[u] || pad!=H5T_STR_NULLTERM); u++)
                    nfound+=character_compare(
                        mem1 + u,
                        mem2 + u, /* offset */
                        i,        /* index position */
                        u,        /* string character position */
                        rank,
                        dims,
                        acc,
                        pos,
                        options,
                        obj1,
                        obj2,
                        ph);
            }
                
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
    nfound+=character_compare_opt(
    mem1 + u,
    mem2 + u, /* offset */
    i,        /* index position */
    rank,
    dims,
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
   nfound+=character_compare_opt(
   mem1 + u,
   mem2 + u, /* offset */
   i,        /* index position */
   rank,
   dims,
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
    enumeration constant and do a string comparison
  */

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
       print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
       printf(SPACES);
       printf(S_FORMAT,enum_name1,enum_name2);
      }
     }
     else
     {
      for (u=0; u<type_size; u++)
       nfound+=character_compare_opt(
       mem1 + u,
       mem2 + u, /* offset */
       i,        /* index position */
       rank,
       dims,
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
     {
         hsize_t adims[H5S_MAX_RANK];
         hsize_t       ndims;
         /* get the array's base datatype for each element */
         memb_type = H5Tget_super(m_type);
         size      = H5Tget_size(memb_type);
         ndims     = H5Tget_array_ndims(m_type);
         H5Tget_array_dims(m_type, adims, NULL);
         assert(ndims >= 1 && ndims <= H5S_MAX_RANK);
         
         /* calculate the number of array elements */
         for (u = 0, nelmts = 1; u <ndims; u++)
             nelmts *= adims[u];
         for (u = 0; u < nelmts; u++)
             nfound+=diff_datum(
             mem1 + u * size,
             mem2 + u * size, /* offset */
             memb_type,
             i,               /* index position */
             rank,
             dims,
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
  * H5T_REFERENCE
  *-------------------------------------------------------------------------
  */


 case H5T_REFERENCE:

  iszero1=all_zero(_mem1, H5Tget_size(m_type));
  iszero2=all_zero(_mem2, H5Tget_size(m_type));
  if (iszero1 != iszero2)
  {
   return 1;
  }
  else if (!iszero1 && !iszero2)
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
    hid_t  obj1_id;
    hid_t  obj2_id;

    if ((obj1_id = H5Rdereference(container1_id, H5R_DATASET_REGION, _mem1))<0)
     ret= -1;
    if ((obj2_id = H5Rdereference(container2_id, H5R_DATASET_REGION, _mem2))<0)
     ret= -1;
    if ((region1_id = H5Rget_region(container1_id, H5R_DATASET_REGION, _mem1))<0)
     ret= -1;
    if ((region2_id = H5Rget_region(container2_id, H5R_DATASET_REGION, _mem2))<0)
     ret= -1;

    if (ret==-1) {
     options->err_stat=1;
     return 0;
    }

    nfound = diff_region(obj1_id,obj2_id,region1_id,region2_id,options);

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
    H5G_obj_t1  obj1_type;
    H5G_obj_t1  obj2_type;
    hid_t      obj1_id;
    hid_t      obj2_id;

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
     printf("Different object types referenced: <%s> and <%s>", obj1, obj2);
     options->not_cmp=1;
     return 0;
    }

    if ((obj1_id = H5Rdereference(container1_id, H5R_OBJECT, _mem1))<0)
     ret= -1;
    if ((obj2_id = H5Rdereference(container2_id, H5R_OBJECT, _mem2))<0)
     ret= -1;
    if (ret==-1) {
     options->err_stat=1;
     return 0;
    }

    /* compare */
    switch (obj1_type) {
    case H5G_DATASET:
     nfound=diff_datasetid(obj1_id,
      obj2_id,
      NULL,
      NULL,
      options);
     break;
    default:
     printf("Warning: Comparison not possible of object types referenced: <%s> and <%s>\n",
      obj1, obj2);
     options->not_cmp=1;
     break;
    }

    close_obj(obj1_type,obj1_id);
    close_obj(obj2_type,obj2_id);

   }/*object reference*/

  }/*is zero*/


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
   dims,
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
  * H5T_INTEGER
  *-------------------------------------------------------------------------
  */        
  
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
          if (ABS(temp1_char-temp2_char) > options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT,temp1_char,temp2_char,ABS(temp1_char-temp2_char));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER(temp1_char,temp2_char);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_char,temp2_char,ABS(temp1_char-temp2_char));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_char,temp2_char,ABS(temp1_char-temp2_char),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER(temp1_char,temp2_char);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_char,temp2_char,ABS(temp1_char-temp2_char));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent && ABS(temp1_char-temp2_char) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_char,temp2_char,ABS(temp1_char-temp2_char),per);
                  }
                  nfound++;
              }
      }
      else if (temp1_char != temp2_char)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(I_FORMAT,temp1_char,temp2_char,ABS(temp1_char-temp2_char));
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
          if ( PDIFF(temp1_uchar,temp2_uchar) > options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER_UNSIGN(signed char,temp1_uchar,temp2_uchar);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER_UNSIGN(signed char,temp1_uchar,temp2_uchar);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent && PDIFF(temp1_uchar,temp2_uchar) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar),per);
                  }
                  nfound++;
              }
      }
      else if (temp1_uchar != temp2_uchar)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(I_FORMAT,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar));
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
          if (ABS(temp1_short-temp2_short) > options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT,temp1_short,temp2_short,ABS(temp1_short-temp2_short));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER(temp1_short,temp2_short);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_short,temp2_short,ABS(temp1_short-temp2_short));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_short,temp2_short,ABS(temp1_short-temp2_short),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER(temp1_short,temp2_short);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_short,temp2_short,ABS(temp1_short-temp2_short));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent && ABS(temp1_short-temp2_short) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_short,temp2_short,ABS(temp1_short-temp2_short),per);
                  }
                  nfound++;
              }
      }
      else if (temp1_short != temp2_short)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(I_FORMAT,temp1_short,temp2_short,ABS(temp1_short-temp2_short));
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
          if ( PDIFF(temp1_ushort,temp2_ushort) > options->delta)
          {
              
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT,temp1_ushort,temp2_ushort,PDIFF(temp1_ushort,temp2_ushort));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER_UNSIGN(signed short,temp1_ushort,temp2_ushort);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_ushort,temp2_ushort,
                      PDIFF(temp1_ushort,temp2_ushort));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_ushort,temp2_ushort,PDIFF(temp1_ushort,temp2_ushort),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER_UNSIGN(signed short,temp1_ushort,temp2_ushort);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_ushort,temp2_ushort,PDIFF(temp1_ushort,temp2_ushort));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent && PDIFF(temp1_ushort,temp2_ushort) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_ushort,temp2_ushort,PDIFF(temp1_ushort,temp2_ushort),per);
                  }
                  nfound++;
              }
      }
      else if (temp1_ushort != temp2_ushort)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(I_FORMAT,temp1_ushort,temp2_ushort,PDIFF(temp1_ushort,temp2_ushort));
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
          if (ABS(temp1_int-temp2_int) > options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT,temp1_int,temp2_int,ABS(temp1_int-temp2_int));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER(temp1_int,temp2_int);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_int,temp2_int,ABS(temp1_int-temp2_int));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_int,temp2_int,ABS(temp1_int-temp2_int),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER(temp1_int,temp2_int);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_int,temp2_int,ABS(temp1_int-temp2_int));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent && ABS(temp1_int-temp2_int) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_int,temp2_int,ABS(temp1_int-temp2_int),per);
                  }
                  nfound++;
              }
      }
      else if (temp1_int != temp2_int)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(I_FORMAT,temp1_int,temp2_int,ABS(temp1_int-temp2_int));
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
          if ( PDIFF(temp1_uint,temp2_uint) > options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(UI_FORMAT,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER_UNSIGN(signed int,temp1_uint,temp2_uint);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER_UNSIGN(signed int,temp1_uint,temp2_uint);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(I_FORMAT_P_NOTCOMP,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent && PDIFF(temp1_uint,temp2_uint) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(I_FORMAT_P,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint),per);
                  }
                  nfound++;
              }
      }
      else if (temp1_uint != temp2_uint)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(UI_FORMAT,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint));
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
          if (ABS(temp1_long-temp2_long) > options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(LI_FORMAT,temp1_long,temp2_long,ABS(temp1_long-temp2_long));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER(temp1_long,temp2_long);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(LI_FORMAT_P_NOTCOMP,temp1_long,temp2_long,
                      ABS(temp1_long-temp2_long));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(LI_FORMAT_P,temp1_long,temp2_long,ABS(temp1_long-temp2_long),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER(temp1_long,temp2_long);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(LI_FORMAT_P_NOTCOMP,temp1_long,temp2_long,ABS(temp1_long-temp2_long));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent && ABS(temp1_long-temp2_long) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(LI_FORMAT_P,temp1_long,temp2_long,
                          ABS(temp1_long-temp2_long),
                          per);
                  }
                  nfound++;
              }
      }
      else if (temp1_long != temp2_long)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(LI_FORMAT,temp1_long,temp2_long,ABS(temp1_long-temp2_long));
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
          if ( PDIFF(temp1_ulong,temp2_ulong) > options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(ULI_FORMAT,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER_UNSIGN(signed long,temp1_ulong,temp2_ulong);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(ULI_FORMAT_P_NOTCOMP,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(ULI_FORMAT_P,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER_UNSIGN(signed long,temp1_ulong,temp2_ulong);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(ULI_FORMAT_P_NOTCOMP,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent && PDIFF(temp1_ulong,temp2_ulong) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(ULI_FORMAT_P,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong),per);
                  }
                  nfound++;
              }
      }
      else if (temp1_ulong != temp2_ulong)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(ULI_FORMAT,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong));
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
          if (ABS(temp1_llong-temp2_llong) > options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(LLI_FORMAT,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          PER(temp1_llong,temp2_llong);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(LLI_FORMAT_P_NOTCOMP,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(LLI_FORMAT_P,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong),per);
                  }
                  nfound++;
              }
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          PER(temp1_llong,temp2_llong);
          
          if (not_comparable && !both_zero) /* not comparable */
          {
              if ( print_data(options) )
              {
                  print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(LLI_FORMAT_P_NOTCOMP,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong));
              }
              options->not_cmp=1;
              nfound++;
          }
          
          else
              
              if ( per > options->percent  && ABS(temp1_llong-temp2_llong) > options->delta )
              {
                  if ( print_data(options) )
                  {
                      print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                      printf(SPACES);
                      printf(LLI_FORMAT_P,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong),per);
                  }
                  nfound++;
              }
      }
      else if (temp1_llong != temp2_llong)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(LLI_FORMAT,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong));
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
          if ( PDIFF(temp1_ullong,temp2_ullong) > (unsigned long_long)options->delta)
          {
              if ( print_data(options) )
              {
                  print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                  printf(SPACES);
                  printf(ULLI_FORMAT,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong));
              }
              nfound++;
          }
      }
      /* !-d and -p */
      else if (!options->d && options->p)
      {
          if ( print_data(options) )
          {
              NOT_SUPPORTED();
          }
          options->not_cmp=1;
          return nfound;
      }
      /* -d and -p */
      else if ( options->d && options->p)
      {
          if ( print_data(options) )
          {
              NOT_SUPPORTED();
          }
          options->not_cmp=1;
          return nfound;
      }
      else if (temp1_ullong != temp2_ullong)
      {
          if ( print_data(options) )
          {
              print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
              printf(SPACES);
              printf(ULLI_FORMAT,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong));
          }
          nfound++;
      }
      
  } /*H5T_NATIVE_ULLONG*/


  break; /* H5T_INTEGER class */

     
   /*-------------------------------------------------------------------------
    * H5T_FLOAT
    *-------------------------------------------------------------------------
    */
        
    case H5T_FLOAT:
        
        
        
       /*-------------------------------------------------------------------------
        * H5T_NATIVE_FLOAT
        *-------------------------------------------------------------------------
        */
        if (H5Tequal(m_type, H5T_NATIVE_FLOAT))
        {
            float temp1_float;
            float temp2_float;
            int   isnan1=0;
            int   isnan2=0;

            assert(type_size==sizeof(float));

            memcpy(&temp1_float, mem1, sizeof(float));
            memcpy(&temp2_float, mem2, sizeof(float));

            /* logic for detecting NaNs is different with options -d, -p and no options */  

           /*-------------------------------------------------------------------------
            * -d and !-p
            *-------------------------------------------------------------------------
            */
            if (options->d && !options->p)
            {
                
                
               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_FLOAT,&temp1_float);
                    isnan2 = my_isnan(FLT_FLOAT,&temp2_float);
                }
                
                if ( !isnan1 && !isnan2)
                {             
                                    
                    if (ABS(temp1_float-temp2_float) > options->delta)
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT,temp1_float,temp2_float,ABS(temp1_float-temp2_float));
                        }
                        nfound++;
                    }
                    
                } /* NaN */
            }
           /*-------------------------------------------------------------------------
            * !-d and -p
            *-------------------------------------------------------------------------
            */
            else if (!options->d && options->p)
            {
                
               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_FLOAT,&temp1_float);
                    isnan2 = my_isnan(FLT_FLOAT,&temp2_float);
                }
                
                if ( !isnan1 && !isnan2)
                {   
                    
                    PER(temp1_float,temp2_float);
                    
                    if (not_comparable && !both_zero) /* not comparable */
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT_P_NOTCOMP,temp1_float,temp2_float,ABS(temp1_float-temp2_float));
                        }
                        options->not_cmp=1;
                        nfound++;
                    }
                    
                    else
                        
                        if ( per > options->percent && ABS(temp1_float-temp2_float) > options->delta )
                        {
                            if ( print_data(options) )
                            {
                                print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                                printf(SPACES);
                                printf(F_FORMAT_P,temp1_float,temp2_float,
                                    ABS(temp1_float-temp2_float),
                                    ABS(1-temp2_float/temp1_float));
                            }
                            nfound++;
                        }
                        
                } /* NaN */
            }

           /*-------------------------------------------------------------------------
            * -d and -p
            *-------------------------------------------------------------------------
            */
            
            else if ( options->d && options->p)
            {
                
               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_FLOAT,&temp1_float);
                    isnan2 = my_isnan(FLT_FLOAT,&temp2_float);
                }
                
                if ( !isnan1 && !isnan2)
                {   
                    
                    PER(temp1_float,temp2_float);
                    
                    if (not_comparable && !both_zero) /* not comparable */
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT_P_NOTCOMP,temp1_float,temp2_float,
                                ABS(temp1_float-temp2_float));
                        }
                        options->not_cmp=1;
                        nfound++;
                    }
                    
                    else
                        
                        if ( per > options->percent )
                        {
                            if ( print_data(options) )
                            {
                                print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                                printf(SPACES);
                                printf(F_FORMAT_P,temp1_float,temp2_float,
                                    ABS(temp1_float-temp2_float), 
                                    ABS(1-temp2_float/temp1_float));
                            }
                            nfound++;
                        }
                        
                } /* NaN */
            }
            
           /*-------------------------------------------------------------------------
            * no -d and -p
            *-------------------------------------------------------------------------
            */
            else if (equal_float(temp1_float,temp2_float,options)==FALSE)
            {
                
                if ( print_data(options) )
                {
                    print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                    printf(SPACES);
                    printf(F_FORMAT,temp1_float,temp2_float,ABS(temp1_float-temp2_float));
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
            double temp1_double;
            double temp2_double;
            int    isnan1=0;
            int    isnan2=0;


            assert(type_size==sizeof(double));
            
            memcpy(&temp1_double, mem1, sizeof(double));
            memcpy(&temp2_double, mem2, sizeof(double));

           /* logic for detecting NaNs is different with options -d, -p and no options */  

           /*-------------------------------------------------------------------------
            * -d and !-p
            *-------------------------------------------------------------------------
            */
            if (options->d && !options->p)
            {

               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */ 
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_DOUBLE,&temp1_double);
                    isnan2 = my_isnan(FLT_DOUBLE,&temp2_double);
                }
                if ( !isnan1 && !isnan2)
                {           
                    
                    if (ABS(temp1_double-temp2_double) > options->delta)
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT,temp1_double,temp2_double,ABS(temp1_double-temp2_double));
                        }
                        nfound++;
                    }
                    
                } /* NaN */
            }

           /*-------------------------------------------------------------------------
            * !-d and -p
            *-------------------------------------------------------------------------
            */
            else if (!options->d && options->p)
            {

               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_DOUBLE,&temp1_double);
                    isnan2 = my_isnan(FLT_DOUBLE,&temp2_double);
                }
                
                if ( !isnan1 && !isnan2)
                {           
                    
                    PER(temp1_double,temp2_double);
                    
                    if (not_comparable && !both_zero) /* not comparable */
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT_P_NOTCOMP,temp1_double,temp2_double,
                                ABS(temp1_double-temp2_double));
                        }
                        options->not_cmp=1;
                        nfound++;
                    }
                    
                    else
                        
                        if ( per > options->percent )
                        {
                            if ( print_data(options) )
                            {
                                print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                                printf(SPACES);
                                printf(F_FORMAT_P,temp1_double,temp2_double,
                                    ABS(temp1_double-temp2_double),
                                    ABS(1-temp2_double/temp1_double));
                            }
                            nfound++;
                        }
                        
                } /* NaN */
            }
            
           /*-------------------------------------------------------------------------
            * -d and -p 
            *-------------------------------------------------------------------------
            */
            else if ( options->d && options->p)
            {

               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_DOUBLE,&temp1_double);
                    isnan2 = my_isnan(FLT_DOUBLE,&temp2_double);
                }
                
                if ( !isnan1 && !isnan2)
                {           
                    
                    PER(temp1_double,temp2_double);
                    
                    if (not_comparable && !both_zero) /* not comparable */
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT_P_NOTCOMP,temp1_double,temp2_double,
                                ABS(temp1_double-temp2_double));
                        }
                        options->not_cmp=1;
                        nfound++;
                    }
                    
                    else
                        
                        if ( per > options->percent && 
                            ABS(temp1_double-temp2_double) > options->delta )
                        {
                            if ( print_data(options) )
                            {
                                print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                                printf(SPACES);
                                printf(F_FORMAT_P,temp1_double,temp2_double,
                                    ABS(temp1_double-temp2_double),
                                    ABS(1-temp2_double/temp1_double));
                            }
                            nfound++;
                        }
                        
                } /* NaN */
            }
            
           /*-------------------------------------------------------------------------
            * no -d and -p 
            *-------------------------------------------------------------------------
            */
            else if (equal_double(temp1_double,temp2_double,options)==FALSE)
            {
                if ( print_data(options) )
                {
                    print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                    printf(SPACES);
                    printf(F_FORMAT,temp1_double,temp2_double,ABS(temp1_double-temp2_double));
                }
                nfound++;
            }
    } /*H5T_NATIVE_DOUBLE*/


#if H5_SIZEOF_LONG_DOUBLE !=0


       /*-------------------------------------------------------------------------
        * H5T_NATIVE_LDOUBLE
        *-------------------------------------------------------------------------
        */

         else if (H5Tequal(m_type, H5T_NATIVE_LDOUBLE))
        {
            long double temp1_double;
            long double temp2_double;
            int    isnan1=0;
            int    isnan2=0;


            assert(type_size==sizeof(long  double));
            
            memcpy(&temp1_double, mem1, sizeof(long double));
            memcpy(&temp2_double, mem2, sizeof(long double));

           /* logic for detecting NaNs is different with options -d, -p and no options */  

           /*-------------------------------------------------------------------------
            * -d and !-p
            *-------------------------------------------------------------------------
            */
            if (options->d && !options->p)
            {

               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_LDOUBLE,&temp1_double);
                    isnan2 = my_isnan(FLT_LDOUBLE,&temp2_double);
                }
                
                if ( !isnan1 && !isnan2)
                {           
                    
                    if (ABS(temp1_double-temp2_double) > options->delta)
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(LD_FORMAT,temp1_double,temp2_double,ABS(temp1_double-temp2_double));
                        }
                        nfound++;
                    }
                    
                } /* NaN */
            }

           /*-------------------------------------------------------------------------
            * !-d and -p
            *-------------------------------------------------------------------------
            */
            else if (!options->d && options->p)
            {

               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_LDOUBLE,&temp1_double);
                    isnan2 = my_isnan(FLT_LDOUBLE,&temp2_double);
                }
                
                if ( !isnan1 && !isnan2)
                {           
                    
                    PER(temp1_double,temp2_double);
                    
                    if (not_comparable && !both_zero) /* not comparable */
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(LD_FORMAT_P_NOTCOMP,temp1_double,temp2_double,
                                ABS(temp1_double-temp2_double));
                        }
                        options->not_cmp=1;
                        nfound++;
                    }
                    
                    else
                        
                        if ( per > options->percent )
                        {
                            if ( print_data(options) )
                            {
                                print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                                printf(SPACES);
                                printf(LD_FORMAT_P,temp1_double,temp2_double,
                                    ABS(temp1_double-temp2_double),
                                    ABS(1-temp2_double/temp1_double));
                            }
                            nfound++;
                        }
                        
                } /* NaN */
            }
            
           /*-------------------------------------------------------------------------
            * -d and -p 
            *-------------------------------------------------------------------------
            */
            else if ( options->d && options->p)
            {

               /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if ( options->do_nans )
                {
                    isnan1 = my_isnan(FLT_LDOUBLE,&temp1_double);
                    isnan2 = my_isnan(FLT_LDOUBLE,&temp2_double);
                }
                
                if ( !isnan1 && !isnan2)
                {           
                    
                    PER(temp1_double,temp2_double);
                    
                    if (not_comparable && !both_zero) /* not comparable */
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(LD_FORMAT_P_NOTCOMP,temp1_double,temp2_double,
                                ABS(temp1_double-temp2_double));
                        }
                        options->not_cmp=1;
                        nfound++;
                    }
                    
                    else
                        
                        if ( per > options->percent && 
                            ABS(temp1_double-temp2_double) > options->delta )
                        {
                            if ( print_data(options) )
                            {
                                print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
                                printf(SPACES);
                                printf(LD_FORMAT_P,temp1_double,temp2_double,
                                    ABS(temp1_double-temp2_double),
                                    ABS(1-temp2_double/temp1_double));
                            }
                            nfound++;
                        }
                        
                } /* NaN */
            }
            
           /*-------------------------------------------------------------------------
            * no -d and -p 
            *-------------------------------------------------------------------------
            */
            else if (equal_ldouble(temp1_double,temp2_double,options)==FALSE)
            {
                if ( print_data(options) )
                {
                    print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
                    printf(SPACES);
                    printf(LD_FORMAT,temp1_double,temp2_double,ABS(temp1_double-temp2_double));
                }
                nfound++;
            }
    } /*H5T_NATIVE_LDOUBLE*/




#endif /* H5_SIZEOF_LONG_DOUBLE */ 
    
    
    break; /* H5T_FLOAT class */
    
 } /* switch */
 
 return nfound;
}

/*-------------------------------------------------------------------------
 * Function: all_zero
 *
 * Purpose: Determines if memory is initialized to all zero bytes.
 *
 * Return: TRUE if all bytes are zero; FALSE otherwise
 *
 *-------------------------------------------------------------------------
 */

static hbool_t all_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char *)_mem;

    while(size-- > 0)
        if(mem[size])
            return FALSE;

    return TRUE;
}

/*-------------------------------------------------------------------------
 * Function: close_obj
 *
 * Purpose: Auxialiary function to close an object
 *
 *-------------------------------------------------------------------------
 */

static void close_obj(H5G_obj_t1 obj_type, hid_t obj_id)
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
 * Function: print_region_block
 *
 * Purpose: print start coordinates and opposite corner of a region block
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

static
void print_region_block(int i, hsize_t *ptdata, int ndims)
{
 int j;

 printf("        ");
 for (j = 0; j < ndims; j++)
  printf("%s%lu", j ? "," : "   (",
  (unsigned long)ptdata[i * 2 * ndims + j]);
 for (j = 0; j < ndims; j++)
  printf("%s%lu", j ? "," : ")-(",
  (unsigned long)ptdata[i * 2 * ndims + j + ndims]);
 printf(")");

}


/*-------------------------------------------------------------------------
 * Function: print_points
 *
 * Purpose: print points of a region reference
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

static
void print_points(int i, hsize_t *ptdata, int ndims)
{
 int j;

 printf("              ");
 for (j = 0; j < ndims; j++)
  printf("%s%lu", j ? "," : "(",
  (unsigned long)(ptdata[i * ndims + j]));
 printf(")");

}

/*-------------------------------------------------------------------------
 * Function: diff_region
 *
 * Purpose: diff a dataspace region
 *
 * Return: number of differences
 *
 *-------------------------------------------------------------------------
 */

static
hsize_t diff_region(hid_t obj1_id,
                    hid_t obj2_id,
                    hid_t region1_id,
                    hid_t region2_id,
                    diff_opt_t *options)

{
 hssize_t   nblocks1, npoints1;
 hssize_t   nblocks2, npoints2;
 H5G_stat_t sb1;
 H5G_stat_t sb2;
 hsize_t    alloc_size;
 hsize_t    *ptdata1;
 hsize_t    *ptdata2;
 int        ndims1;
 int        ndims2;
 int        i, j;
 haddr_t    objno1, objno2; /* compact form of object's location */
 hsize_t    nfound_b=0;     /* block differences found */
 hsize_t    nfound_p=0;     /* point differences found */

 ndims1 = H5Sget_simple_extent_ndims(region1_id);
 ndims2 = H5Sget_simple_extent_ndims(region2_id);

 H5Gget_objinfo(obj1_id, ".", FALSE, &sb1);
 H5Gget_objinfo(obj2_id, ".", FALSE, &sb2);

 objno1 = (haddr_t)sb1.objno[0] | ((haddr_t)sb1.objno[1] << (8 * sizeof(long)));
 objno2 = (haddr_t)sb2.objno[0] | ((haddr_t)sb2.objno[1] << (8 * sizeof(long)));

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
 {
  options->not_cmp=1;
  return 0;
 }

/*-------------------------------------------------------------------------
 * compare block information
 *-------------------------------------------------------------------------
 */
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

  for (i = 0; i < nblocks1; i++)
  {
   /* start coordinates and opposite corner */
   for (j = 0; j < ndims1; j++)
   {
    hsize_t start1, start2, end1, end2;
    start1 = ptdata1[i * 2 * ndims1 + j];
    start2 = ptdata2[i * 2 * ndims1 + j];
    end1   = ptdata1[i * 2 * ndims1 + j + ndims1];
    end2   = ptdata2[i * 2 * ndims1 + j + ndims1];
    if (start1 != start2 || end1 != end2)
    {
     nfound_b++;
    }
   }
  }


  /* print differences if found */
  if (nfound_b && options->m_verbose)
  {
   printf("Referenced dataset      %lu            %lu\n",
    (unsigned long)objno1,(unsigned long)objno2);
   printf("------------------------------------------------------------\n");

   printf("Region blocks\n");
   for (i = 0; i < nblocks1; i++)
   {
    printf("block #%d", i);
    print_region_block(i, ptdata1, ndims1);
    print_region_block(i, ptdata2, ndims1);
    printf("\n");

   }
  }

  HDfree(ptdata1);
  HDfree(ptdata2);
 }

/*-------------------------------------------------------------------------
 * compare point information
 *-------------------------------------------------------------------------
 */

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

  for (i = 0; i < npoints1; i++)
  {
   hsize_t pt1, pt2;

   for (j = 0; j < ndims1; j++)
   {
    pt1 = ptdata1[i * ndims1 + j];
    pt2 = ptdata2[i * ndims1 + j];
    if (pt1 != pt2)
     nfound_p++;
   }
  }

  if (nfound_p && options->m_verbose)
  {
   printf("Region points\n");
   for (i = 0; i < npoints1; i++)
   {
    hsize_t pt1, pt2;
    int     diff_data = 0;
    for (j = 0; j < ndims1; j++)
    {
     pt1 = ptdata1[i * ndims1 + j];
     pt2 = ptdata2[i * ndims1 + j];
     if (pt1 != pt2)
     {
      diff_data = 1;
      break;
     }
    }
    if (diff_data)
    {
     printf("point #%d", i);
     print_points(i, ptdata1, ndims1);
     print_points(i, ptdata2, ndims1);
     printf("\n");
    }
   }
  }


#if defined (H5DIFF_DEBUG)
  for (i = 0; i < npoints1; i++)
  {
   int j;

   printf("%sPt%lu: " ,
    i ? "," : "",
    (unsigned long)i);

   for (j = 0; j < ndims1; j++)
    printf("%s%lu", j ? "," : "(",
    (unsigned long)(ptdata1[i * ndims1 + j]));

   printf(")");
  }
#endif

  HDfree(ptdata1);
  HDfree(ptdata2);
 }

 nfound_b = nfound_b/ndims1;
 nfound_p = nfound_p/ndims1;
 return (nfound_p + nfound_b);
}


/*-------------------------------------------------------------------------
 * Function: character_compare
 *
 * Purpose: do a byte-by-byte comparison and print in char format
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */

static
hsize_t character_compare(unsigned char *mem1,
                  unsigned char *mem2,
                  hsize_t       i,
                  unsigned      u,
                  int           rank,
                  hsize_t       *dims,
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
            print_char_pos(ph,0,i,u,acc,pos,rank,dims,obj1,obj2);
            printf("            ");
            h5diff_print_char(temp1_uchar);
            printf("            ");
            h5diff_print_char(temp2_uchar);
            printf("\n");
        }
        nfound++;
    }
    
    return nfound;
}


/*-------------------------------------------------------------------------
 * Function: character_compare_opt
 *
 * Purpose: do a byte-by-byte comparison and print in numerical format
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */

hsize_t character_compare_opt(unsigned char *mem1,
                          unsigned char *mem2,
                          hsize_t       i,
                          int           rank,
                          hsize_t       *dims,
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
 double             per;
 int                both_zero;
    
 memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
 memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
 
 /* -d and !-p */
 
 if (options->d && !options->p)
 {
     if ( PDIFF(temp1_uchar,temp2_uchar) > options->delta)
     {
         if ( print_data(options) )
         {
             print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
             printf(SPACES);
             printf(I_FORMAT,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar));
         }
         nfound++;
     }
 }
 /* !-d and -p */
 else if (!options->d && options->p)
 {
     PER_UNSIGN(signed char,temp1_uchar,temp2_uchar);
     if ( per > options->percent )
     {
         if ( print_data(options) )
         {
             print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
             printf(SPACES);
             printf(I_FORMAT_P,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar),per);
         }
         nfound++;
     }
 }
 /* -d and -p */
 else if ( options->d && options->p)
 {
     PER_UNSIGN(signed char,temp1_uchar,temp2_uchar);
     if ( per > options->percent && PDIFF(temp1_uchar,temp2_uchar) > options->delta )
     {
         if ( print_data(options) )
         {
             print_pos(ph,1,i,acc,pos,rank,dims,obj1,obj2);
             printf(SPACES);
             printf(I_FORMAT_P,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar),per);
         }
         nfound++;
     }
 }
 else if (temp1_uchar != temp2_uchar)
 {
     if ( print_data(options) )
     {
         print_pos(ph,0,i,acc,pos,rank,dims,obj1,obj2);
         printf(SPACES);
         printf(I_FORMAT,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar));
     }
     nfound++;
 }
 
 return nfound;
 
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
                   hsize_t       hyper_start,
                   int           rank,
                   hsize_t       *dims,
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
    double      per;
    int         both_zero;
    int         isnan1=0;
    int         isnan2=0;
    

 /*-------------------------------------------------------------------------
  * -d and !-p 
  *-------------------------------------------------------------------------
  */
 
    if (options->d && !options->p)
    {
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_float, mem1, sizeof(float));
            memcpy(&temp2_float, mem2, sizeof(float));

           /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_FLOAT,&temp1_float);
                isnan2 = my_isnan(FLT_FLOAT,&temp2_float);
            }

            if ( !isnan1 && !isnan2)
            {             
                if (ABS(temp1_float-temp2_float) > options->delta)
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(F_FORMAT,temp1_float,temp2_float,ABS(temp1_float-temp2_float));
                    }
                    nfound++;
                }
            } /* NaN */
            mem1+=sizeof(float);
            mem2+=sizeof(float);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }

 /*-------------------------------------------------------------------------
  * !-d and -p 
  *-------------------------------------------------------------------------
  */
    else if (!options->d && options->p)
    {
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_float, mem1, sizeof(float));
            memcpy(&temp2_float, mem2, sizeof(float));
            
           /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_FLOAT,&temp1_float);
                isnan2 = my_isnan(FLT_FLOAT,&temp2_float);
            }
            
            if ( !isnan1 && !isnan2)
            {        
                
                PER(temp1_float,temp2_float);
                
                if (not_comparable && !both_zero) /* not comparable */
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(F_FORMAT_P_NOTCOMP,temp1_float,temp2_float,
                            ABS(temp1_float-temp2_float));
                    }
                    options->not_cmp=1;
                    nfound++;
                }
                
                else
                    
                    if ( per > options->percent )
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT_P,temp1_float,temp2_float,
                                ABS(temp1_float-temp2_float),
                                ABS(1-temp2_float/temp1_float));
                        }
                        nfound++;
                    }
            } /* NaN */
            mem1+=sizeof(float);
            mem2+=sizeof(float);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }
    
 /*-------------------------------------------------------------------------
  * -d and -p 
  *-------------------------------------------------------------------------
  */
    else if ( options->d && options->p)
    {
        
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_float, mem1, sizeof(float));
            memcpy(&temp2_float, mem2, sizeof(float));
            
            /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_FLOAT,&temp1_float);
                isnan2 = my_isnan(FLT_FLOAT,&temp2_float);
            }
            
            if ( !isnan1 && !isnan2)
            {        
                
                PER(temp1_float,temp2_float);
                
                if (not_comparable && !both_zero) /* not comparable */
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(F_FORMAT_P_NOTCOMP,temp1_float,temp2_float,
                            ABS(temp1_float-temp2_float));
                    }
                    options->not_cmp=1;
                    nfound++;
                }
                
                else
                    
                    if ( per > options->percent && ABS(temp1_float-temp2_float) > options->delta )
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT_P,temp1_float,temp2_float,
                                ABS(temp1_float-temp2_float),
                                ABS(1-temp2_float/temp1_float));
                        }
                        nfound++;
                    }
                    
            } /* NaN */
            mem1+=sizeof(float);
            mem2+=sizeof(float);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }

 /*-------------------------------------------------------------------------
  * no -d and -p 
  *-------------------------------------------------------------------------
  */
    else
    {
        
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_float, mem1, sizeof(float));
            memcpy(&temp2_float, mem2, sizeof(float));

            
            if (equal_float(temp1_float,temp2_float,options)==FALSE)
            {
                if ( print_data(options) )
                {
                    print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                    printf(SPACES);
                    printf(F_FORMAT,temp1_float,temp2_float,ABS(temp1_float-temp2_float));
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
                    hsize_t       hyper_start,
                    int           rank,
                    hsize_t       *dims,
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
    double      per;
    int         both_zero;
    int         isnan1=0;
    int         isnan2=0;
    

 /*-------------------------------------------------------------------------
  * -d and !-p 
  *-------------------------------------------------------------------------
  */
 
    if (options->d && !options->p)
    {
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_double, mem1, sizeof(double));
            memcpy(&temp2_double, mem2, sizeof(double));

           /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_DOUBLE,&temp1_double);
                isnan2 = my_isnan(FLT_DOUBLE,&temp2_double);
            }

            if ( !isnan1 && !isnan2)
            {             
                if (ABS(temp1_double-temp2_double) > options->delta)
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(F_FORMAT,temp1_double,temp2_double,ABS(temp1_double-temp2_double));
                    }
                    nfound++;
                }
            } /* NaN */
            mem1+=sizeof(double);
            mem2+=sizeof(double);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }

 /*-------------------------------------------------------------------------
  * !-d and -p 
  *-------------------------------------------------------------------------
  */
    else if (!options->d && options->p)
    {
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_double, mem1, sizeof(double));
            memcpy(&temp2_double, mem2, sizeof(double));
            
           /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_DOUBLE,&temp1_double);
                isnan2 = my_isnan(FLT_DOUBLE,&temp2_double);
            }
            
            if ( !isnan1 && !isnan2)
            {        
                
                PER(temp1_double,temp2_double);
                
                if (not_comparable && !both_zero) /* not comparable */
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(F_FORMAT_P_NOTCOMP,temp1_double,temp2_double,
                            ABS(temp1_double-temp2_double));
                    }
                    options->not_cmp=1;
                    nfound++;
                }
                
                else
                    
                    if ( per > options->percent )
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT_P,temp1_double,temp2_double,
                                ABS(temp1_double-temp2_double),
                                ABS(1-temp2_double/temp1_double));
                        }
                        nfound++;
                    }
            } /* NaN */
            mem1+=sizeof(double);
            mem2+=sizeof(double);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }
    
 /*-------------------------------------------------------------------------
  * -d and -p 
  *-------------------------------------------------------------------------
  */
    else if ( options->d && options->p)
    {
        
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_double, mem1, sizeof(double));
            memcpy(&temp2_double, mem2, sizeof(double));
            
            /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_DOUBLE,&temp1_double);
                isnan2 = my_isnan(FLT_DOUBLE,&temp2_double);
            }
            
            if ( !isnan1 && !isnan2)
            {        
                
                PER(temp1_double,temp2_double);
                
                if (not_comparable && !both_zero) /* not comparable */
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(F_FORMAT_P_NOTCOMP,temp1_double,temp2_double,
                            ABS(temp1_double-temp2_double));
                    }
                    options->not_cmp=1;
                    nfound++;
                }
                
                else
                    
                    if ( per > options->percent && ABS(temp1_double-temp2_double) > options->delta )
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(F_FORMAT_P,temp1_double,temp2_double,
                                ABS(temp1_double-temp2_double),
                                ABS(1-temp2_double/temp1_double));
                        }
                        nfound++;
                    }
                    
            } /* NaN */
            mem1+=sizeof(double);
            mem2+=sizeof(double);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }

 /*-------------------------------------------------------------------------
  * no -d and -p 
  *-------------------------------------------------------------------------
  */
    else
    {
        
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_double, mem1, sizeof(double));
            memcpy(&temp2_double, mem2, sizeof(double));
            
            if (equal_double(temp1_double,temp2_double,options)==FALSE)
            {
                if ( print_data(options) )
                {
                    print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                    printf(SPACES);
                    printf(F_FORMAT,temp1_double,temp2_double,ABS(temp1_double-temp2_double));
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
 * Function: diff_double
 *
 * Purpose: diff a H5T_NATIVE_DOUBLE type
 *
 * Return: number of differences found
 *
 *-------------------------------------------------------------------------
 */

#if H5_SIZEOF_LONG_DOUBLE !=0

hsize_t diff_ldouble(unsigned char *mem1,
                    unsigned char *mem2,
                    hsize_t       nelmts,
                    hsize_t       hyper_start,
                    int           rank,
                    hsize_t       *dims,
                    hsize_t       *acc,
                    hsize_t       *pos,
                    diff_opt_t    *options,
                    const char    *obj1,
                    const char    *obj2,
                    int           *ph)

{
    hsize_t     nfound=0;          /* number of differences found */
    long double temp1_double;
    long double temp2_double;
    hsize_t     i;
    double      per;
    int         both_zero;
    int         isnan1=0;
    int         isnan2=0;
    

 /*-------------------------------------------------------------------------
  * -d and !-p 
  *-------------------------------------------------------------------------
  */
 
    if (options->d && !options->p)
    {
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_double, mem1, sizeof(long double));
            memcpy(&temp2_double, mem2, sizeof(long double));

           /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_LDOUBLE,&temp1_double);
                isnan2 = my_isnan(FLT_LDOUBLE,&temp2_double);
            }

            if ( !isnan1 && !isnan2)
            {             
                if (ABS(temp1_double-temp2_double) > options->delta)
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(LD_FORMAT,temp1_double,temp2_double,ABS(temp1_double-temp2_double));
                    }
                    nfound++;
                }
            } /* NaN */
            mem1+=sizeof(long double);
            mem2+=sizeof(long double);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }

 /*-------------------------------------------------------------------------
  * !-d and -p 
  *-------------------------------------------------------------------------
  */
    else if (!options->d && options->p)
    {
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_double, mem1, sizeof(long double));
            memcpy(&temp2_double, mem2, sizeof(long double));
            
           /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_LDOUBLE,&temp1_double);
                isnan2 = my_isnan(FLT_LDOUBLE,&temp2_double);
            }
            
            if ( !isnan1 && !isnan2)
            {        
                
                PER(temp1_double,temp2_double);
                
                if (not_comparable && !both_zero) /* not comparable */
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(LD_FORMAT_P_NOTCOMP,temp1_double,temp2_double,
                            ABS(temp1_double-temp2_double));
                    }
                    options->not_cmp=1;
                    nfound++;
                }
                
                else
                    
                    if ( per > options->percent )
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(LD_FORMAT_P,temp1_double,temp2_double,
                                ABS(temp1_double-temp2_double),
                                ABS(1-temp2_double/temp1_double));
                        }
                        nfound++;
                    }
            } /* NaN */
            mem1+=sizeof(long double);
            mem2+=sizeof(long double);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }
    
 /*-------------------------------------------------------------------------
  * -d and -p 
  *-------------------------------------------------------------------------
  */
    else if ( options->d && options->p)
    {
        
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_double, mem1, sizeof(long double));
            memcpy(&temp2_double, mem2, sizeof(long double));
            
            /*-------------------------------------------------------------------------
            * detect NaNs
            *-------------------------------------------------------------------------
            */
            if ( options->do_nans )
            {
                isnan1 = my_isnan(FLT_LDOUBLE,&temp1_double);
                isnan2 = my_isnan(FLT_LDOUBLE,&temp2_double);
            }
            
            if ( !isnan1 && !isnan2)
            {        
                
                PER(temp1_double,temp2_double);
                
                if (not_comparable && !both_zero) /* not comparable */
                {
                    if ( print_data(options) )
                    {
                        print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                        printf(SPACES);
                        printf(LD_FORMAT_P_NOTCOMP,temp1_double,temp2_double,
                            ABS(temp1_double-temp2_double));
                    }
                    options->not_cmp=1;
                    nfound++;
                }
                
                else
                    
                    if ( per > options->percent && ABS(temp1_double-temp2_double) > options->delta )
                    {
                        if ( print_data(options) )
                        {
                            print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                            printf(SPACES);
                            printf(LD_FORMAT_P,temp1_double,temp2_double,
                                ABS(temp1_double-temp2_double),
                                ABS(1-temp2_double/temp1_double));
                        }
                        nfound++;
                    }
                    
            } /* NaN */
            mem1+=sizeof(long double);
            mem2+=sizeof(long double);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* i */
    }

 /*-------------------------------------------------------------------------
  * no -d and -p 
  *-------------------------------------------------------------------------
  */
    else
    {
        
        for ( i = 0; i < nelmts; i++)
        {
            memcpy(&temp1_double, mem1, sizeof(long double));
            memcpy(&temp2_double, mem2, sizeof(long double));
            
            if (equal_double(temp1_double,temp2_double,options)==FALSE)
            {
                if ( print_data(options) )
                {
                    print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                    printf(SPACES);
                    printf(LD_FORMAT,temp1_double,temp2_double,ABS(temp1_double-temp2_double));
                }
                nfound++;
            }
            
            mem1+=sizeof(long double);
            mem2+=sizeof(long double);
            if (options->n && nfound>=options->count)
                return nfound;
        } /* nelmts */
        
    }
    
    return nfound;
}
#endif /* H5_SIZEOF_LONG_DOUBLE */


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
                   hsize_t       hyper_start,
                   int           rank,
                   hsize_t       *dims,
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
 double      per;
 int         both_zero;
 
 
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         memcpy(&temp1_char, mem1, sizeof(char));
         memcpy(&temp2_char, mem2, sizeof(char));
         
         if (ABS(temp1_char-temp2_char) > options->delta)
         {
             if ( print_data(options) )
             {
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_char,temp2_char,ABS(temp1_char-temp2_char));
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
         
         PER(temp1_char,temp2_char);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_char,temp2_char,
                     ABS(temp1_char-temp2_char));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_char,temp2_char,
                         ABS(temp1_char-temp2_char),
                         per);
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
         
         PER(temp1_char,temp2_char);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_char,temp2_char,
                     ABS(temp1_char-temp2_char));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && ABS(temp1_char-temp2_char) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_char,temp2_char,
                         ABS(temp1_char-temp2_char),
                         per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_char,temp2_char,ABS(temp1_char-temp2_char));
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
                   hsize_t       hyper_start,
                   int           rank,
                   hsize_t       *dims,
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
 double        per;
 int           both_zero;
    
 
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
         memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
         
         if ( PDIFF(temp1_uchar,temp2_uchar) > options->delta)
         {
             if ( print_data(options) )
             {
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar));
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
         
         PER_UNSIGN(signed char,temp1_uchar,temp2_uchar);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_uchar,temp2_uchar,
                     PDIFF(temp1_uchar,temp2_uchar));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_uchar,temp2_uchar,
                         PDIFF(temp1_uchar,temp2_uchar),
                         per);
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
         
         PER_UNSIGN(signed char,temp1_uchar,temp2_uchar);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_uchar,temp2_uchar,
                     PDIFF(temp1_uchar,temp2_uchar));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && PDIFF(temp1_uchar,temp2_uchar) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_uchar,temp2_uchar,
                         PDIFF(temp1_uchar,temp2_uchar),
                         per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_uchar,temp2_uchar,PDIFF(temp1_uchar,temp2_uchar));
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
                   hsize_t       hyper_start,
                   int           rank,
                   hsize_t       *dims,
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
 double        per;
 int           both_zero;
 
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         memcpy(&temp1_short, mem1, sizeof(short));
         memcpy(&temp2_short, mem2, sizeof(short));
         
         if (ABS(temp1_short-temp2_short) > options->delta)
         {
             if ( print_data(options) )
             {
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_short,temp2_short,ABS(temp1_short-temp2_short));
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
         
         PER(temp1_short,temp2_short);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_short,temp2_short,
                     ABS(temp1_short-temp2_short));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_short,temp2_short,
                         ABS(temp1_short-temp2_short),
                         per);
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
         
         PER(temp1_short,temp2_short);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_short,temp2_short,
                     ABS(temp1_short-temp2_short));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && ABS(temp1_short-temp2_short) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_short,temp2_short,
                         ABS(temp1_short-temp2_short),
                         per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_short,temp2_short,ABS(temp1_short-temp2_short));
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
                    hsize_t       hyper_start,
                    int           rank,
                    hsize_t       *dims,
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
 double         per;
 int            both_zero;
    
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         memcpy(&temp1_ushort, mem1, sizeof(unsigned short));
         memcpy(&temp2_ushort, mem2, sizeof(unsigned short));
         
         if ( PDIFF(temp1_ushort,temp2_ushort) > options->delta)
         {
             if ( print_data(options) )
             {
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_ushort,temp2_ushort,PDIFF(temp1_ushort,temp2_ushort));
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
         
         PER_UNSIGN(signed short,temp1_ushort,temp2_ushort);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_ushort,temp2_ushort,
                     PDIFF(temp1_ushort,temp2_ushort));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_ushort,temp2_ushort,
                         PDIFF(temp1_ushort,temp2_ushort),
                         per);
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
         
         PER_UNSIGN(signed short,temp1_ushort,temp2_ushort);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_ushort,temp2_ushort,
                     PDIFF(temp1_ushort,temp2_ushort));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && PDIFF(temp1_ushort,temp2_ushort) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_ushort,temp2_ushort,
                         PDIFF(temp1_ushort,temp2_ushort),
                         per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_ushort,temp2_ushort,PDIFF(temp1_ushort,temp2_ushort));
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
                 hsize_t       hyper_start,
                 int           rank,
                 hsize_t       *dims,
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
 double        per;
 int           both_zero;
    
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         memcpy(&temp1_int, mem1, sizeof(int));
         memcpy(&temp2_int, mem2, sizeof(int));
         
         if (ABS(temp1_int-temp2_int) > options->delta)
         {
             if ( print_data(options) )
             {
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_int,temp2_int,ABS(temp1_int-temp2_int));
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
         
         PER(temp1_int,temp2_int);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_int,temp2_int,
                     ABS(temp1_int-temp2_int));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_int,temp2_int,
                         ABS(temp1_int-temp2_int),
                         per);
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
         
         PER(temp1_int,temp2_int);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_int,temp2_int,
                     ABS(temp1_int-temp2_int));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && ABS(temp1_int-temp2_int) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_int,temp2_int,
                         ABS(temp1_int-temp2_int),
                         per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_int,temp2_int,ABS(temp1_int-temp2_int));
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
                  hsize_t       hyper_start,
                  int           rank,
                  hsize_t       *dims,
                  hsize_t       *acc,
                  hsize_t       *pos,
                  diff_opt_t    *options,
                  const char    *obj1,
                  const char    *obj2,
                  int           *ph)

{
 hsize_t        nfound=0;          /* number of differences found */
 unsigned int   temp1_uint;
 unsigned int   temp2_uint;
 hsize_t        i;
 double         per;
 int            both_zero;
 
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         memcpy(&temp1_uint, mem1, sizeof(unsigned int));
         memcpy(&temp2_uint, mem2, sizeof(unsigned int));
         
         if ( PDIFF(temp1_uint,temp2_uint) > options->delta)
         {
             if ( print_data(options) )
             {
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint));
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
         
         PER_UNSIGN(signed int,temp1_uint,temp2_uint);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_uint,temp2_uint,
                         PDIFF(temp1_uint,temp2_uint),
                         per);
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
         
         PER_UNSIGN(signed int,temp1_uint,temp2_uint);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT_P_NOTCOMP,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && PDIFF(temp1_uint,temp2_uint) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(I_FORMAT_P,temp1_uint,temp2_uint,
                         PDIFF(temp1_uint,temp2_uint),
                         per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(I_FORMAT,temp1_uint,temp2_uint,PDIFF(temp1_uint,temp2_uint));
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
                  hsize_t       hyper_start,
                  int           rank,
                  hsize_t       *dims,
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
 double        per;
 int           both_zero;
 
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         
         for ( i = 0; i < nelmts; i++)
         {
             memcpy(&temp1_long, mem1, sizeof(long));
             memcpy(&temp2_long, mem2, sizeof(long));
             
             if (ABS(temp1_long-temp2_long) > options->delta)
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(LI_FORMAT,temp1_long,temp2_long,ABS(temp1_long-temp2_long));
                 }
                 nfound++;
             }
             mem1+=sizeof(long);
             mem2+=sizeof(long);
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
         memcpy(&temp1_long, mem1, sizeof(long));
         memcpy(&temp2_long, mem2, sizeof(long));
         
         PER(temp1_long,temp2_long);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(LI_FORMAT_P_NOTCOMP,temp1_long,temp2_long,
                     ABS(temp1_long-temp2_long));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(LI_FORMAT_P,temp1_long,temp2_long,
                         ABS(temp1_long-temp2_long),
                         per);
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
         
         PER(temp1_long,temp2_long);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(LI_FORMAT_P_NOTCOMP,temp1_long,temp2_long,
                     ABS(temp1_long-temp2_long));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && ABS(temp1_long-temp2_long) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(LI_FORMAT_P,temp1_long,temp2_long,
                         ABS(temp1_long-temp2_long),
                         per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(LI_FORMAT,temp1_long,temp2_long,ABS(temp1_long-temp2_long));
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
                   hsize_t       hyper_start,
                   int           rank,
                   hsize_t       *dims,
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
 double         per;
 int            both_zero;
    
 
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         
         for ( i = 0; i < nelmts; i++)
         {
             memcpy(&temp1_ulong, mem1, sizeof(unsigned long));
             memcpy(&temp2_ulong, mem2, sizeof(unsigned long));
             
             if ( PDIFF(temp1_ulong,temp2_ulong) > options->delta)
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(LI_FORMAT,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong));
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
         
         PER_UNSIGN(signed long,temp1_ulong,temp2_ulong);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(ULI_FORMAT_P_NOTCOMP,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(LI_FORMAT_P,temp1_ulong,temp2_ulong,
                         PDIFF(temp1_ulong,temp2_ulong),
                         per);
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
         
         PER_UNSIGN(signed long,temp1_ulong,temp2_ulong);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(ULI_FORMAT_P_NOTCOMP,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && PDIFF(temp1_ulong,temp2_ulong) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(LI_FORMAT_P,temp1_ulong,temp2_ulong,
                         PDIFF(temp1_ulong,temp2_ulong),
                         per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(LI_FORMAT,temp1_ulong,temp2_ulong,PDIFF(temp1_ulong,temp2_ulong));
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
                   hsize_t       hyper_start,
                   int           rank,
                   hsize_t       *dims,
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
 double        per;
 int           both_zero;
 
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         memcpy(&temp1_llong, mem1, sizeof(long_long));
         memcpy(&temp2_llong, mem2, sizeof(long_long));
         
         if (ABS( temp1_llong-temp2_llong) > options->delta)
         {
             if ( print_data(options) )
             {
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(LLI_FORMAT,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong));
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
         
         PER(temp1_llong,temp2_llong);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(LLI_FORMAT_P_NOTCOMP,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(LLI_FORMAT_P,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong),per);
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
         
         PER(temp1_llong,temp2_llong);
         
         if (not_comparable && !both_zero) /* not comparable */
         {
             if ( print_data(options) )
             {
                 print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(LLI_FORMAT_P_NOTCOMP,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong));
             }
             options->not_cmp=1;
             nfound++;
         }
         
         else
             
             if ( per > options->percent && ABS(temp1_llong-temp2_llong) > options->delta )
             {
                 if ( print_data(options) )
                 {
                     print_pos(ph,1,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                     printf(SPACES);
                     printf(LLI_FORMAT_P,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong),per);
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(LLI_FORMAT,temp1_llong,temp2_llong,ABS(temp1_llong-temp2_llong));
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
                    hsize_t       hyper_start,
                    int           rank,
                    hsize_t       *dims,
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
     
 /* -d and !-p */
 if (options->d && !options->p)
 {
     
     for ( i = 0; i < nelmts; i++)
     {
         memcpy(&temp1_ullong, mem1, sizeof(unsigned long_long));
         memcpy(&temp2_ullong, mem2, sizeof(unsigned long_long));
         
         if ( PDIFF(temp1_ullong,temp2_ullong) > (unsigned long_long) options->delta)
         {
             if ( print_data(options) )
             {
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(ULLI_FORMAT,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong));
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
     
     if ( print_data(options) )
     {
         NOT_SUPPORTED();
     }
     options->not_cmp=1;
     return nfound;
     
     
 }
 
 /* -d and -p */
 else if ( options->d && options->p)
 {
     
     if ( print_data(options) )
     {
         NOT_SUPPORTED();
     }
     options->not_cmp=1;
     return nfound;
     
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
                 print_pos(ph,0,hyper_start+i,acc,pos,rank,dims,obj1,obj2);
                 printf(SPACES);
                 printf(ULLI_FORMAT,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong));
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

/*-------------------------------------------------------------------------
 * Function:    equal_double
 *
 * Purpose:     use a relative error formula to deal with floating point 
 *              uncertainty 
 *
 * Programmer:  pvn
 *              October 24, 2006
 *
 * Modifications:
 *  8/1/2007. handle NaNs
 *
 *-------------------------------------------------------------------------
 */
static 
hbool_t equal_double(double value, double expected, diff_opt_t *options)                               
{
    if ( options->do_nans )
    {
        
        
        /*-------------------------------------------------------------------------
        * detect NaNs
        *-------------------------------------------------------------------------
        */
        int isnan1 = my_isnan(FLT_DOUBLE,&value);
        int isnan2 = my_isnan(FLT_DOUBLE,&expected);
        
        /*-------------------------------------------------------------------------
        * we consider NaN == NaN to be true 
        *-------------------------------------------------------------------------
        */
        if ( isnan1 && isnan2 )
        {
            return TRUE;
        }
        
        /*-------------------------------------------------------------------------
        * one is a NaN, do not compare but assume difference
        *-------------------------------------------------------------------------
        */
        if ( (isnan1 && !isnan2) || ( !isnan1 && isnan2 ) )
        {
            return FALSE;
        }
        
        /*-------------------------------------------------------------------------
        * both are not NaNs, compare
        *-------------------------------------------------------------------------
        */
        
    }

    if (value == expected)
        return TRUE;

    if ( ABS( (value-expected) ) < DBL_EPSILON)
        return TRUE;

    return FALSE;
    
}

/*-------------------------------------------------------------------------
 * Function:    equal_ldouble
 *
 * Purpose:     use a relative error formula to deal with floating point
 *              uncertainty
 *
 *-------------------------------------------------------------------------
 */

#if H5_SIZEOF_LONG_DOUBLE !=0

static 
hbool_t equal_ldouble(long double value, long double expected, diff_opt_t *options)                               
{
    if ( options->do_nans )
    {
        
        
        /*-------------------------------------------------------------------------
        * detect NaNs
        *-------------------------------------------------------------------------
        */
        int isnan1 = my_isnan(FLT_LDOUBLE,&value);
        int isnan2 = my_isnan(FLT_LDOUBLE,&expected);
        
        /*-------------------------------------------------------------------------
        * we consider NaN == NaN to be true 
        *-------------------------------------------------------------------------
        */
        if ( isnan1 && isnan2 )
        {
            return TRUE;
        }
        
        /*-------------------------------------------------------------------------
        * one is a NaN, do not compare but assume difference
        *-------------------------------------------------------------------------
        */
        if ( (isnan1 && !isnan2) || ( !isnan1 && isnan2 ) )
        {
            return FALSE;
        }
        
        /*-------------------------------------------------------------------------
        * both are not NaNs, compare
        *-------------------------------------------------------------------------
        */
        
    }


    if (value == expected)
        return TRUE;

    if (options->use_system_epsilon) {
        if ( ABS( (value-expected) ) < DBL_EPSILON)
            return TRUE;
    }

    return FALSE;
    
}



#endif /* #if H5_SIZEOF_LONG_DOUBLE !=0 */


/*-------------------------------------------------------------------------
 * Function:    equal_float
 *
 * Purpose:     use a relative error formula to deal with floating point 
 *              uncertainty 
 *
 * Programmer:  pvn
 *              October 24, 2006
 *
 * Modifications:
 *  8/1/2007. handle NaNs
 *
 *-------------------------------------------------------------------------
 */
static 
hbool_t equal_float(float value, float expected, diff_opt_t *options)                               
{
    if ( options->do_nans )
    {
        
        
        /*-------------------------------------------------------------------------
        * detect NaNs
        *-------------------------------------------------------------------------
        */
        int isnan1 = my_isnan(FLT_FLOAT,&value);
        int isnan2 = my_isnan(FLT_FLOAT,&expected);
        
        /*-------------------------------------------------------------------------
        * we consider NaN == NaN to be true 
        *-------------------------------------------------------------------------
        */
        if ( isnan1 && isnan2 )
        {
            return TRUE;
        }
        
        /*-------------------------------------------------------------------------
        * one is a NaN, do not compare but assume difference
        *-------------------------------------------------------------------------
        */
        if ( (isnan1 && !isnan2) || ( !isnan1 && isnan2 ) )
        {
            return FALSE;
        }
        
        /*-------------------------------------------------------------------------
        * both are not NaNs, compare
        *-------------------------------------------------------------------------
        */
        
        
    }
    
    
    if (value == expected)
        return TRUE;

    if (options->use_system_epsilon) {
        if ( ABS( (value-expected) ) < FLT_EPSILON)
            return TRUE;
    }

    return FALSE;

}



/*-------------------------------------------------------------------------
 * Function:	my_isnan
 *
 * Purpose:	Determines whether VAL points to NaN.
 *
 * Return:	TRUE or FALSE
 *
 * Programmer:	Robb Matzke
 *              Monday, July  6, 1998
 *
 * Modifications:
 *  Pedro Vicente, 12 October 2007.
 *  Add a string detection type for WIN32
 *
 *-------------------------------------------------------------------------
 */
static int
my_isnan(dtype_t type, void *val)
{
    int retval = 0;
    char s[256];
    
    if (FLT_FLOAT==type) 
    {
        float x;
        HDmemcpy(&x, val, sizeof(float));
        retval = (x!=x);


 
    } 
    else if (FLT_DOUBLE==type) 
    {
        double x;
        HDmemcpy(&x, val, sizeof(double));
        retval = (x!=x);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
    } 
    else if (FLT_LDOUBLE==type) 
    {
        long double x;
        HDmemcpy(&x, val, sizeof(long double));
        retval = (x!=x);
#endif
    } 
    else 
    {
        return 0;
    }
    
   /*
    * Sometimes NaN==NaN (e.g., DEC Alpha) so we try to print it and see if
    * the result contains a NaN string.
    */
    if (!retval) 
    {
        if (FLT_FLOAT==type) 
        {
            float x;
            HDmemcpy(&x, val, sizeof(float));
            sprintf(s, "%g", x);


        } 
        else if (FLT_DOUBLE==type) 
        {
            double x;
            HDmemcpy(&x, val, sizeof(double));
            sprintf(s, "%g", x);
#if H5_SIZEOF_LONG_DOUBLE!=H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE!=0
        } 
        else if (FLT_LDOUBLE==type) 
        {
            long double x;
            HDmemcpy(&x, val, sizeof(long double));
            sprintf(s, "%Lg", x);
#endif
        } 
        else 
        {
            return 0;
        }
     

        if ( HDstrstr(s, "NaN") || 
             HDstrstr(s, "NAN") || 
             HDstrstr(s, "nan") || 
             HDstrstr(s, "-1.#IND") /* WIN32 */ 
           )
        {

            retval = 1;
        }




    }
    
#ifdef H5_VMS
    /* For "float" and "double" on OpenVMS/Alpha, NaN is
    * actually a valid value of maximal value.*/
    if(!retval) 
    {
        if (FLT_FLOAT==type) 
        {
            float x;
            HDmemcpy(&x, val, sizeof(float));
            retval = (x==FLT_MAX || x==-FLT_MAX);
        } else if (FLT_DOUBLE==type) {
            double x;
            HDmemcpy(&x, val, sizeof(double));
            retval = (x==DBL_MAX || x==-DBL_MAX);
        } else 
        {
            return 0;
        }
    }
#endif /*H5_VMS*/
    
    return retval;
}




 
/*-------------------------------------------------------------------------
 *
 * Local functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: print_data
 *
 * Purpose: print data only in report or verbose modes, and do not print in quiet mode
 *-------------------------------------------------------------------------
 */
static
int print_data(diff_opt_t *options)
{
    return ( (options->m_report || options->m_verbose) && !options->m_quiet)?1:0;
}


/*-------------------------------------------------------------------------
 * Function: print_pos
 *
 * Purpose: print in matrix notation, converting from an array index position
 *
 *-------------------------------------------------------------------------
 */
static
void print_pos( int        *ph,       /* print header */
                int        pp,        /* print percentage */
                hsize_t    curr_pos,
                hsize_t    *acc,
                hsize_t    *pos,
                int        rank,
                hsize_t    *dims,
                const char *obj1,
                const char *obj2 )
{
    int i;
    
    /* print header */
    if ( *ph==1 )
    {
        *ph=0;

        printf("%-16s","size:");
        print_dimensions (rank,dims);
        printf("%-11s","");
        print_dimensions (rank,dims);
        printf("\n");
        
        if (pp)
        {
            printf("%-15s %-15s %-15s %-15s %-15s\n",
                "position",
                (obj1!=NULL) ? obj1 : " ",
                (obj2!=NULL) ? obj2 : " ",
                "difference",
                "relative");
            printf("------------------------------------------------------------------------\n");
        }
        else
        {
            printf("%-15s %-15s %-15s %-20s\n",
                "position",
                (obj1!=NULL) ? obj1 : " ",
                (obj2!=NULL) ? obj2 : " ",
                "difference");
            printf("------------------------------------------------------------\n");
        }
    } /* end print header */
    
    for ( i = 0; i < rank; i++)
    {
        pos[i] = curr_pos/acc[i];
        curr_pos -= acc[i]*pos[i];
    }
    assert( curr_pos == 0 );
    
    
    if ( rank > 0 )
    {
        printf("[ " );
        for ( i = 0; i < rank; i++)
        {
            printf("%"H5_PRINTF_LL_WIDTH"u ", (unsigned long_long)pos[i]);
        }
        printf("]" );
    }
    else
    {
        printf("      ");
        
    }
}


/*-------------------------------------------------------------------------
 * Function: print_char_pos
 *
 * Purpose: print character position in string
 *
 *-------------------------------------------------------------------------
 */

static
void print_char_pos( int        *ph,       /* print header */
                     int        pp,        /* print percentage */
                     hsize_t    curr_pos,
                     unsigned   u,
                     hsize_t    *acc,
                     hsize_t    *pos,
                     int        rank,
                     hsize_t    *dims,
                     const char *obj1,
                     const char *obj2 )
{
    int i;

    /* print header */
    if ( *ph==1 )
    {
        *ph=0;

        printf("%-16s","size:");
        print_dimensions (rank,dims);
        printf("%-11s","");
        print_dimensions (rank,dims);
        printf("\n");

        if (pp)
        {
            printf("%-15s %-15s %-15s %-15s %-15s\n",
                "position",
                (obj1!=NULL) ? obj1 : " ",
                (obj2!=NULL) ? obj2 : " ",
                "difference",
                "relative");
            printf("------------------------------------------------------------------------\n");
        }
        else
        {
            printf("%-15s %-15s %-15s %-20s\n",
                "position",
                (obj1!=NULL) ? obj1 : " ",
                (obj2!=NULL) ? obj2 : " ",
                "difference");
            printf("------------------------------------------------------------\n");
        }
    } /* end print header */

    for ( i = 0; i < rank; i++)
    {
        pos[i] = curr_pos/acc[i];
        curr_pos -= acc[i]*pos[i];
    }
    assert( curr_pos == 0 );

    printf("[ " );
    if ( rank > 0 )
    {
        
        for ( i = 0; i < rank; i++)
        {
            printf("%"H5_PRINTF_LL_WIDTH"u ", (unsigned long_long)pos[i]);
            printf(" ");
        }
        
    }
    else
    {
        printf("%u", (unsigned)u);
    }
    printf("]" );
}

/*-------------------------------------------------------------------------
 * Function:    h5diff_print_char. Adapted from h5tools_print_char
 *
 * Purpose: Print a char
 *
 *-------------------------------------------------------------------------
 */
static void h5diff_print_char(char ch)
{
    
    switch (ch) 
    {
    case '"':
        printf("\\\"");
        break;
    case '\\':
        printf( "\\\\");
        break;
    case '\b':
        printf("\\b");
        break;
    case '\f':
        printf("\\f");
        break;
    case '\n':
        printf("\\n");
        break;
    case '\r':
        printf("\\r");
        break;
    case '\t':
        printf("\\t");
        break;
    default:
        if (isprint(ch))
            printf( "%c", ch);
        else
            printf( "\\%03o", ch);
        
        break;
    }
}
