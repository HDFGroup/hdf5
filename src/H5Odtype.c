/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#define H5T_PACKAGE	/*prevent warning from including H5Tpkg.h	*/

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Tpkg.h>

#define PABLO_MASK	H5O_dtype_mask

/* PRIVATE PROTOTYPES */
static herr_t H5O_dtype_encode (H5F_t *f, size_t raw_size, uint8 *p,
				const void *mesg);
static void *H5O_dtype_decode (H5F_t *f, size_t raw_size, const uint8 *p);
static void *H5O_dtype_copy (const void *_mesg, void *_dest);
static size_t H5O_dtype_size (H5F_t *f, const void *_mesg);
static herr_t H5O_dtype_reset (void *_mesg);
static herr_t H5O_dtype_debug (H5F_t *f, const void *_mesg,
			       FILE *stream, intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_DTYPE[1] = {{
   H5O_DTYPE_ID,		    	/* message id number		*/
   "data_type",				/* message name for debugging	*/
   sizeof (H5T_t),			/* native message size		*/
   H5O_dtype_decode,			/* decode message		*/
   H5O_dtype_encode,			/* encode message		*/
   H5O_dtype_copy,			/* copy the native value 	*/
   H5O_dtype_size,			/* size of raw message		*/
   H5O_dtype_reset,			/* reset method 		*/
   H5O_dtype_debug,			/* debug the message 		*/
}};

/* Interface initialization */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT	NULL


/*-------------------------------------------------------------------------
 * Function:	H5O_dtype_decode_helper
 *
 * Purpose:	Decodes a data type
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dtype_decode_helper (const uint8 **pp, H5T_t *dt)
{
   uintn	flags, perm_word;
   intn		i, j;
   
   FUNC_ENTER (H5O_dtype_decode_helper, FAIL);

   /* check args */
   assert (pp && *pp);
   assert (dt);

   /* decode */
   UINT32DECODE (*pp, flags);
   dt->type = flags & 0xff;
   flags >>= 8;
   UINT32DECODE (*pp, dt->size);

   switch (dt->type) {
   case H5T_INTEGER:
      /*
       * Integer types...
       */
      dt->u.atomic.order = (flags & 0x1) ? H5T_ORDER_BE : H5T_ORDER_LE;
      dt->u.atomic.lsb_pad = (flags & 0x2) ? H5T_PAD_ONE : H5T_PAD_ZERO;
      dt->u.atomic.msb_pad = (flags & 0x4) ? H5T_PAD_ONE : H5T_PAD_ZERO;
      dt->u.atomic.u.i.sign = (flags & 0x8) ? H5T_SGN_2 : H5T_SGN_NONE;
      UINT16DECODE (*pp, dt->u.atomic.offset);
      UINT16DECODE (*pp, dt->u.atomic.prec);
      break;

   case H5T_FLOAT:
      /*
       * Floating-point types...
       */
      dt->u.atomic.order = (flags & 0x1) ? H5T_ORDER_BE : H5T_ORDER_LE;
      dt->u.atomic.lsb_pad = (flags & 0x2) ? H5T_PAD_ONE : H5T_PAD_ZERO;
      dt->u.atomic.msb_pad = (flags & 0x4) ? H5T_PAD_ONE : H5T_PAD_ZERO;
      dt->u.atomic.u.f.pad = (flags & 0x8) ? H5T_PAD_ONE : H5T_PAD_ZERO;
      switch ((flags>>4) & 0x03) {
      case 0:
	 dt->u.atomic.u.f.norm = H5T_NORM_NONE;
	 break;
      case 1:
	 dt->u.atomic.u.f.norm = H5T_NORM_MSBSET;
	 break;
      case 2:
	 dt->u.atomic.u.f.norm = H5T_NORM_IMPLIED;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"unknown floating-point normalization");
      }
      dt->u.atomic.u.f.sign = (flags>>8) & 0xff;
      UINT16DECODE (*pp, dt->u.atomic.offset);
      UINT16DECODE (*pp, dt->u.atomic.prec);
      dt->u.atomic.u.f.epos = *(*pp)++;
      dt->u.atomic.u.f.esize = *(*pp)++;
      assert (dt->u.atomic.u.f.esize>0);
      dt->u.atomic.u.f.mpos = *(*pp)++;
      dt->u.atomic.u.f.msize = *(*pp)++;
      assert (dt->u.atomic.u.f.msize>0);
      UINT32DECODE (*pp, dt->u.atomic.u.f.ebias);
      break;

   case H5T_COMPOUND:
      /*
       * Compound data types...
       */
      dt->u.compnd.nmembs = flags & 0xffff;
      assert (dt->u.compnd.nmembs>0);
      dt->u.compnd.nalloc = dt->u.compnd.nmembs;
      dt->u.compnd.memb = H5MM_xcalloc (dt->u.compnd.nalloc,
					sizeof(H5T_member_t));
      for (i=0; i<dt->u.compnd.nmembs; i++) {
	 dt->u.compnd.memb[i].name = H5MM_xstrdup (*pp);
	 *pp += ((strlen (*pp)+8)/8) * 8; /*multiple of 8 w/ null terminator*/
	 UINT32DECODE (*pp, dt->u.compnd.memb[i].offset);
	 dt->u.compnd.memb[i].ndims = *(*pp)++;
	 assert (dt->u.compnd.memb[i].ndims<=4);
	 *pp += 3; /*reserved bytes*/
	 for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
	    UINT32DECODE (*pp, dt->u.compnd.memb[i].dim[j]);
	 }
	 UINT32DECODE (*pp, perm_word);
	 dt->u.compnd.memb[i].perm[0] = (perm_word >>  0) & 0xff;
	 dt->u.compnd.memb[i].perm[1] = (perm_word >>  8) & 0xff;
	 dt->u.compnd.memb[i].perm[2] = (perm_word >> 16) & 0xff;
	 dt->u.compnd.memb[i].perm[3] = (perm_word >> 24) & 0xff;
	 if (H5O_dtype_decode_helper (pp, &(dt->u.compnd.memb[i].type))<0 ||
	     H5T_COMPOUND==dt->u.compnd.memb[i].type.type) {
	    for (j=0; j<=i; j++) H5MM_xfree (dt->u.compnd.memb[i].name);
	    H5MM_xfree (dt->u.compnd.memb);
	    HRETURN_ERROR (H5E_DATATYPE, H5E_CANTDECODE, FAIL,
			   "can't decode member type");
	 }
      }
      break;
      
   default:
      if (flags) {
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"class flags are non-zero");
      }
      break;
   }
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_dtype_encode_helper
 *
 * Purpose:	Encodes a data type.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dtype_encode_helper (uint8 **pp, const H5T_t *dt)
{
   uintn	flags = 0;
   uintn	perm_word;
   char		*hdr = *pp;
   intn		i, j, n;
   
   FUNC_ENTER (H5O_dtype_encode_helper, FAIL);

   /* check args */
   assert (pp && *pp);
   assert (dt);

   /* skip the type and class bit field for now */
   *pp += 4;
   UINT32ENCODE (*pp, dt->size);

   switch (dt->type) {
   case H5T_INTEGER:
      /*
       * Integer data types...
       */
      switch (dt->u.atomic.order) {
      case H5T_ORDER_LE:
	 break; /*nothing*/
      case H5T_ORDER_BE:
	 flags |= 0x01;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"byte order is not supported in file format yet");
      }

      switch (dt->u.atomic.lsb_pad) {
      case H5T_PAD_ZERO:
	 break; /*nothing*/
      case H5T_PAD_ONE:
	 flags |= 0x02;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"bit padding is not supported in file format yet");
      }
      
      switch (dt->u.atomic.msb_pad) {
      case H5T_PAD_ZERO:
	 break; /*nothing*/
      case H5T_PAD_ONE:
	 flags |= 0x04;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"bit padding is not supported in file format yet");
      }

      switch (dt->u.atomic.u.i.sign) {
      case H5T_SGN_NONE:
	 break; /*nothing*/
      case H5T_SGN_2:
	 flags |= 0x08;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"sign scheme is not supported in file format yet");
      }
      
      UINT16ENCODE (*pp, dt->u.atomic.offset);
      UINT16ENCODE (*pp, dt->u.atomic.prec);
      break;

   case H5T_FLOAT:
      /*
       * Floating-point types...
       */
      switch (dt->u.atomic.order) {
      case H5T_ORDER_LE:
	 break; /*nothing*/
      case H5T_ORDER_BE:
	 flags |= 0x01;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"byte order is not supported in file format yet");
      }

      switch (dt->u.atomic.lsb_pad) {
      case H5T_PAD_ZERO:
	 break; /*nothing*/
      case H5T_PAD_ONE:
	 flags |= 0x02;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"bit padding is not supported in file format yet");
      }
      
      switch (dt->u.atomic.msb_pad) {
      case H5T_PAD_ZERO:
	 break; /*nothing*/
      case H5T_PAD_ONE:
	 flags |= 0x04;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"bit padding is not supported in file format yet");
      }
      
      switch (dt->u.atomic.u.f.pad) {
      case H5T_PAD_ZERO:
	 break; /*nothing*/
      case H5T_PAD_ONE:
	 flags |= 0x08;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"bit padding is not supported in file format yet");
      }

      switch (dt->u.atomic.u.f.norm) {
      case H5T_NORM_NONE:
	 break; /*nothing*/
      case H5T_NORM_MSBSET:
	 flags |= 0x10;
	 break;
      case H5T_NORM_IMPLIED:
	 flags |= 0x20;
	 break;
      default:
	 HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			"normalization scheme is not supported in file "
			"format yet");
      }

      flags |= (dt->u.atomic.u.f.sign << 8) & 0xff00;
      UINT16ENCODE (*pp, dt->u.atomic.offset);
      UINT16ENCODE (*pp, dt->u.atomic.prec);
      *(*pp)++ = dt->u.atomic.u.f.epos;
      *(*pp)++ = dt->u.atomic.u.f.esize;
      *(*pp)++ = dt->u.atomic.u.f.mpos;
      *(*pp)++ = dt->u.atomic.u.f.msize;
      UINT32ENCODE (*pp, dt->u.atomic.u.f.ebias);
      break;

   case H5T_COMPOUND:
      /*
       * Compound data types...
       */
      flags = dt->u.compnd.nmembs & 0xffff;
      for (i=0; i<dt->u.compnd.nmembs; i++) {
	 n = strlen (dt->u.compnd.memb[i].name);
	 for (j=0; j<=n && j%8; j++) {
	    if (j<n) *(*pp)++ = dt->u.compnd.memb[i].name[j];
	    else *(*pp)++ = '\0';
	 }
	 UINT32ENCODE (*pp, dt->u.compnd.memb[i].offset);
	 *(*pp)++ = dt->u.compnd.memb[i].ndims;
	 assert (dt->u.compnd.memb[i].ndims<=4);
	 *(*pp)++ = '\0';
	 *(*pp)++ = '\0';
	 *(*pp)++ = '\0';
	 for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
	    UINT32ENCODE (*pp, dt->u.compnd.memb[i].dim[j]);
	 }
	 for (j=0, perm_word=0; j<dt->u.compnd.memb[i].ndims; j++) {
	    perm_word |= dt->u.compnd.memb[i].perm[j] << (8*j);
	 }
	 UINT32ENCODE (*pp, perm_word);
	 if (H5O_dtype_encode_helper (pp, &(dt->u.compnd.memb[i].type))<0) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_CANTENCODE, FAIL,
			   "can't encode member type");
	 }
      }
      break;

   default:
      /*nothing*/
      break;
   }

   *hdr++ = dt->type;
   *hdr++ = (flags >>  0) & 0xff;
   *hdr++ = (flags >>  8) & 0xff;
   *hdr++ = (flags >> 16) & 0xff;

   FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_decode
 PURPOSE
    Decode a datatype message and return a pointer to a memory struct
        with the decoded information
 USAGE
    void *H5O_dtype_decode(f, raw_size, p)
        H5F_t *f;               IN: pointer to the HDF5 file struct
        size_t raw_size;        IN: size of the raw information buffer
        const uint8 *p;         IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
        This function decodes the "raw" disk form of a simple datatype message
    into a struct in memory native format.  The struct is allocated within this
    function using malloc() and is returned to the caller.
--------------------------------------------------------------------------*/
static void *
H5O_dtype_decode (H5F_t *f, size_t raw_size, const uint8 *p)
{
   H5T_t	*dt = NULL;
   
   FUNC_ENTER (H5O_dtype_decode, NULL);

   /* check args */
   assert (f);
   assert (raw_size>0);
   assert (p);

   dt = H5MM_xcalloc (1, sizeof(H5T_t));

   if (H5O_dtype_decode_helper (&p, dt)<0) {
      H5MM_xfree (dt);
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTDECODE, NULL,
		     "can't decode type");
   }
   assert (raw_size==H5O_dtype_size (f, (void*)dt));
   
   FUNC_LEAVE (dt);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_encode
 PURPOSE
    Encode a simple datatype message 
 USAGE
    herr_t H5O_dtype_encode(f, raw_size, p, mesg)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        size_t raw_size;        IN: size of the raw information buffer
        const uint8 *p;         IN: the raw information buffer
        const void *mesg;       IN: Pointer to the simple datatype struct
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function encodes the native memory form of the simple datatype
    message in the "raw" disk form.
--------------------------------------------------------------------------*/
static herr_t
H5O_dtype_encode (H5F_t *f, size_t raw_size, uint8 *p, const void *mesg)
{
   const H5T_t		*dt = (const H5T_t *)mesg;

   FUNC_ENTER (H5O_dtype_encode, FAIL);

   /* check args */
   assert (f);
   assert (raw_size==H5O_dtype_size (f, mesg));
   assert (p);
   assert (dt);

   /* encode */
   if (H5O_dtype_encode_helper (&p, dt)<0) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTENCODE, FAIL,
		     "can't encode type");
   }

   FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O_dtype_copy(mesg, dest)
        const void *mesg;       IN: Pointer to the source simple datatype
 				    struct 
  	const void *dest;       IN: Pointer to the destination simple
 				    datatype struct 
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
        This function copies a native (memory) simple datatype message,
    allocating the destination structure if necessary.
--------------------------------------------------------------------------*/
static void *
H5O_dtype_copy (const void *_src, void *_dst)
{
   const H5T_t	*src = (const H5T_t *)_src;
   H5T_t	*dst = NULL;
   
   FUNC_ENTER (H5O_dtype_copy, NULL);

   /* check args */
   assert (src);

   /* copy */
   if (NULL==(dst=H5T_copy (src))) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, NULL, "can't copy type");
   }
   
   /* was result already allocated? */
   if (_dst) {
      *((H5T_t*)_dst) = *dst;
      H5MM_xfree (dst);
      dst = (H5T_t*)_dst;
   }

   FUNC_LEAVE ((void*)dst);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    void *H5O_dtype_size(f, mesg)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;     IN: Pointer to the source simple datatype struct
 RETURNS
    Size of message on success, FAIL on failure
 DESCRIPTION
        This function returns the size of the raw simple datatype message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.
--------------------------------------------------------------------------*/
static size_t
H5O_dtype_size (H5F_t *f, const void *mesg)
{
   intn		i;
   size_t	ret_value = 8;
   const H5T_t	*dt = (const H5T_t *)mesg;
   
   FUNC_ENTER (H5O_dtype_size, FAIL);

   assert (mesg);

   switch (dt->type) {
   case H5T_INTEGER:
      ret_value += 4;
      break;

   case H5T_FLOAT:
      ret_value += 12;
      break;

   case H5T_COMPOUND:
      for (i=0; i<dt->u.compnd.nmembs; i++) {
	 ret_value += ((HDstrlen (dt->u.compnd.memb[i].name)+8)/8)*8;
	 ret_value += 12 + dt->u.compnd.memb[i].ndims * 4;
	 ret_value += H5O_dtype_size (f, &(dt->u.compnd.memb[i].type));
      }
      break;

   default:
      /*no properties*/
      break;
   }
   

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_dtype_reset
 *
 * Purpose:	Frees resources within a data type message, but doesn't free
 *		the message itself.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dtype_reset (void *_mesg)
{
   H5T_t	*dt = (H5T_t *)_mesg;
   H5T_t	*tmp = NULL;
   
   FUNC_ENTER (H5O_dtype_reset, FAIL);

   if (dt) {
      tmp = H5MM_xmalloc (sizeof(H5T_t));
      *tmp = *dt;
      H5T_close (tmp);
      HDmemset (dt, 0, sizeof(H5T_t));
   }

   FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_debug
 PURPOSE
    Prints debugging information for a data type message
 USAGE
    void *H5O_dtype_debug(f, mesg, stream, indent, fwidth)
        H5F_t *f;               IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source simple datatype
 				    struct
        FILE *stream;           IN: Pointer to the stream for output data
        intn indent;            IN: Amount to indent information by
        intn fwidth;            IN: Field width (?)
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function prints debugging output to the stream passed as a 
    parameter.
--------------------------------------------------------------------------*/
static herr_t
H5O_dtype_debug (H5F_t *f, const void *mesg, FILE *stream,
		intn indent, intn fwidth)
{
   const H5T_t	*dt = (const H5T_t *)mesg;
   const char	*s;
   char		buf[256];
   intn		i, j;
   
   FUNC_ENTER (H5O_dtype_debug, FAIL);

   /* check args */
   assert (f);
   assert (dt);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   switch (dt->type) {
   case H5T_INTEGER:
      s = "integer";
      break;
   case H5T_FLOAT:
      s = "floating-point";
      break;
   case H5T_TIME:
      s = "date and time";
      break;
   case H5T_STRING:
      s = "text string";
      break;
   case H5T_BITFIELD:
      s = "bit field";
      break;
   case H5T_OPAQUE:
      s = "opaque";
      break;
   case H5T_COMPOUND:
      s = "compound";
      break;
   default:
      sprintf (buf, "H5T_CLASS_%d", (int)(dt->type));
      s = buf;
      break;
   }
   fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	    "Type class:",
	    s);

   fprintf (stream, "%*s%-*s %lu byte%s\n", indent, "", fwidth,
	    "Size:",
	    (unsigned long)(dt->size), 1==dt->size?"":"s");

   if (H5T_COMPOUND==dt->type) {
      fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	       "Number of members:",
	       dt->u.compnd.nmembs);
      for (i=0; i<dt->u.compnd.nmembs; i++) {
	 sprintf (buf, "Member %d:", i);
	 fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
		  buf,
		  dt->u.compnd.memb[i].name);
	 fprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX (0, fwidth-3),
		  "Byte offset:",
		  (unsigned long)(dt->u.compnd.memb[i].offset));
	 fprintf (stream, "%*s%-*s %d%s\n", indent+3, "", MAX (0, fwidth-3),
		  "Dimensionality:",
		  dt->u.compnd.memb[i].ndims,
		  0==dt->u.compnd.memb[i].ndims?" (scalar)":"");
	 if (dt->u.compnd.memb[i].ndims>0) {
	    fprintf (stream, "%*s%-*s {", indent+3, "", MAX (0, fwidth-3),
		     "Size:");
	    for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
	       fprintf (stream, "%s%lu", j?", ":"",
			(unsigned long)(dt->u.compnd.memb[i].dim[j]));
	    }
	    fprintf (stream, "}\n");
	    fprintf (stream, "%*s%-*s {", indent+3, "", MAX (0, fwidth-3),
		     "Permutation:");
	    for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
	       fprintf (stream, "%s%lu", j?", ":"",
			(unsigned long)(dt->u.compnd.memb[i].perm[j]));
	    }
	    fprintf (stream, "}\n");
	 }
	 H5O_dtype_debug (f, &(dt->u.compnd.memb[i].type), stream,
			  indent+3, MAX (0, fwidth-3));
      }
   } else {
      switch (dt->u.atomic.order) {
      case H5T_ORDER_LE:
	 s = "little endian";
	 break;
      case H5T_ORDER_BE:
	 s = "big endian";
	 break;
      case H5T_ORDER_VAX:
	 s = "VAX";
	 break;
      case H5T_ORDER_NONE:
	 s = "none";
	 break;
      default:
	 sprintf (buf, "H5T_ORDER_%d", dt->u.atomic.order);
	 s = buf;
	 break;
      }
      fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	       "Byte order:",
	       s);
      
      fprintf (stream, "%*s%-*s %lu bit%s\n", indent, "", fwidth,
	       "Precision:",
	       (unsigned long)(dt->u.atomic.prec),
	       1==dt->u.atomic.prec?"":"s");

      fprintf (stream, "%*s%-*s %lu bit%s\n", indent, "", fwidth,
	       "Offset:",
	       (unsigned long)(dt->u.atomic.offset),
	       1==dt->u.atomic.offset?"":"s");

      switch (dt->u.atomic.lsb_pad) {
      case H5T_PAD_ZERO:
	 s = "zero";
	 break;
      case H5T_PAD_ONE:
	 s = "one";
	 break;
      default:
	 s = "pad?";
	 break;
      }
      fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	       "Low pad type:", s);

      switch (dt->u.atomic.msb_pad) {
      case H5T_PAD_ZERO:
	 s = "zero";
	 break;
      case H5T_PAD_ONE:
	 s = "one";
	 break;
      default:
	 s = "pad?";
	 break;
      }
      fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	       "High pad type:", s);

      if (H5T_FLOAT==dt->type) {
	 switch (dt->u.atomic.u.f.pad) {
	 case H5T_PAD_ZERO:
	    s = "zero";
	    break;
	 case H5T_PAD_ONE:
	    s = "one";
	    break;
	 default:
	    if (dt->u.atomic.u.f.pad<0) {
	       sprintf (buf, "H5T_PAD_%d", -(dt->u.atomic.u.f.pad));
	    } else {
	       sprintf (buf, "bit-%d", dt->u.atomic.u.f.pad);
	    }
	    s = buf;
	    break;
	 }
	 fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
		  "Internal pad type:", s);

	 switch (dt->u.atomic.u.f.norm) {
	 case H5T_NORM_IMPLIED:
	    s = "implied";
	    break;
	 case H5T_NORM_MSBSET:
	    s = "msb set";
	    break;
	 case H5T_NORM_NONE:
	    s = "none";
	    break;
	 default:
	    sprintf (buf, "H5T_NORM_%d", (int)(dt->u.atomic.u.f.norm));
	    s = buf;
	 }
	 fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
		  "Normalization:", s);
	 
	 fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
		  "Sign bit location:",
		  (unsigned long)(dt->u.atomic.u.f.sign));

	 fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
		  "Exponent location:",
		  (unsigned long)(dt->u.atomic.u.f.epos));

	 fprintf (stream, "%*s%-*s 0x%08lx\n", indent, "", fwidth,
		  "Exponent bias:",
		  (unsigned long)(dt->u.atomic.u.f.ebias));

	 fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
		  "Exponent size:",
		  (unsigned long)(dt->u.atomic.u.f.esize));

	 fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
		  "Mantissa location:",
		  (unsigned long)(dt->u.atomic.u.f.mpos));

	 fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
		  "Mantissa size:",
		  (unsigned long)(dt->u.atomic.u.f.msize));
	 
      } else if (H5T_INTEGER==dt->type) {
	 switch (dt->u.atomic.u.i.sign) {
	 case H5T_SGN_NONE:
	    s = "none";
	    break;
	 case H5T_SGN_2:
	    s = "2's comp";
	    break;
	 default:
	    sprintf (buf, "H5T_SGN_%d", (int)(dt->u.atomic.u.i.sign));
	    s = buf;
	    break;
	 }
	 fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
		  "Sign scheme:", s);
	 
      }
   }

   FUNC_LEAVE (SUCCEED);
}
