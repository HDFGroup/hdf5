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

/*LINTLIBRARY */
/*+
   FILE
       H5Osdim.c
   HDF5 Simple Dimensionality Object Header Message routines

   EXPORTED ROUTINES

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
   + */

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_sim_dim_mask

/* PRIVATE PROTOTYPES */
static void *H5O_sim_dim_decode (hdf5_file_t *f, size_t raw_size, const uint8 *p);
static herr_t H5O_sim_dim_encode (hdf5_file_t *f, size_t size, uint8 *p,
			       const void *_mesg);
static void *H5O_sim_dim_fast (const H5G_entry_t *ent, void *_mesg);
static hbool_t H5O_sim_dim_cache (H5G_entry_t *ent, const void *_mesg);
static void *H5O_sim_dim_copy (const void *_mesg, void *_dest);
static size_t H5O_sim_dim_size (hdf5_file_t *f, const void *_mesg);
static herr_t H5O_sim_dim_debug (hdf5_file_t *f, const void *_mesg,
			      FILE *stream, intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_SIM_DIM[1] = {{
   H5O_SIM_DIM_ID,          /* message id number */
   "sim_dim",               /* message name for debugging */
   sizeof (H5O_sim_dim_t),  /* native message size */
   H5O_sim_dim_decode,      /* decode message */
   H5O_sim_dim_encode,      /* encode message */
   H5O_sim_dim_fast,        /* get message from stab entry */
   H5O_sim_dim_cache,       /* put message into stab entry */
   H5O_sim_dim_copy,        /* copy the native value */
   H5O_sim_dim_size,        /* size of symbol table entry */
   NULL,                    /* default reset method */
   H5O_sim_dim_debug,       /* debug the message */
}};

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dim_decode
 PURPOSE
    Decode a simple dimensionality message and return a pointer to a memory
        struct with the decoded information
 USAGE
    void *H5O_sim_dim_decode(f, raw_size, p)
        hdf5_file_t *f;         IN: pointer to the HDF5 file struct
        size_t raw_size;        IN: size of the raw information buffer
        const uint8 *p;         IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
        This function decodes the "raw" disk form of a simple dimensionality
    message into a struct in memory native format.  The struct is allocated
    within this function using malloc() and is returned to the caller.
--------------------------------------------------------------------------*/
static void *
H5O_sim_dim_decode (hdf5_file_t *f, size_t raw_size, const uint8 *p)
{
    H5O_sim_dim_t *sdim=NULL;   /* New simple dimensionality structure */
    uintn u;                    /* local counting variable */
   
    FUNC_ENTER (H5O_sim_dim_decode, NULL, NULL);

    /* check args */
    assert (f);
    assert (raw_size >= 8); /* at least the rank and flags must be present */
    assert (p);

    /* decode */
    if((sdim = H5MM_xcalloc (1, sizeof(H5O_sim_dim_t)))!=NULL)
      {
        UINT32DECODE(p,sdim->rank);
        UINT32DECODE(p,sdim->dim_flags);
        if(sdim->rank>0)
          {
            sdim->size=H5MM_xmalloc(sizeof(uint32)*sdim->rank);
            for(u=0; u<sdim->size; u++)
                UINT32DECODE(p,sdim->size[u]);
            if(sdim->dim_flags&0x01)
              {
                sdim->max=H5MM_xmalloc(sizeof(uint32)*sdim->rank);
                for(u=0; u<sdim->size; u++)
                    UINT32DECODE(p,sdim->max[u]);
              } /* end if */
            if(sdim->dim_flags&0x02)
              {
                sdim->perm=H5MM_xmalloc(sizeof(uint32)*sdim->rank);
                for(u=0; u<sdim->size; u++)
                    UINT32DECODE(p,sdim->perm[u]);
              } /* end if */
          } /* end if */
      } /* end if */

done:
  if(sdim == NULL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE (sdim);
} /* end H5O_sim_dim_decode() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dim_encode
 PURPOSE
    Encode a simple dimensionality message 
 USAGE
    herr_t H5O_sim_dim_encode(f, raw_size, p, mesg)
        hdf5_file_t *f;         IN: pointer to the HDF5 file struct
        size_t raw_size;        IN: size of the raw information buffer
        const uint8 *p;         IN: the raw information buffer
        const void *mesg;       IN: Pointer to the simple dimensionality struct
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function encodes the native memory form of the simple
    dimensionality message in the "raw" disk form.
--------------------------------------------------------------------------*/
static herr_t
H5O_sim_dim_encode (hdf5_file_t *f, size_t raw_size, uint8 *p, const void *mesg)
{
    const H5O_sim_dim_t *sdim = (const H5O_sim_dim_t *)mesg;
    uintn u;        /* Local counting variable */

    FUNC_ENTER (H5O_sim_dim_encode, NULL, FAIL);

    /* check args */
    assert (f);
    assert (raw_size >= 8); /* at least the rank & flags must be present */
    assert (p);
    assert (sdtype);

    /* encode */
    UINT32ENCODE(p,sdim->rank);
    UINT32ENCODE(p,sdim->dim_flags);
    if(sdim->rank>0)
      {
        for(u=0; u<=sdim->rank; u++);
            UINT32ENCODE(p,sdim->size[u]);
        if(sdim->dim_flags&0x01)
          {
            for(u=0; u<=sdim->rank; u++);
                UINT32ENCODE(p,sdim->max[u]);
          } /* end if */
      } /* end if */

    FUNC_LEAVE (SUCCEED);
} /* end H5O_sim_dtype_encode() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dtype_fast
 PURPOSE
    Initializes a new simple datatype struct with info from a symbol table
        entry.
 USAGE
    void *H5O_sim_dtype_fast(ent, mesg)
        const H5G_entry_t *ent; IN: pointer to the symbol table entry
        const void *mesg;       IN: Pointer to the simple datatype struct
 RETURNS
    Pointer to the message structure (allocated if none is supplied) on success,
        NULL on failure
 DESCRIPTION
        This function fills the native memory form of the simple datatype
    message from a symbol-table entry cache fields.  (This method is required
    for simple datatypes, as they can be cached in the symbol-table entry)
--------------------------------------------------------------------------*/
static void *
H5O_sim_dtype_fast (const H5G_entry_t *ent, void *mesg)
{
   H5O_sim_dtype_t *sdtype = (H5O_sim_dtype_t *)mesg;
   const uint8 *p;
   
   FUNC_ENTER (H5O_sim_dtype_fast, NULL, NULL);

   /* check args */
   assert (ent);

   if (H5G_CACHED_SDATA==ent->type)
     {
      if (!sdtype)
          if((sdtype = H5MM_xcalloc (1, sizeof(H5O_sim_dtype_t)))!=NULL)
            {
              p=&(uint8 *)ent->cache.sdata.nt;
              sdtype->length=*p++;
              sdtype->arch=*p++;
              UINT16DECODE(p,sdtype->type);
            } /* end if */
     } /* end if */
   else
      sdtype = NULL;

   FUNC_LEAVE (sdtype);
} /* end H5O_sim_dtype_fast() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dtype_cache
 PURPOSE
    Copies a simple datatype message into the cache portion of a symbol table
        entry.
 USAGE
    hbool_t H5O_sim_dtype_cache(ent, mesg)
        const H5G_entry_t *ent; IN: Pointer to the symbol table entry
        const void *mesg;       IN: Pointer to the simple datatype struct
 RETURNS
    BTRUE if symbol-table modified, BFALSE if not modified, BFAIL on failure.
 DESCRIPTION
        This function is the opposite of the H5O_sim_dtype_fast method, it
    copies a message into the cached portion of a symbol-table entry.  (This
    method is required for simple datatypes, as they can be cached in the
    symbol-table entry)
--------------------------------------------------------------------------*/
static hbool_t
H5O_sim_dtype_cache (H5G_entry_t *ent, const void *mesg)
{
    const H5O_sim_dtype_t *sdtype = (const H5O_sim_dtype_t *)mesg;
    const uint8 *p;
    hbool_t modified = BFALSE;
   
    FUNC_ENTER (H5O_sim_dtype_cache, NULL, BFAIL);

    /* check args */
    assert (ent);
    assert (sdtype);

    /*
     * We do this in two steps so Purify doesn't complain about
     * uninitialized memory reads even though they don't bother
     * anything.
     */
    p=(uint8 *)&(ent->cache.sdata);
    if (H5G_CACHED_SDATA != ent->type)
      {
        modified = BTRUE;
        ent->type = H5G_CACHED_SDATA;
        *p++=sdtype->length;
        *p++=sdtype->arch;
        UINT16ENCODE(p,sdtype->type);
      } /* end if */
    else
      {
        if(ent->cache.sdata.length= sdtype->length)
          {
            modified = BTRUE;
            ent->cache.sdata.length = sdtype->length;
          } /* end if */

        if (ent->cache.sdata.arch != sdtype->arch)
          {
           modified = BTRUE;
           ent->cache.sdata.arch = sdtype->arch;
          } /* end if */

        if (ent->cache.sdata.type != sdtype->type)
          {
           modified = BTRUE;
           ent->cache.sdata.type = sdtype->type;
          } /* end if */
      } /* end else */

    FUNC_LEAVE (modified);
} /* end H5O_sim_dtype_cache() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dtype_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O_sim_dtype_copy(mesg, dest)
        const void *mesg;       IN: Pointer to the source simple datatype struct
        const void *dest;       IN: Pointer to the destination simple datatype struct
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
        This function copies a native (memory) simple datatype message,
    allocating the destination structure if necessary.
--------------------------------------------------------------------------*/
static void *
H5O_sim_dtype_copy (const void *mesg, void *dest)
{
   const H5O_sim_dtype_t	*src = (const H5O_sim_dtype_t *)mesg;
   H5O_sim_dtype_t		*dst = (H5O_sim_dtype_t *)dest;
   
   FUNC_ENTER (H5O_sim_dtype_copy, NULL, NULL);

   /* check args */
   assert (src);
   if (!dst) dst = H5MM_xcalloc (1, sizeof(H5O_sim_dtype_t));

   /* copy */
   HDmempcy(dst,src,sizeof(H5O_sim_dtype_t));

   FUNC_LEAVE ((void*)dst);
} /* end H5O_sim_dtype_copy() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dtype_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    void *H5O_sim_dtype_copy(f, mesg)
        hdf5_file_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source simple datatype struct
 RETURNS
    Size of message on success, FAIL on failure
 DESCRIPTION
        This function returns the size of the raw simple datatype message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.
--------------------------------------------------------------------------*/
static size_t
H5O_sim_dtype_size (hdf5_file_t *f, const void *mesg)
{
   FUNC_ENTER (H5O_sim_dtype_size, NULL, FAIL);
   FUNC_LEAVE (4);
} /* end H5O_sim_dtype_size() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dtype_debug
 PURPOSE
    Prints debugging information for a simple datatype message
 USAGE
    void *H5O_sim_dtype_debug(f, mesg, stream, indent, fwidth)
        hdf5_file_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source simple datatype struct
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
H5O_sim_dtype_debug (hdf5_file_t *f, const void *mesg, FILE *stream,
		intn indent, intn fwidth)
{
   const H5O_sim_dtype_t	*sdtype = (const H5O_sim_dtype_t *)mesg;
   
   FUNC_ENTER (H5O_sim_dtype_debug, NULL, FAIL);

   /* check args */
   assert (f);
   assert (sdtype);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Length:",
	    (unsigned long)(sdtype->length));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Architecture:",
	    (unsigned long)(sdtype->arch));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Data-Type:",
	    (unsigned long)(sdtype->type));

   FUNC_LEAVE (SUCCEED);
} /* end H5O_sim_dtype_debug() */

