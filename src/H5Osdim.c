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
            for(u=0; u<sdim->rank; u++)
                UINT32DECODE(p,sdim->size[u]);
            if(sdim->dim_flags&0x01)
              {
                sdim->max=H5MM_xmalloc(sizeof(uint32)*sdim->rank);
                for(u=0; u<sdim->rank; u++)
                    UINT32DECODE(p,sdim->max[u]);
              } /* end if */
            if(sdim->dim_flags&0x02)
              {
                sdim->perm=H5MM_xmalloc(sizeof(uint32)*sdim->rank);
                for(u=0; u<sdim->rank; u++)
                    UINT32DECODE(p,sdim->perm[u]);
              } /* end if */
          } /* end if */
      } /* end if */

#ifdef LATER
done:
#endif /* LATER */
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
    assert (sdim);

    /* encode */
    UINT32ENCODE(p,sdim->rank);
    UINT32ENCODE(p,sdim->dim_flags);
    if(sdim->rank>0)
      {
        for(u=0; u<sdim->rank; u++)
            UINT32ENCODE(p,sdim->size[u]);
        if(sdim->dim_flags&0x01)
          {
            for(u=0; u<sdim->rank; u++)
                UINT32ENCODE(p,sdim->max[u]);
          } /* end if */
        if(sdim->dim_flags&0x02)
          {
            for(u=0; u<sdim->rank; u++)
                UINT32ENCODE(p,sdim->perm[u]);
          } /* end if */
      } /* end if */

    FUNC_LEAVE (SUCCEED);
} /* end H5O_sim_dim_encode() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dim_fast
 PURPOSE
    Initializes a new simple dimensionality struct with info from a symbol
        table entry.
 USAGE
    void *H5O_sim_dim_fast(ent, mesg)
        const H5G_entry_t *ent; IN: pointer to the symbol table entry
        const void *mesg;       IN: Pointer to the simple dimensionality struct
 RETURNS
    Pointer to the message structure (allocated if none is supplied) on success,
        NULL on failure
 DESCRIPTION
        This function fills the native memory form of the simple dimensionality
    message from a symbol-table entry cache fields.  (This method is required
    for simple dimensionality, as they can be cached in the symbol-table entry)
--------------------------------------------------------------------------*/
static void *
H5O_sim_dim_fast (const H5G_entry_t *ent, void *mesg)
{
   H5O_sim_dim_t *sdim = (H5O_sim_dim_t *)mesg;
   uintn u;                    /* local counting variable */
   
   FUNC_ENTER (H5O_sim_dim_fast, NULL, NULL);

   /* check args */
   assert (ent);

   if (H5G_CACHED_SDATA==ent->type) {
      if (!sdim) sdim = H5MM_xcalloc (1, sizeof(H5O_sim_dim_t));
      sdim->rank = ent->cache.sdata.ndim;
      assert (sdim->rank<=NELMTS (ent->cache.sdata.dim));
      sdim->dim_flags = 0;
      sdim->size = H5MM_xmalloc (sizeof(uint32) * sdim->rank);
      for (u=0; u<sdim->rank; u++) {
	 sdim->size[u] = ent->cache.sdata.dim[u];
      }
   } else {
      sdim = NULL;
   }
   
   FUNC_LEAVE (sdim);
} /* end H5O_sim_dim_fast() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dim_cache
 PURPOSE
    Copies a simple dimensionality message into the cache portion of a symbol
        table entry.
 USAGE
    hbool_t H5O_sim_dim_cache(ent, mesg)
        const H5G_entry_t *ent; IN: Pointer to the symbol table entry
        const void *mesg;       IN: Pointer to the simple dimensionality struct
 RETURNS
    BTRUE if symbol-table modified, BFALSE if not modified, BFAIL on failure.
 DESCRIPTION
        This function is the opposite of the H5O_sim_dim_fast method, it
    copies a message into the cached portion of a symbol-table entry.  (This
    method is required for simple dimensionalities, as they can be cached in
    the symbol-table entry)
--------------------------------------------------------------------------*/
static hbool_t
H5O_sim_dim_cache (H5G_entry_t *ent, const void *mesg)
{
    const H5O_sim_dim_t *sdim = (const H5O_sim_dim_t *)mesg;
    uintn u;        /* Local counting variable */
    hbool_t modified = BFALSE;
   
    FUNC_ENTER (H5O_sim_dim_cache, NULL, BFAIL);

    /* check args */
    assert (ent);
    assert (sdim);

    if (sdim->rank <= NELMTS (ent->cache.sdata.dim)) {
       if (H5G_CACHED_SDATA != ent->type) {
	  modified = BTRUE;
	  ent->type = H5G_CACHED_SDATA;
	  ent->cache.sdata.ndim = sdim->rank;
	  for (u=0; u<=sdim->rank; u++) {
	     ent->cache.sdata.dim[u] = sdim->size[u];
	  }
       } else {
	  if(ent->cache.sdata.ndim!= sdim->rank) {
	     modified = BTRUE;
	     ent->cache.sdata.ndim = sdim->rank;
          }

	  /* Check each dimension */
	  if (NULL==ent->cache.sdata.dim) {
	     modified = BTRUE;
	  } else {
	     for (u=0; u<sdim->rank; u++) {
                if (ent->cache.sdata.dim[u] != sdim->size[u]) {
                   modified = BTRUE;
                   ent->cache.sdata.dim[u] = sdim->size[u];
		}
	     }
          }
       }
    } else if (H5G_CACHED_SDATA == ent->type) {
       /*
        * Number of dimensions is too large to cache.
        */
       modified = TRUE;
       ent->type = H5G_NOTHING_CACHED;
    }
       
    FUNC_LEAVE (modified);
} /* end H5O_sim_dim_cache() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dim_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O_sim_dim_copy(mesg, dest)
        const void *mesg;       IN: Pointer to the source simple dimensionality struct
        const void *dest;       IN: Pointer to the destination simple dimensionality struct
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
        This function copies a native (memory) simple dimensionality message,
    allocating the destination structure if necessary.
--------------------------------------------------------------------------*/
static void *
H5O_sim_dim_copy (const void *mesg, void *dest)
{
   const H5O_sim_dim_t	*src = (const H5O_sim_dim_t *)mesg;
   H5O_sim_dim_t		*dst = (H5O_sim_dim_t *)dest;
   
   FUNC_ENTER (H5O_sim_dim_copy, NULL, NULL);

   /* check args */
   assert (src);
   if (!dst)
       dst = H5MM_xcalloc (1, sizeof(H5O_sim_dim_t));

   /* copy */
   HDmemcpy(dst,src,sizeof(H5O_sim_dim_t));
   if(src->rank>0)
     {
       dst->size = H5MM_xcalloc (src->rank, sizeof(uint32));
       HDmemcpy(dst->size,src->size,src->rank*sizeof(uint32));
       /* Check for maximum dimensions and copy those */
       if((src->dim_flags&0x01)>0)
         {
           dst->max = H5MM_xcalloc (src->rank, sizeof(uint32));
           HDmemcpy(dst->max,src->max,src->rank*sizeof(uint32));
         } /* end if */
       /* Check for dimension permutation and copy those */
       if((src->dim_flags&0x02)>0)
         {
           dst->perm = H5MM_xcalloc (src->rank, sizeof(uint32));
           HDmemcpy(dst->perm,src->perm,src->rank*sizeof(uint32));
         } /* end if */
     } /* end if */

   FUNC_LEAVE ((void*)dst);
} /* end H5O_sim_dim_copy() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dim_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    void *H5O_sim_dim_copy(f, mesg)
        hdf5_file_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source simple dimensionality struct
 RETURNS
    Size of message on success, FAIL on failure
 DESCRIPTION
        This function returns the size of the raw simple dimensionality message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.
--------------------------------------------------------------------------*/
static size_t
H5O_sim_dim_size (hdf5_file_t *f, const void *mesg)
{
   const H5O_sim_dim_t	*sdim = (const H5O_sim_dim_t *)mesg;
   size_t ret_value=8;  /* all dimensionality messages are at least 8 bytes long (rank and flags) */

   FUNC_ENTER (H5O_sim_dtype_size, NULL, FAIL);

   ret_value+=sdim->rank*4; /* add in the dimension sizes */
   ret_value+=((sdim->dim_flags&0x01)>0)*sdim->rank*4;  /* add in the space for the maximum dimensions, if they are present */
   ret_value+=((sdim->dim_flags&0x02)>0)*sdim->rank*4;  /* add in the space for the dimension permutations, if they are present */

   FUNC_LEAVE (ret_value);
} /* end H5O_sim_dim_size() */

/*--------------------------------------------------------------------------
 NAME
    H5O_sim_dim_debug
 PURPOSE
    Prints debugging information for a simple dimensionality message
 USAGE
    void *H5O_sim_dim_debug(f, mesg, stream, indent, fwidth)
        hdf5_file_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source simple dimensionality struct
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
H5O_sim_dim_debug (hdf5_file_t *f, const void *mesg, FILE *stream,
		intn indent, intn fwidth)
{
   const H5O_sim_dim_t	*sdim = (const H5O_sim_dim_t *)mesg;
   uintn u;     /* local counting variable */
   
   FUNC_ENTER (H5O_sim_dim_debug, NULL, FAIL);

   /* check args */
   assert (f);
   assert (sdim);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Rank:",
	    (unsigned long)(sdim->rank));
   fprintf (stream, "%*s%-*s %lx\n", indent, "", fwidth,
	    "Flags:",
	    (unsigned long)(sdim->dim_flags));
    for(u=0; u<sdim->rank; u++)
       fprintf (stream, "%*s%-*s %lx\n", indent, "", fwidth,
            "Dim Size:",
            (unsigned long)(sdim->size[u]));
    if(sdim->dim_flags&0x01)
        for(u=0; u<sdim->rank; u++)
           fprintf (stream, "%*s%-*s %lx\n", indent, "", fwidth,
                "Dim Max:",
                (unsigned long)(sdim->max[u]));
    if(sdim->dim_flags&0x02)
        for(u=0; u<sdim->rank; u++)
           fprintf (stream, "%*s%-*s %lx\n", indent, "", fwidth,
                "Dim Perm:",
                (unsigned long)(sdim->perm[u]));

   FUNC_LEAVE (SUCCEED);
} /* end H5O_sim_dim_debug() */

