/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5O.c
 * 			Aug  5 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Object header virtual functions.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include <H5private.h>
#include <H5ACprivate.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_mask

/* PRIVATE PROTOTYPES */
static herr_t H5O_flush (hdf5_file_t *f, hbool_t destroy, haddr_t addr,
			 H5O_t *oh);
static H5O_t *H5O_load (hdf5_file_t *f, haddr_t addr, const void *_data);
static intn H5O_find_in_ohdr (hdf5_file_t *f, haddr_t addr,
			      const H5O_class_t **type_p, intn sequence);
static intn H5O_alloc (hdf5_file_t *f, H5O_t *oh, const H5O_class_t *type,
		       size_t size);
static intn H5O_alloc_extend_chunk (H5O_t *oh, intn chunkno, size_t size);
static intn H5O_alloc_new_chunk (hdf5_file_t *f, H5O_t *oh, size_t size);

/* H5O inherits cache-like properties from H5AC */
static const H5AC_class_t H5AC_OHDR[1] = {{
   (void*(*)(hdf5_file_t*,haddr_t,const void*))H5O_load,
   (herr_t(*)(hdf5_file_t*,hbool_t,haddr_t,void*))H5O_flush,
}};

/* Is the interface initialized? */
static intn interface_initialize_g = FALSE;

/* ID to type mapping */
static const H5O_class_t *const message_type_g[] = {
   H5O_NULL,		/*0x0000 Null 					*/
   H5O_SIM_DIM,		/*0x0001 Simple dimensionality			*/
   NULL,		/*0x0002 Data space (fiber bundle?)		*/
   H5O_SIM_DTYPE,	/*0x0003 Simple data type			*/
   NULL,		/*0x0004 Compound data type			*/
   NULL,		/*0x0005 Data storage -- standard object	*/
   NULL,		/*0x0006 Data storage -- compact object		*/
   NULL,		/*0x0007 Data storage -- external object	*/
   NULL,		/*0x0008 Data storage -- indexed object		*/
   NULL,		/*0x0009 Data storage -- chunked object		*/
   NULL,		/*0x000A Data storage -- sparse object		*/
   NULL,		/*0x000B Data storage -- compressed object	*/
   NULL,		/*0x000C Attribute list				*/
   H5O_NAME,		/*0x000D Object name				*/
   NULL,		/*0x000E Object modification date and time	*/
   NULL,		/*0x000F Shared header message			*/
   H5O_CONT,		/*0x0010 Object header continuation		*/
   H5O_STAB,		/*0x0011 Symbol table				*/
};


/*-------------------------------------------------------------------------
 * Function:	H5O_new
 *
 * Purpose:	Creates a new object header, sets the link count
 *		to NLINK, and caches the header.
 *
 * Return:	Success:	Address of new header.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5O_new (hdf5_file_t *f, intn nlink, size_t size_hint)
{
   size_t	size;		/*total size of object header	*/
   haddr_t	addr = FAIL;	/*address of object header	*/
   H5O_t	*oh = NULL;

   FUNC_ENTER (H5O_new, NULL, FAIL);

   /* check args */
   assert (f);
   assert (nlink>=0);
   if (size_hint<H5O_MIN_SIZE) size_hint = H5O_MIN_SIZE;
   H5O_ALIGN (size_hint, H5O_ALIGNMENT);

   /* allocate disk space for header and first chunk */
   size = H5O_SIZEOF_HDR(f) + size_hint;
   if ((addr = H5MF_alloc (f, size))<0) {
      HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL);
   }

   /* allocate the object header in fill in header fields */
   oh = H5MM_xcalloc (1, sizeof(H5O_t));
   oh->dirty = TRUE;
   oh->version = H5O_VERSION;
   oh->alignment = H5O_ALIGNMENT;
   oh->nlink = nlink;

   /* create the chunk list and initialize the first chunk */
   oh->nchunks = 1;
   oh->alloc_nchunks = H5O_NCHUNKS;
   oh->chunk = H5MM_xmalloc (oh->alloc_nchunks * sizeof (H5O_chunk_t));
   
   oh->chunk[0].dirty = TRUE;
   oh->chunk[0].addr = addr + H5O_SIZEOF_HDR(f);
   oh->chunk[0].size = size_hint;
   oh->chunk[0].image = H5MM_xmalloc (size_hint);

   /* create the message list and initialize the first message */
   oh->nmesgs = 1;
   oh->alloc_nmesgs = H5O_NMESGS;
   oh->mesg = H5MM_xcalloc (oh->alloc_nmesgs, sizeof(H5O_mesg_t));

   oh->mesg[0].type = H5O_NULL;
   oh->mesg[0].dirty = TRUE;
   oh->mesg[0].native = NULL;
   oh->mesg[0].raw = oh->chunk[0].image + 4; /*skip id and size fields */
   oh->mesg[0].raw_size = size_hint - 4;
   oh->mesg[0].chunkno = 0;

   /* cache it */
   if (H5AC_set (f, H5AC_OHDR, addr, oh)<0) {
      H5MM_xfree (oh);
      HRETURN_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL);
   }

   FUNC_LEAVE (addr);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_load
 *
 * Purpose:	Loads an object header from disk.
 *
 * Return:	Success:	Pointer to the new object header.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 30 Aug 1997
 *	Plugged memory leaks that occur during error handling.
 *
 *-------------------------------------------------------------------------
 */
static H5O_t *
H5O_load (hdf5_file_t *f, haddr_t addr, const void *_data)
{
   H5O_t	*oh = NULL;
   H5O_t	*ret_value = (void*)SUCCEED; /*kludge for HGOTO_ERROR*/
   uint8	buf[16], *p;
   size_t	hdr_size, mesg_size;
   uintn	id;
   intn		mesgno, chunkno, curmesg=0, nmesgs;
   haddr_t	chunk_addr;
   size_t	chunk_size;
   H5O_cont_t	*cont=NULL;

   FUNC_ENTER (H5O_load, NULL, NULL);

   /* check args */
   assert (f);
   assert (addr>=0);
   assert (!_data);

   /* allocate ohdr and init chunk list */
   oh = H5MM_xcalloc (1, sizeof(H5O_t));

   /* read fixed-lenth part of object header */
   hdr_size = H5O_SIZEOF_HDR (f);
   if (H5F_block_read (f, addr, hdr_size, buf)<0) {
      HGOTO_ERROR (H5E_OHDR, H5E_READERROR, NULL);
   }
   p = buf;

   /* decode version */
   oh->version = *p++;
   if (H5O_VERSION!=oh->version) {
      HGOTO_ERROR (H5E_OHDR, H5E_VERSION, NULL);
   }

   /* decode alignment */
   oh->alignment = *p++;
   if (4!=oh->alignment) {
      HGOTO_ERROR (H5E_OHDR, H5E_ALIGNMENT, NULL);
   }

   /* decode number of messages */
   UINT16DECODE (p, nmesgs);

   /* decode link count */
   UINT32DECODE (p, oh->nlink);

   /* decode first chunk info */
   chunk_addr = addr + H5O_SIZEOF_HDR(f);
   UINT32DECODE (p, chunk_size);

   /* build the message array */
   oh->alloc_nmesgs = MAX (H5O_NMESGS, nmesgs);
   oh->mesg = H5MM_xcalloc (oh->alloc_nmesgs, sizeof(H5O_mesg_t));
      
   /* read each chunk from disk */
   while (chunk_addr) {

      /* increase chunk array size */
      if (oh->nchunks>=oh->alloc_nchunks) {
	 oh->alloc_nchunks += H5O_NCHUNKS;
	 oh->chunk = H5MM_xrealloc (oh->chunk,
				    oh->alloc_nchunks * sizeof(H5O_chunk_t));
      }

      /* read the chunk raw data */
      chunkno = oh->nchunks++;
      oh->chunk[chunkno].dirty = FALSE;
      oh->chunk[chunkno].addr = chunk_addr;
      oh->chunk[chunkno].size = chunk_size;
      oh->chunk[chunkno].image = H5MM_xmalloc (chunk_size);
      if (H5F_block_read (f, chunk_addr, chunk_size,
			  oh->chunk[chunkno].image)<0) {
	 HGOTO_ERROR (H5E_OHDR, H5E_READERROR, NULL);
      }
      

      /* load messages from this chunk */
      for (p = oh->chunk[chunkno].image;
	   p < oh->chunk[chunkno].image + chunk_size;
	   p += mesg_size) {
	 UINT16DECODE (p, id);
	 UINT16DECODE (p, mesg_size);
      
	 if (id>=NELMTS(message_type_g) || NULL==message_type_g[id]) {
	    HGOTO_ERROR (H5E_OHDR, H5E_BADMESG, NULL);
	 }
	 if (p + mesg_size > oh->chunk[chunkno].image + chunk_size) {
	    HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, NULL);
	 }

	 if (H5O_NULL_ID==id && oh->nmesgs>0 &&
	     H5O_NULL_ID==oh->mesg[oh->nmesgs-1].type->id &&
	     oh->mesg[oh->nmesgs-1].chunkno==chunkno) {
	    /* combine adjacent null messages */
	    mesgno = oh->nmesgs - 1;
	    oh->mesg[mesgno].raw_size += 4 + mesg_size;
	 } else {
	    /* new message */
	    if (oh->nmesgs>=nmesgs) {
	       HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, NULL);
	    }
	    mesgno = oh->nmesgs++;
	    oh->mesg[mesgno].type = message_type_g[id];
	    oh->mesg[mesgno].dirty = FALSE;
	    oh->mesg[mesgno].native = NULL;
	    oh->mesg[mesgno].raw = p;
	    oh->mesg[mesgno].raw_size = mesg_size;
	    oh->mesg[mesgno].chunkno = chunkno;
	 }
      }
      assert (p == oh->chunk[chunkno].image + chunk_size);

      /* decode next object header continuation message */
      for (chunk_addr=0; 0==chunk_addr && curmesg<oh->nmesgs; curmesg++) {
	 if (H5O_CONT_ID==oh->mesg[curmesg].type->id) {
	    uint8 *p2 = oh->mesg[curmesg].raw;
	    cont = (H5O_CONT->decode)(f, oh->mesg[curmesg].raw_size, p2);
	    oh->mesg[curmesg].native = cont;
	    chunk_addr = cont->addr;
	    chunk_size = cont->size;
	    cont->chunkno = oh->nchunks; /*the next chunk to allocate*/
	 }
      }
   }

done:
   if (!ret_value && oh) {
      /*
       * Free resources.
       */
      int i;
      for (i=0; i<oh->nchunks; i++) {
	 oh->chunk[i].image = H5MM_xfree (oh->chunk[i].image);
      }
      oh->chunk = H5MM_xfree (oh->chunk);
      oh->mesg = H5MM_xfree (oh->mesg);
      oh = H5MM_xfree (oh);
   }

   FUNC_LEAVE (oh);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_flush
 *
 * Purpose:	Flushes (and destroys) an object header.
 *
 * Return:	Success:	SUCCESS
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_flush (hdf5_file_t *f, hbool_t destroy, haddr_t addr, H5O_t *oh)
{
   uint8	buf[16], *p;
   int		i;
   H5O_cont_t	*cont = NULL;
   
   FUNC_ENTER (H5O_flush, NULL, FAIL);

   /* check args */
   assert (f);
   assert (addr>=0);
   assert (oh);
   
   /* flush */
   if (oh->dirty) {
      p = buf;

      /* encode version */
      *p++ = oh->version;

      /* encode alingment */
      *p++ = oh->alignment;

      /* encode number of messages */
      UINT16ENCODE (p, oh->nmesgs);

      /* encode link count */
      UINT32ENCODE (p, oh->nlink);

      /* encode body size */
      UINT32ENCODE (p, oh->chunk[0].size);

      /* write the object header header */
      if (H5F_block_write (f, addr, H5O_SIZEOF_HDR(f), buf)<0) {
	 HRETURN_ERROR (H5E_OHDR, H5E_WRITEERROR, FAIL);
      }

      /* encode messages */
      for (i=0; i<oh->nmesgs; i++) {
	 if (oh->mesg[i].dirty) {
	    p = oh->mesg[i].raw - 4;
	    UINT16ENCODE (p, oh->mesg[i].type->id);
	    UINT16ENCODE (p, oh->mesg[i].raw_size);
	    if (oh->mesg[i].native) {
	       assert (oh->mesg[i].type->encode);

	       /* allocate file space for chunks that have none yet */
	       if (H5O_CONT_ID==oh->mesg[i].type->id &&
		   ((H5O_cont_t*)(oh->mesg[i].native))->addr<0) {
		  cont = (H5O_cont_t*)(oh->mesg[i].native);
		  assert (cont->chunkno >= 0);
		  assert (cont->chunkno < oh->nchunks);
		  assert (oh->chunk[cont->chunkno].addr<0);
		  cont->size = oh->chunk[cont->chunkno].size;
		  cont->addr = H5MF_alloc (f, cont->size);
		  if (cont->addr<0) {
		     HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL);
		  }
		  oh->chunk[cont->chunkno].addr = cont->addr;
	       }

	       /* encode the message */
	       assert (oh->mesg[i].raw >=
		       oh->chunk[oh->mesg[i].chunkno].image);
	       assert (oh->mesg[i].raw + oh->mesg[i].raw_size <=
		       oh->chunk[oh->mesg[i].chunkno].image +
		       oh->chunk[oh->mesg[i].chunkno].size);
	       if ((oh->mesg[i].type->encode)(f, oh->mesg[i].raw_size,
					      oh->mesg[i].raw,
					      oh->mesg[i].native)<0) {
		  HRETURN_ERROR (H5E_OHDR, H5E_CANTENCODE, FAIL);
	       }
	    }
	    oh->mesg[i].dirty = FALSE;
	    oh->chunk[oh->mesg[i].chunkno].dirty = TRUE;
	 }
      }

      /* write each chunk to disk */
      for (i=0; i<oh->nchunks; i++) {
	 if (oh->chunk[i].dirty) {
	    assert (oh->chunk[i].addr>0);
	    if (H5F_block_write (f, oh->chunk[i].addr, oh->chunk[i].size,
				 oh->chunk[i].image)<0) {
	       HRETURN_ERROR (H5E_OHDR, H5E_WRITEERROR, FAIL);
	    }
	    oh->chunk[i].dirty = FALSE;
	 }
      }
      oh->dirty = FALSE;
   }


   if (destroy) {
      /* destroy chunks */
      for (i=0; i<oh->nchunks; i++) {
	 oh->chunk[i].image = H5MM_xfree (oh->chunk[i].image);
      }
      oh->chunk = H5MM_xfree (oh->chunk);

      /* destroy messages */
      for (i=0; i<oh->nmesgs; i++) {
	 H5O_reset (oh->mesg[i].type, oh->mesg[i].native);
	 oh->mesg[i].native = H5MM_xfree (oh->mesg[i].native);
      }
      oh->mesg = H5MM_xfree (oh->mesg);

      /* destroy object header */
      H5MM_xfree (oh);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_reset
 *
 * Purpose:	Some message data structures have internal fields that
 *		need to be freed.  This function does that if appropriate
 *		but doesn't free NATIVE.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_reset (const H5O_class_t *type, void *native)
{
   FUNC_ENTER (H5O_reset, NULL, FAIL);

   if (native) {
      if (type->reset) {
	 if ((type->reset)(native)<0) {
	    /* reset class method failed */
	    HRETURN_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL);
	 }
      } else {
	 HDmemset (native, 0, type->native_size);
      }
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_link
 *
 * Purpose:	Adjust the link count for an object header by adding
 *		ADJUST to the link count.
 *
 * Return:	Success:       	New link count
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5O_link (hdf5_file_t *f, haddr_t addr, H5G_entry_t *ent, intn adjust)
{
   H5O_t	*oh = NULL;
   
   FUNC_ENTER (H5O_link, NULL, FAIL);

   /* check args */
   assert (f);
   assert (addr>=0);

   /* get header */
   if (NULL==(oh=H5AC_find (f, H5AC_OHDR, addr, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL);
   }

   /* adjust link count */
   if (adjust<0) {
      if (oh->nlink + adjust < 0) {
	 HRETURN_ERROR (H5E_OHDR, H5E_LINKCOUNT, FAIL);
      }
      oh->nlink += adjust;
      if (1==oh->nlink && ent) {
	 fprintf (stderr, "H5O_link: no symbol table entry caching "
		  "(not implemented yet)\n");
      }
   } else {
      oh->nlink += adjust;
      if (oh->nlink>1 && ent) ent->type = H5G_NOTHING_CACHED;
   }
	 

   oh->dirty = TRUE;
   FUNC_LEAVE (oh->nlink);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5O_read
 *
 * Purpose:	Reads a message from an object header and returns a pointer
 *		to it.  The caller will usually supply the memory through
 *		MESG and the return value will be MESG.  But if MESG is
 *		the null pointer, then this function will malloc() memory
 *		to hold the result and return its pointer instead.
 *
 * Return:	Success:	Ptr to message in native format.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_read (hdf5_file_t *f, haddr_t addr, H5G_entry_t *ent,
	  const H5O_class_t *type, intn sequence, void *mesg)
{
   H5O_t	*oh = NULL;
   void		*retval = NULL;
   intn		idx;
   
   FUNC_ENTER (H5O_read, NULL, NULL);

   /* check args */
   assert (f);
   assert (addr>=0);
   assert (sequence>=0);

   /* can we get it from the symbol table? */
   if (ent && H5G_NOTHING_CACHED!=ent->type && type && type->fast) {
      retval = (type->fast)(ent, mesg);
      if (retval) HRETURN (retval);
      H5ECLEAR;
   }

   /* can we get it from the object header? */
   if ((idx = H5O_find_in_ohdr (f, addr, &type, sequence))<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_NOTFOUND, NULL);
   }

   /* copy the message to the user-supplied buffer */
   if (NULL==(oh=H5AC_find (f, H5AC_OHDR, addr, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, NULL);
   }
   retval = (type->copy)(oh->mesg[idx].native, mesg);
   if (!retval) HRETURN_ERROR (H5E_OHDR, H5E_CANTINIT, NULL);

   FUNC_LEAVE (retval);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_find_in_ohdr
 *
 * Purpose:	Find a message in the object header without consulting
 *		a symbol table entry.
 *
 * Return:	Success:	Index number of message.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5O_find_in_ohdr (hdf5_file_t *f, haddr_t addr, const H5O_class_t **type_p,
		  intn sequence)
{
   H5O_t	*oh = NULL;
   int		i;
   
   FUNC_ENTER (H5O_find_in_ohdr, NULL, FAIL);
   
   /* check args */
   assert (f);
   assert (addr>=0);
   assert (type_p);

   /* load the object header */
   if (NULL==(oh=H5AC_find (f, H5AC_OHDR, addr, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL);
   }

   /* scan through the messages looking for the right one */
   for (i=0; i<oh->nmesgs; i++) {
      if (*type_p && (*type_p)->id!=oh->mesg[i].type->id) continue;
      if (--sequence<0) break;
   }
   if (sequence>=0) HRETURN_ERROR (H5E_OHDR, H5E_NOTFOUND, FAIL);
   
   /* decode the message if necessary */
   if (NULL==oh->mesg[i].native) {
      assert (oh->mesg[i].type->decode);
      oh->mesg[i].native = (oh->mesg[i].type->decode)(f,
						      oh->mesg[i].raw_size,
						      oh->mesg[i].raw);
      if (NULL==oh->mesg[i].native) {
	 HRETURN_ERROR (H5E_OHDR, H5E_CANTDECODE, FAIL);
      }
   }

   /*return the message type */
   *type_p = oh->mesg[i].type;

   FUNC_LEAVE (i);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_peek
 *
 * Purpose:	Returns a pointer to a message stored in native format.
 *		The returned memory is read-only, and points directly into
 *		the cache.  It is therefore valid only until the next cache
 *		function is called.
 *
 * Return:	Success:	Ptr to read-only message in native format.
 *				The pointer is guranteed to be valid only
 *				until the next call (directly or indirectly)
 *				to an H5AC function.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const void *
H5O_peek (hdf5_file_t *f, haddr_t addr, const H5O_class_t *type,
	  intn sequence)
{
   intn		idx;
   H5O_t	*oh = NULL;
   
   FUNC_ENTER (H5O_peek, NULL, NULL);

   /* check args */
   assert (f);
   assert (addr>=0);

   if ((idx = H5O_find_in_ohdr (f, addr, &type, sequence))<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_NOTFOUND, NULL);
   }
   if (NULL==(oh=H5AC_find (f, H5AC_OHDR, addr, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, NULL);
   }

   FUNC_LEAVE (oh->mesg[idx].native);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_modify
 *
 * Purpose:	Modifies an existing message or creates a new message.
 *		The object header is at file address ADDR of file F.  An
 *		optional symbol table entry ENT can be supplied in which
 *		case the cache fields in that symbol table are updated if
 *		appropriate.  If the symbol table entry changes then the
 *		optional ENT_MODIFIED arg will point to a non-zero value,
 *		otherwise ENT_MODIFIED isn't changed.
 *
 * 		The OVERWRITE argument is either a sequence number of a
 *		message to overwrite (usually zero) or the constant
 *		H5O_NEW_MESSAGE (-1) to indicate that a new message is to
 *		be created.
 *
 * Return:	Success:	The sequence number of the message that
 *				was modified or created.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5O_modify (hdf5_file_t *f, haddr_t addr, H5G_entry_t *ent,
	    hbool_t *ent_modified, const H5O_class_t *type,
	    intn overwrite, const void *mesg)
{
   H5O_t	*oh = NULL;
   intn		idx, sequence;
   size_t	size;
   
   FUNC_ENTER (H5O_modify, NULL, FAIL);

   /* check args */
   assert (f);
   assert (addr>=0);
   assert (type);
   assert (mesg);

   if (NULL==(oh=H5AC_find (f, H5AC_OHDR, addr, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL);
   }

   /* Count similar messages */
   for (idx=0,sequence=-1; idx<oh->nmesgs; idx++) {
      if (type->id != oh->mesg[idx].type->id) continue;
      if (++sequence==overwrite) break;
   }

   /* Was the right message found? */
   if (overwrite>=0 &&
       (idx>=oh->nmesgs || sequence!=overwrite)) {
      HRETURN_ERROR (H5E_OHDR, H5E_NOTFOUND, FAIL); /*message not found*/
   }

   /* Allocate space for the new message */
   if (overwrite<0) {
      size = (type->raw_size)(f, mesg);
      H5O_ALIGN (size, oh->alignment);
      idx = H5O_alloc (f, oh, type, size);
      if (idx<0) HRETURN_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL);
      sequence++;
   }

   /* Copy the native value into the object header */
   oh->mesg[idx].native = (type->copy)(mesg, oh->mesg[idx].native);
   if (NULL==oh->mesg[idx].native) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL);
   }
   oh->mesg[idx].dirty = TRUE;
   oh->dirty = TRUE;

   /* Copy into the symbol table entry */
   if (oh->nlink<=1 && ent && type->cache) {
      hbool_t modified = (type->cache)(ent, mesg);
      if (modified && ent_modified) *ent_modified = modified;
   }

   FUNC_LEAVE (sequence);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_remove
 *
 * Purpose:	Removes the specified message from the object header.
 *		If sequence is H5O_ALL (-1) then all messages of the
 *		specified type are removed.  Removing a message causes
 *		the sequence numbers to change for subsequent messages of
 *		the same type.
 *
 * 		If the messaage was cached in the symbol table entry then
 *		the type field of the symbol table entry is changed to
 *		H5G_NOTHING_CACHED and the ENT_MODIFIED argument will point
 *		to non-zero (the ENT_MODIFIED argument is unchanged if
 *		the ENT type field doesn't change).
 *
 * 		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 28 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_remove (hdf5_file_t *f, haddr_t addr, H5G_entry_t *ent,
	    hbool_t *ent_modified, const H5O_class_t *type, intn sequence)
{
   H5O_t	*oh = NULL;
   intn		i, seq;
   
   FUNC_ENTER (H5O_remove, NULL, FAIL);

   /* check args */
   assert (f);
   assert (addr>=0);
   assert (type);

   /* load the object header */
   if (NULL==(oh=H5AC_find (f, H5AC_OHDR, addr, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL);
   }

   for (i=seq=0; i<oh->nmesgs; i++) {
      if (type->id != oh->mesg[i].type->id) continue;
      if (seq++ == sequence || H5O_ALL==sequence) {

	 /* clear symbol table entry cache */
	 if (ent && type->cache && H5G_NOTHING_CACHED!=ent->type) {
	    ent->type = H5G_NOTHING_CACHED;
	    if (ent_modified) *ent_modified = TRUE;
	 }

	 /* change message type to nil and zero it */
	 oh->mesg[i].type = H5O_NULL;
	 HDmemset (oh->mesg[i].raw, 0, oh->mesg[i].raw_size);
	 H5O_reset (type, oh->mesg[i].native);
	 oh->mesg[i].native = H5MM_xfree (oh->mesg[i].native);

	 oh->mesg[i].dirty = TRUE;
	 oh->dirty = TRUE;
      }
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_alloc_extend_chunk
 *
 * Purpose:	Extends a chunk which hasn't been allocated on disk yet
 *		to make the chunk large enough to contain a message whose
 *		data size is at least SIZE bytes.
 *
 * 		If the last message of the chunk is the null message, then
 *		that message will be extended with the chunk.  Otherwise a
 *		new null message is created.
 *
 * Return:	Success:	Message index for null message which
 *				is large enough to hold SIZE bytes.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  7 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5O_alloc_extend_chunk (H5O_t *oh, intn chunkno, size_t size)
{
   intn		idx, i;
   size_t	delta;
   uint8	*old_addr;

   FUNC_ENTER (H5O_alloc_extend_chunk, NULL, FAIL);

   /* check args */
   assert (oh);
   assert (chunkno>=0 && chunkno<oh->nchunks);
   assert (size>0);

   if (H5O_NO_ADDR!=oh->chunk[chunkno].addr) {
      HRETURN_ERROR (H5E_OHDR, H5E_NOSPACE, FAIL); /*chunk is on disk*/
   }

   /* try to extend a null message */
   for (idx=0; idx<oh->nmesgs; idx++) {
      if (H5O_NULL_ID==oh->mesg[idx].type->id &&
	  (oh->mesg[idx].raw + oh->mesg[idx].raw_size ==
	   oh->chunk[chunkno].image + oh->chunk[chunkno].size)) {
	 
	 delta = MAX (H5O_MIN_SIZE, size-oh->mesg[idx].raw_size);
	 H5O_ALIGN (delta, oh->alignment);
	 oh->mesg[idx].dirty = TRUE;
	 oh->mesg[idx].raw_size += delta;

	 old_addr = oh->chunk[chunkno].image;
	 oh->chunk[chunkno].size += delta;
	 oh->chunk[chunkno].image = H5MM_xrealloc (old_addr,
						   oh->chunk[chunkno].size);

	 /* adjust raw addresses for messages of this chunk */
	 if (old_addr != oh->chunk[chunkno].image) {
	    for (i=0; i<oh->nmesgs; i++) {
	       if (oh->mesg[i].chunkno==chunkno) {
		  oh->mesg[i].raw = oh->chunk[chunkno].image +
				    (oh->mesg[i].raw - old_addr);
	       }
	    }
	 }
	 HRETURN (idx);
      }
   }

   /* create a new null message */
   if (oh->nmesgs >= oh->alloc_nmesgs) {
      oh->alloc_nmesgs += H5O_NMESGS;
      oh->mesg = H5MM_xrealloc (oh->mesg,
				oh->alloc_nmesgs * sizeof(H5O_mesg_t));
   }
   
   delta = MAX (H5O_MIN_SIZE, 4+size);
   H5O_ALIGN (delta, oh->alignment);
   idx = oh->nmesgs++;
   oh->mesg[idx].type = H5O_NULL;
   oh->mesg[idx].dirty = TRUE;
   oh->mesg[idx].native = NULL;
   oh->mesg[idx].raw = oh->chunk[chunkno].image + oh->chunk[chunkno].size + 4;
   oh->mesg[idx].raw_size = delta-4;
   oh->mesg[idx].chunkno = chunkno;

   old_addr = oh->chunk[chunkno].image;
   oh->chunk[chunkno].size += delta;
   oh->chunk[chunkno].image = H5MM_xrealloc (old_addr,
					     oh->chunk[chunkno].size);

   /* adjust raw addresses for messages of this chunk */
   if (old_addr != oh->chunk[chunkno].image) {
      for (i=0; i<oh->nmesgs; i++) {
	 if (oh->mesg[i].chunkno==chunkno) {
	    oh->mesg[i].raw = oh->chunk[chunkno].image +
			      (oh->mesg[i].raw - old_addr);
	 }
      }
   }

   FUNC_LEAVE (idx);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_alloc_new_chunk
 *
 * Purpose:	Allocates a new chunk for the object header but doen't
 *		give the new chunk a file address yet.  One of the other
 *		chunks will get an object continuation message.  If there
 *		isn't room in any other chunk for the object continuation
 *		message, then some message from another chunk is moved into
 *		this chunk to make room.
 *
 * Return:	Success:	Index number of the null message for the
 *				new chunk.  The null message will be at
 *				least SIZE bytes not counting the message
 *				ID or size fields.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  7 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5O_alloc_new_chunk (hdf5_file_t *f, H5O_t *oh, size_t size)
{
   size_t	cont_size;		/*continuation message size	*/
   intn		found_null=(-1);	/*best fit null message		*/
   intn		found_other=(-1);	/*best fit other message	*/
   intn		idx=FAIL;		/*message number return value	*/
   uint8	*p = NULL;		/*ptr into new chunk		*/
   H5O_cont_t	*cont = NULL;		/*native continuation message	*/
   intn		i, chunkno;
   
   FUNC_ENTER (H5O_alloc_new_chunk, NULL, FAIL);

   /* check args */
   assert (oh);
   assert (size>0);

   /*
    * Find the smallest null message that will hold an object
    * continuation message.  Failing that, find the smallest message
    * that could be moved to make room for the continuation message.
    * Don't ever move continuation message from one chunk to another.
    */
   cont_size = H5F_SIZEOF_OFFSET(f) + H5F_SIZEOF_SIZE(f);
   for (i=0; i<oh->nmesgs; i++) {
      if (H5O_NULL_ID == oh->mesg[i].type->id) {
	 if (cont_size == oh->mesg[i].raw_size) {
	    found_null = i;
	    break;
	 } else if (oh->mesg[i].raw_size >= cont_size &&
		    (found_null<0 ||
		     oh->mesg[i].raw_size < oh->mesg[found_null].raw_size)) {
	    found_null = i;
	 }
      } else if (H5O_CONT_ID == oh->mesg[i].type->id) {
	 /*don't consider continuation messages */
      } else if (oh->mesg[i].raw_size >= cont_size &&
		 (found_other<0 ||
		  oh->mesg[i].raw_size < oh->mesg[found_other].raw_size)) {
	 found_other = i;
      }
   }
   assert (found_null>=0 || found_other>=0);

   /*
    * If we must move some other message to make room for the null
    * message, then make sure the new chunk has enough room for that
    * other message.
    */
   if (found_null<0) size += 4 + oh->mesg[found_other].raw_size;

   /*
    * The total chunk size must include the requested space plus enough
    * for the message header.  This must be at least some minimum and a
    * multiple of the alignment size.
    */
   size = MAX (H5O_MIN_SIZE, size+4);
   H5O_ALIGN (size, oh->alignment);

   /*
    * Create the new chunk without giving it a file address.
    */
   if (oh->nchunks >= oh->alloc_nchunks) {
      oh->alloc_nchunks += H5O_NCHUNKS;
      oh->chunk = H5MM_xrealloc (oh->chunk,
				 oh->alloc_nchunks * sizeof(H5O_chunk_t));
   }
   chunkno = oh->nchunks++;
   oh->chunk[chunkno].dirty = TRUE;
   oh->chunk[chunkno].addr = H5O_NO_ADDR;
   oh->chunk[chunkno].size = size;
   oh->chunk[chunkno].image = p = H5MM_xmalloc (size);

   /*
    * Make sure we have enough space for all possible new messages
    * that could be generated below.
    */
   if (oh->nmesgs+3 > oh->alloc_nmesgs) {
      oh->alloc_nmesgs += MAX (H5O_NMESGS, 3);
      oh->mesg = H5MM_xrealloc (oh->mesg,
				oh->alloc_nmesgs * sizeof(H5O_mesg_t));
   }
			  
   /*
    * Describe the messages of the new chunk.
    */
   if (found_null<0) {
      found_null = i = oh->nmesgs++;
      oh->mesg[i].type = H5O_NULL;
      oh->mesg[i].dirty = TRUE;
      oh->mesg[i].native = NULL;
      oh->mesg[i].raw = oh->mesg[found_other].raw;
      oh->mesg[i].raw_size = oh->mesg[found_other].raw_size;
      oh->mesg[i].chunkno = oh->mesg[found_other].chunkno;

      oh->mesg[found_other].dirty = TRUE;
      oh->mesg[found_other].raw = p+4;
      oh->mesg[found_other].chunkno = chunkno;
      p += 4 + oh->mesg[found_other].raw_size;
      size -= 4 + oh->mesg[found_other].raw_size;
   }

   idx = oh->nmesgs++;
   oh->mesg[idx].type = H5O_NULL;
   oh->mesg[idx].dirty = TRUE;
   oh->mesg[idx].native = NULL;
   oh->mesg[idx].raw = p+4;
   oh->mesg[idx].raw_size = size-4;
   oh->mesg[idx].chunkno = chunkno;

   /*
    * If the null message that will receive the continuation message
    * is larger than the continuation message, then split it into
    * two null messages.
    */
   if (oh->mesg[found_null].raw_size > cont_size) {
      i = oh->nmesgs++;
      oh->mesg[i].type = H5O_NULL;
      oh->mesg[i].dirty = TRUE;
      oh->mesg[i].native = NULL;
      oh->mesg[i].raw = oh->mesg[found_null].raw + cont_size + 4;
      oh->mesg[i].raw_size = oh->mesg[found_null].raw_size - (cont_size+4);
      oh->mesg[i].chunkno = oh->mesg[found_null].chunkno;

      oh->mesg[found_null].dirty = TRUE;
      oh->mesg[found_null].raw_size = cont_size;
   }

   /*
    * Initialize the continuation message.
    */
   oh->mesg[found_null].type = H5O_CONT;
   oh->mesg[found_null].dirty = TRUE;
   cont = H5MM_xcalloc (1, sizeof(H5O_cont_t));
   cont->addr = H5O_NO_ADDR;
   cont->size = 0;
   cont->chunkno = chunkno;
   oh->mesg[found_null].native = cont;


   FUNC_LEAVE (idx);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5O_alloc
 *
 * Purpose:	Allocate enough space in the object header for this message.
 *
 * Return:	Success:	Index of message
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5O_alloc (hdf5_file_t *f, H5O_t *oh, const H5O_class_t *type, size_t size)
{
   intn		chunkno;
   intn		idx;
   intn		null_idx;
   
   FUNC_ENTER (H5O_alloc, NULL, FAIL);

   /* check args */
   assert (oh);
   assert (type);
   H5O_ALIGN (size, oh->alignment);

   /* look for a null message which is large enough */
   for (idx=0; idx<oh->nmesgs; idx++) {
      if (H5O_NULL_ID==oh->mesg[idx].type->id &&
	  oh->mesg[idx].raw_size>=size) break;
   }

#ifdef LATER
   /*
    * Perhaps if we join adjacent null messages we could make one
    * large enough... we leave this as an exercise for future
    * programmers :-)  This isn't a high priority because when an
    * object header is read from disk the null messages are combined
    * anyway.
    */
#endif
   
   /* if we didn't find one, then allocate more header space */
   if (idx>=oh->nmesgs) {

      /*
       * Look for a chunk which hasn't had disk space allocated yet
       * since we can just increase the size of that chunk.
       */
      for (chunkno=0; chunkno<oh->nchunks; chunkno++) {
	 if ((idx=H5O_alloc_extend_chunk (oh, chunkno, size))>=0) break;
	 H5ECLEAR;
      }

      /*
       * Create a new chunk
       */
      if (idx<0) {
	 if ((idx=H5O_alloc_new_chunk (f, oh, size))<0) {
	    HRETURN_ERROR (H5E_OHDR, H5E_NOSPACE, FAIL);
	 }
      }
   }

   /* do we need to split the null message? */
   if (oh->mesg[idx].raw_size > size) {
      assert (oh->mesg[idx].raw_size - size >= 4); /*room for type & size */

      if (oh->nmesgs >= oh->alloc_nmesgs) {
	 oh->alloc_nmesgs += H5O_NMESGS;
	 oh->mesg = H5MM_xrealloc (oh->mesg,
				   oh->alloc_nmesgs * sizeof(H5O_mesg_t));
      }

      null_idx = oh->nmesgs++;
      oh->mesg[null_idx].type = H5O_NULL;
      oh->mesg[null_idx].dirty = TRUE;
      oh->mesg[null_idx].native = NULL;
      oh->mesg[null_idx].raw = oh->mesg[idx].raw + size + 4;
      oh->mesg[null_idx].raw_size = oh->mesg[idx].raw_size - (size + 4);
      oh->mesg[null_idx].chunkno = oh->mesg[idx].chunkno;
      oh->mesg[idx].raw_size = size;
   }

   /* initialize the new message */
   oh->mesg[idx].type = type;
   oh->mesg[idx].dirty = TRUE;
   oh->mesg[idx].native = NULL;
   
   oh->dirty = TRUE;
   FUNC_LEAVE (idx);
}
	 

/*-------------------------------------------------------------------------
 * Function:	H5O_debug
 *
 * Purpose:	Prints debugging info about an object header.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_debug (hdf5_file_t *f, haddr_t addr, FILE *stream,
	   intn indent, intn fwidth)
{
   H5O_t	*oh = NULL;
   intn		i, chunkno;
   size_t	mesg_total=0, chunk_total=0;
   int		*sequence;
   
   FUNC_ENTER (H5O_debug, NULL, FAIL);

   /* check args */
   assert (f);
   assert (addr>=0);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   if (NULL==(oh=H5AC_find (f, H5AC_OHDR, addr, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL);
   }

   /* debug */
   fprintf (stream, "%*sObject Header...\n", indent, "");
   
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Dirty:",
	    (int)(oh->dirty));
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Version:",
	    (int)(oh->version));
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Alignment:",
	    (int)(oh->alignment));
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Number of links:",
	    (int)(oh->nlink));
   fprintf (stream, "%*s%-*s %d (%d)\n", indent, "", fwidth,
	    "Number of messages (allocated):",
	    (int)(oh->nmesgs), (int)(oh->alloc_nmesgs));
   fprintf (stream, "%*s%-*s %d (%d)\n", indent, "", fwidth,
	    "Number of chunks (allocated):",
	    (int)(oh->nchunks), (int)(oh->alloc_nchunks));

   /* debug each chunk */
   for (i=chunk_total=0; i<oh->nchunks; i++) {
      chunk_total += oh->chunk[i].size;
      fprintf (stream, "%*sChunk %d...\n", indent, "", i);
      
      fprintf (stream, "%*s%-*s %d\n", indent+3, "", MAX(0,fwidth-3),
	       "Dirty:",
	       (int)(oh->chunk[i].dirty));
      fprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX(0,fwidth-3),
	       "Address:",
	       (unsigned long)(oh->chunk[i].addr));
      if (0==i && oh->chunk[i].addr!=addr+H5O_SIZEOF_HDR(f)) {
	 fprintf (stream, "*** WRONG ADDRESS!\n");
      }
      fprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX(0,fwidth-3),
	       "Size in bytes:",
	       (unsigned long)(oh->chunk[i].size));
   }

   /* debug each message */
   sequence = H5MM_xcalloc (NELMTS(message_type_g), sizeof(int));
   for (i=mesg_total=0; i<oh->nmesgs; i++) {
      mesg_total += 4 + oh->mesg[i].raw_size;
      fprintf (stream, "%*sMessage %d...\n", indent, "", i);

      /* check for bad message id */
      if (oh->mesg[i].type->id<0 ||
	  oh->mesg[i].type->id>=NELMTS(message_type_g)) {
	 fprintf (stream, "*** BAD MESSAGE ID 0x%04x\n",
		  oh->mesg[i].type->id);
	 continue;
      }

      /* message name and size */
      fprintf (stream, "%*s%-*s 0x%04x %s(%d)\n",
	       indent+3, "", MAX (0, fwidth-3),
	       "Message ID:",
	       (unsigned)(oh->mesg[i].type->id),
	       oh->mesg[i].type->name,
	       sequence[oh->mesg[i].type->id]++);
      fprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX (0, fwidth-3),
	       "Raw size in bytes:",
	       (unsigned long)(oh->mesg[i].raw_size));

      fprintf (stream, "%*s%-*s %d\n", indent+3, "", MAX(0,fwidth-3),
	       "Chunk number:",
	       (int)(oh->mesg[i].chunkno));
      chunkno = oh->mesg[i].chunkno;
      if (chunkno<0 || chunkno>=oh->nchunks) {
	 fprintf (stream, "*** BAD CHUNK NUMBER\n");
      }

      /* check the size */
      if ((oh->mesg[i].raw + oh->mesg[i].raw_size >
	   oh->chunk[chunkno].image + oh->chunk[chunkno].size) ||
	  (oh->mesg[i].raw < oh->chunk[chunkno].image)) {
	 fprintf (stream, "*** BAD MESSAGE RAW ADDRESS\n");
      }
      
      /* decode the message */
      if (NULL==oh->mesg[i].native && oh->mesg[i].type->decode) {
	 oh->mesg[i].native = (oh->mesg[i].type->decode)(f,
							 oh->mesg[i].raw_size,
							 oh->mesg[i].raw);
      }

      /* print the message */
      if (oh->mesg[i].type->debug) {
	 (oh->mesg[i].type->debug)(f, oh->mesg[i].native, stream, indent+3,
				   MAX(0, fwidth-3));
      } else {
	 fprintf (stream, "%*sNo info for this message.\n", indent+3, "");
      }
   }
   sequence = H5MM_xfree (sequence);

   if (mesg_total != chunk_total) {
      fprintf (stream, "*** TOTAL SIZE DOES NOT MATCH ALLOCATED SIZE!\n");
   }

   FUNC_LEAVE (SUCCEED);
}
