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
static herr_t H5O_flush (H5F_t *f, hbool_t destroy, const haddr_t *addr,
			 H5O_t *oh);
static H5O_t *H5O_load (H5F_t *f, const haddr_t *addr, const void *_udata1,
			void *_udata2);
static intn H5O_find_in_ohdr (H5F_t *f, const haddr_t *addr,
			      const H5O_class_t **type_p, intn sequence);
static intn H5O_alloc (H5F_t *f, H5O_t *oh, const H5O_class_t *type,
		       size_t size);
static intn H5O_alloc_extend_chunk (H5O_t *oh, intn chunkno, size_t size);
static intn H5O_alloc_new_chunk (H5F_t *f, H5O_t *oh, size_t size);

/* H5O inherits cache-like properties from H5AC */
static const H5AC_class_t H5AC_OHDR[1] = {{
   H5AC_OHDR_ID,
   (void*(*)(H5F_t*,const haddr_t*,const void*,void*))H5O_load,
   (herr_t(*)(H5F_t*,hbool_t,const haddr_t*,void*))H5O_flush,
}};

/* Interface initialization */
static intn interface_initialize_g = FALSE;
#define INTERFACE_INIT	H5O_init_interface
static herr_t H5O_init_interface (void);

/* ID to type mapping */
static const H5O_class_t *const message_type_g[] = {
   H5O_NULL,    /*0x0000 Null 						*/
   H5O_SDSPACE, /*0x0001 Simple Dimensionality				*/
   NULL,		/*0x0002 Data space (fiber bundle?)		*/
   H5O_DTYPE,   /*0x0003 Data Type					*/
   NULL,		/*0x0004 Not assigned				*/
   NULL,  		/*0x0005 Not assigned				*/
   NULL,		/*0x0006 Data storage -- compact object		*/
   NULL,		/*0x0007 Data storage -- external object	*/
   H5O_LAYOUT, 	/*0x0008 Data Layout					*/
   H5O_EFL,	/*0x0009 External File List				*/
   NULL,		/*0x000A Not assigned				*/
   NULL,		/*0x000B Data storage -- compressed object	*/
   NULL,		/*0x000C Attribute list				*/
   H5O_NAME,	/*0x000D Object name					*/
   NULL,		/*0x000E Object modification date and time	*/
   NULL,		/*0x000F Shared header message			*/
   H5O_CONT,	/*0x0010 Object header continuation			*/
   H5O_STAB,	/*0x0011 Symbol table					*/
};

/*
 * An array of functions indexed by symbol table entry cache type
 * (H5G_type_t) that are called to retrieve constant messages cached in the
 * symbol table entry.
 */
static void *(*H5O_fast_g[H5G_NCACHED])(const H5G_cache_t*,
					const H5O_class_t *,
					void*);



/*-------------------------------------------------------------------------
 * Function:	H5O_init_interface
 *
 * Purpose:	Initialize the H5O interface.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_init_interface (void)
{
   FUNC_ENTER (H5O_init_interface, FAIL);

   /*
    * Initialize functions that decode messages from symbol table entries.
    */
   H5O_fast_g[H5G_CACHED_STAB] = H5O_stab_fast;

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_create
 *
 * Purpose:	Creates a new object header, sets the link count
 *		to 0, and caches the header.  The object header is opened for
 *		write access and should eventually be closed by calling
 *		H5O_close().
 *
 * Return:	Success:	SUCCEED, the ENT argument contains
 *				information about the object header,
 *				including its address.
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
herr_t
H5O_create (H5F_t *f, size_t size_hint, H5G_entry_t *ent/*out*/)
{
   size_t	size;		/*total size of object header	*/
   H5O_t	*oh = NULL;
   haddr_t	tmp_addr;

   FUNC_ENTER (H5O_create, FAIL);

   /* check args */
   assert (f);
   assert (ent);
   HDmemset (ent, 0, sizeof(H5G_entry_t));
   if (size_hint<H5O_MIN_SIZE) size_hint = H5O_MIN_SIZE;
   H5O_ALIGN (size_hint, H5O_ALIGNMENT);

   /* allocate disk space for header and first chunk */
   size = H5O_SIZEOF_HDR(f) + size_hint;
   if (H5MF_alloc (f, H5MF_META, size, &(ent->header)/*out*/)<0) {
      HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
		     "unable to allocate file space for object header hdr");
   }

   /* allocate the object header and fill in header fields */
   oh = H5MM_xcalloc (1, sizeof(H5O_t));
   oh->dirty = TRUE;
   oh->version = H5O_VERSION;
   oh->alignment = H5O_ALIGNMENT;
   oh->nlink = 0;

   /* create the chunk list and initialize the first chunk */
   oh->nchunks = 1;
   oh->alloc_nchunks = H5O_NCHUNKS;
   oh->chunk = H5MM_xmalloc (oh->alloc_nchunks * sizeof (H5O_chunk_t));

   tmp_addr = ent->header;
   H5F_addr_inc (&tmp_addr, H5O_SIZEOF_HDR (f));
   oh->chunk[0].dirty = TRUE;
   oh->chunk[0].addr = tmp_addr;
   oh->chunk[0].size = size_hint;
   oh->chunk[0].image = H5MM_xcalloc (1, size_hint);

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
   if (H5AC_set (f, H5AC_OHDR, &(ent->header), oh)<0) {
      H5MM_xfree (oh);
      HRETURN_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL,
		     "unable to cache object header");
   }

   /* open it */
   if (H5O_open (f, ent)<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTOPENOBJ, FAIL,
		     "unable to open object header");
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_open
 *
 * Purpose:	Opens an object header which is described by the symbol table
 *		entry OBJ_ENT.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, January  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_open (H5F_t *f, H5G_entry_t *obj_ent)
{
   FUNC_ENTER (H5O_open, FAIL);

   /* Check args */
   assert (f);
   assert (obj_ent);

#ifdef H5O_DEBUG
   fprintf (stderr, ">");
   H5F_addr_print (stderr, &(obj_ent->header));
   fprintf (stderr, "\n");
#endif

   /* Increment open-lock counters */
   obj_ent->file = f;
   obj_ent->file->nopen++;
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_close
 *
 * Purpose:	Closes an object header that was previously open.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, January  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_close (H5G_entry_t *obj_ent)
{
   FUNC_ENTER (H5O_close, FAIL);

   /* Check args */
   assert (obj_ent);
   assert (obj_ent->file);
   assert (obj_ent->file->nopen>0);

   /* Decrement open-lock counters */
   --obj_ent->file->nopen;

   /*
    * If the file open-lock count has reached zero and the file has a close
    * pending then close the file.
    */
   if (0==obj_ent->file->nopen && obj_ent->file->close_pending) {
      H5F_close (obj_ent->file);
   }

#ifdef H5O_DEBUG
   fprintf (stderr, "<");
   H5F_addr_print (stderr, &(obj_ent->header));
   fprintf (stderr, "\n");
#endif
   
   FUNC_LEAVE (SUCCEED);
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
 * 	Robb Matzke, 7 Jan 1998
 *	Able to distinguish between constant and variable messages.
 *
 *-------------------------------------------------------------------------
 */
static H5O_t *
H5O_load (H5F_t *f, const haddr_t *addr, const void *_udata1, void *_udata2)
{
   H5O_t	*oh = NULL;
   H5O_t	*ret_value = (void*)1; /*kludge for HGOTO_ERROR*/
   uint8	buf[16], *p;
   size_t	hdr_size, mesg_size;
   uintn	id;
   intn		mesgno, chunkno, curmesg=0, nmesgs;
   haddr_t	chunk_addr;
   size_t	chunk_size;
   H5O_cont_t	*cont=NULL;
   hbool_t	constant;		/*is message a constant mesg?	*/
   

   FUNC_ENTER (H5O_load, NULL);

   /* check args */
   assert (f);
   assert (addr && H5F_addr_defined (addr));
   assert (!_udata1);
   assert (!_udata2);

   /* allocate ohdr and init chunk list */
   oh = H5MM_xcalloc (1, sizeof(H5O_t));

   /* read fixed-lenth part of object header */
   hdr_size = H5O_SIZEOF_HDR (f);
   if (H5F_block_read (f, addr, hdr_size, buf)<0) {
      HGOTO_ERROR (H5E_OHDR, H5E_READERROR, NULL,
		   "unable to read object header");
   }
   p = buf;

   /* decode version */
   oh->version = *p++;
   if (H5O_VERSION!=oh->version) {
      HGOTO_ERROR (H5E_OHDR, H5E_VERSION, NULL,
		   "bad object header version number");
   }

   /* decode alignment */
   oh->alignment = *p++;
   if (4!=oh->alignment) {
      HGOTO_ERROR (H5E_OHDR, H5E_ALIGNMENT, NULL,
		   "unsupported object header alignment");
   }

   /* decode number of messages */
   UINT16DECODE (p, nmesgs);

   /* decode link count */
   UINT32DECODE (p, oh->nlink);

   /* decode first chunk info */
   chunk_addr = *addr;
   H5F_addr_inc (&chunk_addr, H5O_SIZEOF_HDR (f));
   UINT32DECODE (p, chunk_size);

   /* build the message array */
   oh->alloc_nmesgs = MAX (H5O_NMESGS, nmesgs);
   oh->mesg = H5MM_xcalloc (oh->alloc_nmesgs, sizeof(H5O_mesg_t));
      
   /* read each chunk from disk */
   while (H5F_addr_defined (&chunk_addr)) {

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
      if (H5F_block_read (f, &chunk_addr, chunk_size,
			  oh->chunk[chunkno].image)<0) {
	 HGOTO_ERROR (H5E_OHDR, H5E_READERROR, NULL,
		      "unable to read object header data");
      }
      

      /* load messages from this chunk */
      for (p = oh->chunk[chunkno].image;
	   p < oh->chunk[chunkno].image + chunk_size;
	   p += mesg_size) {
	 UINT16DECODE (p, id);
	 UINT16DECODE (p, mesg_size);

	 /*
	  * The message ID field actually contains some bits near the
	  * high-order end that are not part of the ID.
	  */
	 constant = (id & H5O_FLAG_CONSTANT) ? TRUE : FALSE;
	 id &= ~H5O_FLAG_BITS;
      
	 if (id>=NELMTS(message_type_g) || NULL==message_type_g[id]) {
	    HGOTO_ERROR (H5E_OHDR, H5E_BADMESG, NULL,
			 "corrupt object header");
	 }
	 if (p + mesg_size > oh->chunk[chunkno].image + chunk_size) {
	    HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, NULL,
			 "corrupt object header");
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
	       HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, NULL,
			    "corrupt object header");
	    }
	    mesgno = oh->nmesgs++;
	    oh->mesg[mesgno].type = message_type_g[id];
	    oh->mesg[mesgno].dirty = FALSE;
	    oh->mesg[mesgno].constant = constant;
	    oh->mesg[mesgno].native = NULL;
	    oh->mesg[mesgno].raw = p;
	    oh->mesg[mesgno].raw_size = mesg_size;
	    oh->mesg[mesgno].chunkno = chunkno;
	 }
      }
      assert (p == oh->chunk[chunkno].image + chunk_size);

      /* decode next object header continuation message */
      for (H5F_addr_undef (&chunk_addr);
	   !H5F_addr_defined (&chunk_addr) && curmesg<oh->nmesgs;
	   curmesg++) {
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
 * 	Robb Matzke, 7 Jan 1998
 *	Handles constant vs non-constant messages.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_flush (H5F_t *f, hbool_t destroy, const haddr_t *addr, H5O_t *oh)
{
   uint8	buf[16], *p;
   intn		i, id;
   H5O_cont_t	*cont = NULL;
   
   FUNC_ENTER (H5O_flush, FAIL);

   /* check args */
   assert (f);
   assert (addr && H5F_addr_defined (addr));
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
	 HRETURN_ERROR (H5E_OHDR, H5E_WRITEERROR, FAIL,
			"unable to write object header hdr to disk");
      }

      /* encode messages */
      for (i=0; i<oh->nmesgs; i++) {
	 if (oh->mesg[i].dirty) {
	    p = oh->mesg[i].raw - 4;

	    /* The message id has some flags in the high-order bits. */
	    id = oh->mesg[i].type->id;
	    id |= oh->mesg[i].constant ? H5O_FLAG_CONSTANT : 0;
	    UINT16ENCODE (p, id);
	    UINT16ENCODE (p, oh->mesg[i].raw_size);
	    
	    if (oh->mesg[i].native) {
	       assert (oh->mesg[i].type->encode);

	       /* allocate file space for chunks that have none yet */
	       if (H5O_CONT_ID==oh->mesg[i].type->id &&
		   !H5F_addr_defined (&(((H5O_cont_t*)
					 (oh->mesg[i].native))->addr))) {
		  cont = (H5O_cont_t*)(oh->mesg[i].native);
		  assert (cont->chunkno >= 0);
		  assert (cont->chunkno < oh->nchunks);
		  assert (!H5F_addr_defined(&(oh->chunk[cont->chunkno].addr)));
		  cont->size = oh->chunk[cont->chunkno].size;
		  if (H5MF_alloc (f, H5MF_META, cont->size,
				  &(cont->addr)/*out*/)<0) {
		     HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
				    "unable to allocate space for object "
				    "header data");
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
		  HRETURN_ERROR (H5E_OHDR, H5E_CANTENCODE, FAIL,
				 "unable to encode object header message");
	       }
	    }
	    oh->mesg[i].dirty = FALSE;
	    oh->chunk[oh->mesg[i].chunkno].dirty = TRUE;
	 }
      }

      /* write each chunk to disk */
      for (i=0; i<oh->nchunks; i++) {
	 if (oh->chunk[i].dirty) {
	    assert (H5F_addr_defined (&(oh->chunk[i].addr)));
	    if (H5F_block_write (f, &(oh->chunk[i].addr), oh->chunk[i].size,
				 oh->chunk[i].image)<0) {
	       HRETURN_ERROR (H5E_OHDR, H5E_WRITEERROR, FAIL,
			      "unable to write object header data to disk");
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
 *		matzke@llnl.gov
 *		Aug 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_reset (const H5O_class_t *type, void *native)
{
   FUNC_ENTER (H5O_reset, FAIL);

   if (native) {
      if (type->reset) {
	 if ((type->reset)(native)<0) {
	    HRETURN_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL,
			   "reset method failed");
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
H5O_link (H5G_entry_t *ent, intn adjust)
{
   H5O_t	*oh = NULL;
   intn		ret_value = FAIL;
   
   FUNC_ENTER (H5O_link, FAIL);

   /* check args */
   assert (ent);
   assert (ent->file);
   assert (H5F_addr_defined (&(ent->header)));
   
   /* get header */
   if (NULL==(oh=H5AC_protect (ent->file, H5AC_OHDR, &(ent->header),
			       NULL, NULL))) {
      HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL,
		   "unable to load object header");
   }

   /* adjust link count */
   if (adjust<0) {
      if (oh->nlink + adjust < 0) {
	 HGOTO_ERROR (H5E_OHDR, H5E_LINKCOUNT, FAIL,
		      "link count would be negative");
      }
      oh->nlink += adjust;
   } else {
      oh->nlink += adjust;
   }

   oh->dirty = TRUE;
   ret_value = oh->nlink;

 done:
   if (oh && H5AC_unprotect (ent->file, H5AC_OHDR, &(ent->header), oh)<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_PROTECT, FAIL,
		     "unable to release object header");
   }

   FUNC_LEAVE (ret_value);
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
H5O_read (H5G_entry_t *ent, const H5O_class_t *type, intn sequence, void *mesg)
{
   H5O_t	*oh = NULL;
   void		*retval = NULL;
   intn		idx;
   H5G_cache_t	*cache = NULL;
   H5G_type_t	cache_type;
   
   FUNC_ENTER (H5O_read, NULL);

   /* check args */
   assert (ent);
   assert (ent->file);
   assert (H5F_addr_defined (&(ent->header)));
   assert (type);
   assert (sequence>=0);

   /* can we get it from the symbol table entry? */
   cache = H5G_ent_cache (ent, &cache_type);
   if (H5O_fast_g[cache_type]) {
      retval = (H5O_fast_g[cache_type])(cache, type, mesg);
      if (retval) HRETURN (retval);
      H5ECLEAR; /*don't care, try reading from header*/
   }

   /* can we get it from the object header? */
   if ((idx = H5O_find_in_ohdr (ent->file, &(ent->header), &type,
				sequence))<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_NOTFOUND, NULL,
		     "unable to find message in object header");
   }

   /* copy the message to the user-supplied buffer */
   if (NULL==(oh=H5AC_protect (ent->file, H5AC_OHDR, &(ent->header),
			       NULL, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, NULL,
		     "unable to load object header");
   }
   retval = (type->copy)(oh->mesg[idx].native, mesg);
   if (H5AC_unprotect (ent->file, H5AC_OHDR, &(ent->header), oh)<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_PROTECT, NULL,
		     "unable to release object header");
   }
   oh = NULL;
   
   if (!retval) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTINIT, NULL,
		     "unable to copy object header message to user space");
   }
   
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
H5O_find_in_ohdr (H5F_t *f, const haddr_t *addr, const H5O_class_t **type_p,
		  intn sequence)
{
   H5O_t	*oh = NULL;
   int		i;
   
   FUNC_ENTER (H5O_find_in_ohdr, FAIL);
   
   /* check args */
   assert (f);
   assert (addr && H5F_addr_defined (addr));
   assert (type_p);

   /* load the object header */
   if (NULL==(oh=H5AC_find (f, H5AC_OHDR, addr, NULL, NULL))) {
      HRETURN_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL,
		     "unable to load object header");
   }

   /* scan through the messages looking for the right one */
   for (i=0; i<oh->nmesgs; i++) {
      if (*type_p && (*type_p)->id!=oh->mesg[i].type->id) continue;
      if (--sequence<0) break;
   }
   if (sequence>=0) {
      HRETURN_ERROR (H5E_OHDR, H5E_NOTFOUND, FAIL,
		     "unable to find object header message");
   }
   
   /* decode the message if necessary */
   if (NULL==oh->mesg[i].native) {
      assert (oh->mesg[i].type->decode);
      oh->mesg[i].native = (oh->mesg[i].type->decode)(f,
						      oh->mesg[i].raw_size,
						      oh->mesg[i].raw);
      if (NULL==oh->mesg[i].native) {
	 HRETURN_ERROR (H5E_OHDR, H5E_CANTDECODE, FAIL,
			"unable to decode message");
      }
   }

   /*return the message type */
   *type_p = oh->mesg[i].type;

   FUNC_LEAVE (i);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_modify
 *
 * Purpose:	Modifies an existing message or creates a new message.
 *		The cache fields in that symbol table entry ENT are *not*
 *		updated, you must do that separately because they often
 *		depend on multiple object header messages.  Besides, we
 *		don't know which messages will be constant and which will
 *		not.
 *
 * 		The OVERWRITE argument is either a sequence number of a
 *		message to overwrite (usually zero) or the constant
 *		H5O_NEW_MESSAGE (-1) to indicate that a new message is to
 *		be created.  If the message to overwrite doesn't exist then
 *		it is created (but only if it can be inserted so its sequence
 *		number is OVERWRITE; that is, you can create a message with
 *		the sequence number 5 if there is no message with sequence
 *		number 4).
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
 * 	Robb Matzke, 7 Jan 1998
 *	Handles constant vs non-constant messages.  Once a message is made
 *	constant it can never become non-constant.  Constant messages cannot
 *	be modified.
 *
 *-------------------------------------------------------------------------
 */
intn
H5O_modify (H5G_entry_t *ent, const H5O_class_t *type, intn overwrite,
	    uintn flags, const void *mesg)
{
   H5O_t	*oh = NULL;
   intn		idx, sequence;
   intn		ret_value = FAIL;
   size_t	size;
   
   FUNC_ENTER (H5O_modify, FAIL);

   /* check args */
   assert (ent);
   assert (ent->file);
   assert (H5F_addr_defined (&(ent->header)));
   assert (type);
   assert (mesg);
   
   if (NULL==(oh=H5AC_protect (ent->file, H5AC_OHDR, &(ent->header),
			       NULL, NULL))) {
      HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL,
		   "unable to load object header");
   }

   /* Count similar messages */
   for (idx=0,sequence=-1; idx<oh->nmesgs; idx++) {
      if (type->id != oh->mesg[idx].type->id) continue;
      if (++sequence==overwrite) break;
   }

   /* Was the right message found? */
   if (overwrite>=0 &&
       (idx>=oh->nmesgs || sequence!=overwrite)) {

      /* But can we insert a new one with this sequence number? */
      if (overwrite==sequence+1) {
	 overwrite = -1;
      } else {
	 HGOTO_ERROR (H5E_OHDR, H5E_NOTFOUND, FAIL, "message not found");
      }
   }

   if (overwrite<0) {
      /* Allocate space for the new message */
      size = (type->raw_size)(ent->file, mesg);
      H5O_ALIGN (size, oh->alignment);
      idx = H5O_alloc (ent->file, oh, type, size);
      if (idx<0) {
	 HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL,
		      "unable to allocate object header space for message");
      }
      sequence++;
      
   } else if (oh->mesg[idx].constant) {
      HGOTO_ERROR (H5E_OHDR, H5E_WRITEERROR, FAIL,
		   "unable to modify constant message");
   }

   /* Copy the native value into the object header */
   oh->mesg[idx].native = (type->copy)(mesg, oh->mesg[idx].native);
   if (NULL==oh->mesg[idx].native) {
      HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL,
		   "unable to copy message to object header");
   }
   oh->mesg[idx].constant = (flags & H5O_FLAG_CONSTANT) ? TRUE : FALSE;
   oh->mesg[idx].dirty = TRUE;
   oh->dirty = TRUE;
   ret_value = sequence;

 done:
   if (oh && H5AC_unprotect (ent->file, H5AC_OHDR, &(ent->header), oh)<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_PROTECT, FAIL,
		     "unable to release object header");
   }

   FUNC_LEAVE (ret_value);
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
 * 		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 28 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 7 Jan 1998
 *	Does not remove constant messages.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_remove (H5G_entry_t *ent, const H5O_class_t *type, intn sequence)
{
   H5O_t	*oh = NULL;
   intn		i, seq, nfailed=0;
   herr_t	ret_value = FAIL;
   
   FUNC_ENTER (H5O_remove, FAIL);

   /* check args */
   assert (ent);
   assert (ent->file);
   assert (H5F_addr_defined (&(ent->header)));
   assert (type);

   /* load the object header */
   if (NULL==(oh=H5AC_protect (ent->file, H5AC_OHDR, &(ent->header),
			       NULL, NULL))) {
      HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL,
		   "unable to load object header");
   }

   for (i=seq=0; i<oh->nmesgs; i++) {
      if (type->id != oh->mesg[i].type->id) continue;
      if (seq++ == sequence || H5O_ALL==sequence) {

	 /*
	  * Keep track of how many times we failed trying to remove constant
	  * messages.
	  */
	 if (oh->mesg[i].constant) {
	    nfailed++;
	    continue;
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

   /* Fail if we tried to remove any constant messages */
   if (nfailed) {
      HGOTO_ERROR (H5E_OHDR, H5E_CANTINIT, FAIL,
		   "unable to remove constant message(s)");
   }

   ret_value = SUCCEED;

 done:
   if (oh && H5AC_unprotect (ent->file, H5AC_OHDR, &(ent->header), oh)<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_PROTECT, FAIL,
		     "unable to release object header");
   }

   FUNC_LEAVE (ret_value);
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

   FUNC_ENTER (H5O_alloc_extend_chunk, FAIL);

   /* check args */
   assert (oh);
   assert (chunkno>=0 && chunkno<oh->nchunks);
   assert (size>0);

   if (H5F_addr_defined (&(oh->chunk[chunkno].addr))) {
      HRETURN_ERROR (H5E_OHDR, H5E_NOSPACE, FAIL, "chunk is on disk");
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

	 /* Be careful not to indroduce garbage */
	 oh->chunk[chunkno].image = H5MM_xrealloc (old_addr,
						   (oh->chunk[chunkno].size +
						    delta));
	 HDmemset (oh->chunk[chunkno].image + oh->chunk[chunkno].size,
		   0, delta);
	 oh->chunk[chunkno].size += delta;

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
H5O_alloc_new_chunk (H5F_t *f, H5O_t *oh, size_t size)
{
   size_t	cont_size;		/*continuation message size	*/
   intn		found_null=(-1);	/*best fit null message		*/
   intn		found_other=(-1);	/*best fit other message	*/
   intn		idx=FAIL;		/*message number return value	*/
   uint8	*p = NULL;		/*ptr into new chunk		*/
   H5O_cont_t	*cont = NULL;		/*native continuation message	*/
   intn		i, chunkno;
   
   FUNC_ENTER (H5O_alloc_new_chunk, FAIL);

   /* check args */
   assert (oh);
   assert (size>0);

   /*
    * Find the smallest null message that will hold an object
    * continuation message.  Failing that, find the smallest message
    * that could be moved to make room for the continuation message.
    * Don't ever move continuation message from one chunk to another.
    */
   cont_size = H5F_SIZEOF_ADDR(f) + H5F_SIZEOF_SIZE(f);
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
   H5F_addr_undef (&(oh->chunk[chunkno].addr));
   oh->chunk[chunkno].size = size;
   oh->chunk[chunkno].image = p = H5MM_xcalloc (1, size);

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
   H5F_addr_undef (&(cont->addr));
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
H5O_alloc (H5F_t *f, H5O_t *oh, const H5O_class_t *type, size_t size)
{
   intn		chunkno;
   intn		idx;
   intn		null_idx;
   
   FUNC_ENTER (H5O_alloc, FAIL);

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
	    HRETURN_ERROR (H5E_OHDR, H5E_NOSPACE, FAIL,
			   "unable to create a new object header data chunk");
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
H5O_debug (H5F_t *f, const haddr_t *addr, FILE *stream, intn indent,
	   intn fwidth)
{
   H5O_t	*oh = NULL;
   intn		i, chunkno;
   size_t	mesg_total=0, chunk_total=0;
   int		*sequence;
   haddr_t	tmp_addr;
   herr_t	ret_value = FAIL;
   
   FUNC_ENTER (H5O_debug, FAIL);

   /* check args */
   assert (f);
   assert (addr && H5F_addr_defined (addr));
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   if (NULL==(oh=H5AC_protect (f, H5AC_OHDR, addr, NULL, NULL))) {
      HGOTO_ERROR (H5E_OHDR, H5E_CANTLOAD, FAIL,
		     "unable to load object header");
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
      
      fprintf (stream, "%*s%-*s ", indent+3, "", MAX(0,fwidth-3),
	       "Address:");
      H5F_addr_print (stream, &(oh->chunk[i].addr));
      fprintf (stream, "\n");

      tmp_addr = *addr;
      H5F_addr_inc (&tmp_addr, H5O_SIZEOF_HDR(f));
      if (0==i && H5F_addr_ne (&(oh->chunk[i].addr), &tmp_addr)) {
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
      fprintf (stream, "%*s%-*s %s\n", indent+3, "", MAX (0, fwidth-3),
	       "Constant:",
	       oh->mesg[i].constant ? "Yes" : "No");
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

   ret_value = SUCCEED;

 done:
   if (oh && H5AC_unprotect (f, H5AC_OHDR, addr, oh)<0) {
      HRETURN_ERROR (H5E_OHDR, H5E_PROTECT, FAIL,
		     "unable to release object header");
   }

   FUNC_LEAVE (ret_value);
}
