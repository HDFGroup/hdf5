/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@viper.llnl.gov>
 *             Wednesday, October 22, 1997
 *
 * Purpose:	This file contains virtual functions for the H5F_low
 *		class.  These are functions that operate on various kinds
 *		of files at a level where the file is just a one-dimensional
 *		array of bytes.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#include <sys/types.h>
#include <sys/stat.h>


#define PABLO_MASK	H5F_low
static hbool_t	interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5F_low_open
 *
 * Purpose:	Opens a file of type TYPE with name NAME according to the
 *		field of bit flags FLAGS which are:
 *		
 *		H5F_ACC_WRITE:	The file is open for read/write access.
 *				Without this bit set, the file would be open
 *				for read-only access.
 *
 *		H5F_ACC_CREAT:	The file is created if it doesn't already
 *				exist.  On unix, the file permissions are set
 *				to 0666 modified by the umask.
 *
 *		H5F_ACC_EXCL:	This function will fail if the file already
 *				exists.
 *
 *		H5F_ACC_TRUNC:	Truncate the file to a zero-length file as it
 *				is opened.  This allows existing files to be
 *				overwritten.
 *
 *		The KEY argument is initialized with data which is unique to
 *		this file.  Opening the same file (even by a different name)
 *		should return the same key.
 *
 *		This is a virtual function only; the actual open operation is
 *		performed by the subclass.  This function will fail if the
 *		subclass hasn't defined an open method.
 *
 * Errors:
 *		IO        CANTOPENFILE  Open failed. 
 *
 * Return:	Success:	Pointer to the new file descriptor.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5F_low_t *
H5F_low_open (const H5F_low_class_t *type, const char *name, uintn flags,
	      H5F_search_t *key/*out*/)
{
   H5F_low_t	*lf = NULL;
   
   FUNC_ENTER (H5F_low_open, NULL, NULL);

   assert (type && type->open);
   assert (name && *name);

   if (NULL==(lf=(type->open)(name, flags, key))) {
      HRETURN_ERROR (H5E_IO, H5E_CANTOPENFILE, NULL);/*open failed*/
   }
   lf->type = type;

   FUNC_LEAVE (lf);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_close
 *
 * Purpose:	Closes a low-level file. The subclass should free all
 *		resources used by the file descriptor but should not free the
 *		file descriptor itself.  The close method in the subclass is
 *		optional; lack of a close method results in only the file
 *		descriptor being freed.
 *
 *		It is safe to call this function with a null pointer for the
 *		file descriptor.  This function returns a null pointer that
 *		the caller can assign to the file descriptor pointer as it's
 *		closed like `desc=H5F_low_close(desc)'.
 *
 * Errors:
 *		IO        CLOSEERROR    Close failed. 
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5F_low_t *
H5F_low_close (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_low_close, NULL, NULL);

   if (lf) {
      if ((lf->type->close)(lf)<0) {
	 H5MM_xfree (lf);
	 HRETURN_ERROR (H5E_IO, H5E_CLOSEERROR, NULL); /*close failed*/
      }
      H5MM_xfree (lf);
   }

   FUNC_LEAVE (NULL);
}
      

/*-------------------------------------------------------------------------
 * Function:	H5F_low_read
 *
 * Purpose:	Reads SIZE bytes of data beginning at address ADDR of the
 *		file LF and puts the result in BUF.  Behavior when reading
 *		past the logical or physical end of file is to return zeros
 *		for that part of the request.
 *
 *		This is only a virtual function; the subclass must define a
 *		read method or this function will fail.
 *
 * Errors:
 *		IO        READERROR     Read failed. 
 *		IO        UNSUPPORTED   No read method. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_read (H5F_low_t *lf, const haddr_t *addr, size_t size,
	      uint8 *buf/*out*/)
{
   herr_t	ret_value = FAIL;
   
   FUNC_ENTER (H5F_low_read, NULL, FAIL);

   assert (lf && lf->type);
   assert (addr && H5F_addr_defined (addr));
   assert (buf);

   if (lf->type->read) {
      if ((ret_value = (lf->type->read)(lf, addr, size, buf))<0) {
	 HRETURN_ERROR (H5E_IO, H5E_READERROR, ret_value);/*read failed*/
      }
   } else {
      HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL);/*no read method*/
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_write
 *
 * Purpose:	Writes SIZE bytes of data from BUF into the file LF beginning
 *		at address ADDR of the file. Writing past the logical or
 *		physical end of file causes the file to be extended.
 *
 *		This is a virtual function only; if the subclass doesn't
 *		define a write method then this function will fail.
 *
 * Errors:
 *		IO        UNSUPPORTED   No write method. 
 *		IO        WRITEERROR    Write failed. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_write (H5F_low_t *lf, const haddr_t *addr, size_t size,
	       const uint8 *buf)
{
   herr_t	ret_value = FAIL;
   haddr_t	tmp_addr;
   
   FUNC_ENTER (H5F_low_write, NULL, FAIL);

   assert (lf && lf->type);
   assert (addr && H5F_addr_defined (addr));
   assert (buf);

   /* Extend the file eof marker if we write past it */
   tmp_addr = *addr;
   H5F_addr_inc (&tmp_addr, size);
   if (H5F_addr_gt (&tmp_addr, &(lf->eof))) {
      fprintf (stderr, "HDF5-DIAG: extending file w/o allocation\n");
      lf->eof = tmp_addr;
   }

   /* Write the data */
   if (lf->type->write) {
      if ((ret_value = (lf->type->write)(lf, addr, size, buf))<0) {
	 HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, ret_value);/*write failed*/
      }
   } else {
      HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL);/*no write method*/
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_flush
 *
 * Purpose:	Flushes file buffers to disk.  For instance, the stdio.h
 *		driver would call fflush().
 *
 *		If the subclass doesn't define a flush method then this
 *		function doesn't do anything.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, November 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_flush (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_low_flush, NULL, FAIL);

   assert (lf && lf->type);

   if (lf->type->flush) {
      if ((lf->type->flush)(lf)<0) {
	 /* Low level flush failed */
	 HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL);
      }
   }

   FUNC_LEAVE (SUCCEED);
}
	 

/*-------------------------------------------------------------------------
 * Function:	H5F_low_size
 *
 * Purpose:	Returns the current logical size of the file in bytes.  This
 *		may differ from the physical size of the file (most
 *		subclasses extend the physical file size during the write
 *		operation instead of the alloc operation).
 *		
 *		The next absolute file address is returned through the
 *		EOF argument.  This is the address of the logical end of
 *		file (that is, the address of the first byte past the last
 *		byte which is logically in the file).
 *
 * Warning:	The return value type (size_t) may not be large enough to
 *		represent the true size of the file.  In such cases, the
 *		maximum possible size is returned.  It is better to look at
 *		the EOF output argument to determine the total size.
 *
 * Errors:
 *		IO        UNSUPPORTED   No size method. 
 *
 * Return:	Success:	Current size of file
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5F_low_size (H5F_low_t *lf, haddr_t *eof/*out*/)
{
   size_t	size = (size_t)(-1);	/*max possible size*/
   
   FUNC_ENTER (H5F_low_size, NULL, 0);

   assert (lf && lf->type);
   assert (eof);

   *eof = lf->eof;
   if (eof->offset < size) size = eof->offset;

   FUNC_LEAVE (size);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_access
 *
 * Purpose:	Determines if a file can be accessed in a particular way by a
 *		particular subclass.  The access modes for a file are the
 *		same as those of access(2), namely
 *
 *		F_OK:	determines if the file (or all parts of a multi-part
 *			file) exists.
 *
 *		R_OK:	determines if the file (or all parts of a multi-part
 *			file) are readable.
 *
 *		W_OK:	determines if the file (or all parts of a multi-part
 *			file) are writable.
 *
 *		If a subclass doesn't define an access method, then we treat
 *		the name as if it were a local Unix file and test
 *		accessibility with the access(2) function.  The KEY is
 *		returned as a device number and i-node pair.
 *
 * Return:	Success:	TRUE or FALSE.  If TRUE, then KEY is
 *				initialized with data that makes this file
 *				unique (same value as H5F_low_open).
 *
 *		Failure:	FAIL, KEY is undefined.
 *
 * Programmer:	Robb Matzke
 *              Friday, October 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5F_low_access (const H5F_low_class_t *type, const char *name, int mode,
		H5F_search_t *key/*out*/)
{
   hbool_t	ret_value;
   struct stat	sb;
   
   FUNC_ENTER (H5F_low_size, NULL, 0);
   assert (type);

   if (type->access) {
      ret_value = (type->access)(name, mode, key/*out*/);
      
   } else {
      ret_value = (0==access (name, mode));
      if (key) {
	 stat (name, &sb);
	 key->dev = sb.st_dev;
	 key->ino = sb.st_ino;
      }
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_extend
 *
 * Purpose:	Increases the logical size of a file by moving the logical
 *		end of file marker.  A subclass can override this function by
 *		providing its own allocation method.
 *
 * Return:	Success:	SUCCEED, the address of the old end-of-file
 *				is returned through the ADDR argument and the
 *				logical size of the file has been extended by
 *				SIZE bytes.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_extend (H5F_low_t *lf, intn op, size_t size, haddr_t *addr/*out*/)
{
   FUNC_ENTER (H5F_low_alloc, NULL, FAIL);

   assert (lf);
   assert (size>0);
   assert (addr);

   if (lf->type->extend) {
      if ((lf->type->extend)(lf, op, size, addr/*out*/)<0) {
	 /* Unable to extend file */
	 HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL);
      }
   } else {
      *addr = lf->eof;
      H5F_addr_inc (&(lf->eof), size);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_seteof
 *
 * Purpose:	Sets the logical end-of-file to the specified address.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_seteof (H5F_low_t *lf, const haddr_t *addr)
{
   FUNC_ENTER (H5F_low_seteof, NULL, FAIL);

   assert (lf);
   assert (addr && H5F_addr_defined (addr));

   lf->eof = *addr;

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_addr_cmp
 *
 * Purpose:	Compares two addresses.
 *
 * Return:	Success:	<0 if A1<A2
 * 				=0 if A1=A2
 * 				>0 if A1>A2
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5F_addr_cmp (const haddr_t *a1, const haddr_t *a2)
{
   FUNC_ENTER (H5F_addr_cmp, NULL, FAIL);
   
   assert (a1 && H5F_addr_defined (a1));
   assert (a2 && H5F_addr_defined (a2));

   if (a1->offset<a2->offset) HRETURN (-1);
   if (a1->offset>a2->offset) HRETURN (1);

   FUNC_LEAVE (0);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_undef
 *
 * Purpose:	Cause an address to become undefined.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_undef (haddr_t *addr)
{
   addr->offset = -1;
}


/*-------------------------------------------------------------------------
 * Function:	H5F_addr_defined
 *
 * Purpose:	Determines if an address has a defined value.
 *
 * Return:	Success:	TRUE or FALSE
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5F_addr_defined (const haddr_t *addr)
{
   FUNC_ENTER (H5F_addr_defined, NULL, FAIL);
   FUNC_LEAVE (-1!=addr->offset && addr->offset>=0);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_reset
 *
 * Purpose:	Reset the address to zero.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_reset (haddr_t *addr)
{
   addr->offset = 0;
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_zerop
 *
 * Purpose:	Determines if an address is zero.
 *
 * Return:	Success:	TRUE or FALSE
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5F_addr_zerop (const haddr_t *addr)
{
   FUNC_ENTER (H5F_addr_zerop, NULL, FAIL);
   FUNC_LEAVE (0==addr->offset);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_encode
 *
 * Purpose:	Encodes an address into the buffer pointed to by *PP and
 *		then increments the pointer to the first byte after the
 *		address.  An undefined value is stored as all 1's.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_encode (H5F_t *f, uint8 **pp, const haddr_t *addr)
{
   int		i;
   haddr_t	tmp;
   
   assert (f);
   assert (pp && *pp);
   assert (addr);

   if (H5F_addr_defined (addr)) {
      tmp = *addr;
      for (i=0; i<H5F_SIZEOF_OFFSET (f); i++) {
	 *(*pp)++ = tmp.offset & 0xff;
	 tmp.offset >>= 8;
      }
      assert ("overflow" && 0==tmp.offset);
      
   } else {
      for (i=0; i<H5F_SIZEOF_OFFSET (f); i++) {
	 *(*pp)++ = 0xff;
      }
   }
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_decode
 *
 * Purpose:	Decodes an address from the buffer pointed to by *PP and
 *		updates the pointer to point to the next byte after the
 *		address.
 *
 *		If the value read is all 1's then the address is returned
 *		with an undefined value.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_decode (H5F_t *f, const uint8 **pp, haddr_t *addr/*out*/)
{
   int		i;
   haddr_t	tmp;
   uint8	c;
   hbool_t	all_zero = TRUE;
   
   assert (f);
   assert (pp && *pp);
   assert (addr);

   addr->offset = 0;
   for (i=0; i<H5F_SIZEOF_OFFSET (f); i++) {
      c = *(*pp)++;
      if (c!=0xff) all_zero = FALSE;
      
      if (i<sizeof(addr->offset)) {
	 tmp.offset = c;
	 tmp.offset <<= i*8; /*use tmp to get casting right*/
	 addr->offset |= tmp.offset;
      } else if (!all_zero) {
	 assert (0==**pp); /*overflow*/
      }
   }
   if (all_zero) H5F_addr_undef (addr);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_print
 *
 * Purpose:	Print an address
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_print (FILE *stream, const haddr_t *addr)
{
   haddr_t	tmp;
   
   assert (stream);
   assert (addr);

   if (H5F_addr_defined (addr)) {
      /*
       * It would be nice if we could use the `%Lu', `%llu', or `%qu', but
       * we don't know which is supported.  So we split the address into a
       * low 4-bytes and a high 4-bytes.  If the high 4-bytes are non-zero
       * then we print the address in hexadecimal, otherwise we use decimal.
       */
      tmp = *addr;
      tmp.offset >>= 32;
      if (tmp.offset) {
	 fprintf (stream, "0x%08lx%08lx",
		  (unsigned long)(tmp.offset),
		  (unsigned long)(addr->offset & 0xffffffff));
      } else {
	 fprintf (stream, "%lu", (unsigned long)(addr->offset));
      }
   } else {
      fprintf (stream, "UNDEF");
   }
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_pow2
 *
 * Purpose:	Returns an address which is a power of two.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_pow2 (uintn n, haddr_t *addr/*out*/)
{
   assert (n>=0);
   assert (addr);
   assert (n<8*sizeof(addr->offset));

   addr->offset = 1;
   addr->offset <<= n;
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_inc
 *
 * Purpose:	Increments an address by some number of bytes.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_inc (haddr_t *addr/*in,out*/, size_t inc)
{
   assert (addr && H5F_addr_defined (addr));
   assert (addr->offset<=addr->offset+inc);
   addr->offset += inc;
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_add
 *
 * Purpose:	Adds two addresses and puts the result in the first argument.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_add (haddr_t *a1/*in,out*/, const haddr_t *a2)
{
   assert (a1 && H5F_addr_defined (a1));
   assert (a2 && H5F_addr_defined (a2));
   a1->offset += a2->offset;
}



/*-------------------------------------------------------------------------
 * Function:	H5F_addr_hash
 *
 * Purpose:	Computes a hash value of an address between 0 and MOD-1,
 *		inclusive.
 *
 * Return:	Success:	The hash value
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
uintn
H5F_addr_hash (const haddr_t *addr, uintn mod)
{
   assert (addr && H5F_addr_defined (addr));
   assert (mod>0);

   return addr->offset % mod; /*ignore file number*/
}
