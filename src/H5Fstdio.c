/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Wednesday, October 22, 1997
 *
 * Purpose:    This is the Posix stdio.h I/O subclass of H5Flow.
 */
#include <H5private.h>
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#include <sys/types.h>
#include <sys/stat.h>


#define PABLO_MASK	H5F_sec2
static hbool_t interface_initialize_g = FALSE;

static H5F_low_t *H5F_stdio_open (const char *name, uintn flags,
				  H5F_search_t *key);
static herr_t H5F_stdio_close (H5F_low_t *lf);
static herr_t H5F_stdio_read (H5F_low_t *lf, const haddr_t *addr, size_t size,
			      uint8 *buf);
static herr_t H5F_stdio_write (H5F_low_t *lf, const haddr_t *addr, size_t size,
			      const uint8 *buf);
static herr_t H5F_stdio_flush (H5F_low_t *lf);

const H5F_low_class_t H5F_LOW_STDIO[1] = {{
   NULL,			/* use default access(2) func  		*/
   H5F_stdio_open, 		/* open method				*/
   H5F_stdio_close, 		/* close method				*/
   H5F_stdio_read,		/* read method				*/
   H5F_stdio_write, 		/* write method				*/
   H5F_stdio_flush, 		/* flush method				*/
   NULL, 			/* extend method			*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_open
 *
 * Purpose:	Opens a file with name NAME.  The FLAGS are a bit field with
 *		the possible values defined in H5F_low_open().
 *
 * Bugs:	H5F_ACC_EXCL has a race condition.
 *
 * Errors:
 *		IO        CANTOPENFILE  File doesn't exist and CREAT wasn't
 *		                        specified. 
 *		IO        CANTOPENFILE  Fopen failed. 
 *		IO        FILEEXISTS    File exists but CREAT and EXCL were
 *		                        specified. 
 *
 * Return:	Success:	Low-level file pointer
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
static H5F_low_t *
H5F_stdio_open (const char *name, uintn flags, H5F_search_t *key/*out*/)
{
   H5F_low_t	*lf=NULL;
   FILE		*f=NULL;
   struct stat	sb;

   FUNC_ENTER (H5F_stdio_open, NULL, NULL);

   if (access (name, F_OK)<0) {
      if ((flags & H5F_ACC_CREAT) && (flags & H5F_ACC_WRITE)) {
	 f = fopen (name, "wb+");
      } else {
	 /* File doesn't exist and CREAT wasn't specified */
	 HRETURN_ERROR (H5E_IO, H5E_CANTOPENFILE, NULL);
      }
      
   } else if ((flags & H5F_ACC_CREAT) && (flags & H5F_ACC_EXCL)) {
      /* File exists but CREAT and EXCL were specified */
      HRETURN_ERROR (H5E_IO, H5E_FILEEXISTS, NULL);
      
   } else if (flags & H5F_ACC_WRITE) {
      if (flags & H5F_ACC_TRUNC) f = fopen (name, "wb+");
      else f = fopen (name, "rb+");
      
   } else {
      f = fopen (name, "rb");
   }
   if (!f) HRETURN_ERROR (H5E_IO, H5E_CANTOPENFILE, NULL); /*fopen failed*/

   /* Build the return value */
   lf = H5MM_xcalloc (1, sizeof(H5F_low_t));
   lf->u.stdio.f = f;
   lf->u.stdio.op = H5F_OP_SEEK;
   lf->u.stdio.cur = 0;
   H5F_addr_reset (&(lf->eof));
   if (fseek (lf->u.stdio.f, 0, SEEK_END)<=0) {
      lf->u.stdio.op = H5F_OP_UNKNOWN;
   } else {
      H5F_addr_inc (&(lf->eof), ftell (lf->u.stdio.f));
   }

   /* The unique key */
   if (key) {
      fstat (fileno (f), &sb);
      key->dev = sb.st_dev;
      key->ino = sb.st_ino;
   }

   FUNC_LEAVE (lf);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_close
 *
 * Purpose:	Closes a file.
 *
 * Errors:
 *		IO        CLOSEERROR    Close failed. 
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
static herr_t
H5F_stdio_close (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_stdio_close, NULL, FAIL);

   if (fclose (lf->u.stdio.f)<0) {
      HRETURN_ERROR (H5E_IO, H5E_CLOSEERROR, FAIL); /*close failed*/
   }
   lf->u.stdio.f = NULL;

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_read
 *
 * Purpose:	Reads SIZE bytes beginning at address ADDR in file LF and
 *		places them in buffer BUF.  Reading past the logical or
 *		physical end of file returns zeros instead of failing.
 *
 * Errors:
 *		IO        READERROR     Fread failed. 
 *		IO        SEEKERROR     Fseek failed. 
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
static herr_t
H5F_stdio_read (H5F_low_t *lf, const haddr_t *addr, size_t size, uint8 *buf)
{
   size_t	n;
   off_t	offset;
   
   FUNC_ENTER (H5F_stdio_read, NULL, FAIL);

   /* Check for overflow */
   offset = addr->offset;
   assert ("address overflowed" && offset==addr->offset);
   assert ("overflow" && offset+size>=offset);

   /* Check easy cases */
   if (0==size) HRETURN (SUCCEED);
   if (offset>=lf->eof.offset) {
      HDmemset (buf, 0, size);
      HRETURN (SUCCEED);
   }
   
   /*
    * Seek to the correct file position.
    */
   if (!H5F_OPT_SEEK ||
       lf->u.stdio.op!=H5F_OP_READ ||
       lf->u.stdio.cur!=offset) {
      if (fseek (lf->u.stdio.f, offset, SEEK_SET)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL); /*fseek failed*/
      }
      lf->u.stdio.cur = offset;
   }

   /*
    * Read zeros past the logical end of file (physical is handled below)
    */   
   if ((size_t)offset+size>lf->eof.offset) {
      size_t nbytes = (size_t)offset+size - lf->eof.offset;
      HDmemset (buf+size-nbytes, 0, nbytes);
      size -= nbytes;
   }

   /*
    * Read the data.  Since we're reading single-byte values, a partial read
    * will advance the file position by N.  If N is negative or an error
    * occurs then the file position is undefined.
    */
   n = fread (buf, 1, size, lf->u.stdio.f);
   if (n<=0 && ferror (lf->u.stdio.f)) {
      lf->u.stdio.op = H5F_OP_UNKNOWN;
      HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL); /*fread failed*/
   } else if (n<size) {
      HDmemset (buf+n, 0, size-n);
   }

   /*
    * Update the file position data.
    */
   lf->u.stdio.op = H5F_OP_READ;
   lf->u.stdio.cur = offset + n;
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_write
 *
 * Purpose:	Writes SIZE bytes from the beginning of BUF into file LF at
 *		file address ADDR.
 *
 * Errors:
 *		IO        SEEKERROR     Fseek failed. 
 *		IO        WRITEERROR    Fwrite failed. 
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
static herr_t
H5F_stdio_write (H5F_low_t *lf, const haddr_t *addr, size_t size,
		 const uint8 *buf)
{
   off_t	offset;

   FUNC_ENTER (H5F_stdio_write, NULL, FAIL);

   /* Check for overflow */
   offset = addr->offset;
   assert ("address overflowed" && offset==addr->offset);
   assert ("overflow" && offset+size>=offset);

   /*
    * Seek to the correct file position.
    */
   if (!H5F_OPT_SEEK ||
       lf->u.stdio.op!=H5F_OP_WRITE ||
       lf->u.stdio.cur!=offset) {
      if (fseek (lf->u.stdio.f, offset, SEEK_SET)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL); /*fseek failed*/
      }
      lf->u.stdio.cur = offset;
   }

   /*
    * Write the buffer.  On successful return, the file position will be
    * advanced by the number of bytes read.  Otherwise nobody knows where it
    * is.
    */
   if (size != fwrite (buf, 1, size, lf->u.stdio.f)) {
      lf->u.stdio.op = H5F_OP_UNKNOWN;
      HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL); /*fwrite failed*/
   }

   /*
    * Update seek optimizing data.
    */
   lf->u.stdio.op = H5F_OP_WRITE;
   lf->u.stdio.cur = offset + size;
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_flush
 *
 * Purpose:	Makes sure that all data is on disk.
 *
 * Errors:
 *		IO        WRITEERROR    Fflush failed. 
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
static herr_t
H5F_stdio_flush (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_stdio_flush, NULL, FAIL);

   /*
    * What happens to the file position?  Is it guaranteed to be the same
    * after the fflush() as it was before?
    */
   lf->u.stdio.op = H5F_OP_UNKNOWN;

   /*
    * Flush
    */
   if (fflush (lf->u.stdio.f)<0) {
      HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL); /*fflush failed*/
   }
   
   FUNC_LEAVE (SUCCEED);
}
