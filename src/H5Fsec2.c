/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Wednesday, October 22, 1997
 *
 * Purpose:    This is the Posix section-2 I/O subclass of H5Flow.
 *
 * Notes:      This driver keeps track of its own file position in order to
 *	       minimize the number of calls to lseek().  We assume that
 *	       opening a file sets the current file position to the beginning
 *	       and that read() and write() modify the file position as
 *	       expected when they return successfully (unsuccessful return
 *	       leaves the file position undefined).
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#include <sys/types.h>
#include <sys/stat.h>


#define PABLO_MASK	H5F_sec2
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT	NULL

static H5F_low_t *H5F_sec2_open (const char *name, uintn flags, H5F_search_t*);
static herr_t H5F_sec2_close (H5F_low_t *lf);
static herr_t H5F_sec2_read (H5F_low_t *lf, const haddr_t *addr, size_t size,
			     uint8 *buf);
static herr_t H5F_sec2_write (H5F_low_t *lf, const haddr_t *addr, size_t size,
			      const uint8 *buf);

const H5F_low_class_t H5F_LOW_SEC2[1] = {{
   NULL, 			/* access method			*/
   H5F_sec2_open, 		/* open method				*/
   H5F_sec2_close, 		/* close method				*/
   H5F_sec2_read,		/* read method				*/
   H5F_sec2_write, 		/* write method				*/
   NULL,	 		/* flush method				*/
   NULL, 			/* extend method			*/
}};



/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_open
 *
 * Purpose:	Opens a file with name NAME.  The FLAGS are a bit field with
 *		the possible values defined in H5F_low_open().
 *
 * Errors:
 *		IO        CANTOPENFILE  Open failed. 
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
H5F_sec2_open (const char *name, uintn flags, H5F_search_t *key/*out*/)
{
   uintn		oflags;
   H5F_low_t		*lf = NULL;
   int			fd;
   struct stat		sb;

   FUNC_ENTER (H5F_sec2_open, NULL);

   oflags = (flags & H5F_ACC_WRITE) ? O_RDWR : O_RDONLY;
   oflags |= (flags & H5F_ACC_CREAT) ? O_CREAT : 0;
   oflags |= (flags & H5F_ACC_EXCL) ? O_EXCL : 0;
   oflags |= (flags & H5F_ACC_TRUNC) ? O_TRUNC : 0;

   if ((fd=open (name, oflags, 0666))<0) {
      HRETURN_ERROR (H5E_IO, H5E_CANTOPENFILE, NULL);/*open failed*/
   }
      
   lf = H5MM_xcalloc (1, sizeof(H5F_low_t));
   lf->u.sec2.fd = fd;
   lf->u.sec2.op = H5F_OP_SEEK;
   lf->u.sec2.cur = 0;
   fstat (fd, &sb);
   lf->eof.offset = sb.st_size;

   if (key) {
      key->dev = sb.st_dev;
      key->ino = sb.st_ino;
   }

   FUNC_LEAVE (lf);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_close
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
H5F_sec2_close (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_sec2_close, FAIL);

   if (close (lf->u.sec2.fd)<0) {
      HRETURN_ERROR (H5E_IO, H5E_CLOSEERROR, FAIL); /*close failed*/
   }
   lf->u.sec2.fd = -1;

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_read
 *
 * Purpose:	Reads SIZE bytes beginning at address ADDR in file LF and
 *		places them in buffer BUF.  Reading past the logical or
 *		physical end of file returns zeros instead of failing.
 *
 * Errors:
 *		IO        READERROR     Read failed. 
 *		IO        SEEKERROR     Lseek failed. 
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
H5F_sec2_read (H5F_low_t *lf, const haddr_t *addr, size_t size, uint8 *buf)
{
   ssize_t	n;
   off_t	offset;
   
   FUNC_ENTER (H5F_sec2_read, FAIL);


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
    * Optimize seeking.  If that optimization is disabled then always call
    * lseek().
    */
   if (!H5F_OPT_SEEK ||
       lf->u.sec2.op==H5F_OP_UNKNOWN ||
       lf->u.sec2.cur!=offset) {
      if (lseek (lf->u.sec2.fd, offset, SEEK_SET)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL); /*lseek failed*/
      }
      lf->u.sec2.cur = offset;
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
    * Read the data.  If a read error occurs then set the last file operation
    * to UNKNOWN because the file position isn't guaranteed by Posix.
    */
   if ((n=read (lf->u.sec2.fd, buf, size))<0) {
      lf->u.sec2.op = H5F_OP_UNKNOWN;
      HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL); /*read failed*/
   } else if (n<size) {
      HDmemset (buf+n, 0, size-n);
   }

   /*
    * Update the file position with the number of bytes actually read.  This
    * might be different than the number requested.
    */
   lf->u.sec2.op = H5F_OP_READ;
   lf->u.sec2.cur = offset + n;
   assert ("address overflowed" && lf->u.sec2.cur>=offset);
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_write
 *
 * Purpose:	Writes SIZE bytes from the beginning of BUF into file LF at
 *		file address ADDR.
 *
 * Errors:
 *		IO        SEEKERROR     Lseek failed. 
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
static herr_t
H5F_sec2_write (H5F_low_t *lf, const haddr_t *addr, size_t size,
		const uint8 *buf)
{
   off_t	offset;
   
   FUNC_ENTER (H5F_sec2_write, FAIL);

   /* Check for overflow */
   offset = addr->offset;
   assert ("address overflowed" && offset==addr->offset);
   assert ("overflow" && offset+size>=offset);

   /*
    * Optimize seeking. If that optimization is disabled then always call
    * lseek().
    */
   if (!H5F_OPT_SEEK ||
       lf->u.sec2.op==H5F_OP_UNKNOWN ||
       lf->u.sec2.cur!=offset) {
      if (lseek (lf->u.sec2.fd, offset, SEEK_SET)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL); /*lseek failed*/
      }
      lf->u.sec2.cur = offset;
   }

   /*
    * Write the data to the file.  If the write failed then set the
    * operation back to UNKNOWN since Posix doesn't gurantee its value.
    */
   if (size != write (lf->u.sec2.fd, buf, size)) {
      lf->u.sec2.op = H5F_OP_UNKNOWN;
      HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL); /*write failed*/
   }

   /*
    * Update the file position.
    */
   lf->u.sec2.op = H5F_OP_WRITE;
   lf->u.sec2.cur = offset + size;
   assert ("address overflowed" && lf->u.sec2.cur>=offset);

   FUNC_LEAVE (SUCCEED);
}
