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

static H5F_low_t *H5F_sec2_open (const char *name, uintn flags, H5F_search_t*);
static herr_t H5F_sec2_close (H5F_low_t *lf);
static herr_t H5F_sec2_read (H5F_low_t *lf, haddr_t addr, size_t size,
			     uint8 *buf);
static herr_t H5F_sec2_write (H5F_low_t *lf, haddr_t addr, size_t size,
			      const uint8 *buf);
static herr_t H5F_sec2_flush (H5F_low_t *lf);
static size_t H5F_sec2_size (H5F_low_t *lf);


const H5F_low_class_t H5F_LOW_SEC2[1] = {{
   NULL, 			/* use default access(2) func		*/
   H5F_sec2_open, 		/* open method				*/
   H5F_sec2_close, 		/* close method				*/
   H5F_sec2_read,		/* read method				*/
   H5F_sec2_write, 		/* write method				*/
   H5F_sec2_flush, 		/* flush method				*/
   H5F_sec2_size, 		/* file size method			*/
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
H5F_sec2_open (const char *name, uintn flags, H5F_search_t *key)
{
   uintn		oflags;
   H5F_low_t		*lf = NULL;
   int			fd;
   struct stat		sb;

   FUNC_ENTER (H5F_sec2_open, NULL, NULL);

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

   if (key) {
      fstat (fd, &sb);
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
   FUNC_ENTER (H5F_sec2_close, NULL, FAIL);

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
 *		places them in buffer BUF.  Reading past the end of the
 *		file returns zeros instead of failing.
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
H5F_sec2_read (H5F_low_t *lf, haddr_t addr, size_t size, uint8 *buf)
{
   ssize_t	n;
   
   FUNC_ENTER (H5F_sec2_read, NULL, FAIL);

   /*
    * Optimize seeking.  If that optimization is disabled then alwasy call
    * lseek().
    */
   if (!H5F_OPT_SEEK ||
       lf->u.sec2.op==H5F_OP_UNKNOWN || lf->u.sec2.cur!=addr) {
      if (lseek (lf->u.sec2.fd, addr, SEEK_SET)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL); /*lseek failed*/
      }
      lf->u.sec2.cur = addr;
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
   lf->u.sec2.cur = addr + n;
   
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
H5F_sec2_write (H5F_low_t *lf, haddr_t addr, size_t size, const uint8 *buf)
{
   FUNC_ENTER (H5F_sec2_write, NULL, FAIL);

   /*
    * Optimize seeking. If that optimization is disabled then always call
    * lseek().
    */
   if (!H5F_OPT_SEEK ||
       lf->u.sec2.op==H5F_OP_UNKNOWN || lf->u.sec2.cur!=addr) {
      if (lseek (lf->u.sec2.fd, addr, SEEK_SET)<0) {
	 HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL); /*lseek failed*/
      }
      lf->u.sec2.cur = addr;
   }

   /*
    * Read the data from the file.  If the write failed then set the
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
   lf->u.sec2.cur = addr + size;
   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_flush
 *
 * Purpose:	Makes sure that all data is on disk.
 *
 * Errors:
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
H5F_sec2_flush (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_sec2_flush, NULL, FAIL);

   /* Not necessary with this driver */

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_size
 *
 * Purpose:	Returns the current size of the file in bytes.
 *
 * Bugs:	There is no way to determine if this function failed.
 *
 * Errors:
 *		IO        SEEKERROR     Lseek failed. 
 *
 * Return:	Success:	Size of file in bytes
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
static size_t
H5F_sec2_size (H5F_low_t *lf)
{
   off_t	size;
   
   FUNC_ENTER (H5F_sec2_size, NULL, 0);

   if ((size=lseek (lf->u.sec2.fd, 0, SEEK_END))<0) {
      lf->u.sec2.op = H5F_OP_UNKNOWN;
      HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, 0); /*lseek failed*/
   }
   
   lf->u.sec2.op = H5F_OP_SEEK;
   lf->u.sec2.cur = size;

   FUNC_LEAVE (size);
}
