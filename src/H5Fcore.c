/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Wednesday, October 22, 1997
 *
 * Purpose:    This file implements an in-core temporary file.  It's intended
 *	       for storing small temporary files such as wrappers generated
 *	       on the fly.
 *
 * Note:       This is mostly an exercise to help clean up parts of the H5F
 *	       package since this driver is quite different than the other
 *	       low level drivers we have so far.
 *
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#define H5F_CORE_INC	10240	/*amount by which to grow file		*/
#define H5F_CORE_DEV	0xffff	/*pseudo dev for core until we fix things*/

#define PABLO_MASK	H5F_core
static hbool_t interface_initialize_g = FALSE;

static hbool_t H5F_core_access (const char *name, int mode, H5F_search_t *key);
static H5F_low_t *H5F_core_open (const char *name, uintn flags, H5F_search_t*);
static herr_t H5F_core_close (H5F_low_t *lf);
static herr_t H5F_core_read (H5F_low_t *lf, haddr_t addr, size_t size,
			     uint8 *buf);
static herr_t H5F_core_write (H5F_low_t *lf, haddr_t addr, size_t size,
			      const uint8 *buf);
static herr_t H5F_core_flush (H5F_low_t *lf);
static size_t H5F_core_size (H5F_low_t *lf);


const H5F_low_class_t H5F_LOW_CORE[1] = {{
   H5F_core_access, 		/* access method			*/
   H5F_core_open, 		/* open method				*/
   H5F_core_close, 		/* close method				*/
   H5F_core_read,		/* read method				*/
   H5F_core_write, 		/* write method				*/
   H5F_core_flush, 		/* flush method				*/
   H5F_core_size, 		/* file size method			*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5F_core_access
 *
 * Purpose:	Determines if the specified file already exists.  This driver
 *		doesn't use names, so every call to H5F_core_open() would
 *		create a new file.  Therefore, this function always returns
 *		false.
 *
 * Return:	Success:	FALSE
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, October 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5F_core_access (const char *name, int mode, H5F_search_t *key)
{
   FUNC_ENTER (H5F_core_access, NULL, FAIL);
   FUNC_LEAVE (FALSE);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_core_open
 *
 * Purpose:	Opens a temporary file which will exist only in memory.  The
 *		NAME argument is unused.  The FLAGS are a bit field with
 *		the possible values defined in H5F_low_open().
 *
 * Errors:
 *		IO        CANTOPENFILE  Must creat file with write access. 
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
H5F_core_open (const char *name, uintn flags, H5F_search_t *key)
{
   H5F_low_t		*lf = NULL;
   static ino_t		ino=0;

   FUNC_ENTER (H5F_core_open, NULL, NULL);

   if (0==(flags & H5F_ACC_WRITE) || 0==(flags & H5F_ACC_CREAT)) {
      /* must creat file with write access */
      HRETURN_ERROR (H5E_IO, H5E_CANTOPENFILE, NULL);
   }
      
   lf = H5MM_xcalloc (1, sizeof(H5F_low_t));
   lf->u.core.mem = H5MM_xmalloc (H5F_CORE_INC);
   lf->u.core.alloc = H5F_CORE_INC;
   lf->u.core.size = 0;

   if (key) {
      key->dev = H5F_CORE_DEV;
      key->ino = ino++;
   }
   
   FUNC_LEAVE (lf);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_core_close
 *
 * Purpose:	Closes a file.
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
H5F_core_close (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_core_close, NULL, FAIL);

   lf->u.core.mem = H5MM_xfree (lf->u.core.mem);
   lf->u.core.size = 0;
   lf->u.core.alloc = 0;

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_core_read
 *
 * Purpose:	Reads SIZE bytes beginning at address ADDR in file LF and
 *		places them in buffer BUF.  Reading past the end of the
 *		file returns zeros instead of failing.
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
H5F_core_read (H5F_low_t *lf, haddr_t addr, size_t size, uint8 *buf)
{
   size_t	n;
   
   FUNC_ENTER (H5F_core_read, NULL, FAIL);

   if (addr>=lf->u.core.size) {
      HDmemset (buf, 0, size);
   } else {
      n = MIN (size, lf->u.core.size - addr);
      HDmemcpy (buf, lf->u.core.mem + addr, n);
      HDmemset (buf+n, 0, size-n);
   }
   
   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_core_write
 *
 * Purpose:	Writes SIZE bytes from the beginning of BUF into file LF at
 *		file address ADDR.
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
H5F_core_write (H5F_low_t *lf, haddr_t addr, size_t size, const uint8 *buf)
{
   FUNC_ENTER (H5F_core_write, NULL, FAIL);

   /* Allocate more space */
   if (addr+size>lf->u.core.alloc) {
      lf->u.core.alloc = lf->u.core.alloc + MAX (addr+size-lf->u.core.alloc,
						 H5F_CORE_INC);
      lf->u.core.mem = H5MM_xrealloc (lf->u.core.mem, lf->u.core.alloc);
   }

   /* Move EOF marker */
   if (addr+size>lf->u.core.size) {
      lf->u.core.size = addr + size;
   }

   /* Copy data */
   HDmemcpy (lf->u.core.mem+addr, buf, size);

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_core_flush
 *
 * Purpose:	Makes sure that all data is flushed.  This doesn't apply to
 *		this driver.
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
H5F_core_flush (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_core_flush, NULL, FAIL);

   /* Not necessary with this driver */

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_core_size
 *
 * Purpose:	Returns the current size of the file in bytes.
 *
 * Bugs:	There is no way to determine if this function failed.
 *
 * Errors:
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
H5F_core_size (H5F_low_t *lf)
{
   FUNC_ENTER (H5F_core_size, NULL, 0);

   FUNC_LEAVE (lf->u.core.size);
}
