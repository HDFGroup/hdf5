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
H5F_low_open (const H5F_low_class_t *type, const char *name, uintn flags)
{
   H5F_low_t	*lf = NULL;
   
   FUNC_ENTER (H5F_low_open, NULL, NULL);

   assert (type && type->open);
   assert (name && *name);

   if (NULL==(lf=(type->open)(name, flags))) {
      HRETURN_ERROR (H5E_IO, H5E_CANTOPENFILE, NULL);/*open failed*/
   }
   lf->type = type;

   FUNC_LEAVE (lf);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_low_close
 *
 * Purpose:	Closes a low-level file.
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

   if (lf && (lf->type->close)(lf)<0) {
      HRETURN_ERROR (H5E_IO, H5E_CLOSEERROR, NULL); /*close failed*/
      H5MM_xfree (lf);
   }

   FUNC_LEAVE (NULL);
}
      

/*-------------------------------------------------------------------------
 * Function:	H5F_low_read
 *
 * Purpose:	Reads SIZE bytes of data beginning at address ADDR of the
 *		file LF and puts the result in BUF.
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
H5F_low_read (H5F_low_t *lf, haddr_t addr, size_t size, uint8 *buf)
{
   herr_t	ret_value = FAIL;
   
   FUNC_ENTER (H5F_low_read, NULL, FAIL);

   assert (lf && lf->type);
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
 *		at address ADDR of the file.
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
H5F_low_write (H5F_low_t *lf, haddr_t addr, size_t size, const uint8 *buf)
{
   herr_t	ret_value = FAIL;
   
   FUNC_ENTER (H5F_low_write, NULL, FAIL);

   assert (lf && lf->type);
   assert (buf);

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
 * Function:	H5F_low_size
 *
 * Purpose:	Returns the current size of the file in bytes.
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
H5F_low_size (H5F_low_t *lf)
{
   size_t	size;
   
   FUNC_ENTER (H5F_low_size, NULL, 0);

   assert (lf && lf->type);

   if (lf->type->size) {
      size = (lf->type->size)(lf);
   } else {
      HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, 0);/*no size method*/
   }

   FUNC_LEAVE (size);
}

