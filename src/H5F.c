/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*
* MODIFICATIONS
* 	Robb Matzke, 30 Aug 1997
*	Added `ERRORS' fields to function prologues.
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*
   FILE
       hdf5file.c
   HDF5 file I/O routines

   EXPORTED ROUTINES
       H5Fcreate    -- Create an HDF5 file
       H5Fclose     -- Close an open HDF5 file

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
       H5F_init_interface    -- initialize the H5F interface
 */

/* Packages needed by this file... */
#include <H5private.h>      	/*library functions			*/
#include <H5Aprivate.h>		/*atoms					*/
#include <H5ACprivate.h>	/*cache					*/
#include <H5Cprivate.h>		/*templates				*/
#include <H5Eprivate.h>		/*error handling			*/
#include <H5Gprivate.h>		/*symbol tables				*/
#include <H5Mprivate.h>		/*meta data				*/
#include <H5MMprivate.h>	/*core memory management		*/

#define PABLO_MASK	H5F_mask

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize_g = FALSE;

/*--------------------- Local function prototypes ----------------------------*/
static herr_t H5F_init_interface(void);
static hdf5_file_t *H5F_new (void);
static hdf5_file_t *H5F_dest (hdf5_file_t *f);
static herr_t H5F_flush (hdf5_file_t *f, hbool_t invalidate);

/*--------------------------------------------------------------------------
NAME
   H5F_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5F_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

ERRORS

Modifications:
    Robb Matzke, 4 Aug 1997
    Changed pablo mask from H5_mask to H5F_mask for the FUNC_LEAVE call.
    It was already H5F_mask for the PABLO_TRACE_ON call.

--------------------------------------------------------------------------*/
static herr_t H5F_init_interface(void)
{
    herr_t ret_value = SUCCEED;
    FUNC_ENTER (H5F_init_interface, NULL, FAIL);

    /* Initialize the atom group for the file IDs */
    if((ret_value=H5Ainit_group(H5_FILE,H5A_FILEID_HASHSIZE,0))!=FAIL)
        ret_value=H5_add_exit(&H5F_term_interface);

    FUNC_LEAVE(ret_value);
}	/* H5F_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5F_term_interface
 PURPOSE
    Terminate various H5F objects
 USAGE
    void H5F_term_interface()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void H5F_term_interface (void)
{
    H5Adestroy_group(H5_FILE);
} /* end H5F_term_interface() */

#ifdef LATER
/*--------------------------------------------------------------------------
 NAME
       H5F_encode_length_unusual -- encode an unusual length size
 USAGE
       void H5F_encode_length_unusual(f, p, l)
       const hdf5_file_t *f;             IN: pointer to the file record
       uint8 **p;               IN: pointer to buffer pointer to encode length in
       uint8 *l;                IN: pointer to length to encode

 ERRORS

 RETURNS
    none
 DESCRIPTION
    Encode non-standard (i.e. not 2, 4 or 8-byte) lengths in file meta-data.
--------------------------------------------------------------------------*/
void H5F_encode_length_unusual(const hdf5_file_t *f, uint8 **p, uint8 *l)
{
    intn i = H5F_SIZEOF_SIZE (f);

/* For non-little-endian platforms, encode each byte in memory backwards */
#if ((DF_MT&0xFFF0)!=0x4440)
    for(; i>=0; i--,(*p)++)
        *(*p)=*(l+i);
#else   /* platform has little-endian integers */
    for(; i>=0; i--,(*p)++)
        *(*p)=*l;
#endif

#ifdef LATER
done:
    if(ret_value == FALSE)   
      { /* Error condition cleanup */

      } /* end if */
#endif /* LATER */

    /* Normal function cleanup */

}	/* H5F_encode_length_unusual */

/*--------------------------------------------------------------------------
 NAME
       H5F_encode_offset_unusual -- encode an unusual offset size
 USAGE
       void H5F_encode_offset_unusual(f, p, o)
       const hdf5_file_t *f;             IN: pointer to the file record
       uint8 **p;               IN: pointer to buffer pointer to encode offset in
       uint8 *o;                IN: pointer to offset to encode

ERRORS

 RETURNS
    none
 DESCRIPTION
    Encode non-standard (i.e. not 2, 4 or 8-byte) offsets in file meta-data.
--------------------------------------------------------------------------*/
void H5F_encode_offset_unusual(const hdf5_file_t *f, uint8 **p, uint8 *o)
{
    intn i = H5F_SIZEOF_OFFSET(f);

/* For non-little-endian platforms, encode each byte in memory backwards */
#if ((DF_MT&0xFFF0)!=0x4440)
    for(; i>=0; i--,(*p)++)
        *(*p)=*(o+i);
#else   /* platform has little-endian integers */
    for(; i>=0; i--,(*p)++)
        *(*p)=*o;
#endif

#ifdef LATER
done:
    if(ret_value == FALSE)   
      { /* Error condition cleanup */

      } /* end if */
#endif /* LATER */

    /* Normal function cleanup */

}	/* H5F_encode_offset_unusual */
#endif /* LATER */

/*--------------------------------------------------------------------------
 NAME
       H5F_compare_filename -- compare file objects for the atom API
 USAGE
       intn HPcompare_filename(obj, key)
       const VOIDP obj;             IN: pointer to the file record
       const VOIDP key;             IN: pointer to the name of file

 ERRORS

 RETURNS
       TRUE if the key matches the obj, FALSE otherwise
 DESCRIPTION
       Look inside the file record for the atom API and compare the the
       filenames.
--------------------------------------------------------------------------*/
intn
H5F_compare_filename (const VOIDP _obj, const VOIDP _key)
{
   const hdf5_file_t	*obj = (const hdf5_file_t *)_obj;
   const char		*key = (const char *)_key;
   int			ret_value = FALSE;
   
   FUNC_ENTER (H5F_compare_filename, NULL, FALSE);

   ret_value = !HDstrcmp (obj->filename, key);

   FUNC_LEAVE (ret_value);
}	/* H5F_compare_filename */

/*--------------------------------------------------------------------------
 NAME
    H5Fget_create_template

 PURPOSE
    Get an atom for a copy of the file-creation template for this file

 USAGE
    hatom_t H5Fget_create_template(fid)
        hatom_t fid;    IN: File ID

 ERRORS
    ATOM      BADATOM       Can't get file struct. 
    FUNC      CANTCREATE    Can't create template. 
    FUNC      CANTINIT      Can't init template. 

 RETURNS
    Returns template ID on success, FAIL on failure

 DESCRIPTION
        This function returns an atom with a copy of the template parameters
    used to create a file.
--------------------------------------------------------------------------*/
hatom_t H5Fget_create_template(hatom_t fid)
{
    hdf5_file_t *file=NULL;         /* file struct for file to close */
    hatom_t ret_value = FAIL;

    FUNC_ENTER(H5Fget_create_template, H5F_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the file structure */
    if((file=H5Aatom_object(fid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL); /*can't get file struct*/

    /* Create the template object to return */
    if((ret_value=H5Mcreate(fid,H5_TEMPLATE,NULL))==FAIL)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTCREATE, FAIL); /*can't create template*/

    if(H5C_init(ret_value,&(file->file_create_parms))==FAIL)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL); /*can't init template*/

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Fget_create_template() */

/*--------------------------------------------------------------------------
 NAME
    H5Fis_hdf5

 PURPOSE
    Check the file signature to detect an HDF5 file.

 USAGE
    hbool_t H5Fis_hdf5(filename)
        const char *filename;   IN: Name of the file to check
 ERRORS
    ARGS      BADRANGE      No filename specified. 
    FILE      BADFILE       Low-level file open failure. 
    IO        READERROR     Read error. 
    IO        READERROR     Seek error. 
    IO        SEEKERROR     Unable to determine length of file due to seek
                            failure. 

 RETURNS
    TRUE/FALSE/FAIL

 DESCRIPTION
    This function determines if a file is an HDF5 format file.
--------------------------------------------------------------------------*/
hbool_t H5Fis_hdf5(const char *filename)
{
    hdf_file_t f_handle=H5F_INVALID_FILE;      /* file handle */
    uint8 temp_buf[H5F_SIGNATURE_LEN];    /* temporary buffer for checking file signature */
    haddr_t curr_off=0;          /* The current offset to check in the file */
    size_t file_len=0;          /* The length of the file we are checking */
    hbool_t ret_value = BFALSE;

    FUNC_ENTER(H5Fis_hdf5, H5F_init_interface, BFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(filename==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, BFAIL); /*no filename specified*/

    /* Open the file */
    f_handle=H5F_OPEN(filename,0);
    if(H5F_OPENERR(f_handle)) {
       /* Low-level file open failure */
       HGOTO_ERROR(H5E_FILE, H5E_BADFILE, BFAIL);
    }

    /* Get the length of the file */
    if(H5F_SEEKEND(f_handle)==FAIL) {
       /* Unable to determine length of file due to seek failure */
       HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, BFAIL);
    }
    file_len=H5F_TELL(f_handle);

    /* Check the offsets where the file signature is possible */
    while(curr_off<file_len)
      {
        if(H5F_SEEK(f_handle,curr_off)==FAIL)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, BFAIL); /*seek error*/
        if(H5F_READ(f_handle,temp_buf, H5F_SIGNATURE_LEN)==FAIL)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, BFAIL); /*read error*/
        if(HDmemcmp(temp_buf,H5F_SIGNATURE,H5F_SIGNATURE_LEN)==0)
          {
            ret_value=BTRUE;
            break;
          } /* end if */
        if(curr_off==0)
            curr_off=512;
        else
            curr_off*=2;
      } /* end while */
    H5F_CLOSE(f_handle);   /* close the file we opened */

done:
  if(ret_value == BFAIL)
    { /* Error condition cleanup */

      /* Check if we left a dangling file handle */
      if(f_handle!=H5F_INVALID_FILE)
        H5F_CLOSE(f_handle);   /* close the file we opened */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Fis_hdf5() */


/*-------------------------------------------------------------------------
 * Function:	H5F_new
 *
 * Purpose:	Creates a new file object and initializes it.  The
 *		H5Fopen and H5Fcreate functions then fill in various
 *		fields.
 *
 * Errors:
 *
 * Return:	Success:	Ptr to a new file struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hdf5_file_t *
H5F_new (void)
{
   hdf5_file_t	*f = H5MM_xcalloc (1, sizeof(hdf5_file_t));

   /* Create a main cache */
   H5AC_new (f, H5AC_NSLOTS);

   /* Create the shadow hash table */
   f->nshadows = H5G_NSHADOWS;
   f->shadow = H5MM_xcalloc (f->nshadows, sizeof(struct H5G_hash_t*));

   /* Create a root symbol slot */
   f->root_sym = H5G_ent_calloc ();
   
   return f;
}


/*-------------------------------------------------------------------------
 * Function:	H5F_dest
 *
 * Purpose:	Destroys a file structure.  This function does not flush
 *		the cache or anything else; it only frees memory associated
 *		with the file struct.
 *
 * Errors:
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hdf5_file_t *
H5F_dest (hdf5_file_t *f)
{
   if (f) {
      H5AC_dest (f);
      f->dir = H5MM_xfree (f->dir);
      f->filename = H5MM_xfree (f->filename);
      f->root_sym = H5MM_xfree (f->root_sym);
      f->nshadows = 0;
      f->shadow = H5MM_xfree (f->shadow);
      H5MM_xfree (f);
   }
   return NULL;
}

/*--------------------------------------------------------------------------
 NAME
    H5Fcreate

 PURPOSE
    Create a new HDF5 file.

 USAGE
    int32 H5Fcreate(filename, flags, create_temp, access_temp)
        const char *filename;   IN: Name of the file to create
        uintn flags;            IN: Flags to indicate various options.
        hatom_t create_temp;    IN: File-creation template ID
        hatom_t access_temp;    IN: File-access template ID

 ERRORS
    ARGS      BADVALUE      Invalid file name. 
    ARGS      BADVALUE      Invalid flags. 
    ATOM      BADATOM       Can't atomize template. 
    ATOM      BADATOM       Can't unatomize template. 
    ATOM      CANTREGISTER  Can't atomize file. 
    FILE      CANTCREATE    Unable to create the file due to low level create
                            failure. 
    FILE      FILEEXISTS    File already exists but overwrite permission
                            was not given. 
    FILE      FILEOPEN      File already open. 
    IO        CANTINIT      Can't write file boot block. 
    RESOURCE  NOSPACE       H5F_new() failed. 

 RETURNS
    Returns file ID on success, FAIL on failure

 DESCRIPTION
        This is the primary function for creating HDF5 files . The flags
    parameter determines whether an existing file will be overwritten or not.
    All newly created files are opened for both reading and writing.  All flags
    may be combined with the "||" (logical OR operator) to change the behavior
    of the file open call.
        The flags currently defined:
            H5ACC_OVERWRITE - Truncate file, if it already exists. The file will
                be truncated, erasing all data previously stored in the file.
        The more complex behaviors of a file's creation and access are
    controlled through the file-creation and file-access templates.  The value
    of 0 for a template value indicates that the library should use the default
    values for the appropriate template.  (Documented in the template module).
    [Access templates are currently unused in this routine, although they will
    be implemented in the future]

 MODIFICATIONS:
    Robb Matzke, 18 Jul 1997
    File struct creation and destruction is through H5F_new() H5F_dest().
    Writing the root symbol table entry is done with H5G_encode().

    Robb Matzke, 29 Aug 1997
    Moved creation of the boot block to H5F_flush().
--------------------------------------------------------------------------*/
hatom_t H5Fcreate(const char *filename, uintn flags, hatom_t create_temp, hatom_t access_temp)
{
    hdf5_file_t *new_file=NULL;     /* file struct for new file */
    hdf_file_t f_handle=H5F_INVALID_FILE;  /* file handle */
    const file_create_temp_t *f_create_parms;    /* pointer to the parameters to use when creating the file */
    intn file_exists=0;             /* flag to indicate that file exists already */
    hatom_t ret_value = FAIL;

    FUNC_ENTER(H5Fcreate, H5F_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(filename==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL); /*invalid file name*/
    if((flags&~H5ACC_OVERWRITE)!=0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL); /*invalid flags*/

    /* See if this file is already open */
    if(H5Asearch_atom(H5_FILE,H5F_compare_filename,(const VOIDP)filename)!=NULL)
        HGOTO_ERROR(H5E_FILE, H5E_FILEOPEN, FAIL); /*file already open*/

    /* Check if the file already exists */
    f_handle=H5F_OPEN(filename,0);
    if(!H5F_OPENERR(f_handle))
      {
        file_exists=1;  /* set the flag to indicate that the file already exists */
        H5F_CLOSE(f_handle);   /* close the file we opened */
        f_handle=H5F_INVALID_FILE;
      } /* end if */

    if((flags&H5ACC_OVERWRITE)==0 && file_exists) {
       /* File already exists but overwrite permission was not given */
       HGOTO_ERROR(H5E_FILE, H5E_FILEEXISTS, FAIL);
    }

    /* OK to create/overwrite the file */
    f_handle=H5F_CREATE(filename);
    if(H5F_OPENERR(f_handle)) {
       /* Unable to create the file due to low level create failure */
       HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL);
    }

    /* Create the file node */
    if (NULL==(new_file=H5F_new()))
       HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL); /*H5F_new() failed*/

    /* Set the non-zero elements of the file structure */
    new_file->dir=HDgetcwd(NULL,0); /* get the directory we just created the file within */
    new_file->filename=HDstrdup(filename);  /* make a copy of the filename */
    new_file->acc_perm=H5ACC_WRITE;     /* all new files we create have write permission */
    new_file->file_handle=f_handle;     /* keep the file handle we just opened */
    new_file->ref_count=1;              /* only 1 fid handed out so far */
    new_file->consist_flags=0x03;       /* Set file-consistency flags: write-access and "file is consistent" */
    new_file->smallobj_off=0;           /* Set the offset of the small-object heap */
    new_file->freespace_off=0;          /* Set the offset of the free-space info */
    /* Get the file-creation template & record it */
    if(create_temp==0)
        create_temp=H5C_get_default_atom(H5_TEMPLATE);
    if((f_create_parms=H5Aatom_object(create_temp))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL); /*can't atomize template*/
    HDmemcpy(&new_file->file_create_parms,f_create_parms,sizeof(file_create_temp_t));

#ifdef LATER
    /* Get the file-access template & record it */
    if(access_temp==0)
        access_temp=H5CPget_default_atom(H5_TEMPLATE);
    if((f_access_parms=H5Aatom_object(access_temp))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL); /*can't unatomize template*/
    HDmemcpy(&new_file->file_access_parms,f_access_parms,sizeof(file_access_temp_t));
#endif /* LATER */

    /* Flush the file signature and boot block */
    if (H5F_flush (new_file, FALSE)<0) {
       HGOTO_ERROR (H5E_IO, H5E_CANTINIT, FAIL); /*can't write file boot block*/
    }

    /* Get an atom for the file */
    if((ret_value=H5Aregister_atom(H5_FILE, new_file))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL); /*can't atomize file*/

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

      /* Check if we left a dangling file handle */
      if(f_handle!=H5F_INVALID_FILE)
        H5F_CLOSE(f_handle);   /* close the file we opened */

      /* Check if we left a dangling file struct */
      if (new_file)
          H5F_dest (new_file);
    }

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Fcreate() */


/*--------------------------------------------------------------------------
 NAME
    H5Fopen

 PURPOSE
    Open an existing HDF5 file.

 USAGE
    hatom_t H5Fopen(filename, flags, access_temp)
        const char *filename;   IN: Name of the file to create
        uintn flags;            IN: Flags to indicate various options.
        hatom_t access_temp;    IN: File-access template

 ERRORS
    ARGS      BADRANGE      Invalid file name. 
    ATOM      BADATOM       Can't atomize template. 
    ATOM      BADATOM       Can't unatomize template. 
    ATOM      CANTREGISTER  Can't atomize file. 
    ATOM      CANTREGISTER  Can't register new_file atom. 
    FILE      CANTOPEN      File doesn't exist. 
    FILE      NOTHDF5       Not an HDF5 file. 
    FILE      NOTHDF5       The file exists but doesn't appear to be an
                            HDF5 file. 
    IO        READERROR     Can't decode root symbol table entry. 
    IO        READERROR     Read boot block failed. 
    IO        READERROR     Read boot block signature failed. 
    IO        READERROR     Seek to boot block failed. 
    IO        SEEKERROR     Seek failed. 
    RESOURCE  NOSPACE       H5F_new() failed. 

 RETURNS
    Returns file ID on success, FAIL on failure

 DESCRIPTION
        This is the primary function for accessing existing HDF5 files. The
    flags parameter determines whether writing to an existing file will be allowed
    or not.  All flags may be combined with the "||" (logical OR operator) to
    change the behavior of the file open call.
        The flags currently defined:
            H5ACC_WRITE - Allow writing to the file.
        The more complex behaviors of a file's access are controlled through
    the file-access template.

 MODIFICATIONS:
    Robb Matzke, 18 Jul 1997
    File struct creation and destruction is through H5F_new() H5F_dest().
    Reading the root symbol table entry is done with H5G_decode().
--------------------------------------------------------------------------*/
hatom_t H5Fopen(const char *filename, uintn flags, hatom_t access_temp)
{
    hdf5_file_t *new_file=NULL;     /* file struct for new file */
    hdf_file_t f_handle=H5F_INVALID_FILE;  /* file handle */
    hatom_t create_temp;            /* file-creation template ID */
    const file_create_temp_t *f_create_parms;    /* pointer to the parameters to use when creating the file */
    uint8 temp_buf[2048], *p;       /* temporary buffer for encoding header */
    haddr_t curr_off=0;          /* The current offset to check in the file */
    size_t file_len=0;          /* The length of the file we are checking */
    hatom_t ret_value = FAIL;
    size_t	variable_size;	/*size of the variable part of the bb */

    FUNC_ENTER(H5Fopen, H5F_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(filename==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);/*invalid file name*/

    /* See if this file is already open */
    new_file=H5Asearch_atom(H5_FILE,H5F_compare_filename,(const VOIDP)filename);

    /* If the file is already open, check the access permissions and go ahead with it */
    if(new_file!=NULL && new_file->acc_perm==flags)
      {
        /* Get an atom for the file */
        new_file->ref_count++;  /* increment the reference count for the file */
        if((ret_value=H5Aregister_atom(H5_FILE, new_file))==FAIL) {
	   /* Can't register new_file atom */
	   HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);
	}
        HGOTO_DONE(ret_value);
      } /* end if */

    /* 
     * If the file exists but has different permissions or if it's a new file,
     * start a new file handle for it, etc.
     */
    if(H5Fis_hdf5(filename)==BFALSE) {
       /* The file exists but doesn't appear to be an HDF5 file */
       HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, FAIL);
    }

    /* Check if the file already exists */
    f_handle=H5F_OPEN(filename,flags);
    if(H5F_OPENERR(f_handle))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL);/*file doesn't exist*/

    /* Create the file node */
    if (NULL==(new_file=H5F_new()))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);/*H5F_new() failed*/

    /* Set the non-zero elements of the file structure */
    new_file->dir=HDgetcwd(NULL,0); /* get the directory we just created the file within */
    new_file->filename=HDstrdup(filename);  /* make a copy of the filename */
    new_file->acc_perm=flags;       /* set the access permissions */
    new_file->file_handle=f_handle;     /* keep the file handle we just opened */
    new_file->ref_count=1;              /* only 1 fid handed out so far */
    create_temp=H5C_get_default_atom(H5_TEMPLATE);
    if((f_create_parms=H5Aatom_object(create_temp))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);/*can't unatomize template*/
    HDmemcpy(&new_file->file_create_parms,f_create_parms,sizeof(file_create_temp_t));

#ifdef LATER
    if(access_temp<=0)
        access_temp=H5CPget_default_atom(H5_TEMPLATE);
    if((f_access_parms=H5Aatom_object(access_temp))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);/*can't atomize template*/
    HDmemcpy(&new_file->file_access_parms,f_access_parms,sizeof(file_access_temp_t));
#endif /* LATER */

    /* Read the basic skeleton of the file */

    /* Seek to the correct offset to read in the file signature & boot-block */
    /* Get the length of the file */
    if(H5F_SEEKEND(new_file->file_handle)==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, BFAIL);/*seek failed*/
    file_len=H5F_TELL(new_file->file_handle);

    /* Check the offsets where the file signature is possible */
    while(curr_off<file_len)
      {
	if(H5F_SEEK(new_file->file_handle,curr_off)==FAIL) {
	    /*seek to boot block failed*/
            HGOTO_ERROR(H5E_IO, H5E_READERROR, BFAIL);
	}
        if(H5F_READ(new_file->file_handle,temp_buf, H5F_SIGNATURE_LEN)==FAIL) {
	    /*read boot block signature failed*/
            HGOTO_ERROR(H5E_IO, H5E_READERROR, BFAIL);
	}
        if(HDmemcmp(temp_buf,H5F_SIGNATURE,H5F_SIGNATURE_LEN)==0)
          {
            new_file->file_create_parms.userblock_size=curr_off;
            break;
          } /* end if */
        if(curr_off==0)
            curr_off=512;
        else
            curr_off*=2;
      } /* end while */
    if(curr_off>file_len)   /* Why didn't H5Fis_hdf5 catch this? */
        HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, FAIL); /*not an HDF5 file*/
    
    /* Read in the fixed-size part of the boot-block */
    if(H5F_READ(new_file->file_handle,temp_buf,16)==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL); /*read boot block failed*/

    /* Decode fixed-size part of the boot block */
    p=temp_buf;
    new_file->file_create_parms.bootblock_ver=*p++;    /* Decode Boot-block version # */
    new_file->file_create_parms.smallobject_ver=*p++;  /* Decode Small-Object Heap version # */
    new_file->file_create_parms.freespace_ver=*p++;    /* Decode Free-Space Info version # */
    new_file->file_create_parms.objectdir_ver=*p++;    /* Decode Object Directory Format version # */
    new_file->file_create_parms.sharedheader_ver=*p++; /* Decode Shared-Header Info version # */
    new_file->file_create_parms.offset_size=*p++;   /* Decode the number of bytes for the offset */
    new_file->file_create_parms.length_size=*p++;   /* Decode the number of bytes for the length */
    p++;                         /* Decode the reserved byte :-) */
    UINT16DECODE (p, new_file->file_create_parms.sym_leaf_k); /*stab leaf 1/2 rank*/
    UINT16DECODE (p, new_file->file_create_parms.btree_k[H5B_SNODE_ID]); /*stab internal 1/2 rank*/
    UINT32DECODE(p,new_file->consist_flags);       /* Decode File-Consistancy flags */

    /* Read the variable-size part of the boot-block */
    variable_size = H5F_SIZEOF_OFFSET(new_file) +	/*offset of global small-object heap*/
		    H5F_SIZEOF_OFFSET(new_file) +	/*offset of global free list*/
		    H5F_SIZEOF_SIZE(new_file) +		/*logical size of HDF5 file*/
		    H5G_SIZEOF_ENTRY(new_file);		/*root symbol table entry*/
    if (H5F_READ(new_file->file_handle, temp_buf, variable_size)<0)
       HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL); /*read boot block failed*/

    /* Decode the variable-size part of the boot block */
    p = temp_buf;
    H5F_decode_offset(new_file,p,new_file->smallobj_off);  /* Decode offset of global small-object heap */
    H5F_decode_offset(new_file,p,new_file->freespace_off);  /* Decode offset of global free-space heap */
    H5F_decode_length(new_file,p,new_file->logical_len); /* Decode logical length of file */

    /* Decode the root symbol table entry */
    if (H5G_ent_decode (new_file, &p, new_file->root_sym)<0) {
       /*can't decode root symbol table entry */
       HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL);
    }

    /* Get an atom for the file */
    if((ret_value=H5Aregister_atom(H5_FILE, new_file))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);/*can't atomize file*/

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

      /* Check if we left a dangling file handle */
      if(f_handle!=H5F_INVALID_FILE)
        H5F_CLOSE(f_handle);   /* close the file we opened */

      /* Check if we left a dangling file struct */
      if(new_file) HDfree(new_file);
    }

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Fopen() */

/*--------------------------------------------------------------------------
 NAME
    H5Fflush

 PURPOSE
    Flush all cached data to disk and optionally invalidates all cached
    data.

 USAGE
    herr_t H5Fclose(fid, invalidate)
        hatom_t fid;      	IN: File ID of file to close.
        hbool_t invalidate;	IN: Invalidate all of the cache?

 ERRORS
    ARGS      BADTYPE       Not a file atom. 
    ATOM      BADATOM       Can't get file struct. 
    CACHE     CANTFLUSH     Flush failed. 

 RETURNS
    SUCCEED/FAIL

 DESCRIPTION
        This function flushes all cached data to disk and, if INVALIDATE
    is non-zero, removes cached objects from the cache so they must be
    re-read from the file on the next access to the object.

 MODIFICATIONS:
--------------------------------------------------------------------------*/
herr_t
H5Fflush (hatom_t fid, hbool_t invalidate)
{
   hdf5_file_t	*file = NULL;

   FUNC_ENTER (H5Fflush, H5F_init_interface, FAIL);
   H5ECLEAR;

   /* check arguments */
   if (H5_FILE!=H5Aatom_group (fid)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL); /*not a file atom*/
   }
   if (NULL==(file=H5Aatom_object (fid))) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL); /*can't get file struct*/
   }

   /* do work */
   if (H5F_flush (file, invalidate)<0) {
      HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL); /*flush failed*/
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_flush
 *
 * Purpose:	Flushes (and optionally invalidates) cached data plus the
 *		file boot block.  If the logical file size field is zero
 *		then it is updated to be the length of the boot block.
 *
 * Errors:
 *		CACHE     CANTFLUSH     Can't flush cache. 
 *		IO        WRITEERROR    Can't write header. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *				-2 if the there are open objects and
 * 				INVALIDATE was non-zero.
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 29 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_flush (hdf5_file_t *f, hbool_t invalidate)
{
   uint8	buf[2048], *p=buf;
   herr_t	shadow_flush;
   
   FUNC_ENTER (H5F_flush, H5F_init_interface, FAIL);

   /* nothing to do if the file is read only */
   if (0==(H5ACC_WRITE & f->acc_perm)) HRETURN (SUCCEED);

   /*
    * Flush all open object info. If this fails just remember it and return
    * failure at the end.  At least that way we get a consistent file.
    */
   shadow_flush = H5G_shadow_flush (f,  invalidate);
      
   /* flush (and invalidate) the entire cache */
   if (H5AC_flush (f, NULL, 0, invalidate)<0) {
      HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL); /*can't flush cache*/
   }

   /* encode the file boot block */
   HDmemcpy (p, H5F_SIGNATURE, H5F_SIGNATURE_LEN);
   p += H5F_SIGNATURE_LEN;
   
   *p++ = f->file_create_parms.bootblock_ver;
   *p++ = f->file_create_parms.smallobject_ver;
   *p++ = f->file_create_parms.freespace_ver;
   *p++ = f->file_create_parms.objectdir_ver;
   *p++ = f->file_create_parms.sharedheader_ver;
   *p++ = H5F_SIZEOF_OFFSET (f);
   *p++ = H5F_SIZEOF_SIZE (f);
   *p++ = 0; /*reserved*/
   UINT16ENCODE (p, f->file_create_parms.sym_leaf_k);
   UINT16ENCODE (p, f->file_create_parms.btree_k[H5B_SNODE_ID]);
   UINT32ENCODE (p, f->consist_flags);
   H5F_encode_offset (f, p, f->smallobj_off);
   H5F_encode_offset (f, p, f->freespace_off);
   H5F_encode_length (f, p, f->logical_len);
   H5G_ent_encode (f, &p, f->root_sym);

   /* write the boot block to disk */
   if (H5F_block_write (f, 0, p-buf, buf)<0) {
      HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL); /*can't write header*/
   }

   /* update file length if necessary */
   if (f->logical_len<=0) f->logical_len = p-buf;

   /* Did shadow flush fail above? */
   if (shadow_flush<0) {
      HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, -2);/*object are still open*/
   }
   
   FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5Fclose

 PURPOSE
    Close an open HDF5 file.

 USAGE
    int32 H5Fclose(fid)
        int32 fid;      IN: File ID of file to close

 ERRORS
    ARGS      BADTYPE       Not a file atom. 
    ATOM      BADATOM       Can't remove atom. 
    ATOM      BADATOM       Can't unatomize file. 
    CACHE     CANTFLUSH     Can't flush cache. 

 RETURNS
    SUCCEED/FAIL

 DESCRIPTION
        This function terminates access to an HDF5 file.  If this is the last
    file ID open for a file and if access IDs are still in use, this function
    will fail.

 MODIFICATIONS:
    Robb Matzke, 18 Jul 1997
    File struct destruction is through H5F_dest().

    Robb Matzke, 29 Aug 1997
    The file boot block is flushed to disk since it's contents may have
    changed.
--------------------------------------------------------------------------*/
herr_t H5Fclose(hatom_t fid)
{
    hdf5_file_t *file=NULL;         /* file struct for file to close */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Fclose, H5F_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(H5Aatom_group(fid)!=H5_FILE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);/*not a file atom*/

    /* Get the file handle to close */
    if((file=H5Aatom_object(fid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);/*can't unatomize file*/

    /* Decrement the ref. count and recycle the file structure */
    if((--file->ref_count)==0)
      {
        if(file->file_handle!=H5F_INVALID_FILE) {
	   if (-2==(ret_value=H5F_flush (file, TRUE))) {
	      /*objects are still open*/
	   } else if (ret_value<0) {
	      /*can't flush cache*/
	      HGOTO_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL);
	   }
           H5F_CLOSE(file->file_handle);
        }
        H5F_dest (file);
        if(H5Aremove_atom(fid)==NULL) {
           HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);/*can't remove atom*/
        }
      } /* end if */

      /* Did the H5F_flush() fail because of open objects? */
      if (ret_value<0) {
	 HGOTO_ERROR (H5E_SYM, H5E_CANTFLUSH, FAIL);
      }
	
done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
} /* end H5Fclose() */


/*-------------------------------------------------------------------------
 * Function:	H5F_block_read
 *
 * Purpose:	Reads some data from a file/server/etc into a buffer.
 *		The data is contiguous.
 *
 * Errors:
 *		IO        READERROR     Low-level read failure. 
 *		IO        SEEKERROR     Low-level seek failure. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_read (hdf5_file_t *f, haddr_t addr, size_t size, void *buf)
{
   FUNC_ENTER (H5F_block_read, H5F_init_interface, FAIL);

   if (0==size) return 0;
   addr += f->file_create_parms.userblock_size;
   
   if (H5F_SEEK (f->file_handle, addr)<0) {
      /* low-level seek failure */
      HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL);
   }
   if (H5F_READ (f->file_handle, buf, size)<0) {
      /* low-level read failure */
      HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_block_write
 *
 * Purpose:	Writes some data from memory to a file/server/etc.  The
 *		data is contiguous.
 *
 * Errors:
 *		IO        SEEKERROR     Low-level seek failure. 
 *		IO        WRITEERROR    Low-level write failure. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_write (hdf5_file_t *f, haddr_t addr, size_t size, void *buf)
{
   FUNC_ENTER (H5F_block_write, H5F_init_interface, FAIL);

   if (0==size) return 0;
   addr += f->file_create_parms.userblock_size;

   if (H5F_SEEK (f->file_handle, addr)<0) {
      /* low-level seek failure */
      HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL);
   }
   if (H5F_WRITE (f->file_handle, buf, size)<0) {
      /* low-level write failure */
      HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_debug
 *
 * Purpose:	Prints a file header to the specified stream.  Each line
 *		is indented and the field name occupies the specified width
 *		number of characters.
 *
 * Errors:
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_debug (hdf5_file_t *f, haddr_t addr, FILE *stream, intn indent,
	   intn fwidth)
{
   FUNC_ENTER (H5F_debug, H5F_init_interface, FAIL);

   /* check args */
   assert (f);
   assert (addr>=0);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   /* debug */
   fprintf (stream, "%*sFile Boot Block...\n", indent, "");
   
   fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	    "Directory:",
	    f->dir);
   fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	    "File name:",
	    f->filename);
   fprintf (stream, "%*s%-*s 0x%08x\n", indent, "", fwidth,
	    "Permissions",
	    (unsigned)(f->acc_perm));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Reference count:",
	    (unsigned)(f->ref_count));
   fprintf (stream, "%*s%-*s 0x%08lx\n", indent, "", fwidth,
	    "Consistency flags:",
	    (unsigned long)(f->consist_flags));
   fprintf (stream, "%*s%-*s %ld\n", indent, "", fwidth,
	    "Small object heap address:",
	    (long)(f->smallobj_off));
   fprintf (stream, "%*s%-*s %ld\n", indent, "", fwidth,
	    "Free list address:",
	    (long)(f->freespace_off));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Logical file length:",
	    (unsigned long)(f->logical_len));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Size of user block:",
	    (unsigned long)(f->file_create_parms.userblock_size));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Size of file size_t type:",
	    (unsigned)(f->file_create_parms.offset_size));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Size of file off_t type:",
	    (unsigned)(f->file_create_parms.length_size));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Symbol table leaf node 1/2 rank:",
	    (unsigned)(f->file_create_parms.sym_leaf_k));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Symbol table internal node 1/2 rank:",
	    (unsigned)(f->file_create_parms.btree_k[H5B_SNODE_ID]));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Boot block version number:",
	    (unsigned)(f->file_create_parms.bootblock_ver));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Small object heap version number:",
	    (unsigned)(f->file_create_parms.smallobject_ver));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Free list version number:",
	    (unsigned)(f->file_create_parms.freespace_ver));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Object directory version number:",
	    (unsigned)(f->file_create_parms.objectdir_ver));
   fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Shared header version number:",
	    (unsigned)(f->file_create_parms.sharedheader_ver));

   fprintf (stream, "%*sRoot symbol table entry:\n", indent, "");
   H5G_ent_debug (f, f->root_sym, stream, indent+3, MAX(0, fwidth-3));
	    
   FUNC_LEAVE (SUCCEED);
}
