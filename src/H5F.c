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

#define HDF5_FILE_MASTER
#include "hdf5.h"
#undef HDF5_FILE_MASTER

/* Packages needed by this file... */
#include "H5private.h"      	/*library functions			*/
#include "H5ACprivate.h"	/*cache					*/
#include "H5Gprivate.h"		/*symbol tables				*/
#include "H5MMprivate.h"	/*core memory management		*/

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize = FALSE;

/*--------------------- Local function prototypes ----------------------------*/
static herr_t H5F_init_interface(void);
static hdf5_file_t *H5F_new (void);
static hdf5_file_t *H5F_dest (hdf5_file_t *f);

/*--------------------------------------------------------------------------
NAME
   H5F_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5F_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t H5F_init_interface(void)
{
#ifdef LATER
    CONSTR(FUNC, "H5F_init_interface");	/* For HERROR */
#endif /* LATER */
    herr_t ret_value = SUCCEED;

    /* Don't use "FUNC_ENTER" macro, to avoid potential infinite recursion */
    PABLO_TRACE_ON(H5F_mask, ID_H5F_init_interface);

    /* Don't call this routine again... */
    interface_initialize = TRUE;

    /* Initialize the atom group for the file IDs */
    ret_value=H5Ainit_group(H5_FILE,HDF5_FILEID_HASHSIZE,0);

    FUNC_LEAVE(H5_mask, ID_H5F_init_interface, ret_value);
}	/* H5F_init_interface */

#ifdef LATER
/*--------------------------------------------------------------------------
 NAME
       H5F_encode_length_unusual -- encode an unusual length size
 USAGE
       void H5F_encode_length_unusual(f, p, l)
       const hdf5_file_t *f;             IN: pointer to the file record
       uint8 **p;               IN: pointer to buffer pointer to encode length in
       uint8 *l;                IN: pointer to length to encode
 RETURNS
    none
 DESCRIPTION
    Encode non-standard (i.e. not 2, 4 or 8-byte) lengths in file meta-data.
--------------------------------------------------------------------------*/
void H5F_encode_length_unusual(const hdf5_file_t *f, uint8 **p, uint8 *l)
{
#ifdef LATER
    CONSTR(FUNC, "H5F_encode_length_unusual");
#endif /* LATER */
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
 RETURNS
    none
 DESCRIPTION
    Encode non-standard (i.e. not 2, 4 or 8-byte) offsets in file meta-data.
--------------------------------------------------------------------------*/
void H5F_encode_offset_unusual(const hdf5_file_t *f, uint8 **p, uint8 *o)
{
#ifdef LATER
    CONSTR(FUNC, "H5F_encode_offset_unusual");
#endif /* LATER */
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
 RETURNS
       TRUE if the key matches the obj, FALSE otherwise
 DESCRIPTION
       Look inside the file record for the atom API and compare the the
       filenames.
--------------------------------------------------------------------------*/
intn H5F_compare_filename(const VOIDP obj, const VOIDP key)
{
#ifdef LATER
    CONSTR(FUNC, "H5F_compare_filename");
#endif /* LATER */


#ifdef LATER
done:
    if(ret_value == FALSE)   
      { /* Error condition cleanup */

      } /* end if */
#endif /* LATER */

    /* Normal function cleanup */

    return(!HDstrcmp(((const hdf5_file_t *)obj)->filename,(const char *)key));
}	/* H5F_compare_filename */

/*--------------------------------------------------------------------------
 NAME
    H5Fget_create_template
 PURPOSE
    Get an atom for a copy of the file-creation template for this file
 USAGE
    hatom_t H5Fget_create_template(fid)
        hatom_t fid;    IN: File ID
 RETURNS
    Returns template ID on success, FAIL on failure
 DESCRIPTION
        This function returns an atom with a copy of the template parameters
    used to create a file.
--------------------------------------------------------------------------*/
hatom_t H5Fget_create_template(hatom_t fid)
{
    CONSTR(FUNC, "H5Fget_create_template");      /* for HERROR */
    hdf5_file_t *file=NULL;         /* file struct for file to close */
    hatom_t ret_value = FAIL;

    FUNC_ENTER(H5F_mask, ID_H5Fget_create_template, H5F_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the file structure */
    if((file=H5Aatom_object(fid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Create the template object to return */
    if((ret_value=H5Mcreate(fid,H5_TEMPLATE,NULL))==FAIL)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTCREATE, FAIL);

    if(H5C_init(ret_value,&(file->file_create_parms))==FAIL)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(H5F_mask, ID_H5Fget_create_template, ret_value);
} /* end H5Fget_create_template() */

/*--------------------------------------------------------------------------
 NAME
    H5Fis_hdf5
 PURPOSE
    Check the file signature to detect an HDF5 file.
 USAGE
    hbool_t H5Fis_hdf5(filename)
        const char *filename;   IN: Name of the file to check
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
    This function determines if a file is an HDF5 format file.
--------------------------------------------------------------------------*/
hbool_t H5Fis_hdf5(const char *filename)
{
    CONSTR(FUNC, "H5Fis_hdf5");        /* for HERROR */
    hdf_file_t f_handle=H5F_INVALID_FILE;      /* file handle */
    uint8 temp_buf[HDF5_FILE_SIGNATURE_LEN];    /* temporary buffer for checking file signature */
    haddr_t curr_off=0;          /* The current offset to check in the file */
    size_t file_len=0;          /* The length of the file we are checking */
    hbool_t ret_value = BFALSE;

    FUNC_ENTER(H5F_mask, ID_H5Fis_hdf5, H5F_init_interface, BFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(filename==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, BFAIL);

    /* Open the file */
    f_handle=H5F_OPEN(filename,0);
    if(H5F_OPENERR(f_handle))
        HGOTO_ERROR(H5E_FILE, H5E_BADFILE, BFAIL);

    /* Get the length of the file */
    if(H5F_SEEKEND(f_handle)==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, BFAIL);
    file_len=H5F_TELL(f_handle);

    /* Check the offsets where the file signature is possible */
    while(curr_off<file_len)
      {
        if(H5F_SEEK(f_handle,curr_off)==FAIL)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, BFAIL);
        if(H5F_READ(f_handle,temp_buf, HDF5_FILE_SIGNATURE_LEN)==FAIL)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, BFAIL);
        if(HDmemcmp(temp_buf,HDF5_FILE_SIGNATURE,HDF5_FILE_SIGNATURE_LEN)==0)
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

    FUNC_LEAVE(H5F_mask, ID_H5Fis_hdf5, ret_value);
} /* end H5Fis_hdf5() */


/*-------------------------------------------------------------------------
 * Function:	H5F_new
 *
 * Purpose:	Creates a new file object and initializes it.  The
 *		H5Fopen and H5Fcreate functions then fill in various
 *		fields.
 *
 * Return:	Success:	Ptr to a new file struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
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

   /* Create an error stack for errors with this file */
   f->file_err = H5Enew_err_stack(HDF5_FILE_ERRSTACK);

   /* Create a cache */
   H5AC_new (f);

   /* Create a root symbol slot */
   f->root_sym = H5MM_xcalloc (1, sizeof (H5G_entry_t));
   f->root_sym->type = H5G_NOTHING_CACHED;
   
   return f;
}


/*-------------------------------------------------------------------------
 * Function:	H5F_dest
 *
 * Purpose:	Destroys a file structure.  This function does not flush
 *		the cache or anything else; it only frees memory associated
 *		with the file struct.
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
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
      if(f->file_err!=0) H5Edelete_err_stack (f->file_err);
      f->dir = H5MM_xfree (f->dir);
      f->filename = H5MM_xfree (f->filename);
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
    int32 H5Fcreate(filename, flags)
        const char *filename;   IN: Name of the file to create
        uintn flags;            IN: Flags to indicate various options.
        hatom_t create_temp;    IN: File-creation template
        hatom_t access_temp;    IN: File-access template
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
    controlled through the file-creation and file-access templates.

 MODIFICATIONS:
    Robb Matzke, 18 Jul 1997
    File struct creation and destruction is through H5F_new() H5F_dest().
    Writing the root symbol table entry is done with H5G_encode().
--------------------------------------------------------------------------*/
hatom_t H5Fcreate(const char *filename, uintn flags, hatom_t create_temp, hatom_t access_temp)
{
    CONSTR(FUNC, "H5Fcreate");      /* for HERROR */
    hdf5_file_t *new_file=NULL;     /* file struct for new file */
    hdf_file_t f_handle=H5F_INVALID_FILE;  /* file handle */
    const file_create_temp_t *f_create_parms;    /* pointer to the parameters to use when creating the file */
    uint8 temp_buf[2048], *p;       /* temporary buffer for encoding header */
    intn file_exists=0;             /* flag to indicate that file exists already */
    hatom_t ret_value = FAIL;

    FUNC_ENTER(H5F_mask, ID_H5Fcreate, H5F_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(filename==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    /* See if this file is already open */
    if(H5Asearch_atom(H5_FILE,H5F_compare_filename,(const VOIDP)filename)!=NULL)
        HGOTO_ERROR(H5E_FILE, H5E_FILEOPEN, FAIL);

    /* Check if the file already exists */
    f_handle=H5F_OPEN(filename,0);
    if(!H5F_OPENERR(f_handle))
      {
        file_exists=1;  /* set the flag to indicate that the file already exists */
        H5F_CLOSE(f_handle);   /* close the file we opened */
        f_handle=H5F_INVALID_FILE;
      } /* end if */

    /* throw an error if the file exists and we aren't allowed to overwrite it */
    if((flags&H5ACC_OVERWRITE)==0 && file_exists) 
        HGOTO_ERROR(H5E_FILE, H5E_FILEEXISTS, FAIL);

    /* OK to create/overwrite the file */
    f_handle=H5F_CREATE(filename);
    if(H5F_OPENERR(f_handle))
        HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL);

    /* Create the file node */
    if (NULL==(new_file=H5F_new()))
       HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);

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
    if(create_temp<=0)
        create_temp=H5C_get_default_atom(H5_TEMPLATE);
    if((f_create_parms=H5Aatom_object(create_temp))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    HDmemcpy(&new_file->file_create_parms,f_create_parms,sizeof(file_create_temp_t));

#ifdef LATER
    /* Get the file-access template & record it */
    if(access_temp<=0)
        access_temp=H5CPget_default_atom(H5_TEMPLATE);
    if((f_access_parms=H5Aatom_object(access_temp))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    HDmemcpy(&new_file->file_access_parms,f_access_parms,sizeof(file_access_temp_t));
#endif /* LATER */

    /* Create the basic skeleton of the file */

    /* Seek to the correct offset to write out the file signature & boot-block */
    if(new_file->file_create_parms.userblock_size>0)
        if(H5F_SEEK(new_file->file_handle,new_file->file_create_parms.userblock_size)==FAIL)
            HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL);
    
    /* Write out the file-signature */
    if(H5F_WRITE(new_file->file_handle,HDF5_FILE_SIGNATURE,HDF5_FILE_SIGNATURE_LEN)==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL);

    /* Encode the boot block */
    p=temp_buf;
    *p++=f_create_parms->bootblock_ver;    /* Encode Boot-block version # */
    *p++=f_create_parms->smallobject_ver;  /* Encode Small-Object Heap version # */
    *p++=f_create_parms->freespace_ver;    /* Encode Free-Space Info version # */
    *p++=f_create_parms->objectdir_ver;    /* Encode Object Directory Format version # */
    *p++=f_create_parms->sharedheader_ver; /* Encode Shared-Header Info version # */
    *p++=(uint8)f_create_parms->offset_size; /* Encode the number of bytes for the offset */
    *p++=(uint8)f_create_parms->length_size; /* Encode the number of bytes for the length */
    *p++=0;                         /* Encode the reserved byte :-) */
    UINT32ENCODE(p,f_create_parms->btree_page_size);    /* Encode the B-Tree page size */
    UINT32ENCODE(p,new_file->consist_flags);       /* Encode File-Consistancy flags */
    H5F_encode_offset(new_file,p,new_file->smallobj_off);  /* Encode offset of global small-object heap */
    H5F_encode_offset(new_file,p,new_file->freespace_off);  /* Encode offset of global free-space heap */
    /* Predict the header length and encode it: */
    H5F_encode_length(new_file,p,(p-temp_buf)+f_create_parms->length_size+H5F_symbol_table_size(new_file)); /* Encode length of boot-block */
    
    /* Encode the (bogus) symbol-table entry */
    if (H5G_encode (new_file, &p, new_file->root_sym)<0) {
       HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL);
    }

    /* Write out the boot block */
    if(H5F_WRITE(new_file->file_handle,temp_buf,(size_t)(p-temp_buf))==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL);
    new_file->logical_len = p - temp_buf;
    

    /* Get an atom for the file */
    if((ret_value=H5Aregister_atom(H5_FILE, new_file))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

      /* Check if we left a dangling file handle */
      if(f_handle!=H5F_INVALID_FILE)
        H5F_CLOSE(f_handle);   /* close the file we opened */

      /* Check if we left a dangling file struct */
      if (new_file) H5F_dest (new_file);
    }

    /* Normal function cleanup */

    FUNC_LEAVE(H5F_mask, ID_H5Fcreate, ret_value);
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
    CONSTR(FUNC, "H5Fopen");        /* for HERROR */
    hdf5_file_t *new_file=NULL;     /* file struct for new file */
    hdf_file_t f_handle=H5F_INVALID_FILE;  /* file handle */
    hatom_t create_temp;            /* file-creation template ID */
    const file_create_temp_t *f_create_parms;    /* pointer to the parameters to use when creating the file */
    uint8 temp_buf[2048], *p;       /* temporary buffer for encoding header */
    haddr_t curr_off=0;          /* The current offset to check in the file */
    size_t file_len=0;          /* The length of the file we are checking */
    hatom_t ret_value = FAIL;

    FUNC_ENTER(H5F_mask, ID_H5Fopen, H5F_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(filename==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    /* See if this file is already open */
    new_file=H5Asearch_atom(H5_FILE,H5F_compare_filename,(const VOIDP)filename);

    /* If the file is already open, check the access permissions and go ahead with it */
    if(new_file!=NULL && new_file->acc_perm==flags)
      {
        /* Get an atom for the file */
        new_file->ref_count++;  /* increment the reference count for the file */
        if((ret_value=H5Aregister_atom(H5_FILE, new_file))==FAIL)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);
        HGOTO_DONE(ret_value);
      } /* end if */

    /* 
     * If the file exists but has different permissions or if it's a new file,
     * start a new file handle for it, etc.
     */
    if(H5Fis_hdf5(filename)==BFALSE)
        HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, FAIL);

    /* Check if the file already exists */
    f_handle=H5F_OPEN(filename,flags);
    if(H5F_OPENERR(f_handle))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPEN, FAIL);

    /* Create the file node */
    if (NULL==(new_file=H5F_new()))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);

    /* Set the non-zero elements of the file structure */
    new_file->dir=HDgetcwd(NULL,0); /* get the directory we just created the file within */
    new_file->filename=HDstrdup(filename);  /* make a copy of the filename */
    new_file->acc_perm=flags;       /* set the access permissions */
    new_file->file_handle=f_handle;     /* keep the file handle we just opened */
    new_file->ref_count=1;              /* only 1 fid handed out so far */
    create_temp=H5C_get_default_atom(H5_TEMPLATE);
    if((f_create_parms=H5Aatom_object(create_temp))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    HDmemcpy(&new_file->file_create_parms,f_create_parms,sizeof(file_create_temp_t));

#ifdef LATER
    if(access_temp<=0)
        access_temp=H5CPget_default_atom(H5_TEMPLATE);
    if((f_access_parms=H5Aatom_object(access_temp))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    HDmemcpy(&new_file->file_access_parms,f_access_parms,sizeof(file_access_temp_t));
#endif /* LATER */

    /* Read the basic skeleton of the file */

    /* Seek to the correct offset to read in the file signature & boot-block */
    /* Get the length of the file */
    if(H5F_SEEKEND(new_file->file_handle)==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, BFAIL);
    file_len=H5F_TELL(new_file->file_handle);

    /* Check the offsets where the file signature is possible */
    while(curr_off<file_len)
      {
        if(H5F_SEEK(new_file->file_handle,curr_off)==FAIL)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, BFAIL);
        if(H5F_READ(new_file->file_handle,temp_buf, HDF5_FILE_SIGNATURE_LEN)==FAIL)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, BFAIL);
        if(HDmemcmp(temp_buf,HDF5_FILE_SIGNATURE,HDF5_FILE_SIGNATURE_LEN)==0)
          {
            new_file->file_create_parms.userblock_size=curr_off;
            break;
          } /* end if */
        if(curr_off==0)
            curr_off=512;
        else
            curr_off*=2;
      } /* end while */
    if(curr_off>file_len)
        HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, FAIL);
    
    /* Read in the fixed-size part of the boot-block */
    if(H5F_READ(new_file->file_handle,temp_buf,16)==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL);

    /* Decode the boot block */
    p=temp_buf;
    new_file->file_create_parms.bootblock_ver=*p++;    /* Decode Boot-block version # */
    new_file->file_create_parms.smallobject_ver=*p++;  /* Decode Small-Object Heap version # */
    new_file->file_create_parms.freespace_ver=*p++;    /* Decode Free-Space Info version # */
    new_file->file_create_parms.objectdir_ver=*p++;    /* Decode Object Directory Format version # */
    new_file->file_create_parms.sharedheader_ver=*p++; /* Decode Shared-Header Info version # */
    new_file->file_create_parms.offset_size=*p++;   /* Decode the number of bytes for the offset */
    new_file->file_create_parms.length_size=*p++;   /* Decode the number of bytes for the length */
    p++;                         /* Decode the reserved byte :-) */
    UINT32DECODE(p,new_file->file_create_parms.btree_page_size);    /* Decode the B-Tree page size */
    UINT32DECODE(p,new_file->consist_flags);       /* Decode File-Consistancy flags */
    H5F_decode_offset(new_file,p,new_file->smallobj_off);  /* Decode offset of global small-object heap */
    H5F_decode_offset(new_file,p,new_file->freespace_off);  /* Decode offset of global free-space heap */
    H5F_decode_length(new_file,p,new_file->logical_len); /* Decode logical length of file */

    /* Decode the root symbol table entry */
    if (H5G_decode (new_file, &p, new_file->root_sym)<0) {
       HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL);
    }

    /* Get an atom for the file */
    if((ret_value=H5Aregister_atom(H5_FILE, new_file))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

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

    FUNC_LEAVE(H5F_mask, ID_H5Fopen, ret_value);
} /* end H5Fopen() */

/*--------------------------------------------------------------------------
 NAME
    H5Fclose
 PURPOSE
    Close an open HDF5 file.
 USAGE
    int32 H5Fclose(fid)
        int32 fid;      IN: File ID of file to close
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function terminates access to an HDF5 file.  If this is the last
    file ID open for a file and if access IDs are still in use, this function
    will fail.

 MODIFICATIONS:
    Robb Matzke, 18 Jul 1997
    File struct destruction is through H5F_dest().
--------------------------------------------------------------------------*/
herr_t H5Fclose(hatom_t fid)
{
    CONSTR(FUNC, "H5Fclose");       /* for HERROR */
    hdf5_file_t *file=NULL;         /* file struct for file to close */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5F_mask, ID_H5Fclose, H5F_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(H5Aatom_group(fid)!=H5_FILE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);

    /* Get the file handle to close */
    if((file=H5Aatom_object(fid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Decrement the ref. count and recycle the file structure */
    if((--file->ref_count)==0)
      {
        H5AC_flush (file, NULL, 0, TRUE);
        if(file->file_handle!=H5F_INVALID_FILE) {
	   H5F_CLOSE(file->file_handle);
	}
	H5F_dest (file);
        if(H5Aremove_atom(fid)==NULL) {
	   HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
	}
      } /* end if */

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */
    FUNC_LEAVE(H5F_mask, ID_H5Fclose, ret_value);
} /* end H5Fclose() */


/*-------------------------------------------------------------------------
 * Function:	H5F_block_read
 *
 * Purpose:	Reads some data from a file/server/etc into a buffer.
 *		The data is contiguous.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul 10 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_read (hdf5_file_t *f, haddr_t addr, size_t size, void *buf)
{
    CONSTR(FUNC, "H5F_block_read");       /* for HERROR */

    PABLO_TRACE_ON(H5F_mask, ID_H5F_block_read);

    if (0==size) return 0;
    addr += f->file_create_parms.userblock_size;
   
    if (H5F_SEEK (f->file_handle, addr)<0)
        HRETURN_ERROR(H5E_IO, H5E_SEEKERROR, FAIL);
    if (H5F_READ (f->file_handle, buf, size)<0)
        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL);
    PABLO_TRACE_OFF(H5F_mask, ID_H5F_block_read);
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	H5F_block_write
 *
 * Purpose:	Writes some data from memory to a file/server/etc.  The
 *		data is contiguous.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul 10 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_write (hdf5_file_t *f, haddr_t addr, size_t size, void *buf)
{
    CONSTR(FUNC, "H5F_block_write");       /* for HERROR */
     
    PABLO_TRACE_ON(H5F_mask, ID_H5F_block_read);

    if (0==size) return 0;
    addr += f->file_create_parms.userblock_size;

    if (H5F_SEEK (f->file_handle, addr)<0)
        HRETURN_ERROR(H5E_IO, H5E_SEEKERROR, FAIL);
    if (H5F_WRITE (f->file_handle, buf, size)<0)
        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL);
    PABLO_TRACE_OFF(H5F_mask, ID_H5F_block_write);
    return SUCCEED;
}
