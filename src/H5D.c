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
/*+
   FILE
       H5D.c
   HDF5 Dataset routines

   EXPORTED ROUTINES
       H5Dcreate    -- Create a dataset
       H5Drelease   -- Release access to a dataset

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
       H5P_init_interface    -- initialize the interface
   + */

#include <H5private.h>  /* Generic Functions */
#include <H5Aprivate.h> /* Atoms */
#include <H5Mprivate.h> /* Meta-Object API */
#include <H5Dprivate.h> /* Dataset functions */
#include <H5Eprivate.h> /* Error handling */
#include <H5Mprivate.h> /* Meta data */
#include <H5Gprivate.h> /* Group headers */
#include <H5Oprivate.h> /* Object headers */
#include <H5MFprivate.h> /* File space allocation header */

#define PABLO_MASK	H5D_mask

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize_g = FALSE;
static herr_t H5D_init_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5D_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5D_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t H5D_init_interface(void)
{
    herr_t ret_value = SUCCEED;
    FUNC_ENTER (H5D_init_interface, NULL, FAIL);

    /* Initialize the atom group for the file IDs */
    ret_value=H5Ainit_group(H5_OID,H5A_OID_HASHSIZE,0);
    ret_value=H5Ainit_group(H5_DATASET,H5A_DATASETID_HASHSIZE,H5D_RESERVED_ATOMS);

    FUNC_LEAVE(ret_value);
}	/* H5D_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5D_create
 PURPOSE
    Create a new HDF5 dataset object
 USAGE
    hatom_t H5D_create(owner_id, type, name)
        hatom_t owner_id;       IN: Group/file which owns this object
        hobjtype_t type;        IN: Type of object to create
        const char *name;       IN: Name of the object
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function actually creates the dataset object.
--------------------------------------------------------------------------*/
hatom_t H5D_create(hatom_t owner_id, hobjtype_t type, const char *name)
{
    H5D_dataset_t *new_dset;        /* new dataset object to create */
    hatom_t ret_value = SUCCEED;

    FUNC_ENTER(H5D_create, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Allocate space for the new dataset */
    if((new_dset=HDmalloc(sizeof(H5D_dataset_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    
    /* Initialize the dataset object */
    if(H5Aatom_group(owner_id)!=H5_FILE)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);
    new_dset->file=H5Aatom_object(owner_id);
    if((new_dset->name=HDmalloc(sizeof(H5O_name_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    new_dset->name->s=HDstrdup(name);  /* make a copy of the dataset name */
    new_dset->modified=BTRUE;       /* Yep, we're new... */
    new_dset->type=NULL;
    new_dset->dim=NULL;
    new_dset->header=(-1);          /* don't know where we are... */
    new_dset->data=(-1);            /* don't know where we should put the data, either... */

    /* Register the new datatype and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_DATASET, (const VOIDP)new_dset))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5D_create() */

/*--------------------------------------------------------------------------
 NAME
    H5D_access
 PURPOSE
    Start access to an existing HDF5 dataset object
 USAGE
    hatom_t H5D_access(oid)
        hatom_t oid;       IN: Atom for OID of dataset
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function initiates access to a dataset object.
--------------------------------------------------------------------------*/
hatom_t H5D_access(hatom_t oid)
{
    H5D_dataset_t *dset;        /* dataset object to access */
    H5D_oid_t *ohdr;            /* OID for reference to dataset */
    H5O_std_store_t store;      /* standard storage info */
    hatom_t ret_value = SUCCEED;

    FUNC_ENTER(H5D_access, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Go get the OID for the dataset */
    if((ohdr=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Allocate space for the dataset */
    if((dset=HDmalloc(sizeof(H5D_dataset_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);

    /* Pull information about the dataset out of the file */
    dset->file=H5Aatom_object(ohdr->fid);   /* Get the pointer to the file for the dataset */
    /* Get the dataset's name */
    if((dset->name=H5O_read(dset->file,ohdr->ohdr,&ohdr->ent,H5O_NAME,0,NULL))==NULL)
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL);
    dset->modified=BFALSE;       /* nothing changed yet */
    /* Get the dataset's type (currently only atomic types) */
    if((dset->type=H5O_read(dset->file,ohdr->ohdr,&ohdr->ent,H5O_SIM_DTYPE,0,NULL))==NULL)
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL);
    /* Get the dataset's dimensionality (currently only simple dataspaces) */
    if((dset->dim=HDmalloc(sizeof(H5P_dim_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    dset->dim->type=H5P_TYPE_SIMPLE;    /* for now... */
    if((dset->dim->s=H5O_read(dset->file,ohdr->ohdr,&ohdr->ent,H5O_SIM_DIM,0,NULL))==NULL)
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL);
    dset->header=ohdr->ohdr;        /* keep the object header location for later */
    /* Get the dataset's data offset (currently only standard storage) */
    if(NULL==H5O_read(dset->file,ohdr->ohdr,&ohdr->ent,H5O_STD_STORE,0,&store))
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL);
    dset->data=store.off;
    
    /* Register the new datatype and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_DATASET, (const VOIDP)dset))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5D_access() */

/*--------------------------------------------------------------------------
 NAME
    H5D_find_name
 PURPOSE
    Get the OID for accessing an existing HDF5 dataset object
 USAGE
    hoid_t H5D_find_name(grp_id, type, name)
        hatom_t grp_id;         IN: Atom for directory to search for dataset
        hobjtype_t type;        IN: Type of object to search for (dataset in this case)
        const char *name;       IN: Name of the object to search for
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function finds for a dataset by name in a directory.
--------------------------------------------------------------------------*/
hatom_t H5D_find_name(hatom_t grp_id, hobjtype_t type, const char *name)
{
    hdf5_file_t *file;          /* Pointer to the file-store of this object */
    H5D_oid_t *ohdr;            /* OID for reference to dataset */
    hatom_t ret_value = SUCCEED;

    FUNC_ENTER(H5D_find_name, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Allocate space for the dataset */
    if((ohdr=HDmalloc(sizeof(H5D_oid_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);

    /* Get directory symbols, etc... */
    if(H5Aatom_group(grp_id)!=H5_FILE)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);
    file=H5Aatom_object(grp_id);
/* WARNING! WARNING! WARNING! */
/* The following line explicitly uses the root symbol as the
   current working directory.  This should be changed to something more
   appropriate and is only hacked in here to get the prototype working. -QAK
*/
/* WARNING! WARNING! WARNING! */
    if(H5G_find(file,file->root_sym,&(ohdr->dir),name,&(ohdr->ent))==FAIL)
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL);
    ohdr->fid=grp_id;
    ohdr->ohdr=ohdr->ent.header;
    
    /* Register the new OID and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_OID, (const VOIDP)ohdr))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5D_find_name() */

/*--------------------------------------------------------------------------
 NAME
    H5Dset_info
 PURPOSE
    Set the type and dimensionality of a dataset.
 USAGE
    herr_t H5Dset_info(oid)
        hatom_t oid;       IN: Dataset object to modify
        hatom_t tid;       IN: Datatype object to use as node element
        hatom_t did;       IN: Dimensionality object to use as dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function sets the datatype and dataspace of a dataset.
--------------------------------------------------------------------------*/
herr_t H5Dset_info(hatom_t oid, hatom_t tid, hatom_t did)
{
    H5D_dataset_t *dataset;         /* dataset object to modify */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Dset_info, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Check that we've received correctly typed parameters */
    if(H5Aatom_group(tid)!=H5_DATATYPE || H5Aatom_group(did)!=H5_DATASPACE)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    /* Check that the datatype & dataspace haven't already been initialized */
    if(dataset->type!=0 || dataset->dim!=0)
        HGOTO_ERROR(H5E_FUNC, H5E_ALREADYINIT, FAIL);

    dataset->type=H5Aatom_object(tid);
    dataset->dim=H5Aatom_object(did);
    dataset->modified=BTRUE;       /* indicate the values have changed */

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Dset_info() */

/*--------------------------------------------------------------------------
 NAME
    H5Dget_info
 PURPOSE
    Get the type and dimensionality of a dataset.
 USAGE
    herr_t H5Dget_info(oid, tid, sid)
        hatom_t oid;       IN: Dataset object to query
        hatom_t *tid;      OUT: Datatype object to use as node element
        hatom_t *sid;      OUT: Dimensionality object to use as dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function starts access to the datatype and dataspace of an
    existing dataset.  H5Mendaccess must be called to release the datatype and
    dataspace returned from this function.
--------------------------------------------------------------------------*/
herr_t H5Dget_info(hatom_t oid, hatom_t *tid, hatom_t *sid)
{
    H5D_dataset_t *dataset;         /* dataset object to query */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Dget_info, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Check that we've received correctly typed parameters */
    if(H5Aatom_group(oid)!=H5_DATASET)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    /* Check that the datatype & dataspace haven't already been initialized */
    if(dataset->type==NULL || dataset->dim==NULL)
        HGOTO_ERROR(H5E_DATASET, H5E_UNINITIALIZED, FAIL);

    if((*tid=H5Aregister_atom(H5_DATATYPE, (const VOIDP)dataset->type))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);
    if((*sid=H5Aregister_atom(H5_DATASPACE, (const VOIDP)dataset->dim))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Dset_info() */

/*--------------------------------------------------------------------------
 NAME
    H5Dread
 PURPOSE
    Read data from a dataset
 USAGE
    herr_t H5Dread(oid)
        hatom_t oid;       IN: Dataset to read
        hatom_t did;       IN: Dimensionality object to use as dataspace for I/O
        VOIDP buf;         IN: Buffer to fill with data from the file
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function reads dataset object data to the file.  The dataspace
    ID determines the slice/hyper-slab/portion of the dataset to write.
    H5P_SCALAR is a special value which indicates that the entire dataset is
    to be written out.  (For datasets which have a scalar dataspace for the
    entire dataset, this is somewhat redundant.... :-)
--------------------------------------------------------------------------*/
herr_t H5Dread(hatom_t oid, hatom_t did, VOIDP buf)
{
    H5D_dataset_t *dataset; /* dataset object to do I/O on */
    uintn   toread;         /* number of bytes to read in */
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER(H5Dread, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Check that we've received correctly typed parameters */
    if(H5Aatom_group(did)!=H5_DATASPACE || H5Aatom_group(oid)!=H5_DATASET)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);
    if(buf==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Check that the datatype & dataspace have already been initialized */
    if(dataset->type==NULL || dataset->dim==NULL || dataset->data==(-1))
        HGOTO_ERROR(H5E_FUNC, H5E_UNINITIALIZED, FAIL);

    /* Compute the number of bytes to read */
    if(did==H5P_SCALAR) /* Check if we are reading the entire dataset */
        toread=H5T_size(dataset->type,BTRUE)*H5P_nelem(dataset->dim);
    else
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);

/* Check memory to disk datatype conversions, etc. */
/* DO THIS! -QAK */
    
    /* Write the data out to disk */
    if(H5F_block_read(dataset->file,dataset->data,toread,buf)==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Dread() */

/*--------------------------------------------------------------------------
 NAME
    H5Dwrite
 PURPOSE
    Write data for a dataset
 USAGE
    herr_t H5Dwrite(oid)
        hatom_t oid;       IN: Dataset object to modify
        hatom_t did;       IN: Dimensionality object to use as dataspace for I/O
        VOIDP buf;         IN: Buffer with data to write to the file
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function writes dataset object data to the file.  The dataspace
    ID determines the slice/hyper-slab/portion of the dataset to write.
    H5P_SCALAR is a special value which indicates that the entire dataset is
    to be written out.  (For datasets which have a scalar dataspace for the
    entire dataset, this is somewhat redundant.... :-)
--------------------------------------------------------------------------*/
herr_t H5Dwrite(hatom_t oid, hatom_t did, VOIDP buf)
{
    H5D_dataset_t *dataset;         /* dataset object to do I/O on */
    uintn   towrite;        /* number of bytes to write out */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Dwrite, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Check that we've received correctly typed parameters */
    if(H5Aatom_group(did)!=H5_DATASPACE || H5Aatom_group(oid)!=H5_DATASET)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);
    if(buf==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
        
    /* Check that the datatype & dataspace haven't already been initialized */
    if(dataset->type==NULL || dataset->dim==NULL)
        HGOTO_ERROR(H5E_FUNC, H5E_UNINITIALIZED, FAIL);

    /* Compute the number of bytes to write out */
    if(did==H5P_SCALAR) /* Check if we are writing out the entire dataset */
        towrite=H5T_size(dataset->type,BTRUE)*H5P_nelem(dataset->dim);
    else
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);

    /* Check if we have space for the dataset yet */
    if(dataset->data==(-1))
        if((dataset->data=H5MF_alloc(dataset->file,towrite))==FAIL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);

/* Check memory to disk datatype conversions, etc. */
/* DO THIS! -QAK */
    
    /* Write the data out to disk */
    if(H5F_block_write(dataset->file,dataset->data,towrite,buf)==FAIL)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Dwrite() */

/*--------------------------------------------------------------------------
 NAME
    H5D_flush
 PURPOSE
    Flush an an HDF5 dataset object to disk.
 USAGE
    herr_t H5D_flush(oid)
        hatom_t oid;       IN: Object to flush to disk
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function flushes a dataset to disk.  (i.e. makes the disk version
    agree with what's in memory, it does _not_ update the memory version with
    any changes on disk)  This function is primarily called from H5Mflush, but
    internal library routines may call it also.
--------------------------------------------------------------------------*/
herr_t H5D_flush(hatom_t oid)
{
    H5D_dataset_t *dataset;         /* dataset object to release */
    herr_t        ret_value = SUCCEED;
    H5G_entry_t	  d_sym;

    FUNC_ENTER(H5D_flush, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    
    /* Check if we have information to flush to the file... */
    if (dataset->modified) {
        if (dataset->header<=0) {
          /*
           * Create the object header.
           */
            if ((dataset->header = H5O_new (dataset->file, 0, H5D_MINHDR_SIZE))<0) {
             HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL); /*can't create header*/
          }

          /*
           * Start creating the symbol table entry.  Inserting messages
           * into the header may cache things in this entry.
           */
            d_sym.header = dataset->header;
            d_sym.type = H5G_NOTHING_CACHED;

          /*
           * Write the necessary messages to the header.
           */
            /* Write the dataset's name */
            if(dataset->name!=NULL)
              {
                if(H5O_modify(dataset->file,dataset->header,&d_sym,NULL,H5O_NAME,H5O_NEW_MESG,dataset->name)==FAIL)
                    HGOTO_ERROR (H5E_INTERNAL, H5E_CANTCREATE, FAIL);
              } /* end if */
            /* Check if this dataset has an "atomic" datatype */
            if(BTRUE==H5T_is_atomic(dataset->type))
              {
                if(H5O_modify(dataset->file,dataset->header,&d_sym,NULL,H5O_SIM_DTYPE,H5O_NEW_MESG,dataset->type)==FAIL)
                    HGOTO_ERROR (H5E_INTERNAL, H5E_CANTCREATE, FAIL);
              } /* end if */
            else
              { /* if it's not an atomic datatype, fail for right now */
                 HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);
              } /* end else */
            /* Check if this dataset has "simple" dimensionality */
            if(BTRUE==H5P_is_simple(dataset->dim))
              {
                if(H5O_modify(dataset->file,dataset->header,&d_sym,NULL,H5O_SIM_DIM,H5O_NEW_MESG,dataset->dim->s)==FAIL)
                    HGOTO_ERROR (H5E_INTERNAL, H5E_CANTCREATE, FAIL);
              } /* end if */
            else
              { /* if it's not an atomic datatype, fail for right now */
                 HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);
              } /* end else */
            /* Write the dataset's storage information */
            if(dataset->data>0)
              {
                H5O_std_store_t store;  /* standard storage info */

                store.len=H5T_size(dataset->type,BTRUE)*H5P_nelem(dataset->dim);
                store.off=dataset->data;
                if(H5O_modify(dataset->file,dataset->header,&d_sym,NULL,H5O_STD_STORE,H5O_NEW_MESG,&store)==FAIL)
                    HGOTO_ERROR (H5E_INTERNAL, H5E_CANTCREATE, FAIL);
              } /* end if */

	  /*
	   * Give the object header a name so others can access it.
	   */
#if 1 /* SEE_BELOW */
	  d_sym.type = H5G_NOTHING_CACHED;
#endif

	  /*
	   * Give the object header a name so others can access it.
	   */
	  if (H5G_insert (dataset->file, dataset->file->root_sym, NULL, dataset->name->s,
			  &d_sym)<0) {
	     HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL); /*can't name header*/
	  }
	     

       } else {
	  /*
	   * Update existing header messages if necessary.  If updating the
	   * header messages changes the symbol table entry because new
	   * info is cached, then we must re-insert the symbol entry into
	   * the directory.
	   */
	  hbool_t entry_changed = FALSE;

/*-------------------------------------------------------------------------
 * Quincey, you can get rid of this statement if either of the
 * following are true:  You don't change any messages or the
 * changed messages aren't cached in the symbol table entry.
 *-------------------------------------------------------------------------
 */
	  if (H5G_find (dataset->file, dataset->file->root_sym, NULL, dataset->name->s, &d_sym)<0) {
	     HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
	  }

	  /*
	   * Change any messaages that need to be updated.
	   */

#if 0
	  H5O_modify (dataset->file, d_sym.header, &d_sym, &entry_changed,
		      H5O_WHATEVER, 0, &the_message);
	  H5O_modify (dataset->file, d_sym.header, &d_sym, &entry_changed,
		      H5O_WHATEVER, 0, &the_message);
	  H5O_modify (dataset->file, d_sym.header, &d_sym, &entry_changed,
		      H5O_WHATEVER, 0, &the_message);
#endif

/*-------------------------------------------------------------------------
 * Quincey, you can get rid of this `if' statement and the
 * H5G_modify() call subject to the same constraints as above.
 *-------------------------------------------------------------------------
 */
	  if (entry_changed) {
	     /*
	      *	Make sure the symbol table entry as modified by the
	      *	changing of messages gets back to the file.
	      */
	     H5G_modify (dataset->file, dataset->file->root_sym, NULL, dataset->name->s, &d_sym);
	  }
       }
       dataset->modified = FALSE;
    }
       
done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5D_flush() */

/*--------------------------------------------------------------------------
 NAME
    H5D_release
 PURPOSE
    Release access to an HDF5 dataset object.
 USAGE
    herr_t H5D_release(oid)
        hatom_t oid;       IN: Object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function releases a dataset from active use by a user.
--------------------------------------------------------------------------*/
herr_t H5D_release(hatom_t oid)
{
    H5D_dataset_t *dataset;         /* dataset object to release */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5D_release, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the dataset so we can check for changes and release it */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Check if we have information to flush to the file... */
    if(dataset->modified==BTRUE)
        H5D_flush(oid);

    /* release the memory used for the dataset */
    if(dataset->name!=NULL)
      {
        HDfree(dataset->name->s);
        HDfree(dataset->name);
      } /* end if */
    HDfree(dataset);

    /* Delete the dataset from the atom group */
    if(H5Aatom_object(oid)==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5D_release() */

