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
#include <H5Gprivate.h> /* Groups */
#include <H5Oprivate.h> /* Object Headers */
#include <H5Mprivate.h> /* Meta-Object API */
#include <H5Dprivate.h> /* Dataset functions */
#include <H5Eprivate.h> /* Error handling */
#include <H5Gprivate.h> /* Symbol tables */
#include <H5Mprivate.h> /* Meta data */
#include <H5Oprivate.h> /* Object headers */

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
    if(H5Aatom_group(owner_id)==H5_FILE)
        new_dset->file=owner_id;
    else
        new_dset->file=H5Mget_file(owner_id);
    new_dset->parent=owner_id;      /* set the owner's ID */
    new_dset->name=HDstrdup(name);  /* make a copy of the dataset name */
    new_dset->modified=BTRUE;       /* Yep, we're new... */
    new_dset->type=0;
    new_dset->dim=0;
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

    dataset->type=tid;
    dataset->dim=did;
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
    H5Dwrite
 PURPOSE
    Write data for a dataset
 USAGE
    herr_t H5Dset_info(oid)
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
    H5D_dataset_t *dataset;         /* dataset object to release */
#if 0
    uintn   towrite;        /* number of bytes to write out */
#endif
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Dwrite, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Check that we've received correctly typed parameters */
    if(H5Aatom_group(did)!=H5_DATASPACE)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);
    if(buf==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    /* Check that the datatype & dataspace haven't already been initialized */
    if(dataset->type==0 || dataset->dim==0)
        HGOTO_ERROR(H5E_FUNC, H5E_UNINITIALIZED, FAIL);

#if 0
    towrite=H5Tsize(dataset->type)*H5Pnelem(did);
#endif
/* Check memory to disk datatype conversions, etc. */
/* data out */

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
    any changes on disk)
--------------------------------------------------------------------------*/
herr_t H5D_flush(hatom_t oid)
{
    H5D_dataset_t *dataset;         /* dataset object to release */
    herr_t        ret_value = SUCCEED;
    hdf5_file_t	  *file = NULL;
    H5G_entry_t	  d_sym;

    FUNC_ENTER(H5D_flush, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    
    /* Check if we have information to flush to the file... */
    if (dataset->modified) {
       /* Get the dataset's file... (I'm not fond of this. -QAK) */
       if((file=H5Aatom_object(dataset->file))==NULL)
	  HGOTO_ERROR(H5E_INTERNAL, H5E_BADATOM, FAIL);


       if (dataset->header<=0) {
	  /*
	   * Create the object header.
	   */
	  if ((dataset->header = H5O_new (file, 0, H5D_MINHDR_SIZE))<0) {
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
	            /*
		     * Quincey?  It should be just a matter of filling in the
		     * appropriate H5O_*_t struct and passing it to
		     * H5O_modify() along with &d_sym.
		     */


	  /*
	   * Give the object header a name so others can access it.
	   */
#if 1 /* SEE_BELOW */
	  d_sym.type = H5G_NOTHING_CACHED;
#endif
	  if (H5G_insert (file, file->root_sym, NULL, dataset->name,
			  &d_sym)<0) {
	     HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL); /*can't name header*/
	  }
	     

       } else {
	  /*
	   * Update existing header messages if necessary.  If updating the
	   * header messages changes the symbol table entry because new
	   * info is cached, then we must re-insert the symbol entry into
	   * the directory.  I'm still working on caching things in symbol
	   * table entries, so the easiest thing to do is:
	   *
	   * 	A. Don't change any messages, or
	   *	B. Don't let the message cache anything, or
	   *	C. Uncomment the `SEE_BELOW' statement above.
	   */
	  
	             /*
		      *	Quincey? It should be just a matter of filling in the
		      *	appropriate H5O_*_t structs and passing them to
		      *	H5O_modify(). Don't worry about caching things in
		      *	the symbol table entry yet (pass NULL for the
		      *	symbol table entry).
		      */

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

    /* relase the memory used for the dataset */
    if(dataset->name!=NULL)
        HDfree(dataset->name);
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


