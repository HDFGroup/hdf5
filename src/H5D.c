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
#include <H5Dprivate.h> /* Dataset functions */
#include <H5Eprivate.h> /* Error handling */
#include <H5Gprivate.h> /* Group headers */
#include <H5Mprivate.h> /* Meta data */
#include <H5MFprivate.h> /* File space allocation header */
#include <H5MMprivate.h> /* Memory management */
#include <H5Mprivate.h> /* Meta-Object API */
#include <H5Oprivate.h> /* Object headers */

#define PABLO_MASK	H5D_mask

/*
 * A dataset is the following struct.
 */
typedef struct H5D_t {
   H5F_t	*file; 		/* File store for this object       */
   H5G_entry_t	*ent;   /* Cached object header stuff       */
   hid_t tid;           /* Datatype ID of this dataset      */
   hid_t sid;           /* Dataspace ID of this dataset     */
   haddr_t	data_addr; 	/* Data storage address			    */
   hbool_t	dirty;		/* Header messages not updated yet  */
} H5D_t;

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

    /* Make certain the H5T & H5P interfaces have been initialized */
    H5T_init();
    H5P_init();

    /* Initialize the atom group for the file IDs */
    if((ret_value=H5Ainit_group(H5_DATASET,H5A_DATASETID_HASHSIZE,H5D_RESERVED_ATOMS,NULL))!=FAIL)
        ret_value=H5_add_exit(&H5D_term_interface);

    FUNC_LEAVE(ret_value);
}	/* H5D_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5D_term_interface
 PURPOSE
    Terminate various H5D objects
 USAGE
    void H5D_term_interface()
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
void H5D_term_interface (void)
{
    H5Adestroy_group(H5_DATASET);
} /* end H5D_term_interface() */

/*--------------------------------------------------------------------------
 NAME
    H5D_create
 PURPOSE
    Create a new HDF5 dataset object
 USAGE
    hid_t H5D_create(owner_id, type, name)
        hid_t owner_id;       IN: Group/file which owns this object
        hobjtype_t type;        IN: Type of object to create
        const char *name;       IN: Name of the object
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
    This function actually creates a dataset object in a file (of course,
    output might not happen for some time).
--------------------------------------------------------------------------*/
hid_t H5D_create(hid_t owner_id, hobjtype_t type, const char *name)
{
    H5D_t *new_dset;        /* new dataset object to create */
    hid_t ret_value = SUCCEED;
    H5F_t *file = NULL;
    
    FUNC_ENTER(H5D_create, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Convert atom arguments to pointers */
    if(H5Aatom_group(owner_id)!=H5_FILE)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);
    file = H5Aatom_object (owner_id);
    
    /* Allocate space for the new dataset */
    if((new_dset=HDcalloc(1, sizeof(H5D_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    
    /* Initialize the dataset object */
    new_dset->file = file;
    new_dset->tid=(-1);		/* No type yet */
    new_dset->sid=(-1);			/* No dimensions yet */
    H5F_addr_undef (&(new_dset->data_addr)); /* No data yet */
    new_dset->dirty = FALSE;		/* There are no messages yet */

    /* Open (and create) a new file object */
    if (NULL==(new_dset->ent = H5G_create (file, name, H5D_MINHDR_SIZE))) {
       HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
    }

    /* Register the new datatype and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_DATASET, (const VOIDP)new_dset))<0)
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
    H5D_find_name
 PURPOSE
    Get the OID for accessing an existing HDF5 dataset object
 USAGE
    hoid_t H5D_find_name(grp_id, type, name)
        hid_t grp_id;         IN: Atom for group to search for dataset
        hobjtype_t type;        IN: Type of object to search for (dataset in
 	                        this case)
        const char *name;       IN: Name of the object to search for
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function finds for a dataset by name in a group.
--------------------------------------------------------------------------*/
hid_t H5D_find_name(hid_t grp_id, hobjtype_t obj_type, const char *name)
{
    H5F_t *file;          /* Pointer to the file-store of this object */
    H5D_t	*dset = NULL;	/* The dataset */
    h5_datatype_t *type;    /* The dataset's type */
    H5P_dim_t *dim;         /* The dataset's dataspace */
    hid_t ret_value = SUCCEED;
    H5O_std_store_t store;

    FUNC_ENTER(H5D_find_name, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Convert atom arguments to pointers */
    if(H5Aatom_group(grp_id)!=H5_FILE)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);
    file=H5Aatom_object(grp_id);

    /* Allocate space for the dataset */
    if(NULL==(dset=HDcalloc(1, sizeof(H5D_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);

    /* Initialize file, group, name fields */
    dset->file = file;
    dset->dirty = FALSE;
    
    /* Open the dataset object */
    if (NULL==(dset->ent=H5G_open (file, name))) {
       HGOTO_ERROR (H5E_DATASET, H5E_NOTFOUND, FAIL);
    }

    /* Get the dataset's type (currently only atomic types) */
    if((type=HDcalloc(1,sizeof(h5_datatype_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    if (NULL==H5O_read (dset->file, NO_ADDR, dset->ent, H5O_SIM_DTYPE, 0,
			type))
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL);
    if((dset->tid=H5Aregister_atom(H5_DATATYPE, (const VOIDP)type))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);
    
    /* Get the dataset's dimensionality (currently only simple dataspaces) */
    if((dim=HDcalloc(1,sizeof(H5P_dim_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    dim->type=H5P_TYPE_SIMPLE;    /* for now... */
    if (NULL==(dim->s=H5O_read (dset->file, NO_ADDR, dset->ent,
				      H5O_SIM_DIM, 0, NULL)))
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL);
    if((dset->sid=H5Aregister_atom(H5_DATASPACE, (const VOIDP)dim))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

    /* Get the dataset's data offset (currently only standard storage) */
    if (NULL==H5O_read (dset->file, NO_ADDR, dset->ent, H5O_STD_STORE, 0,
			&store))
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL);
    dset->data_addr=store.off;

    /* Register the new OID and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_DATASET, (const VOIDP)dset))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */
       if (dset) {
#ifdef LATER
	  /* We might need to free the `type' and `dim' fields also... */
#endif
	  H5MM_xfree (dset);
       }
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
        hid_t oid;       IN: Dataset object to modify
        hid_t tid;       IN: Datatype object to use as node element
        hid_t sid;       IN: Dimensionality object to use as dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function sets the datatype and dataspace of a dataset.
--------------------------------------------------------------------------*/
herr_t H5Dset_info(hid_t oid, hid_t tid, hid_t sid)
{
    H5D_t	*dataset = NULL;         /* dataset object to modify */
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER(H5Dset_info, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Check that we've received correctly typed parameters */
    if(H5Aatom_group(tid)!=H5_DATATYPE || H5Aatom_group(sid)!=H5_DATASPACE)
        HGOTO_ERROR(H5E_ATOM, H5E_BADTYPE, FAIL);

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    /* Check that the datatype & dataspace haven't already been initialized */
    if(dataset->tid!=(-1) || dataset->sid!=(-1))
        HGOTO_ERROR(H5E_FUNC, H5E_ALREADYINIT, FAIL);

    /* Increment the reference count for the datatype */
    if(H5Ainc_ref(tid)==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    dataset->tid=tid;

    /* Increment the reference count for the datatype */
    if(H5Ainc_ref(sid)==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    dataset->sid=sid;

    /* Mark the dataset as modified (Huh? - QAK) */
    dataset->dirty = TRUE;

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
        hid_t oid;       IN: Dataset object to query
        hid_t *tid;      OUT: Datatype object to use as node element
        hid_t *sid;      OUT: Dimensionality object to use as dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function starts access to the datatype and dataspace of an
    existing dataset.  H5Mendaccess must be called to release the datatype and
    dataspace returned from this function.
--------------------------------------------------------------------------*/
herr_t H5Dget_info(hid_t oid, hid_t *tid, hid_t *sid)
{
    H5D_t 	  *dataset;         /* dataset object to query */
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
    /* Check that the datatype & dataspace have already been initialized */
    if(dataset->tid==(-1) || dataset->sid==(-1))
        HGOTO_ERROR(H5E_DATASET, H5E_UNINITIALIZED, FAIL);

    /* Get the Dataset's IDs */
    if(H5Ainc_ref(dataset->tid)==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    *tid=dataset->tid;
    if(H5Ainc_ref(dataset->sid)==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    *sid=dataset->sid;

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
        hid_t oid;       IN: Dataset to read
        hid_t did;       IN: Dimensionality object to use as dataspace for I/O
        VOIDP buf;         IN: Buffer to fill with data from the file
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function reads dataset object data from the file.  The dataspace
    ID determines the slice/hyper-slab/portion of the dataset to write.
    H5P_ALL is a special value which indicates that the entire dataset is
    to be written out.  (For datasets which have a scalar dataspace for the
    entire dataset, this is somewhat redundant.... :-)
--------------------------------------------------------------------------*/
herr_t H5Dread(hid_t oid, hid_t did, VOIDP buf)
{
    H5D_t *dataset; /* dataset object to do I/O on */
    void *readbuf=NULL;     /* pointer to buffer to write out */
    uintn   free_buf=0;     /* if temporary conversion buffer needs to be free'd */
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
    if(dataset->tid==(-1) || dataset->sid==(-1) ||
       !H5F_addr_defined (&(dataset->data_addr)))
        HGOTO_ERROR(H5E_FUNC, H5E_UNINITIALIZED, FAIL);

    /* Compute the number of bytes to read */
    if(did==H5P_ALL) /* Check if we are reading the entire dataset */
        toread=H5Tsize(dataset->tid,BTRUE)*H5Pnelem(dataset->sid);
    else
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);

    /* Check memory to disk datatype conversions, etc. */
/* This is totally hacked up code, but I'm in a hurry. ;-/ -QAK */
    if(H5Tarch(dataset->tid)!=H5T_ARCH_TYPE)
      {
        if((readbuf=HDmalloc(toread))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
        free_buf=1;
      } /* end if */
    else
        readbuf=buf;
    
    
    /* Read data from disk */
    if (H5F_block_read (dataset->file, &(dataset->data_addr), toread,
			readbuf)<0) {
       HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL);
    }
    
    if(free_buf!=0)
        H5D_convert_buf(buf,readbuf,toread,H5Tsize(dataset->tid,BTRUE));

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */
    if(free_buf!=0) /* check if we need to release the conversion buffer */
        HDfree(readbuf);

    FUNC_LEAVE(ret_value);
} /* end H5Dread() */

/*--------------------------------------------------------------------------
 NAME
    H5Dwrite
 PURPOSE
    Write data for a dataset
 USAGE
    herr_t H5Dwrite(oid)
        hid_t oid;       IN: Dataset object to modify
        hid_t did;       IN: Dimensionality object to use as dataspace for I/O
        VOIDP buf;         IN: Buffer with data to write to the file
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function writes dataset object data to the file.  The dataspace
    ID determines the slice/hyper-slab/portion of the dataset to write.
    H5P_ALL is a special value which indicates that the entire dataset is
    to be written out.  (For datasets which have a scalar dataspace for the
    entire dataset, this is somewhat redundant.... :-)
--------------------------------------------------------------------------*/
herr_t H5Dwrite(hid_t oid, hid_t did, VOIDP buf)
{
    H5D_t *dataset;         /* dataset object to do I/O on */
    uintn   towrite;        /* number of bytes to write out */
    void *writebuf=NULL;    /* pointer to buffer to write out */
    uintn   free_buf=0;     /* if temporary conversion buffer needs to be free'd */
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
        
    /* Check that the datatype & dataspace have already been initialized */
    if(dataset->tid==(-1) || dataset->sid==(-1))
        HGOTO_ERROR(H5E_FUNC, H5E_UNINITIALIZED, FAIL);

    /* Compute the number of bytes to write out */
    if(did==H5P_ALL) /* Check if we are writing out the entire dataset */
        towrite=H5Tsize(dataset->tid,BTRUE)*H5Pnelem(dataset->sid);
    else
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);

    /* Check if we have space for the dataset yet */
    if (!H5F_addr_defined (&(dataset->data_addr))) {
        if (H5MF_alloc (dataset->file, H5MF_RAW, towrite,
			&(dataset->data_addr)/*out*/)<0) {
	   HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
	}
	dataset->dirty = TRUE;
    }

    /* Check memory to disk datatype conversions, etc. */
/* This is totally hacked up code, but I'm in a hurry. ;-/ -QAK */
    if(H5Tarch(dataset->tid)!=H5T_ARCH_TYPE)
      {
        if((writebuf=HDmalloc(towrite))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
        H5D_convert_buf(writebuf,buf,towrite,H5Tsize(dataset->tid,BTRUE));
        free_buf=1;
      } /* end if */
    else
        writebuf=buf;
    
    /* Write the data out to disk */
    if (H5F_block_write (dataset->file, &(dataset->data_addr), towrite,
			 writebuf)<0) {
       HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL);
    }

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */
    if(free_buf!=0) /* check if we need to release the conversion buffer */
        HDfree(writebuf);

    FUNC_LEAVE(ret_value);
} /* end H5Dwrite() */

/*--------------------------------------------------------------------------
 NAME
    H5D_flush
 PURPOSE
    Flush an an HDF5 dataset object to disk.
 USAGE
    herr_t H5D_flush(oid)
        hid_t oid;       IN: Object to flush to disk
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function flushes a dataset to disk.  (i.e. makes the disk version
    agree with what's in memory, it does _not_ update the memory version with
    any changes on disk)  This function is primarily called from H5Mflush, but
    internal library routines may call it also.
--------------------------------------------------------------------------*/
herr_t H5D_flush(hid_t oid)
{
    H5D_t 	*dataset;         /* dataset object to release */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER(H5D_flush, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the object */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    
    if (dataset->dirty) {

	/*
	 * Modify/create messages for this dataset.  Begin with the
	 * type information.
	 */
	if (H5Tis_atomic (dataset->tid)) {
        h5_datatype_t *type=H5Aatom_object(dataset->tid);

	   if (H5O_modify (dataset->file, NO_ADDR, dataset->ent,
			   H5O_SIM_DTYPE, 0, type)<0) {
	      /* Can't create/update type message */
	      HGOTO_ERROR (H5E_INTERNAL, H5E_CANTCREATE, FAIL);
	   }
	} else {
	   /* Not an atomic datatype */
	   HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);
	}

	/*
	 * Modify/create the dimensionality information.
	 */
	if (H5Pis_simple (dataset->sid)) {
        H5P_dim_t *dim=H5Aatom_object(dataset->sid);

	   if (H5O_modify (dataset->file, NO_ADDR, dataset->ent,
			   H5O_SIM_DIM, 0, dim->s)<0) {
	      /* Can't create/update dimensionality message */
	      HGOTO_ERROR (H5E_INTERNAL, H5E_CANTCREATE, FAIL);
	   }
	} else {
	   /* Not an atomic datatype */
	   HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);
	}

	/*
	 * Modify/create the dataset's storage information.
	 */
	if (H5F_addr_defined (&(dataset->data_addr))) {
	   H5O_std_store_t store;  /* standard storage info */

	   store.len = H5Tsize (dataset->tid, BTRUE) * H5Pnelem (dataset->sid);
	   store.off = dataset->data_addr;
	   if (H5O_modify (dataset->file, NO_ADDR, dataset->ent,
			   H5O_STD_STORE, 0, &store)<0) {
	      /* Can't create/modify storage information */
	      HGOTO_ERROR (H5E_INTERNAL, H5E_CANTCREATE, FAIL);
	   }
	}

	dataset->dirty = FALSE;	/*it's clean now*/
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
        hid_t oid;       IN: Object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function releases a dataset from active use by a user.
--------------------------------------------------------------------------*/
herr_t H5D_release(hid_t oid)
{
    H5D_t 	*dataset;         /* dataset object to release */
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER(H5D_release, H5D_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Get the dataset so we can check for changes and release it */
    if((dataset=H5Aatom_object(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Check if we have information to flush to the file... */
    if(dataset->dirty && H5D_flush(oid)<0) {
       /* Can't flush dataset */
       HGOTO_ERROR (H5E_OHDR, H5E_CANTFLUSH, FAIL);
    }

    /* Close the dataset object */
    H5G_close (dataset->file, dataset->ent);
    dataset->ent = NULL;

    /* Release the atoms for the datatype and dataspace */
    H5Adec_ref(dataset->tid);
    H5Adec_ref(dataset->sid);
    
    /* release the memory used for the dataset */
    H5MM_xfree (dataset);

    /* Delete the dataset from the atom group */
    if(H5Aremove_atom(oid)==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5D_release() */

