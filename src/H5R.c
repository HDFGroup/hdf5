/****************************************************************************
* NCSA HDF								   *
* Software Development Group						   *
* National Center for Supercomputing Applications			   *
* University of Illinois at Urbana-Champaign				   *
* 605 E. Springfield, Champaign IL 61820				   *
*									   *
* For conditions of distribution and use, see the accompanying		   *
* hdf/COPYING file.							   *
*									   *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <H5private.h>		/* Generic Functions */
#include <H5Iprivate.h>		/* ID Functions */
#include <H5Dprivate.h>		/* Datasets */
#include <H5Eprivate.h>		/* Error handling */
#include <H5Fprivate.h>		/* Files */
#include <H5Gprivate.h>		/* Groups */
#include <H5Rprivate.h>		/* References */
#include <H5Sprivate.h>		/* Dataspaces */

/* Interface initialization */
#define PABLO_MASK	H5R_mask
#define INTERFACE_INIT	H5R_init_interface
static intn		interface_initialize_g = FALSE;
static herr_t		H5R_init_interface(void);
static void		H5R_term_interface(void);

/* Static functions */
static herr_t H5R_create(void *ref, H5G_entry_t *loc, const char *name,
        H5R_type_t ref_type, H5S_t *space);
static hid_t H5R_dereference(H5D_t *dset, H5R_type_t ref_type, void *_ref);
static H5S_t * H5R_get_region(void *ref);


/*--------------------------------------------------------------------------
NAME
   H5R_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5R_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5R_init_interface(void)
{
    herr_t		    ret_value = SUCCEED;
    FUNC_ENTER(H5R_init_interface, FAIL);

    /* Initialize the atom group for the file IDs */
    if ((ret_value = H5I_init_group(H5I_REFERENCE, H5I_REFID_HASHSIZE,
            H5R_RESERVED_ATOMS, (herr_t (*)(void *)) NULL)) != FAIL) {
        ret_value = H5_add_exit(&H5R_term_interface);
    }

    FUNC_LEAVE(ret_value);
}   /* end H5R_init_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5R_term_interface
 PURPOSE
    Terminate various H5R objects
 USAGE
    void H5R_term_interface()
 RETURNS
    void
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5R_term_interface(void)
{
    /* Free ID group */
    H5I_destroy_group(H5I_REFERENCE);
}   /* end H5R_term_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5R_create
 PURPOSE
    Creates a particular kind of reference for the user
 USAGE
    herr_t H5R_create(ref, loc, name, ref_type, space)
        void *ref;          OUT: Reference created
        H5G_entry_t *loc;   IN: File location used to locate object pointed to
        const char *name;   IN: Name of object at location LOC_ID of object
                                    pointed to
        H5R_type_t ref_type;    IN: Type of reference to create
        H5S_t *space;       IN: Dataspace ID with selection, used for Dataset
                                    Region references.
        
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Creates a particular type of reference specified with REF_TYPE, in the
    space pointed to by REF.  The LOC_ID and NAME are used to locate the object
    pointed to and the SPACE_ID is used to choose the region pointed to (for
    Dataset Region references).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5R_create(void *_ref, H5G_entry_t *loc, const char *name, H5R_type_t ref_type, H5S_t __unused__ *space)
{
    H5G_stat_t sb;              /* Stat buffer for retrieving OID */
    herr_t ret_value = FAIL;

    FUNC_ENTER(H5R_create, FAIL);

    assert(_ref);
    assert(loc);
    assert(name);
    assert(ref_type>H5R_BADTYPE || ref_type<H5R_MAXTYPE);

    if (H5G_get_objinfo (loc, name, 0, &sb)<0)
        HGOTO_ERROR (H5E_REFERENCE, H5E_NOTFOUND, FAIL, "unable to stat object");

    switch(ref_type) {
        case H5R_OBJECT:
        {
            hobj_ref_t *ref=(hobj_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            uint8 *p;       /* Pointer to OID to store */

            /* Set information for reference */
            p=(uint8 *)ref->oid;
            H5F_addr_encode(loc->file,&p,(const haddr_t *)sb.objno);
            break;
        }

        case H5R_DATASET_REGION:
        case H5R_INTERNAL:
            HRETURN_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL,
                  "Dataset region and internal references are not supported yet");

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
            assert("unknown reference type" && 0);
            HRETURN_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL,
                  "internal error (unknown reference type)");
    } /* end switch */

    /* Return success */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
}   /* end H5R_create() */


/*--------------------------------------------------------------------------
 NAME
    H5Rcreate
 PURPOSE
    Creates a particular kind of reference for the user
 USAGE
    herr_t H5Rcreate(ref, loc_id, name, ref_type, space_id)
        void *ref;          OUT: Reference created
        hid_t loc_id;       IN: Location ID used to locate object pointed to
        const char *name;   IN: Name of object at location LOC_ID of object
                                    pointed to
        H5R_type_t ref_type;    IN: Type of reference to create
        hid_t space_id;     IN: Dataspace ID with selection, used for Dataset
                                    Region references.
        
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Creates a particular type of reference specified with REF_TYPE, in the
    space pointed to by REF.  The LOC_ID and NAME are used to locate the object
    pointed to and the SPACE_ID is used to choose the region pointed to (for
    Dataset Region references).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Rcreate(void *ref, hid_t loc_id, const char *name, H5R_type_t ref_type, hid_t space_id)
{
    H5G_entry_t *loc = NULL;        /* File location */
    H5S_t	*space = NULL;          /* Pointer to dataspace containing region */
    herr_t ret_value = FAIL;

    FUNC_ENTER(H5Rcreate, FAIL);
    H5TRACE5("e","xisRti",ref,loc_id,name,ref_type,space_id);

    /* Check args */
    if(ref==NULL)
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer");
    if (NULL==(loc=H5G_loc (loc_id)))
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
    if(ref_type<=H5R_BADTYPE || ref_type>=H5R_MAXTYPE)
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type");
    if(ref_type!=H5R_OBJECT)
        HRETURN_ERROR (H5E_ARGS, H5E_UNSUPPORTED, FAIL, "reference type not supported");
    if (space_id!=(-1) && (H5I_DATASPACE!=H5I_get_type (space_id) || NULL==(space=H5I_object (space_id))))
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace");

    /* Create reference */
    if ((ret_value=H5R_create(ref,loc,name,ref_type,space))<0)
        HGOTO_ERROR (H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to create reference");

done:
    FUNC_LEAVE(ret_value);
}   /* end H5Rcreate() */


/*--------------------------------------------------------------------------
 NAME
    H5R_dereference
 PURPOSE
    Opens the HDF5 object referenced.
 USAGE
    hid_t H5R_dereference(ref)
        H5D_t *dset;        IN: Dataset reference object is in.
        H5R_type_t ref_type;    IN: Type of reference
        void *ref;          IN: Reference to open.
        
 RETURNS
    Valid ID on success, FAIL on failure
 DESCRIPTION
    Given a reference to some object, open that object and return an ID for
    that object.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Currently only set up to work with references to datasets
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static hid_t
H5R_dereference(H5D_t *dset, H5R_type_t ref_type, void *_ref)
{
    H5D_t *dataset;             /* Pointer to dataset to open */
    hobj_ref_t *ref=(hobj_ref_t *)_ref; /* Only object references currently supported */
    H5G_entry_t ent;            /* Symbol table entry */
    uint8 *p;                   /* Pointer to OID to store */
    hid_t ret_value = FAIL;

    FUNC_ENTER(H5R_dereference, FAIL);

    assert(ref);
    assert(ref_type==H5R_OBJECT);

    /*
     * Switch on object type, when we implement that feature, always try to
     *  open a dataset for now
     */
    /* Allocate the dataset structure */
    if (NULL==(dataset = H5D_new(NULL))) {
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
		     "memory allocation failed");
    }

    /* Initialize the symbol table entry */
    HDmemset(&ent,0,sizeof(H5G_entry_t));
    ent.type=H5G_NOTHING_CACHED;
    ent.file=H5D_get_file(dset);
    p=(uint8 *)ref->oid;
    H5F_addr_decode(ent.file,(const uint8 **)&p,&(ent.header));

    /* Open the dataset object */
    if (H5D_open_oid(dataset, &ent) < 0) {
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "not found");
    }

    /* Create an atom for the dataset */
    if ((ret_value = H5I_register(H5I_DATASET, dataset)) < 0) {
        H5D_close(dataset);
        HRETURN_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL,
		      "can't register dataset");
    }

done:
    FUNC_LEAVE(ret_value);
}   /* end H5R_dereference() */


/*--------------------------------------------------------------------------
 NAME
    H5Rdereference
 PURPOSE
    Opens the HDF5 object referenced.
 USAGE
    hid_t H5Rdereference(ref)
        hid_t dataset;      IN: Dataset reference object is in.
        H5R_type_t ref_type;    IN: Type of reference to create
        void *ref;          IN: Reference to open.
        
 RETURNS
    Valid ID on success, FAIL on failure
 DESCRIPTION
    Given a reference to some object, open that object and return an ID for
    that object.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Rdereference(hid_t dataset, H5R_type_t ref_type, void *_ref)
{
    H5D_t		   *dset = NULL;	/* dataset object */
    hid_t ret_value = FAIL;

    FUNC_ENTER(H5Rdereference, FAIL);
    H5TRACE3("i","iRtx",dataset,ref_type,_ref);

    /* Check args */
    if (H5I_DATASET != H5I_get_type(dataset) || NULL == (dset = H5I_object(dataset)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if(ref_type<=H5R_BADTYPE || ref_type>=H5R_MAXTYPE)
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type");
    if(_ref==NULL)
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer");

    /* Create reference */
    if ((ret_value=H5R_dereference(dset, ref_type, _ref))<0)
        HGOTO_ERROR (H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable dereference object");

done:
    FUNC_LEAVE(ret_value);
}   /* end H5Rdereference() */


/*--------------------------------------------------------------------------
 NAME
    H5R_get_region
 PURPOSE
    Retrieves a dataspace with the region pointed to selected.
 USAGE
    H5S_t *H5R_get_region(ref)
        void *ref;        IN: Reference to open.
        
 RETURNS
    Pointer to the dataspace on success, NULL on failure
 DESCRIPTION
    Given a reference to some object, creates a copy of the dataset pointed
    to's dataspace and defines a selection in the copy which is the region
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5S_t *
H5R_get_region(void __unused__ *ref)
{
    H5S_t *ret_value = NULL;

    FUNC_ENTER(H5R_get_region, NULL);

    assert(ref);

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE(ret_value);
}   /* end H5R_get_region() */


/*--------------------------------------------------------------------------
 NAME
    H5Rget_region
 PURPOSE
    Retrieves a dataspace with the region pointed to selected.
 USAGE
    hid_t H5Rget_region(ref)
        void *ref;        IN: Reference to open.
        
 RETURNS
    Valid ID on success, FAIL on failure
 DESCRIPTION
    Given a reference to some object, creates a copy of the dataset pointed
    to's dataspace and defines a selection in the copy which is the region
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Rget_region(hid_t dset, H5R_type_t rtype, void *ref)
{
    H5S_t *space = NULL;
    hid_t ret_value = FAIL;

    FUNC_ENTER(H5Rget_region, FAIL);

    /* Check args */
    if(ref==NULL)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer");

    /* Create reference */
    if ((space=H5R_get_region(ref))==NULL)
        HGOTO_ERROR (H5E_REFERENCE, H5E_CANTCREATE, FAIL, "unable to create dataspace");

    /* Atomize */
    if ((ret_value=H5I_register (H5I_DATASPACE, space))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom");

done:
    FUNC_LEAVE(ret_value);
}   /* end H5Rget_region() */

