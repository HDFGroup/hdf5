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
       H5T.c
   HDF5 Data-type routines

   EXPORTED ROUTINES
       H5Tget_num_fields  -- Get the number of fields in a compound datatype
       H5Tis_field_atomic -- Check if a field is atomic
       H5Tis_atomic/H5T_is_atomic -- Check if a datatype is atomic
       H5Tset_type        -- Set the base type of a user-defined datatype
       H5Tget_type        -- Get the base type of a datatype
       H5Tadd_field       -- Add a field to a compound datatype
       H5Tsize            -- Determine the size of a datatype

   LIBRARY-SCOPED ROUTINES
       H5T_create         -- (Meta-Object) Create a datatype
       H5T_release        -- (Meta-Object) Release access to a datatype

   LOCAL ROUTINES
       H5T_init_interface    -- initialize the interface
   + */

#include <H5private.h>  /* Generic Functions */
#include <H5Aprivate.h> /* Atom functions */
#include <H5Eprivate.h> /* Error handling */
#include <H5Mprivate.h> /* Meta data */
#include <H5Pprivate.h> /* Data space */
#include <H5Tprivate.h> /* Data-type functions */

#define PABLO_MASK	H5T_mask

/*--------------------- Locally scoped variables -----------------------------*/

/* Whether we've installed the library termination function yet for this interface */
static intn interface_initialize_g = FALSE;

/*------------------_-- Local function prototypes ----------------------------*/
static herr_t H5T_init_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5T_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5T_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t H5T_init_interface(void)
{
    herr_t ret_value = SUCCEED;
    FUNC_ENTER (H5T_init_interface, NULL, FAIL);

    /* Initialize the atom group for the file IDs */
    ret_value=H5Ainit_group(H5_DATATYPE,H5A_DATATYPEID_HASHSIZE,H5T_RESERVED_ATOMS);

    FUNC_LEAVE(ret_value);
}	/* H5T_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5T_create
 PURPOSE
    Create a new HDF5 data-type object
 USAGE
    hatom_t H5T_create(owner_id, type, name)
        hatom_t owner_id;       IN: Group/file which owns this object
        hobjtype_t type;        IN: Type of object to create
        const char *name;       IN: Name of the object
 RETURNS
    Returns ID (atom) on success, FAIL on failure
 DESCRIPTION
        This function actually creates the data-type object.
--------------------------------------------------------------------------*/
hatom_t H5T_create(hatom_t owner_id, hobjtype_t type, const char *name)
{
    h5_datatype_t *new_dt;            /* new data-type object to create */
    hatom_t ret_value = SUCCEED;

    FUNC_ENTER(H5T_create, H5T_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Allocate space for the new data-type */
    if((new_dt=HDmalloc(sizeof(h5_datatype_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    
    /* Initialize the datatype */
    new_dt->dt.base=0;           /* No Default datatype */
    new_dt->name=HDstrdup(name); /* Make a copy of the datatype's name */
    new_dt->ci=NULL;             /* Set the complex information to NULL */

    /* Register the new datatype and get an ID for it */
    if((ret_value=H5Aregister_atom(H5_DATATYPE, (const VOIDP)new_dt))==FAIL)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5T_create() */

/*--------------------------------------------------------------------------
 NAME
    H5Tget_num_fields
 PURPOSE
    Return the number of fields in a compound datatype
 USAGE
    uint32 H5Tget_num_fields(tid)
        hatom_t tid;            IN: Datatype object to query
 RETURNS
    The number of fields in a compound datatype on success, UFAIL on failure
 DESCRIPTION
        This function checks the number of fields in a compound user-defined
    datatype.  UFAIL is returned on an error or if an atomic datatype is
    queried, otherwise the number of fields is returned.
--------------------------------------------------------------------------*/
uint32 H5Tget_num_fields(hatom_t tid)
{
    h5_datatype_t *dt;         /* new data-type object to create */
    uint32        ret_value = UFAIL;

    FUNC_ENTER(H5Tget_num_fields, H5T_init_interface, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Go get the object */
    if((dt=H5Aatom_object(tid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Check the base type of the datatype */
    if(H5T_COMPOUND!=dt->dt.base)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);
    
    /* Check if the compound information has been initialized */
    if(NULL==dt->ci)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNINITIALIZED, FAIL);

    /* Grab the number of fields */
    ret_value=dt->ci->n;

done:
  if(ret_value == UFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Tget_num_fields() */

/*--------------------------------------------------------------------------
 NAME
    H5Tis_field_atomic
 PURPOSE
    Check if a field in a compound datatype is atomic
 USAGE
    hbool_t H5Tis_field_atomic(tid, fidx)
        hatom_t tid;            IN: Datatype object to query
        uintn fidx;             IN: Index of the field to query
 RETURNS
    BFAIL/BTRUE/BFALSE
 DESCRIPTION
        This function checks the basic type of field in a user-defined datatype.
    BTRUE is returned if the datatype is atomic (i.e. not compound), BFALSE is
    returned if the datatype is compound, BFAIL on error.
--------------------------------------------------------------------------*/
hbool_t H5Tis_field_atomic(hatom_t tid, uintn fidx)
{
    h5_datatype_t *dt;         /* new data-type object to create */
    hbool_t        ret_value = BTRUE;

    FUNC_ENTER(H5Tis_field_atomic, H5T_init_interface, BFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Go get the object */
    if((dt=H5Aatom_object(tid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Check the base type of the datatype */
    if(H5T_COMPOUND!=dt->dt.base)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);
    
    /* Check if the compound information has been initialized */
    if(NULL==dt->ci)
        HGOTO_ERROR(H5E_INTERNAL, H5E_UNINITIALIZED, FAIL);

    /* Check if the field is valid*/
    if(fidx>=dt->ci->n)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL);

    /* Check the base type of the field */
    if(H5T_COMPOUND==dt->ci->flist[fidx].dt.base || H5P_SCALAR!=dt->ci->flist[fidx].dim_id)
        ret_value=BFALSE;

done:
  if(ret_value == BFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Tis_field_atomic() */

/*--------------------------------------------------------------------------
 NAME
    H5T_is_atomic
 PURPOSE
    Check if a datatype is atomic (internal)
 USAGE
    hbool_t H5Tis_atomic(type)
        h5_datatype_t *type;            IN: Ptr to datatype object to query
 RETURNS
    BFAIL/BTRUE/BFALSE
 DESCRIPTION
        This function checks the basic type of a user-defined datatype.  BTRUE
    is returned if the datatype is atomic (i.e. not compound), BFALSE is
    returned if the datatype is compound, BFAIL on error.
--------------------------------------------------------------------------*/
hbool_t H5T_is_atomic(h5_datatype_t *type)
{
    hbool_t        ret_value = BTRUE;

    FUNC_ENTER(H5T_is_atomic, H5T_init_interface, BFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Check the base type of the datatype */
    if(H5T_COMPOUND==type->dt.base)
        ret_value=BFALSE;

#ifdef LATER
done:
#endif /* LATER */
  if(ret_value == BFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5T_is_atomic() */

/*--------------------------------------------------------------------------
 NAME
    H5Tis_atomic
 PURPOSE
    Check if a datatype is atomic (API)
 USAGE
    hbool_t H5Tis_atomic(tid)
        hatom_t tid;            IN: Datatype object to query
 RETURNS
    BFAIL/BTRUE/BFALSE
 DESCRIPTION
        This function checks the basic type of a user-defined datatype.  BTRUE
    is returned if the datatype is atomic (i.e. not compound), BFALSE is
    returned if the datatype is compound, BFAIL on error.
--------------------------------------------------------------------------*/
hbool_t H5Tis_atomic(hatom_t tid)
{
    h5_datatype_t *dt;         /* new data-type object to create */
    hbool_t        ret_value = BTRUE;

    FUNC_ENTER(H5Tis_atomic, H5T_init_interface, BFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Go get the object */
    if((dt=H5Aatom_object(tid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Check the base type of the datatype */
    ret_value=H5T_is_atomic(dt);

done:
  if(ret_value == BFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Tis_atomic() */

/*--------------------------------------------------------------------------
 NAME
    H5Tset_type
 PURPOSE
    Set the base type of a user-defined datatype 
 USAGE
    herr_t H5Tset_type(tid, base, len, arch)
        hatom_t tid;            IN: Datatype object to modify
        hatom_t base;           IN: Base type to set the datatype to
        uint8 len;              IN: Length of the object in bytes
        uint8 arch;             IN: Architecture format to store type with
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function sets the basic type of a user-defined datatype.  Each
    datatype is either an atomic type (i.e. has no further divisions of the
    type) or is a compound type (like a C structure).  If the datatype is set
    to a compound type, the 'len' argument is not used.
--------------------------------------------------------------------------*/
herr_t H5Tset_type(hatom_t tid,hatom_t base,uint8 len,uint8 arch)
{
    h5_datatype_t *dt;         /* new data-type object to create */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Tset_type, H5T_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(base<H5T_CHAR || base>H5T_COMPOUND)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

    /* Go get the object */
    if((dt=H5Aatom_object(tid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    if(dt->dt.base!=0)
        HGOTO_ERROR(H5E_FUNC, H5E_ALREADYINIT, FAIL);
    
    /* Set the basic datatype information */
    dt->dt.base=base;
    dt->dt.len=len;
    dt->dt.arch=arch;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Tset_type() */

/*--------------------------------------------------------------------------
 NAME
    H5Tget_type
 PURPOSE
    Get the base type of a datatype 
 USAGE
    herr_t H5Tget_type(tid, base, len, arch)
        hatom_t tid;            IN: Datatype object to modify
        hatom_t *base;          IN: Base type of the datatype
        uint8 *len;             IN: Length of the object in bytes
        uint8 *arch;            IN: Architecture format type stored with
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function gets the basic type of a user-defined datatype.  Each
    datatype is either an atomic type (i.e. has no further divisions of the
    type) or is a compound type (like a C structure).  If the datatype is 
    to a compound type, the 'len' argument is not used.
--------------------------------------------------------------------------*/
herr_t H5Tget_type(hatom_t tid,hatom_t *base,uint8 *len,uint8 *arch)
{
    h5_datatype_t *dt;         /* new data-type object to create */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5Tget_type, H5T_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(H5Aatom_group(tid)!=H5_DATATYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);

    /* Go get the object */
    if((dt=H5Aatom_object(tid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    if(dt->dt.base==0)
        HGOTO_ERROR(H5E_FUNC, H5E_UNINITIALIZED, FAIL);
    
    /* Set the basic datatype information */
    if(base!=NULL)
        *base=dt->dt.base;
    if(len!=NULL)
        *len=dt->dt.len;
    if(arch!=NULL)
        *arch=dt->dt.arch;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Tget_type() */

/*--------------------------------------------------------------------------
 NAME
    H5Tadd_field
 PURPOSE
    Add a field to a compound datatype
 USAGE
    herr_t H5Tadd_field(tid, name, base, len, arch, space)
        hatom_t tid;            IN: Datatype object to query
        const char *fidx;       IN: Field name
        hatom_t base;           IN: Field's base type, either an atom ID for
                                    an existing compound type, or an atomic
                                    base type
        uint8 len;              IN: Length of an atomic base type
        uint8 arch;             IN: Architecture format of an atomic base type
        hatom_t space;          IN: The dimensionality of the field to add
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function adds a field to a user-defined compound datatype.  The
    field can either be a base/len/arch triplet or an existing compound type
    (passed in the base argument).  The space parameter is either H5P_SCALAR
    (to indicate a scalar field) or the atom of a datatype for more complex
    dimensionality fields.
--------------------------------------------------------------------------*/
herr_t H5Tadd_field(hatom_t tid, const char *name, hatom_t base, uint8 len, uint8 arch, hatom_t space)
{
    h5_field_info_t *new_field; /* pointer to new field to add */
    h5_datatype_t *dt;          /* data-type object to manipulate */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER(H5Tadd_field, H5T_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Go get the object */
    if((dt=H5Aatom_object(tid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

    /* Check the base type of the datatype */
    if(H5T_COMPOUND!=dt->dt.base)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);
    
    /* Check if the compound information has been initialized */
    if(NULL==dt->ci)
      {
        if(NULL==(dt->ci=HDmalloc(sizeof(h5_compound_info_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
        dt->ci->n=0;            /* Set the number of fields to 0 */
        dt->ci->mem_size=0;     /* Set the size of the structure */
        dt->ci->disk_size=0;    /* Set the size of the structure */
        dt->ci->flist=NULL;     /* No field information yet */
      } /* end if */

    if(NULL==(new_field=HDrealloc(dt->ci->flist,(dt->ci->n+1)*sizeof(h5_field_info_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL);
    dt->ci->n++;    /* increment the number of fields */
    dt->ci->flist=new_field;    /* save the pointer to the increased array of fields */
    new_field=&dt->ci->flist[dt->ci->n-1];   /* get a "convenience" pointer to the new field */

    new_field->name=HDstrdup(name); /* copy the name */
    new_field->name_off=0;          /* name isn't stored yet */
    new_field->struct_off=dt->ci->disk_size;    /* Set the offset of the field on disk */
    new_field->dim_id=H5Mcopy(space);    /* Make a copy of the dimension space for the field */
    if((H5Ais_reserved(base)==BTRUE) && base!=H5T_COMPOUND) /* Check if this is a "simple" datatype */
      {
        new_field->dt.base=base;        /* Make a copy of the datatype for the field */
        new_field->dt.len=len;
        new_field->dt.arch=arch;
      } /* end if */
    else
      {
        new_field->dt.base=H5Mcopy(base);    /* Make a copy of the datatype for the field */
        new_field->dt.len=H5Tsize(base,len,arch,BTRUE);
        new_field->dt.arch=arch;
      } /* end else */

done:
  if(ret_value == FAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Tadd_field() */

/*--------------------------------------------------------------------------
 NAME
    H5Tsize
 PURPOSE
    Determine the size of a datatype
 USAGE
    uintn H5Tsize(tid, mem_flag)
        hatom_t tid;            IN: Datatype object to query
        hbool_t mem_flag;       IN: Whether the memory or disk size is desired
 RETURNS
    The size of the datatype on success or UFAIL on failure.
 DESCRIPTION
        Ths function returns the size of the datatype in bytes as it is stored
    on disk or in memory, depending on the mem_flag.  Setting the mem_flag to
    BTRUE returns the size in memory, BFALSE returns the size on disk.
--------------------------------------------------------------------------*/
uintn H5Tsize(hatom_t tid, uint8 len, uint8 arch, hbool_t mem_flag)
{
    uintn ret_value = UFAIL;

    FUNC_ENTER(H5Tsize, H5T_init_interface, UFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    if((H5Ais_reserved(tid)==BTRUE) && tid!=H5T_COMPOUND) /* Check if this is a "simple" datatype */
      {
        switch(tid)
          {
            case H5T_CHAR:
            case H5T_INT:
            case H5T_FLOAT:     /* All three of thes types use the length as the number of bytes */
                ret_value=len;
                break;

            case H5T_DATE:
                ret_value=8;    /* Number of characters for ISO 8601 format */
                break;

            case H5T_TIME:
                ret_value=6;    /* Number of characters for ISO 8601 format */
                break;

            case H5T_SPTR:
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, UFAIL);
                break;

            case H5T_PPTR:
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, UFAIL);
                break;

            default:
                HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, UFAIL);
          } /* end switch */
      } /* end if */
    else
      {
        h5_datatype_t *dt;  /* datatype pointer */

        /* Go get the object */
        if((dt=H5Aatom_object(tid))==NULL)
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);

        if(dt->dt.base==H5T_COMPOUND)
          {
            intn i;             /* local counting variable */

            /* Check the base type of the datatype */
            if(H5T_COMPOUND!=dt->dt.base)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL);
            
            /* Check if the compound information has been initialized */
            if(NULL==dt->ci)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNINITIALIZED, FAIL);

            /* Grab the number of fields */
            for(i=0; i<=dt->ci->n; i++)
                ret_value+=H5Tsize(dt->ci->flist[i].dt.base, dt->ci->flist[i].dt.len,
                        dt->ci->flist[i].dt.arch,mem_flag)*H5Pnelem(dt->ci->flist[i].dim_id);
          } /* end if */
        else
          { /* Simple, user-defined datatypes */
            switch(dt->dt.base)
              {
                case H5T_CHAR:
                case H5T_INT:
                case H5T_FLOAT:     /* All three of thes types use the length as the number of bytes */
                    ret_value=dt->dt.len;
                    break;

                case H5T_DATE:
                    ret_value=8;    /* Number of characters for ISO 8601 format */
                    break;

                case H5T_TIME:
                    ret_value=6;    /* Number of characters for ISO 8601 format */
                    break;

                case H5T_SPTR:
                    HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, UFAIL);
                    break;

                case H5T_PPTR:
                    HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, UFAIL);
                    break;

                default:
                    HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, UFAIL);
              } /* end switch */
          } /* end else */
      } /* end else */

done:
  if(ret_value == UFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Tsize() */

/*--------------------------------------------------------------------------
 NAME
    H5Tget_fields
 PURPOSE
    Determine the size of a datatype
 USAGE
    herr_t H5Tget_fields(tid, field_list)
        hatom_t tid;            IN: Datatype object to query
        hoid_t *field_list;     IN: Array to store list of fields
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        Ths function returns a list of OIDs for the fields in a compound
    datatype.  Atomic fields are returned in the list of OIDs, but have special
    OID values which cannot be further dereferenced.
--------------------------------------------------------------------------*/
herr_t H5Tget_fields(hatom_t tid, hatom_t *field_list)
{
    herr_t ret_value = FAIL;

    FUNC_ENTER(H5Tget_fields, H5T_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if(H5Aatom_group(tid)!=H5_DATATYPE)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    if(field_list==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);

done:
  if(ret_value == UFAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5Tget_fields() */

/*--------------------------------------------------------------------------
 NAME
    H5T_release
 PURPOSE
    Release access to an HDF5 datatype object.
 USAGE
    herr_t H5T_release(oid)
        hatom_t oid;       IN: Object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function releases a datatype from active use by a user.
--------------------------------------------------------------------------*/
herr_t H5T_release(hatom_t oid)
{
    h5_datatype_t *dt;         /* new data-type object to create */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5T_release, H5T_init_interface, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;

    /* Chuck the object! :-) */
    if((dt=H5Aremove_atom(oid))==NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
    if(dt->name!=NULL)
        HDfree(dt->name);
    if(dt->ci!=NULL)
      {
      } /* end if */
    HDfree(dt);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5T_release() */

