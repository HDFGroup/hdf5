/****************************************************************************
* NCSA HDF								                                    *
* Software Development Group						                        *
* National Center for Supercomputing Applications			                *
* University of Illinois at Urbana-Champaign				                *
* 605 E. Springfield, Champaign IL 61820				                    *
*									                                        *
* For conditions of distribution and use, see the accompanying		        *
* hdf/COPYING file.							                                *
*									                                        *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "$Revision$";
#endif

/* $Id$ */

#define H5A_PACKAGE		/*suppress error about including H5Apkg	  */

/* Private header files */
#include <H5private.h>		/* Generic Functions			*/
#include <H5Iprivate.h>		/* IDs			  	*/
#include <H5Bprivate.h>		/* B-tree subclass names	  	*/
#include <H5Dprivate.h>		    /* Datasets				*/
#include <H5Gprivate.h>		    /* Groups				*/
#include <H5Tprivate.h>		    /* Datatypes				*/
#include <H5Eprivate.h>		/* Error handling		  	*/
#include <H5MMprivate.h>	/* Memory management			*/
#include <H5Pprivate.h>		/* Property lists			*/
#include <H5Oprivate.h>     /* Object Headers       */
#include <H5Apkg.h>		    /* Attributes		*/

#define PABLO_MASK	H5A_mask

/* Is the interface initialized? */
static hbool_t		interface_initialize_g = FALSE;
#define INTERFACE_INIT H5A_init_interface
static herr_t		H5A_init_interface(void);

/* PRIVATE PROTOTYPES */
static void		H5A_term_interface(void);
static hid_t H5A_create(const H5G_entry_t *ent, const char *name, const H5T_t *type, const H5S_t *space);
static hid_t H5A_open(H5G_entry_t *ent, unsigned idx);
static herr_t H5A_write(H5A_t *attr, const H5T_t *mem_type, void *buf);
static herr_t H5A_read(H5A_t *attr, const H5T_t *mem_type, void *buf);
static int H5A_get_index(H5G_entry_t *ent, const char *name);


/*--------------------------------------------------------------------------
NAME
   H5A_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5A_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5A_init_interface(void)
{
    herr_t		    ret_value = SUCCEED;

    FUNC_ENTER(H5A_init_interface, FAIL);

    /*
     * Register cleanup function.
     */
    if ((ret_value = H5I_init_group(H5_ATTR, H5I_ATTRID_HASHSIZE,
				    H5A_RESERVED_ATOMS,
				    (herr_t (*)(void *)) H5A_close)) == FAIL) {
        HRETURN_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL,
		      "unable to initialize attribute group");
    }

    if (H5_add_exit(H5A_term_interface) < 0) {
	HRETURN_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL,
		      "unable to install atexit function");
    }
    
    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5A_term_interface
 PURPOSE
    Terminate various H5A objects
 USAGE
    void H5A_term_interface()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5A_term_interface(void)
{
}


/*--------------------------------------------------------------------------
 NAME
    H5Acreate
 PURPOSE
    Creates a dataset as an attribute of another dataset or group
 USAGE
    hid_t H5Acreate (loc_id, name, datatype, dataspace, create_plist)
        hid_t loc_id;       IN: Object (dataset or group) to be attached to
        const char *name;   IN: Name of attribute to create
        hid_t datatype;     IN: ID of datatype for attribute
        hid_t dataspace;    IN: ID of dataspace for attribute
        hid_t create_plist; IN: ID of creation property list (currently not used)
 RETURNS
    SUCCEED/FAIL
 
 ERRORS

 DESCRIPTION
        This function creates an attribute which is attached to the object
    specified with 'location_id'.  The name specified with 'name' for each
    attribute for an object must be unique for that object.  The 'type_id'
    and 'space_id' are created with the H5T and H5S interfaces respectively.
    Currently only simple dataspaces are allowed for attribute dataspaces.
    The 'create_plist_id' property list is currently un-used, but will be
    used int the future for optional properties of attributes.  The attribute
    ID returned from this function must be released with H5Aclose or resource
    leaks will develop.
        The link created (see H5G API documentation for more information on
    link types) is a hard link, so the attribute may be shared among datasets
    and will not be removed from the file until the reference count for the
    attribute is reduced to zero.
        The location object may be either a group or a dataset, both of
    which may have any sort of attribute.
--------------------------------------------------------------------------*/
hid_t
H5Acreate(hid_t loc_id, const char *name, hid_t datatype, hid_t dataspace,
    hid_t create_plist)
{
    void           *obj = NULL;
    H5G_entry_t    *ent = NULL;
    H5T_t		   *type = NULL;
    H5S_t		   *space = NULL;
    const H5D_create_t	   *create_parms = NULL;
    hid_t		    ret_value = FAIL;

    FUNC_ENTER(H5Acreate, FAIL);

    /* check arguments */
    if (!(H5_DATASET != H5I_group(loc_id) || H5_GROUP != H5I_group(loc_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "attribute target not a dataset or group");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    if (H5_DATATYPE != H5I_group(datatype) ||
	NULL == (type = H5I_object(datatype))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a type");
    }
    if (H5_DATASPACE != H5I_group(dataspace) ||
	NULL == (space = H5I_object(dataspace))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if (create_plist >= 0) {
	if (H5P_DATASET_CREATE != H5Pget_class(create_plist) ||
	    NULL == (create_parms = H5I_object(create_plist))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
			  "not a dataset creation property list");
	}
    } else {
	create_parms = &H5D_create_dflt;
    }

    /* Get the dataset or group's pointer */
    if(NULL == (obj = H5I_object(loc_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADATOM, FAIL, "illegal object atom");
    if (H5_DATASET == H5I_group(loc_id))
	ent = H5D_entof ((H5D_t*)obj);
    else
	ent = H5G_entof ((H5G_t*)obj);

    /* Go do the real work for attaching the attribute to the dataset */
    ret_value=H5A_create(ent,name,type,space);

    FUNC_LEAVE(ret_value);
} /* H5Acreate() */


/*-------------------------------------------------------------------------
 * Function:	H5A_create
 *
 * Purpose:	
 *      This is the guts of the H5Acreate function.
 * Usage:
 *  hid_t H5A_create (ent, name, type, space)
 *      const H5G_entry_t *ent;   IN: Pointer to symbol table entry for object to attribute
 *      const char *name;   IN: Name of attribute
 *      H5T_t *type;        IN: Datatype of attribute
 *      H5S_t *space;       IN: Dataspace of attribute
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		April 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5A_create(const H5G_entry_t *ent, const char *name, const H5T_t *type, const H5S_t *space)
{
    H5A_t       *attr = NULL;
    H5A_t       *found_attr = NULL;
    intn        seq=0;
    hid_t	    ret_value = FAIL;

    FUNC_ENTER(H5A_create, FAIL);

    /* check args */
    assert(ent);
    assert(name);
    assert(type);
    assert(space);

    /* Build the attribute information */
    if((attr = H5MM_xcalloc(1, sizeof(H5A_t)))==NULL)
        HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
		      "unable to allocate space for attribute info");
    attr->name=HDstrdup(name);
    attr->dt=H5T_copy(type);
    attr->ds=H5S_copy(space);

    /* Copy the symbol table entry */
    attr->ent=*ent;

    /* Compute the internal sizes */
    attr->dt_size=(H5O_DTYPE[0].raw_size)(attr->ent.file,type);
    attr->ds_size=(H5O_SDSPACE[0].raw_size)(attr->ent.file,&(space->u.simple));
    attr->data_size=H5S_get_npoints(space)*H5T_get_size(type);

    /* Hold the symbol table entry (and file) open */
    if (H5O_open(&(attr->ent)) < 0) {
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open");
    }
    attr->ent_opened=1;

    /* Read in the existing attributes to check for duplicates */
    seq=0;
    while((found_attr=H5O_read(&(attr->ent), H5O_ATTR, seq, NULL))!=NULL)
      {
        /* Compare found attribute name to new attribute name reject creation if names are the same */
        if(HDstrcmp(found_attr->name,attr->name)==0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTCREATE, FAIL,
                "attribute already exists");
        seq++;
      } /* end while */

    /* Create the attribute message and save the attribute index */
    if (H5O_modify(&(attr->ent), H5O_ATTR, H5O_NEW_MESG, 0, attr) < 0) 
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL,
            "can't update attribute header messages");

    /* Register the new attribute and get an ID for it */
    if ((ret_value = H5I_register(H5_ATTR, attr)) < 0) {
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		    "unable to register attribute for ID");
    }

done:
    if (ret_value < 0) {
        if(attr)
            H5A_close(attr);
    }

    FUNC_LEAVE(ret_value);
} /* H5A_create() */


/*--------------------------------------------------------------------------
 NAME
    H5A_get_index
 PURPOSE
    Determine the index of an attribute in an object header
 USAGE
    int H5A_get_index (ent, name)
        H5G_entry_t    *ent; IN: Symbol table entry of object
        const char *name;    IN: Name of dataset to find
 RETURNS
    non-negative on success, negative on failure
 
 ERRORS

 DESCRIPTION
        This function determines the index of the attribute within an object
    header.  This is not stored in the attribute structure because it is only
    a subjective measure and can change if attributes are deleted from the
    object header.
--------------------------------------------------------------------------*/
static int
H5A_get_index(H5G_entry_t *ent, const char *name)
{
    H5A_t      *found_attr = NULL;
    int		    ret_value = FAIL;

    FUNC_ENTER(H5A_get_index, FAIL);

    assert(ent);
    assert(name);

    /* Look up the attribute for the object */
    ret_value=0;
    while((found_attr=H5O_read(ent, H5O_ATTR, ret_value, NULL))!=NULL)
      {
          /* Compare found attribute name to new attribute name reject creation if names are the same */
          if(HDstrcmp(found_attr->name,name)==0)
              break;
          ret_value++;
      } /* end while */
    if(found_attr==NULL) {
        HRETURN_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL,
		      "attribute not found");
      }

    FUNC_LEAVE(ret_value);
} /* H5A_get_index() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen_name
 PURPOSE
    Opens an attribute for an object by looking up the attribute name
 USAGE
    hid_t H5Aopen_name (loc_id, name)
        hid_t loc_id;       IN: Object (dataset or group) to be attached to
        const char *name;   IN: Name of attribute to locate and open
 RETURNS
    ID of attribute on success, negative on failure
 
 ERRORS

 DESCRIPTION
        This function opens an existing attribute for access.  The attribute
    name specified is used to look up the corresponding attribute for the
    object.  The attribute ID returned from this function must be released with
    H5Aclose or resource leaks will develop.
        The location object may be either a group or a dataset, both of
    which may have any sort of attribute.
--------------------------------------------------------------------------*/
hid_t
H5Aopen_name(hid_t loc_id, const char *name)
{
    H5G_entry_t    *ent = NULL;     /* Symbol table entry of object to attribute */
    void           *obj = NULL;
    intn            idx=0;
    hid_t		    ret_value = FAIL;

    FUNC_ENTER(H5Aopen_name, FAIL);

    /* check arguments */
    if (!(H5_DATASET != H5I_group(loc_id) || H5_GROUP != H5I_group(loc_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "attribute target not a dataset or group");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }

    /* Get the dataset or group's pointer */
    if(NULL == (obj = H5I_object(loc_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADATOM, FAIL, "illegal object atom");

    /* Copy the object header entry for the object */
    if (H5_DATASET == H5I_group(loc_id))
	ent = H5D_entof ((H5D_t*)obj);
    else
	ent = H5G_entof ((H5G_t*)obj);

    /* Look up the attribute for the object */
    if((idx=H5A_get_index(ent,name))<0)
        HRETURN_ERROR(H5E_ATTR, H5E_BADVALUE, FAIL, "attribute not found");

    /* Go do the real work for opening the attribute */
    ret_value=H5A_open(ent, (unsigned)idx);

    FUNC_LEAVE(ret_value);
} /* H5Aopen_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen_idx
 PURPOSE
    Opens the n'th attribute for an object
 USAGE
    hid_t H5Aopen_idx (loc_id, idx)
        hid_t loc_id;       IN: Object (dataset or group) to be attached to
        unsigned idx;       IN: Index (0-based) attribute to open
 RETURNS
    ID of attribute on success, negative on failure
 
 ERRORS

 DESCRIPTION
        This function opens an existing attribute for access.  The attribute
    index specified is used to look up the corresponding attribute for the
    object.  The attribute ID returned from this function must be released with
    H5Aclose or resource leaks will develop.
        The location object may be either a group or a dataset, both of
    which may have any sort of attribute.
--------------------------------------------------------------------------*/
hid_t
H5Aopen_idx(hid_t loc_id, unsigned idx)
{
    H5G_entry_t    *ent = NULL;     /* Symbol table entry of object to attribute */
    void           *obj = NULL;
    hid_t		    ret_value = FAIL;

    FUNC_ENTER(H5Aopen_idx, FAIL);

    /* check arguments */
    if (!(H5_DATASET != H5I_group(loc_id) || H5_GROUP != H5I_group(loc_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "attribute target not a dataset or group");
    }

    /* Get the dataset or group's pointer */
    if(NULL == (obj = H5I_object(loc_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADATOM, FAIL, "illegal object atom");

    /* Copy the object header entry for the object */
    if (H5_DATASET == H5I_group(loc_id))
	ent = H5D_entof ((H5D_t*)obj);
    else
	ent = H5G_entof ((H5G_t*)obj);

    /* Go do the real work for opening the attribute */
    ret_value=H5A_open(ent, idx);

    FUNC_LEAVE(ret_value);
} /* H5Aopen_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5A_open
 *
 * Purpose:	
 *      This is the guts of the H5Aopen_xxx functions
 * Usage:
 *  herr_t H5A_open (ent, idx)
 *      const H5G_entry_t *ent;   IN: Pointer to symbol table entry for object to attribute
 *      unsigned idx;       IN: index of attribute to open (0-based)
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		April 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5A_open(H5G_entry_t *ent, unsigned idx)
{
    H5A_t       *attr = NULL;
    hid_t	    ret_value = FAIL;

    FUNC_ENTER(H5A_open, FAIL);

    /* check args */
    assert(ent);

    /* Build the attribute information */
    if((attr = H5MM_xcalloc(1, sizeof(H5A_t)))==NULL)
        HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
		      "unable to allocate space for attribute info");

    /* Read in attribute with H5O_read() */
    if (NULL==(H5O_read(ent, H5O_ATTR, idx, attr))) {
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL,
		    "unable to load attribute info from dataset header");
    }

    /* Copy the symbol table entry */
    attr->ent=*ent;

    /* Hold the symbol table entry (and file) open */
    if (H5O_open(&(attr->ent)) < 0) {
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open");
    }
    attr->ent_opened=1;

    /* Register the new attribute and get an ID for it */
    if ((ret_value = H5I_register(H5_ATTR, attr)) < 0) {
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		    "unable to register attribute for ID");
    }

done:
    if (ret_value < 0) {
        if(attr)
            H5A_close(attr);
    }

    FUNC_LEAVE(ret_value);
} /* H5A_open() */


/*--------------------------------------------------------------------------
 NAME
    H5Awrite
 PURPOSE
    Write out data to an attribute
 USAGE
    herr_t H5Awrite (attr_id, mem_dt, buf)
        hid_t attr_id;       IN: Attribute to write
        hid_t mem_dt;        IN: Memory datatype of buffer
        void *buf;           IN: Buffer of data to write
 RETURNS
    SUCCEED/FAIL
 
 ERRORS

 DESCRIPTION
        This function writes a complete attribute to disk.
--------------------------------------------------------------------------*/
herr_t
H5Awrite(hid_t attr_id, hid_t mem_dt, void *buf)
{
    H5A_t		   *attr = NULL;
    const H5T_t    *mem_type = NULL;
    herr_t		    ret_value = FAIL;

    FUNC_ENTER(H5Awrite, FAIL);

    /* check arguments */
    if (H5_ATTR != H5I_group(attr_id) ||
            (NULL == (attr = H5I_object(attr_id)))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute");
    }
    if (H5_DATATYPE != H5I_group(mem_dt) ||
            NULL == (mem_type = H5I_object(mem_dt))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (NULL == buf) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "null attribute buffer");
    }

    /* Go write the actual data to the attribute */
    if ((ret_value=H5A_write(attr,mem_type,buf))<0) {
        HRETURN_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL,
            "can't write attribute");
    }

    FUNC_LEAVE(ret_value);
} /* H5Awrite() */


/*--------------------------------------------------------------------------
 NAME
    H5A_write
 PURPOSE
    Actually write out data to an attribute
 USAGE
    herr_t H5A_write (attr, mem_type, buf)
        H5A_t *attr;         IN: Attribute to write
        const H5T_t *mem_type;     IN: Memory datatype of buffer
        void *buf;           IN: Buffer of data to write
 RETURNS
    SUCCEED/FAIL
 
 ERRORS

 DESCRIPTION
    This function writes a complete attribute to disk.
--------------------------------------------------------------------------*/
static herr_t
H5A_write(H5A_t *attr, const H5T_t *mem_type, void *buf)
{
    uint8		*tconv_buf = NULL;	/* data type conv buffer	*/
    size_t		    nelmts;		    /* elements in attribute	*/
    H5T_conv_t		tconv_func = NULL;	/* conversion function	*/
    H5T_cdata_t		*cdata = NULL;		/* type conversion data	*/
    hid_t		src_id = -1, dst_id = -1;/* temporary type atoms */
    size_t		src_type_size;		/* size of source type	*/
    size_t		dst_type_size;		/* size of destination type*/
    size_t		buf_size;		    /* desired buffer size	*/
    int         idx;                /* index of attribute in object header */
    herr_t		    ret_value = FAIL;
#ifdef H5T_DEBUG
    H5_timer_t		timer;
#endif

    FUNC_ENTER(H5A_write, FAIL);

    assert(attr);
    assert(mem_type);
    assert(buf);


    /* Create buffer for data to store on disk */
    nelmts=H5S_get_npoints (attr->ds);

    /* Get the memory and file datatype sizes */
    src_type_size = H5T_get_size(mem_type);
    dst_type_size = H5T_get_size(attr->dt);

    /* Get the maximum buffer size needed and allocate it */
    buf_size = nelmts*MAX(src_type_size,dst_type_size);
	tconv_buf = H5MM_xmalloc (buf_size);

    /* Copy the user's data into the buffer for conversion */
    HDmemcpy(tconv_buf,buf,src_type_size*nelmts);

/* Convert memory buffer into disk buffer */
    /* Set up type conversion function */
    if (NULL == (tconv_func = H5T_find(mem_type, attr->dt,
				       H5T_BKG_NO, &cdata))) {
        HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL,
		    "unable to convert between src and dest data types");
    } else if (H5T_conv_noop!=tconv_func) {
        if ((src_id = H5I_register(H5_DATATYPE, H5T_copy(mem_type)))<0 ||
                (dst_id = H5I_register(H5_DATATYPE, H5T_copy(attr->dt)))<0) {
            HGOTO_ERROR(H5E_ATTR, H5E_CANTREGISTER, FAIL,
			"unable to register types for conversion");
        }
    }

	/* Perform data type conversion.  */
#ifdef H5T_DEBUG
    H5T_timer_begin (&timer, cdata);
#endif
	cdata->command = H5T_CONV_CONV;
	if ((tconv_func) (src_id, dst_id, cdata, nelmts, tconv_buf, NULL)<0) {
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL,
			"data type conversion failed");
	}
#ifdef H5T_DEBUG
    H5T_timer_end (&timer, cdata, nelmts);
#endif

    /* Free the previous attribute data buffer, if there is one */
    if(attr->data)
        H5MM_xfree(attr->data);

    /* Look up the attribute for the object */
    if((idx=H5A_get_index(&(attr->ent),attr->name))<0)
        HGOTO_ERROR(H5E_ATTR, H5E_BADVALUE, FAIL, "attribute not found");

    /* Modify the attribute data */
    attr->data=tconv_buf;   /* Set the data pointer temporarily */
    if (H5O_modify(&(attr->ent), H5O_ATTR, idx, 0, attr) < 0) 
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL,
            "can't update attribute header messages");
    attr->data=NULL;    /* un-do the data pointer */

    /* Indicate the the attribute doesn't need fill-values */
    attr->initialized=TRUE;

    ret_value=SUCCEED;

done:
    /* Release resources */
    if (src_id >= 0) 
        H5I_dec_ref(src_id);
    if (dst_id >= 0) 
        H5I_dec_ref(dst_id);
    if (tconv_buf)
        H5MM_xfree(tconv_buf);

    FUNC_LEAVE(ret_value);
} /* H5A_write() */


/*--------------------------------------------------------------------------
 NAME
    H5Aread
 PURPOSE
    Read in data from an attribute
 USAGE
    herr_t H5Aread (attr_id, mem_dt, buf)
        hid_t attr_id;       IN: Attribute to read
        hid_t mem_dt;        IN: Memory datatype of buffer
        void *buf;           IN: Buffer for data to read
 RETURNS
    SUCCEED/FAIL
 
 ERRORS

 DESCRIPTION
        This function reads a complete attribute from disk.
--------------------------------------------------------------------------*/
herr_t
H5Aread(hid_t attr_id, hid_t mem_dt, void *buf)
{
    H5A_t		   *attr = NULL;
    const H5T_t    *mem_type = NULL;
    herr_t		    ret_value = FAIL;

    FUNC_ENTER(H5Aread, FAIL);

    /* check arguments */
    if (H5_ATTR != H5I_group(attr_id) ||
            (NULL == (attr = H5I_object(attr_id)))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute");
    }
    if (H5_DATATYPE != H5I_group(mem_dt) ||
            NULL == (mem_type = H5I_object(mem_dt))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (NULL == buf) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "null attribute buffer");
    }

    /* Go write the actual data to the attribute */
    if ((ret_value=H5A_read(attr,mem_type,buf))<0) {
        HRETURN_ERROR(H5E_ATTR, H5E_READERROR, FAIL,
            "can't read attribute");
    }

    FUNC_LEAVE(ret_value);
} /* H5Aread() */


/*--------------------------------------------------------------------------
 NAME
    H5A_read
 PURPOSE
    Actually read in data from an attribute
 USAGE
    herr_t H5A_read (attr, mem_type, buf)
        H5A_t *attr;         IN: Attribute to read
        const H5T_t *mem_type;     IN: Memory datatype of buffer
        void *buf;           IN: Buffer for data to read
 RETURNS
    SUCCEED/FAIL
 
 ERRORS

 DESCRIPTION
    This function read a complete attribute to disk.
--------------------------------------------------------------------------*/
static herr_t
H5A_read(H5A_t *attr, const H5T_t *mem_type, void *buf)
{
    uint8		*tconv_buf = NULL;	/* data type conv buffer	*/
    size_t		    nelmts;		    /* elements in attribute	*/
    H5T_conv_t		tconv_func = NULL;	/* conversion function	*/
    H5T_cdata_t		*cdata = NULL;		/* type conversion data	*/
    hid_t		src_id = -1, dst_id = -1;/* temporary type atoms */
    size_t		src_type_size;		/* size of source type	*/
    size_t		dst_type_size;		/* size of destination type*/
    size_t		buf_size;		    /* desired buffer size	*/
    herr_t		    ret_value = FAIL;
#ifdef H5T_DEBUG
    H5_timer_t		timer;
#endif

    FUNC_ENTER(H5A_read, FAIL);

    assert(attr);
    assert(mem_type);
    assert(buf);


    /* Create buffer for data to store on disk */
    nelmts=H5S_get_npoints (attr->ds);

    /* Get the memory and file datatype sizes */
    src_type_size = H5T_get_size(attr->dt);
    dst_type_size = H5T_get_size(mem_type);

    /* Get the maximum buffer size needed and allocate it */
    buf_size = nelmts*MAX(src_type_size,dst_type_size);
	tconv_buf = H5MM_xmalloc (buf_size);

    /* Copy the attribute data into the buffer for conversion */
    HDmemcpy(tconv_buf,attr->data,src_type_size*nelmts);

/* Convert memory buffer into disk buffer */
    /* Set up type conversion function */
    if (NULL == (tconv_func = H5T_find(attr->dt, mem_type,
				       H5T_BKG_NO, &cdata))) {
        HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL,
		    "unable to convert between src and dest data types");
    } else if (H5T_conv_noop!=tconv_func) {
        if ((src_id = H5I_register(H5_DATATYPE, H5T_copy(attr->dt)))<0 ||
                (dst_id = H5I_register(H5_DATATYPE, H5T_copy(mem_type)))<0) {
            HGOTO_ERROR(H5E_ATTR, H5E_CANTREGISTER, FAIL,
			"unable to register types for conversion");
        }
    }

	/* Perform data type conversion.  */
#ifdef H5T_DEBUG
    H5T_timer_begin (&timer, cdata);
#endif
	cdata->command = H5T_CONV_CONV;
	if ((tconv_func) (src_id, dst_id, cdata, nelmts, tconv_buf, NULL)<0) {
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL,
			"data type conversion failed");
	}
#ifdef H5T_DEBUG
    H5T_timer_end (&timer, cdata, nelmts);
#endif

    /* Copy the converted data into the user's buffer */
    HDmemcpy(buf,tconv_buf,dst_type_size*nelmts);


    ret_value=SUCCEED;

done:
    /* Release resources */
    if (src_id >= 0) 
        H5I_dec_ref(src_id);
    if (dst_id >= 0) 
        H5I_dec_ref(dst_id);
    if (tconv_buf)
        H5MM_xfree(tconv_buf);

    FUNC_LEAVE(ret_value);
} /* H5A_read() */


/*--------------------------------------------------------------------------
 NAME
    H5Aget_space
 PURPOSE
    Gets a copy of the dataspace for an attribute
 USAGE
    hid_t H5Aget_space (attr_id)
        hid_t attr_id;       IN: Attribute to get dataspace of
 RETURNS
    A dataspace ID on success, negative on failure
 
 ERRORS

 DESCRIPTION
        This function retrieves a copy of the dataspace for an attribute.
    The dataspace ID returned from this function must be released with H5Sclose
    or resource leaks will develop.
--------------------------------------------------------------------------*/
hid_t
H5Aget_space(hid_t attr_id)
{
    H5A_t		   *attr = NULL;
    H5S_t	*dst = NULL;
    hid_t		        ret_value = FAIL;

    FUNC_ENTER(H5Aget_space, FAIL);

    /* check arguments */
    if (H5_ATTR != H5I_group(attr_id) ||
            (NULL == (attr = H5I_object(attr_id)))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute");
    }

    /* Copy the attribute's dataspace */
    if (NULL==(dst=H5S_copy (attr->ds))) {
	HRETURN_ERROR (H5E_ATTR, H5E_CANTINIT, FAIL,
		       "unable to copy dataspace");
    }

    /* Atomize */
    if ((ret_value=H5I_register (H5_DATASPACE, dst))<0) {
        HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register dataspace atom");
    }

    FUNC_LEAVE(ret_value);
} /* H5Aget_space() */


/*--------------------------------------------------------------------------
 NAME
    H5Aget_type
 PURPOSE
    Gets a copy of the datatype for an attribute
 USAGE
    hid_t H5Aget_type (attr_id)
        hid_t attr_id;       IN: Attribute to get datatype of
 RETURNS
    A datatype ID on success, negative on failure
 
 ERRORS

 DESCRIPTION
        This function retrieves a copy of the datatype for an attribute.
    The datatype ID returned from this function must be released with H5Tclose
    or resource leaks will develop.
--------------------------------------------------------------------------*/
hid_t
H5Aget_type(hid_t attr_id)
{
    H5A_t		   *attr = NULL;
    H5T_t	*dst = NULL;
    hid_t		        ret_value = FAIL;

    FUNC_ENTER(H5Aget_type, FAIL);

    /* check arguments */
    if (H5_ATTR != H5I_group(attr_id) ||
            (NULL == (attr = H5I_object(attr_id)))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute");
    }

    /* Copy the attribute's dataspace */
    if (NULL==(dst=H5T_copy (attr->dt))) {
	HRETURN_ERROR (H5E_ATTR, H5E_CANTINIT, FAIL,
		       "unable to copy datatype");
    }

    /* Atomize */
    if ((ret_value=H5I_register (H5_DATATYPE, dst))<0) {
        HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register datatype atom");
    }

    FUNC_LEAVE(ret_value);
} /* H5Aget_type() */


/*--------------------------------------------------------------------------
 NAME
    H5Aget_name
 PURPOSE
    Gets a copy of the name for an attribute
 USAGE
    size_t H5Aget_name (attr_id, buf, buf_size)
        hid_t attr_id;      IN: Attribute to get name of
        char *buf;          IN: Buffer to store name in
        size_t buf_size;    IN: The size of the buffer to store the string in.
 RETURNS
    This function returns the length of the attribute's name (which may be
    longer than 'buf_size') on success or negative for failure.
 
 ERRORS

 DESCRIPTION
        This function retrieves the name of an attribute for an attribute ID.
    Up to 'buf_size' characters are stored in 'buf' followed by a '\0' string
    terminator.  If the name of the attribute is longer than 'buf_size'-1,
    the string terminator is stored in the last position of the buffer to
    properly terminate the string.
--------------------------------------------------------------------------*/
size_t
H5Aget_name(hid_t attr_id, char *buf, size_t buf_size)
{
    H5A_t		   *attr = NULL;
    size_t              copy_len=0;
    size_t		        ret_value = FAIL;

    FUNC_ENTER(H5Aget_name, FAIL);

    /* check arguments */
    if (H5_ATTR != H5I_group(attr_id) ||
            (NULL == (attr = H5I_object(attr_id)))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute");
    }
    if (!buf || buf_size==0) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid buffer");
    }

    /* get the real attribute length */
    ret_value=HDstrlen(attr->name);

    /* compute the string length which will fit into the user's buffer */
    copy_len=MIN(buf_size-1,ret_value);

    /* Copy all/some of the name */
    HDmemcpy(buf,attr->name,copy_len);

    /* Terminate the string */
    buf[copy_len]='\0';

    FUNC_LEAVE(ret_value);
} /* H5Aget_type() */


/*--------------------------------------------------------------------------
 NAME
    H5Anum_attrs
 PURPOSE
    Determines the number of attributes attached to an object
 USAGE
    int H5Anum_attrs (loc_id)
        hid_t loc_id;       IN: Object (dataset or group) to be queried
 RETURNS
    Number of attributes on success, negative on failure
 
 ERRORS

 DESCRIPTION
        This function returns the number of attributes attached to a dataset or
    group, 'location_id'.
--------------------------------------------------------------------------*/
int
H5Anum_attrs(hid_t loc_id)
{
    H5G_entry_t    *ent = NULL;     /* Symbol table entry of object to attribute */
    void           *obj = NULL;
    int		        ret_value = 0;

    FUNC_ENTER(H5Anum_attrs, FAIL);

    /* check arguments */
    if (!(H5_DATASET != H5I_group(loc_id) || H5_GROUP != H5I_group(loc_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "attribute target not a dataset or group");
    }

    /* Get the dataset or group's pointer */
    if(NULL == (obj = H5I_object(loc_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADATOM, FAIL, "illegal object atom");

    /* Copy the object header entry for the object */
    if (H5_DATASET == H5I_group(loc_id))
	ent = H5D_entof ((H5D_t*)obj);
    else
	ent = H5G_entof ((H5G_t*)obj);

    /* Look up the attribute for the object */
    ret_value=H5O_count(ent, H5O_ATTR);

    FUNC_LEAVE(ret_value);
} /* H5Anum_attrs() */


/*--------------------------------------------------------------------------
 NAME
    H5Aiterate
 PURPOSE
    Calls a user's function for each attribute on an object
 USAGE
    herr_t H5Aiterate (loc_id, attr_num, op, data)
        hid_t loc_id;       IN: Object (dataset or group) to be iterated over
        unsigned *attr_num; IN/OUT: Starting (IN) & Ending (OUT) attribute number
        H5A_operator_t op;  IN: User's function to pass each attribute to
        void *op_data;      IN/OUT: User's data to pass through to iterator operator function
 RETURNS
        Returns a negative value if something is wrong, the return value of the
    last operator if it was non-zero, or zero if all attributes were processed.
 
 ERRORS

 DESCRIPTION
        This function interates over the attributes of dataset or group
    specified with 'loc_id'.  For each attribute of the object, the
    'op_data' and some additional information (specified below) are passed
    to the 'op' function.  The iteration begins with the '*attr_number'
    object in the group and the next attribute to be processed by the operator
    is returned in '*attr_number'.
        The operation receives the ID for the group or dataset being iterated
    over ('loc_id'), the name of the current attribute about the object
    ('attr_name') and the pointer to the operator data passed in to H5Aiterate
    ('op_data').  The return values from an operator are:
        A. Zero causes the iterator to continue, returning zero when all 
            attributes have been processed.
        B. Positive causes the iterator to immediately return that positive
            value, indicating short-circuit success.  The iterator can be
            restarted at the next attribute.
        C. Negative causes the iterator to immediately return that value,
            indicating failure.  The iterator can be restarted at the next
            attribute.
--------------------------------------------------------------------------*/
int
H5Aiterate(hid_t loc_id, unsigned *attr_num, H5A_operator_t op, void *op_data)
{
    H5G_entry_t    *ent = NULL;     /* Symbol table entry of object to attribute */
    void           *obj = NULL;
    H5A_t          *found_attr = NULL;
    int		        ret_value = 0;

    FUNC_ENTER(H5Anum_attrs, FAIL);

    /* check arguments */
    if (!(H5_DATASET != H5I_group(loc_id) || H5_GROUP != H5I_group(loc_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "attribute target not a dataset or group");
    }
    if (!attr_num) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index");
    }
    if (!op) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid operator");
    }

    /* Get the dataset or group's pointer */
    if(NULL == (obj = H5I_object(loc_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADATOM, FAIL, "illegal object atom");

    /* Copy the object header entry for the object */
    if (H5_DATASET == H5I_group(loc_id))
	ent = H5D_entof ((H5D_t*)obj);
    else
	ent = H5G_entof ((H5G_t*)obj);

    /* Look up the attribute for the object */
    if((int)*attr_num<H5O_count(ent, H5O_ATTR))   /* Make certain the start point is reasonable */
        while((found_attr=H5O_read(ent, H5O_ATTR, *attr_num, NULL))!=NULL)
          {
              /* Compare found attribute name to new attribute name reject creation if names are the same */
              (*attr_num)++;
              if((ret_value=op(loc_id,found_attr->name,op_data))!=0)
                  break;
          } /* end while */

    FUNC_LEAVE(ret_value);
} /* H5Aiterate() */


/*--------------------------------------------------------------------------
 NAME
    H5Adelete
 PURPOSE
    Deletes an attribute from a location
 USAGE
    herr_t H5Adelete (loc_id, name)
        hid_t loc_id;       IN: Object (dataset or group) to have attribute deleted from
        const char *name;   IN: Name of attribute to delete
 RETURNS
    0 on success, negative on failure
 
 ERRORS

 DESCRIPTION
        This function removes the named attribute from a dataset or group.
    This function should not be used when attribute IDs are open on 'loc_id'
    as it may cause the internal indexes of the attributes to change and future 
    writes to the open attributes to produce incorrect results.
--------------------------------------------------------------------------*/
herr_t
H5Adelete(hid_t loc_id, const char *name)
{
    H5A_t          *found_attr = NULL;
    H5G_entry_t    *ent = NULL;     /* Symbol table entry of object to attribute */
    void           *obj = NULL;
    intn            idx=0;
    hid_t		    ret_value = FAIL;

    FUNC_ENTER(H5Aopen_name, FAIL);

    /* check arguments */
    if (!(H5_DATASET != H5I_group(loc_id) || H5_GROUP != H5I_group(loc_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "attribute target not a dataset or group");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }

    /* Get the dataset or group's pointer */
    if(NULL == (obj = H5I_object(loc_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADATOM, FAIL, "illegal object atom");

    /* Copy the object header entry for the object */
    if (H5_DATASET == H5I_group(loc_id))
	ent = H5D_entof ((H5D_t*)obj);
    else
	ent = H5G_entof ((H5G_t*)obj);

    /* Look up the attribute for the object */
    idx=0;
    while((found_attr=H5O_read(ent, H5O_ATTR, idx, NULL))!=NULL)
      {
          /* Compare found attribute name to new attribute name reject creation if names are the same */
          if(HDstrcmp(found_attr->name,name)==0)
              break;
          idx++;
      } /* end while */
    if(found_attr==NULL) {
        HRETURN_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL,
		      "attribute not found");
      }

    /* Delete the attribute from the location */
    if ((ret_value=H5O_remove(ent, H5O_ATTR, idx)) < 0) 
        HRETURN_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL,
            "can't delete attribute header message");

    FUNC_LEAVE(ret_value);
} /* H5Adelete() */


/*--------------------------------------------------------------------------
 NAME
    H5Aclose
 PURPOSE
    Close an attribute ID
 USAGE
    herr_t H5Aclose (attr_id)
        hid_t attr_id;       IN: Attribute to release access to
 RETURNS
    SUCCEED/FAIL
 
 ERRORS

 DESCRIPTION
        This function releases an attribute from use.  Further use of the
    attribute ID will result in undefined behavior.
--------------------------------------------------------------------------*/
herr_t
H5Aclose(hid_t attr_id)
{
    H5A_t		   *attr = NULL;
    herr_t		    ret_value = FAIL;

    FUNC_ENTER(H5Aclose, FAIL);

    /* check arguments */
    if (H5_ATTR != H5I_group(attr_id) ||
            (NULL == (attr = H5I_object(attr_id)))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute");
    }

    /* Check if the attribute has any data yet, if not, fill with zeroes */
    if(!attr->initialized) {
        uint8 *tmp_buf=H5MM_xcalloc(1,attr->data_size);

        if (NULL == tmp_buf) {
            HRETURN_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL,
                "can't allocate attribute fill-value");
        }

        /* Go write the fill data to the attribute */
        if ((ret_value=H5A_write(attr,attr->dt,tmp_buf))<0) {
            HRETURN_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL,
                "can't write attribute");
        }

        /* Free temporary buffer */
        H5MM_xfree(tmp_buf);
    } /* end if */

    /* Free the memory used for the attribute */
    H5A_close(attr);
    ret_value=SUCCEED;

    FUNC_LEAVE(ret_value);
} /* H5Aclose() */


/*-------------------------------------------------------------------------
 * Function:	H5A_copy
 *
 * Purpose:	Copies attribute OLD_ATTR.
 *
 * Return:	Success:	Pointer to a new copy of the OLD_ATTR argument.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A_copy(const H5A_t *old_attr)
{
    H5A_t	*new_attr=NULL;

    FUNC_ENTER(H5A_copy, NULL);

    /* check args */
    assert(old_attr);

    /* get space */
    new_attr = H5MM_xcalloc(1, sizeof(H5A_t));

    /* Copy the top level of the attribute */
    *new_attr = *old_attr;

    /* Don't open the object header for a copy */
    new_attr->ent_opened=0;

    /* Copy the guts of the attribute */
    new_attr->name=HDstrdup(old_attr->name);
    new_attr->dt=H5T_copy(old_attr->dt);
    new_attr->ds=H5S_copy(old_attr->ds);
    if(old_attr->data) {
        new_attr->data=H5MM_xmalloc(old_attr->data_size);
        HDmemcpy(new_attr->data,old_attr->data,old_attr->data_size);
    } /* end if */

    /* Copy the share info? */
    
    FUNC_LEAVE(new_attr);
}


/*-------------------------------------------------------------------------
 * Function:	H5A_close
 *
 * Purpose:	Frees a attribute and all associated memory.  
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_close(H5A_t *attr)
{
    FUNC_ENTER(H5A_close, FAIL);

    assert(attr);

    /* Free dynamicly allocated items */
    if(attr->name)
        H5MM_xfree(attr->name);
    if(attr->dt)
        H5T_close(attr->dt);
    if(attr->ds)
        H5S_close(attr->ds);
    if(attr->data)
        H5MM_xfree(attr->data);

    /* Close the object's symbol-table entry */
    if(attr->ent_opened)
        H5O_close(&(attr->ent));

/* Do something with the shared information? */

    H5MM_xfree(attr);
    
    FUNC_LEAVE(SUCCEED);
}

