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
       hdf5file.c
   HDF5 "personality template" routines

   EXPORTED ROUTINES
       H5Csetparm   -- Set a parameter for a template
       H5Cgetparm   -- Get a parameter for a template

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
       H5C_init_interface    -- initialize the interface
   + */

/* Private header files */
#include <H5private.h>      	/* Generic Functions 			*/
#include <H5Aprivate.h>		/* Atoms				*/
#include <H5Bprivate.h>	    	/* B-tree subclass names 		*/
#include <H5Cprivate.h>     	/* Template information 		*/
#include <H5Dprivate.h>		/* Datasets				*/
#include <H5Eprivate.h>		/* Error handling			*/
#include <H5MMprivate.h>	/* Memory management			*/

#define PABLO_MASK	H5C_mask

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT H5C_init_interface
static herr_t H5C_init_interface(void);

/* PRIVATE PROTOTYPES */
static void H5C_term_interface (void);

/*--------------------------------------------------------------------------
NAME
   H5C_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5C_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5C_init_interface (void)
{
   herr_t 	ret_value = SUCCEED;
   intn		i;
   herr_t	status;
   
   FUNC_ENTER (H5C_init_interface, FAIL);
   
   assert (H5C_NCLASSES <= H5_TEMPLATE_MAX-H5_TEMPLATE_0);

   /*
    * Initialize the mappings between template classes and atom groups. We
    * keep the two separate because template classes are publicly visible but
    * atom groups aren't.
    */
   for (i=0; i<H5C_NCLASSES; i++) {
      status = H5Ainit_group (H5_TEMPLATE_0+i, H5A_TEMPID_HASHSIZE, 0, NULL);
      if (status<0) ret_value = FAIL;
   }
   if (ret_value<0) HRETURN_ERROR (H5E_ATOM, H5E_CANTINIT, FAIL);

   /*
    * Register cleanup function.
    */
   if (H5_add_exit (H5C_term_interface)<0) {
      HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL);
   }

   FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5C_term_interface
 PURPOSE
    Terminate various H5C objects
 USAGE
    void H5C_term_interface()
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
static void
H5C_term_interface (void)
{
   intn		i;

   for (i=0; i<H5C_NCLASSES; i++) {
      H5Adestroy_group (H5_TEMPLATE_0+i);
   }
}

/*--------------------------------------------------------------------------
 NAME
    H5Ccreate
 PURPOSE
    Returns a copy of the default template for some class of templates.
 USAGE
    herr_t H5Ccreate (type)
        H5C_class_t type;	IN: Template class whose default is desired.
 RETURNS
    Template ID or FAIL
 
 ERRORS
    ARGS      BADVALUE      Unknown template class. 
    ATOM      CANTINIT      Can't register template. 
    INTERNAL  UNSUPPORTED   Not implemented yet. 

 DESCRIPTION
    Returns a copy of the default template for some class of templates.
--------------------------------------------------------------------------*/
hid_t
H5Ccreate (H5C_class_t type)
{
   hid_t	ret_value = FAIL;
   void		*tmpl = NULL;

   FUNC_ENTER (H5Ccreate, FAIL);

   /* Allocate a new template and initialize it with default values */
   switch (type) {
   case H5C_FILE_CREATE:
      tmpl = H5MM_xmalloc (sizeof(H5F_create_t));
      memcpy (tmpl, &H5F_create_dflt, sizeof(H5F_create_t));
      break;

   case H5C_FILE_ACCESS:
      /* Not implemented yet */
      HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);

   case H5C_DATASET_CREATE:
      tmpl = H5MM_xmalloc (sizeof(H5D_create_t));
      memcpy (tmpl, &H5D_create_dflt, sizeof(H5D_create_t));
      break;

   case H5C_DATASET_XFER:
      tmpl = H5MM_xmalloc (sizeof(H5D_xfer_t));
      memcpy (tmpl, &H5D_xfer_dflt, sizeof(H5D_xfer_t));
      break;
      
   default:
      /* Unknown template class */
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL);
   }

   /* Atomize the new template */
   if ((ret_value = H5C_create (type, tmpl))<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_CANTINIT, FAIL); /*can't register template*/
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5C_create
 *
 * Purpose:	Given a pointer to some template struct, atomize the template
 *		and return its ID. The template memory is not copied, so the
 *		caller should not free it; it will be freed by H5C_release().
 *
 * Return:	Success:	A new template ID.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5C_create (H5C_class_t type, void *tmpl)
{
   hid_t	ret_value = FAIL;
   
   FUNC_ENTER (H5C_create, FAIL);

   /* check args */
   assert (type>=0 && type<H5C_NCLASSES);
   assert (tmpl);

   /* Atomize the new template */
   if ((ret_value = H5Aregister_atom (H5_TEMPLATE_0+type, tmpl))<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_CANTINIT, FAIL); /*can't register template*/
   }

   FUNC_LEAVE (ret_value);
}
   
/*--------------------------------------------------------------------------
 NAME
    H5Cclose
 PURPOSE
    Release access to a template object.
 USAGE
    herr_t H5Cclose(oid)
        hid_t oid;       IN: Template object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function releases access to a template object
--------------------------------------------------------------------------*/
herr_t
H5Cclose (hid_t template)
{
   void 	*tmpl=NULL;

   FUNC_ENTER (H5Cclose, FAIL);

   /* Chuck the object! :-) */
   if (NULL==(tmpl=H5Aremove_atom (template))) {
      HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);
   }
   H5MM_xfree (tmpl);

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5C_class
 *
 * Purpose:	Returns the class identifier for a template.
 *
 * Return:	Success:	A template class
 *
 *		Failure:	H5C_NO_CLASS (-1)
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5C_class_t
H5C_class (hid_t template)
{
   group_t	group;
   H5C_class_t	ret_value = H5C_NO_CLASS;
   
   FUNC_ENTER (H5C_class, H5C_NO_CLASS);
   
   if ((group = H5Aatom_group (template))<0 ||
       group<H5_TEMPLATE_0 || group>=H5_TEMPLATE_MAX) {
      /* not a template */
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, H5C_NO_CLASS);
   }

   ret_value = group - H5_TEMPLATE_0;
   FUNC_LEAVE (ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5Cgetparm
 PURPOSE
    Get a property value from a template
 USAGE
    herr_t H5Cgetparm(tid, parm, buf)
        hid_t tid;        IN: Template object to retrieve parameter from
        file_create_param_t parm;   IN: Paramter to retrieve
        VOIDP buf;          OUT: Pointer to buffer to store parameter in
 RETURNS
    SUCCEED/FAIL

 ERRORS
    ARGS      BADRANGE      No result buffer argument supplied. 
    ARGS      BADRANGE      Unknown property for dataset create template. 
    ARGS      BADRANGE      Unknown property for dataset transfer template. 
    ARGS      BADRANGE      Unknown property for file access template. 
    ARGS      BADRANGE      Unknown property for file create template. 
    ARGS      BADRANGE      Unknown template class. 
    ATOM      BADTYPE       Can't unatomize template. 

 DESCRIPTION
        This function retrieves the value of a specific parameter from a
    template

 MODIFICATIONS
 	Robb Matzke, 13 Aug 1997
	Removed H5_BTREE_SIZE and replaced it with H5_SYM_LEAF_K and
	H5_SYM_INTERN_K.
 
  	Robb Matzke, 17 Oct 1997
 	Added H5_ISTORE_K.
--------------------------------------------------------------------------*/
herr_t
H5Cgetparm (hid_t template, H5C_prop_t prop, void *buf)
{
   H5C_class_t		type;
    
   const void		*tmpl=NULL;
   const H5F_create_t	*file_create=NULL;
   const H5D_create_t	*dset_create=NULL;

   FUNC_ENTER (H5Cgetparm, FAIL);
   H5ECLEAR;

   /* check args */
   if (NULL==(tmpl = H5Aatom_object (template)) ||
       (type=H5C_class (template))<0) {
      /* Can't unatomize template */
      HRETURN_ERROR (H5E_ATOM, H5E_BADTYPE, FAIL);
   }
   if (!buf) {
      /* No result buffer argument supplied */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
   }

   /* Handle each class of template */
   switch (type) {
   case H5C_FILE_CREATE:
      file_create = (const H5F_create_t *)tmpl;
      switch (prop) {
      case H5F_USERBLOCK_SIZE:
	 *(uintn *)buf=file_create->userblock_size;
	 break;

      case H5F_OFFSET_SIZE:
	 *(uint8 *)buf=file_create->sizeof_addr;
	 break;

      case H5F_LENGTH_SIZE:
	 *(uint8 *)buf=file_create->sizeof_size;
	 break;

      case H5F_SYM_LEAF_K:
	 *(uintn *)buf=file_create->sym_leaf_k;
	 break;

      case H5F_SYM_INTERN_K:
	 *(uintn *)buf = file_create->btree_k[H5B_SNODE_ID];
	 break;

      case H5F_ISTORE_K:
	 *(uintn *)buf = file_create->btree_k[H5B_ISTORE_ID];
	 break;

      case H5F_BOOTBLOCK_VER:
	 *(uint8 *)buf=file_create->bootblock_ver;
	 break;

      case H5F_SMALLOBJECT_VER:
	 *(uint8 *)buf=file_create->smallobject_ver;
	 break;

      case H5F_FREESPACE_VER:
	 *(uint8 *)buf=file_create->freespace_ver;
	 break;

      case H5F_OBJECTDIR_VER:
	 *(uint8 *)buf=file_create->objectdir_ver;
	 break;

      case H5F_SHAREDHEADER_VER:
	 *(uint8 *)buf=file_create->sharedheader_ver;
	 break;

      default:
	 /* Unknown property for file create template */
	 HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
      }
      break;

   case H5C_FILE_ACCESS:
      /* Unknown property for file access template */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
       
   case H5C_DATASET_CREATE:
      dset_create = (const H5D_create_t *)tmpl;
      switch (prop) {
      case H5D_LAYOUT:
	 *(H5D_layout_t*)buf = dset_create->layout;
	 break;

      case H5D_CHUNK_NDIMS:
      case H5D_CHUNK_SIZE:
      case H5D_COMPRESS:
      case H5D_PRE_OFFSET:
      case H5D_PRE_SCALE:
	 /* Not implemented yet */
	 HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);

      default:
	 /* Unknown property for dataset create template */
	 HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
      }
      break;
       
   case H5C_DATASET_XFER:
      /* Unknown property for dataset transfer template */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);

   default:
      /* Unknown template class */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
   }
   FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5Csetparm
 PURPOSE
    Set a parameter from a template
 USAGE
    herr_t H5Csetparm(tid, parm, buf)
        hid_t tid;        IN: Template object to store parameter in
        file_create_param_t parm;   IN: Parameter to store
        const VOIDP buf;    IN: Pointer to parameter buffer
 RETURNS
    SUCCEED/FAIL

 ERRORS
    ARGS      BADRANGE      Indexed storage internal node 1/2 rank is not
                            valid. 
    ARGS      BADRANGE      No buffer argument specified. 
    ARGS      BADRANGE      Symbol internal node 1/2 rank is not valid. 
    ARGS      BADRANGE      Symbol leaf node 1/2 rank is not valid. 
    ARGS      BADRANGE      This is a read-only property. 
    ARGS      BADRANGE      Unknown file creation property. 
    ARGS      BADRANGE      Unknown property for dataset create template. 
    ARGS      BADRANGE      Unknown property for dataset transfer template. 
    ARGS      BADRANGE      Unknown property for file access template. 
    ARGS      BADRANGE      Unknown template class. 
    ARGS      BADVALUE      File haddr_t size is not valid. 
    ARGS      BADVALUE      File size_t size is not valid. 
    ARGS      BADVALUE      Userblock size is not valid. 
    ATOM      BADTYPE       Can't unatomize template. 

 DESCRIPTION
        This function stores the value of a specific parameter for a template

 MODIFICATIONS
 	Robb Matzke, 13 Aug 1997
	Removed H5_BTREE_SIZE and replaced it with H5_SYM_LEAF_K and
	H5_SYM_INTERN_K.

	Robb Matzke, 26 Aug 1997
	Changed `hash_size' to `val' in two places.

        Robb Matzke, 15 Sep 1997
        The H5_OFFSET_SIZE and H5_LENGTH_SIZE parameters should be passed
        a uint8 pointer for the BUF value.

	Robb Matzke, 15 Sep 1997
	Fixed the power-of-two test to work with any size integer.
 
  	Robb Matzke, 17 Oct 1997
 	Added H5_ISTORE_K.
--------------------------------------------------------------------------*/
herr_t
H5Csetparm (hid_t template, H5C_prop_t prop, const void *buf)
{
   void		*tmpl = NULL;
   H5F_create_t	*file_create = NULL;
   H5D_create_t	*dset_create = NULL;
   H5C_class_t	type;
   H5D_layout_t	layout;
   uintn	val;
   intn		i;

   FUNC_ENTER (H5Csetparm, FAIL);
   H5ECLEAR;

   /* check args */
   if (NULL==(tmpl = H5Aatom_object (template)) ||
       (type = H5C_class (template))<0) {
      /* Can't unatomize template */
      HRETURN_ERROR (H5E_ATOM, H5E_BADTYPE, FAIL);
   }
   if (!buf) {
      /* No buffer argument specified */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
   }

   /* Handle each class of template */
   switch (type) {
   case H5C_FILE_CREATE:
      file_create = (H5F_create_t *)tmpl;
      
      switch (prop) {
      case H5F_USERBLOCK_SIZE:
	 val = *(const uintn *)buf;
	 for (i=8; i<8*sizeof(int); i++) {
	    uintn p2 = 8==i ? 0 :1<<i;
	    if (val==p2) break;
	 }
	 if (i>=8*sizeof(int)) {
	    /* Userblock size is not valid */
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL);
	 }
	 file_create->userblock_size=val;
	 break;

      case H5F_OFFSET_SIZE:
	 val = *(const uint8 *)buf;
	 if (val!=2 && val!=4 && val!=8 && val!=16) {
	    /* file haddr_t size is not valid */
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL);
	 }
	 file_create->sizeof_addr=val;
	 break;

      case H5F_LENGTH_SIZE:
	 val = *(const uint8 *)buf;
	 if(val!=2 && val!=4 && val!=8 && val!=16) {
	    /* file size_t size is not valid */
	    HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL);
	 }
	 file_create->sizeof_size=val;
	 break;

      case H5F_SYM_LEAF_K:
	 val = *(const uintn *)buf;
	 if (val<2) {
	    /* Symbol leaf node 1/2 rank is not valid */
	    HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
	 }
	 file_create->sym_leaf_k = val;
	 break;

      case H5F_SYM_INTERN_K:
	 val = *(const uintn *)buf;
	 if (val<2) {
	    /* Symbol internal node 1/2 rank is not valid */
	    HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
	 }
	 file_create->btree_k[H5B_SNODE_ID] = val;
	 break;

      case H5F_ISTORE_K:
	 val = *(const uintn *)buf;
	 if (val<2) {
	    /* Indexed storage internal node 1/2 rank is not valid */
	    HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
	 }
	 file_create->btree_k[H5B_ISTORE_ID] = val;
	 break;
	    
      case H5F_BOOTBLOCK_VER:
      case H5F_SMALLOBJECT_VER:
      case H5F_FREESPACE_VER:
      case H5F_OBJECTDIR_VER:
      case H5F_SHAREDHEADER_VER:
	 /* This is a read-only property */
	 HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);

      default:
	 /* Unknown file creation property */
	 HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL);
      }
      break;

   case H5C_FILE_ACCESS:
      /* Unknown property for file access template */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
       
   case H5C_DATASET_CREATE:
      dset_create = (H5D_create_t *)tmpl;
      switch (prop) {
      case H5D_LAYOUT:
	 layout = *(const H5D_layout_t*)buf;
	 if (layout<0 || layout>=H5D_NLAYOUTS) {
	    /* Raw data layout method is not valid */
	    HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
	 }
	 dset_create->layout = layout;
	 break;
	 
      case H5D_CHUNK_NDIMS:
      case H5D_CHUNK_SIZE:
      case H5D_COMPRESS:
      case H5D_PRE_OFFSET:
      case H5D_PRE_SCALE:
	 /* Not implemented yet */
	 HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);

      default:
	 /* Unknown property for dataset create template */
	 HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
      }
      break;
       
   case H5C_DATASET_XFER:
      /* Unknown property for dataset transfer template */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);

   default:
      /* Unknown template class */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
   }
   
   FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5Ccopy
 PURPOSE
    Copy a template
 USAGE
    hid_t H5C_copy(tid)
        hid_t tid;        IN: Template object to copy
 RETURNS
    Returns template ID (atom) on success, FAIL on failure

 ERRORS
    ARGS      BADRANGE      Unknown template class. 
    ATOM      BADATOM       Can't unatomize template. 
    ATOM      CANTREGISTER  Register the atom for the new template. 
    INTERNAL  UNSUPPORTED   Dataset transfer properties are not implemented
                            yet. 
    INTERNAL  UNSUPPORTED   File access properties are not implemented yet. 

 DESCRIPTION
    This function creates a new copy of a template with all the same parameter
    settings.
--------------------------------------------------------------------------*/
hid_t
H5Ccopy (hid_t template)
{
   const void	*tmpl = NULL;
   void		*new_tmpl = NULL;
   H5C_class_t	type;
   size_t	size;
   hid_t	ret_value = FAIL;
   group_t	group;

   FUNC_ENTER (H5Ccopy, FAIL);
   H5ECLEAR;

   /* check args */
   if (NULL==(tmpl=H5Aatom_object (template)) ||
       (type=H5C_class (template))<0 ||
       (group=H5Aatom_group (template))<0) {
      /* Can't unatomize template */
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL);
   }

   /* How big is the template */
   switch (type) {
   case H5C_FILE_CREATE:
      size = sizeof(H5F_create_t);
      break;

   case H5C_FILE_ACCESS:
      /* File access properties are not implemented yet */
      HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL);

   case H5C_DATASET_CREATE:
      size = sizeof(H5D_create_t);
      break;
      
   case H5C_DATASET_XFER:
      size = sizeof(H5D_xfer_t);
      break;

   default:
      /* Unknown template class */
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL);
   }

   /* Create the new template */
   new_tmpl = H5MM_xmalloc (size);
   HDmemcpy (new_tmpl, tmpl, size);

   /* Register the atom for the new template */
   if ((ret_value=H5Aregister_atom (group, new_tmpl))<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL);
   }

   FUNC_LEAVE (ret_value);
}
