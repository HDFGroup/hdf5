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

#include <stdarg.h>

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
   if (ret_value<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_CANTINIT, FAIL,
		     "unable to initialize atom group");
   }

   /*
    * Register cleanup function.
    */
   if (H5_add_exit (H5C_term_interface)<0) {
      HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL,
		     "unable to install atexit function");
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
      HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
		     "not implemented yet");

   case H5C_DATASET_CREATE:
      tmpl = H5MM_xmalloc (sizeof(H5D_create_t));
      memcpy (tmpl, &H5D_create_dflt, sizeof(H5D_create_t));
      break;

   case H5C_DATASET_XFER:
      tmpl = H5MM_xmalloc (sizeof(H5D_xfer_t));
      memcpy (tmpl, &H5D_xfer_dflt, sizeof(H5D_xfer_t));
      break;
      
   default:
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "unknown template class");
   }

   /* Atomize the new template */
   if ((ret_value = H5C_create (type, tmpl))<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_CANTINIT, FAIL,
		     "can't register template");
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
      HRETURN_ERROR (H5E_ATOM, H5E_CANTINIT, FAIL,
		     "can't register template");
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
      HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "unable to remove atom");
   }
   H5MM_xfree (tmpl);

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5Cget_class
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
H5Cget_class (hid_t template)
{
   group_t	group;
   H5C_class_t	ret_value = H5C_NO_CLASS;
   
   FUNC_ENTER (H5Cget_class, H5C_NO_CLASS);
   
   if ((group = H5Aatom_group (template))<0 ||
       group<H5_TEMPLATE_0 || group>=H5_TEMPLATE_MAX) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, H5C_NO_CLASS, "not a template");
   }

   ret_value = group - H5_TEMPLATE_0;
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Cget_prop
 *
 * Purpose:	Retrieves a property value from a template.  The value is
 *		returned through an argument, BUF, of the appropriate pointer
 *		type.
 *
 *		Properties of H5C_FILE_CREATE templates and the type for the
 *		return argument(s):
 *
 *	 		H5F_USERBLOCK_SIZE	(size_t*)
 *			Size of the initial part of the file that is not used
 *			by HDF5 and may contain user-defined data.
 *
 *			H5F_SIZEOF_ADDR		(size_t*)
 *			Size in bytes of addresses stored in the file.  This
 *			is similar in nature to the `off_t' type in memory.
 *
 *			H5F_SIZEOF_SIZE		(size_t*)
 *			Size in bytes of fields that contains object sizes in
 *			the file.  This is similar in nature to the `size_t'
 *			type in memory.
 *
 *			H5F_SYM_LEAF_K		(int*)
 *			One half of the number of symbols that can be stored
 *			in a symbol table node.  A symbol table node is the
 *			leaf of a symbol table tree which is used to store a
 *			group.  When symbols are insterted randomly into a
 *			group, the group's symbol table nodes are 75% full on
 *			average.  That is, they contain 1.5 times the number
 *			of symbols specified by this property.
 *
 *			H5F_SYM_INTERN_K	(int*)
 *			One half the rank of a tree that stores a symbol
 *			table.  Internal nodes of the symbol table are on
 *			average 75% full.  That is, the average rank of the
 *			tree is 1.5 times the value specified for this
 *			property.
 *
 *			H5F_ISTORE_K		(int*)
 *			One half the rank of a tree that stores chunked raw
 *			data. On average, such a tree will be 75% full, or
 *			have an average rank of 1.5 times the value specified
 *			for this property.
 *
 *			H5F_BOOTBLOCK_VER	(int*)
 *			The version number of the file boot block.  This is a
 *			read-only property.
 *
 *			H5F_SMALLOBJECT_VER	(int*)
 *			The version number of the global heap.  This is a
 *			read-only property.
 *
 *			H5F_FREESPACE_VER	(int*)
 *			The version number of the free list.  This is a
 *			read-only property.
 *
 *			H5F_OBJECTDIR_VER	(int*)
 *			The version number of the root symbol table entry.
 *			This is a read-only property.
 *
 *			H5F_SHAREDHEADER_VER	(int*)
 *			The version number for shared object header messages.
 *			This is a read-only property.
 *
 *		Properties of H5C_FILE_ACCESS templates and the type for the
 *		return argument(s):
 *
 * 			** None defined yet **
 *
 *		Properties of H5C_DATASET_CREATE templates and the type for
 *		the return argument(s):
 *
 * 			H5D_LAYOUT		(H5D_layout_t*)
 *			The layout of raw data on disk.
 *
 * 			H5D_CHUNK_NDIMS		(int*)
 *			The number of dimensions per chunk.  This is actually
 *			one less than the real number of dimensions since the
 *			real chunk dimensions also include the data type
 *			itself.  The number of dimensions returned here
 *			should be the same as the number of dimensions
 *			returned by H5Pget_ndims().
 *
 * 			H5D_CHUNK_SIZE		(size_t[])
 *			An array that specifies the chunk size in each
 *			dimension. The actual dimension list omits the final
 *			dimension corresponding to the data type and returns
 *			only the dimensions corresponding to the data space.
 *			The array should be large enough to hold at least the
 *			number of dimension sizes returned by the
 *			H5D_CHUNK_NDIMS property.
 *
 * 			H5D_COMPRESS		(H5D_compress_t*)
 *			The raw data compression algorithm.
 *
 * 			H5D_PRE_OFFSET		(double*)
 *			The value which is added to each data point before
 *			compression or subtracted from each data point after
 *			uncompression.
 *
 *			H5D_PRE_SCALE		(double*)
 *			The value by which each data point is multiplied
 *			before compression or divided after uncompression.
 *
 *		Properties of H5C_DATASET_XFER templates and the type for
 *		the return argument(s):
 * 			
 * 			** None defined yet **
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS      BADRANGE      Unknown property for dataset create
 *		                        template. 
 *		ARGS      BADRANGE      Unknown property for dataset transfer
 *		                        template. 
 *		ARGS      BADRANGE      Unknown property for file access
 *		                        template. 
 *		ARGS      BADRANGE      Unknown property for file create
 *		                        template. 
 *		ARGS      BADRANGE      Unknown template class. 
 *		ARGS      BADTYPE       Not a template. 
 *		INTERNAL  UNSUPPORTED   Not implemented yet. 
 *		TEMPLATE  UNINITIALIZED Chunk dimensionality is not initialized
 *		TEMPLATE  UNINITIALIZED Chunk size is not initialized. 
 *
 * Programmer:	Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cget_prop (hid_t template, H5C_prop_t prop, void *buf/*out*/)
{
   const void		*tmpl = NULL;
   const H5F_create_t	*file_create = NULL;
   const H5D_create_t	*dset_create = NULL;
   H5C_class_t		type;
   intn			i;
   
   FUNC_ENTER (H5Cget_prop, FAIL);
   H5ECLEAR;

   
   /* check args */
   if ((type=H5Cget_class (template))<0 ||
       NULL==(tmpl=H5Aatom_object (template))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a template");
   }

   /* Handle each class of template */
   switch (type) {
   case H5C_FILE_CREATE:
      file_create = (const H5F_create_t *)tmpl;
      
      switch (prop) {
      case H5F_SIZEOF_USERBLOCK:
	 *(size_t *)buf = file_create->userblock_size;
	 break;

      case H5F_SIZEOF_ADDR:
	 *(size_t *)buf = file_create->sizeof_addr;
	 break;

      case H5F_SIZEOF_SIZE:
	 *(size_t *)buf = file_create->sizeof_size;
	 break;

      case H5F_SYM_LEAF_K:
	 *(int *)buf=file_create->sym_leaf_k;
	 break;

      case H5F_SYM_INTERN_K:
	 *(int *)buf = file_create->btree_k[H5B_SNODE_ID];
	 break;

      case H5F_ISTORE_K:
	 *(int *)buf = file_create->btree_k[H5B_ISTORE_ID];
	 break;

      case H5F_BOOTBLOCK_VER:
	 *(int *)buf=file_create->bootblock_ver;
	 break;

      case H5F_SMALLOBJECT_VER:
	 *(int *)buf=file_create->smallobject_ver;
	 break;

      case H5F_FREESPACE_VER:
	 *(int *)buf=file_create->freespace_ver;
	 break;

      case H5F_OBJECTDIR_VER:
	 *(int *)buf=file_create->objectdir_ver;
	 break;

      case H5F_SHAREDHEADER_VER:
	 *(int *)buf=file_create->sharedheader_ver;
	 break;

      default:
	 HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
			"unknown property for file create template");
      }
      break;

   case H5C_FILE_ACCESS:
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		     "unknown property for file access template");
       
   case H5C_DATASET_CREATE:
      dset_create = (const H5D_create_t *)tmpl;
      switch (prop) {
      case H5D_LAYOUT:
	 *(H5D_layout_t*)buf = dset_create->layout;
	 break;

      case H5D_CHUNK_NDIMS:
	 if (H5D_CHUNKED==dset_create->layout) {
	    *(int*)buf = dset_create->chunk_ndims; 
	 } else {
	    HRETURN_ERROR (H5E_TEMPLATE, H5E_UNINITIALIZED, FAIL,
			   "chunk dimensionality is not initialized");
	 }
	 break;

      case H5D_CHUNK_SIZE:
	 if (H5D_CHUNKED==dset_create->layout) {
	    for (i=0; i<dset_create->chunk_ndims; i++) {
	       ((size_t*)buf)[i] = dset_create->chunk_size[i];
	    }
	 } else {
	    HRETURN_ERROR (H5E_TEMPLATE, H5E_UNINITIALIZED, FAIL,
			   "chunk size is not initialized");
	 }
	 break;

      case H5D_COMPRESS:
      case H5D_PRE_OFFSET:
      case H5D_PRE_SCALE:
	 HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
			"not implemented yet");

      default:
	 HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
			"unknown property for dataset create template");
      }
      break;
       
   case H5C_DATASET_XFER:
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		     "unknown property for dataset transfer template");

   default:
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		     "unknown template class");
   }
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Cset_prop
 *
 * Purpose:	Sets a property, PROP, to a specified value in a TEMPLATE.
 *		The value data type depends on the property being set and is
 *		documented in H5Cget_prop().  The data type does not include
 *		the pointer (that is, property values are passed by value,
 *		not reference).
 *
 * Note:	Not all properties that can be queried with H5Cget_prop() can
 *		be set to a value with H5Cset_prop().  Such properties are
 *		documented as read-only in H5Cget_prop().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Cset_prop (hid_t template, H5C_prop_t prop, ...)
{
   void			*tmpl = NULL;
   H5F_create_t		*file_create = NULL;
   H5D_create_t		*dset_create = NULL;
   H5C_class_t		type;
   H5D_layout_t		layout;
   size_t		size, *dims = NULL;
   intn			i, n;
   va_list		ap;
   herr_t		ret_value = FAIL;

   FUNC_ENTER (H5Cset_prop, FAIL);
   H5ECLEAR;

   va_start (ap, prop);
   
   if ((type=H5Cget_class (template))<0 ||
       NULL==(tmpl=H5Aatom_object (template))) {
      HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a template");
   }

   /* Handle each class of template */
   switch (type) {
   case H5C_FILE_CREATE:
      file_create = (H5F_create_t *)tmpl;
      
      switch (prop) {
      case H5F_SIZEOF_USERBLOCK:
	 size = va_arg (ap, size_t);
	 for (i=8; i<8*sizeof(int); i++) {
	    uintn p2 = 8==i ? 0 :1<<i;
	    if (size==p2) break;
	 }
	 if (i>=8*sizeof(int)) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			 "userblock size is not valid");
	 }
	 file_create->userblock_size = size;
	 break;

      case H5F_SIZEOF_ADDR:
	 size = va_arg (ap, size_t);
	 if (size!=2 && size!=4 && size!=8 && size!=16) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			 "file haddr_t size is not valid");
	 }
	 file_create->sizeof_addr = size;
	 break;

      case H5F_SIZEOF_SIZE:
	 size = va_arg (ap, size_t);
	 if (size!=2 && size!=4 && size!=8 && size!=16) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			 "file size_t size is not valid");
	 }
	 file_create->sizeof_size = size;
	 break;

      case H5F_SYM_LEAF_K:
	 n = va_arg (ap, int);
	 if (n<2) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
			 "symbol leaf node 1/2 rank is not valid");
	 }
	 file_create->sym_leaf_k = n;
	 break;

      case H5F_SYM_INTERN_K:
	 n = va_arg (ap, int);
	 if (n<2) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
			 "symbol internal node 1/2 rank is not valid");
	 }
	 file_create->btree_k[H5B_SNODE_ID] = n;
	 break;

      case H5F_ISTORE_K:
	 n = va_arg (ap, int);
	 if (n<2) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
			 "indexed storage internal node 1/2 rank not valid");
	 }
	 file_create->btree_k[H5B_ISTORE_ID] = n;
	 break;
	    
      case H5F_BOOTBLOCK_VER:
      case H5F_SMALLOBJECT_VER:
      case H5F_FREESPACE_VER:
      case H5F_OBJECTDIR_VER:
      case H5F_SHAREDHEADER_VER:
	 HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		      "this is a read-only property");

      default:
	 HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
		     "unknown file creation property");
      }
      break;

   case H5C_FILE_ACCESS:
      HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		   "unknown property for file access template");
       
   case H5C_DATASET_CREATE:
      dset_create = (H5D_create_t *)tmpl;
      
      switch (prop) {
      case H5D_LAYOUT:
	 layout = va_arg (ap, H5D_layout_t);
	 if (layout<0 || layout>=H5D_NLAYOUTS) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
			 "raw data layout method is not valid");
	 }
	 dset_create->layout = layout;
	 break;
	 
      case H5D_CHUNK_NDIMS:
	 n = va_arg (ap, int);
	 if (H5D_CHUNKED!=dset_create->layout) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
			 "not a chunked layout template");
	 }
	 if (n<=0 || n>NELMTS (dset_create->chunk_size)) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			 "invalid number of dimensions");
	 }
	 dset_create->chunk_ndims = n;
	 for (i=0; i<n; i++) dset_create->chunk_size[i] = 1;
	 break;

      case H5D_CHUNK_SIZE:
	 dims = va_arg (ap, size_t*);
	 if (H5D_CHUNKED!=dset_create->layout) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
			 "not a chunked layout template");
	 }
	 if (!dims) {
	    HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no dims");
	 }
	 for (i=0; i<dset_create->chunk_ndims; i++) {
	    if (dims[i]<=0) {
	       HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			    "invalid dimension size");
	    }
	 }
	 for (i=0; i<dset_create->chunk_ndims; i++) {
	    dset_create->chunk_size[i] = dims[i];
	 }
	 break;

      case H5D_COMPRESS:
      case H5D_PRE_OFFSET:
      case H5D_PRE_SCALE:
	 HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
		      "not implemented yet");

      default:
	 HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		      "unknown property for dataset create template");
      }
      break;
       
   case H5C_DATASET_XFER:
      HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		   "unknown property for dataset transfer template");

   default:
      HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		   "unknown template class");
   }

   ret_value = SUCCEED;

 done:
   va_end (ap);
   FUNC_LEAVE (ret_value);
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
       (type=H5Cget_class (template))<0 ||
       (group=H5Aatom_group (template))<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL,
		     "can't unatomize template");
   }

   /* How big is the template */
   switch (type) {
   case H5C_FILE_CREATE:
      size = sizeof(H5F_create_t);
      break;

   case H5C_FILE_ACCESS:
      HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
		     "file access properties are not implemented yet");

   case H5C_DATASET_CREATE:
      size = sizeof(H5D_create_t);
      break;
      
   case H5C_DATASET_XFER:
      size = sizeof(H5D_xfer_t);
      break;

   default:
      HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		     "unknown template class");
   }

   /* Create the new template */
   new_tmpl = H5MM_xmalloc (size);
   HDmemcpy (new_tmpl, tmpl, size);

   /* Register the atom for the new template */
   if ((ret_value=H5Aregister_atom (group, new_tmpl))<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		     "unable to atomize template pointer");
   }

   FUNC_LEAVE (ret_value);
}
