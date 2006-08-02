/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Friday, September 19, 1997
 */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"        /* Memory Management                    */

/* Private macros */
#define H5G_NO_CHANGE   (-1)            /*see H5G_ent_modified()             */

/* Private prototypes */
#ifdef NOT_YET
static herr_t H5G_ent_modified(H5G_entry_t *ent, H5G_type_t cache_type);
#endif /* NOT_YET */
static herr_t H5G_ent_encode(H5F_t *f, uint8_t **pp, const H5G_entry_t *ent);
static herr_t H5G_ent_decode(H5F_t *f, const uint8_t **pp,
			      H5G_entry_t *ent/*out*/);

/* Declare extern the PQ free list for the wrapped strings */
H5FL_BLK_EXTERN(str_buf);


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_modified
 *
 * Purpose:     This function should be called after you make any
 *              modifications to a symbol table entry cache.  Supply the new
 *              type for the cache.  If CACHE_TYPE is the constant
 *              H5G_NO_CHANGE then the cache type isn't changed--just the
 *              dirty bit is set.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_modified(H5G_entry_t *ent, H5G_type_t cache_type)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_ent_modified)

    HDassert(ent);

    /* Update cache type, if requested */
    if (H5G_NO_CHANGE != cache_type)
        ent->type = cache_type;
    ent->dirty = TRUE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_ent_modified */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_decode_vec
 *
 * Purpose:     Same as H5G_ent_decode() except it does it for an array of
 *              symbol table entries.
 *
 * Errors:
 *              SYM       CANTDECODE    Can't decode.
 *
 * Return:      Success:        Non-negative, with *pp pointing to the first byte
 *                              after the last symbol.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_decode_vec(H5F_t *f, const uint8_t **pp, H5G_entry_t *ent, unsigned n)
{
    unsigned    u;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_ent_decode_vec, FAIL);

    /* check arguments */
    assert(f);
    assert(pp);
    assert(ent);

    /* decode entries */
    for (u = 0; u < n; u++)
        if (H5G_ent_decode(f, pp, ent + u) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode")

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_decode
 *
 * Purpose:     Decodes a symbol table entry pointed to by `*pp'.
 *
 * Errors:
 *
 * Return:      Success:        Non-negative with *pp pointing to the first byte
 *                              following the symbol table entry.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 * Modifications:
 *	Robb Matzke, 17 Jul 1998
 * 	Added a 4-byte padding field for alignment and future expansion.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_ent_decode(H5F_t *f, const uint8_t **pp, H5G_entry_t *ent)
{
    const uint8_t	*p_ret = *pp;
    uint32_t		tmp;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_ent_decode)

    /* check arguments */
    HDassert(f);
    HDassert(pp);
    HDassert(ent);

    ent->file = f;

    /* decode header */
    H5F_DECODE_LENGTH(f, *pp, ent->name_off);
    H5F_addr_decode(f, pp, &(ent->header));
    UINT32DECODE(*pp, tmp);
    *pp += 4; /*reserved*/
    ent->type=(H5G_type_t)tmp;

    /* decode scratch-pad */
    switch (ent->type) {
        case H5G_NOTHING_CACHED:
            break;

        case H5G_CACHED_STAB:
            assert(2 * H5F_SIZEOF_ADDR(f) <= H5G_SIZEOF_SCRATCH);
            H5F_addr_decode(f, pp, &(ent->cache.stab.btree_addr));
            H5F_addr_decode(f, pp, &(ent->cache.stab.heap_addr));
            break;

        case H5G_CACHED_SLINK:
            UINT32DECODE (*pp, ent->cache.slink.lval_offset);
            break;

        case H5G_CACHED_ULINK:
            UINT32DECODE (*pp, ent->cache.ulink.udata_size);
            UINT32DECODE (*pp, ent->cache.ulink.udata_offset);
            UINT32DECODE (*pp, ent->cache.ulink.link_type);
            break;
        default:
            /* Error or unknown type. Bail out. */
            return -1;
    }

    *pp = p_ret + H5G_SIZEOF_ENTRY(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_ent_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_encode_vec
 *
 * Purpose:     Same as H5G_ent_encode() except it does it for an array of
 *              symbol table entries.
 *
 * Errors:
 *              SYM       CANTENCODE    Can't encode.
 *
 * Return:      Success:        Non-negative, with *pp pointing to the first byte
 *                              after the last symbol.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_encode_vec(H5F_t *f, uint8_t **pp, const H5G_entry_t *ent, unsigned n)
{
    unsigned    u;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_ent_encode_vec, FAIL);

    /* check arguments */
    assert(f);
    assert(pp);
    assert(ent);

    /* encode entries */
    for (u = 0; u < n; u++)
        if (H5G_ent_encode(f, pp, ent + u) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "can't encode");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_encode
 *
 * Purpose:     Encodes the specified symbol table entry into the buffer
 *              pointed to by *pp.
 *
 * Errors:
 *
 * Return:      Success:        Non-negative, with *pp pointing to the first byte
 *                              after the symbol table entry.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 * Modifications:
 *
 *      Robb Matzke, 8 Aug 1997
 *      Writes zeros for the bytes that aren't used so the file doesn't
 *      contain junk.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_ent_encode(H5F_t *f, uint8_t **pp, const H5G_entry_t *ent)
{
    uint8_t		*p_ret = *pp + H5G_SIZEOF_ENTRY(f);

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_ent_encode);

    /* check arguments */
    assert(f);
    assert(pp);

    if (ent) {
        /* encode header */
        H5F_ENCODE_LENGTH(f, *pp, ent->name_off);
        H5F_addr_encode(f, pp, ent->header);
        UINT32ENCODE(*pp, ent->type);
	UINT32ENCODE(*pp, 0); /*reserved*/

        /* encode scratch-pad */
        switch (ent->type) {
            case H5G_NOTHING_CACHED:
                break;

            case H5G_CACHED_STAB:
                assert(2 * H5F_SIZEOF_ADDR(f) <= H5G_SIZEOF_SCRATCH);
                H5F_addr_encode(f, pp, ent->cache.stab.btree_addr);
                H5F_addr_encode(f, pp, ent->cache.stab.heap_addr);
                break;

            case H5G_CACHED_SLINK:
                UINT32ENCODE (*pp, ent->cache.slink.lval_offset);
                break;

            case H5G_CACHED_ULINK:
                UINT32ENCODE (*pp, ent->cache.ulink.udata_size);
                UINT32ENCODE (*pp, ent->cache.ulink.udata_offset);
                UINT32ENCODE (*pp, ent->cache.ulink.link_type);
                break;

            default:
                /* Unknown cached type. Bail out. */
                return -1;
        }
    } else {
        H5F_ENCODE_LENGTH(f, *pp, 0);
        H5F_addr_encode(f, pp, HADDR_UNDEF);
        UINT32ENCODE(*pp, H5G_NOTHING_CACHED);
	UINT32ENCODE(*pp, 0); /*reserved*/
    }

    /* fill with zero */
    while (*pp < p_ret) *(*pp)++ = 0;
    *pp = p_ret;

    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_copy
 *
 * Purpose:     Do a deep copy of symbol table entries
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Pedro Vicente
 *              pvn@ncsa.uiuc.edu
 *              ???day, August ??, 2002
 *
 * Notes:       'depth' parameter determines how much of the group entry
 *              structure we want to copy.  The values are:
 *                  H5_COPY_SHALLOW - Copy all the fields from the source
 *                      to the destination, including the user path and
 *                      canonical path. (Destination "takes ownership" of
 *                      user and canonical paths)
 *                  H5_COPY_DEEP - Copy all the fields from the source to
 *                      the destination, deep copying the user and canonical
 *                      paths.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_copy(H5G_entry_t *dst, const H5G_entry_t *src, H5_copy_depth_t depth)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_ent_copy)

    /* Check arguments */
    HDassert(src);
    HDassert(dst);
    HDassert(depth == H5_COPY_SHALLOW || depth == H5_COPY_DEEP);

    /* Copy the top level information */
    HDmemcpy(dst, src, sizeof(H5G_entry_t));

    /* Deep copy the names */
    if(depth == H5_COPY_DEEP) {
        /* Nothing currently */
        ;
    } else if(depth == H5_COPY_SHALLOW) {
        /* Discarding 'const' qualifier OK - QAK */
        H5G_ent_reset((H5G_entry_t *)src);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_ent_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5G_ent_reset
 *
 * Purpose:	Reset a symbol table entry to an empty state
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              ?day, August ??, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_reset(H5G_entry_t *ent)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_ent_reset)

    /* Check arguments */
    HDassert(ent);

    /* Clear the symbol table entry to an empty state */
    HDmemset(ent, 0, sizeof(H5G_entry_t));
    ent->header = HADDR_UNDEF;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_ent_reset() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_convert
 *
 * Purpose:     Convert a link to a symbol table entry
 *
 * Return:      Success:        Non-negative, with *pp pointing to the first byte
 *                              after the last symbol.
 *
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Sep 20 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_convert(H5F_t *f, haddr_t heap_addr, const char *name, const H5O_link_t *lnk,
    H5G_entry_t *ent, hid_t dxpl_id)
{
    size_t	name_offset;            /* Offset of name in heap */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_ent_convert, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(heap_addr));
    HDassert(name);
    HDassert(lnk);

    /* Reset the new entry */
    H5G_ent_reset(ent);

    /*
     * Add the new name to the heap.
     */
    name_offset = H5HL_insert(f, dxpl_id, heap_addr, HDstrlen(name) + 1, name);
    if(0 == name_offset || (size_t)(-1) == name_offset)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, H5B_INS_ERROR, "unable to insert symbol name into heap")
    ent->name_off = name_offset;

    /* Build correct information for symbol table entry based on link type */
    switch(lnk->type) {
        case H5L_LINK_HARD:
            ent->type = H5G_NOTHING_CACHED;
            ent->header = lnk->u.hard.addr;
            break;

        case H5L_LINK_SOFT:
            {
                size_t	lnk_offset;		/* Offset to sym-link value	*/

                /* Insert link value into local heap */
                if((size_t)(-1) == (lnk_offset = H5HL_insert(f, dxpl_id, heap_addr,
                        HDstrlen(lnk->u.soft.name) + 1, lnk->u.soft.name)))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write link value to local heap")

                ent->type = H5G_CACHED_SLINK;
                ent->cache.slink.lval_offset = lnk_offset;
            } /* end case */
            break;

        default:
          if(lnk->type < H5L_LINK_UD_MIN)
              HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unrecognized link type")

          {
                size_t udata_offset = (size_t) (-1);  /* Offset to data buffer */

                if(lnk->u.ud.size > 0)
                {
                  if((size_t)(-1) == (udata_offset = H5HL_insert(f, dxpl_id,
                        heap_addr, lnk->u.ud.size, lnk->u.ud.udata)))
                      HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write user data to local heap")
                }

                ent->type = H5G_CACHED_ULINK;
                ent->cache.ulink.udata_size = lnk->u.ud.size;
                ent->cache.ulink.udata_offset = udata_offset;
                ent->cache.ulink.link_type = lnk->type;
          }
    } /* end switch */

    /* Set the file for the entry */
    ent->file = f;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_ent_convert() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ent_debug
 *
 * Purpose:     Prints debugging information about a symbol table entry.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 29 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_debug(H5F_t UNUSED *f, hid_t dxpl_id, const H5G_entry_t *ent, FILE * stream,
	      int indent, int fwidth, haddr_t heap)
{
    const char		*lval = NULL;
    int nested_indent, nested_fwidth;

    FUNC_ENTER_NOAPI_NOFUNC(H5G_ent_debug);

    /* Calculate the indent & field width values for nested information */
    nested_indent=indent+3;
    nested_fwidth=MAX(0,fwidth-3);

    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	      "Name offset into private heap:",
	      (unsigned long) (ent->name_off));

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Object header address:", ent->header);

    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty:",
	      ent->dirty ? "Yes" : "No");
    HDfprintf(stream, "%*s%-*s ", indent, "", fwidth,
	      "Cache info type:");
    switch (ent->type) {
        case H5G_NOTHING_CACHED:
            HDfprintf(stream, "Nothing Cached\n");
            break;

        case H5G_CACHED_STAB:
            HDfprintf(stream, "Symbol Table\n");

            HDfprintf(stream, "%*s%-*s\n", indent, "", fwidth,
                      "Cached entry information:");
            HDfprintf(stream, "%*s%-*s %a\n", nested_indent, "", nested_fwidth,
                      "B-tree address:", ent->cache.stab.btree_addr);

            HDfprintf(stream, "%*s%-*s %a\n", nested_indent, "", nested_fwidth,
                      "Heap address:", ent->cache.stab.heap_addr);
            break;

        case H5G_CACHED_SLINK:
            HDfprintf (stream, "Symbolic Link\n");
            HDfprintf(stream, "%*s%-*s\n", indent, "", fwidth,
                      "Cached information:");
            HDfprintf (stream, "%*s%-*s %lu\n", nested_indent, "", nested_fwidth,
                       "Link value offset:",
                       (unsigned long)(ent->cache.slink.lval_offset));
            if (heap>0 && H5F_addr_defined(heap)) {
                const H5HL_t *heap_ptr;

                heap_ptr = H5HL_protect(ent->file, dxpl_id, heap);
                lval = H5HL_offset_into(ent->file, heap_ptr, ent->cache.slink.lval_offset);
                HDfprintf (stream, "%*s%-*s %s\n", nested_indent, "", nested_fwidth,
                           "Link value:",
                           lval);
                H5HL_unprotect(ent->file, dxpl_id, heap_ptr, heap, H5AC__NO_FLAGS_SET);
            }
            else
                HDfprintf(stream, "%*s%-*s\n", nested_indent, "", nested_fwidth, "Warning: Invalid heap address given, name not displayed!");
            break;

        case H5G_CACHED_ULINK:
            if(ent->cache.ulink.link_type == H5L_LINK_EXTERNAL)
            {
              HDfprintf (stream, "External Link\n");
              HDfprintf(stream, "%*s%-*s\n", indent, "", fwidth,
                        "Cached information:");
              if(ent->cache.ulink.udata_size > 0)
              {
                  HDfprintf (stream, "%*s%-*s %lu\n", nested_indent, "", nested_fwidth,
                              "User data offset:",
                              (unsigned long)(ent->cache.ulink.udata_offset));
              }
              HDfprintf (stream, "%*s%-*s %lu\n", nested_indent, "", nested_fwidth,
                          "User data size:",
                          (unsigned long)(ent->cache.ulink.udata_size));
              if (heap>0 && H5F_addr_defined(heap)) {
                  const H5HL_t *heap_ptr;
                  char * filename;
                  char * pathname;

                  heap_ptr = H5HL_protect(ent->file, dxpl_id, heap);
                  lval = H5HL_offset_into(ent->file, heap_ptr, ent->cache.ulink.udata_offset);
                  if(H5Lunpack_elink_val(lval, &filename, &pathname) < 0) return FAIL;

                  HDfprintf (stream, "%*s%-*s %s\n", nested_indent, "", nested_fwidth,
                            "External link file name:",
                            lval);
                  HDfprintf (stream, "%*s%-*s %s\n", nested_indent, "", nested_fwidth,
                            "External link object name:",
                            pathname);
                  H5HL_unprotect(ent->file, dxpl_id, heap_ptr, heap, H5AC__NO_FLAGS_SET);
              } else {
                  HDfprintf(stream, "%*s%-*s\n", nested_indent, "", nested_fwidth, "Warning: Invalid heap address given!");
              }
            }
            else
            {
                HDfprintf (stream, "User-defined Link\n");
                HDfprintf(stream, "%*s%-*s\n", indent, "", fwidth,
                          "Cached information:");
                HDfprintf (stream, "%*s%-*s %lu\n", nested_indent, "", nested_fwidth,
                            "Link class:",
                            (unsigned long)(ent->cache.ulink.link_type));
                if(ent->cache.ulink.udata_size > 0)
                {
                     HDfprintf (stream, "%*s%-*s %lu\n", nested_indent, "", nested_fwidth,
                                "User data offset:",
                                (unsigned long)(ent->cache.ulink.udata_offset));
                }
                HDfprintf (stream, "%*s%-*s %lu\n", nested_indent, "", nested_fwidth,
                            "User data size:",
                            (unsigned long)(ent->cache.ulink.udata_size));
                if (heap<=0 || !H5F_addr_defined(heap)) {
                    HDfprintf(stream, "%*s%-*s\n", nested_indent, "", nested_fwidth, "Warning: Invalid heap address given!");
                }
            }
            break;

        default:
            HDfprintf(stream, "*** Unknown symbol type %d\n", ent->type);
            break;
    }

    FUNC_LEAVE_NOAPI(SUCCEED);
}

