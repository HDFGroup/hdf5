/****************************************************************************
* NCSA HDF                                                                  *
* Software Development Group                                                *
* National Center for Supercomputing Applications                           *
* University of Illinois at Urbana-Champaign                                *
* 605 E. Springfield, Champaign IL 61820                                    *
*                                                                           *
* For conditions of distribution and use, see the accompanying              *
* hdf/COPYING file.                                                         *
*                                                                           *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#define H5T_PACKAGE		/*suppress error about including H5Tpkg	     */

#include <H5private.h>		/* Generic Functions			*/
#include <H5Eprivate.h>     /* Errors */
#include <H5HGprivate.h>    /* Global Heaps */
#include <H5Iprivate.h>     /* IDs */
#include <H5MMprivate.h>    /* Memory Allocation */
#include <H5Tpkg.h>         /* Datatypes */

#define PABLO_MASK	H5Tvlen_mask

/* Interface initialization */
static intn interface_initialize_g = 0;
#define INTERFACE_INIT NULL

/* Local functions */
static herr_t H5T_vlen_reclaim_recurse(void *elem, H5T_t *dt, H5MM_free_t free_func, void *free_info);


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_set_loc
 *
 * Purpose:	Sets the location of a VL datatype to be either on disk or in memory
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Friday, June 4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_set_loc(H5T_t *dt, H5F_t *f, H5T_vlen_type_t loc)
{
    FUNC_ENTER (H5T_vlen_set_loc, FAIL);

    /* check parameters */
    assert(dt);
    assert(loc>H5T_VLEN_BADTYPE && loc<H5T_VLEN_MAXTYPE);

    switch(loc) {
        case H5T_VLEN_MEMORY:   /* Memory based VL datatype */
            assert(f==NULL);

            /* Mark this type as being stored in memory */
            dt->u.vlen.type=H5T_VLEN_MEMORY;

            /* size in memory, disk size is different */
            dt->size = sizeof(hvl_t);

            /* Set up the function pointers to access the VL information (in memory) */
            dt->u.vlen.getlen=H5T_vlen_mem_getlen;
            dt->u.vlen.read=H5T_vlen_mem_read;
            dt->u.vlen.alloc=H5T_vlen_mem_alloc;
            dt->u.vlen.write=H5T_vlen_mem_write;

            /* Reset file ID (since this VL is in memory) */
            dt->u.vlen.f=NULL;
            break;

        case H5T_VLEN_DISK:   /* Disk based VL datatype */
            assert(f);

            /* Mark this type as being stored on disk */
            dt->u.vlen.type=H5T_VLEN_DISK;

            /* 
             * Size of element on disk is 4 bytes for the length, plus the size
             * of an address in this file.  Memory size is different
             */
            dt->size = H5F_SIZEOF_ADDR(f)+4;

            /* Set up the function pointers to access the VL information (in memory) */
            dt->u.vlen.getlen=H5T_vlen_disk_getlen;
            dt->u.vlen.read=H5T_vlen_disk_read;
            dt->u.vlen.alloc=H5T_vlen_disk_alloc;
            dt->u.vlen.write=H5T_vlen_disk_write;

            /* Set file ID (since this VL is on disk) */
            dt->u.vlen.f=f;
            break;

        default:
            HRETURN_ERROR (H5E_DATATYPE, H5E_BADRANGE, FAIL, "invalid VL datatype location");
    } /* end switch */

    FUNC_LEAVE (SUCCEED);
}   /* end H5T_vlen_set_loc() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_mem_getlen
 *
 * Purpose:	Retrieves the length of a memory based VL element.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t H5T_vlen_mem_getlen(H5F_t UNUSED *f, void *vl_addr)
{
    hvl_t *vl=(hvl_t *)vl_addr;   /* Pointer to the user's hvl_t information */
    hsize_t	ret_value = FAIL;	/*return value			*/

    FUNC_ENTER (H5T_vlen_mem_getlen, FAIL);

    /* check parameters */
    assert(vl);

    ret_value=vl->len;

    FUNC_LEAVE (ret_value);
}   /* end H5T_vlen_mem_getlen() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_mem_read
 *
 * Purpose:	"Reads" the memory based VL sequence into a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5T_vlen_mem_read(H5F_t UNUSED *f, void *vl_addr, void *buf, size_t len)
{
    hvl_t *vl=(hvl_t *)vl_addr;   /* Pointer to the user's hvl_t information */

    FUNC_ENTER (H5T_vlen_mem_read, FAIL);

    /* check parameters */
    assert(vl && vl->p);
    assert(buf);

    HDmemcpy(buf,vl->p,len);

    FUNC_LEAVE (SUCCEED);
}   /* end H5T_vlen_mem_read() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_mem_alloc
 *
 * Purpose:	Allocates a memory based VL sequence
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5T_vlen_mem_alloc(const H5F_xfer_t *xfer_parms, void *vl_addr, hsize_t seq_len, hsize_t base_size)
{
    hvl_t *vl=(hvl_t *)vl_addr;   /* Pointer to the user's hvl_t information */

    FUNC_ENTER (H5T_vlen_mem_alloc, FAIL);

    /* check parameters */
    assert(vl);

    /* Use the user's memory allocation routine is one is defined */
    if(xfer_parms->vlen_alloc!=NULL) {
        if(NULL==(vl->p=(xfer_parms->vlen_alloc)(seq_len*base_size,xfer_parms->alloc_info)))
            HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for VL data");
      } /* end if */
    else {  /* Default to system malloc */
        if(NULL==(vl->p=H5MM_malloc(seq_len*base_size)))
            HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for VL data");
      } /* end else */
    vl->len=seq_len;

    FUNC_LEAVE (SUCCEED);
}   /* end H5T_vlen_mem_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_mem_write
 *
 * Purpose:	"Writes" the memory based VL sequence from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5T_vlen_mem_write(H5F_t UNUSED *f, void *vl_addr, void *buf, size_t len)
{
    hvl_t *vl=(hvl_t *)vl_addr;   /* Pointer to the user's hvl_t information */

    FUNC_ENTER (H5T_vlen_mem_write, FAIL);

    /* check parameters */
    assert(vl && vl->p);
    assert(buf);

    HDmemcpy(vl->p,buf,len);

    FUNC_LEAVE (SUCCEED);
}   /* end H5T_vlen_mem_write() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_getlen
 *
 * Purpose:	Retrieves the length of a memory based VL element.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t H5T_vlen_disk_getlen(H5F_t UNUSED *f, void *vl_addr)
{
    uint8_t *vl=(uint8_t *)vl_addr;   /* Pointer to the disk VL information */
    hsize_t	ret_value = FAIL;	/*return value			*/

    FUNC_ENTER (H5T_vlen_disk_getlen, FAIL);

    /* check parameters */
    assert(vl);

    UINT32DECODE(vl, ret_value);

    FUNC_LEAVE (ret_value);
}   /* end H5T_vlen_disk_getlen() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_read
 *
 * Purpose:	Reads the disk based VL sequence into a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5T_vlen_disk_read(H5F_t *f, void *vl_addr, void *buf, size_t UNUSED len)
{
    uint8_t *vl=(uint8_t *)vl_addr;   /* Pointer to the user's hvl_t information */
    H5HG_t hobjid;
    uint32_t seq_len;

    FUNC_ENTER (H5T_vlen_disk_read, FAIL);

    /* check parameters */
    assert(vl);
    assert(buf);
    assert(f);

    /* Get the length of the sequence */
    UINT32DECODE(vl, seq_len); /* Not used */
    
    /* Get the heap information */
    H5F_addr_decode(f,(const uint8_t **)&vl,&(hobjid.addr));
    INT32DECODE(vl,hobjid.idx);

    /* Read the VL information from disk */
    if(H5HG_read(f,&hobjid,buf)==NULL)
        HRETURN_ERROR(H5E_DATATYPE, H5E_READERROR, FAIL, "Unable to read VL information");

    FUNC_LEAVE (SUCCEED);
}   /* end H5T_vlen_disk_read() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_alloc
 *
 * Purpose:	Allocates a disk based VL sequence
 *      NOTE: This function is currently a NOOP, allocation of the heap block
 *          is done when the block is written out.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5T_vlen_disk_alloc(const H5F_xfer_t UNUSED *f, void UNUSED *vl_addr, hsize_t UNUSED seq_len, hsize_t UNUSED base_size)
{
    FUNC_ENTER (H5T_vlen_disk_alloc, FAIL);

    /* check parameters */

    FUNC_LEAVE (SUCCEED);
}   /* end H5T_vlen_disk_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_write
 *
 * Purpose:	Writes the disk based VL sequence from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5T_vlen_disk_write(H5F_t *f, void *vl_addr, void *buf, size_t len)
{
    uint8_t *vl=(uint8_t *)vl_addr;   /* Pointer to the user's hvl_t information */
    H5HG_t hobjid;
    uint32_t seq_len;

    FUNC_ENTER (H5T_vlen_disk_write, FAIL);

    /* check parameters */
    assert(vl);
    assert(buf);
    assert(f);

    /* Set the length of the sequence */
    UINT32ENCODE(vl, seq_len);
    
    /* Write the VL information to disk (allocates space also) */
    if(H5HG_insert(f,len,buf,&hobjid)<0)
        HRETURN_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "Unable to write VL information");

    /* Get the heap information */
    H5F_addr_encode(f,&vl,&hobjid.addr);
    INT32ENCODE(vl,hobjid.idx);

    FUNC_LEAVE (SUCCEED);
}   /* end H5T_vlen_disk_write() */


/*--------------------------------------------------------------------------
 NAME
    H5T_vlen_reclaim_recurse
 PURPOSE
    Internal recursive routine to free VL datatypes
 USAGE
    herr_t H5T_vlen_reclaim(elem,dt)
        void *elem;  IN/OUT: Pointer to the dataset element
        H5T_t *dt;   IN: Datatype of dataset element
        
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Frees any dynamic memory used by VL datatypes in the current dataset
    element.  Performs a recursive depth-first traversal of all compound
    datatypes to free all VL datatype information allocated by any field.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t 
H5T_vlen_reclaim_recurse(void *elem, H5T_t *dt, H5MM_free_t free_func, void *free_info)
{
    intn i,j;   /* local counting variable */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER(H5T_vlen_reclaim_recurse, FAIL);

    assert(elem);
    assert(dt);

    /* Check the datatype of this element */
    switch(dt->type) {
        /* Check each field and recurse on VL and compound ones */
        case H5T_COMPOUND:
            for (i=0; i<dt->u.compnd.nmembs; i++) {
                /* Recurse if it's VL or compound */
                if(dt->u.compnd.memb[i].type->type==H5T_COMPOUND || dt->u.compnd.memb[i].type->type==H5T_VLEN) {
                    uintn nelem=1;  /* Number of array elements in field */
                    void *off;     /* offset of field */

                    /* Compute the number of array elements in field */
                    for(j=0; j<dt->u.compnd.memb[i].ndims; j++)
                        nelem *= dt->u.compnd.memb[i].dim[j];
                    
                    /* Calculate the offset of each array element and recurse on it */
                    while(nelem>0) {
                        off=((uint8_t *)elem)+dt->u.compnd.memb[i].offset+(nelem-1)*dt->u.compnd.memb[i].type->size;
                        if(H5T_vlen_reclaim_recurse(off,dt->u.compnd.memb[i].type,free_func,free_info)<0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "Unable to free compound field");
                        nelem--;
                    } /* end while */
                } /* end if */
            } /* end for */
            break;

        /* Recurse on the VL information if it's VL or compound, then free VL sequence */
        case H5T_VLEN:
            {
                hvl_t *vl=(hvl_t *)elem;    /* Temp. ptr to the vl info */

                /* Recurse if it's VL or compound */
                if(dt->parent->type==H5T_COMPOUND || dt->parent->type==H5T_VLEN) {
                    void *off;     /* offset of field */

                    /* Calculate the offset of each array element and recurse on it */
                    while(vl->len>0) {
                        off=((uint8_t *)vl->p)+(vl->len-1)*dt->parent->size;
                        if(H5T_vlen_reclaim_recurse(off,dt->parent,free_func,free_info)<0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "Unable to free VL element");
                        vl->len--;
                    } /* end while */
                } /* end if */

                /* Free the VL sequence */
                if(free_func!=NULL)
                    (*free_func)(vl->p,free_info);
                else
                    H5MM_xfree(vl->p);
            } /* end case */
            break;

        default:
            break;
    } /* end switch */

done:
    FUNC_LEAVE(ret_value);
}   /* end H5T_vlen_reclaim_recurse() */


/*--------------------------------------------------------------------------
 NAME
    H5T_vlen_reclaim
 PURPOSE
    Default method to reclaim any VL data for a buffer element
 USAGE
    herr_t H5T_vlen_reclaim(elem,type_id,ndim,point,op_data)
        void *elem;  IN/OUT: Pointer to the dataset element
        hid_t type_id;   IN: Datatype of dataset element
        hsize_t ndim;    IN: Number of dimensions in dataspace
        hssize_t *point; IN: Coordinate location of element in dataspace
        void *op_data    IN: Operator data
        
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Frees any dynamic memory used by VL datatypes in the current dataset
    element.  Recursively descends compound datatypes to free all VL datatype
    information allocated by any field.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t 
H5T_vlen_reclaim(void *elem, hid_t type_id, hsize_t UNUSED ndim, hssize_t UNUSED *point, void *op_data)
{
    H5F_xfer_t	   *xfer_parms = (H5F_xfer_t *)op_data; /* Dataset transfer plist from iterator */
    H5T_t	*dt = NULL;
    herr_t ret_value = FAIL;

    FUNC_ENTER(H5T_vlen_reclaim, FAIL);

    assert(elem);
    assert(H5I_DATATYPE == H5I_get_type(type_id));

    /* Check args */
    if (H5I_DATATYPE!=H5I_get_type(type_id) || NULL==(dt=H5I_object(type_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

    /* Pull the free function and free info pointer out of the op_data and call the recurse datatype free function */
    ret_value=H5T_vlen_reclaim_recurse(elem,dt,xfer_parms->vlen_free,xfer_parms->free_info);

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE(ret_value);
}   /* end H5T_vlen_reclaim() */


/*--------------------------------------------------------------------------
 NAME
    H5T_vlen_mark
 PURPOSE
    Recursively mark any VL datatypes as on disk/in memory
 USAGE
    herr_t H5T_vlen_mark(dt,f,loc)
        H5T_t *dt;              IN/OUT: Pointer to the datatype to mark
        H5F_t *dt;              IN: Pointer to the file the datatype is in
        H5T_vlen_type_t loc     IN: location of VL type
        
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Recursively descends any VL or compound datatypes to mark all VL datatypes
    as either on disk or in memory.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5T_vlen_mark(H5T_t *dt, H5F_t *f, H5T_vlen_type_t loc)
{
    intn i;   /* local counting variable */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER(H5T_vlen_mark, FAIL);

    assert(dt);
    assert(loc>H5T_VLEN_BADTYPE && loc<H5T_VLEN_MAXTYPE);

    /* Check the datatype of this element */
    switch(dt->type) {
        /* Check each field and recurse on VL and compound ones */
        case H5T_COMPOUND:
            for (i=0; i<dt->u.compnd.nmembs; i++) {
                /* Recurse if it's VL or compound */
                if(dt->u.compnd.memb[i].type->type==H5T_COMPOUND || dt->u.compnd.memb[i].type->type==H5T_VLEN) {
                    if(H5T_vlen_mark(dt->u.compnd.memb[i].type,f,loc)<0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "Unable to set VL location");
                } /* end if */
            } /* end for */
            break;

        /* Recurse on the VL information if it's VL or compound, then free VL sequence */
        case H5T_VLEN:
            /* Recurse if it's VL or compound */
            if(dt->parent->type==H5T_COMPOUND || dt->parent->type==H5T_VLEN) {
                if(H5T_vlen_mark(dt->parent,f,loc)<0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "Unable to set VL location");
            } /* end if */

            /* Mark this VL sequence */
            if(H5T_vlen_set_loc(dt,f,loc)<0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "Unable to set VL location");
            break;

        default:
            break;
    } /* end switch */

done:
    FUNC_LEAVE(ret_value);
}   /* end H5T_vlen_mark() */
