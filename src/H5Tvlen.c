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
#include <H5MMprivate.h>    /* Memory Allocation */
#include <H5Tpkg.h>         /* Datatypes */

#define PABLO_MASK	H5Tvlen_mask

/* Interface initialization */
static intn interface_initialize_g = 0;
#define INTERFACE_INIT NULL


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
herr_t H5T_vlen_set_loc(H5T_t *dt, H5F_t *f, H5T_vlen_type_t loc)
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
}   /* end H5T_vlen_disk_write() */


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
herr_t H5T_vlen_mem_alloc(H5F_t UNUSED *f, void *vl_addr, hsize_t seq_len, hsize_t base_size)
{
    hvl_t *vl=(hvl_t *)vl_addr;   /* Pointer to the user's hvl_t information */

    FUNC_ENTER (H5T_vlen_mem_alloc, FAIL);

    /* check parameters */
    assert(vl);

	if(NULL==(vl->p=H5MM_malloc(seq_len*base_size)))
	    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for VL data");
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
herr_t H5T_vlen_disk_alloc(H5F_t UNUSED *f, void UNUSED *vl_addr, hsize_t UNUSED seq_len, hsize_t UNUSED base_size)
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

