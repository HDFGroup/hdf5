/*
 * Copyright (C) 2000 NCSA
 *		      All rights reserved.
 *
 * Programmer: 	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *	       	Thursday, September 28, 2000
 *
 * Purpose:	Contiguous dataset I/O functions.  These routines are similar
 *      to the H5F_istore_* routines and really only abstract away dealing
 *      with the data sieve buffer from the H5F_arr_read/write and
 *      H5F_seg_read/write.
 *
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5FDprivate.h>	/*file driver				  */
#include <H5MMprivate.h>

/* Interface initialization */
#define PABLO_MASK	H5Fcontig_mask
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT NULL


/*-------------------------------------------------------------------------
 * Function:	H5F_contig_read
 *
 * Purpose:	Reads some data from a dataset into a buffer.
 *		The data is contiguous.	 The address is relative to the base
 *		address for the file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 28, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_contig_read(H5F_t *f, haddr_t addr, hsize_t size, hid_t dxpl_id,
	       void *_buf/*out*/)
{
    uint8_t	*buf = (uint8_t*)_buf;		/*cast for arithmetic	*/
    haddr_t	eoa;		        /*end of file address		*/
   
    FUNC_ENTER(H5F_contig_read, FAIL);

    /* Check args */
    assert(f);
    assert(size<SIZET_MAX);
    assert(buf);

    /* Check if data sieving is enabled */
    if(f->shared->lf->feature_flags&H5FD_FEAT_DATA_SIEVE) {
        /* Try reading from the data sieve buffer */
        if(f->shared->sieve_buf) {
            /* If entire read is within the sieve buffer, read it from the buffer */
            if((addr>=f->shared->sieve_loc && addr<(f->shared->sieve_loc+f->shared->sieve_size))
                    && ((addr+size-1)>=f->shared->sieve_loc && (addr+size-1)<(f->shared->sieve_loc+f->shared->sieve_size))) {
                /* Grab the data out of the buffer */
                HDmemcpy(buf,f->shared->sieve_buf+(addr-f->shared->sieve_loc),size);
            } /* end if */
            /* Entire request is not within this data sieve buffer */
            else {
                /* Check if we can actually hold the I/O request in the sieve buffer */
                if(size>f->shared->sieve_buf_size) {
                    /* Check for any overlap with the current sieve buffer */
                    if((f->shared->sieve_loc>=addr && f->shared->sieve_loc<(addr+size))
                            || ((f->shared->sieve_loc+f->shared->sieve_size-1)>=addr && (f->shared->sieve_loc+f->shared->sieve_size-1)<(addr+size))) {
                        /* Flush the sieve buffer, if it's dirty */
                        if(f->shared->sieve_dirty) {
                            /* Write to file */
                            if (H5F_block_write(f, H5FD_MEM_DRAW, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                                HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                  "block write failed");
                            }

                            /* Reset sieve buffer dirty flag */
                            f->shared->sieve_dirty=0;
                        } /* end if */
                    } /* end if */

                    /* Read directly into the user's buffer */
                    if (H5F_block_read(f, addr, size, dxpl_id, buf)<0) {
                        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                  "block read failed");
                    }
                } /* end if */
                /* Element size fits within the buffer size */
                else {
                    /* Flush the sieve buffer if it's dirty */
                    if(f->shared->sieve_dirty) {
                        /* Write to file */
                        if (H5F_block_write(f, H5FD_MEM_DRAW, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                            HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                              "block write failed");
                        }

                        /* Reset sieve buffer dirty flag */
                        f->shared->sieve_dirty=0;
                    } /* end if */

                    /* Determine the new sieve buffer size & location */
                    f->shared->sieve_loc=addr;

                    /* Make certain we don't read off the end of the file */
                    if (HADDR_UNDEF==(eoa=H5FD_get_eoa(f->shared->lf))) {
                        HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                            "unable to determine file size");
                    }
                    f->shared->sieve_size=MIN(eoa-addr,f->shared->sieve_buf_size);

                    /* Read the new sieve buffer */
                    if (H5F_block_read(f, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                  "block read failed");
                    }

                    /* Reset sieve buffer dirty flag */
                    f->shared->sieve_dirty=0;

                    /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                    HDmemcpy(buf,f->shared->sieve_buf,size);
                } /* end else */
            } /* end else */
        } /* end if */
        /* No data sieve buffer yet, go allocate one */
        else {
            /* Check if we can actually hold the I/O request in the sieve buffer */
            if(size>f->shared->sieve_buf_size) {
                if (H5F_block_read(f, addr, size, dxpl_id, buf)<0) {
                    HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                              "block read failed");
                }
            } /* end if */
            else {
                /* Allocate room for the data sieve buffer */
                if (NULL==(f->shared->sieve_buf=H5MM_malloc(f->shared->sieve_buf_size))) {
                    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                          "memory allocation failed");
                }

                /* Determine the new sieve buffer size & location */
                f->shared->sieve_loc=addr;

                /* Make certain we don't read off the end of the file */
                if (HADDR_UNDEF==(eoa=H5FD_get_eoa(f->shared->lf))) {
                    HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "unable to determine file size");
                }
                f->shared->sieve_size=MIN(eoa-addr,f->shared->sieve_buf_size);

                /* Read the new sieve buffer */
                if (H5F_block_read(f, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                    HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                              "block read failed");
                }

                /* Reset sieve buffer dirty flag */
                f->shared->sieve_dirty=0;

                /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                HDmemcpy(buf,f->shared->sieve_buf,size);
            } /* end else */
        } /* end else */
    } /* end if */
    else {
        if (H5F_block_read(f, addr, size, dxpl_id, buf)<0) {
            HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                      "block read failed");
        }
    } /* end else */

    FUNC_LEAVE(SUCCEED);
}   /* End H5F_contig_read() */


/*-------------------------------------------------------------------------
 * Function:	H5F_contig_write
 *
 * Purpose:	Writes some data from a dataset into a buffer.
 *		The data is contiguous.	 The address is relative to the base
 *		address for the file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 28, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_contig_write(H5F_t *f, H5FD_mem_t type, haddr_t addr, hsize_t size,
        hid_t dxpl_id, const void *buf)
{
    haddr_t	eoa;		        /*end of file address		*/

    FUNC_ENTER(H5F_block_write, FAIL);

    assert (f);
    assert (size<SIZET_MAX);
    assert (buf);

    /* Check if data sieving is enabled */
    if(f->shared->lf->feature_flags&H5FD_FEAT_DATA_SIEVE) {
        /* Try writing to the data sieve buffer */
        if(f->shared->sieve_buf) {
            /* If entire write is within the sieve buffer, write it to the buffer */
            if((addr>=f->shared->sieve_loc && addr<(f->shared->sieve_loc+f->shared->sieve_size))
                    && ((addr+size-1)>=f->shared->sieve_loc && (addr+size-1)<(f->shared->sieve_loc+f->shared->sieve_size))) {
                /* Grab the data out of the buffer */
                HDmemcpy(f->shared->sieve_buf+(addr-f->shared->sieve_loc),buf,size);

                /* Set sieve buffer dirty flag */
                f->shared->sieve_dirty=1;

            } /* end if */
            /* Entire request is not within this data sieve buffer */
            else {
                /* Check if we can actually hold the I/O request in the sieve buffer */
                if(size>f->shared->sieve_buf_size) {
                    /* Check for any overlap with the current sieve buffer */
                    if((f->shared->sieve_loc>=addr && f->shared->sieve_loc<(addr+size))
                            || ((f->shared->sieve_loc+f->shared->sieve_size-1)>=addr && (f->shared->sieve_loc+f->shared->sieve_size-1)<(addr+size))) {
                        /* Flush the sieve buffer, if it's dirty */
                        if(f->shared->sieve_dirty) {
                            /* Write to file */
                            if (H5F_block_write(f, H5FD_MEM_DRAW, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                                HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                  "block write failed");
                            }

                            /* Reset sieve buffer dirty flag */
                            f->shared->sieve_dirty=0;
                        } /* end if */

                        /* Force the sieve buffer to be re-read the next time */
                        f->shared->sieve_loc=HADDR_UNDEF;
                        f->shared->sieve_size=0;
                    } /* end if */

                    /* Write directly from the user's buffer */
                    if (H5F_block_write(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0) {
                        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                  "block write failed");
                    }
                } /* end if */
                /* Element size fits within the buffer size */
                else {
                    /* Flush the sieve buffer if it's dirty */
                    if(f->shared->sieve_dirty) {
                        /* Write to file */
                        if (H5F_block_write(f, H5FD_MEM_DRAW, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                            HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                              "block write failed");
                        }

                        /* Reset sieve buffer dirty flag */
                        f->shared->sieve_dirty=0;
                    } /* end if */

                    /* Determine the new sieve buffer size & location */
                    f->shared->sieve_loc=addr;

                    /* Make certain we don't read off the end of the file */
                    if (HADDR_UNDEF==(eoa=H5FD_get_eoa(f->shared->lf))) {
                        HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                            "unable to determine file size");
                    }
                    f->shared->sieve_size=MIN(eoa-addr,f->shared->sieve_buf_size);

                    /* Read the new sieve buffer */
                    if (H5F_block_read(f, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                  "block read failed");
                    }

                    /* Grab the data out of the buffer (must be first piece of data in buffer) */
                    HDmemcpy(f->shared->sieve_buf,buf,size);

                    /* Set sieve buffer dirty flag */
                    f->shared->sieve_dirty=1;

                } /* end else */
            } /* end else */
        } /* end if */
        /* No data sieve buffer yet, go allocate one */
        else {
            /* Check if we can actually hold the I/O request in the sieve buffer */
            if(size>f->shared->sieve_buf_size) {
                if (H5F_block_write(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0) {
                    HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                              "block write failed");
                }
            } /* end if */
            else {
                /* Allocate room for the data sieve buffer */
                if (NULL==(f->shared->sieve_buf=H5MM_malloc(f->shared->sieve_buf_size))) {
                    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                          "memory allocation failed");
                }

                /* Determine the new sieve buffer size & location */
                f->shared->sieve_loc=addr;

                /* Make certain we don't read off the end of the file */
                if (HADDR_UNDEF==(eoa=H5FD_get_eoa(f->shared->lf))) {
                    HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "unable to determine file size");
                }
                f->shared->sieve_size=MIN(eoa-addr,f->shared->sieve_buf_size);

                /* Read the new sieve buffer */
                if (H5F_block_read(f, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                    HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                              "block read failed");
                }

                /* Grab the data out of the buffer (must be first piece of data in buffer) */
                HDmemcpy(f->shared->sieve_buf,buf,size);

                /* Set sieve buffer dirty flag */
                f->shared->sieve_dirty=1;
            } /* end else */
        } /* end else */
    } /* end if */
    else {
        if (H5F_block_write(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0) {
            HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                      "block write failed");
        }
    } /* end else */

    FUNC_LEAVE(SUCCEED);
}   /* End H5F_contig_write() */
