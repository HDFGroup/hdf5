/*
 * Copyright (C) 2000-2001 NCSA
 *		           All rights reserved.
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

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

#include "H5private.h"
#include "H5Eprivate.h"
#include "H5Fpkg.h"
#include "H5FDprivate.h"	/*file driver				  */
#include "H5MMprivate.h"

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
 *              Re-written in terms of the new readv call, QAK, 7/7/01
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_contig_read(H5F_t *f, hsize_t max_data, H5FD_mem_t type, haddr_t addr,
    size_t size, hid_t dxpl_id, void *buf/*out*/)
{
    hsize_t offset=0;   /* Offset for vector call */
   
    FUNC_ENTER(H5F_contig_read, FAIL);

    /* Check args */
    assert(f);
    assert(buf);

    if (H5F_contig_readv(f, max_data, type, addr, 1, &size, &offset, dxpl_id, buf)<0)
        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, "vector read failed");

    FUNC_LEAVE(SUCCEED);
}   /* end H5F_contig_read() */


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
 *              Re-written in terms of the new readv call, QAK, 7/7/01
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_contig_write(H5F_t *f, hsize_t max_data, H5FD_mem_t type, haddr_t addr, size_t size,
        hid_t dxpl_id, const void *buf)
{
    hsize_t offset=0;   /* Offset for vector call */

    FUNC_ENTER(H5F_contig_write, FAIL);

    assert (f);
    assert (buf);

    if (H5F_contig_writev(f, max_data, type, addr, 1, &size, &offset, dxpl_id, buf)<0)
        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "vector write failed");

    FUNC_LEAVE(SUCCEED);
}   /* end H5F_contig_write() */


/*-------------------------------------------------------------------------
 * Function:	H5F_contig_readv
 *
 * Purpose:	Reads some data vectors from a dataset into a buffer.
 *		The data is contiguous.	 The address is the start of the dataset,
 *              relative to the base address for the file and the offsets and
 *              sequence lengths are in bytes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 3, 2001
 *
 * Notes:
 *      Offsets in the sequences must be monotonically increasing
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_contig_readv(H5F_t *f, hsize_t _max_data, H5FD_mem_t type, haddr_t _addr,
    size_t nseq, size_t size_arr[], hsize_t offset_arr[], hid_t dxpl_id,
    void *_buf/*out*/)
{
    unsigned char *buf=(unsigned char *)_buf;      /* Pointer to buffer to fill */
    haddr_t abs_eoa;	        /* Absolute end of file address		*/
    haddr_t rel_eoa;	        /* Relative end of file address		*/
    haddr_t addr;               /* Actual address to read */
    hsize_t max_data;           /* Actual maximum size of data to cache */
    size_t size;                /* Size of sequence in bytes */
    size_t u;                   /* Counting variable */
#ifndef SLOW_WAY
    size_t max_seq;        /* Maximum sequence to copy */
    haddr_t temp_end;      /* Temporary end of buffer variable */
    size_t max_search;     /* Maximum number of sequences to search */
    size_t mask;           /* Bit mask */
    intn bit_loc;          /* Bit location of the leftmost '1' in max_search */
    size_t *size_arr_p;    /* Pointer into the size array */
    hsize_t *offset_arr_p; /* Pointer into the offset array */
#endif /* SLOW_WAY */
   
    FUNC_ENTER(H5F_contig_readv, FAIL);

    /* Check args */
    assert(f);
    assert(buf);

    /* Check if data sieving is enabled */
    if(f->shared->lf->feature_flags&H5FD_FEAT_DATA_SIEVE) {

        /* Outer loop, guarantees working through all the sequences */
        for(u=0; u<nseq; ) {

            /* Try reading from the data sieve buffer */
            if(f->shared->sieve_buf) {
                haddr_t sieve_start, sieve_end;     /* Start & end locations of sieve buffer */
                haddr_t contig_end;             /* End locations of block to write */
                size_t sieve_size;              /* size of sieve buffer */

                /* Stash local copies of these value */
                sieve_start=f->shared->sieve_loc;
                sieve_size=f->shared->sieve_size;
                sieve_end=sieve_start+sieve_size;
                
                /* Next-outer loop works through sequences as fast as possible */
                for(; u<nseq; ) {
                    size=size_arr[u];
                    addr=_addr+offset_arr[u];

                    /* Compute end of sequence to retrieve */
                    contig_end=addr+size-1;

                    /* If entire read is within the sieve buffer, read it from the buffer */
                    if(addr>=sieve_start && contig_end<sieve_end) {
                        unsigned char *base_sieve_buf=f->shared->sieve_buf+(_addr-sieve_start);
                        unsigned char *temp_sieve_buf;
                        haddr_t temp_addr=_addr-1;      /* Temporary address */

#ifdef SLOW_WAY
                        /* Retrieve all the sequences out of the current sieve buffer */
                        while(contig_end<sieve_end) {
                            /* Set the location within the sieve buffer to the correct offset */
                            temp_sieve_buf=base_sieve_buf+offset_arr[u];

                            /* Grab the data out of the buffer */
                            HDmemcpy(buf,temp_sieve_buf,size_arr[u]);

                            /* Increment offset in buffer */
                            buf += size_arr[u];

                            /* Increment sequence number, check for finished with sequences */
                            if((++u) >= nseq)
                                break;

                            /* Re-compute end of sequence to retrieve */
                            contig_end=temp_addr+offset_arr[u]+size_arr[u];
                        } /* end while */
#else /* SLOW_WAY */
                        /* Find log2(n) where n is the number of elements to search */

                        /* Set initial parameters */
                        mask=(size_t)0xff<<((sizeof(size_t)-1)*8);  /* Get a mask for the leftmost byte */
                        max_search=((nseq-1)-u)+1;          /* Compute 'n' for the log2 */
                        assert(max_search>0);               /* Sanity check */
                        bit_loc=(sizeof(size_t)*8)-1;       /* Initial bit location */

                        /* Search for the first byte with a bit set */
                        while((max_search & mask)==0) {
                            mask>>=8;
                            bit_loc-=8;
                        } /* end while */

                        /* Switch to searching for a bit */
                        mask=1<<bit_loc;
                        while((max_search & mask)==0) {
                            mask>>=1;
                            bit_loc--;
                        } /* end while */

                        /* location of the leftmost bit, plus 1, is log2(n) */
                        max_seq=bit_loc+1;

                        /* Don't walk off the array */
                        max_seq=MIN(u+max_seq,nseq-1);

                        /* Determine if a linear search is faster than a binary search */
                        temp_end=temp_addr+offset_arr[max_seq]+size_arr[max_seq];
                        if(temp_end>=sieve_end) {
                            /* Linear search is faster */

                            /* Set the initial search values */
                            max_seq=u;
                            temp_end=temp_addr+offset_arr[max_seq]+size_arr[max_seq];

                            /* Search for the first sequence ending greater than the sieve buffer end */
                            while(temp_end<sieve_end) {
                                if(++max_seq>=nseq)
                                    break;
                                temp_end=temp_addr+offset_arr[max_seq]+size_arr[max_seq];
                            } /* end while */

                            /* Adjust back one element */
                            max_seq--;

                        } /* end if */
                        else {
                            size_t lo,hi;           /* Low and high bounds for binary search */
                            uintn found=0;          /* Flag to indicate bounds have been found */

                            /* Binary search is faster */

                            /* Find the value 'u' which will be beyond the end of the sieve buffer */
                            lo=u;
                            hi=nseq-1;
                            max_seq=(lo+hi)/2;
                            while(!found) {
                                /* Get the address of the end of sequence for the 'max_seq' position */
                                temp_end=temp_addr+offset_arr[max_seq]+size_arr[max_seq];

                                /* Current high bound is too large */
                                if(temp_end>=sieve_end) {
                                    if((lo+1)<hi) {
                                        hi=max_seq;
                                        max_seq=(lo+hi)/2;
                                    } /* end if */
                                    else {
                                        found=1;
                                    } /* end else */
                                } /* end if */
                                /* Current low bound is too small */
                                else {
                                    if((lo+1)<hi) {
                                        lo=max_seq;
                                        max_seq=(lo+hi+1)/2;
                                    } /* end if */
                                    else {
                                        found=1;
                                    } /* end else */
                                } /* end else */
                            } /* end while */

                            /* Check for non-exact match */
                            if(lo!=hi) {
                                temp_end=temp_addr+offset_arr[hi]+size_arr[hi];
                                if(temp_end<sieve_end)
                                    max_seq=hi;
                                else
                                    max_seq=lo;
                            } /* end if */
                        } /* end else */

                        /* Set the pointers to the correct locations in the offset & size arrays */
                        size_arr_p=&size_arr[u];
                        offset_arr_p=&offset_arr[u];

#ifdef NO_DUFFS_DEVICE
                        /* Retrieve all the sequences out of the current sieve buffer */
                        while(u<=max_seq) {
                            /* Set the location within the sieve buffer to the correct offset */
                            temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                            /* Grab the data out of the buffer */
                            HDmemcpy(buf,temp_sieve_buf,*size_arr_p);

                            /* Increment offset in buffer */
                            buf += *size_arr_p++;

                            /* Increment the offset in the array */
                            u++;
                        } /* end while */
#else /* NO_DUFFS_DEVICE */
{
    size_t seq_count;

                    seq_count=(max_seq-u)+1;
                    switch (seq_count % 4) {
                        case 0:
                            do
                              {
                                /* Set the location within the sieve buffer to the correct offset */
                                temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                                /* Grab the data out of the buffer */
                                HDmemcpy(buf,temp_sieve_buf,*size_arr_p);

                                /* Increment offset in buffer */
                                buf += *size_arr_p++;

                                /* Increment the offset in the array */
                                u++;

                        case 3:
                                /* Set the location within the sieve buffer to the correct offset */
                                temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                                /* Grab the data out of the buffer */
                                HDmemcpy(buf,temp_sieve_buf,*size_arr_p);

                                /* Increment offset in buffer */
                                buf += *size_arr_p++;

                                /* Increment the offset in the array */
                                u++;

                        case 2:
                                /* Set the location within the sieve buffer to the correct offset */
                                temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                                /* Grab the data out of the buffer */
                                HDmemcpy(buf,temp_sieve_buf,*size_arr_p);

                                /* Increment offset in buffer */
                                buf += *size_arr_p++;

                                /* Increment the offset in the array */
                                u++;

                        case 1:
                                /* Set the location within the sieve buffer to the correct offset */
                                temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                                /* Grab the data out of the buffer */
                                HDmemcpy(buf,temp_sieve_buf,*size_arr_p);

                                /* Increment offset in buffer */
                                buf += *size_arr_p++;

                                /* Increment the offset in the array */
                                u++;

                          } while (u<=max_seq);
                    } /* end switch */

}
#endif /* NO_DUFFS_DEVICE */
#endif /* SLOW_WAY */
                    } /* end if */
                    /* Entire request is not within this data sieve buffer */
                    else {
                        /* Check if we can actually hold the I/O request in the sieve buffer */
                        if(size>f->shared->sieve_buf_size) {
                            /* Check for any overlap with the current sieve buffer */
                            if((sieve_start>=addr && sieve_start<(contig_end+1))
                                    || ((sieve_end-1)>=addr && (sieve_end-1)<(contig_end+1))) {
                                /* Flush the sieve buffer, if it's dirty */
                                if(f->shared->sieve_dirty) {
                                    /* Write to file */
                                    if (H5F_block_write(f, H5FD_MEM_DRAW, sieve_start, sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                                        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                          "block write failed");
                                    }

                                    /* Reset sieve buffer dirty flag */
                                    f->shared->sieve_dirty=0;
                                } /* end if */
                            } /* end if */

                            /* Read directly into the user's buffer */
                            if (H5F_block_read(f, type, addr, size, dxpl_id, buf)<0) {
                                HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                          "block read failed");
                            }
                        } /* end if */
                        /* Element size fits within the buffer size */
                        else {
                            /* Flush the sieve buffer if it's dirty */
                            if(f->shared->sieve_dirty) {
                                /* Write to file */
                                if (H5F_block_write(f, H5FD_MEM_DRAW, sieve_start, sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                                    HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                      "block write failed");
                                }

                                /* Reset sieve buffer dirty flag */
                                f->shared->sieve_dirty=0;
                            } /* end if */

                            /* Determine the new sieve buffer size & location */
                            f->shared->sieve_loc=addr;

                            /* Make certain we don't read off the end of the file */
                            if (HADDR_UNDEF==(abs_eoa=H5FD_get_eoa(f->shared->lf))) {
                                HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                                    "unable to determine file size");
                            }

                            /* Adjust absolute EOA address to relative EOA address */
                            rel_eoa=abs_eoa-f->shared->base_addr;

                            /* Only need this when resizing sieve buffer */
                            max_data=_max_data-offset_arr[u];

                            /* Compute the size of the sieve buffer */
                            /* Don't read off the end of the file, don't read past the end of the data element and don't read more than the buffer size */
                            f->shared->sieve_size=MIN(rel_eoa-f->shared->sieve_loc,MIN(max_data,f->shared->sieve_buf_size));

                            /* Update local copies of sieve information */
                            sieve_start=f->shared->sieve_loc;
                            sieve_size=f->shared->sieve_size;
                            sieve_end=sieve_start+sieve_size;

                            /* Read the new sieve buffer */
                            if (H5F_block_read(f, type, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                                HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                          "block read failed");
                            }

                            /* Reset sieve buffer dirty flag */
                            f->shared->sieve_dirty=0;

                            /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                            HDmemcpy(buf,f->shared->sieve_buf,size);
                        } /* end else */

                        /* Increment offset in buffer */
                        buf += size_arr[u];

                        /* Increment sequence number */
                        u++;
                    } /* end else */
                } /* end for */
            } /* end if */
            /* No data sieve buffer yet, go allocate one */
            else {
                /* Set up the buffer parameters */
                size=size_arr[u];
                addr=_addr+offset_arr[u];
                max_data=_max_data-offset_arr[u];

                /* Check if we can actually hold the I/O request in the sieve buffer */
                if(size>f->shared->sieve_buf_size) {
                    if (H5F_block_read(f, type, addr, size, dxpl_id, buf)<0) {
                        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                  "block read failed");
                    }
                } /* end if */
                else {
                    /* Allocate room for the data sieve buffer */
                    if (NULL==(f->shared->sieve_buf=H5MM_malloc(f->shared->sieve_buf_size))) {
                        HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                              "memory allocation failed");
                    }

                    /* Determine the new sieve buffer size & location */
                    f->shared->sieve_loc=addr;

                    /* Make certain we don't read off the end of the file */
                    if (HADDR_UNDEF==(abs_eoa=H5FD_get_eoa(f->shared->lf))) {
                        HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                            "unable to determine file size");
                    }

                    /* Adjust absolute EOA address to relative EOA address */
                    rel_eoa=abs_eoa-f->shared->base_addr;

                    /* Compute the size of the sieve buffer */
                    f->shared->sieve_size=MIN(rel_eoa-f->shared->sieve_loc,MIN(max_data,f->shared->sieve_buf_size));

                    /* Read the new sieve buffer */
                    if (H5F_block_read(f, type, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                  "block read failed");
                    }

                    /* Reset sieve buffer dirty flag */
                    f->shared->sieve_dirty=0;

                    /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                    HDmemcpy(buf,f->shared->sieve_buf,size);
                } /* end else */

                /* Increment offset in buffer */
                buf += size_arr[u];

                /* Increment sequence number */
                u++;
            } /* end else */
        } /* end for */
    } /* end if */
    else {
        /* Work through all the sequences */
        for(u=0; u<nseq; u++) {
            size=size_arr[u];
            addr=_addr+offset_arr[u];

            if (H5F_block_read(f, type, addr, size, dxpl_id, buf)<0) {
                HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                          "block read failed");
            }

            /* Increment offset in buffer */
            buf += size_arr[u];
        } /* end for */
    } /* end else */

    FUNC_LEAVE(SUCCEED);
}   /* end H5F_contig_readv() */


/*-------------------------------------------------------------------------
 * Function:	H5F_contig_writev
 *
 * Purpose:	Writes some data vectors into a dataset from a buffer.
 *		The data is contiguous.	 The address is the start of the dataset,
 *              relative to the base address for the file and the offsets and
 *              sequence lengths are in bytes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 5, 2001
 *
 * Notes:
 *      Offsets in the sequences must be monotonically increasing
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_contig_writev(H5F_t *f, hsize_t _max_data, H5FD_mem_t type, haddr_t _addr,
    size_t nseq, size_t size_arr[], hsize_t offset_arr[], hid_t dxpl_id,
    const void *_buf)
{
    const unsigned char *buf=_buf;      /* Pointer to buffer to fill */
    haddr_t abs_eoa;	        /* Absolute end of file address		*/
    haddr_t rel_eoa;	        /* Relative end of file address		*/
    haddr_t addr;               /* Actual address to read */
    hsize_t max_data;           /* Actual maximum size of data to cache */
    size_t size;                /* Size of sequence in bytes */
    size_t u;                   /* Counting variable */
#ifndef SLOW_WAY
    size_t max_seq;        /* Maximum sequence to copy */
    haddr_t temp_end;      /* Temporary end of buffer variable */
    size_t max_search;     /* Maximum number of sequences to search */
    size_t mask;           /* Bit mask */
    intn bit_loc;          /* Bit location of the leftmost '1' in max_search */
    size_t *size_arr_p;    /* Pointer into the size array */
    hsize_t *offset_arr_p; /* Pointer into the offset array */
#endif /* SLOW_WAY */
   
    FUNC_ENTER(H5F_contig_writev, FAIL);

    /* Check args */
    assert(f);
    assert(buf);

    /* Check if data sieving is enabled */
    if(f->shared->lf->feature_flags&H5FD_FEAT_DATA_SIEVE) {

        /* Outer loop, guarantees working through all the sequences */
        for(u=0; u<nseq; ) {

            /* Try writing into the data sieve buffer */
            if(f->shared->sieve_buf) {
                haddr_t sieve_start, sieve_end;     /* Start & end locations of sieve buffer */
                haddr_t contig_end;             /* End locations of block to write */
                size_t sieve_size;              /* size of sieve buffer */

                /* Stash local copies of these value */
                sieve_start=f->shared->sieve_loc;
                sieve_size=f->shared->sieve_size;
                sieve_end=sieve_start+sieve_size;
                
                /* Next-outer loop works through sequences as fast as possible */
                for(; u<nseq; ) {
                    size=size_arr[u];
                    addr=_addr+offset_arr[u];

                    /* Compute end of sequence to retrieve */
                    contig_end=addr+size-1;

                    /* If entire write is within the sieve buffer, write it to the buffer */
                    if(addr>=sieve_start && contig_end<sieve_end) {
                        unsigned char *base_sieve_buf=f->shared->sieve_buf+(_addr-sieve_start);
                        unsigned char *temp_sieve_buf;
                        haddr_t temp_addr=_addr-1;      /* Temporary address */

#ifdef SLOW_WAY
                        /* Retrieve all the sequences out of the current sieve buffer */
                        while(contig_end<sieve_end) {
                            /* Set the location within the sieve buffer to the correct offset */
                            temp_sieve_buf=base_sieve_buf+offset_arr[u];

                            /* Grab the data out of the buffer */
                            HDmemcpy(temp_sieve_buf,buf,size_arr[u]);

                            /* Increment offset in buffer */
                            buf += size_arr[u];

                            /* Increment sequence number, check for finished with sequences */
                            if((++u) >= nseq)
                                break;

                            /* Re-compute end of sequence to retrieve */
                            contig_end=temp_addr+offset_arr[u]+size_arr[u];
                        } /* end while */
#else /* SLOW_WAY */
                        /* Find log2(n) where n is the number of elements to search */

                        /* Set initial parameters */
                        mask=(size_t)0xff<<((sizeof(size_t)-1)*8);  /* Get a mask for the leftmost byte */
                        max_search=((nseq-1)-u)+1;          /* Compute 'n' for the log2 */
                        assert(max_search>0);               /* Sanity check */
                        bit_loc=(sizeof(size_t)*8)-1;       /* Initial bit location */

                        /* Search for the first byte with a bit set */
                        while((max_search & mask)==0) {
                            mask>>=8;
                            bit_loc-=8;
                        } /* end while */

                        /* Switch to searching for a bit */
                        mask=1<<bit_loc;
                        while((max_search & mask)==0) {
                            mask>>=1;
                            bit_loc--;
                        } /* end while */

                        /* location of the leftmost bit, plus 1, is log2(n) */
                        max_seq=bit_loc+1;

                        /* Don't walk off the array */
                        max_seq=MIN(u+max_seq,nseq-1);

                        /* Determine if a linear search is faster than a binary search */
                        temp_end=temp_addr+offset_arr[max_seq]+size_arr[max_seq];
                        if(temp_end>=sieve_end) {
                            /* Linear search is faster */

                            /* Set the initial search values */
                            max_seq=u;
                            temp_end=temp_addr+offset_arr[max_seq]+size_arr[max_seq];

                            /* Search for the first sequence ending greater than the sieve buffer end */
                            while(temp_end<sieve_end) {
                                if(++max_seq>=nseq)
                                    break;
                                temp_end=temp_addr+offset_arr[max_seq]+size_arr[max_seq];
                            } /* end while */

                            /* Adjust back one element */
                            max_seq--;

                        } /* end if */
                        else {
                            size_t lo,hi;           /* Low and high bounds for binary search */
                            uintn found=0;          /* Flag to indicate bounds have been found */

                            /* Binary search is faster */

                            /* Find the value 'u' which will be beyond the end of the sieve buffer */
                            lo=u;
                            hi=nseq-1;
                            max_seq=(lo+hi)/2;
                            while(!found) {
                                /* Get the address of the end of sequence for the 'max_seq' position */
                                temp_end=temp_addr+offset_arr[max_seq]+size_arr[max_seq];

                                /* Current high bound is too large */
                                if(temp_end>=sieve_end) {
                                    if((lo+1)<hi) {
                                        hi=max_seq;
                                        max_seq=(lo+hi)/2;
                                    } /* end if */
                                    else {
                                        found=1;
                                    } /* end else */
                                } /* end if */
                                /* Current low bound is too small */
                                else {
                                    if((lo+1)<hi) {
                                        lo=max_seq;
                                        max_seq=(lo+hi+1)/2;
                                    } /* end if */
                                    else {
                                        found=1;
                                    } /* end else */
                                } /* end else */
                            } /* end while */

                            /* Check for non-exact match */
                            if(lo!=hi) {
                                temp_end=temp_addr+offset_arr[hi]+size_arr[hi];
                                if(temp_end<sieve_end)
                                    max_seq=hi;
                                else
                                    max_seq=lo;
                            } /* end if */
                        } /* end else */

                        /* Set the pointers to the correct locations in the offset & size arrays */
                        size_arr_p=&size_arr[u];
                        offset_arr_p=&offset_arr[u];

#ifdef NO_DUFFS_DEVICE
                        /* Retrieve all the sequences out of the current sieve buffer */
                        while(u<=max_seq) {
                            /* Set the location within the sieve buffer to the correct offset */
                            temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                            /* Grab the data out of the buffer */
                            HDmemcpy(temp_sieve_buf,buf,*size_arr_p);

                            /* Increment offset in buffer */
                            buf += *size_arr_p++;

                            /* Increment the offset in the array */
                            u++;
                        } /* end while */
#else /* NO_DUFFS_DEVICE */
{
    size_t seq_count;

                    seq_count=(max_seq-u)+1;
                    switch (seq_count % 4) {
                        case 0:
                            do
                              {
                                /* Set the location within the sieve buffer to the correct offset */
                                temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                                /* Grab the data out of the buffer */
                                HDmemcpy(temp_sieve_buf,buf,*size_arr_p);

                                /* Increment offset in buffer */
                                buf += *size_arr_p++;

                                /* Increment the offset in the array */
                                u++;

                        case 3:
                                /* Set the location within the sieve buffer to the correct offset */
                                temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                                /* Grab the data out of the buffer */
                                HDmemcpy(temp_sieve_buf,buf,*size_arr_p);

                                /* Increment offset in buffer */
                                buf += *size_arr_p++;

                                /* Increment the offset in the array */
                                u++;

                        case 2:
                                /* Set the location within the sieve buffer to the correct offset */
                                temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                                /* Grab the data out of the buffer */
                                HDmemcpy(temp_sieve_buf,buf,*size_arr_p);

                                /* Increment offset in buffer */
                                buf += *size_arr_p++;

                                /* Increment the offset in the array */
                                u++;

                        case 1:
                                /* Set the location within the sieve buffer to the correct offset */
                                temp_sieve_buf=base_sieve_buf+*offset_arr_p++;

                                /* Grab the data out of the buffer */
                                HDmemcpy(temp_sieve_buf,buf,*size_arr_p);

                                /* Increment offset in buffer */
                                buf += *size_arr_p++;

                                /* Increment the offset in the array */
                                u++;

                          } while (u<=max_seq);
                    } /* end switch */

}
#endif /* NO_DUFFS_DEVICE */
#endif /* SLOW_WAY */
                        /* Set sieve buffer dirty flag */
                        f->shared->sieve_dirty=1;

                    } /* end if */
                    /* Entire request is not within this data sieve buffer */
                    else {
                        /* Check if we can actually hold the I/O request in the sieve buffer */
                        if(size>f->shared->sieve_buf_size) {
                            /* Check for any overlap with the current sieve buffer */
                            if((sieve_start>=addr && sieve_start<(contig_end+1))
                                    || ((sieve_end-1)>=addr && (sieve_end-1)<(contig_end+1))) {
                                /* Flush the sieve buffer, if it's dirty */
                                if(f->shared->sieve_dirty) {
                                    /* Write to file */
                                    if (H5F_block_write(f, H5FD_MEM_DRAW, sieve_start, sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
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
                            if (H5F_block_write(f, type, addr, size, dxpl_id, buf)<0) {
                                HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                          "block write failed");
                            }
                        } /* end if */
                        /* Element size fits within the buffer size */
                        else {
                            /* Check if it is possible to (exactly) prepend or append to existing (dirty) sieve buffer */
                            if(((addr+size)==sieve_start || addr==sieve_end) &&
                                    (size+sieve_size)<=f->shared->sieve_buf_size &&
                                    f->shared->sieve_dirty) {
                                /* Prepend to existing sieve buffer */
                                if((addr+size)==sieve_start) {
                                    /* Move existing sieve information to correct location */
                                    HDmemmove(f->shared->sieve_buf+size,f->shared->sieve_buf,sieve_size);

                                    /* Copy in new information (must be first in sieve buffer) */
                                    HDmemcpy(f->shared->sieve_buf,buf,size);

                                    /* Adjust sieve location */
                                    f->shared->sieve_loc=addr;
                                    
                                } /* end if */
                                /* Append to existing sieve buffer */
                                else {
                                    /* Copy in new information */
                                    HDmemcpy(f->shared->sieve_buf+sieve_size,buf,size);
                                } /* end else */

                                /* Adjust sieve size */
                                f->shared->sieve_size += size;
                                
                                /* Update local copies of sieve information */
                                sieve_start=f->shared->sieve_loc;
                                sieve_size=f->shared->sieve_size;
                                sieve_end=sieve_start+sieve_size;

                            } /* end if */
                            /* Can't add the new data onto the existing sieve buffer */
                            else {
                                /* Flush the sieve buffer if it's dirty */
                                if(f->shared->sieve_dirty) {
                                    /* Write to file */
                                    if (H5F_block_write(f, H5FD_MEM_DRAW, sieve_start, sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                                        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                          "block write failed");
                                    }

                                    /* Reset sieve buffer dirty flag */
                                    f->shared->sieve_dirty=0;
                                } /* end if */

                                /* Determine the new sieve buffer size & location */
                                f->shared->sieve_loc=addr;

                                /* Make certain we don't read off the end of the file */
                                if (HADDR_UNDEF==(abs_eoa=H5FD_get_eoa(f->shared->lf))) {
                                    HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                                        "unable to determine file size");
                                }

                                /* Adjust absolute EOA address to relative EOA address */
                                rel_eoa=abs_eoa-f->shared->base_addr;

                                /* Only need this when resizing sieve buffer */
                                max_data=_max_data-offset_arr[u];

                                /* Compute the size of the sieve buffer */
                                /* Don't read off the end of the file, don't read past the end of the data element and don't read more than the buffer size */
                                f->shared->sieve_size=MIN(rel_eoa-f->shared->sieve_loc,MIN(max_data,f->shared->sieve_buf_size));

                                /* Update local copies of sieve information */
                                sieve_start=f->shared->sieve_loc;
                                sieve_size=f->shared->sieve_size;
                                sieve_end=sieve_start+sieve_size;

                                /* Check if there is any point in reading the data from the file */
                                if(f->shared->sieve_size>size) {
                                    /* Read the new sieve buffer */
                                    if (H5F_block_read(f, type, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                                        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                                  "block read failed");
                                    } /* end if */
                                } /* end if */

                                /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                                HDmemcpy(f->shared->sieve_buf,buf,size);

                                /* Set sieve buffer dirty flag */
                                f->shared->sieve_dirty=1;

                            } /* end else */
                        } /* end else */

                        /* Increment offset in buffer */
                        buf += size_arr[u];

                        /* Increment sequence number */
                        u++;
                    } /* end else */
                } /* end for */
            } /* end if */
            /* No data sieve buffer yet, go allocate one */
            else {
                /* Set up the buffer parameters */
                size=size_arr[u];
                addr=_addr+offset_arr[u];
                max_data=_max_data-offset_arr[u];

                /* Check if we can actually hold the I/O request in the sieve buffer */
                if(size>f->shared->sieve_buf_size) {
                    if (H5F_block_write(f, type, addr, size, dxpl_id, buf)<0) {
                        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                  "block write failed");
                    }
                } /* end if */
                else {
                    /* Allocate room for the data sieve buffer */
                    if (NULL==(f->shared->sieve_buf=H5MM_malloc(f->shared->sieve_buf_size))) {
                        HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                              "memory allocation failed");
                    }

                    /* Determine the new sieve buffer size & location */
                    f->shared->sieve_loc=addr;

                    /* Make certain we don't read off the end of the file */
                    if (HADDR_UNDEF==(abs_eoa=H5FD_get_eoa(f->shared->lf))) {
                        HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                            "unable to determine file size");
                    }

                    /* Adjust absolute EOA address to relative EOA address */
                    rel_eoa=abs_eoa-f->shared->base_addr;

                    /* Compute the size of the sieve buffer */
                    f->shared->sieve_size=MIN(rel_eoa-f->shared->sieve_loc,MIN(max_data,f->shared->sieve_buf_size));

                    /* Check if there is any point in reading the data from the file */
                    if(f->shared->sieve_size>size) {
                        /* Read the new sieve buffer */
                        if (H5F_block_read(f, type, f->shared->sieve_loc, f->shared->sieve_size, dxpl_id, f->shared->sieve_buf)<0) {
                            HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
                                      "block read failed");
                        } /* end if */
                    } /* end if */

                    /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                    HDmemcpy(f->shared->sieve_buf,buf,size);

                    /* Set sieve buffer dirty flag */
                    f->shared->sieve_dirty=1;
                } /* end else */

                /* Increment offset in buffer */
                buf += size_arr[u];

                /* Increment sequence number */
                u++;
            } /* end else */
        } /* end for */
    } /* end if */
    else {
        /* Work through all the sequences */
        for(u=0; u<nseq; u++) {
            size=size_arr[u];
            addr=_addr+offset_arr[u];

            if (H5F_block_write(f, type, addr, size, dxpl_id, buf)<0) {
                HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                          "block write failed");
            }

            /* Increment offset in buffer */
            buf += size_arr[u];
        } /* end for */
    } /* end else */

    FUNC_LEAVE(SUCCEED);
}   /* end H5F_contig_writev() */

