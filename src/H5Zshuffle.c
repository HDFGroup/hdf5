/*
 * Copyright © 1999-2001 NCSA
 *                       All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, August 27, 1999
 */
#include "H5private.h"
#include "H5Eprivate.h"
#include "H5MMprivate.h"
#include "H5Zprivate.h"

#ifdef H5_HAVE_FILTER_SHUFFLE

/* Interface initialization */
#define PABLO_MASK	H5Z_shuffle_mask
#define INTERFACE_INIT	NULL
static int interface_initialize_g = 0;


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_shuffle
 *
 * Purpose:	Implement an I/O filter which "de-interlaces" a block of data
 *              by putting all the bytes in a byte-position for each element
 *              together in the block.  For example, for 4-byte elements stored
 *              as: 012301230123, shuffling will store them as: 000111222333
 *              Usually, the bytes in each byte position are more related to
 *              each other and putting them together will increase compression.
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Kent Yang
 *              Wednesday, November 13, 2002
 *
 * Modifications:
 *              Quincey Koziol, November 13, 2002
 *              Cleaned up code.
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Z_filter_shuffle(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
                   size_t nbytes, size_t *buf_size, void **buf)
{
    void *dest = NULL;          /* Buffer to deposit [un]shuffled bytes into */
    unsigned char *_src;        /* Alias for source buffer */
    unsigned char *_dest;       /* Alias for destination buffer */
    unsigned bytesoftype;       /* Number of bytes per element */
    size_t numofelements;       /* Number of elements in buffer */
    size_t i,j;                 /* Local index variables */
    size_t leftover;            /* Extra bytes at end of buffer */
    size_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI(H5Z_filter_shuffle, 0);

    /* Check arguments */
    if (cd_nelmts!=1 || cd_values[0]==0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid shuffle parameters");

    /* Get the number of bytes per element from the parameter block */
    bytesoftype=cd_values[0];

    /* Don't do anything for 1-byte elements */
    if(bytesoftype>1) {
        /* Compute the number of elements in buffer */
        numofelements=nbytes/bytesoftype;

        /* Compute the leftover bytes if there are any */
        leftover = nbytes%bytesoftype;

        /* Allocate the destination buffer */
        if (NULL==(dest = H5MM_malloc(nbytes)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for shuffle buffer");

        if(flags & H5Z_FLAG_REVERSE) {
            /* Get the pointer to the source buffer */
            _src =(unsigned char *)(*buf);

            /* Input; unshuffle */
            for(i=0; i<bytesoftype; i++) {
                _dest=((unsigned char *)dest)+i;
                for(j=0; j<numofelements; j++) {
                    *_dest=*_src++;
                    _dest+=bytesoftype;
                } /* end for */
            } /* end for */

            /* Add leftover to the end of data */ 
            if(leftover>0) {
                /* Adjust back to end of shuffled bytes */
                _dest -= (bytesoftype - 1);
                HDmemcpy((void*)_dest, (void*)_src, leftover);
            }
        } /* end if */
        else {
            /* Get the pointer to the destination buffer */
            _dest =(unsigned char *)dest;

            /* Output; shuffle */
            for(i=0; i<bytesoftype; i++) {
                _src=((unsigned char *)(*buf))+i;
                for(j=0; j<numofelements; j++) {
                    *_dest++=*_src;
                    _src+=bytesoftype;
                } /* end for */
            } /* end for */

            /* Add leftover to the end of data */ 
            if(leftover>0) {
                /* Adjust back to end of shuffled bytes */
                _src -= (bytesoftype - 1);
                HDmemcpy((void*)_dest, (void*)_src, leftover);
            }
        } /* end else */

        /* Set the buffer information to return */
        H5MM_xfree(*buf);
        *buf = dest;
        *buf_size=nbytes;
    } /* end else */

    /* Set the return value */
    ret_value = nbytes;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
#endif /*H5_HAVE_FILTER_SHUFFLE */
