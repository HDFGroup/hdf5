/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5MS.c
 *			Sept 18 2020
 *			Quincey Koziol <koziol@lbl.gov>
 *
 * Purpose:		Implements a "managed" string that efficiently
 *                      handles dynamically allocations and appends.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5ESmodule.h"         /* This source code file is part of the H5ES module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MSprivate.h"        /* Managed strings                      */


/****************/
/* Local Macros */
/****************/

/* Initial buffer size to allocate */
#define H5MS_ALLOC_SIZE 256


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5MS_asprintf_cat
 *
 * Purpose:     This function appends formatted output to a "managed string",
 *              allocating the managed string if necessary.  The formatting
 *              string is printf() compatible.
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *	        Friday, September 18, 2020
 *
 *-------------------------------------------------------------------------
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the sprintf() calls with
 *      format_templ in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
H5_GCC_DIAG_OFF(format-nonliteral)
herr_t
H5MS_asprintf_cat(H5MS_t *ms, const char *fmt, ...)
{
    va_list args1, args2;
    size_t out_len;

    /* FUNC_ENTER() should not be called */

    /* Sanity checks */
    HDassert(ms);
    HDassert(fmt);

    /* Allocate the underlying string, if necessary */
    if(NULL == ms->s) {
        ms->max = H5MS_ALLOC_SIZE;
        ms->len = 0;
        ms->s = ms->end = HDmalloc(H5MS_ALLOC_SIZE);
        HDassert(ms->s);
        *ms->s = '\0';
    } /* end if */

    /* Attempt to write formatted output into the managed string */
    HDva_start(args1, fmt);
    HDva_copy(args2, args1);
    while((out_len = (size_t)HDvsnprintf(ms->end, (ms->max - ms->len), fmt, args1)) >= (ms->max - ms->len)) {
        /* Allocate a large enough buffer */
        while(out_len >= (ms->max - ms->len))
            ms->max *= 2;
        ms->s = HDrealloc(ms->s, ms->max);
        HDassert(ms->s);
        ms->end = ms->s + ms->len;

        /* Restart the va_list */
        HDva_end(args1);
        HDva_copy(args1, args2);
    } /* end while */

    /* Increment the size & end of the string */
    ms->len += out_len;
    ms->end += out_len;

    /* Finish access to varargs */
    HDva_end(args1);
    HDva_end(args2);

    return SUCCEED;
} /* end H5MS_asprintf_cat() */
H5_GCC_DIAG_ON(format-nonliteral)


/*-------------------------------------------------------------------------
 * Function:    H5MS_acat
 *
 * Purpose:     This function appends a character string to a "managed string",
 *              allocating the managed string if necessary.
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *	        Friday, September 18, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MS_acat(H5MS_t *ms, const char *s)
{
    /* FUNC_ENTER() should not be called */

    /* Sanity checks */
    HDassert(ms);
    HDassert(s);

    /* Allocate the underlying string, if necessary */
    if(NULL == ms->s) {
        ms->max = H5MS_ALLOC_SIZE;
        ms->len = 0;
        ms->s = ms->end = HDmalloc(H5MS_ALLOC_SIZE);
        HDassert(ms->s);
        *ms->s = '\0';
    } /* end if */

    /* Concatenate the provided string on to the managed string */
    if(*s) {
        do {
            /* Increase the managed string's buffer size if necessary */
            if((ms->len + 1) >= ms->max) {
                ms->max *= 2;
                ms->s = HDrealloc(ms->s, ms->max);
                HDassert(ms->s);
                ms->end = ms->s + ms->len;
            } /* end if */

            /* Append the current character */
            *ms->end++ = *s++;
            ms->len++;
        } while(*s);
        *ms->end = '\0';
    } /* end if */

    return SUCCEED;
} /* end H5MS_acat() */


/*-------------------------------------------------------------------------
 * Function:    H5MS_aputc
 *
 * Purpose:     This function appends a character to a "managed string",
 *              allocating the managed string if necessary.
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *	        Friday, September 18, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MS_aputc(H5MS_t *ms, int c)
{
    /* FUNC_ENTER() should not be called */

    /* Sanity checks */
    HDassert(ms);
    HDassert(c);

    /* Allocate the underlying string, if necessary */
    if(NULL == ms->s) {
        ms->max = H5MS_ALLOC_SIZE;
        ms->len = 0;
        ms->s = ms->end = HDmalloc(H5MS_ALLOC_SIZE);
        HDassert(ms->s);
        *ms->s = '\0';
    } /* end if */

    /* Increase the managed string's buffer size if necessary */
    if((ms->len + 1) >= ms->max) {
        ms->max *= 2;
        ms->s = HDrealloc(ms->s, ms->max);
        HDassert(ms->s);
        ms->end = ms->s + ms->len;
    } /* end if */

    /* Append the current character */
    *ms->end++ = (char)c;
    ms->len++;
    *ms->end = '\0';

    return SUCCEED;
} /* end H5MS_aputc() */


