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

#include <ctype.h>
#include <stddef.h>                                 
#include "H5f90.h"


/*
 * Routines from HDF4 to deal with C-FORTRAN issues.
 *
 * HD5c2fstr      -- convert a C string into a Fortran string IN PLACE
 * HD5f2cstring   -- convert a Fortran string to a C string
 */

/* ------------------------------- HD5c2fstr ------------------------------- 
NAME
   HD5c2fstr -- convert a C string into a Fortran string IN PLACE
USAGE
   int HD5c2fstr(str, len)
   char * str;       IN: string to convert
   int   len;       IN: length of Fortran string
RETURNS
   SUCCEED
DESCRIPTION
   Change a C string (NULL terminated) into a Fortran string.
   Basically, all that is done is that the NULL is ripped out
   and the string is padded with spaces

---------------------------------------------------------------------------*/
int 
HD5c2fstr(char *str, int len)
{
    int         i;

    i=(int)HDstrlen(str);
    for (; i < len; i++)
        str[i] = ' ';
    return 0;
}   /* HD5c2fstr */

/* ----------------------------- HD5f2cstring ------------------------------ */
/*
NAME
   HD5f2cstring -- convert a Fortran string to a C string
USAGE
   char * HDf2cstring(fdesc, len)
   _fcd  fdesc;     IN: Fortran string descriptor
   int  len;       IN: length of Fortran string
RETURNS
   Pointer to the C string if success, else NULL
DESCRIPTION
   Chop off trailing blanks off of a Fortran string and
   move it into a newly allocated C string.  It is up
   to the user to free this string.

---------------------------------------------------------------------------*/
char *
HD5f2cstring(_fcd fdesc, int len)
{
    char       *cstr, *str;
    int         i;

    str = _fcdtocp(fdesc);
    /* This should be equivalent to the above test -QAK */
    for(i=len-1; i>=0 && !isgraph((int)str[i]); i--)
        /*EMPTY*/;
    cstr = (char *) HDmalloc( (i + 2));
    if (!cstr) return NULL;
    cstr[i + 1] = '\0';
    HDmemcpy(cstr,str,i+1);
    return cstr;
}   /* HD5f2cstring */

/* ---------------------------- HD5packFstring ----------------------------- */
/*
NAME
   HD5packFstring -- convert a C string into a Fortran string
USAGE
   int HD5packFstring(src, dest, len)
   char * src;          IN:  source string
   char * dest;         OUT: destination
   int   len;          IN:  length of string
RETURNS
   SUCCEED / FAIL
DESCRIPTION
   given a NULL terminated C string 'src' convert it to
   a space padded Fortran string 'dest' of length 'len'

   This is very similar to HDc2fstr except that function does
   it in place and this one copies.  We should probably only
   support one of these.

---------------------------------------------------------------------------*/
int 
HD5packFstring(char *src, char *dest, int len)
{
    int        sofar;

    for (sofar = 0; (sofar < len) && (*src != '\0'); sofar++)
        *dest++ = *src++;

    while (sofar++ < len)
        *dest++ = ' ';

    return 0;
}	/* HD5packFstring */

