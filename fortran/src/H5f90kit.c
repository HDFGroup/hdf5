#include <ctype.h>
#include <stddef.h>                                 
#include "H5f90.h"


/*
 * Routines from HDF4 to deal with C-FORTRAN issues.
 *
 * HD5c2fstr      -- convert a C string into a Fortran string IN PLACE
 * HD5f2cstring   -- convert a Fortran string to a C string
 */

/* ------------------------------- HDc2fstr ------------------------------- 
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

/* ----------------------------- HDf2cstring ------------------------------ */
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

/* ---------------------------- HDpackFstring ----------------------------- */
/*
NAME
   HDpackFstring -- convert a C string into a Fortran string
USAGE
   intn HDpackFstring(src, dest, len)
   char * src;          IN:  source string
   char * dest;         OUT: destination
   intn   len;          IN:  length of string
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
HDpackFstring(char *src, char *dest, int len)
{
    int        sofar;

    for (sofar = 0; (sofar < len) && (*src != '\0'); sofar++)
        *dest++ = *src++;

    while (sofar++ < len)
        *dest++ = ' ';

    return 0;
}	/* HDpackFstring */

