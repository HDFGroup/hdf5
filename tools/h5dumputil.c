/*
 *
 * Purpose:	A library for displaying the values of a dataset or an attribute
 *              in a human readable format.
 *
 * Note:        h5dumputil is a modification of h5tools.c 
 */
#include <assert.h>
#include <ctype.h>
#include <h5dump.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "h5tools.h"

/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed.  This constant sets the limit on the
 * size of that temporary buffer in bytes.  For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */
#if 0
#define H5DUMP_BUFSIZE	(1024*1024)
#else
#define H5DUMP_BUFSIZE	(1024)
#endif

#define MIN(X,Y)	((X)<(Y)?(X):(Y))
#define NELMTS(X)	(sizeof(X)/sizeof(*X))
#define ALIGN(A,Z)	((((A)+(Z)-1)/(Z))*(Z))


extern void indentation(int);

int print_data(hid_t , hid_t , int);

/*-------------------------------------------------------------------------
 * Function:	h5dump_sprint
 *
 * Purpose:	Prints the value pointed to by VP into the string S assuming
 *		the data type of VP is TYPE.
 *
 * Return:	void
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#if 0
static void
h5dump_sprint(char *s/*out*/, hid_t type, void *vp)
{
    size_t	i, n;
    char	temp[1024];
    int		j;
    H5T_str_t   str_pad;

    if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
	sprintf(s, "%g", *((double*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
	sprintf(s, "%g", *((float*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
	sprintf(s, "%d", *((short*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
	sprintf(s, "%u", *((unsigned short*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_INT)) {
	sprintf(s, "%d", *((int*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
	sprintf(s, "%u", *((unsigned*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
	sprintf(s, "%ld", *((long*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
	sprintf(s, "%lu", *((unsigned long*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
        sprintf(s, "%d", *((signed char*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
        sprintf(s, "%u", *((unsigned char*)vp));
    } else if (H5T_STRING==H5Tget_class(type)) {
           str_pad = H5Tget_strpad(type) ;
           j = 0;
           for (i = 0; i < H5Tget_size(type); i++) {
	        switch (*((char*)vp+i)) {
	        case '"':
	              strcpy(s+j, "\\\"");
                      j += strlen("\\\"");
	              break;
	        case '\\':
	              strcpy(s+j, "\\\\");
                      j += strlen("\\\\");
	              break;
	        case '\b':
	             strcpy(s+j, "\\b");
                      j += strlen("\\b");
	             break;
	        case '\f':
	             strcpy(s+j, "\\f");
                      j += strlen("\\f");
	             break;
	        case '\n':
	             strcpy(s+j, "\\n");
                      j += strlen("\\n");
	             break;
	        case '\r':
	             strcpy(s+j, "\\r");
                      j += strlen("\\r");
	             break;
	        case '\t':
	             strcpy(s+j, "\\t");
                      j += strlen("\\t");
	             break;
	        default:
                    if (isprint(*((char*)vp+i))){
                        sprintf(s+j, "%c", *((char*)vp+i));
                        j += strlen(s+j);
                    } else { 
                        if (str_pad == H5T_STR_NULLTERM &&
                            *((unsigned char*)vp+i) == '\0' ) {
                            sprintf(s+j, "%c", *((unsigned char*)vp+i));
                            i = H5Tget_size(type);
                        } else {
                            sprintf(s+j, "\\%03o", *((unsigned char*)vp+i));
                            j += strlen(s+j);
                        }
                    }
                   break;
	        }
           }
    } else {
	strcpy(temp, "0x");
	n = H5Tget_size(type);
	for (i=0; i<n; i++) {
	    sprintf(temp+strlen(temp), "%02x", ((unsigned char*)vp)[i]);
	}
       sprintf(s,  "%s", temp);
    }

}


#endif


