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

extern int indent;
extern void indentation(int);
int compound_data=0;
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


/*-------------------------------------------------------------------------
 * Function:	display_numeric_data
 *
 * Purpose:	Display numeric data in ddl format.
 *
 * Return:	void
 *
 * Comment:     hs_nelmts     number of elements to be printed
 *              p_type        memory data type
 *              sm_buf        data buffer
 *              p_type_nbytes size of p_type
 *              p_nelmts      total number of elements
 *              dim_n_size    size of dimemsion n
 *              elmtno        element index
 *
 *-------------------------------------------------------------------------
 */
static void display_numeric_data 
(hsize_t hs_nelmts, hid_t p_type, unsigned char *sm_buf, size_t p_type_nbytes, 
 hsize_t p_nelmts, hsize_t dim_n_size, hsize_t elmtno) {

hsize_t i;
char p_buf[256];		
char out_buf[ncols];

    out_buf[0] = '\0';
    if ((indent+col) > ncols) indent = 0;

    for (i=0; i<hs_nelmts && (elmtno+i) < p_nelmts; i++) {
         h5dump_sprint(p_buf, p_type, sm_buf+i*p_type_nbytes);

         if ((int)(strlen(out_buf)+strlen(p_buf)+1) > (ncols-indent-col)) {
             /* first row of member */
             if (compound_data && (elmtno+i+1) == dim_n_size)
                 printf("%s\n", out_buf);
             else {
                 indentation(indent+col);
                 printf("%s\n", out_buf);
             }
             strcpy(out_buf, p_buf);
             if ((elmtno+i+1) % dim_n_size) 
                 strcat(out_buf, ", ");
             else { /* end of a row, flush out_buf */
                 indentation(indent+col);
                 printf("%s", out_buf);
                 if ((elmtno+i+1) != p_nelmts) /* not last element */
                     printf(",\n");
                 else if (compound_data) { /* last element of member data*/
                     if ((ncols-strlen(out_buf)-indent-col) < 2) { 
                          /* 2 for space and ] */
                         printf("\n");
                         indentation(indent+col-3);
                     }
                 } else
                     printf("\n"); /* last row */
                 *out_buf = '\0';
             }
        } else {
             strcat(out_buf, p_buf);
             if ((elmtno+i+1) % dim_n_size) {
                  if ((ncols-strlen(out_buf)-indent-col-1) > 0)
                      strcat(out_buf, ", ");
                  else 
                      strcat(out_buf, ",");
             } else { /* end of a row */
                 /* 1st row of member data */
                 if (compound_data && (elmtno+i+1) == dim_n_size) 
                     printf("%s", out_buf);
                 else {
                     indentation(indent+col);
                     printf("%s", out_buf);
                 }

                 /* if it's the last element */
                 if ((elmtno+i+1) != p_nelmts)
                     printf(",\n");
                 else if (compound_data) { /* last row of member data*/
                     /* 2 for space and ] */
                     if ((ncols-strlen(out_buf)-indent-col) < 2) { 
                         printf("\n");
                         indentation(indent+col-3);
                     }
                 } else
                     printf("\n"); /* last row */
                 *out_buf = '\0';
             }
        }
    }
}


/*-------------------------------------------------------------------------
 * Function:	display_string
 *
 * Purpose:	Display string in ddl format
 *
 * Return:	void
 *
 * Comment:     concatenator operator : '//'
 *              separator between elements: ','
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void display_string
(hsize_t hs_nelmts, hid_t p_type, unsigned char *sm_buf, size_t p_type_nbytes, 
 hsize_t p_nelmts, hsize_t dim_n_size, hsize_t elmtno) {
hsize_t i, row_size=0;
int j, m, x, y, z,  first_row=1;
int free_space, long_string = 0;
char p_buf[256], out_buf[ncols];

    out_buf[0] = '\0';
    if ((indent+col) > ncols) indent = 0;

    for (i=0; i<hs_nelmts && (elmtno+i) < p_nelmts; i++) {
         row_size++;
         h5dump_sprint(p_buf, p_type, sm_buf+i*p_type_nbytes);

         free_space = ncols - indent - col - strlen(out_buf);

         if ((elmtno+i+1) == p_nelmts) { /* last element */
            /* 2 for double quotes */
            if (((int)strlen(p_buf) + 2) > free_space) long_string = 1;
         } else 
            /* 3 for double quotes and one comma */
            if (((int)strlen(p_buf) + 3) > free_space) long_string = 1;

         if (long_string) {

             if (free_space < 5) {  /* 5 for double quotes, one space and two '/'s */
                 /* flush out_buf */
                 if (compound_data && first_row) {
                     printf("%s\n", out_buf);
                     first_row = 0;
                 } else {
                     indentation(indent+col); 
                     printf("%s\n", out_buf);
                 }
                 out_buf[0] = '\0';
                 x = 0 ;
             } else {
                 x = free_space - 5;
                 if (compound_data && first_row) {
                     printf("%s\"", out_buf);
                     strncpy(out_buf, p_buf, x);
                     out_buf[x] = '\0';
                     printf("%s\" //\n", out_buf);
                     first_row = 0;
                 } else {
                     indentation(indent+col); 
                     printf("%s\"", out_buf);
                     strncpy(out_buf, p_buf, x);
                     out_buf[x] = '\0';
                     printf("%s\" //\n", out_buf);
                 }
                 out_buf[0] = '\0';
             }

             y = ncols - indent -col - 5;

             m = (strlen(p_buf) - x)/y;

             z = (strlen(p_buf) - x) % y;


             for (j = 0; j < m - 1 ; j++) {
                  indentation(indent+col);
                  strncpy(out_buf, p_buf+x+j*y, y);
                  out_buf[y] = '\0';
                  printf("\"%s\" //\n", out_buf);
             }

             if ((elmtno+i+1) == p_nelmts) { /* last element */
                  if ((int)strlen(p_buf+x+j*y) > (ncols - indent - col -2)) { /* 2 for double quotes */
                     indentation(indent+col);
                     strncpy(out_buf, p_buf+x+j*y, y);
                     out_buf[y] = '\0';
                     printf("\"%s\" //\n", out_buf);
                     indentation(indent+col);
                     printf("\"%s\"", p_buf+x+m*y);
                     if (compound_data) {
                         if ((ncols-strlen(out_buf)-indent-col) < 2) {
                              printf("\n");
                              indentation(indent+col-3);
                         }
                     } else 
                         printf("\n");

                  } else {
                     indentation(indent+col);
                     printf("\"%s\"", p_buf+x+j*y);
                     if (compound_data) {
                         if ((ncols-strlen(out_buf)-indent-col) < 2) {
                              printf("\n");
                              indentation(indent+col-3);
                         }
  
                     } else
                         printf("\n");
                  }
                  out_buf[0] = '\0';
             } else if ( row_size == dim_n_size) {
                  if ((int)strlen(p_buf+x+j*y) > (ncols - indent - col -3)) { /* 3 for 2 "'s and 1 , */
                     indentation(indent+col);
                     strncpy(out_buf, p_buf+x+j*y, y);
                     out_buf[y] = '\0';
                     printf("\"%s\" //\n", out_buf);
                     indentation(indent+col);
                     printf("\"%s\",\n", p_buf+x+m*y);
                  } else {
                     indentation(indent+col);
                     printf("\"%s\",\n", p_buf+x+j*y);
                  }
                  out_buf[0] = '\0';
                  row_size = 0;

             } else {
                  if ((int)strlen(p_buf+x+j*y) > (ncols - indent - col -3)) { /* 3 for 2 "'s and 1 , */
                     indentation(indent+col);
                     strncpy(out_buf, p_buf+x+j*y, y);
                     out_buf[y] = '\0';
                     printf("\"%s\" //\n", out_buf);
                     strcpy(out_buf, "\"");
                     strcat(out_buf, p_buf+x+m*y);
                     strcat(out_buf, "\",");
                     if ((int)strlen(out_buf) < (ncols-indent-col)) strcat(out_buf, " "); 
                  } else {
                     strcpy(out_buf, "\"");
                     strcat (out_buf, p_buf+x+j*y);
                     strcat(out_buf, "\",");
                     if ((int)strlen(out_buf) < (ncols-indent-col)) strcat(out_buf, " "); 
                  }
             }
             long_string = 0;

         } else {

            /* flush out_buf if it's end of a row */
            if (row_size == dim_n_size) {
                if (compound_data && (elmtno+i+1) == dim_n_size) { /* 1st row */
                    printf("%s\"%s\"", out_buf, p_buf);
                    first_row = 0;
                } else {
                    indentation(indent+col); 
                    printf("%s\"%s\"", out_buf, p_buf);
                }

               if ((elmtno+i+1) != p_nelmts) 
                   printf(",\n");
               else if (compound_data) {
                       if ((ncols-strlen(out_buf)-strlen(p_buf)-indent-col) < 2) {
                           /* 2 for space and ] */
                           printf("\n");
                           indentation(indent+col-3);
                       }
               } else
                   printf("\n");
   
               out_buf[0] = '\0';
               row_size = 0;
            } else {
                 strcat(out_buf, "\"");
                 strcat(out_buf, p_buf);
                 strcat(out_buf, "\",");
                 if ((int)strlen(out_buf) < (ncols-indent-col)) strcat(out_buf, " ");
            }

         }
    }
}


/*-------------------------------------------------------------------------
 * Function:	display_compound_data
 *
 * Purpose:	Display compound data in ddl format
 *
 * Return:	void
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void display_compound_data
(hsize_t hs_nelmts, hid_t p_type, unsigned char *sm_buf, size_t p_type_nbytes, 
 hsize_t p_nelmts, hsize_t elmtno) {
size_t  offset, size, dims[4]; 
hsize_t nelmts, dim_n_size=0;
hid_t   memb;
int     nmembs, i, j, k, ndims, perm[4];

    if ((indent+col) > ncols) indent = 0;

    for (i=0; i<(int)hs_nelmts && (elmtno+i) < p_nelmts; i++) {

       nmembs = H5Tget_nmembers(p_type);

       indentation(indent+col); 
       printf("{\n");

       indent+= col;
       for (j=0; j<nmembs; j++) {

	    offset = H5Tget_member_offset(p_type, j);
	    memb = H5Tget_member_type(p_type, j);
	    size = H5Tget_size(memb);
	    ndims = H5Tget_member_dims(p_type, j, dims, perm);
            if (ndims > 0) dim_n_size = dims[ndims-1];
            else dim_n_size = 1;
	    for (k=0, nelmts=1; k<ndims; k++) nelmts *= dims[k];

            indentation(indent+col);
	    printf("[ ");

            indent+=2;
            switch (H5Tget_class(memb)) {
            case H5T_INTEGER:
                 display_numeric_data
                 (nelmts, memb, sm_buf+offset+i*p_type_nbytes, size, nelmts, dim_n_size, 0) ;
                 break;

            case H5T_FLOAT:
                 display_numeric_data
                 (nelmts, memb, sm_buf+offset+i*p_type_nbytes, size, nelmts, dim_n_size, 0) ;
                 break;

            case H5T_TIME:
                 break;

            case H5T_STRING:
                 display_string
                 (nelmts, memb, sm_buf+offset+i*p_type_nbytes, size, nelmts, dim_n_size, 0 ) ;
                 break;

            case H5T_BITFIELD:
                 break;

            case H5T_OPAQUE:
                 break;

            default: break;

            }
            indent-=2;

            if ( j == nmembs-1) printf(" ]\n");
            else printf(" ],\n");

	    H5Tclose(memb);
       }
       indent-= col;

       indentation(indent+col);
       if ((elmtno+i+1) == p_nelmts) printf("}\n");
       else printf("},\n");
    }

}


/*-------------------------------------------------------------------------
 * Function:	h5dump_simple
 *
 * Purpose:	Print some values from a dataset or an attribute with a 
 *              simple data space.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Modifications: 
 *
 *-------------------------------------------------------------------------
 */
static int
h5dump_simple(hid_t oid, hid_t p_type, int obj_data)
{
    hid_t		f_space;		/*file data space	*/
    int			ndims;			/*dimensionality	*/
    hsize_t		elmtno, i;		/*counters		*/
    int			carry;			/*counter carry value	*/
    hssize_t		zero[8];		/*vector of zeros	*/

    /* Print info */
    hsize_t		p_min_idx[8];		/*min selected index	*/
    hsize_t		p_max_idx[8];		/*max selected index	*/
    size_t		p_type_nbytes;		/*size of memory type	*/
    hsize_t		p_nelmts;		/*total selected elmts	*/

    /* Stripmine info */
    hsize_t		sm_size[8];		/*stripmine size	*/
    hsize_t		sm_nbytes;		/*bytes per stripmine	*/
    hsize_t		sm_nelmts;		/*elements per stripmine*/
    unsigned char	*sm_buf;		/*buffer for raw data	*/
    hid_t		sm_space;		/*stripmine data space	*/

    /* Hyperslab info */
    hssize_t		hs_offset[8];		/*starting offset	*/
    hsize_t		hs_size[8];		/*size this pass	*/
    hsize_t		hs_nelmts;		/*elements in request	*/
    hsize_t             dim_n_size;


    if (obj_data == DATASET_DATA) 
        f_space = H5Dget_space(oid);
    else 
        f_space = H5Aget_space(oid);

    /*
     * Check that everything looks okay.  The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */

    ndims = H5Sget_simple_extent_ndims(f_space);

    if ((size_t)ndims>NELMTS(sm_size)) return -1;

    /* Assume entire data space to be printed */
    for (i=0; i<(hsize_t)ndims; i++) p_min_idx[i] = 0;
    H5Sget_simple_extent_dims(f_space, p_max_idx, NULL);
    for (i=0, p_nelmts=1; i<(hsize_t)ndims; i++) {
	p_nelmts *= p_max_idx[i]-p_min_idx[i];
    }
    if (0==p_nelmts) return 0; /*nothing to print*/

    /*
     * Determine the strip mine size and allocate a buffer.  The strip mine is
     * a hyperslab whose size is manageable.
     */
    p_type_nbytes = H5Tget_size(p_type);
    for (i=ndims, sm_nbytes=p_type_nbytes; i>0; --i) {
	sm_size[i-1] = MIN (p_max_idx[i-1]-p_min_idx[i-1],
			    H5DUMP_BUFSIZE/sm_nbytes);
	sm_nbytes *= sm_size[i-1];
	assert(sm_nbytes>0);
    }
    sm_buf = malloc(sm_nbytes);
    sm_nelmts = sm_nbytes/p_type_nbytes;
    sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

    /* The stripmine loop */
    memset(hs_offset, 0, sizeof hs_offset);
    memset(zero, 0, sizeof zero);


    for (elmtno=0; elmtno<p_nelmts; elmtno+=hs_nelmts) {


        /* Calculate the hyperslab size */
        if (ndims > 0) {
            for (i=0, hs_nelmts=1; i<(hsize_t)ndims; i++) {
                hs_size[i] = MIN(sm_size[i], p_max_idx[i]-hs_offset[i]);
                hs_nelmts *= hs_size[i];
            }
            H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL,
                                hs_size, NULL);
            H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL,
                                &hs_nelmts, NULL);
            dim_n_size = p_max_idx[ndims-1];
        } else {
            H5Sselect_all(f_space);
            H5Sselect_all(sm_space);
            hs_nelmts = 1;
            dim_n_size = 1;
        }

        if (obj_data == DATASET_DATA) {
            if (H5Dread(oid, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) <0)
                return -1;
        } else {
            if (H5Aread(oid, p_type, sm_buf) < 0) 
                return -1;
        }

	/* Print the data */
        switch (H5Tget_class(p_type)) {
        case H5T_INTEGER:
             display_numeric_data (hs_nelmts, p_type, sm_buf, p_type_nbytes, 
                                   p_nelmts, dim_n_size, elmtno);
             break;

        case H5T_FLOAT:
             display_numeric_data (hs_nelmts, p_type, sm_buf, p_type_nbytes, 
                                   p_nelmts, dim_n_size, elmtno);
             break;

        case H5T_TIME:
             break;

        case H5T_STRING:
             display_string (hs_nelmts, p_type, sm_buf, p_type_nbytes, 
                             p_nelmts, dim_n_size, elmtno);
             break;

        case H5T_BITFIELD:
             break;

        case H5T_OPAQUE:
             break;

        case H5T_COMPOUND:
             compound_data = 1;
             display_compound_data (hs_nelmts, p_type, sm_buf, p_type_nbytes, p_nelmts, elmtno);
             compound_data = 0;
             break;

        default: break;
        }
	
	/* Calculate the next hyperslab offset */
	for (i=ndims, carry=1; i>0 && carry; --i) {
	    hs_offset[i-1] += hs_size[i-1];
	    if (hs_offset[i-1]==(hssize_t)p_max_idx[i-1]) {
		hs_offset[i-1] = p_min_idx[i-1];
	    } else {
		carry = 0;
	    }
	}
    }

    H5Sclose(sm_space);
    H5Sclose(f_space);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_fixtype
 *
 * Purpose:	Given a file data type choose a memory data type which is
 *		appropriate for printing the data.
 *
 * Return:	Success:	Memory data type
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
h5dump_fixtype(hid_t f_type)
{
    hid_t	m_type=-1, f_memb;
    hid_t	*memb=NULL;
    char	**name=NULL;
    int		nmembs=0, i, j, *ndims=NULL, perm[4];
    size_t	size, offset, *dims=NULL, nelmts;
    H5T_str_t strpad;


    size = H5Tget_size(f_type);
    switch (H5Tget_class(f_type)) {

    case H5T_INTEGER:
	/*
	 * Use the smallest native integer type of the same sign as the file
	 * such that the memory type is at least as large as the file type.
	 * If there is no memory type large enough then use the largest
	 * memory type available.
	 */
	if (size<=sizeof(char)) {
	    m_type = H5Tcopy(H5T_NATIVE_SCHAR);
	} else if (size<=sizeof(short)) {
	    m_type = H5Tcopy(H5T_NATIVE_SHORT);
	} else if (size<=sizeof(int)) {
	    m_type = H5Tcopy(H5T_NATIVE_INT);
	} else if (size<=sizeof(long)) {
	    m_type = H5Tcopy(H5T_NATIVE_LONG);
	} else {
	    m_type = H5Tcopy(H5T_NATIVE_LLONG);
	}
	H5Tset_sign(m_type, H5Tget_sign(f_type));
	break;
	
    case H5T_FLOAT:
	/*
	 * Use the smallest native floating point type available such that
	 * its size is at least as large as the file type.  If there is not
	 * native type large enough then use the largest native type.
	 */
	if (size<=sizeof(float)) {
	    m_type = H5Tcopy(H5T_NATIVE_FLOAT);
	} else if (size<=sizeof(double)) {
	    m_type = H5Tcopy(H5T_NATIVE_DOUBLE);
	} else {
	    m_type = H5Tcopy(H5T_NATIVE_LDOUBLE);
	}
	break;

    case H5T_COMPOUND:
	/*
	 * We have to do this in two steps.  The first step scans the file
	 * type and converts the members to native types and remembers all
	 * their names and sizes, computing the size of the memory compound
	 * type at the same time.  Then we create the memory compound type
	 * and add the members.
	 */
	nmembs = H5Tget_nmembers(f_type);
	memb = calloc(nmembs, sizeof(hid_t));
	name = calloc(nmembs, sizeof(char*));
	ndims = calloc(nmembs, sizeof(int));
	dims = calloc(nmembs*4, sizeof(size_t));
	
	for (i=0, size=0; i<nmembs; i++) {

	    /* Get the member type and fix it */
	    f_memb = H5Tget_member_type(f_type, i);
	    memb[i] = h5dump_fixtype(f_memb);
	    H5Tclose(f_memb);
	    if (memb[i]<0) goto done;

	    /* Get the member dimensions */
	    ndims[i] = H5Tget_member_dims(f_type, i, dims+i*4, perm);
	    assert(ndims[i]>=0 && ndims[i]<=4);
	    for (j=0, nelmts=1; j<ndims[i]; j++) nelmts *= dims[i*4+j];

	    /* Get the member name */
	    name[i] = H5Tget_member_name(f_type, i);
	    if (NULL==name[i]) goto done;

	    /*
	     * Compute the new offset so each member is aligned on a byte
	     * boundary which is the same as the member size.
	     */
	    size = ALIGN(size, H5Tget_size(memb[i])) +
		     nelmts * H5Tget_size(memb[i]);
	}

	m_type = H5Tcreate(H5T_COMPOUND, size);
	for (i=0, offset=0; i<nmembs; i++) {
	    H5Tinsert_array(m_type, name[i], offset, ndims[i], dims+i*4,
			    perm, memb[i]);
	    for (j=0, nelmts=1; j<ndims[i]; j++) nelmts *= dims[i*4+j];
	    offset = ALIGN(offset, H5Tget_size(memb[i])) +
		     nelmts * H5Tget_size(memb[i]);
	}
	break;

    case H5T_TIME:
    case H5T_STRING:

        m_type = H5Tcopy(H5T_C_S1);
        H5Tset_size(m_type, size);
        strpad = H5Tget_strpad(f_type) ;
        H5Tset_strpad(m_type, strpad);

        if (H5Tequal(m_type,f_type) < 0) {
            H5Tclose(m_type);
            m_type = H5Tcopy(H5T_FORTRAN_S1);
            H5Tset_size(m_type, size);
            H5Tset_strpad(m_type, strpad);
            if (H5Tequal(m_type,f_type) < 0) 
                m_type = -1;
        } 

        break;
         
    case H5T_BITFIELD:
    case H5T_OPAQUE:
	/*
	 * These type classes are not implemented yet.
	 */
	break;

    default:
	/* What the heck? */
	break;
    }

 done:
    /* Clean up temp buffers */
    if (memb && name && ndims && dims) {
	for (i=0; i<nmembs; i++) {
	    if (memb[i]>=0) H5Tclose(memb[i]);
	    if (name[i]) free(name[i]);
	}
	free(memb);
	free(name);
	free(ndims);
	free(dims);
    }
    
    return m_type;
}

/*-------------------------------------------------------------------------
 * Function:	print_data	
 *
 * Purpose:	Print some values from a dataset or an attribute to the 
 *              file STREAM after converting all types to P_TYPE (which
 *              should be a native type).  If P_TYPE is a negative value 
 *              then it will be computed from the dataset/attribute type 
 *              using only native types.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
print_data(hid_t oid, hid_t _p_type, int obj_data)
{
    hid_t	f_space;
    hid_t	p_type = _p_type;
    hid_t	f_type;
    int		status = -1;


    if (p_type < 0) {

        if (obj_data == DATASET_DATA) 
            f_type = H5Dget_type(oid);
        else
            f_type = H5Aget_type(oid);

        if (f_type < 0) return status;

	p_type = h5dump_fixtype(f_type);

	H5Tclose(f_type);

	if (p_type < 0) return status;
    }

    /* Check the data space */
    if (obj_data == DATASET_DATA) 
        f_space = H5Dget_space(oid);
    else
        f_space = H5Aget_space(oid);

    if (f_space < 0) return status;
 
    if (H5Sis_simple(f_space) >= 0) 
        status = h5dump_simple(oid, p_type, obj_data);

    H5Sclose(f_space);

    if (p_type != _p_type) H5Tclose(p_type);

    return status;
}
