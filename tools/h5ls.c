/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, March 23, 1998
 */


/*
 * We include the private header file so we can get to the uniform
 * programming environment it declares.  Other than that, h5ls only calls
 * HDF5 API functions.
 */
#include <H5private.h>
#include <h5tools.h>

/* Command-line switches */
static int verbose_g = 0;		/*lots of extra output		     */
static int width_g = 80;		/*output width in characters	     */
static hbool_t dump_g = FALSE;		/*display dataset values?	     */
static hbool_t label_g = FALSE;		/*label compound values?	     */
static hbool_t string_g = FALSE;	/*print 1-byte numbers as ASCII?     */

/* Information about how to display each type of object */
static struct dispatch_t {
    const char	*name;
    hid_t	(*open)(hid_t loc, const char *name);
    herr_t	(*close)(hid_t obj);
    herr_t	(*list1)(hid_t obj);
    herr_t	(*list2)(hid_t obj);
} dispatch_g[H5G_NTYPES];

#define DISPATCH(TYPE,NAME,OPEN,CLOSE,LIST1,LIST2) {			      \
    dispatch_g[TYPE].name = (NAME);					      \
    dispatch_g[TYPE].open = (OPEN);					      \
    dispatch_g[TYPE].close = (CLOSE);					      \
    dispatch_g[TYPE].list1 = (LIST1);					      \
    dispatch_g[TYPE].list2 = (LIST2);					      \
}

static herr_t list (hid_t group, const char *name, void *cd);
static void display_type(hid_t type, int indent);


/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Prints a usage message on stderr and then returns.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (const char *progname)
{
    fprintf(stderr, "\
usage: %s [OPTIONS] FILE [OBJECTS...]\n\
   OPTIONS\n\
      -h, -?, --help   Print a usage message and exit\n\
      -d, --dump       Print the values of datasets\n\
      -l, --label      Label members of compound datasets\n\
      -s, --string     Print 1-byte integer datasets as ASCII\n\
      -wN, --width=N   Set the number of columns of output\n\
      -v, --verbose    Generate more verbose output\n\
      -V, --version    Print version number and exit\n\
   FILE\n\
      The file name may include a printf(3C) integer format such as\n\
      \"%%05d\" to open a file family.\n\
   OBJECTS\n\
      The names of zero or more objects about which information should be\n\
      displayed.  If a group is mentioned then information about each of its\n\
      members is displayed.  If no object names are specified then\n\
      information about all of the objects in the root group is displayed.\n",
	    progname);
}


/*-------------------------------------------------------------------------
 * Function:	display_string
 *
 * Purpose:	Print a string value by escaping unusual characters.
 *
 * Return:	Number of characters printed.
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
display_string(const char *s)
{
    int		nprint=0;
    
    for (/*void*/; s && *s; s++) {
	switch (*s) {
	case '"':
	    printf("\\\"");
	    nprint += 2;
	    break;
	case '\\':
	    printf("\\\\");
	    nprint += 2;
	    break;
	case '\b':
	    printf("\\b");
	    nprint += 2;
	    break;
	case '\f':
	    printf("\\f");
	    nprint += 2;
	    break;
	case '\n':
	    printf("\\n");
	    nprint += 2;
	    break;
	case '\r':
	    printf("\\r");
	    nprint += 2;
	    break;
	case '\t':
	    printf("\\t");
	    nprint += 2;
	    break;
	default:
	    if (isprint(*s)) {
		putchar(*s);
		nprint++;
	    } else {
		printf("\\%03o", *((const unsigned char*)s));
		nprint += 4;
	    }
	    break;
	}
    }
    return nprint;
}


/*-------------------------------------------------------------------------
 * Function:	display_native_type
 *
 * Purpose:	Prints the name of a native C data type.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE, nothing printed.
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_native_type(hid_t type, int __unused__ indent)
{
    if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
	printf("native signed char");
    } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
	printf("native unsigned char");
    } else if (H5Tequal(type, H5T_NATIVE_INT)) {
	printf("native int");
    } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
	printf("native unsigned int");
    } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
	printf("native short");
    } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
	printf("native unsigned short");
    } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
	printf("native long");
    } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
	printf("native unsigned long");
    } else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
	printf("native long long");
    } else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
	printf("native unsigned long long");
    } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
	printf("native float");
    } else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
	printf("native double");
    } else if (H5Tequal(type, H5T_NATIVE_LDOUBLE)) {
	printf("native long double");
    } else if (H5Tequal(type, H5T_NATIVE_B8)) {
	printf("native 8-bit field");
    } else if (H5Tequal(type, H5T_NATIVE_B16)) {
	printf("native 16-bit field");
    } else if (H5Tequal(type, H5T_NATIVE_B32)) {
	printf("native 32-bit field");
    } else if (H5Tequal(type, H5T_NATIVE_B64)) {
	printf("native 64-bit field");
    } else if (H5Tequal(type, H5T_NATIVE_HSIZE)) {
	printf("native hsize_t");
    } else if (H5Tequal(type, H5T_NATIVE_HSSIZE)) {
	printf("native hssize_t");
    } else if (H5Tequal(type, H5T_NATIVE_HERR)) {
	printf("native herr_t");
    } else if (H5Tequal(type, H5T_NATIVE_HBOOL)) {
	printf("native hbool_t");
    } else {
	return FALSE;
    }
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:	display_ieee_type
 *
 * Purpose:	Print the name of an IEEE floating-point data type.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE, nothing printed
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_ieee_type(hid_t type, int __unused__ indent)
{
    if (H5Tequal(type, H5T_IEEE_F32BE)) {
	printf("IEEE 32-bit big-endian float");
    } else if (H5Tequal(type, H5T_IEEE_F32LE)) {
	printf("IEEE 32-bit little-endian float");
    } else if (H5Tequal(type, H5T_IEEE_F64BE)) {
	printf("IEEE 64-bit big-endian float");
    } else if (H5Tequal(type, H5T_IEEE_F64LE)) {
	printf("IEEE 64-bit little-endian float");
    } else {
	return FALSE;
    }
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:	display_precision
 *
 * Purpose:	Prints information on the next line about precision and
 *		padding if the precision is less than the total data type
 *		size.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
display_precision(hid_t type, int indent)
{
    size_t		prec;		/*precision			*/
    H5T_pad_t		plsb, pmsb;	/*lsb and msb padding		*/
    const char		*plsb_s=NULL;	/*lsb padding string		*/
    const char		*pmsb_s=NULL;	/*msb padding string		*/
    size_t		nbits;		/*number of bits		*/

    /*
     * If the precision is less than the total size then show the precision
     * and offset on the following line.  Also display the padding
     * information.
     */
    if (8*H5Tget_size(type)!=(prec=H5Tget_precision(type))) {
	printf("\n%*s(%lu bit%s of precision beginning at bit %lu)",
	       indent, "", (unsigned long)prec, 1==prec?"":"s",
	       (unsigned long)H5Tget_offset(type));

	H5Tget_pad(type, &plsb, &pmsb);
	if (H5Tget_offset(type)>0) {
	    switch (plsb) {
	    case H5T_PAD_ZERO:
		plsb_s = "zero";
		break;
	    case H5T_PAD_ONE:
		plsb_s = "one";
		break;
	    case H5T_PAD_BACKGROUND:
		plsb_s = "bkg";
		break;
	    case H5T_PAD_ERROR:
	    case H5T_NPAD:
		plsb_s = "unknown";
		break;
	    }
	}
	if (H5Tget_offset(type)+prec<8*H5Tget_size(type)) {
	    switch (pmsb) {
	    case H5T_PAD_ZERO:
		pmsb_s = "zero";
		break;
	    case H5T_PAD_ONE:
		pmsb_s = "one";
		break;
	    case H5T_PAD_BACKGROUND:
		pmsb_s = "bkg";
		break;
	    case H5T_PAD_ERROR:
	    case H5T_NPAD:
		pmsb_s = "unknown";
		break;
	    }
	}
	if (plsb_s || pmsb_s) {
	    printf("\n%*s(", indent, "");
	    if (plsb_s) {
		nbits = H5Tget_offset(type);
		printf("%lu %s bit%s at bit 0",
		       (unsigned long)nbits, plsb_s, 1==nbits?"":"s");
	    }
	    if (plsb_s && pmsb_s) printf(", ");
	    if (pmsb_s) {
		nbits = 8*H5Tget_size(type)-(H5Tget_offset(type)+prec);
		printf("%lu %s bit%s at bit %lu",
		       (unsigned long)nbits, pmsb_s, 1==nbits?"":"s",
		       (unsigned long)(8*H5Tget_size(type)-nbits));
	    }
	    printf(")");
	}
    }
}


/*-------------------------------------------------------------------------
 * Function:	display_int_type
 *
 * Purpose:	Print the name of an integer data type.  Common information
 *		like number of bits, byte order, and sign scheme appear on
 *		the first line. Additional information might appear in
 *		parentheses on the following lines.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE, nothing printed
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_int_type(hid_t type, int indent)
{
    H5T_order_t		order;		/*byte order value		*/
    const char 		*order_s=NULL;	/*byte order string		*/
    H5T_sign_t		sign;		/*sign scheme value		*/
    const char		*sign_s=NULL;	/*sign scheme string		*/
    
    if (H5T_INTEGER!=H5Tget_class(type)) return FALSE;
    
    /* Byte order */
    if (H5Tget_size(type)>1) {
	order = H5Tget_order(type);
	if (H5T_ORDER_LE==order) {
	    order_s = " little-endian";
	} else if (H5T_ORDER_BE==order) {
	    order_s = " big-endian";
	} else if (H5T_ORDER_VAX==order) {
	    order_s = " mixed-endian";
	} else {
	    order_s = " unknown-byte-order";
	}
    } else {
	order_s = "";
    }

    /* Sign */
    if (H5T_SGN_NONE==sign) {
	sign_s = " unsigned";
    } else if (H5T_SGN_2==sign) {
	sign_s = "";
    } else {
	sign_s = " unknown-sign";
    }
    
    /*
     * Print size, order, and sign on first line, precision and padding
     * information on the subsequent lines
     */
    printf("%lu-bit%s%s integer",
	   (unsigned long)(8*H5Tget_size(type)), order_s, sign_s);
    display_precision(type, indent);
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:	display_float_type
 *
 * Purpose:	Print info about a floating point data type.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE, nothing printed
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_float_type(hid_t type, int indent)
{
    H5T_order_t		order;		/*byte order value		*/
    const char 		*order_s=NULL;	/*byte order string		*/
    size_t		spos;		/*sign bit position		*/
    size_t		esize, epos;	/*exponent size and position	*/
    size_t		msize, mpos;	/*significand size and position	*/
    size_t		ebias;		/*exponent bias			*/
    H5T_norm_t		norm;		/*significand normalization	*/
    const char		*norm_s=NULL;	/*normalization string		*/
    H5T_pad_t		pad;		/*internal padding value	*/
    const char		*pad_s=NULL;	/*internal padding string	*/
    
    if (H5T_FLOAT!=H5Tget_class(type)) return FALSE;
    
    /* Byte order */
    if (H5Tget_size(type)>1) {
	order = H5Tget_order(type);
	if (H5T_ORDER_LE==order) {
	    order_s = " little-endian";
	} else if (H5T_ORDER_BE==order) {
	    order_s = " big-endian";
	} else if (H5T_ORDER_VAX==order) {
	    order_s = " mixed-endian";
	} else {
	    order_s = " unknown-byte-order";
	}
    } else {
	order_s = "";
    }

    /*
     * Print size and byte order on first line, precision and padding on
     * subsequent lines.
     */
    printf("%lu-bit%s floating-point",
	   (unsigned long)(8*H5Tget_size(type)), order_s);
    display_precision(type, indent);

    /* Print sizes, locations, and other information about each field */
    H5Tget_fields (type, &spos, &epos, &esize, &mpos, &msize);
    ebias = H5Tget_ebias(type);
    norm = H5Tget_norm(type);
    switch (norm) {
    case H5T_NORM_IMPLIED:
	norm_s = ", msb implied";
	break;
    case H5T_NORM_MSBSET:
	norm_s = ", msb always set";
	break;
    case H5T_NORM_NONE:
	norm_s = ", no normalization";
	break;
    case H5T_NORM_ERROR:
	norm_s = ", unknown normalization";
	break;
    }
    printf("\n%*s(significant for %lu bit%s at bit %lu%s)", indent, "", 
	   (unsigned long)msize, 1==msize?"":"s", (unsigned long)mpos,
	   norm_s);
    printf("\n%*s(exponent for %lu bit%s at bit %lu, bias is 0x%lx)",
	   indent, "", (unsigned long)esize, 1==esize?"":"s",
	   (unsigned long)epos, (unsigned long)ebias);
    printf("\n%*s(sign bit at %lu)", indent, "", (unsigned long)spos);
    return TRUE;

    /* Display internal padding */
    if (1+esize+msize<H5Tget_precision(type)) {
	pad = H5Tget_inpad(type);
	switch (pad) {
	case H5T_PAD_ZERO:
	    pad_s = "zero";
	    break;
	case H5T_PAD_ONE:
	    pad_s = "one";
	    break;
	case H5T_PAD_BACKGROUND:
	    pad_s = "bkg";
	    break;
	case H5T_PAD_ERROR:
	case H5T_NPAD:
	    pad_s = "unknown";
	    break;
	}
	printf("\n%*s(internal padding bits are %s)", indent, "", pad_s);
    }
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:	display_cmpd_type
 *
 * Purpose:	Print info about a compound data type.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE, nothing printed
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_cmpd_type(hid_t type, int indent)
{
    char	*name=NULL;	/*member name				*/
    int		ndims;		/*dimensionality			*/
    size_t	dims[8];	/*dimensions				*/
    int		perm[8];	/*index permutation			*/
    hid_t	subtype;	/*member data type			*/
    int		i, j, n;	/*miscellaneous counters		*/
    
    
    if (H5T_COMPOUND!=H5Tget_class(type)) return FALSE;
    printf("struct {");
    for (i=0; i<H5Tget_nmembers(type); i++) {

	/* Name and offset */
	name = H5Tget_member_name(type, i);
	printf("\n%*s\"", indent+4, "");
	n = display_string(name);
	printf("\"%*s +%-4lu ", MAX(0, 16-n), "",
	       (unsigned long)H5Tget_member_offset(type, i));
	free(name);

	/* Dimensions and permutation */
	ndims = H5Tget_member_dims(type, i, dims, perm);
	if (ndims>0) {
	    printf("[");
	    for (j=0; j<ndims; j++) {
		printf("%s%lu", j?",":"", (unsigned long)(dims[j]));
	    }
	    printf("]");
	    for (j=0; j<ndims; j++) {
		if (perm[j]!=j) break;
	    }
	    if (j<ndims) {
		printf("x[");
		for (j=0; j<ndims; j++) {
		    printf("%s%d", j?",":"", perm[j]);
		}
		printf("]");
	    }
	    printf(" ");
	}
	
	/* Data type */
	subtype = H5Tget_member_type(type, i);
	display_type(subtype, indent+4);
	H5Tclose(subtype);
    }
    printf("\n%*s}", indent, "");
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:	display_string
 *
 * Purpose:	Print information about a string data type.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE, nothing printed
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_string_type(hid_t type, int __unused__ indent)
{
    H5T_str_t		pad;
    const char		*pad_s=NULL;
    H5T_cset_t		cset;
    const char		*cset_s=NULL;
    
    if (H5T_STRING!=H5Tget_class(type)) return FALSE;

    /* Padding */
    pad = H5Tget_strpad(type);
    switch (pad) {
    case H5T_STR_NULLTERM:
	pad_s = "null-terminated";
	break;
    case H5T_STR_NULLPAD:
	pad_s = "null-padded";
	break;
    case H5T_STR_SPACEPAD:
	pad_s = "space-padded";
	break;
    case H5T_STR_RESERVED_3:
    case H5T_STR_RESERVED_4:
    case H5T_STR_RESERVED_5:
    case H5T_STR_RESERVED_6:
    case H5T_STR_RESERVED_7:
    case H5T_STR_RESERVED_8:
    case H5T_STR_RESERVED_9:
    case H5T_STR_RESERVED_10:
    case H5T_STR_RESERVED_11:
    case H5T_STR_RESERVED_12:
    case H5T_STR_RESERVED_13:
    case H5T_STR_RESERVED_14:
    case H5T_STR_RESERVED_15:
    case H5T_STR_ERROR:
	pad_s = "unknown-format";
	break;
    }

    /* Character set */
    cset = H5Tget_cset(type);
    switch (cset) {
    case H5T_CSET_ASCII:
	cset_s = "ASCII";
	break;
    case H5T_CSET_RESERVED_1:
    case H5T_CSET_RESERVED_2:
    case H5T_CSET_RESERVED_3:
    case H5T_CSET_RESERVED_4:
    case H5T_CSET_RESERVED_5:
    case H5T_CSET_RESERVED_6:
    case H5T_CSET_RESERVED_7:
    case H5T_CSET_RESERVED_8:
    case H5T_CSET_RESERVED_9:
    case H5T_CSET_RESERVED_10:
    case H5T_CSET_RESERVED_11:
    case H5T_CSET_RESERVED_12:
    case H5T_CSET_RESERVED_13:
    case H5T_CSET_RESERVED_14:
    case H5T_CSET_RESERVED_15:
    case H5T_CSET_ERROR:
	cset_s = "unknown-character-set";
	break;
    }

    printf("%lu-byte %s %s string",
	   (unsigned long)H5Tget_size(type), pad_s, cset_s);
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:	display_reference_type
 *
 * Purpose:	Prints information about a reference data type.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE, nothing printed
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
display_reference_type(hid_t type, int __unused__ indent)
{
    if (H5T_REFERENCE!=H5Tget_class(type)) return FALSE;

    printf("%lu-byte unknown reference",
	   (unsigned long)H5Tget_size(type));
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:	display_type
 *
 * Purpose:	Prints a data type definition.  The definition is printed
 *		without any leading space or trailing line-feed (although
 *		there might be line-feeds inside the type definition).  The
 *		first line is assumed to have INDENT characters before it on
 *		the same line (printed by the caller).
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
display_type(hid_t type, int indent)
{
    H5T_class_t		data_class;
    
    /* Bad data type */
    if (type<0) {
	printf("<ERROR>");
	return;
    }

    /* Shared? */
    if (H5Tcommitted(type)) printf("shared ");

    /* Print the type */
    if (display_native_type(type, indent) ||
	display_ieee_type(type, indent) ||
	display_int_type(type, indent) ||
	display_float_type(type, indent) ||
	display_cmpd_type(type, indent) ||
	display_string_type(type, indent) ||
	display_reference_type(type, indent)) {
	return;
    }

    /* Unknown type */
    printf("%lu-byte class-%u unknown",
	   (unsigned long)H5Tget_size(type),
	   (unsigned)data_class);
}


/*-------------------------------------------------------------------------
 * Function:	dump_dataset_values
 *
 * Purpose:	Prints all values of a dataset.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_dataset_values(hid_t dset)
{
    hid_t		f_type = H5Dget_type(dset);
    size_t		size = H5Tget_size(f_type);
    h5dump_t		info;

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);
    info.idx_fmt = "        (%s) ";
    info.line_ncols = width_g;
    if (label_g) info.cmpd_name = "%s=";

    /*
     * If the dataset is a 1-byte integer type then format it as an ASCI
     * character string instead of integers if the `-s' or `--string'
     * command-line option was given.
     */
    if (string_g && 1==size && H5T_INTEGER==H5Tget_class(f_type)) {
	info.ascii = TRUE;
	info.elmt_suf1 = "";
	info.elmt_suf2 = "";
	info.idx_fmt = "        (%s) \"";
	info.line_suf = "\"";
    }

    
    /*
     * Print all the values.
     */
    printf("    Data:\n");
    if (h5dump(stdout, &info, dset, -1)<0) {
	printf("        Unable to print data.\n");
    }

    H5Tclose(f_type);
}


/*-------------------------------------------------------------------------
 * Function:	list_attr
 *
 * Purpose:	Prints information about attributes.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, June  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list_attr (hid_t obj, const char *attr_name, void __unused__ *op_data)
{
    hid_t	attr;
    int		i;

    printf("    %-10s %-10s", "Attribute:", attr_name);
    if ((attr = H5Aopen_name (obj, attr_name))) {
	hid_t space = H5Aget_space (attr);
	hsize_t size[64];
	int ndims = H5Sget_simple_extent_dims (space, size, NULL);
	H5Sclose (space);
	printf (" {");
	for (i=0; i<ndims; i++) {
	    HDfprintf (stdout, "%s%Hu", i?", ":"", size[i]);
	}
	putchar ('}');
	H5Aclose (attr);
    }
    
    putchar ('\n');
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	dataset_list1
 *
 * Purpose:	List information about a dataset which should appear on the
 *		same line as the dataset name.  This information will precede
 *		information which is applicable to all objects which will be
 *		printed by the caller.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_list1(hid_t dset)
{
    hsize_t		cur_size[64];	/*current dataset dimensions	*/
    hsize_t		max_size[64];	/*maximum dataset dimensions	*/
    hid_t		space;		/*data space			*/
    int 		ndims;		/*dimensionality		*/
    int			i;

    /*
     * Information that goes on the same row as the name.  The name has
     * already been printed.
     */
    space = H5Dget_space(dset);
    ndims = H5Sget_simple_extent_dims(space, cur_size, max_size);
    printf (" {");
    for (i=0; i<ndims; i++) {
	HDfprintf (stdout, "%s%Hu", i?", ":"", cur_size[i]);
	if (max_size[i]==H5S_UNLIMITED) {
	    HDfprintf (stdout, "/%s", "Inf");
	} else if (max_size[i]!=cur_size[i] || verbose_g>0) {
	    HDfprintf(stdout, "/%Hu", max_size[i]);
	}
    }
    if (0==ndims) printf("SCALAR");
    putchar('}');
    H5Sclose (space);

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	dataset_list2
 *
 * Purpose:	List information about a dataset which should appear after
 *		information which is general to all objects.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_list2(hid_t dset)
{
    hid_t		dcpl;		/*dataset creation property list*/
    hid_t		type;		/*data type of dataset		*/
    hid_t		space;		/*data space of dataset		*/
    int			nf;		/*number of filters		*/
    unsigned		filt_flags;	/*filter flags			*/
    H5Z_filter_t	filt_id;	/*filter identification number	*/
    unsigned		cd_values[20];	/*filter client data values	*/
    size_t		cd_nelmts;	/*filter client number of values*/
    size_t		cd_num;		/*filter client data counter	*/
    char		f_name[256];	/*filter/file name		*/
    char		s[64];		/*temporary string buffer	*/
    off_t		f_offset;	/*offset in external file	*/
    hsize_t		f_size;		/*bytes used in external file	*/
    hsize_t		total;		/*total size or offset		*/
    hsize_t		chsize[64];	/*chunk size in elements	*/
    int			ndims;		/*dimensionality		*/
    int			i;
    
    if (verbose_g>0) {
	dcpl = H5Dget_create_plist(dset);
	space = H5Dget_space(dset);
	type = H5Dget_type(dset);

	/* Print information about chunked storage */
	if (H5D_CHUNKED==H5Pget_layout(dcpl)) {
	    ndims = H5Pget_chunk(dcpl, NELMTS(chsize), chsize/*out*/);
	    printf("    %-10s {", "Chunks:");
	    total = H5Tget_size(type);
	    for (i=0; i<ndims; i++) {
		printf("%s%lu", i?", ":"", (unsigned long)(chsize[i]));
		total *= chsize[i];
	    }
	    printf("} %lu bytes\n", (unsigned long)total);
	}

	/* Print information about external strorage */
	if ((nf = H5Pget_external_count(dcpl))>0) {
	    printf("    %-10s %d external file%s (num/addr/offset/length)\n",
		   "Extern:", nf, 1==nf?"":"s");
	    for (i=0, total=0; i<nf; i++) {
		if (H5Pget_external(dcpl, i, sizeof(f_name), f_name, &f_offset,
				    &f_size)<0) {
		    HDfprintf(stdout,
			      "        #%03d %10Hu %10s %10s ***ERROR*** %s\n",
			      i, total, "", "",
			      i+1<nf?"Following addresses are incorrect":"");
		} else if (H5S_UNLIMITED==f_size) {
		    HDfprintf(stdout, "        #%03d %10Hu %10Hu %10s \"",
			      i, total, (hsize_t)f_offset, "INF");
		    display_string(f_name);
		} else {
		    HDfprintf(stdout, "        #%03d %10Hu %10Hu %10Hu \"",
			      i, total, (hsize_t)f_offset, f_size);
		    display_string(f_name);
		}
		printf("\"\n");
		total += f_size;
	    }
	}

	/* Print information about raw data filters */
	if ((nf = H5Pget_nfilters(dcpl))>0) {
	    for (i=0; i<nf; i++) {
		cd_nelmts = NELMTS(cd_values);
		filt_id = H5Pget_filter(dcpl, i, &filt_flags, &cd_nelmts,
					cd_values, sizeof(f_name), f_name);
		f_name[sizeof(f_name)-1] = '\0';
		sprintf(s, "Filter-%d:", i);
		printf("    %-10s %s-%u %s {", s,
		       f_name[0]?f_name:"method",
		       (unsigned)filt_id,
		       filt_flags & H5Z_FLAG_OPTIONAL?"OPT":"");
		for (cd_num=0; cd_num<cd_nelmts; cd_num++) {
		    printf("%s%u", cd_num?", ":"", cd_values[cd_num]);
		}
		printf("}\n");
	    }
	}

	/* Print data type */
	printf("    %-10s ", "Type:");
	display_type(type, 15);
	printf("\n");

	/* Close stuff */
	H5Tclose(type);
	H5Sclose(space);
	H5Pclose(dcpl);
    }

    if (dump_g) dump_dataset_values(dset);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	datatype_list2
 *
 * Purpose:	List information about a data type which should appear after
 *		information which is general to all objects.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
datatype_list2(hid_t type)
{
    printf("    %-10s ", "Type:");
    display_type(type, 15);
    printf("\n");
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	ragged_list2
 *
 * Purpose:	List information about a ragged array which should appear
 *		after information which is general to all objects.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, November  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
ragged_list2(hid_t __unused__ ra)
{
    if (dump_g) {
	puts("    Data:      Not implemented yet (see values of member");
	puts("               datasets `raw', `over', and `meta')");
    }
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	link_open
 *
 * Purpose:	This gets called to open a symbolic link.  Since symbolic
 *		links don't correspond to actual objects we simply print the
 *		link information and return failure.
 *
 * Return:	Success:	never succeeds
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
link_open(hid_t location, const char *name)
{
    char	buf[64];
    
    if (H5Gget_linkval (location, name, sizeof(buf), buf)<0) return -1;
    if (NULL==HDmemchr(buf, 0, sizeof(buf))) {
	strcpy(buf+sizeof(buf)-4, "...");
    }
    puts(buf);

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	list
 *
 * Purpose:	Prints the group member name.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list (hid_t group, const char *name, void __unused__ *cd)
{
    hid_t	obj;
    char	buf[512], comment[50];
    H5G_stat_t	sb;
    struct tm	*tm;
    herr_t	status;
    
    /* Print the object name */
    printf("%-25s ", name);
    
    /* Get object information */
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(group, name, FALSE, &sb);
    } H5E_END_TRY;
    if (status<0) {
	puts("**NOT FOUND**");
	return 0;
    } else if (sb.type<0 || sb.type>=H5G_NTYPES) {
	printf("Unknown type(%d)", sb.type);
	sb.type = -1;
    }
    if (sb.type>=0 && dispatch_g[sb.type].name) {
	fputs(dispatch_g[sb.type].name, stdout);
    }

    /*
     * Open the object.  Not all objects can be opened.  If this is the case
     * then return right away.
     */
    if (sb.type>=0 &&
	(NULL==dispatch_g[sb.type].open ||
	 (obj=(dispatch_g[sb.type].open)(group, name))<0)) return 0;

    /*
     * List the first line of information for the object.
     */
    if (sb.type>=0 && dispatch_g[sb.type].list1) {
	(dispatch_g[sb.type].list1)(obj);
    }
    putchar('\n');
    
    /*
     * Show detailed information about the object, beginning with information
     * which is common to all objects.
     */
    if (verbose_g>0) {
	if (sb.type>=0) H5Aiterate(obj, NULL, list_attr, NULL);
	printf("    %-10s %lu:%lu:%lu:%lu\n", "Location:",
	       sb.fileno[1], sb.fileno[0], sb.objno[1], sb.objno[0]);
	printf("    %-10s %u\n", "Links:", sb.nlink);
	if (sb.mtime>0 && NULL!=(tm=localtime(&(sb.mtime)))) {
	    strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
	    printf("    %-10s %s\n", "Modified:", buf);
	}
	comment[0] = '\0';
	H5Gget_comment(group, name, sizeof(comment), comment);
	strcpy(comment+sizeof(comment)-4, "...");
	if (comment[0]) printf("    %-10s %s\n", "Comment:", comment);
    }
    if (sb.type>0 && dispatch_g[sb.type].list2) {
	(dispatch_g[sb.type].list2)(obj);
    }
    
    /*
     * Close the object.
     */
    if (sb.type>0) (dispatch_g[sb.type].close)(obj);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	get_width
 *
 * Purpose:	Figure out how wide the screen is.  This is highly
 *		unportable, but the user can always override the width we
 *		detect by giving a command-line option. These code snippets
 *		were borrowed from the GNU less(1).
 *
 * Return:	Success:	Number of columns.
 *
 *		Failure:	Some default number of columms.
 *
 * Programmer:	Robb Matzke
 *              Friday, November  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
get_width(void)
{
    int		width = 80;		/*the default			*/
    char	*s;

    /*
     * Try to get it from the COLUMNS environment variable first since it's
     * value is sometimes wrong.
     */
    if ((s=getenv("COLUMNS")) && *s && isdigit(*s)) {
	width = strtol(s, NULL, 0);
    }

#if defined(HAVE_STRUCT_VIDEOCONFIG) && defined(HAVE__GETVIDEOCONFIG)
    {
	/* Microsoft C */
	struct videoconfig w;
	_getvideoconfig(&w);
	width = w.numtextcols;
    }
#elif defined(HAVE_STRUCT_TEXT_INFO) && defined(HAVE_GETTEXTINFO)
    {
	/* Borland C or DJGPPC */
	struct text_info w;
	gettextinfo(&w);
	width = w.screenwidth;
    }
#elif defined(HAVE_GETCONSOLESCREENBUFFERINFO)
    {
	/* Win32 C */
	CONSOLE_SCREEN_BUFFER_INFO scr;
	GetConsoleScreenBufferInfo(con_out, &scr);
	width = scr.srWindow.Right - scr.srWindow.Left + 1;
    }
#elif defined(HAVE__SCRSIZE)
    {
	/* OS/2 */
	int w[2];
	_scrsize(w);
	width = w[0];
    }
#elif defined(HAVE_TIOCGWINSZ) && defined(HAVE_IOCTL)
    {
	/* Unix with ioctl(TIOCGWINSZ) */
	struct winsize w;
	if (ioctl(2, TIOCGWINSZ, &w)>=0 && w.ws_col>0) {
	    width = w.ws_col;
	}
    }
#elif defined(HAVE_TIOCGETD) && defined(HAVE_IOCTL)
    {
	/* Unix with ioctl(TIOCGETD) */
	struct uwdata w;
	if (ioctl(2, WIOCGETD, &w)>=0 && w.uw_width>0) {
	    width = w.uw_width / w.uw_hs;
	}
    }
#endif

    /* Set to at least 1 */
    if (width<1) width = 1;
    return width;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Opens a file and lists the specified group
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    hid_t	file, plist=H5P_DEFAULT, root;
    const char	*fname = NULL;
    const char	*progname;
    const char	*s;
    char	*rest;
    int		argno;
    H5G_stat_t	sb;

    DISPATCH(H5G_DATASET, "Dataset", H5Dopen, H5Dclose,
	     dataset_list1, dataset_list2);
    DISPATCH(H5G_GROUP, "Group", H5Gopen, H5Gclose,
	     NULL, NULL);
    DISPATCH(H5G_TYPE, "Type", H5Topen, H5Tclose,
	     NULL, datatype_list2);
    DISPATCH(H5G_LINK, "-> ", link_open, NULL,
	     NULL, NULL);
    DISPATCH(H5G_RAGGED, "Ragged Array", H5Gopen, H5Gclose,
	     NULL, ragged_list2);

    /* Name of this program without the path */
    if ((progname=strrchr (argv[0], '/'))) progname++;
    else progname = argv[0];

    /* Default output width */
    width_g = get_width();

    /* Switches come before non-switch arguments */
    for (argno=1; argno<argc && '-'==argv[argno][0]; argno++) {
	if (!strcmp(argv[argno], "--")) {
	    /* Last switch */
	    argno++;
	    break;
	} else if (!strcmp(argv[argno], "--help")) {
	    usage(progname);
	    exit(0);
	} else if (!strcmp(argv[argno], "--dump")) {
	    dump_g = TRUE;
	} else if (!strcmp(argv[argno], "--label")) {
	    label_g = TRUE;
	} else if (!strcmp(argv[argno], "--string")) {
	    string_g = TRUE;
	} else if (!strncmp(argv[argno], "--width=", 8)) {
	    width_g = strtol(argv[argno]+8, &rest, 0);
	    if (width_g<=0 || *rest) {
		usage(progname);
		exit(1);
	    }
	} else if (!strcmp(argv[argno], "--verbose")) {
	    verbose_g++;
	} else if (!strcmp(argv[argno], "--version")) {
	    printf("This is %s version %u.%u release %u\n",
		   progname, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
	    exit(0);
	} else if (!strncmp(argv[argno], "-w", 2)) {
	    if (argv[argno][2]) {
		s = argv[argno]+2;
	    } else if (argno+1>=argc) {
		usage(progname);
		exit(1);
	    } else {
		s = argv[++argno];
	    }
	    width_g = strtol(s, &rest, 0);
	    if (width_g<=0 || *rest) {
		usage(progname);
		exit(1);
	    }
	} else if ('-'!=argv[argno][1]) {
	    /* Single-letter switches */
	    for (s=argv[argno]+1; *s; s++) {
		switch (*s) {
		case '?':
		case 'h':	/* --help */
		    usage(progname);
		    exit(0);
		case 'd':	/* --dump */
		    dump_g++;
		    break;
		case 'l':	/* --label */
		    label_g = TRUE;
		    break;
		case 's':	/* --string */
		    string_g = TRUE;
		    break;
		case 'v':	/* --verbose */
		    verbose_g++;
		    break;
		case 'V':	/* --version */
		    printf("This is %s version %u.%u release %u\n",
			   progname, H5_VERS_MAJOR, H5_VERS_MINOR,
			   H5_VERS_RELEASE);
		    exit(0);
		default:
		    usage(progname);
		    exit(1);
		}
	    }
	} else {
	    usage(progname);
	    exit(1);
	}
    }

    /*
     * The first non-switch argument is a file name.  If the file name
     * contains a `%' then assume that a file family is being opened.
     */
    if (argno<argc) {
	fname = argv[argno++];
    } else {
	usage(progname);
	exit(1);
    }
    if (strchr (fname, '%')) {
	plist = H5Pcreate (H5P_FILE_ACCESS);
	H5Pset_family (plist, 0, H5P_DEFAULT);
    }
    if ((file = H5Fopen (fname, H5F_ACC_RDONLY, plist))<0) exit (1);

    /*
     * The remaining optional arguments are the names of the objects to list.
     * If there are no arguments then list `/'.
     */
    if (argno>=argc) {
	H5Giterate(file, "/", NULL, list, NULL);
    } else {
	for (/*void*/; argno<argc; argno++) {
	    if (H5Gget_objinfo(file, argv[argno], TRUE, &sb)>=0 &&
		H5G_GROUP==sb.type) {
		H5Giterate(file, argv[argno], NULL, list, NULL);
	    } else if ((root=H5Gopen(file, "/"))<0) {
		exit(1);
	    } else {
		list(root, argv[argno], NULL);
		if (H5Gclose(root)<0) exit(1);
	    }
	}
    }

    if (H5Fclose(file)<0) exit(1);
    return 0;
}
