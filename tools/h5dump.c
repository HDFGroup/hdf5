#include "h5dump.h"
#include <stdio.h>
#include "H5private.h"
#include "h5tools.h"

static int display_oid = 0;
static int display_data = 1;
static int status = 0;
static int unamedtype = 0;  /* shared data type with no name */

static int prefix_len = 1024;
static char *prefix;
static table_t *group_table = NULL, *dset_table = NULL, *type_table = NULL;
static dump_header *dump_header_format;


static void dump_group (hid_t , const char* );
static void dump_dataset (hid_t, const  char*);
static void dump_data (hid_t, int);
static void dump_named_datatype (hid_t , const char *);

void indentation(int);
static void print_enum(hid_t type);

extern int print_data(hid_t, hid_t, int);
static void dump_oid(hid_t oid);


static h5dump_t dataformat = {

	0,/*raw*/

	"", /*fmt_raw*/
	"%d",	 /*fmt_int*/
	"%u", /*fmt_uint*/
	"%d", /*fmt_schar*/
	"%u", /*fmt_uchar*/
	"%d", /*fmt_short*/
	"%u", /*fmt_ushort*/
	"%ld", /*fmt_long*/
	"%lu", /*fmt_ulong*/
	NULL, /*fmt_llong*/
	NULL, /*fmt_ullong*/
	"%g", /*fmt_double*/
	"%g", /*fmt_float*/

	0,     /*ascii*/
	0, /*str_locale*/
	0,/*str_repeat*/


	"[ ",	/*arr_pre*/
	", ",	/*arr_sep*/
	" ]",	/*arr_suf*/
	1,	/*arr_linebreak*/

	"", /*cmpd_name*/
	",\n",/*cmpd_sep*/
	"{\n",/*cmpd_pre*/
	"}",/*cmpd_suf*/
	"\n",/*cmpd_end*/

	"%s", /*elmt_fmt*/
	",",/*elmt_suf1*/
	" ",	/*elmt_suf2*/

	"", /*idx_n_fmt*/
	"",/*idx_sep*/
	"",/*idx_fmt*/

	80, /*line_ncols*//*standard default columns*/
	0, /*line_per_line*/
	"         ",/*line_pre*/
	"        %s ",/*line_1st*/
	"        %s",/*line_cont*/
	"",/*line_suf*/
	"",/*line_sep*/
	1,/*line_multi_new*/
	"   ",/*line_indent*/

	1, /*skip_first*/

	1,/*obj_hidefileno*/
	" %lu:%lu", /*obj_format*/

	1, /*dset_hidefileno*/
	"DATASET %lu:%lu ",/*dset_format*/
	"%s",/*dset_blockformat_pre*/
	"%s",/*dset_ptformat_pre*/
	"%s",/*dset_ptformat*/
	

};
static const dump_header standardformat = {
	"standardformat",	/*name*/
	"HDF5",				/*fileebgin*/
	"",					/*fileend*/
	BOOT_BLOCK,			/*bootblockbegin*/
	"",					/*bootblockend*/
	GROUPNAME,			/*groupbegin*/
	"",					/*groupend*/
	DATASET,			/*datasetbegin*/
	"",					/*datasetend*/
	ATTRIBUTE,			/*attributebegin*/
	"",					/*attributeend*/
	DATATYPE,			/*datatypebegin*/
	"",					/*datatypeend*/
	DATASPACE,			/*dataspacebegin*/
	"",					/*dataspaceend*/
	DATA,				/*databegin*/
	"",					/*dataend*/
	SOFTLINK,			/*softlinkbegin*/
	"",					/*softlinkend*/


	"{",				/*fileblockbegin*/
	"}",				/*fileblockend*/
	"{",				/*bootblockblockbegin*/
	"}",				/*bootblockblockend*/
	"{",				/*groupblockbegin*/
	"}",				/*groupblockend*/
	"{",				/*datasetblockbegin*/
	"}",				/*datasetblockend*/
	"{",				/*attributeblockbegin*/
	"}",				/*attributeblockend*/
	"{",				/*datatypeblockbegin*/
	"}",				/*datatypeblockend*/
	"{",				/*dataspaceblockbegin*/
	"}",				/*dataspaceblockend*/
	"{",				/*datablockbegin*/
	"}",				/*datablockend*/
	"{",				/*softlinkblockbegin*/
	"}",				/*softlinkblockend*/
	"{",				/*strblockbegin*/
	"}",				/*strblockend*/
	"{",				/*enumblockbegin*/
	"}",				/*enumblockend*/

	"{",				/*dataspacedescriptionbegin*/
	"}",				/*dataspacedescriptionend*/
	"(",				/*dataspacedimbegin*/
	")",				/*dataspacedimend*/
};

static const dump_header xmlformat = {
	"xml", /*name*/
	"<FILE>",         /*filebegin*/
	"</FILE>",         /*fileend*/
	"<BOOTBLOCK>",         /*bootblockbegin*/
	"</BOOTBLOCK>",         /*bootblockend*/
	"<GROUP>",         /*groupbegin*/
	"</GROUP>",	         /*groupend*/
	"<DATASET>",	         /*datasetbegin*/
	"</DATASET>",		/*datasetend*/	
	"<ATTRIBUTE>",         /*attributebegin*/
	"</ATTRIBUTE>",	          /*attributeend*/
	"<DATATYPE>",          /*datatypeend*/
	"</DATATYPE>",	          /*datatypeend*/
	"<DATASPACE>",	          /*dataspacebegin*/
	"</DATASPACE>",          /*dataspaceend*/
	"<DATA>",	          /*databegin*/
	"</DATA>",          /*dataend*/
	"<SOFTLINK>",          /*softlinkbegin*/
	"</SOFTLINK>",          /*softlinkend*/

	"",          /*fileblockbegin*/
	"",          /*fileblockend*/
	"",          /*bootblockblockbegin*/
	"",          /*bootblockblockend*/
	"",          /*groupblockbegin*/
	"",          /*groupblockend*/
	"",          /*datasetblockbegin*/
	"",          /*datasetblockend*/
	"",          /*attributeblockbegin*/
	"",          /*attributeblockend*/
	"",          /*datatypeblockbegin*/
	"",          /*datatypeblockend*/
	"",          /*dataspaceblockbegin*/
	"",          /*dataspaceblockend*/
	"",          /*datablockbegin*/
	"",          /*datablockend*/
	"",          /*softlinkblockbegin*/
	"",          /*softlinkblockend*/
	"",				/*strblockbegin*/
	"",				/*strblockend*/
	"",				/*enumblockbegin*/
	"",				/*enumblockend*/

	"",           /*dataspacedescriptionbegin*/
	"",           /*dataspacedescriptionend*/
	"(",           /*dataspacedimbegin*/
	")",           /*dataspacedimend*/

};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message about dumper
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
usage(void)
{
fprintf(stderr, 
"\nUsage of HDF5 Dumper:\n\n  \
h5dump [-h] [-bb] [-header] [-a <names>] [-d <names>] [-g <names>]\n  \
       [-l <names>] [-t <names>] [-w <number>] <file>\n\n\
  -h            Print information on this command and exit.\n\
  -bb           Display the conent of the boot block. The default is not to display.\n\
  -header       Display header only; no data is displayed.\n\
  -v            Display the object ids\n\
  -V            Display version information and exit.\n\
  -xml			Display the output in XML format.\n\
  -a <names>    Display the specified attribute(s).\n\
  -d <names>    Display the specified dataset(s).\n\
  -g <names>    Display the specified group(s) and all the members.\n\
  -l <names>    Displays the value(s) of the specified soft link(s).\n\
  -t <names>    Display the specified named data type(s).\n\
  -w <number>   Display the information with the specified maximum number of columns.\n\
  \n\
  <names> is one or more appropriate object names.\n\
  <number> is an integer greater than 1.\n\n");
}




/*-------------------------------------------------------------------------
 * Function:    print_datatype
 *
 * Purpose:     print the data type.
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
print_datatype(hid_t type) {
char *fname ;
hid_t nmembers, mtype, str_type;
int i, j, ndims, perm[H5DUMP_MAX_RANK];
size_t dims[H5DUMP_MAX_RANK], size;
H5T_str_t str_pad; 
H5T_cset_t cset;
H5G_stat_t statbuf;

	hid_t super;

    switch (H5Tget_class(type)) {

    case H5T_INTEGER:

        if (H5Tequal(type, H5T_STD_I8BE))
            printf( "H5T_STD_I8BE");
        else if (H5Tequal(type, H5T_STD_I8LE))
            printf( "H5T_STD_I8LE");
        else if (H5Tequal(type, H5T_STD_I16BE))
            printf( "H5T_STD_I16BE");
        else if (H5Tequal(type, H5T_STD_I16LE))
            printf( "H5T_STD_I16LE");
        else if (H5Tequal(type, H5T_STD_I32BE))
            printf( "H5T_STD_I32BE");
        else if (H5Tequal(type, H5T_STD_I32LE))
            printf( "H5T_STD_I32LE");
        else if (H5Tequal(type, H5T_STD_I64BE))
            printf( "H5T_STD_I64BE");
        else if (H5Tequal(type, H5T_STD_I64LE))
            printf( "H5T_STD_I64LE");
        else if (H5Tequal(type, H5T_STD_U8BE))
            printf( "H5T_STD_U8BE");
        else if (H5Tequal(type, H5T_STD_U8LE))
            printf( "H5T_STD_U8LE");
        else if (H5Tequal(type, H5T_STD_U16BE))
            printf( "H5T_STD_U16BE");
        else if (H5Tequal(type, H5T_STD_U16LE))
            printf( "H5T_STD_U16LE");
        else if (H5Tequal(type, H5T_STD_U32BE))
            printf( "H5T_STD_U32BE");
        else if (H5Tequal(type, H5T_STD_U32LE))
            printf( "H5T_STD_U32LE");
        else if (H5Tequal(type, H5T_STD_U64BE))
            printf( "H5T_STD_U64BE");
        else if (H5Tequal(type, H5T_STD_U64LE))
            printf( "H5T_STD_U64LE");
        else if (H5Tequal(type, H5T_NATIVE_SCHAR)) 
            printf( "H5T_NATIVE_SCHAR");
        else if (H5Tequal(type, H5T_NATIVE_UCHAR))
            printf( "H5T_NATIVE_UCHAR");
        else if (H5Tequal(type, H5T_NATIVE_SHORT))
            printf( "H5T_NATIVE_SHORT");
        else if (H5Tequal(type, H5T_NATIVE_USHORT))
            printf( "H5T_NATIVE_USHORT");
        else if (H5Tequal(type, H5T_NATIVE_INT))
            printf( "H5T_NATIVE_INT");
        else if (H5Tequal(type, H5T_NATIVE_UINT))
            printf( "H5T_NATIVE_UINT");
        else if (H5Tequal(type, H5T_NATIVE_LONG))
            printf( "H5T_NATIVE_LONG");
        else if (H5Tequal(type, H5T_NATIVE_ULONG))
            printf( "H5T_NATIVE_ULONG");
        else if (H5Tequal(type, H5T_NATIVE_LLONG))
            printf( "H5T_NATIVE_LLONG");
        else if (H5Tequal(type, H5T_NATIVE_ULLONG))
            printf( "H5T_NATIVE_ULLONG");
        else {
            printf( "undefined integer");
            status = 1;
        }
        break;

    case H5T_FLOAT: 
        if (H5Tequal(type, H5T_IEEE_F32BE))
            printf( "H5T_IEEE_F32BE");
        else if (H5Tequal(type, H5T_IEEE_F32LE))
            printf( "H5T_IEEE_F32LE");
        else if (H5Tequal(type, H5T_IEEE_F64BE))
            printf( "H5T_IEEE_F64BE");
        else if (H5Tequal(type, H5T_IEEE_F64LE))
            printf( "H5T_IEEE_F64LE");
        else if (H5Tequal(type, H5T_NATIVE_FLOAT))
            printf( "H5T_NATIVE_FLOAT");
        else if (H5Tequal(type, H5T_NATIVE_DOUBLE))
            printf( "H5T_NATIVE_DOUBLE");
        else if (H5Tequal(type, H5T_NATIVE_LDOUBLE))
            printf( "H5T_NATIVE_LDOUBLE");
        else {
            printf( "undefined float");
            status = 1;
        }
        break;

    case H5T_TIME: 
        printf( "H5T_TIME: not yet implemented");
        break;

    case H5T_STRING: 

        size = H5Tget_size(type); 
        str_pad = H5Tget_strpad(type) ;
        cset = H5Tget_cset(type);

        indentation (indent+COL);
        printf("%s %s %d;\n", dump_header_format->strblockbegin, STRSIZE, (int)size);
  
        indentation (indent+COL);
        printf("  %s ", STRPAD);
        if (str_pad == H5T_STR_NULLTERM )

            printf("H5T_STR_NULLTERM;\n");

        else if (str_pad == H5T_STR_NULLPAD )

            printf("H5T_STR_NULLPAD;\n");

        else if (str_pad == H5T_STR_SPACEPAD )

            printf("H5T_STR_SPACEPAD;\n");

        else
            printf("H5T_STR_ERROR;\n");

        indentation (indent+COL);
        printf("  %s ", CSET);
        if (cset == H5T_CSET_ASCII)
            printf("H5T_CSET_ASCII;\n");
        else 
            printf("unknown_cset;\n");

        str_type = H5Tcopy(H5T_C_S1);
        H5Tset_cset(str_type, cset ) ;
        H5Tset_size(str_type, size);
        H5Tset_strpad(str_type, str_pad);

        indentation (indent+COL);
        printf("  %s ", CTYPE);
        if (H5Tequal(type,str_type)) {
            printf("H5T_C_S1;\n");
            H5Tclose(str_type);
        } else {

          H5Tclose(str_type);
          str_type = H5Tcopy(H5T_FORTRAN_S1);
          H5Tset_cset(str_type, cset ) ;
          H5Tset_size(str_type, size);
          H5Tset_strpad(str_type, str_pad );
          if (H5Tequal(type,str_type)) 
              printf( "H5T_FORTRAN_S1;\n");
          else {
            printf("unknown_one_character_type;\n ");
            status = 1;
          }
          H5Tclose(str_type);
        }
        indentation (indent+COL);
        printf("%s", dump_header_format->strblockend);

        break;

    case H5T_BITFIELD: 

        if (H5Tequal(type, H5T_STD_B8BE))
            printf( "H5T_STD_B8BE");
        else if (H5Tequal(type, H5T_STD_B8LE))
            printf( "H5T_STD_B8LE");
        else if (H5Tequal(type, H5T_STD_B16BE))
            printf( "H5T_STD_B16BE");
        else if (H5Tequal(type, H5T_STD_B16LE))
            printf( "H5T_STD_B16LE");
        else if (H5Tequal(type, H5T_STD_B32BE))
            printf( "H5T_STD_B32BE");
        else if (H5Tequal(type, H5T_STD_B32LE))
            printf( "H5T_STD_B32LE");
        else if (H5Tequal(type, H5T_STD_B64BE))
            printf( "H5T_STD_B64BE");
        else if (H5Tequal(type, H5T_STD_B64LE))
            printf( "H5T_STD_B64LE");
        else {
            printf( "undefined bitfield");
            status = 1;
        }
        break;

    case H5T_OPAQUE: 
        printf( "H5T_OPAQUE: not yet implemented");
        break;

    case H5T_COMPOUND: 

        if (H5Tcommitted(type) > 0) {
            H5Gget_objinfo(type, ".", TRUE, &statbuf);

            i = search_obj (type_table, statbuf.objno);

            indentation (indent+COL);
            if (i >= 0) {
                if (!type_table->objs[i].recorded) 
                    printf("\"/#%lu:%lu\"\n", type_table->objs[i].objno[0],
                                             type_table->objs[i].objno[1]);
                else
                    printf("\"%s\"\n", type_table->objs[i].objname);
            } else {
                printf("h5dump error: unknown committed type.\n");
                status = 1;
            }

        } else {

            nmembers = H5Tget_nmembers(type);
 
            for (i = 0; i < nmembers; i++) {
                 fname = H5Tget_member_name(type, i);

                 mtype = H5Tget_member_type(type, i);

                 ndims = H5Tget_member_dims(type, i, dims, perm);

                 if (H5Tget_class(mtype) != H5T_STRING)/* && (H5Tget_class(mtype) != H5T_COMPOUND))*/
                     indentation (indent+COL);
				 if (H5Tget_class(mtype) == H5T_COMPOUND) {
					 indent += COL; 
					 printf("{\n");
				 }
                 print_datatype(mtype);
				 if (H5Tget_class(mtype) == H5T_COMPOUND) {
					 indent -= COL;
                     indentation (indent+COL);
					 printf("}");
				 }
                 printf (" \"%s\"", fname);

                 if (ndims != 1 || dims[0] != 1) {
                     for (j = 0; j < ndims; j++) 
                          printf("[%d]",(int)dims[j]);
                 }

                 printf (";\n");

                 free (fname);
            }
        }

        break;
	case H5T_REFERENCE:
		printf("H5T_REFERENCE");
		break;
	case H5T_ENUM:
		printf("H5T_ENUM\n");
		indentation(indent + COL);
		printf("%s ", dump_header_format->enumblockbegin);	
		super = H5Tget_super(type);
		print_datatype(super);
		printf(";");
		print_enum(type);
		printf("\n");
		indentation (indent + COL);
		printf("%s", dump_header_format->enumblockend);
		break;
    default:
        printf( "unknown data type");
        status = 1;
        break;
    }

}


/*-------------------------------------------------------------------------
 * Function:    dump_bb
 *
 * Purpose:     Dump the boot block
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_bb(void) {

    printf ("%s %s boot block not yet implemented %s\n", BOOT_BLOCK, BEGIN, END);

}


/*-------------------------------------------------------------------------
 * Function:    dump_datatype
 *
 * Purpose:     Dump the data type. Data type can be HDF5 predefined
 *              atomic data type or committed/transient data type.
 *
 * Return:      void 
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_datatype (hid_t type) {

    indent += COL;
    indentation (indent);
    if (H5Tget_class(type) == H5T_COMPOUND) {
       printf ("%s %s\n", dump_header_format->datatypebegin , dump_header_format->datatypeblockbegin);
        print_datatype(type);
        indentation (indent);
        printf ("%s %s\n", dump_header_format->datatypeblockend, dump_header_format->datatypeend);
		
    } else if (H5Tget_class(type) == H5T_STRING) {
        printf ("%s %s\n", dump_header_format->datatypebegin, dump_header_format->datatypeblockbegin);
        print_datatype(type);
        printf("\n");
        indentation (indent);
		printf ("%s %s\n", dump_header_format->datatypeblockend, dump_header_format->datatypeend);
    } else {
        printf ("%s %s ", dump_header_format->datatypebegin, dump_header_format->datatypeblockbegin);
        print_datatype(type);
		printf (" %s %s\n", dump_header_format->datatypeblockend, dump_header_format->datatypeend);
    }
    indent -= COL;
}


/*-------------------------------------------------------------------------
 * Function:    dump_dataspace
 *
 * Purpose:     Dump the data space. Data space can be named data space,
 *              array, or others.
 *
 * Return:      void    
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_dataspace (hid_t space)
{
    hsize_t size[H5DUMP_MAX_RANK];
    hsize_t maxsize[H5DUMP_MAX_RANK]; 
    int ndims = H5Sget_simple_extent_dims(space, size, maxsize);
    int i;

    indentation (indent+COL);

    printf("%s ", dump_header_format->dataspacebegin);

    if (H5Sis_simple(space)) {

        if (ndims == 0) /* scalar dataspace */ 
            HDfprintf (stdout, "%s %s ", dump_header_format->dataspacedescriptionbegin, SCALAR);
        else { /* simple dataspace */
            HDfprintf (stdout, "%s %s %s %Hu",dump_header_format->dataspacedescriptionbegin, SIMPLE, 
					dump_header_format->dataspacedimbegin,size[0]);
            for (i = 1; i < ndims; i++) 
                HDfprintf (stdout, ", %Hu", size[i]);
            printf(" %s / ",dump_header_format->dataspacedimend);
           
           if (maxsize[0]==H5S_UNLIMITED)
               HDfprintf (stdout, "%s %s", dump_header_format->dataspacedimbegin,"H5S_UNLIMITED");
            else
               HDfprintf (stdout, "%s %Hu", dump_header_format->dataspacedimbegin,maxsize[0]);

            for (i = 1; i < ndims; i++) 
                 if (maxsize[i]==H5S_UNLIMITED)
                     HDfprintf (stdout, ", %s", "H5S_UNLIMITED");
                 else
                     HDfprintf (stdout, ", %Hu", maxsize[i]);

            printf(" %s ", dump_header_format->dataspacedimend);
        }

    } else
        printf("%s not yet implemented %s\n", BEGIN, END);

	end_obj(dump_header_format->dataspaceend, dump_header_format->dataspaceblockend);

}


/*-------------------------------------------------------------------------
 * Function:    dump_attr
 *
 * Purpose:     dump the attribute
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static herr_t
dump_attr (hid_t attr, const char *attr_name, void UNUSED *op_data)
{
hid_t  attr_id, type, space;

    indentation(indent);
    begin_obj (dump_header_format->attributebegin, attr_name, dump_header_format->attributeblockbegin); 

    if ((attr_id = H5Aopen_name (attr, attr_name))>= 0) {

        type = H5Aget_type(attr_id);
        space = H5Aget_space(attr_id);
        dump_datatype(type);
        dump_dataspace(space);
		if (display_oid){
			dump_oid(attr_id);
		}
        if (display_data) dump_data(attr_id, ATTRIBUTE_DATA);
        H5Tclose(type);
        H5Sclose(space);
		H5Aclose (attr_id);
        indentation (indent);
        end_obj(dump_header_format->attributeend,dump_header_format->attributeblockend );

    } else {
        indentation (indent+COL);
        printf("h5dump error: unable to open attribute.\n");
        indentation (indent);
        end_obj(dump_header_format->attributeend,dump_header_format->attributeblockend);
        status = 1;
        return FAIL;
    }


    return SUCCEED;

}

/*-------------------------------------------------------------------------
 * Function:    dump_selected_attr
 *
 * Purpose:     dump the selected attribute
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static herr_t
dump_selected_attr (hid_t loc_id, char *name)
{
int j;
char *obj_name, *attr_name;
hid_t  oid, attr_id, type, space;
H5G_stat_t statbuf;

    j = (int)strlen(name)-1;
    obj_name = malloc ((j+2)* sizeof(char));

    /* find the last / */
    while (name[j] != '/' && j >=0) j--;

    /* object name */
    if (j == -1) strcpy(obj_name, "/");
    else {
      strncpy(obj_name, name, j+1);
      obj_name[j+1] = '\0';
    }

    attr_name = name+j+1;
    begin_obj (dump_header_format->attributebegin, name, dump_header_format->attributeblockbegin);

    H5Gget_objinfo(loc_id, obj_name, FALSE , &statbuf);
    switch (statbuf.type) {
    case H5G_GROUP:
         if ((oid = H5Gopen (loc_id, obj_name))<0) {
             indentation (COL);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj(dump_header_format->attributeend, dump_header_format->attributeblockend);
             status = 1;
             return FAIL;
         }
         break;
    case H5G_DATASET:
         if ((oid = H5Dopen (loc_id, obj_name))<0) {
             indentation (COL);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj(dump_header_format->attributeend, dump_header_format->attributeblockend);
             status = 1;
             return FAIL;
         }
         break;
    case H5G_TYPE:
         if ((oid =  H5Topen(loc_id, obj_name)) < 0 ) {
             indentation (COL);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj(dump_header_format->attributeend, dump_header_format->attributeblockend);
             status = 1;
             return FAIL;
         } 
         break;
    default:
         indentation (COL);
         fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
         end_obj(dump_header_format->attributeend, dump_header_format->attributeblockend);
         status = 1;
         return FAIL;
    }

    if ((attr_id = H5Aopen_name (oid, attr_name))>= 0) {

        type = H5Aget_type(attr_id);
        space = H5Aget_space(attr_id);
        dump_datatype(type);
        dump_dataspace(space);
		if (display_oid){
			dump_oid(attr_id);
		}
        if (display_data) dump_data(attr_id, ATTRIBUTE_DATA);
        H5Tclose(type);
        H5Sclose(space);
		H5Aclose (attr_id);
        end_obj(dump_header_format->attributeend, dump_header_format->attributeblockend);

    } else {
        indentation (COL);
        printf("h5dump error: unable to open attribute.\n");
        end_obj(dump_header_format->attributeend, dump_header_format->attributeblockend);
        status = 1;
    }

    switch (statbuf.type) {
    case H5G_GROUP:
         if (H5Gclose (oid) < 0) {
         status = 1;
         return FAIL;
         } 
         break;
          
    case H5G_DATASET:
         if (H5Dclose (oid) < 0 ) {
             status = 1;
             return FAIL;
         } 
         break;

    case H5G_TYPE:
         if (H5Tclose(oid) < 0 ) {
             status = 1;
             return FAIL;
         } 
         break;
    default:
         status = 1;
         return FAIL;
    }

    free(obj_name);
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:    dump all
 *
 * Purpose:     Dump everything in the specified object
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static herr_t
dump_all (hid_t group, const char *name, void UNUSED *op_data)
{
hid_t obj;
char *buf, *tmp;
H5G_stat_t statbuf;
int i;


    H5Gget_objinfo(group, name, FALSE, &statbuf);

    tmp = (char *) malloc ((strlen(prefix)+strlen(name)+2) * sizeof(char));
    strcpy(tmp, prefix);

    switch (statbuf.type) {
    case H5G_LINK:
        indentation (indent);

        buf = malloc (statbuf.linklen*sizeof(char));

        begin_obj(dump_header_format->softlinkbegin, name, dump_header_format->softlinkblockbegin);
        indentation (indent+COL);
        if (H5Gget_linkval (group, name, statbuf.linklen, buf)>=0) 
            printf ("LINKTARGET \"%s\"\n", buf);
        else {
            printf ("h5dump error: unable to get link value.\n");
            status = 1;
        }

        indentation (indent);
        end_obj(dump_header_format->softlinkend, dump_header_format->softlinkblockend);
        free (buf);
        break;

    case H5G_GROUP:
         if ((obj=H5Gopen (group, name))>=0) {
             strcat(strcat(prefix,"/"), name);
             dump_group (obj, name);
             strcpy(prefix, tmp); 
             H5Gclose (obj);
         } else {
             printf ("h5dump error: unable to dump group %s\n",name);
             status = 1;
         }

         break;

    case H5G_DATASET:

         if ((obj=H5Dopen (group, name))>=0) {              

             /* hard link */
             H5Gget_objinfo(obj, ".", TRUE, &statbuf);
             if (statbuf.nlink > 1) {
                 i = search_obj (dset_table, statbuf.objno);
                 if (i < 0) {
                     indentation (indent);
                     begin_obj(dump_header_format->datasetbegin, name, dump_header_format->datasetblockbegin);
                     indentation (indent+COL);
                     printf("h5dump error: internal error\n");
                     indentation (indent);
                     end_obj(dump_header_format->datasetend, dump_header_format->datasetblockend);
                     status = 1;
                     goto done;
                 } else if (dset_table->objs[i].displayed) {
                     indentation (indent);
                     begin_obj(dump_header_format->datasetbegin, name, dump_header_format->datasetblockbegin);
                     indentation (indent+COL);
                     printf("%s \"%s\"\n", HARDLINK, dset_table->objs[i].objname);
                     indentation (indent);
                     end_obj(dump_header_format->datasetend, dump_header_format->datasetblockend);
                     goto done;
                 } else {
                     dset_table->objs[i].displayed = 1;
                     strcat(tmp,"/");
                     strcat(tmp,name); 
                     strcpy(dset_table->objs[i].objname, tmp);
                 }
             }
             dump_dataset (obj, name);
             H5Dclose (obj);
         } else {
             printf ("h5dump error: unable to dump dataset %s\n",name);
             status = 1;
         }

         break;

    case H5G_TYPE:
		if ((obj=H5Topen (group, name)) >= 0) {
			dump_named_datatype (obj, name);
			H5Tclose(obj);
		} else {
			printf ("h5dump error: unable to dump data type %s\n",name);
			status = 1;
		}
		break;

    default:
         printf ("h5dump error: unknown object %s\n", name);
         status = 1; 
         return FAIL;

    }

done:
    free(tmp);
    return SUCCEED;

}


/*-------------------------------------------------------------------------
 * Function:    dump_named_datatype
 *
 * Purpose:     Dump named data type
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: Comments: not yet implemented.
 *
 *-----------------------------------------------------------------------*/
static void
dump_named_datatype (hid_t type, const char *name) {
	int nmembers = 1, x,j;
	hid_t comptype;
	char *compname;
	int ndims, perm[H5DUMP_MAX_RANK];
size_t dims[H5DUMP_MAX_RANK];


    indentation (indent);
    begin_obj(dump_header_format->datatypebegin, name, dump_header_format->datatypeblockbegin);

	if (H5Tget_class(type) == H5T_COMPOUND){
		nmembers = H5Tget_nmembers(type);
		for (x = 0; x < nmembers; x++){
			comptype = H5Tget_member_type(type,x);
			compname = H5Tget_member_name(type,x);
			ndims = H5Tget_member_dims(type, x, dims, perm);
		    indentation (indent+COL);
			print_datatype(comptype);
			printf(" \"%s\"",compname);
			if (ndims != 1 || dims[0] != 1) {
				for (j = 0; j < ndims; j++) 
					printf("[%d]",(int)dims[j]);
			}
			printf(";\n");		   
		}
	}
	else {
		indentation (indent+COL);	
		print_datatype(type);
		printf(";\n");
	}
    indentation (indent);
    end_obj(dump_header_format->datatypeend, dump_header_format->datatypeblockend);
}

/*-------------------------------------------------------------------------
 * Function:    dump_group
 *
 * Purpose:     Dump everything within the specified group
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_group (hid_t gid, const char *name) {
H5G_stat_t statbuf;
hid_t dset, type;
char typename[1024], *tmp;
int i;

    tmp = (char *) malloc ((strlen(prefix)+strlen(name)+2) * sizeof(char));
    strcpy(tmp, prefix);


    indentation (indent);
    begin_obj(dump_header_format->groupbegin, name, dump_header_format->groupblockbegin);
    indent += COL;
	if (display_oid) {
		dump_oid(gid);
	}
    if (!strcmp(name,"/") && unamedtype) { /* dump unamed type in root group */
        for (i = 0; i < type_table->nobjs; i++)
             if (!type_table->objs[i].recorded) {
                 dset = H5Dopen (gid, type_table->objs[i].objname);
                 type = H5Dget_type (dset);
                 sprintf(typename,"#%lu:%lu", type_table->objs[i].objno[0],
                                              type_table->objs[i].objno[1]);
                 dump_named_datatype (type, typename);
                 H5Tclose(type);
                 H5Dclose(dset);
             }
    }

    H5Gget_objinfo(gid, ".", TRUE, &statbuf);
    if (statbuf.nlink > 1) { 

        i = search_obj (group_table, statbuf.objno);
        if (i < 0) {

            indentation (indent);
            printf("h5dump error: internal error\n");
            status = 1;

        } else if (group_table->objs[i].displayed) {

            indentation (indent);
            printf("%s \"%s\"\n",HARDLINK, group_table->objs[i].objname);

        } else {

            strcpy(group_table->objs[i].objname, prefix);
            group_table->objs[i].displayed = 1;
            H5Aiterate (gid, NULL, dump_attr, NULL);
            H5Giterate (gid, ".", NULL, dump_all, NULL);

        } 
           
    } else {

       H5Aiterate (gid, NULL, dump_attr, NULL);
       H5Giterate (gid, ".", NULL, dump_all, NULL);

    }

    indent -= COL;
    indentation (indent);
    end_obj(dump_header_format->groupend, dump_header_format->groupblockend);
    
    free(tmp);

}


/*-------------------------------------------------------------------------
 * Function:    dump_dataset
 *
 * Purpose:     Dump the specified data set
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_dataset (hid_t did, const char *name) {
hid_t  type, space;

    indentation (indent);
    begin_obj(dump_header_format->datasetbegin, name, dump_header_format->datasetblockbegin);

    type = H5Dget_type (did);
    space = H5Dget_space (did);
    dump_datatype(type);
    dump_dataspace(space);
	if (display_oid){
		dump_oid(did);
	}

    if (display_data)
    switch (H5Tget_class(type)) {
    case H5T_INTEGER:
         dump_data(did, DATASET_DATA);
         break;
    case H5T_FLOAT:
         dump_data(did, DATASET_DATA);
         break;
    case H5T_TIME:
         indent += COL;
         indentation (indent);
         indent -= COL;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_STRING:
         dump_data(did, DATASET_DATA);
         break;
    case H5T_BITFIELD:
         indent += COL;
         indentation (indent);
         indent -= COL;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_OPAQUE:
         indent += COL;
         indentation (indent);
         indent -= COL;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_COMPOUND:
         dump_data(did, DATASET_DATA);
         break;
	case H5T_REFERENCE:
		 dump_data(did, DATASET_DATA);
		 break;
	case H5T_ENUM:
		 dump_data(did,DATASET_DATA);
		 break;

    default: break;
    }

    indent += COL;
    H5Aiterate (did, NULL, dump_attr, NULL);
    indent -= COL;

    H5Tclose(type);
    H5Sclose(space);

    indentation (indent);
    end_obj(dump_header_format->datasetend, dump_header_format->datasetblockend);
    
}



#if H5DUMP_DEBUG
/*-------------------------------------------------------------------------
 * Function:    dump_tables
 *
 * Purpose:     display the contents of tables for debugging purposes
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/

static void 
dump_tables(void) {
int i;

    printf("group_table: # of entries = %d\n", group_table->nobjs);
    for ( i = 0; i < group_table->nobjs; i++)
        printf ("%ul %ul %s %d %d\n", group_table->objs[i].objno[0],
                                   group_table->objs[i].objno[1],
                                   group_table->objs[i].objname,
                                   group_table->objs[i].displayed,
                                   group_table->objs[i].recorded);

    printf("\ndset_table: # of entries = %d\n", dset_table->nobjs);
    for ( i = 0; i < dset_table->nobjs; i++)
        printf ("%ul %ul %s %d %d\n", dset_table->objs[i].objno[0],
                                   dset_table->objs[i].objno[1],
                                   dset_table->objs[i].objname,
                                   dset_table->objs[i].displayed,
                                   dset_table->objs[i].recorded);
   
    printf("\ntype_table: # of entries = %d\n", type_table->nobjs);
    for ( i = 0; i < type_table->nobjs; i++)
        printf ("%ul %ul %s %d %d\n", type_table->objs[i].objno[0],
                                   type_table->objs[i].objno[1],
                                   type_table->objs[i].objname,
                                   type_table->objs[i].displayed,
                                   type_table->objs[i].recorded);
}

#endif



/*-------------------------------------------------------------------------
 * Function:    dump_data
 *
 * Purpose:     Dump attribute or dataset data
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------
 */
static void
dump_data (hid_t obj_id, int obj_data) {
	h5dump_t *outputformat = &dataformat;
	int status = -1;
	void *buf;
	char *attr_name = malloc(sizeof(char)*80);
	hid_t space, type, p_type;
	int ndims, i;
	hsize_t size[64], nelmts = 1;
	int depth;
	int stdindent = COL; /* should be 3*/


	outputformat->line_ncols = nCols;

    indent += COL;
	/*the depth will tell us how far we need to indent extra.  we use to just
	use indent but with the merging of the tools lib we have to do something different
	for the lib funtions... the normal indentation is 6 so when we dont need any extra
	indentation, depth will be 0.*/
	depth = indent/stdindent - 2;

    indentation (indent);
  /*  printf("%s %s\n", dump_header_format->databegin, BEGIN);*/
	begin_obj(dump_header_format->databegin, NULL, dump_header_format->datablockbegin);

    /* Print all the values. */
	if (obj_data == DATASET_DATA){
		status = h5dump_dset(stdout, outputformat, obj_id, -1,depth);
	}
	else { /* need to call h5dump_mem for the attribute data */	

		type = H5Aget_type(obj_id);
	/*	if (type < 0) {
			return (status);
		}*/
		p_type = h5dump_fixtype(type);
		H5Tclose(type);

	/*	if (p_type < 0) return status;*/

		space = H5Aget_space(obj_id);

/*		if (space < 0) return status;
*/


		ndims = H5Sget_simple_extent_dims(space, size, NULL);
	    for (i=0; i<ndims; i++) {
			nelmts *= size[i];
	    }


		buf = malloc(nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type)));
	   	assert(buf);
	    if (H5Aread(obj_id, p_type, buf)>=0) {
			status = h5dump_mem(stdout, outputformat, p_type, space, buf, depth);
	    }
	    free(buf);
	    H5Tclose(p_type); 
		H5Sclose(space);
		H5Tclose(type);
	}

	
    if (status < 0) {
        indentation(indent+COL);
        printf("Unable to print data.\n");
        status = 1;
    }
    indentation(indent);
  /*  printf("%s\n", END);*/
	end_obj(dump_header_format->dataend, dump_header_format->datablockend);
    indent -= COL;
}




/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 dumper
 *
 * Return:      Success:	0
 *              Failure:	1
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
    hid_t fid, gid, dsetid, typeid;
    hid_t plist=H5P_DEFAULT;
    const char *fname = NULL;
    int i, index, curr_arg, display_bb=0, display_all=1, newwidth= 0;
    int nopts=0, *opts;
    char *buf, name[128], name1[128];
    H5G_stat_t statbuf;
    void *edata;
    hid_t (*func)(void*);
	find_objs_t *info = malloc(sizeof(find_objs_t));

	dump_header_format = &standardformat;

    /* Disable error reporting */
    H5Eget_auto (&func, &edata);
    H5Eset_auto (NULL, NULL);

    if (argc < 2 ) {
        usage();
        exit(1);
    }

    opts = malloc((argc/2) * sizeof (int));
    opts[0] = -1;
    /* parse command line options */
    for (curr_arg = 1; curr_arg < argc; curr_arg++) 

         if (argv[curr_arg][0] == '-') {

             opts[nopts++] = curr_arg;

             if (!strcmp(argv[curr_arg],"-h")) {
                 usage();
                 free(opts);
                 exit(0);
             }
	     else if (!strcmp(argv[curr_arg],"-V")){
		print_version("h5dump");
                free(opts);
                exit(0);
	    }

	     else if (!strcmp(argv[curr_arg],"-bb"))

                 display_bb = 1;

             else if (!strcmp(argv[curr_arg],"-header")) 

                 display_data=0;

	     else if (!strcmp(argv[curr_arg],"-v"))

		     display_oid = 1;

	     else if (!strcmp(argv[curr_arg],"-w")){
		     /*
		     this way we know which arg was the -w
		     we know it won't be 0 since curr_arg starts at 1
		     */
		     newwidth = curr_arg;
	     }
		 else if (!strcmp(argv[curr_arg], "-xml")){
			dump_header_format = &xmlformat;
		 }
             else if (strcmp(argv[curr_arg],"-a") &&
                      strcmp(argv[curr_arg],"-d") &&
                      strcmp(argv[curr_arg],"-g") &&
                      strcmp(argv[curr_arg],"-l") &&
                      strcmp(argv[curr_arg],"-t")) {

                 fprintf(stderr, "h5dump error: illegal option %s \n", 
                         argv[curr_arg]);
                 usage();
                 free(opts);
                 exit(1);
             } else display_all = 0;
         } 

    /* check names */
    if (argc == 2) {
        if (opts[0] == 1) { /* argv[1] is an option */
            fprintf(stderr, "h5dump error: no <names> or no <file>\n");
            usage();
            free(opts);
            exit(1);
        }
    } else {
        for (i = 0; i < nopts-1; i++) {
             if (opts[i+1]-opts[i] == 1) {
                if (strcmp(argv[opts[i]], "-bb") &&
                    strcmp(argv[opts[i]], "-header") &&
					strcmp(argv[opts[i]], "-xml") &&
					strcmp(argv[opts[i]], "-v")) {
                    fprintf(stderr,"h5dump error: no <names> after option %s\n",
                            argv[opts[i]]);
                    usage();
                    free(opts);
                    exit(1);
                } 
             }
        }
        if (argc - opts[nopts-1] == 1) {
            fprintf(stderr,"h5dump error: no <file>\n");
            usage();
            free(opts);
            exit(1);
        } 
        if (argc - opts[nopts-1] == 2) {
            if (strcmp(argv[opts[i]], "-bb") &&
                strcmp(argv[opts[i]], "-header") &&
				strcmp(argv[opts[i]], "-xml") &&
                strcmp(argv[opts[i]], "-v")) {
                fprintf (stderr, "h5dump error: no <file> or no <names> or no <number> after option %s\n", argv[opts[i]]);
                usage();
                free(opts);
                exit(1);
            }
        }
    } 


    if (argv[argc-1][0] == '\\') fname = &argv[argc-1][1];
    else fname = argv[argc-1];

    if ((fid = H5Fopen (fname, H5F_ACC_RDONLY, plist)) < 0) {
         fprintf (stderr, "h5dump error: unable to open file %s \n", fname);
         free(opts);
         exit(1);
    }

    /* allocate and initialize internal data structure */
    init_table(&group_table);
    init_table(&type_table);
    init_table(&dset_table);
	init_prefix(&prefix, prefix_len);

	/*init the find_objs_t*/
	info->threshold = 0;
	info->prefix_len = 1024;
	info->prefix = malloc(sizeof(char)*info->prefix_len);
	*(info->prefix) = '\0';
	info->group_table = group_table;
	info->type_table = type_table;
	info->dset_table = dset_table;
	info->status = status;



    /* find all shared objects */
    H5Giterate (fid, "/", NULL, find_objs, (void*)info);
    strcpy(prefix, "");

    /* does there exist unamed committed data type */
    for ( i = 0; i < type_table->nobjs; i++)
          if (type_table->objs[i].recorded == 0) unamedtype = 1;


    #ifdef H5DUMP_DEBUG
        dump_tables();
    #endif


    if (info->status) {
        printf("internal error! \n");
        goto done;
    }

    /* start to dump */
    begin_obj(dump_header_format->filebegin, fname, dump_header_format->fileblockbegin);

	
    if (display_bb) dump_bb();
	
	if (newwidth) {
		sscanf(argv[newwidth + 1], "%d", &nCols);
	}

    if (display_all) {

        if ((gid = H5Gopen (fid, "/")) < 0 ) {
             fprintf(stdout, "h5dump error: unable to open root group\n");
             status = 1;
        } else 
             dump_group(gid, "/");

        if (H5Gclose (gid) < 0) {
            fprintf(stdout, "h5dump error: unable to close root group\n");
            status = 1;
        }

    } else

      for (i = 0; i < nopts; i++) {
           if (!strcmp(argv[opts[i]],"-a")) { 

               for (curr_arg = opts[i]+1; 
                    curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                    curr_arg++) 
                    dump_selected_attr (fid, argv[curr_arg]);

           } else if (!strcmp(argv[opts[i]],"-d")) {
                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {

                     if ((dsetid = H5Dopen (fid, argv[curr_arg]))<0) {
                         begin_obj (dump_header_format->datasetbegin, argv[curr_arg], dump_header_format->datasetblockbegin); 
                         indentation (COL);
                         fprintf (stdout, "h5dump error: unable to open %s\n", 
                                  argv[curr_arg]);
                         end_obj(dump_header_format->datasetend, dump_header_format->datasetblockend);
                         status = 1;
                     } else {
                         H5Gget_objinfo(dsetid, ".", TRUE, &statbuf);
                         if (statbuf.nlink > 1) {
                             index = search_obj (dset_table, statbuf.objno);
                             if (index >= 0) {
                                 if (dset_table->objs[index].displayed) {
                                     begin_obj(dump_header_format->datasetbegin, argv[curr_arg], dump_header_format->datasetblockbegin);
                                     indentation (indent+COL);
                                     printf("%s \"%s\"\n", HARDLINK,
                                                           dset_table->objs[index].objname);
                                     indentation (indent);
                                     end_obj(dump_header_format->datasetend, dump_header_format->datasetblockend);
                                 } else {
                                     strcpy(dset_table->objs[index].objname, argv[curr_arg]);
                                     dset_table->objs[index].displayed = 1;
                                     dump_dataset(dsetid, argv[curr_arg]);
                                 }
                             } else status = 1;
                         } else
                             dump_dataset(dsetid, argv[curr_arg]);
                         if (H5Dclose(dsetid)<1) status = 1;
                     }
          
                }



           } else if (!strcmp(argv[opts[i]],"-g")) {

                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {
                     if ((gid = H5Gopen (fid, argv[curr_arg])) < 0) {
                         begin_obj (dump_header_format->groupbegin, argv[curr_arg], dump_header_format->groupblockbegin); 
                         indentation (COL);
                         fprintf (stdout, "h5dump error: unable to open %s\n", 
                                  argv[curr_arg]);
                         end_obj(dump_header_format->groupend, dump_header_format->groupblockend);
                         status = 1;
                     } else {
                         H5Gget_objinfo(gid, ".", TRUE, &statbuf);
                         strcpy(prefix, argv[curr_arg]);
                         dump_group(gid, argv[curr_arg]);
                         if (H5Gclose (gid) < 0) status = 1;
                     }
                }

           } else if (!strcmp(argv[opts[i]],"-l")) {

                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {


                     if (H5Gget_objinfo(fid, argv[curr_arg], FALSE, &statbuf) < 0) {
                         begin_obj(dump_header_format->softlinkbegin, argv[curr_arg], dump_header_format->softlinkblockbegin);
                         indentation (COL);
                         fprintf(stdout, "h5dump error: unable to get obj info from %s\n", argv[curr_arg]);
                         end_obj(dump_header_format->softlinkend, dump_header_format->softlinkblockend);
                         status = 1;

                     } else if (statbuf.type == H5G_LINK) {

                         buf = malloc(statbuf.linklen*sizeof(char));
                         begin_obj(dump_header_format->softlinkbegin, argv[curr_arg], dump_header_format->softlinkblockbegin);
                         indentation (COL);
                         if (H5Gget_linkval (fid, argv[curr_arg], statbuf.linklen, buf)>=0) 
                             printf ("LINKTARGET \"%s\"\n", buf);
                         else {
                             fprintf (stdout, "h5dump error: unable to get link value\n");
                             status = 1;
                         }
                         end_obj(dump_header_format->softlinkend, dump_header_format->softlinkblockend);
                         free(buf);

                     } else {
                         begin_obj(dump_header_format->softlinkbegin, argv[curr_arg], dump_header_format->softlinkblockbegin);
                         indentation (COL);
                         fprintf(stdout, "h5dump error: %s is not a link\n", argv[curr_arg]);
                         end_obj(dump_header_format->softlinkend, dump_header_format->softlinkblockend);
                         status = 1;
                     }

                 }

           } else if (!strcmp(argv[opts[i]],"-t")) {


                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {

                     if ((typeid=H5Topen (fid, argv[curr_arg])) < 0) {

                         /* check if argv[curr_arg] is unamed data type */
                         index = 0;
                         while (index < type_table->nobjs ) {
                             if (!type_table->objs[index].recorded) { /* unamed data type */
                                 sprintf(name,"#%lu:%lu\n", type_table->objs[index].objno[0], 
                                                            type_table->objs[index].objno[1]);
                                 sprintf(name1,"/#%lu:%lu\n", type_table->objs[index].objno[0], 
                                                            type_table->objs[index].objno[1]);
                                 if (!strncmp(name, argv[curr_arg], strlen(argv[curr_arg])) || 
                                     !strncmp(name1, argv[curr_arg], strlen(argv[curr_arg]))) {
                                      break;
                                  }
                             } 
                             index++;
                         }
                         if (index ==  type_table->nobjs) {  /* unknown type */
                              begin_obj (dump_header_format->datatypebegin, argv[curr_arg], dump_header_format->datatypeblockbegin); 
                              indentation (COL);
                              fprintf (stdout, "h5dump error: unable to open %s\n", 
                                       argv[curr_arg]);
                              end_obj(dump_header_format->datatypeend, dump_header_format->datatypeblockend);
                              status = 1;
                         } else {
                              dsetid = H5Dopen (fid, type_table->objs[index].objname) ;
                              typeid = H5Dget_type (dsetid);
                              dump_named_datatype (typeid, argv[curr_arg]);
                              H5Tclose(typeid);
                              H5Dclose(dsetid);
                         }

                     } else {
                         dump_named_datatype (typeid, argv[curr_arg]);
                         if (H5Tclose(typeid) < 0) status = 1;
                     }
                }
           } 
      }

    end_obj(dump_header_format->fileend, dump_header_format->fileblockend);

done:

    H5Eset_auto (func, edata);
    free(opts);
    if (H5Fclose (fid) < 0) status = 1;

    free (group_table->objs);
    free (dset_table->objs);
    free (type_table->objs);
    free (prefix);
	free (info->prefix);
	free (info);
    return status;

}


/*-------------------------------------------------------------------------
 * Function:    print_enum
 *
 * Purpose:     prints the enum data - 
 *
 * Return:      void
 *
 * Programmer:  Patrick Lu
 *
 * Modifications:
 *
 * NOTE: this function was taken from h5ls. should be moved into the toolslib
 * 
 *-----------------------------------------------------------------------*/
static void print_enum(hid_t type){
    char	**name=NULL;	/*member names				*/
    unsigned char *value=NULL;	/*value array				*/
    int		nmembs;		/*number of members			*/
    int		nchars;		/*number of output characters		*/
    hid_t	super;		/*enum base integer type		*/
    hid_t	native=-1;	/*native integer data type		*/
    size_t	dst_size;	/*destination value type size		*/
    int		i;		/*miscellaneous counters		*/
    size_t	j;	
 
	nmembs =  H5Tget_nmembers(type);
    super = H5Tget_super(type);
	/*
     * Determine what data type to use for the native values.  To simplify
     * things we entertain three possibilities:
     *  1. long_long -- the largest native signed integer
     *	2. unsigned long_long -- the largest native unsigned integer
     *	3. raw format
     */
    if (H5Tget_size(type)<=sizeof(long_long)) {
	dst_size = sizeof(long_long);
	if (H5T_SGN_NONE==H5Tget_sign(type)) {
	    native = H5T_NATIVE_ULLONG;
	} else {
	    native = H5T_NATIVE_LLONG;
	}
    } else {
	dst_size = H5Tget_size(type);
    }

    /* Get the names and raw values of all members */
    name = calloc(nmembs, sizeof(char*));
    value = calloc(nmembs, MAX(H5Tget_size(type), dst_size));
    for (i=0; i<nmembs; i++) {
	name[i] = H5Tget_member_name(type, i);
	H5Tget_member_value(type, i, value+i*H5Tget_size(type));
    }

    /* Convert values to native data type */
    if (native>0) H5Tconvert(super, native, nmembs, value, NULL, H5P_DEFAULT);

    /* Sort members by increasing value */
    /*not implemented yet*/

    /* Print members */
    for (i=0; i<nmembs; i++) {
	printf("\n%*s", indent+4, "");
	nchars = printf("\"%s\"",name[i]);/*display_string(stdout, name[i], TRUE);*/
	printf("%*s   ", MAX(0, 16-nchars), "");

	if (native<0) {
	    printf("0x");
	    for (j=0; j<dst_size; j++) {
		printf("%02x", value[i*dst_size+j]);
	    }
	} else if (H5T_SGN_NONE==H5Tget_sign(native)) {
	    printf("%"PRINTF_LL_WIDTH"u",
		   *((unsigned long_long*)((void*)(value+i*dst_size))));
	} else {
	    printf("%"PRINTF_LL_WIDTH"d",
		   *((long_long*)((void*)(value+i*dst_size))));
	}
	printf(";");
    }

    /* Release resources */
    for (i=0; i<nmembs; i++) free(name[i]);
    free(name);
    free(value);
    H5Tclose(super);

    if (0==nmembs) printf("\n%*s <empty>", indent+4, "");
   /* printf("\n%*s}", indent, "");*/
   /* return TRUE;*/
}

/*-------------------------------------------------------------------------
 * Function:    dump_oid
 *
 * Purpose:     Prints the object ids
 *
 * Return:      void
 *
 * Programmer:  Patrick Lu
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void 
dump_oid(hid_t oid){	
	

    indent += COL;
    indentation (indent);

    printf ("%s %s ", OBJID, BEGIN);
	printf("%d", oid);
    printf (" %s\n", END);  
	indent -= COL;
}
