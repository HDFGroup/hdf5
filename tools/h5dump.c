#include <H5private.h>
#include <h5dump.h>

static int indent = 0;

static void dump_group (hid_t , const char* );
static void dump_dataset (hid_t, const  char* );
static void dump_data (hid_t, int);
static void dump_named_datatype (hid_t , const char *);


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

printf("Usage: h5dump <file>\n");

/*
     h5dump [-h] [-bb] [-header] [-a <names>] [-d <names>] [-g <names>] 
            [-l <names>] <files>

     -h               Print information on this command.

     -bb              Display the conent of boot block. The default is not to
                      display.

     -header          Display header only; that is, no data displayed.

     -a <names>       Display the specified attribute.

     -d <names>       Display the specified dataset.

     -g <names>       Display all the objects within the specified group.

     -l <names>       Display the specified link value.

*/
}


/*-------------------------------------------------------------------------
 * Function:    indentation
 *
 * Purpose:     Print spaces for indentation
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void indentation(int x) {

  while (x>0) {
         printf(" "); 
         x--;
  }

}


/*-------------------------------------------------------------------------
 * Function:    datatype_name
 *
 * Purpose:     Returns the name of data type. 
 *
 * Return:      Returns a pointer to a string. 
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static const char*
datatype_name(hid_t type) {

    switch (H5Tget_class(type)) {

    case H5T_INTEGER:

        if (H5Tequal(type, H5T_NATIVE_CHAR)) 
            return "H5T_NATIVE_CHAR";
        else if (H5Tequal(type, H5T_NATIVE_UCHAR))
            return "H5T_NATIVE_UCHAR";
        else if (H5Tequal(type, H5T_NATIVE_SHORT))
            return "H5T_NATIVE_SHORT";
        else if (H5Tequal(type, H5T_NATIVE_USHORT))
            return "H5T_NATIVE_USHORT";
        else if (H5Tequal(type, H5T_NATIVE_INT))
            return "H5T_NATIVE_INT";
        else if (H5Tequal(type, H5T_NATIVE_UINT))
            return "H5T_NATIVE_UINT";
        else if (H5Tequal(type, H5T_NATIVE_LONG))
            return "H5T_NATIVE_LONG";
        else if (H5Tequal(type, H5T_NATIVE_ULONG))
            return "H5T_NATIVE_ULONG";
        else if (H5Tequal(type, H5T_NATIVE_LLONG))
            return "H5T_NATIVE_LLONG";
        else if (H5Tequal(type, H5T_NATIVE_ULLONG))
            return "H5T_NATIVE_ULLONG";
        else if (H5Tequal(type, H5T_STD_I8BE))
            return "H5T_STD_I8BE";
        else if (H5Tequal(type, H5T_STD_I8LE))
            return "H5T_STD_I8LE";
        else if (H5Tequal(type, H5T_STD_I16BE))
            return "H5T_STD_I16BE";
        else if (H5Tequal(type, H5T_STD_I16LE))
            return "H5T_STD_I16LE";
        else if (H5Tequal(type, H5T_STD_I32BE))
            return "H5T_STD_I32BE";
        else if (H5Tequal(type, H5T_STD_I32LE))
            return "H5T_STD_I32LE";
        else if (H5Tequal(type, H5T_STD_I64BE))
            return "H5T_STD_I64BE";
        else if (H5Tequal(type, H5T_STD_I64LE))
            return "H5T_STD_I64LE";
        else if (H5Tequal(type, H5T_STD_U8BE))
            return "H5T_STD_U8BE";
        else if (H5Tequal(type, H5T_STD_U8LE))
            return "H5T_STD_U8LE";
        else if (H5Tequal(type, H5T_STD_U16BE))
            return "H5T_STD_U16BE";
        else if (H5Tequal(type, H5T_STD_U16LE))
            return "H5T_STD_U16LE";
        else if (H5Tequal(type, H5T_STD_U32BE))
            return "H5T_STD_U32BE";
        else if (H5Tequal(type, H5T_STD_U32LE))
            return "H5T_STD_U32LE";
        else if (H5Tequal(type, H5T_STD_U64BE))
            return "H5T_STD_U64BE";
        else if (H5Tequal(type, H5T_STD_U64LE))
            return "H5T_STD_U64LE";
        else return "unknown integer";

    case H5T_FLOAT: 

        if (H5Tequal(type, H5T_NATIVE_FLOAT))
            return "H5T_NATIVE_FLOAT";
        else if (H5Tequal(type, H5T_NATIVE_DOUBLE))
            return "H5T_NATIVE_DOUBLE";
        else if (H5Tequal(type, H5T_NATIVE_LDOUBLE))
            return "H5T_NATIVE_LDOUBLE";
        else if (H5Tequal(type, H5T_IEEE_F32BE))
            return "H5T_IEEE_F32BE";
        else if (H5Tequal(type, H5T_IEEE_F32LE))
            return "H5T_IEEE_F32LE";
        else if (H5Tequal(type, H5T_IEEE_F64BE))
            return "H5T_IEEE_F64BE";
        else if (H5Tequal(type, H5T_IEEE_F64LE))
            return "H5T_IEEE_F64LE";
        else return "unknown float";

    case H5T_TIME: return "time: not yet implemented";

    case H5T_STRING: 

        if (H5Tequal(type,H5T_C_S1))
            return "H5T_C_S1";
        else if (H5Tequal(type,H5T_FORTRAN_S1))
            return "H5T_FORTRAN_S1";
        else return "unknown string";

    case H5T_BITFIELD: 

        if (H5Tequal(type, H5T_STD_B8BE))
            return "H5T_STD_B8BE";
        else if (H5Tequal(type, H5T_STD_B8LE))
            return "H5T_STD_B8LE";
        else if (H5Tequal(type, H5T_STD_B16BE))
            return "H5T_STD_B16BE";
        else if (H5Tequal(type, H5T_STD_B16LE))
            return "H5T_STD_B16LE";
        else if (H5Tequal(type, H5T_STD_B32BE))
            return "H5T_STD_B32BE";
        else if (H5Tequal(type, H5T_STD_B32LE))
            return "H5T_STD_B32LE";
        else if (H5Tequal(type, H5T_STD_B64BE))
            return "H5T_STD_B64BE";
        else if (H5Tequal(type, H5T_STD_B64LE))
            return "H5T_STD_B64LE";
        else return "unknown bitfield";

    case H5T_OPAQUE: return "opaque: not yet implemented";

    case H5T_COMPOUND: 

         return "compound: not yet implemented";

    default:
            return "unknown data type";
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
/*
static void
dump_bb() {

    printf ("%s %s %s\n", BOOT_BLOCK, BEGIN, END);

}
*/


/*-------------------------------------------------------------------------
 * Function:    dump_datatype
 *
 * Purpose:     Dump the data type. Data type can be HDF5 predefined
 *              atomic data type, named data type or compound data type. 
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
const char *pt;

    indent += col;
    indentation (indent);
    pt = datatype_name(type);
    printf ("%s %s \"%s\" %s\n", DATATYPE, BEGIN, pt, END);
    indent -= col;
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
    hsize_t size[64];
    hsize_t maxsize[64];  /* check max dims size */
    int ndims = H5Sget_simple_extent_dims(space, size, maxsize);
    int i;

    indent += col;

    indentation (indent);

    printf("%s ", DATASPACE);

    if (H5Sis_simple(space)) {

        HDfprintf (stdout, "%s %s ( %Hu",BEGIN, ARRAY, size[0]);
        for (i = 1; i < ndims; i++) 
             HDfprintf (stdout, ", %Hu", size[i]);
        printf(" ) ");
           
        if (maxsize[0]==H5S_UNLIMITED)
            HDfprintf (stdout, "( %s", "H5S_UNLIMITED");
        else
            HDfprintf (stdout, "( %Hu", maxsize[0]);

        for (i = 1; i < ndims; i++) 
             if (maxsize[i]==H5S_UNLIMITED)
                 HDfprintf (stdout, ", %s", "H5S_UNLIMITED");
             else
                 HDfprintf (stdout, ", %Hu", maxsize[i]);

        printf(" ) %s\n", END);

    } else

        printf("%s not yet implemented %s\n", BEGIN, END);

    indent -= col;

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
dump_attr (hid_t attr, const char *attr_name, void __unused__ *op_data)
{
hid_t  attr_id, type, space;

    indentation(indent);
    begin_obj (ATTRIBUTE, attr_name); 

    if ((attr_id = H5Aopen_name (attr, attr_name))>= 0) {

        type = H5Aget_type(attr_id);
        space = H5Aget_space(attr_id);
        dump_datatype(type);
        dump_dataspace(space);
        dump_data(attr_id, ATTRIBUTE_DATA);
        H5Tclose(type);
        H5Sclose(space);
	H5Aclose (attr_id);

    } else {
        indent += col;
        indentation (indent);
        printf("Unable to open attribute %s.\n", attr_name);
        indent -= col;
        return FAIL;
    }

    indentation (indent);
    end_obj();

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
dump_all (hid_t group, const char *name, void __unused__ *op_data)
{
    hid_t obj;
    hid_t (*func)(void*);
    void *edata;
    char *buf;
    H5G_stat_t statbuf;

    /* Disable error reporting */
    H5Eget_auto (&func, &edata);
    H5Eset_auto (NULL, NULL);


    H5Gget_stat(group, name, FALSE, &statbuf);

    switch (statbuf.type) {
    case H5G_LINK:

        indentation (indent);

        buf = malloc (statbuf.linklen*sizeof(char));

        begin_obj(SOFTLINK, name);
        indent += col;
        indentation (indent);
        if (H5Gget_linkval (group, name, statbuf.linklen, buf)>=0) 
            printf ("linktarget \"%s\"\n", buf);
        else
            printf ("unable to get link value.\n");

        indent -= col;
        indentation (indent);
        end_obj();
        free (buf);
        break;

    case H5G_GROUP:
         if ((obj=H5Gopen (group, name))>=0) {
             dump_group (obj, name);
             H5Gclose (obj);
         } else 
             printf ("unable to dump group %s\n",name);

         break;

    case H5G_DATASET:

         if ((obj=H5Dopen (group, name))>=0) {              
             dump_dataset (obj, name);
             H5Dclose (obj);
         } else 
             printf ("unable to dump dataset %s\n",name);

         break;

    case H5G_TYPE:
         dump_named_datatype (obj, name);
         H5Tclose(obj);
         break;

    default:
         printf ("Unknown Object %s\n", name);
         H5Eset_auto (func, edata);
         return FAIL;
         break;

    }


    /* Restore error reporting */
    H5Eset_auto (func, edata);

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
dump_named_datatype (hid_t type_id, const char *name) {
hid_t id;
/*
char *fname ;
hid_t nmembers, mtype;
int i, ndims, perm[512];
*/

id = type_id; /*doesn't like warning message */

    indentation (indent);
    begin_obj(DATATYPE, name);

    indent += col;
    indentation(indent);
    printf("named data type: not yet implemented.\n");
    indent -= col;

    indentation (indent);
    end_obj();

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

    indentation (indent);
    begin_obj(GROUP, name);
    indent += col;
    H5Aiterate (gid, NULL, dump_attr, NULL);
    H5Giterate (gid, ".", NULL, dump_all, NULL);
    indent -= col;
    indentation (indent);
    end_obj();

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
    begin_obj(DATASET, name);
    type = H5Dget_type (did);
    space = H5Dget_space (did);
    dump_datatype(type);
    dump_dataspace(space);

    switch (H5Tget_class(type)) {
    case H5T_INTEGER:
         dump_data(did, DATASET_DATA);
         break;
    case H5T_FLOAT:
         dump_data(did, DATASET_DATA);
         break;
    case H5T_TIME:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_STRING:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_BITFIELD:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_OPAQUE:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_COMPOUND:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    default: break;
    }

    indent += col;
    H5Aiterate (did, NULL, dump_attr, NULL);
    indent -= col;

    H5Tclose(type);
    H5Sclose(space);
    indentation (indent);
    end_obj();
    
}


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
 *-----------------------------------------------------------------------*/
static void
dump_data (hid_t obj_id, int obj_data) {

    hid_t               f_type ;
    size_t              size ;
    h5dump_t            info;

    indent += col;
    indentation (indent);
    printf("%s %s", DATA, BEGIN);

    if (obj_data == DATASET_DATA)
        f_type = H5Dget_type(obj_id);
    else
        f_type = H5Aget_type(obj_id);

    size = H5Tget_size(f_type);

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);
    info.idx_fmt = "        (%s) ";
    info.line_ncols = 80;

    /*
     * If the dataset is a 1-byte integer type then format it as an ASCI
     * character string instead of integers.
     */
    if (1==size && H5T_INTEGER==H5Tget_class(f_type)) {
        info.elmt_suf1 = "";
        info.elmt_suf2 = "";
        info.idx_fmt = "        (%s) \"";
        info.line_suf = "\"";
        printf("\"");
    }

   
    /*
     * Print all the values.
     */

    if (h5dump1(stdout, &info, obj_id, -1, obj_data)<0) {
        printf("Unable to print data.");
    }

    if (1==size && H5T_INTEGER==H5Tget_class(f_type)) 
        printf("\"");

    printf("%s\n", END);
    indent -= col;

    H5Tclose(f_type);

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
main(int argc, char *argv[]) {
hid_t fid, gid;
hid_t plist=H5P_DEFAULT;
const char *fname = NULL;

    if (argc > 2 || argc <= 1) {
        usage();
        exit(1);
    }

    /* arguments */
    fname = argv[1];

    /* open file */
    if ((fid = H5Fopen (fname, H5F_ACC_RDONLY, plist))<0) exit(1);
   
    begin_obj("HDF5", fname);

    gid = H5Gopen (fid, "/");

    dump_group(gid, "/");

    end_obj();
    
    if (H5Gclose (gid) < 0) exit(1);

    if (H5Fclose (fid) < 0) exit(1);
    

    return 0;

}
