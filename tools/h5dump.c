#include "h5dump.h"
#include <stdio.h>
#include "H5private.h"

static int indent = 0;
static int display_data = 1;
static int status = 0;

static void dump_group (hid_t , const char* );
static void dump_dataset (hid_t, const  char*);
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
fprintf(stderr, 
"\nUsage of HDF5 Dumper:\n\n  \
h5dump [-h] [-bb] [-header] [-a <names>] [-d <names>] [-g <names>]\n  \
       [-l <names>] <file>\n\n\
  -h            Print information on this command.\n\
  -bb           Display the conent of boot block. The default is not to display.\n\
  -header       Display header only; that is, no data displayed.\n\
  -a <names>    Display the specified attribute(s).\n\
  -d <names>    Display the specified dataset(s).\n\
  -g <names>    Display all the objects within the specified group(s).\n\
  -l <names>    Display the specified link value(s).\n\n"); 

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
 * Purpose:     Prints the name of data type. 
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
datatype_name(hid_t type) {

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
        else if (H5Tequal(type, H5T_NATIVE_CHAR)) 
            printf( "H5T_NATIVE_CHAR");
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
        else printf( "unknown integer");
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
        else printf( "unknown float");
        break;

    case H5T_TIME: 
        printf( "time: not yet implemented");

    case H5T_STRING: 

        if (H5Tequal(type,H5T_C_S1))
            printf( "H5T_C_S1");
        else if (H5Tequal(type,H5T_FORTRAN_S1))
            printf( "H5T_FORTRAN_S1");
        else printf( "unknown string");
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
        else printf( "unknown bitfield");
        break;

    case H5T_OPAQUE: 
        printf( "opaque: not yet implemented");
        break;

    case H5T_COMPOUND: 

        printf( "compound: not yet implemented");
        break;

    default:
        printf( "unknown data type");
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

    indentation (indent+col);
    printf ("%s %s \"", DATATYPE, BEGIN);
    datatype_name(type);
    printf ("\" %s\n", END);
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

    indentation (indent+col);

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
        if (display_data) dump_data(attr_id, ATTRIBUTE_DATA);
        H5Tclose(type);
        H5Sclose(space);
	H5Aclose (attr_id);
        indentation (indent);
        end_obj();

    } else {
        indentation (indent+col);
        printf("h5dump error: unable to open attribute.\n");
        indentation (indent);
        end_obj();
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



    j = strlen(name)-1;
    obj_name = malloc ((j+2) * sizeof(char));
    /* find the last / */
    while (name[j] != '/' && j >=0) j--;
    /* object name */
    if (j == -1) strcpy(obj_name, "/");
    else strncpy(obj_name, name, j+1);
 
    attr_name = name+j+1;

    begin_obj (ATTRIBUTE, name);

    H5Gget_objinfo(loc_id, obj_name, FALSE, &statbuf);
    switch (statbuf.type) {
    case H5G_GROUP:
         if ((oid = H5Gopen (loc_id, obj_name))<0) {
             indentation (col);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj();
             status = 1;
             return FAIL;
         }
         break;
    case H5G_DATASET:
         if ((oid = H5Dopen (loc_id, obj_name))<0) {
             indentation (col);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj();
             status = 1;
             return FAIL;
         }
         break;
    case H5G_TYPE:
         if ((oid =  H5Topen(loc_id, obj_name)) < 0 ) {
             indentation (col);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj();
             status = 1;
             return FAIL;
         } 
         break;
    default:
         indentation (col);
         fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
         end_obj();
         status = 1;
         return FAIL;
    }

    if ((attr_id = H5Aopen_name (oid, attr_name))>= 0) {

        type = H5Aget_type(attr_id);
        space = H5Aget_space(attr_id);
        dump_datatype(type);
        dump_dataspace(space);
        if (display_data) dump_data(attr_id, ATTRIBUTE_DATA);
        H5Tclose(type);
        H5Sclose(space);
	H5Aclose (attr_id);
        end_obj();

    } else {
        indentation (col);
        printf("h5dump error: unable to open attribute.\n");
        end_obj();
        status = 1;
        return FAIL;
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
dump_all (hid_t group, const char *name, void __unused__ *op_data)
{
    hid_t obj;
    char *buf;
    H5G_stat_t statbuf;



    H5Gget_objinfo(group, name, FALSE, &statbuf);


    switch (statbuf.type) {
    case H5G_LINK:
        indentation (indent);

        buf = malloc (statbuf.linklen*sizeof(char));

        begin_obj(SOFTLINK, name);
        indentation (indent+col);
        if (H5Gget_linkval (group, name, statbuf.linklen, buf)>=0) 
            printf ("linktarget \"%s\"\n", buf);
        else {
            printf ("h5dump error: unable to get link value.\n");
            status = 1;
        }

        indentation (indent);
        end_obj();
        free (buf);
        break;

    case H5G_GROUP:
         if ((obj=H5Gopen (group, name))>=0) {
             dump_group (obj, name);
             H5Gclose (obj);
         } else {
             printf ("h5dump error: unable to dump group %s\n",name);
             status = 1;
         }

         break;

    case H5G_DATASET:

         if ((obj=H5Dopen (group, name))>=0) {              
             dump_dataset (obj, name);
             H5Dclose (obj);
         } else {
             printf ("h5dump error: unable to dump dataset %s\n",name);
             status = 1;
         }

         break;

    case H5G_TYPE:
         dump_named_datatype (obj, name);
         H5Tclose(obj);
         break;

    default:
         printf ("Unknown Object %s\n", name);
         status = 1; 
         return FAIL;
         break;

    }



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

    indentation(indent+col);
    printf("named data type: not yet implemented.\n");

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
H5G_stat_t statbuf;

    indentation (indent);
    begin_obj(GROUP, name);
    indent += col;

    /* hard link */
    H5Gget_objinfo(gid, ".", TRUE, &statbuf);
    if (statbuf.nlink > 1) { 
        indentation (indent);
        printf("%s %s (%s %u) (%s %lu %lu) (%s %lu %lu ) %s\n", 
               HARDLINK, BEGIN, NLINK, statbuf.nlink, FILENO, statbuf.fileno[0],
               statbuf.fileno[1], OBJNO, statbuf.objno[0], statbuf.objno[1], END); 
    }

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
H5G_stat_t statbuf;

    indentation (indent);
    begin_obj(DATASET, name);

    /* hard link */
    H5Gget_objinfo(did, ".", TRUE, &statbuf);
    if (statbuf.nlink > 1) {
        indentation (indent+col);
        printf("%s %s (%s %u) (%s %lu %lu) (%s %lu %lu ) %s\n",
               HARDLINK, BEGIN, NLINK, statbuf.nlink, FILENO, statbuf.fileno[0],
               statbuf.fileno[1], OBJNO, statbuf.objno[0], statbuf.objno[1], END);
    }

    type = H5Dget_type (did);
    space = H5Dget_space (did);
    dump_datatype(type);
    dump_dataspace(space);

    if (display_data)
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
 * Function:    
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
hid_t fid, gid, dsetid;
hid_t plist=H5P_DEFAULT;
const char *fname = NULL;
int i, curr_arg, display_bb=0, display_all=1;
int nopts=0, *opts;
char *buf;
H5G_stat_t statbuf;
void __unused__ *op_data;
    hid_t (*func)(void*);
    void *edata;
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
                 exit(0);

             } else if (!strcmp(argv[curr_arg],"-bb"))

                 display_bb = 1;

             else if (!strcmp(argv[curr_arg],"-header")) 

                 display_data=0;

             else if (strcmp(argv[curr_arg],"-a") &&
                      strcmp(argv[curr_arg],"-d") &&
                      strcmp(argv[curr_arg],"-g") &&
                      strcmp(argv[curr_arg],"-l")) {

                 fprintf(stderr, "h5dump error: illegal option %s \n", 
                         argv[curr_arg]);
                 usage();
                 exit(1);
             } else display_all = 0;
         } 

    /* check names */
    if (argc == 2) {
        if (opts[0] == 1) { /* argv[1] is an option */
            fprintf(stderr, "h5dump error: no <names> or no <file>\n");
            usage();
            exit(1);
        }
    } else {
        for (i = 0; i < nopts-1; i++) {
             if (opts[i+1]-opts[i] == 1) {
                if (strcmp(argv[opts[i]], "-bb") &&
                    strcmp(argv[opts[i]], "-header") ) {
                    fprintf(stderr,"h5dump error: no <names> after option %s\n",
                            argv[opts[i]]);
                    usage();
                    exit(1);
                } 
             }
        }
        if (argc - opts[nopts-1] == 1) {
            fprintf(stderr,"h5dump error: no <file>\n");
            usage();
            exit(1);
        } 
        if (argc - opts[nopts-1] == 2) {
            if (strcmp(argv[opts[i]], "-bb") &&
                strcmp(argv[opts[i]], "-header") ) {
                fprintf (stderr, "h5dump error: no <file> or no <names> after option %s\n", argv[opts[i]]);
                usage();
                exit(1);
            }
        }
    } 


    if (argv[argc-1][0] == '\\') fname = &argv[argc-1][1];
    else fname = argv[argc-1];

    if ((fid = H5Fopen (fname, H5F_ACC_RDONLY, plist)) < 0) {
         fprintf (stderr, "h5dump error: unable to open file %s \n", fname);
         exit(1);
    }

    begin_obj("HDF5", fname);

    if (display_bb) dump_bb();
 
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
                         begin_obj (DATASET, argv[curr_arg]); 
                         indentation (col);
                         fprintf (stdout, "h5dump error: unable to open %s\n", 
                                  argv[curr_arg]);
                         end_obj();
                         status = 1;
                     } else {
                         dump_dataset(dsetid, argv[curr_arg]);
                         if (H5Dclose(dsetid)<1) status = 1;
                     }
          
                }

           } else if (!strcmp(argv[opts[i]],"-g")) {

                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {
                     if ((gid = H5Gopen (fid, argv[curr_arg])) < 0) {
                         begin_obj (GROUP, argv[curr_arg]); 
                         indentation (col);
                         fprintf (stdout, "h5dump error: unable to open %s\n", 
                                  argv[curr_arg]);
                         end_obj();
                         status = 1;
                     } else {
                         dump_group(gid, argv[curr_arg]);
                         if (H5Gclose (gid) < 0) status = 1;
                     }
                }

           } else if (!strcmp(argv[opts[i]],"-l")) {

                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {


                     if (H5Gget_objinfo(fid, argv[curr_arg], FALSE, &statbuf) < 0) {
                         begin_obj(SOFTLINK, argv[curr_arg]);
                         indentation (col);
                         fprintf(stdout, "h5dump error: unable to get obj info from %s\n", argv[curr_arg]);
                         end_obj();
                         status = 1;

                     } else if (statbuf.type == H5G_LINK) {

                         buf = malloc(statbuf.linklen*sizeof(char));
                         begin_obj(SOFTLINK, argv[curr_arg]);
                         indentation (col);
                         if (H5Gget_linkval (fid, argv[curr_arg], statbuf.linklen, buf)>=0) 
                             printf ("linktarget \"%s\"\n", buf);
                         else {
                             fprintf (stdout, "h5dump error: unable to get link value\n");
                             status = 1;
                         }
                         end_obj();
                         free(buf);

                     } else {
                         begin_obj(SOFTLINK, argv[curr_arg]);
                         indentation (col);
                         fprintf(stdout, "h5dump error: %s is not a link\n", argv[curr_arg]);
                         end_obj();
                         status = 1;
                     }

                 }

           }
      }

    end_obj();

    free(opts);

    /* Restore error reporting */
    H5Eset_auto (func, edata);
    if (H5Fclose (fid) < 0) exit(1);

    return status;

}
