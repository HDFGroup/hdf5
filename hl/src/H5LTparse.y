%{
#include<stdio.h>
#include<string.h>
#include<hdf5.h>

#define STACK_SIZE      16

/*structure for compound type information*/
struct cmpd_info {
    hid_t       id;             /*type ID*/
    hbool_t     is_field;       /*flag to lexer for compound member*/
    hbool_t     first_memb;     /*flag for first compound member*/
};
/*stack for nested compound type*/
struct cmpd_info cmpd_stack[STACK_SIZE] = { 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1,
                                    0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1,
                                    0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1,
                                    0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1 };
int csindex = -1;                /*pointer to the top of compound stack*/

/*structure for array type information*/
struct arr_info {
    hsize_t             dims[H5S_MAX_RANK];     /*size of each dimension, limited to 32 dimensions*/
    int                 ndims;                  /*number of dimensions*/
    hbool_t             is_dim;                 /*flag to lexer for dimension*/
};
/*stack for nested array type*/
struct arr_info arr_stack[STACK_SIZE];
int asindex = -1;               /*pointer to the top of array stack*/ 

hbool_t     is_str_size = 0;        /*flag to lexer for string size*/
hbool_t     is_str_pad = 0;         /*flag to lexer for string padding*/

hid_t   enum_id;                    /*type ID*/
hbool_t     is_enum = 0;            /*flag to lexer for enum type*/
hbool_t     is_enum_memb = 0;       /*flag to lexer for enum member*/
char*   enum_memb_symbol;           /*enum member symbol string*/

hbool_t is_opq_size = 0;               /*flag to lexer for opaque type size*/
hbool_t is_opq_tag = 0;                /*flag to lexer for opaque type tag*/

%}
%union {
    int   ival;         /*for integer token*/
    char  *sval;        /*for compound member name*/
}

%token <ival> H5T_STD_I8BE_TOKEN H5T_STD_I8LE_TOKEN H5T_STD_I16BE_TOKEN  H5T_STD_I16LE_TOKEN
%token <ival> H5T_STD_I32BE_TOKEN H5T_STD_I32LE_TOKEN H5T_STD_I64BE_TOKEN H5T_STD_I64LE_TOKEN
%token <ival> H5T_STD_U8BE_TOKEN H5T_STD_U8LE_TOKEN H5T_STD_U16BE_TOKEN  H5T_STD_U16LE_TOKEN
%token <ival> H5T_STD_U32BE_TOKEN H5T_STD_U32LE_TOKEN H5T_STD_U64BE_TOKEN H5T_STD_U64LE_TOKEN
%token <ival> H5T_NATIVE_CHAR_TOKEN H5T_NATIVE_SCHAR_TOKEN H5T_NATIVE_UCHAR_TOKEN 
%token <ival> H5T_NATIVE_SHORT_TOKEN H5T_NATIVE_USHORT_TOKEN H5T_NATIVE_INT_TOKEN H5T_NATIVE_UINT_TOKEN 
%token <ival> H5T_NATIVE_LONG_TOKEN H5T_NATIVE_ULONG_TOKEN H5T_NATIVE_LLONG_TOKEN H5T_NATIVE_ULLONG_TOKEN

%token <ival> H5T_IEEE_F32BE_TOKEN H5T_IEEE_F32LE_TOKEN H5T_IEEE_F64BE_TOKEN H5T_IEEE_F64LE_TOKEN
%token <ival> H5T_NATIVE_FLOAT_TOKEN H5T_NATIVE_DOUBLE_TOKEN H5T_NATIVE_LDOUBLE_TOKEN

%token <ival> H5T_STRING_TOKEN STRSIZE_TOKEN STRPAD_TOKEN CSET_TOKEN CTYPE_TOKEN H5T_VARIABLE_TOKEN
%token <ival> H5T_STR_NULLTERM_TOKEN H5T_STR_NULLPAD_TOKEN H5T_STR_SPACEPAD_TOKEN 
%token <ival> H5T_CSET_ASCII_TOKEN H5T_C_S1_TOKEN H5T_FORTRAN_S1_TOKEN

%token <ival> H5T_OPAQUE_TOKEN OPQ_SIZE_TOKEN OPQ_TAG_TOKEN

%token <ival> H5T_COMPOUND_TOKEN
%token <ival> H5T_ENUM_TOKEN
%token <ival> H5T_ARRAY_TOKEN
%token <ival> H5T_VLEN_TOKEN

%token <sval> STRING
%token <ival> NUMBER
%token <ival> '{' '}' '[' ']' '"' ';' 

%%
start   :       { memset(arr_stack, 0, STACK_SIZE*sizeof(struct arr_info)); /*initialize here?*/ }
        |       ddl_type  { return $<ival>$;}
        ;
ddl_type        :       atomic_type
                |       compound_type
                |       array_type
                |       vlen_type
                ;
atomic_type     :       integer_type
                |       fp_type
                |       string_type
                |       enum_type
                |       opaque_type
                ;

integer_type    :       H5T_STD_I8BE_TOKEN  { $<ival>$ = H5Tcopy(H5T_STD_I8BE); }
                |       H5T_STD_I8LE_TOKEN  { $<ival>$ = H5Tcopy(H5T_STD_I8LE); }
                |       H5T_STD_I16BE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_I16BE); }
                |       H5T_STD_I16LE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_I16LE); }
                |       H5T_STD_I32BE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_I32BE); }
                |       H5T_STD_I32LE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_I32LE); }
                |       H5T_STD_I64BE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_I64BE); }
                |       H5T_STD_I64LE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_I64LE); }
                |       H5T_STD_U8BE_TOKEN  { $<ival>$ = H5Tcopy(H5T_STD_U8BE); }
                |       H5T_STD_U8LE_TOKEN  { $<ival>$ = H5Tcopy(H5T_STD_U8LE); }
                |       H5T_STD_U16BE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_U16BE); }
                |       H5T_STD_U16LE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_U16LE); }
                |       H5T_STD_U32BE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_U32BE); }
                |       H5T_STD_U32LE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_U32LE); }
                |       H5T_STD_U64BE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_U64BE); }
                |       H5T_STD_U64LE_TOKEN { $<ival>$ = H5Tcopy(H5T_STD_U64LE); }
                |       H5T_NATIVE_CHAR_TOKEN   { $<ival>$ = H5Tcopy(H5T_NATIVE_CHAR); }
                |       H5T_NATIVE_SCHAR_TOKEN  { $<ival>$ = H5Tcopy(H5T_NATIVE_SCHAR); }
                |       H5T_NATIVE_UCHAR_TOKEN  { $<ival>$ = H5Tcopy(H5T_NATIVE_UCHAR); }
                |       H5T_NATIVE_SHORT_TOKEN  { $<ival>$ = H5Tcopy(H5T_NATIVE_SHORT); }
                |       H5T_NATIVE_USHORT_TOKEN { $<ival>$ = H5Tcopy(H5T_NATIVE_USHORT); }
                |       H5T_NATIVE_INT_TOKEN    { $<ival>$ = H5Tcopy(H5T_NATIVE_INT); }
                |       H5T_NATIVE_UINT_TOKEN   { $<ival>$ = H5Tcopy(H5T_NATIVE_UINT); }
                |       H5T_NATIVE_LONG_TOKEN   { $<ival>$ = H5Tcopy(H5T_NATIVE_LONG); }
                |       H5T_NATIVE_ULONG_TOKEN  { $<ival>$ = H5Tcopy(H5T_NATIVE_ULONG); }
                |       H5T_NATIVE_LLONG_TOKEN  { $<ival>$ = H5Tcopy(H5T_NATIVE_LLONG); }
                |       H5T_NATIVE_ULLONG_TOKEN { $<ival>$ = H5Tcopy(H5T_NATIVE_ULLONG); }
                ;

fp_type         :       H5T_IEEE_F32BE_TOKEN { $<ival>$ = H5Tcopy(H5T_IEEE_F32BE); }
                |       H5T_IEEE_F32LE_TOKEN { $<ival>$ = H5Tcopy(H5T_IEEE_F32LE); }
                |       H5T_IEEE_F64BE_TOKEN { $<ival>$ = H5Tcopy(H5T_IEEE_F64BE); }
                |       H5T_IEEE_F64LE_TOKEN { $<ival>$ = H5Tcopy(H5T_IEEE_F64LE); }
                |       H5T_NATIVE_FLOAT_TOKEN    { $<ival>$ = H5Tcopy(H5T_NATIVE_FLOAT); }
                |       H5T_NATIVE_DOUBLE_TOKEN   { $<ival>$ = H5Tcopy(H5T_NATIVE_DOUBLE); }
                |       H5T_NATIVE_LDOUBLE_TOKEN  { $<ival>$ = H5Tcopy(H5T_NATIVE_LDOUBLE); }
                ;

compound_type   :       H5T_COMPOUND_TOKEN
                            { csindex++; cmpd_stack[csindex].id = H5Tcreate(H5T_COMPOUND, 1); /*temporarily set size to 1*/ } 
                        '{' memb_list '}'     
                            { $<ival>$ = cmpd_stack[csindex].id; 
                              cmpd_stack[csindex].id = 0;
                              cmpd_stack[csindex].first_memb = 1; 
                              csindex--;
                            }
                ;
memb_list       :       
                |       memb_list memb_def
                ;
memb_def        :       ddl_type { cmpd_stack[csindex].is_field = 1; /*notify lexer a compound member is parsed*/ } 
                        '"' field_name '"' ';'
                        {   int origin_size, new_size;
                            hid_t dtype_id = cmpd_stack[csindex].id;

                            /*Adjust size and insert member. Leave no space between.*/
                            if(cmpd_stack[csindex].first_memb) { /*reclaim the size 1 temporarily set*/
                                new_size = H5Tget_size($<ival>1);
                                H5Tset_size(dtype_id, new_size);
                                /*member name is saved in yylval.sval by lexer*/
                                H5Tinsert(dtype_id, yylval.sval, 0, $<ival>1);

                                cmpd_stack[csindex].first_memb = 0;
                            } else {
                                origin_size = H5Tget_size(dtype_id);
                                new_size = origin_size + H5Tget_size($<ival>1);
                                H5Tset_size(dtype_id, new_size);
                                H5Tinsert(dtype_id, yylval.sval, origin_size, $<ival>1);
                            }
                          
                            cmpd_stack[csindex].is_field = 0;
                            H5Tclose($<ival>1);
                             
                            new_size = H5Tget_size(dtype_id);
                        }
                ;
field_name      :       STRING
                ;

array_type      :       H5T_ARRAY_TOKEN { asindex++; /*pushd onto the stack*/ }
                        '{' dim_list ddl_type '}'
                        { 
                          $<ival>$ = H5Tarray_create($<ival>5, arr_stack[asindex].ndims, arr_stack[asindex].dims, NULL);
                          arr_stack[asindex].ndims = 0;
                          asindex--;
                          H5Tclose($<ival>5);
                        }            
                ;
dim_list        :
                |       dim_list dim
                ;
dim             :       '[' { arr_stack[asindex].is_dim = 1; /*notice lexer of dimension size*/ }
                        dimsize { int ndims = arr_stack[asindex].ndims;
                                  arr_stack[asindex].dims[ndims] = (hsize_t)yylval.ival; 
                                  arr_stack[asindex].ndims++;
                                  arr_stack[asindex].is_dim = 0; 
                                } 
                        ']'
                ;
dimsize         :       NUMBER
                ;

vlen_type       :       H5T_VLEN_TOKEN '{' ddl_type '}'
                            { $<ival>$ = H5Tvlen_create($<ival>3); H5Tclose($<ival>3); }
                ;

opaque_type     :       H5T_OPAQUE_TOKEN
                        '{' 
                            OPQ_SIZE_TOKEN { is_opq_size = 1; } opaque_size ';'
                            {   
                                size_t size = (size_t)yylval.ival;
                                $<ival>$ = H5Tcreate(H5T_OPAQUE, size);
                                is_opq_size = 0;    
                            } 
                            OPQ_TAG_TOKEN { is_opq_tag = 1; } '"' opaque_tag '"' ';'
                            {  
                                H5Tset_tag($<ival>7, yylval.sval);
                                is_opq_tag = 0;
                            } 
                        '}' { $<ival>$ = $<ival>7; }
                ;
opaque_size     :       NUMBER
                ;
opaque_tag      :       STRING
                ;

string_type     :       H5T_STRING_TOKEN 
                        '{' 
                            CTYPE_TOKEN ctype ';'
                            {
                                if($<ival>4 == H5T_C_S1_TOKEN)
                                    $<ival>$ = H5Tcopy(H5T_C_S1);
                                else if($<ival>4 == H5T_FORTRAN_S1_TOKEN)
                                    $<ival>$ = H5Tcopy(H5T_FORTRAN_S1);
                            }
                            STRSIZE_TOKEN { is_str_size = 1; } strsize ';'
                            {  
                                if($<ival>9 == H5T_VARIABLE_TOKEN)
                                    H5Tset_size($<ival>6, H5T_VARIABLE);
                                else 
                                    H5Tset_size($<ival>6, yylval.ival);
                                is_str_size = 0; 
                            }
                            STRPAD_TOKEN strpad ';'
                            {
                                if($<ival>13 == H5T_STR_NULLTERM_TOKEN)
                                    H5Tset_strpad($<ival>6, H5T_STR_NULLTERM);
                                else if($<ival>13 == H5T_STR_NULLPAD_TOKEN)
                                    H5Tset_strpad($<ival>6, H5T_STR_NULLPAD);
                                else if($<ival>13 == H5T_STR_SPACEPAD_TOKEN)
                                    H5Tset_strpad($<ival>6, H5T_STR_SPACEPAD);
                            }
                            CSET_TOKEN cset ';'
                            {  
                                if($<ival>17 == H5T_CSET_ASCII_TOKEN)
                                    H5Tset_cset($<ival>6, H5T_CSET_ASCII);
                            }
                        '}' { $<ival>$ = $<ival>6; }

                ;
strsize         :       H5T_VARIABLE_TOKEN     {$<ival>$ = H5T_VARIABLE_TOKEN;}
                |       NUMBER
                ;
strpad          :       H5T_STR_NULLTERM_TOKEN {$<ival>$ = H5T_STR_NULLTERM_TOKEN;}
                |       H5T_STR_NULLPAD_TOKEN  {$<ival>$ = H5T_STR_NULLPAD_TOKEN;}
                |       H5T_STR_SPACEPAD_TOKEN {$<ival>$ = H5T_STR_SPACEPAD_TOKEN;}
                ;
cset            :       H5T_CSET_ASCII_TOKEN {$<ival>$ = H5T_CSET_ASCII_TOKEN;}
                ;
ctype           :       H5T_C_S1_TOKEN         {$<ival>$ = H5T_C_S1_TOKEN;}
                |       H5T_FORTRAN_S1_TOKEN   {$<ival>$ = H5T_FORTRAN_S1_TOKEN;}
                ;

enum_type       :       H5T_ENUM_TOKEN '{' integer_type ';' 
                            { is_enum = 1; enum_id = H5Tenum_create($<ival>3); H5Tclose($<ival>3); }
                        enum_list '}'
                            { is_enum = 0; /*reset*/ $<ival>$ = enum_id; }
                ;
enum_list       :
                |       enum_list enum_def
                ;
enum_def        :       '"' enum_symbol '"' {
                                                is_enum_memb = 1; /*indicate member of enum*/
                                                enum_memb_symbol = strdup(yylval.sval); 
                                            }
                        enum_val ';'
                            {
                                int memb_val;
                                if(is_enum && is_enum_memb) { /*if it's an enum member*/
                                    H5Tenum_insert(enum_id, enum_memb_symbol, (memb_val=yylval.ival,&memb_val));
                                    is_enum_memb = 0; 
                                    if(enum_memb_symbol) free(enum_memb_symbol);
                                }
                            }
                ;
enum_symbol     :       STRING
                ;
enum_val        :       NUMBER
                ;
