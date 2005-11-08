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

#ifndef lint
static char const 
yyrcsid[] = "$FreeBSD: src/usr.bin/yacc/skeleton.c,v 1.28 2000/01/17 02:04:06 bde Exp $";
#endif
#include <stdlib.h>
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYLEX yylex()
#define YYEMPTY -1
#define yyclearin (yychar=(YYEMPTY))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING() (yyerrflag!=0)
static int yygrowstack();
#define YYPREFIX "yy"
#line 2 "H5LTparse.y"
#include<stdio.h>
#include<string.h>
#include<hdf5.h>

extern int yylex();
extern int yyerror(char *);

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

#line 46 "H5LTparse.y"
typedef union {
    int   ival;         /*for integer token*/
    char  *sval;        /*for compound member name*/
} YYSTYPE;
#line 66 "y.tab.c"
#define YYERRCODE 256
#define H5T_STD_I8BE_TOKEN 257
#define H5T_STD_I8LE_TOKEN 258
#define H5T_STD_I16BE_TOKEN 259
#define H5T_STD_I16LE_TOKEN 260
#define H5T_STD_I32BE_TOKEN 261
#define H5T_STD_I32LE_TOKEN 262
#define H5T_STD_I64BE_TOKEN 263
#define H5T_STD_I64LE_TOKEN 264
#define H5T_STD_U8BE_TOKEN 265
#define H5T_STD_U8LE_TOKEN 266
#define H5T_STD_U16BE_TOKEN 267
#define H5T_STD_U16LE_TOKEN 268
#define H5T_STD_U32BE_TOKEN 269
#define H5T_STD_U32LE_TOKEN 270
#define H5T_STD_U64BE_TOKEN 271
#define H5T_STD_U64LE_TOKEN 272
#define H5T_NATIVE_CHAR_TOKEN 273
#define H5T_NATIVE_SCHAR_TOKEN 274
#define H5T_NATIVE_UCHAR_TOKEN 275
#define H5T_NATIVE_SHORT_TOKEN 276
#define H5T_NATIVE_USHORT_TOKEN 277
#define H5T_NATIVE_INT_TOKEN 278
#define H5T_NATIVE_UINT_TOKEN 279
#define H5T_NATIVE_LONG_TOKEN 280
#define H5T_NATIVE_ULONG_TOKEN 281
#define H5T_NATIVE_LLONG_TOKEN 282
#define H5T_NATIVE_ULLONG_TOKEN 283
#define H5T_IEEE_F32BE_TOKEN 284
#define H5T_IEEE_F32LE_TOKEN 285
#define H5T_IEEE_F64BE_TOKEN 286
#define H5T_IEEE_F64LE_TOKEN 287
#define H5T_NATIVE_FLOAT_TOKEN 288
#define H5T_NATIVE_DOUBLE_TOKEN 289
#define H5T_NATIVE_LDOUBLE_TOKEN 290
#define H5T_STRING_TOKEN 291
#define STRSIZE_TOKEN 292
#define STRPAD_TOKEN 293
#define CSET_TOKEN 294
#define CTYPE_TOKEN 295
#define H5T_VARIABLE_TOKEN 296
#define H5T_STR_NULLTERM_TOKEN 297
#define H5T_STR_NULLPAD_TOKEN 298
#define H5T_STR_SPACEPAD_TOKEN 299
#define H5T_CSET_ASCII_TOKEN 300
#define H5T_C_S1_TOKEN 301
#define H5T_FORTRAN_S1_TOKEN 302
#define H5T_OPAQUE_TOKEN 303
#define OPQ_SIZE_TOKEN 304
#define OPQ_TAG_TOKEN 305
#define H5T_COMPOUND_TOKEN 306
#define H5T_ENUM_TOKEN 307
#define H5T_ARRAY_TOKEN 308
#define H5T_VLEN_TOKEN 309
#define STRING 310
#define NUMBER 311
const short yylhs[] = {                                        -1,
    0,    0,    1,    1,    1,    1,    2,    2,    2,    2,
    2,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
    6,    6,    6,    6,    6,    6,    6,    6,    7,    7,
    7,    7,    7,    7,    7,   11,    3,   12,   12,   14,
   13,   15,   16,    4,   17,   17,   20,   21,   18,   19,
    5,   23,   24,   25,   27,   10,   22,   26,   29,   31,
   32,   34,   36,    8,   30,   30,   33,   33,   33,   35,
   28,   28,   38,    9,   37,   37,   42,   39,   40,   41,
};
const short yylen[] = {                                         2,
    0,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    0,    5,    0,    2,    0,
    6,    1,    0,    6,    0,    2,    0,    0,    5,    1,
    4,    0,    0,    0,    0,   15,    1,    1,    0,    0,
    0,    0,    0,   20,    1,    1,    1,    1,    1,    1,
    1,    1,    0,    7,    0,    2,    0,    6,    1,    1,
};
const short yydefred[] = {                                      0,
   12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
   22,   23,   24,   25,   26,   27,   28,   29,   30,   31,
   32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
   42,   43,   44,   45,    0,    0,   46,    0,   53,    0,
    0,    2,    3,    4,    5,    6,    7,    8,    9,   10,
   11,    0,    0,    0,    0,    0,    0,    0,   62,   48,
    0,   55,    0,   81,   82,    0,    0,    0,   83,    0,
   61,   69,   67,    0,   47,   50,   49,   85,   57,    0,
   56,    0,   63,    0,    0,    0,   54,   70,    0,    0,
   84,    0,   86,   60,   58,    0,   64,   52,    0,   89,
    0,    0,   75,   76,    0,    0,    0,   87,   59,   71,
    0,   51,    0,    0,   68,    0,   90,    0,    0,    0,
   88,   77,   78,   79,    0,   65,   72,    0,    0,   66,
    0,   80,    0,   73,    0,   74,
};
const short yydgoto[] = {                                      41,
   42,   43,   44,   45,   46,   47,   48,   49,   50,   51,
   54,   68,   77,   84,   99,   56,   70,   81,   95,   86,
  102,   74,   67,   89,  106,  116,  128,   66,   82,  105,
   96,  114,  125,  129,  133,  135,   85,   78,   93,  101,
  118,  113,
};
const short yysindex[] = {                                   -255,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  -81,  -80,    0,  -79,    0,  -78,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0, -249, -257,  -74, -202,  -73, -255, -261,    0,    0,
   25,    0,  -40,    0,    0,   27, -223,  -38,    0,  -91,
    0,    0,    0,   30,    0,    0,    0,    0,    0,  -35,
    0, -201,    0,   59,  -33, -216,    0,    0, -207, -211,
    0, -210,    0,    0,    0, -214,    0,    0,   67,    0,
   68,   10,    0,    0,   45,   71,   47,    0,    0,    0,
 -203,    0, -200, -185,    0,   75,    0,   51, -260,   53,
    0,    0,    0,    0,   54,    0,    0,  -11, -179,    0,
 -184,    0,   58,    0,   -7,    0,
};
const short yyrindex[] = {                                    119,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,
};
const short yygindex[] = {                                      0,
   26,    0,    0,    0,    0,   65,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,
};
#define YYTABLESIZE 271
const short yytable[] = {                                      79,
   92,    1,    2,    3,    4,    5,    6,    7,    8,    9,
   10,   11,   12,   13,   14,   15,   16,   17,   18,   19,
   20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
   30,   31,   32,   33,   34,   35,  122,  123,  124,   64,
   65,   52,   53,   55,   57,   58,   59,   36,   60,   62,
   37,   38,   39,   40,    1,    2,    3,    4,    5,    6,
    7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
   17,   18,   19,   20,   21,   22,   23,   24,   25,   26,
   27,  103,   63,   69,   71,   72,   75,   73,   83,   87,
   88,   91,   90,   76,   94,   80,  104,   97,   98,  100,
  107,  108,  109,  110,  111,  112,  115,  119,  120,  121,
  117,  126,  127,  130,  131,  132,  134,  136,    1,   61,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    1,    2,    3,    4,    5,
    6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
   16,   17,   18,   19,   20,   21,   22,   23,   24,   25,
   26,   27,   28,   29,   30,   31,   32,   33,   34,   35,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   36,    0,    0,   37,   38,   39,   40,    1,    2,
    3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
   13,   14,   15,   16,   17,   18,   19,   20,   21,   22,
   23,   24,   25,   26,   27,   28,   29,   30,   31,   32,
   33,   34,   35,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   36,    0,    0,   37,   38,   39,
   40,
};
const short yycheck[] = {                                      91,
   34,  257,  258,  259,  260,  261,  262,  263,  264,  265,
  266,  267,  268,  269,  270,  271,  272,  273,  274,  275,
  276,  277,  278,  279,  280,  281,  282,  283,  284,  285,
  286,  287,  288,  289,  290,  291,  297,  298,  299,  301,
  302,  123,  123,  123,  123,  295,  304,  303,  123,  123,
  306,  307,  308,  309,  257,  258,  259,  260,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,  279,  280,  281,  282,
  283,  296,   57,   59,  125,   59,  125,  311,   59,  125,
  292,  125,   34,   68,  311,   70,  311,  305,  310,  310,
   34,   34,   93,   59,   34,   59,  310,  293,   34,   59,
  311,   59,   59,  125,  294,  300,   59,  125,    0,   55,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  257,  258,  259,  260,  261,
  262,  263,  264,  265,  266,  267,  268,  269,  270,  271,
  272,  273,  274,  275,  276,  277,  278,  279,  280,  281,
  282,  283,  284,  285,  286,  287,  288,  289,  290,  291,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  303,   -1,   -1,  306,  307,  308,  309,  257,  258,
  259,  260,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,  271,  272,  273,  274,  275,  276,  277,  278,
  279,  280,  281,  282,  283,  284,  285,  286,  287,  288,
  289,  290,  291,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  303,   -1,   -1,  306,  307,  308,
  309,
};
#define YYFINAL 41
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 311
#if YYDEBUG
const char * const yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
"'\"'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"';'",0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'['",0,"']'",0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'",0,"'}'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
"H5T_STD_I8BE_TOKEN","H5T_STD_I8LE_TOKEN","H5T_STD_I16BE_TOKEN",
"H5T_STD_I16LE_TOKEN","H5T_STD_I32BE_TOKEN","H5T_STD_I32LE_TOKEN",
"H5T_STD_I64BE_TOKEN","H5T_STD_I64LE_TOKEN","H5T_STD_U8BE_TOKEN",
"H5T_STD_U8LE_TOKEN","H5T_STD_U16BE_TOKEN","H5T_STD_U16LE_TOKEN",
"H5T_STD_U32BE_TOKEN","H5T_STD_U32LE_TOKEN","H5T_STD_U64BE_TOKEN",
"H5T_STD_U64LE_TOKEN","H5T_NATIVE_CHAR_TOKEN","H5T_NATIVE_SCHAR_TOKEN",
"H5T_NATIVE_UCHAR_TOKEN","H5T_NATIVE_SHORT_TOKEN","H5T_NATIVE_USHORT_TOKEN",
"H5T_NATIVE_INT_TOKEN","H5T_NATIVE_UINT_TOKEN","H5T_NATIVE_LONG_TOKEN",
"H5T_NATIVE_ULONG_TOKEN","H5T_NATIVE_LLONG_TOKEN","H5T_NATIVE_ULLONG_TOKEN",
"H5T_IEEE_F32BE_TOKEN","H5T_IEEE_F32LE_TOKEN","H5T_IEEE_F64BE_TOKEN",
"H5T_IEEE_F64LE_TOKEN","H5T_NATIVE_FLOAT_TOKEN","H5T_NATIVE_DOUBLE_TOKEN",
"H5T_NATIVE_LDOUBLE_TOKEN","H5T_STRING_TOKEN","STRSIZE_TOKEN","STRPAD_TOKEN",
"CSET_TOKEN","CTYPE_TOKEN","H5T_VARIABLE_TOKEN","H5T_STR_NULLTERM_TOKEN",
"H5T_STR_NULLPAD_TOKEN","H5T_STR_SPACEPAD_TOKEN","H5T_CSET_ASCII_TOKEN",
"H5T_C_S1_TOKEN","H5T_FORTRAN_S1_TOKEN","H5T_OPAQUE_TOKEN","OPQ_SIZE_TOKEN",
"OPQ_TAG_TOKEN","H5T_COMPOUND_TOKEN","H5T_ENUM_TOKEN","H5T_ARRAY_TOKEN",
"H5T_VLEN_TOKEN","STRING","NUMBER",
};
const char * const yyrule[] = {
"$accept : start",
"start :",
"start : ddl_type",
"ddl_type : atomic_type",
"ddl_type : compound_type",
"ddl_type : array_type",
"ddl_type : vlen_type",
"atomic_type : integer_type",
"atomic_type : fp_type",
"atomic_type : string_type",
"atomic_type : enum_type",
"atomic_type : opaque_type",
"integer_type : H5T_STD_I8BE_TOKEN",
"integer_type : H5T_STD_I8LE_TOKEN",
"integer_type : H5T_STD_I16BE_TOKEN",
"integer_type : H5T_STD_I16LE_TOKEN",
"integer_type : H5T_STD_I32BE_TOKEN",
"integer_type : H5T_STD_I32LE_TOKEN",
"integer_type : H5T_STD_I64BE_TOKEN",
"integer_type : H5T_STD_I64LE_TOKEN",
"integer_type : H5T_STD_U8BE_TOKEN",
"integer_type : H5T_STD_U8LE_TOKEN",
"integer_type : H5T_STD_U16BE_TOKEN",
"integer_type : H5T_STD_U16LE_TOKEN",
"integer_type : H5T_STD_U32BE_TOKEN",
"integer_type : H5T_STD_U32LE_TOKEN",
"integer_type : H5T_STD_U64BE_TOKEN",
"integer_type : H5T_STD_U64LE_TOKEN",
"integer_type : H5T_NATIVE_CHAR_TOKEN",
"integer_type : H5T_NATIVE_SCHAR_TOKEN",
"integer_type : H5T_NATIVE_UCHAR_TOKEN",
"integer_type : H5T_NATIVE_SHORT_TOKEN",
"integer_type : H5T_NATIVE_USHORT_TOKEN",
"integer_type : H5T_NATIVE_INT_TOKEN",
"integer_type : H5T_NATIVE_UINT_TOKEN",
"integer_type : H5T_NATIVE_LONG_TOKEN",
"integer_type : H5T_NATIVE_ULONG_TOKEN",
"integer_type : H5T_NATIVE_LLONG_TOKEN",
"integer_type : H5T_NATIVE_ULLONG_TOKEN",
"fp_type : H5T_IEEE_F32BE_TOKEN",
"fp_type : H5T_IEEE_F32LE_TOKEN",
"fp_type : H5T_IEEE_F64BE_TOKEN",
"fp_type : H5T_IEEE_F64LE_TOKEN",
"fp_type : H5T_NATIVE_FLOAT_TOKEN",
"fp_type : H5T_NATIVE_DOUBLE_TOKEN",
"fp_type : H5T_NATIVE_LDOUBLE_TOKEN",
"$$1 :",
"compound_type : H5T_COMPOUND_TOKEN $$1 '{' memb_list '}'",
"memb_list :",
"memb_list : memb_list memb_def",
"$$2 :",
"memb_def : ddl_type $$2 '\"' field_name '\"' ';'",
"field_name : STRING",
"$$3 :",
"array_type : H5T_ARRAY_TOKEN $$3 '{' dim_list ddl_type '}'",
"dim_list :",
"dim_list : dim_list dim",
"$$4 :",
"$$5 :",
"dim : '[' $$4 dimsize $$5 ']'",
"dimsize : NUMBER",
"vlen_type : H5T_VLEN_TOKEN '{' ddl_type '}'",
"$$6 :",
"$$7 :",
"$$8 :",
"$$9 :",
"opaque_type : H5T_OPAQUE_TOKEN '{' OPQ_SIZE_TOKEN $$6 opaque_size ';' $$7 OPQ_TAG_TOKEN $$8 '\"' opaque_tag '\"' ';' $$9 '}'",
"opaque_size : NUMBER",
"opaque_tag : STRING",
"$$10 :",
"$$11 :",
"$$12 :",
"$$13 :",
"$$14 :",
"string_type : H5T_STRING_TOKEN '{' CTYPE_TOKEN ctype ';' $$10 STRSIZE_TOKEN $$11 strsize ';' $$12 STRPAD_TOKEN strpad ';' $$13 CSET_TOKEN cset ';' $$14 '}'",
"strsize : H5T_VARIABLE_TOKEN",
"strsize : NUMBER",
"strpad : H5T_STR_NULLTERM_TOKEN",
"strpad : H5T_STR_NULLPAD_TOKEN",
"strpad : H5T_STR_SPACEPAD_TOKEN",
"cset : H5T_CSET_ASCII_TOKEN",
"ctype : H5T_C_S1_TOKEN",
"ctype : H5T_FORTRAN_S1_TOKEN",
"$$15 :",
"enum_type : H5T_ENUM_TOKEN '{' integer_type ';' $$15 enum_list '}'",
"enum_list :",
"enum_list : enum_list enum_def",
"$$16 :",
"enum_def : '\"' enum_symbol '\"' $$16 enum_val ';'",
"enum_symbol : STRING",
"enum_val : NUMBER",
};
#endif
#if YYDEBUG
#include <stdio.h>
#endif
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 10000
#define YYMAXDEPTH 10000
#endif
#endif
#define YYINITSTACKSIZE 200
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short *yyss;
short *yysslim;
YYSTYPE *yyvs;
int yystacksize;
/* allocate initial stack or double stack size, up to YYMAXDEPTH */
static int yygrowstack()
{
    int newsize, i;
    short *newss;
    YYSTYPE *newvs;

    if ((newsize = yystacksize) == 0)
        newsize = YYINITSTACKSIZE;
    else if (newsize >= YYMAXDEPTH)
        return -1;
    else if ((newsize *= 2) > YYMAXDEPTH)
        newsize = YYMAXDEPTH;
    i = yyssp - yyss;
    newss = yyss ? (short *)realloc(yyss, newsize * sizeof *newss) :
      (short *)malloc(newsize * sizeof *newss);
    if (newss == NULL)
        return -1;
    yyss = newss;
    yyssp = newss + i;
    newvs = yyvs ? (YYSTYPE *)realloc(yyvs, newsize * sizeof *newvs) :
      (YYSTYPE *)malloc(newsize * sizeof *newvs);
    if (newvs == NULL)
        return -1;
    yyvs = newvs;
    yyvsp = newvs + i;
    yystacksize = newsize;
    yysslim = yyss + newsize - 1;
    return 0;
}

#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab

#ifndef YYPARSE_PARAM
#if defined(__cplusplus) || __STDC__
#define YYPARSE_PARAM_ARG void
#define YYPARSE_PARAM_DECL
#else	/* ! ANSI-C/C++ */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif	/* ANSI-C/C++ */
#else	/* YYPARSE_PARAM */
#ifndef YYPARSE_PARAM_TYPE
#define YYPARSE_PARAM_TYPE void *
#endif
#if defined(__cplusplus) || __STDC__
#define YYPARSE_PARAM_ARG YYPARSE_PARAM_TYPE YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else	/* ! ANSI-C/C++ */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL YYPARSE_PARAM_TYPE YYPARSE_PARAM;
#endif	/* ANSI-C/C++ */
#endif	/* ! YYPARSE_PARAM */

int
yyparse (YYPARSE_PARAM_ARG)
    YYPARSE_PARAM_DECL
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register const char *yys;

    if ((yys = getenv("YYDEBUG")))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    if (yyss == NULL && yygrowstack()) goto yyoverflow;
    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if ((yyn = yydefred[yystate])) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yyssp >= yysslim && yygrowstack())
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#if defined(lint) || defined(__GNUC__)
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#if defined(lint) || defined(__GNUC__)
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yysslim && yygrowstack())
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 1:
#line 78 "H5LTparse.y"
{ memset(arr_stack, 0, STACK_SIZE*sizeof(struct arr_info)); /*initialize here?*/ }
break;
case 2:
#line 79 "H5LTparse.y"
{ return yyval.ival;}
break;
case 12:
#line 93 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_I8BE); }
break;
case 13:
#line 94 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_I8LE); }
break;
case 14:
#line 95 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_I16BE); }
break;
case 15:
#line 96 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_I16LE); }
break;
case 16:
#line 97 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_I32BE); }
break;
case 17:
#line 98 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_I32LE); }
break;
case 18:
#line 99 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_I64BE); }
break;
case 19:
#line 100 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_I64LE); }
break;
case 20:
#line 101 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_U8BE); }
break;
case 21:
#line 102 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_U8LE); }
break;
case 22:
#line 103 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_U16BE); }
break;
case 23:
#line 104 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_U16LE); }
break;
case 24:
#line 105 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_U32BE); }
break;
case 25:
#line 106 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_U32LE); }
break;
case 26:
#line 107 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_U64BE); }
break;
case 27:
#line 108 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_STD_U64LE); }
break;
case 28:
#line 109 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_CHAR); }
break;
case 29:
#line 110 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_SCHAR); }
break;
case 30:
#line 111 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_UCHAR); }
break;
case 31:
#line 112 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_SHORT); }
break;
case 32:
#line 113 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_USHORT); }
break;
case 33:
#line 114 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_INT); }
break;
case 34:
#line 115 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_UINT); }
break;
case 35:
#line 116 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_LONG); }
break;
case 36:
#line 117 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_ULONG); }
break;
case 37:
#line 118 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_LLONG); }
break;
case 38:
#line 119 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_ULLONG); }
break;
case 39:
#line 122 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_IEEE_F32BE); }
break;
case 40:
#line 123 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_IEEE_F32LE); }
break;
case 41:
#line 124 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_IEEE_F64BE); }
break;
case 42:
#line 125 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_IEEE_F64LE); }
break;
case 43:
#line 126 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_FLOAT); }
break;
case 44:
#line 127 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_DOUBLE); }
break;
case 45:
#line 128 "H5LTparse.y"
{ yyval.ival = H5Tcopy(H5T_NATIVE_LDOUBLE); }
break;
case 46:
#line 132 "H5LTparse.y"
{ csindex++; cmpd_stack[csindex].id = H5Tcreate(H5T_COMPOUND, 1); /*temporarily set size to 1*/ }
break;
case 47:
#line 134 "H5LTparse.y"
{ yyval.ival = cmpd_stack[csindex].id; 
                              cmpd_stack[csindex].id = 0;
                              cmpd_stack[csindex].first_memb = 1; 
                              csindex--;
                            }
break;
case 50:
#line 143 "H5LTparse.y"
{ cmpd_stack[csindex].is_field = 1; /*notify lexer a compound member is parsed*/ }
break;
case 51:
#line 145 "H5LTparse.y"
{   int origin_size, new_size;
                            hid_t dtype_id = cmpd_stack[csindex].id;

                            /*Adjust size and insert member. Leave no space between.*/
                            if(cmpd_stack[csindex].first_memb) { /*reclaim the size 1 temporarily set*/
                                new_size = H5Tget_size(yyvsp[-5].ival);
                                H5Tset_size(dtype_id, new_size);
                                /*member name is saved in yylval.sval by lexer*/
                                H5Tinsert(dtype_id, yylval.sval, 0, yyvsp[-5].ival);

                                cmpd_stack[csindex].first_memb = 0;
                            } else {
                                origin_size = H5Tget_size(dtype_id);
                                new_size = origin_size + H5Tget_size(yyvsp[-5].ival);
                                H5Tset_size(dtype_id, new_size);
                                H5Tinsert(dtype_id, yylval.sval, origin_size, yyvsp[-5].ival);
                            }
                          
                            cmpd_stack[csindex].is_field = 0;
                            H5Tclose(yyvsp[-5].ival);
                             
                            new_size = H5Tget_size(dtype_id);
                        }
break;
case 53:
#line 172 "H5LTparse.y"
{ asindex++; /*pushd onto the stack*/ }
break;
case 54:
#line 174 "H5LTparse.y"
{ 
                          yyval.ival = H5Tarray_create(yyvsp[-1].ival, arr_stack[asindex].ndims, arr_stack[asindex].dims, NULL);
                          arr_stack[asindex].ndims = 0;
                          asindex--;
                          H5Tclose(yyvsp[-1].ival);
                        }
break;
case 57:
#line 184 "H5LTparse.y"
{ arr_stack[asindex].is_dim = 1; /*notice lexer of dimension size*/ }
break;
case 58:
#line 185 "H5LTparse.y"
{ int ndims = arr_stack[asindex].ndims;
                                  arr_stack[asindex].dims[ndims] = (hsize_t)yylval.ival; 
                                  arr_stack[asindex].ndims++;
                                  arr_stack[asindex].is_dim = 0; 
                                }
break;
case 61:
#line 196 "H5LTparse.y"
{ yyval.ival = H5Tvlen_create(yyvsp[-1].ival); H5Tclose(yyvsp[-1].ival); }
break;
case 62:
#line 201 "H5LTparse.y"
{ is_opq_size = 1; }
break;
case 63:
#line 202 "H5LTparse.y"
{   
                                size_t size = (size_t)yylval.ival;
                                yyval.ival = H5Tcreate(H5T_OPAQUE, size);
                                is_opq_size = 0;    
                            }
break;
case 64:
#line 207 "H5LTparse.y"
{ is_opq_tag = 1; }
break;
case 65:
#line 208 "H5LTparse.y"
{  
                                H5Tset_tag(yyvsp[-6].ival, yylval.sval);
                                is_opq_tag = 0;
                            }
break;
case 66:
#line 212 "H5LTparse.y"
{ yyval.ival = yyvsp[-8].ival; }
break;
case 69:
#line 222 "H5LTparse.y"
{
                                if(yyvsp[-1].ival == H5T_C_S1_TOKEN)
                                    yyval.ival = H5Tcopy(H5T_C_S1);
                                else if(yyvsp[-1].ival == H5T_FORTRAN_S1_TOKEN)
                                    yyval.ival = H5Tcopy(H5T_FORTRAN_S1);
                            }
break;
case 70:
#line 228 "H5LTparse.y"
{ is_str_size = 1; }
break;
case 71:
#line 229 "H5LTparse.y"
{  
                                if(yyvsp[-1].ival == H5T_VARIABLE_TOKEN)
                                    H5Tset_size(yyvsp[-4].ival, H5T_VARIABLE);
                                else 
                                    H5Tset_size(yyvsp[-4].ival, yylval.ival);
                                is_str_size = 0; 
                            }
break;
case 72:
#line 237 "H5LTparse.y"
{
                                if(yyvsp[-1].ival == H5T_STR_NULLTERM_TOKEN)
                                    H5Tset_strpad(yyvsp[-8].ival, H5T_STR_NULLTERM);
                                else if(yyvsp[-1].ival == H5T_STR_NULLPAD_TOKEN)
                                    H5Tset_strpad(yyvsp[-8].ival, H5T_STR_NULLPAD);
                                else if(yyvsp[-1].ival == H5T_STR_SPACEPAD_TOKEN)
                                    H5Tset_strpad(yyvsp[-8].ival, H5T_STR_SPACEPAD);
                            }
break;
case 73:
#line 246 "H5LTparse.y"
{  
                                if(yyvsp[-1].ival == H5T_CSET_ASCII_TOKEN)
                                    H5Tset_cset(yyvsp[-12].ival, H5T_CSET_ASCII);
                            }
break;
case 74:
#line 250 "H5LTparse.y"
{ yyval.ival = yyvsp[-14].ival; }
break;
case 75:
#line 253 "H5LTparse.y"
{yyval.ival = H5T_VARIABLE_TOKEN;}
break;
case 77:
#line 256 "H5LTparse.y"
{yyval.ival = H5T_STR_NULLTERM_TOKEN;}
break;
case 78:
#line 257 "H5LTparse.y"
{yyval.ival = H5T_STR_NULLPAD_TOKEN;}
break;
case 79:
#line 258 "H5LTparse.y"
{yyval.ival = H5T_STR_SPACEPAD_TOKEN;}
break;
case 80:
#line 260 "H5LTparse.y"
{yyval.ival = H5T_CSET_ASCII_TOKEN;}
break;
case 81:
#line 262 "H5LTparse.y"
{yyval.ival = H5T_C_S1_TOKEN;}
break;
case 82:
#line 263 "H5LTparse.y"
{yyval.ival = H5T_FORTRAN_S1_TOKEN;}
break;
case 83:
#line 267 "H5LTparse.y"
{ is_enum = 1; enum_id = H5Tenum_create(yyvsp[-1].ival); H5Tclose(yyvsp[-1].ival); }
break;
case 84:
#line 269 "H5LTparse.y"
{ is_enum = 0; /*reset*/ yyval.ival = enum_id; }
break;
case 87:
#line 274 "H5LTparse.y"
{
                                                is_enum_memb = 1; /*indicate member of enum*/
                                                enum_memb_symbol = strdup(yylval.sval); 
                                            }
break;
case 88:
#line 279 "H5LTparse.y"
{
                                int memb_val;
                                if(is_enum && is_enum_memb) { /*if it's an enum member*/
                                    H5Tenum_insert(enum_id, enum_memb_symbol, (memb_val=yylval.ival,&memb_val));
                                    is_enum_memb = 0; 
                                    if(enum_memb_symbol) free(enum_memb_symbol);
                                }
                            }
break;
#line 956 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
#endif
    if (yyssp >= yysslim && yygrowstack())
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}
